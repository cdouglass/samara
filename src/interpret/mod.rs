mod lex;
use self::lex::build_lexer;
use self::lex::build_lexer_decl;
use self::lex::TokenStream;

mod structures;
use self::structures::Atom::*;
use self::structures::LetBinding;
use self::structures::Op::*;
use self::structures::Term;
use self::structures::Term::*;
use self::structures::Type;
use self::structures::sums::SumTypeScheme;
pub use self::structures::sums::SumTypeDefs;

mod parse;
use self::parse::parse_expr;
use self::parse::parse_decl;

pub mod infer;
use self::infer::infer_type;
use self::infer::GenTypeVar;

#[cfg(test)]
mod tests;

pub fn type_of(expr: &str, bindings: &[LetBinding], mut gen: &mut GenTypeVar, sum_types: &SumTypeDefs) -> (Result<Term, String>, Result<Type, String>) {
    let ast = parse_from_str(expr, bindings, sum_types);
    match ast {
        Ok(term) =>{
            let typ = infer_type(&term, bindings, &mut gen, sum_types);
            (Ok(term), typ)
        },
        Err(msg) => (Err(msg), Err(String::from("Syntax error")))
    }
}

pub fn declare_sum_type(decl: &str, mut gen: &mut GenTypeVar, mut sum_types: &mut SumTypeDefs) -> Result<SumTypeScheme, String> {
    match build_lexer_decl(decl) {
        TokenStream::Decl(mut tokens) => {
            let sum_type_scheme = parse_decl(&mut tokens, gen, sum_types)?;
            let variants = sum_type_scheme.variants.iter().cloned().collect();
            sum_types.add_type(&sum_type_scheme.name, variants, sum_type_scheme.universals.clone())?;
            Ok(sum_type_scheme)
        },
        _ => panic!()
    }
}

fn parse_from_str(expr: &str, bindings: &[LetBinding], sum_types: &SumTypeDefs) -> Result<Term, String> {
    let mut identifiers: Vec<String> = bindings.into_iter().map(|x| x.name.clone()).collect();
    match build_lexer(expr.trim()) {
        TokenStream::Expr(mut tokens) => parse_expr(&mut tokens, &mut identifiers, sum_types),
        _ => panic!()
    }
}

pub fn evaluate(expr: &str, mut session_bindings: &mut Vec<LetBinding>, mut gen: &mut GenTypeVar, sum_types: &SumTypeDefs) -> Result<Term, String> {
    match type_of(expr, session_bindings, gen, sum_types) {
        (Err(msg), _) | (_, Err(msg)) => Err(msg),
        (Ok(ast), Ok(typ)) => {
            let term = reduce(ast, session_bindings)?;

            if let Let(ref s, ref value, None) = term {
                session_bindings.push(LetBinding{name: s.clone(), term: *value.clone(), typ: typ.clone()});
            }

            Ok(term)
        }
    }
}

fn reduce(ast: Term, session_bindings: &[LetBinding]) -> Result<Term, String> {
    match ast {
        App(func, arg) => {
            let f = reduce(*func, session_bindings);
            let a = reduce(*arg, session_bindings);
            match (f, a) {
                (Ok(f), Ok(a)) => apply(f, a, session_bindings),
                _ => Err(String::from("Type error"))
            }
        },
        Conditional(pred, true_case, false_case) => {
            match reduce(*pred, session_bindings) {
                Ok(Term::Atom(Bool(true))) => reduce(*true_case, session_bindings),
                Ok(Term::Atom(Bool(false))) => reduce(*false_case, session_bindings),
                Ok(_) => panic!(),
                Err(msg) => Err(msg)
            }
        },
        Let(name, value, body) => {
            // for now, same as lambda application except with sugar for self-reference
            // BUT will have to change once types are enforced
            match reduce(fix(name.clone(), *value), session_bindings) {
                Ok(val) => {
                    match body {
                        Some(b) => reduce(unshift_indices(sub_at_index(*b, &val, 0), 1), session_bindings),
                        None => Ok(Let(name, Box::new(val), None))
                    }
                },
                Err(msg) => Err(msg)
            }
        },
        // if not defined, would already have blown up in parse
        Var(n, _) => Ok(session_bindings[session_bindings.len() - n - 1].term.clone()),
        term => Ok(term)
    }
}

fn apply(func: Term, arg: Term, session_bindings: &[LetBinding]) -> Result<Term, String> {
    let type_err = String::from("Type error");
    match func {
        Atom(a) => {
            match a {
                BuiltIn(op) => Ok(App(Box::new(Atom(BuiltIn(op))), Box::new(arg))),
                Int(_) | Bool(_) | Unit => Err(type_err)
            }
        },
        App(f, g) => {
            match (*f, *g, arg) {
                (Atom(BuiltIn(op)), Atom(Int(m)), Atom(Int(n))) => Ok(Atom(
                    match op {
                        Add => Int(m + n),
                        Sub => Int(m - n),
                        Mul => Int(m * n),
                        Div => Int(m / n),
                        Mod => Int(m % n),
                        Exp => Int(m ^ n),
                        Eql => Bool(m == n),
                        Gt  => Bool(m > n),
                        Lt  => Bool(m < n),
                })),
                _ => Err(type_err)
            }
        },
        Lambda(body, _) => {
            reduce(unshift_indices(sub_at_index(*body, &arg, 0), 1), session_bindings)
        },
        // func is already reduced, so should not be in any of these forms
        Conditional(_, _, _) | Var(_, _) | Let(_, _, _) | Constructor(_, _) | Sum(_, _) => {
            Err(type_err)
        }
    }
}

// no evaluation, so can't go wrong
fn sub_at_index(body: Term, t: &Term, index: usize) -> Term {
    match body {
        Atom(a) => Atom(a),
        Constructor(n, constructor) => Constructor(n, constructor),
        Sum(constructor, value) => Sum(constructor, value),
        App(a, b) => {
            let subbed_a = sub_at_index(*a, t, index);
            let subbed_b = sub_at_index(*b, t, index);
            App(Box::new(subbed_a), Box::new(subbed_b))
        },
        Lambda(lambda_body, s) => {
            let new_lambda_body = sub_at_index(*lambda_body, t, index + 1);
            Lambda(Box::new(new_lambda_body), s)
        },
        Var(n, s) => {
            if n == index {
                shift_indices(t, index, 0)
            } else { Var(n, s) }
        },
        Conditional(pred, true_case, false_case) => {
            let subbed_pred = sub_at_index(*pred, t, index);
            let subbed_true_case = sub_at_index(*true_case, t, index);
            let subbed_false_case = sub_at_index(*false_case, t, index);
            Conditional(Box::new(subbed_pred), Box::new(subbed_true_case), Box::new(subbed_false_case))
        },
        Let(name, value, let_body) => {
            // increment index in both value and body, since let binding applies in both
            let subbed_value = sub_at_index(*value, t, index + 1);
            let subbed_let_body = let_body.map(|b| Box::new(sub_at_index(*b, t, index + 1)));
            Let(name, Box::new(subbed_value), subbed_let_body)
        }
    }
}

fn shift_indices(term: &Term, distance: usize, cutoff: usize) -> Term {
    match *term {
        Atom(ref a) => Atom(a.clone()),
        Constructor(ref n, ref constructor) => Constructor(*n, constructor.clone()),
        Sum(ref constructor, ref value) => Sum(constructor.clone(), value.clone()),
        App(ref a, ref b) => {
            let a_ = shift_indices(a, distance, cutoff);
            let b_ = shift_indices(b, distance, cutoff);
            App(Box::new(a_), Box::new(b_))
        }, Lambda(ref lambda_body, ref s) => {
            Lambda(Box::new(shift_indices(lambda_body, distance, cutoff + 1)), s.clone())
        },
        Var(ref n, ref s) => {
            let m = if *n >= cutoff { *n + 1 } else { *n };
            Var(m, s.clone())
        },
        Conditional(ref pred, ref true_case, ref false_case) => {
            let shifted_pred = shift_indices(pred, distance, cutoff);
            let shifted_true_case = shift_indices(true_case, distance, cutoff);
            let shifted_false_case = shift_indices(false_case, distance, cutoff);
            Conditional(Box::new(shifted_pred), Box::new(shifted_true_case), Box::new(shifted_false_case))
        },
        Let(ref name, ref value, ref body) => {
            let shifted_value = shift_indices(value, distance, cutoff + 1);
            let shifted_body = body.clone().map(|b| Box::new(shift_indices(&b, distance, cutoff + 1)));
            Let(name.clone(), Box::new(shifted_value), shifted_body)
        }
    }
}

fn unshift_indices(term: Term, cutoff: usize) -> Term {
    match term {
        Atom(a) => Atom(a),
        Constructor(n, constructor) => Constructor(n, constructor),
        Sum(constructor, value) => Sum(constructor, value),
        App(a, b) => {
            let a_ = unshift_indices(*a, cutoff);
            let b_ = unshift_indices(*b, cutoff);
            App(Box::new(a_), Box::new(b_))
        },
        Lambda(lambda_body, s) => {
            Lambda(Box::new(unshift_indices(*lambda_body, cutoff + 1)), s)
        },
        Var(n, s) => {
            if n >= cutoff {
                Var(n-1, s)
            } else { Var(n, s) }
        },
        Conditional(pred, true_case, false_case) => {
            let unshifted_pred = unshift_indices(*pred, cutoff);
            let unshifted_true_case = unshift_indices(*true_case, cutoff);
            let unshifted_false_case = unshift_indices(*false_case, cutoff);
            Conditional(Box::new(unshifted_pred), Box::new(unshifted_true_case), Box::new(unshifted_false_case))
        },
        Let(name, value, body) => {
            let unshifted_value = unshift_indices(*value, cutoff + 1);
            let unshifted_body = body.map(|b| Box::new(unshift_indices(*b, cutoff + 1)));
            Let(name, Box::new(unshifted_value), unshifted_body)
        }
    }
}


fn fix(name: String, value: Term) -> Term {
    let expr = "(\\f -> (\\x -> f (\\y -> x x y)) (\\x -> f (\\y -> x x y)))";
    match build_lexer(expr) {
        TokenStream::Expr(mut fix_toks) => {
            let y = parse_expr(&mut fix_toks, &mut vec![], &SumTypeDefs::new()).unwrap();
            App(Box::new(y), Box::new(Lambda(Box::new(value), name)))
        },
        _ => panic!()
    }
}
