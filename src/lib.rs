pub mod ui;

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
pub use self::structures::sums::SumType;
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
        Ok(term) => {
            let typ = infer_type(&term, bindings, &mut gen, sum_types);
            (Ok(term), typ)
        },
        Err(msg) => (Err(msg), Err(String::from("Syntax error")))
    }
}

pub fn declare_sum_type(decl: &str, sum_types: &mut SumTypeDefs) -> Result<SumType, String> {
    match build_lexer_decl(decl) {
        TokenStream::Decl(mut tokens) => {
            let sum_type = parse_decl(&mut tokens, sum_types)?;
            sum_types.add_type(&sum_type.name, sum_type.variants.clone(), sum_type.params.clone())?;
            Ok(sum_type)
        },
        _ => panic!()
    }
}

fn parse_from_str(expr: &str, bindings: &[LetBinding], sum_types: &SumTypeDefs) -> Result<Term, String> {
    let mut identifiers: Vec<String> = bindings.iter().map(|x| x.name.clone()).collect();
    match build_lexer(expr.trim()) {
        TokenStream::Expr(mut tokens) => parse_expr(&mut tokens, &mut identifiers, sum_types),
        _ => panic!()
    }
}

pub fn evaluate(expr: &str, session_bindings: &mut Vec<LetBinding>, gen: &mut GenTypeVar, sum_types: &SumTypeDefs) -> Result<Term, String> {
    match type_of(expr, session_bindings, gen, sum_types) {
        (Err(msg), _) | (_, Err(msg)) => Err(msg),
        (Ok(ast), Ok(typ)) => {
            let term = reduce(ast, session_bindings, sum_types)?;

            if let Let(s, value, None) = term {
                session_bindings.push(LetBinding{name: s, term: *value.clone(), typ: typ});
                Ok(*value)
            } else {
                Ok(term)
            }
        }
    }
}

fn reduce(ast: Term, session_bindings: &[LetBinding], sum_types: &SumTypeDefs) -> Result<Term, String> {
    match ast {
        App(func, arg) => {
            let f = reduce(*func, session_bindings, sum_types);
            let a = reduce(*arg, session_bindings, sum_types);
            match (f, a) {
                (Ok(f), Ok(a)) => apply(f, a, session_bindings, sum_types),
                _ => Err(String::from("Type error"))
            }
        },
        Conditional(pred, true_case, false_case) => {
            match reduce(*pred, session_bindings, sum_types) {
                Ok(Term::Atom(Bool(true))) => reduce(*true_case, session_bindings, sum_types),
                Ok(Term::Atom(Bool(false))) => reduce(*false_case, session_bindings, sum_types),
                Ok(_) => panic!(),
                Err(msg) => Err(msg)
            }
        },
        Let(name, value, body) => {
            match reduce(fix(name.clone(), *value), session_bindings, sum_types) {
                Ok(val) => {
                    match body {
                        Some(b) => reduce(unshift_indices(sub_at_index(*b, &val, 0), 1), session_bindings, sum_types),
                        None => Ok(Let(name, Box::new(val), None))
                    }
                },
                Err(msg) => Err(msg)
            }
        },
        // if not defined, would already have blown up in parse
        Var(n, _) => Ok(session_bindings[session_bindings.len() - n - 1].term.clone()),
        Sum(n, constructor, values) => {
            let mut new_values = vec![];
            for val in values {
                let new_val = reduce(val, session_bindings, sum_types)?;
                new_values.push(new_val);
            }
            Ok(Sum(n, constructor, new_values))
        }
        Case(arg, cases, default) => {
            let reduced_arg = reduce(*arg, session_bindings, sum_types)?;
            for (pattern, arm) in cases {
                if let Some(values) = pattern.match_term(&reduced_arg) {
                    let f = |acc, val: &Term| { unshift_indices(sub_at_index(acc, val, 0), 1) };
                    let subbed_arm = values.iter().rev().fold(arm, f);
                    return reduce(subbed_arm, session_bindings, sum_types);
                }
            }
            reduce(*default, session_bindings, sum_types)
        },
        term => Ok(term)
    }
}

fn apply(func: Term, arg: Term, session_bindings: &[LetBinding], sum_types: &SumTypeDefs) -> Result<Term, String> {
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
                        Eql => Bool(m == n),
                        Gt  => Bool(m > n),
                        Lt  => Bool(m < n),
                })),
                _ => Err(type_err)
            }
        },
        Lambda(body, _) => {
            reduce(unshift_indices(sub_at_index(*body, &arg, 0), 1), session_bindings, sum_types)
        },
        // func is already reduced, so should not be in any of these forms
        Conditional(_, _, _) | Var(_, _) | Let(_, _, _) | Sum(_, _, _) | Case(_, _, _) => {
            Err(type_err)
        }
    }
}

// no evaluation, so can't go wrong
fn sub_at_index(body: Term, t: &Term, index: usize) -> Term {
    match body {
        Atom(a) => Atom(a),
        Sum(n, constructor, values) => {
            let subbed_values = values.into_iter().map(|val| sub_at_index(val, t, index)).collect();
            Sum(n, constructor, subbed_values)
        },
        Case(arg, cases, default) => {
            let new_arg = sub_at_index(*arg, t, index);
            let new_default = sub_at_index(*default, t, index);

            let mut new_cases = vec![];
            for (pattern, arm) in cases {
                let new_arm = sub_at_index(arm, t, index + 1);
                new_cases.push((pattern, new_arm));
            }
            Case(Box::new(new_arg), new_cases, Box::new(new_default))
        },
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
                shift_indices(t.clone(), index, 0)
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

fn shift_indices(term: Term, distance: usize, cutoff: usize) -> Term {
    match term {
        Atom(a) => Atom(a),
        Sum(n, constructor, values) => Sum(n, constructor, values),
        Case(arg, cases, default) => {
            let new_arg = shift_indices(*arg, distance, cutoff);
            let new_default = shift_indices(*default, distance, cutoff);
            let mut new_cases = vec![];
            for (pattern, arm) in cases {
                let new_arm = shift_indices(arm, distance, cutoff + 1);
                new_cases.push((pattern, new_arm));
            }
            Case(Box::new(new_arg), new_cases, Box::new(new_default))
        },
        App(a, b) => {
            let a_ = shift_indices(*a, distance, cutoff);
            let b_ = shift_indices(*b, distance, cutoff);
            App(Box::new(a_), Box::new(b_))
        }, Lambda(lambda_body, s) => {
            Lambda(Box::new(shift_indices(*lambda_body, distance, cutoff + 1)), s)
        },
        Var(n, s) => {
            let m = if n >= cutoff { n + 1 } else { n };
            Var(m, s)
        },
        Conditional(pred, true_case, false_case) => {
            let shifted_pred = shift_indices(*pred, distance, cutoff);
            let shifted_true_case = shift_indices(*true_case, distance, cutoff);
            let shifted_false_case = shift_indices(*false_case, distance, cutoff);
            Conditional(Box::new(shifted_pred), Box::new(shifted_true_case), Box::new(shifted_false_case))
        },
        Let(name, value, body) => {
            let shifted_value = shift_indices(*value, distance, cutoff + 1);
            let shifted_body = body.map(|b| Box::new(shift_indices(*b, distance, cutoff + 1)));
            Let(name, Box::new(shifted_value), shifted_body)
        }
    }
}

fn unshift_indices(term: Term, cutoff: usize) -> Term {
    match term {
        Atom(a) => Atom(a),
        Sum(n, constructor, values) => {
            let new_values = values.into_iter().map(|val| unshift_indices(val, cutoff)).collect();
            Sum(n, constructor, new_values)
        },
        Case(arg, cases, default) => {
            let new_arg = unshift_indices(*arg, cutoff);
            let new_default = unshift_indices(*default, cutoff);
            let mut new_cases = vec![];
            for (pattern, arm) in cases {
                let new_arm = unshift_indices(arm, cutoff + 1);
                new_cases.push((pattern, new_arm));
            }
            Case(Box::new(new_arg), new_cases, Box::new(new_default))
        },
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
