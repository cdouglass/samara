mod tokenize;
use self::tokenize::build_lexer;

mod types;
use self::types::Atom::*;
use self::types::Op::*;
use self::types::Term;
use self::types::Term::*;

mod parse;
use self::parse::parse;

#[cfg(test)]
mod tests;

pub fn evaluate(expr: &str) -> Result<Term, String> {
    let mut tokens = build_lexer(expr.trim());
    parse(&mut tokens).and_then(reduce)
}

fn reduce(ast: Term) -> Result<Term, String> {
    match ast {
        App(func, arg) => {
            let f = reduce(*func);
            let a = reduce(*arg);
            match (f, a) {
                (Ok(f), Ok(a)) => apply(f, a),
                _ => Err(String::from("Type error"))
            }
        },
        Atom(a) => {
            Ok(Atom(a))
        },
        Lambda(body, s) => {
            Ok(Lambda(Box::new(*body), s))
        },
        Var(n, s) => {
            Ok(Var(n, s))
        }
    }
}

fn apply(func: Term, arg: Term) -> Result<Term, String> {
    let type_err = String::from("Type error");
    match func {
        Atom(a) => {
            match a {
                BuiltIn(op) => Ok(App(Box::new(Atom(BuiltIn(op))), Box::new(arg))),
                Int(_) => Err(type_err),
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
                })),
                _ => Err(type_err)
            }
        },
        Lambda(body, _) => {
            reduce(unshift_indices(sub_at_index(*body, arg.clone(), 0), 1))
        },
        Var(_, _) => Err(type_err)
    }
}

// no evaluation, so can't go wrong
fn sub_at_index(body: Term, t: Term, index: usize) -> Term {
    match body {
        App(a, b) => {
            let subbed_a = sub_at_index(*a, t.clone(), index);
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
        Atom(a) => Atom(a)
    }
}

fn shift_indices(term: Term, distance: usize, cutoff: usize) -> Term {
    match term {
        App(a, b) => {
            let a_ = shift_indices(*a, distance, cutoff);
            let b_ = shift_indices(*b, distance, cutoff);
            App(Box::new(a_), Box::new(b_))
        }, Lambda(lambda_body, s) => {
            Lambda(Box::new(shift_indices(*lambda_body, distance, cutoff + 1)), s)
        },
        Var(n, s) => {
            if n >= cutoff {
                Var(n+1, s)
            } else { Var(n, s) }
        },
        Atom(a) => Atom(a)
    }
}

fn unshift_indices(term: Term, cutoff: usize) -> Term {
    match term {
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
        Atom(a) => Atom(a)
    }
}
