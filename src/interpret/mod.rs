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
        a => Ok(a)
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
        Lambda(_, _) => {
            Err(String::from("Not implemented: Lambda application"))
        }
        Var(_, _) => Err(type_err)
    }
}
