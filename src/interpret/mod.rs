mod tokenize;
use self::tokenize::build_lexer;

mod types;
use self::types::Atom;
use self::types::Term;

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
        Term::Atom(_, _) => Ok(ast),
        // would only have gotten here if types already validated
        Term::App(func, arg, typ) => {
            let f = reduce(*func);
            let a = reduce(*arg);
            match (f, a) {
                (Ok(Term::Atom(Atom::Func1(f), _)), Ok(Term::Atom(Atom::Int(n), _))) => Ok(Term::Atom(Atom::Int((*f)(n)), typ)),
                (Ok(Term::Atom(Atom::Func2(f), _)), Ok(Term::Atom(Atom::Int(n), _))) => Ok(Term::Atom(Atom::Func1((*f)(n)), typ)),
                _ => Err(String::from("Not implemented"))
            }
        }
    }
}
