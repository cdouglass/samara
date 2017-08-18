use interpret::types::Atom;
use interpret::types::Op;
use interpret::types::Term;
use interpret::types::Type;
use interpret::types::Type::*;

pub fn infer_type(term: &Term) -> Result<Type, String> {
    match *term {
        Term::Atom(ref atom) => Ok(base_type(atom)),
        Term::App(ref func, ref arg) => {
            match (infer_type(func), infer_type(arg)) {
                (Ok(Arrow(t1, t2)), Ok(t3)) => unify(*t1, t3).map(|_| *t2.clone()),
                (Ok(t1), Ok(t2)) => Err(String::from(format!("Type error: {:?} is not a function and cannot accept an argument {:?}", t1, t2))),
                (Err(msg), _) | (_, Err(msg)) => Err(msg)
            }
        },
        Term::Conditional(ref pred, ref true_case, ref false_case) => {
            infer_type(pred)
                .and_then(|t| unify(t, Bool).map_err(|_| String::from(format!("Type error: {:?} is not a boolean and cannot be the predicate for a conditional", pred))))
                .and_then(|_|
                    match (infer_type(true_case), infer_type(false_case)) {
                        (Ok(t1), Ok(t2)) => unify(t1, t2),
                        (Err(msg), _) | (_, Err(msg)) => Err(msg)
                    })
        },
        Term::Lambda(_, _) |
            Term::Var(_, _) |
            Term::Let(_, _, _) |
            Term::SessionVar(_) => Err(String::from("Not implemented: unification")),
    }
}

fn arrow(t1: Type, t2: Type) -> Type {
    Arrow(Box::new(t1), Box::new(t2))
}

fn base_type(atom: &Atom) -> Type {
    match *atom {
        Atom::Bool(_) => Bool,
        Atom::Int(_) => Int,
        Atom::BuiltIn(ref op) => match *op {
            Op::Eql | Op::Gt | Op::Lt => arrow(Int, arrow(Int, Bool)),
            _ => arrow(Int, arrow(Int, Int))
        }
    }
}

fn unify(t1: Type, t2: Type) -> Result<Type, String> {
    if t1 == t2 {
        Ok(t1)
    } else {
        Err(String::from("Not implemented: unification"))
    }
}
