use std::collections::HashMap;
use std::iter::Iterator;

use interpret::types::Atom;
use interpret::types::Op;
use interpret::types::Term;
use interpret::types::Type;
use interpret::types::Type::*;

pub fn infer_type(term: &Term) -> Result<Type, String> {
    let mut gen = GenTypeVar{n: 0};
    let mut context = vec![];
    let (schema, mut constraints) = get_constraints(term, &mut context, &mut gen);
    match unify(&mut constraints) {
        Ok(substitution) => Ok(apply_substitution(&substitution, schema)),
        Err(msg) => Err(msg)
    }
}

fn apply_substitution(substitution: &HashMap<usize, Type>, schema: Type) -> Type {
    match schema {
        TypeVar(n) => {
            match substitution.get(&n) {
                Some(t) => t.clone(),
                None => TypeVar(n)
            }
        },
        Arrow(left, right) => {
            arrow(apply_substitution(substitution, *left),
                  apply_substitution(substitution, *right))
        },
        t => t
    }
}

// (type schema, vector of constraints )
// constraint: left type in pair is to be made equal to right by applying appropriate substitution
fn get_constraints(term: &Term, mut context: &mut Vec<Type>, mut gen: &mut GenTypeVar) -> (Type, Vec<(Type, Type)>) {
    match *term {
        Term::Atom(ref atom) => (base_type(atom), vec![]),
        Term::App(ref left, ref right) => {
            let (left_type, left_constraints) = get_constraints(left, &mut context, gen);
            let (right_type, right_constraints) = get_constraints(right, &mut context, gen);
            let a = gen.next().unwrap();
            let b = gen.next().unwrap();
            let mut constraints = left_constraints;
            constraints.extend(right_constraints);
            constraints.push((arrow(a.clone(), b.clone()), left_type));
            constraints.push((a, right_type));

            (b, constraints)
        },
        Term::Lambda(ref body, _) => {
            context.push(gen.next().unwrap());
            let (body_type, body_constraints) = get_constraints(body, &mut context, gen);
            (arrow(context.pop().unwrap(), body_type), body_constraints)
        },
        Term::Var(ref n, _) => {
            let mut stack = context.iter().rev();
            (stack.nth(*n).unwrap().clone(), vec![])
        },
        Term::Conditional(_, _, _) | Term::Let(_, _, _) | Term::SessionVar(_) => panic!()
    }
}

fn unify(mut constraints: &mut Vec<(Type, Type)>) -> Result<HashMap<usize, Type>, String> {
    match constraints.pop() {
        Some((s, t)) => {
            if s == t {
                unify(constraints)
            } else if let Some((n, typ)) = match_constraint_with_variable(&s, &t) {
                let mut sub = HashMap::new();
                sub.insert(n, typ.clone());

                let mut new_constraints = vec![];
                for &mut(ref k, ref v) in constraints {
                    let k1 = apply_substitution(&sub, k.clone());
                    let v1 = apply_substitution(&sub, v.clone());
                    new_constraints.push((k1, v1));
                };

                let mut result = unify(&mut new_constraints);
                if let Ok(ref mut substitution) = result {
                    insert_sub(substitution, n, typ.clone());
                }
                result
            } else if let (Arrow(s1, s2), Arrow(t1, t2)) = (s.clone(), t.clone()) {
                constraints.push((*s1, *t1));
                constraints.push((*s2, *t2));
                unify(constraints)
            } else {
                Err(String::from(format!("Type error: {:?} != {:?}", s, t)))
            }
        },
        None => {
            let substitution = HashMap::new();
            Ok(substitution)
        }
    }
}

/* Helpers for unification */

fn occurs_in(n: usize, t: &Type) -> bool {
    match *t {
        TypeVar(m) => m == n,
        Arrow(ref t1, ref t2) => occurs_in(n, t1) || occurs_in(n, t2),
        _ => false
    }
}

fn match_constraint_with_variable(t1: &Type, t2: &Type) -> Option<(usize, Type)> {
    if let TypeVar(n) = *t1 {
        if !occurs_in(n, t2) { return Some((n, t2.clone())); }
    }
    if let TypeVar(n) = *t2 {
        if !occurs_in(n, t1) { return Some((n, t1.clone())); }
    }
    None
}

fn insert_sub(mut sub: &mut HashMap<usize, Type>, key: usize, value: Type) {
    let new_val = apply_substitution(sub, value);
    sub.insert(key, new_val);
}

/* Helpers for constraints */

struct GenTypeVar {
    n: usize
}

impl Iterator for GenTypeVar {
    type Item = Type;
    fn next(&mut self) -> Option<Type> {
        self.n += 1;
        Some(TypeVar(self.n))
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

#[cfg(test)]
mod tests {
    use super::*;
    use self::Op::*;

    const FIVE : Term = Term::Atom(Atom::Int(5));

    fn apply(func: Term, arg: Term) -> Term {
        Term::App(Box::new(func.clone()), Box::new(arg.clone()))
    }

    fn assert_type(expr: &Term, t: &Type) {
        match infer_type(expr) {
            Ok(t1) => assert_eq!(t1, *t),
            _ => panic!()
        }
    }

    fn assert_type_err(expr: &Term, s: &str) {
        match infer_type(expr) {
            Err(msg) => assert_eq!(&msg, s),
            _ => panic!()
        }
    }

    fn int_to_term(n: i64) -> Term {
        Term::Atom(Atom::Int(n))
    }

    fn bool_to_term(b: bool) -> Term {
        Term::Atom(Atom::Bool(b))
    }

    fn op_to_term(op: Op) -> Term {
        Term::Atom(Atom::BuiltIn(op))
    }

    /* Test misc helper functions */

    #[test]
    fn test_compose_substitutions() {
        let mut sub1 = HashMap::new();
        sub1.insert(1, Int);
        insert_sub(&mut sub1, 2, TypeVar(1));
        assert_eq!(sub1.get(&2), Some(&Int));
    }

    /* Test type inference */

    #[test]
    fn test_infer_base_types() {
        for &(ref term, ref typ) in [(int_to_term(5), Int), (bool_to_term(false), Bool), (op_to_term(Add), arrow(Int, arrow(Int, Int))), (op_to_term(Eql), arrow(Int, arrow(Int, Bool)))].iter() {
            assert_type(term, typ);
        }
    }

    #[test]
    fn test_application_of_builtins() {
        use self::Op::*;
        let eql_5 = apply(op_to_term(Eql), FIVE);
        assert_type(&eql_5, &arrow(Int, Bool));
        let eql_5_4 = apply(eql_5, int_to_term(4));
        assert_type(&eql_5_4, &Bool);
        let add_5 = apply(op_to_term(Add), FIVE);
        assert_type(&add_5, &arrow(Int, Int));
        let add_5_4 = apply(add_5, int_to_term(4));
        assert_type(&add_5_4, &Int);
    }

    #[test]
    fn test_identity() {
        let id = Term::Lambda(Box::new(Term::Var(0, String::from("x"))), String::from("x"));
        assert_type(&id, &arrow(TypeVar(1), TypeVar(1)));
        let x = apply(id.clone(), FIVE);
        assert_type(&x, &Int);
        let x = apply(id.clone(), bool_to_term(true));
        assert_type(&x, &Bool);
    }

    #[test]
    fn test_type_mismatch() {
        let eql = Term::Atom(Atom::BuiltIn(Op::Eql));
        let tru = Term::Atom(Atom::Bool(true));
        let x = apply(eql, tru);
        assert_type_err(&x, "Type error: Bool != Int");
    }

    #[test]
    fn test_application_of_non_function() {
        let x = apply(FIVE, FIVE);
        assert_type_err(&x, "Type error: Int -> t2 != Int");
    }

    #[test]
    fn test_no_recursive_function_types() {
        let x = Term::Var(0, String::from("x"));
        let untypable = Term::Lambda(Box::new(apply(x.clone(), x.clone())), String::from("x"));
        assert_type_err(&untypable, "Type error: t1 -> t3 != t1");
    }
}
