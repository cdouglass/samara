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
            context.push(gen.next().unwrap()); // argument
            let (body_type, body_constraints) = get_constraints(body, &mut context, gen);
            (arrow(context.pop().unwrap(), body_type), body_constraints)
        },
        Term::Var(ref n, _) => {
            let mut stack = context.iter().rev();
            (stack.nth(*n).unwrap().clone(), vec![])
        },
        _ => panic!()
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

fn unify(mut constraints: &mut Vec<(Type, Type)>) -> Result<HashMap<usize, Type>, String> {
    match constraints.pop() {
        Some((s, t)) => {
            if s == t {
                return unify(constraints);
            }

            match s {
                //TODO need a test case where the occurs check will fail
                TypeVar(n) => { if !occurs_in(n, &t) {
                        let mut sub = HashMap::new();
                        sub.insert(n, t.clone());

                        let mut new_constraints = vec![];
                        for &mut(ref k, ref v) in constraints {
                            new_constraints.push((k.clone(), apply_substitution(&sub, v.clone())));
                        };

                        //TODO handle error
                        let mut substitution = unify(&mut new_constraints).unwrap();
                        for val in substitution.values_mut() {
                            *val = apply_substitution(&sub, val.clone());
                        }
                        substitution.insert(n, t);
                        return Ok(substitution)
                    }
                },
                _ => { }
            }
            // case: both are functions (combine unifications of left and right sides)
            // case: s is single var not in free vars of t
            // case: t is single var not in free vars of s
            // case: anything else [fail]
        },
        None => { }
    }
    let substitution = HashMap::new();
    Ok(substitution)
}

fn occurs_in(n: usize, t: &Type) -> bool {
    match *t {
        TypeVar(m) => m == n,
        Arrow(ref t1, ref t2) => occurs_in(n, t1) || occurs_in(n, t2),
        _ => false
    }
}

// need to apply whole substitution set at once
fn apply_substitution(substitution: &HashMap<usize, Type>, schema: Type) -> Type {
    match schema {
        Bool => Bool,
        Int => Int,
        TypeVar(n) => {
            match substitution.get(&n) {
                Some(t) => t.clone(),
                None => TypeVar(n)
            }
        },
        Arrow(left, right) => {
            arrow(apply_substitution(substitution, *left),
                  apply_substitution(substitution, *right))
        }
    }
}

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
