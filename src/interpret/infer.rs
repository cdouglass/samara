use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::Iterator;

use interpret::SumTypeDefs;
use interpret::structures::arrow;
use interpret::structures::Atom;
use interpret::structures::LetBinding;
use interpret::structures::Op;
use interpret::structures::Term;
use interpret::structures::Type;
use interpret::structures::Type::*;

pub fn infer_type(term: &Term, bindings: &[LetBinding], mut gen: &mut GenTypeVar, sum_types: &SumTypeDefs) -> Result<Type, String> {
    let mut context = vec![];

    for b in bindings {
        let universals = type_vars_free_in(&b.typ);
        context.push((b.typ.clone(), universals));
    }

    let (mut typ, constraints) = get_constraints(term, &mut context, &mut gen, sum_types)?;
    let substitution = unify(constraints)?;
    apply_substitution(&substitution, &mut typ);
    Ok(typ)
}

//TODO move to structures module
pub fn apply_substitution(substitution: &HashMap<usize, Type>, typ: &mut Type) {
    match *typ {
        TypeVar(n) => {
            if let Some(t) = substitution.get(&n) {
                    *typ = t.clone();
            }
        },
        Arrow(ref mut left, ref mut right) => {
            apply_substitution(substitution, left);
            apply_substitution(substitution, right);
        },
        _ => { }
    }
}

fn instantiate(mut typ: Type, universals: &HashSet<usize>, mut gen: &mut GenTypeVar) -> Type {
    let mut sub = HashMap::new();
    for k in universals.iter() {
        sub.insert(*k, gen.next().unwrap());
    }
    apply_substitution(&sub, &mut typ);
    typ
}

fn get_constraints(term: &Term, mut context: &mut Vec<(Type, HashSet<usize>)>, mut gen: &mut GenTypeVar, sum_types: &SumTypeDefs) -> Result<(Type, Vec<(Type, Type)>), String> {
    match *term {
        Term::Atom(ref atom) => Ok((base_type(atom), vec![])),
        Term::App(ref left, ref right) => {
            let (left_type, left_constraints) = get_constraints(left, &mut context, gen, sum_types)?;
            let (right_type, right_constraints) = get_constraints(right, &mut context, gen, sum_types)?;
            let a = gen.next().unwrap();
            let b = gen.next().unwrap();
            let mut constraints = left_constraints;
            constraints.extend(right_constraints);
            constraints.push((arrow(a.clone(), b.clone()), left_type));
            constraints.push((a, right_type));

            Ok((b, constraints))
        },
        Term::Lambda(ref body, _) => {
            context.push((gen.next().unwrap(), HashSet::new()));
            let (body_type, body_constraints) = get_constraints(body, &mut context, gen, sum_types)?;
            Ok((arrow(context.pop().unwrap().0, body_type), body_constraints))
        },
        Term::Var(ref n, _) => {
            let (ref typ, ref universals) = context[context.len() - n - 1];
            let fresh_typ = instantiate(typ.clone(), universals, gen);
            Ok((fresh_typ, vec![]))
        },
        Term::Conditional(ref pred, ref true_case, ref false_case) => {
            let (pred_type, pred_constraints) = get_constraints(pred, &mut context, gen, sum_types)?;
            let (true_type, true_constraints) = get_constraints(true_case, &mut context, gen, sum_types)?;
            let (false_type, false_constraints) = get_constraints(false_case, &mut context, gen, sum_types)?;
            let mut constraints = pred_constraints;
            constraints.extend(true_constraints);
            constraints.extend(false_constraints);
            constraints.push((pred_type, Bool));
            constraints.push((true_type.clone(), false_type));

            Ok((true_type, constraints))
        },
        Term::Let(_, ref value, ref body) => {
            context.push((gen.next().unwrap(), HashSet::new()));
            match *body {
                None =>  {
                    let (value_type, value_constraints) = get_constraints(value, &mut context, gen, sum_types)?;
                    Ok((value_type, value_constraints))
                },
                Some(ref b) => {
                    let (mut value_type, value_constraints) = get_constraints(value, &mut context, gen, sum_types)?;
                    let value_sub = unify(value_constraints.clone())?;
                    apply_substitution(&value_sub, &mut value_type);
                    let universals = type_vars_free_in(&value_type);

                    context.push((value_type, universals));
                    let result = get_constraints(b, &mut context, gen, sum_types)?;
                    context.pop();
                    Ok(result)
                }
            }
        },
        Term::Constructor(ref n, _) => {
            let ref binding = sum_types.bindings[*n];
            Ok((binding.typ.clone(), vec![]))
        },
        Term::Sum(ref constructor, ref value) => {
            let (ref type_scheme, ref input_typ) = sum_types.type_info(constructor.clone())?;
            let (value_type, value_constraints) = get_constraints(value, &mut context, gen, sum_types)?;
            Ok((Unit, vec![]))
                //TODO
        }
    }
}

fn unify(mut constraints: Vec<(Type, Type)>) -> Result<HashMap<usize, Type>, String> {

    fn match_constraint_with_variable(t1: &Type, t2: &Type) -> Option<(usize, Type)> {
        if let TypeVar(n) = *t1 {
            if !occurs_in(n, t2) { return Some((n, t2.clone())); }
        }
        if let TypeVar(n) = *t2 {
            if !occurs_in(n, t1) { return Some((n, t1.clone())); }
        }
        None
    }

    match constraints.pop() {
        Some((s, t)) => {
            if s == t {
                unify(constraints)
            } else if let Some((n, typ)) = match_constraint_with_variable(&s, &t) {
                let mut sub = HashMap::new();
                sub.insert(n, typ.clone());

                for &mut (ref mut k, ref mut v) in &mut constraints {
                    apply_substitution(&sub, k);
                    apply_substitution(&sub, v);
                };

                let mut substitution = unify(constraints)?;
                insert_sub(&mut substitution, n, typ);
                Ok(substitution)
            } else if let (Arrow(s1, s2), Arrow(t1, t2)) = (s.clone(), t.clone()) {
                constraints.push((*s1, *t1));
                constraints.push((*s2, *t2));
                unify(constraints)
            } else {
                Err(String::from(format!("Type error: {:?} != {:?}", s, t)))
            }
        },
        None => Ok(HashMap::new())
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

fn insert_sub(mut sub: &mut HashMap<usize, Type>, key: usize, mut value: Type) {
    apply_substitution(sub, &mut value);
    sub.insert(key, value);
}

/* Helpers for constraints */

pub struct GenTypeVar {
    pub n: usize
}

impl Iterator for GenTypeVar {
    type Item = Type;
    fn next(&mut self) -> Option<Type> {
        self.n += 1;
        Some(TypeVar(self.n))
    }
}

impl GenTypeVar {
    pub fn new() -> GenTypeVar {
        GenTypeVar{n: 0}
    }
}

fn type_vars_free_in(typ: &Type) -> HashSet<usize> {
    let mut tvars = HashSet::new();
    match *typ {
        TypeVar(n) => {
            tvars.insert(n);
        },
        Arrow(ref t1, ref t2) => {
            tvars.extend(type_vars_free_in(t1));
            tvars.extend(type_vars_free_in(t2));
        },
        _ => { }
    }
    tvars
}

fn base_type(atom: &Atom) -> Type {
    match *atom {
        Atom::Unit => Unit,
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
    use interpret::evaluate;
    use interpret::SumTypeDefs;
    use self::LetBinding;
    use self::Op::*;

    const FIVE : Term = Term::Atom(Atom::Int(5));

    fn apply(func: Term, arg: Term) -> Term {
        Term::App(Box::new(func), Box::new(arg))
    }

    fn assert_type_with_context(expr: &Term, t: &Type, bindings: &[LetBinding], gen: &mut GenTypeVar, sum_types: &SumTypeDefs) {
        match infer_type(expr, bindings, gen, sum_types) {
            Ok(t1) => assert_eq!(t1, *t),
            Err(msg) => {
                println!("Expected type {:?} but got error {:?}", t, msg);
                panic!()
            }
        }
    }

    fn assert_type_err_with_context(expr: &Term, expected: &str, bindings: &[LetBinding], gen: &mut GenTypeVar, sum_types: &SumTypeDefs) {
        match infer_type(expr, bindings, gen, sum_types) {
            Ok(t1) => {
                println!("Expected type error {:?} but got type {:?}", expected, t1);
                panic!()
            },
            Err(msg) => assert_eq!(&msg, expected)
        }
    }

    fn assert_type(expr: &Term, t: &Type) {
        let bindings = vec![];
        let mut gen = GenTypeVar::new();
        let sum_types = SumTypeDefs::new();
        match infer_type(expr, &bindings, &mut gen, &sum_types) {
            Ok(t1) => assert_eq!(t1, *t),
            Err(msg) => {
                println!("Expected type {:?} but got error {:?}", t, msg);
                panic!()
            }
        }
    }

    fn assert_type_err(expr: &Term, s: &str) {
        let bindings = vec![];
        let mut gen = GenTypeVar::new();
        let sum_types = SumTypeDefs::new();
        match infer_type(expr, &bindings, &mut gen, &sum_types) {
            Err(msg) => assert_eq!(&msg, s),
            Ok(t) => {
                println!("Expected type error {:?} but got type {:?}", s, t);
                panic!()
            }
        }
    }

    fn unit() -> Term {
        Term::Atom(Atom::Unit)
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
        for &(ref term, ref typ) in [(int_to_term(5), Int), (bool_to_term(false), Bool), (op_to_term(Add), arrow(Int, arrow(Int, Int))), (op_to_term(Eql), arrow(Int, arrow(Int, Bool))), (unit(), Unit)].iter() {
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
        let untypable = Term::Lambda(Box::new(apply(x.clone(), x)), String::from("x"));
        assert_type_err(&untypable, "Type error: t1 -> t3 != t1");
    }

    #[test]
    fn test_valid_conditional() {
        let cond = Term::Conditional(Box::new(bool_to_term(true)), Box::new(FIVE), Box::new(int_to_term(4)));
        assert_type(&cond, &Int);
    }

    #[test]
    fn test_conditional_with_invalid_predicate() {
        let cond = Term::Conditional(Box::new(int_to_term(5)), Box::new(FIVE), Box::new(int_to_term(4)));
        assert_type_err(&cond, "Type error: Int != Bool");
    }

    #[test]
    fn test_conditional_with_mismatched_arms() {
        let cond = Term::Conditional(Box::new(bool_to_term(false)), Box::new(FIVE), Box::new(bool_to_term(true)));
        assert_type_err(&cond, "Type error: Int != Bool");
    }

    #[test]
    fn test_polymorphic_let() {
        let id = Term::Lambda(Box::new(Term::Var(0, String::from("x"))), String::from("x"));
        let v = Term::Var(0, String::from("id"));
        let poly = apply(apply(v.clone(), op_to_term(Gt)), apply(v, FIVE));
        let term = Term::Let(String::from("id"), Box::new(id), Some(Box::new(poly)));
        assert_type(&term, &arrow(Int, Bool));
    }

    #[test]
    fn test_ill_typed_let_value_not_used_in_body() {
        let invalid = apply(op_to_term(Add), bool_to_term(false));
        let term = Term::Let(String::from("invalid"), Box::new(invalid), Some(Box::new(FIVE)));
        assert_type_err(&term, "Type error: Bool != Int");
    }

    #[test]
    fn test_fresh_instantiation() {
        let mut bindings = vec![];
        let mut gen = GenTypeVar::new();
        let mut sum_types = SumTypeDefs::new();
        evaluate("let id = (\\x -> x)", &mut bindings, &mut gen, &sum_types).unwrap();
        let x = Term::Var(0, String::new());
        assert_type_with_context(&x, &arrow(TypeVar(3), TypeVar(3)), &bindings, &mut gen, &sum_types);
        assert_type_with_context(&x, &arrow(TypeVar(4), TypeVar(4)), &bindings, &mut gen, &sum_types);
        assert_type_with_context(&x, &arrow(TypeVar(5), TypeVar(5)), &bindings, &mut gen, &sum_types);
    }

    #[test]
    fn test_nullary_constructor() {
        use interpret::SumTypeDefs;
        use interpret::structures::sums::SumType;
        let mut gen = GenTypeVar::new();
        let mut sum_types = SumTypeDefs::new();
        let mut variants = vec![(String::from("Foo"), Unit)];
        sum_types.add_type("Baz", variants, vec![]);

        let term = Term::Constructor(0, String::from("Foo"));
        let typ = Sum(SumType::new("Baz", vec![(String::from("Foo"), Unit)], vec![]));
        assert_type_with_context(&term, &typ, &vec![], &mut gen, &sum_types);

        let invalid_0 = Term::Sum(String::from("Foo"), Box::new(FIVE));
        let expected = format!("Type error: {:?} != Int -> t1", typ);
        //TODO
        //assert_type_err_with_context(&invalid_0, &expected, &vec![], &mut gen, &sum_types);

        let invalid_1 = Term::App(Box::new(Term::Constructor(0, String::from("Foo"))), Box::new(FIVE));
        let expected = format!("Type error: Int -> t2 != {:?}", typ);
        assert_type_err_with_context(&invalid_1, &expected, &vec![], &mut gen, &sum_types);
    }

    #[test]
    fn test_unary_constructor() {
        //TODO alone
        //TODO with argument
    }
}
