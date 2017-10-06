use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::Iterator;

use SumTypeDefs;
use structures::arrow;
use structures::Atom;
use structures::LetBinding;
use structures::Op;
use structures::Term;
use structures::Type;
use structures::Type::*;
use structures::patterns::Pattern;
use structures::sums::ConstructorBinding;

pub fn infer_type(term: &Term, bindings: &[LetBinding], mut gen: &mut GenTypeVar, sum_types: &SumTypeDefs) -> Result<Type, String> {
    let mut context = vec![];
    let mut ctor_bindings = vec![];

    for b in bindings {
        let universals = type_vars_free_in(&b.typ);
        context.push((b.typ.clone(), universals));
    }

    for cb in &sum_types.bindings {
        let mut universals = HashSet::new();
        for typ in &cb.result_type.params {
            universals.extend(type_vars_free_in(typ));
        }
        ctor_bindings.push(((*cb).clone(), universals));
    }

    let (mut typ, constraints) = get_constraints(term.clone(), &mut context, &mut gen, ctor_bindings.as_slice())?;
    let substitution = unify(constraints)?;
    apply_substitution(&substitution, &mut typ);
    universalize(&mut typ);
    Ok(typ)
}

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
        Sum(ref mut sum_type) => {
            for mut param in &mut sum_type.params {
                apply_substitution(substitution, &mut param);
            }

            for &mut(_, ref mut arg_types) in &mut sum_type.variants {
                for mut typ in arg_types {
                    apply_substitution(substitution, &mut typ);
                }
            }
        },
        _ => { }
    }
}

fn get_constraints(term: Term, mut context: &mut Vec<(Type, HashSet<usize>)>, gen: &mut GenTypeVar, constructor_bindings: &[(ConstructorBinding, HashSet<usize>)]) -> Result<(Type, Vec<(Type, Type)>), String> {
    match term {
        Term::Atom(ref atom) => Ok((base_type(atom), vec![])),
        Term::App(left, right) => {
            let (left_type, left_constraints) = get_constraints(*left, &mut context, gen, constructor_bindings)?;
            let (right_type, right_constraints) = get_constraints(*right, &mut context, gen, constructor_bindings)?;
            let (a, b) = (gen.next().unwrap(), gen.next().unwrap());
            let mut constraints = left_constraints;
            constraints.extend(right_constraints);
            constraints.push((arrow(a.clone(), b.clone()), left_type));
            constraints.push((a, right_type));

            Ok((b, constraints))
        },
        Term::Lambda(body, _) => {
            context.push((gen.next().unwrap(), HashSet::new()));
            let (body_type, body_constraints) = get_constraints(*body, &mut context, gen, constructor_bindings)?;
            Ok((arrow(context.pop().unwrap().0, body_type), body_constraints))
        },
        Term::Var(ref n, _) => {
            let (ref typ, ref universals) = context[context.len() - n - 1];
            let mut fresh_typ = typ.clone();
            instantiate(&mut fresh_typ, &mut vec![], universals, gen);
            Ok((fresh_typ, vec![]))
        },
        Term::Conditional(pred, true_case, false_case) => {
            let (pred_type, mut constraints) = get_constraints(*pred, &mut context, gen, constructor_bindings)?;
            constraints.push((pred_type, Bool));
            let (true_type, true_constraints) = get_constraints(*true_case, &mut context, gen, constructor_bindings)?;
            constraints.extend(true_constraints);
            let (false_type, false_constraints) = get_constraints(*false_case, &mut context, gen, constructor_bindings)?;
            constraints.extend(false_constraints);
            constraints.push((true_type.clone(), false_type));

            Ok((true_type, constraints))
        },
        Term::Case(arg, cases, default) => {
            let (arg_type, mut constraints) = get_constraints(*arg, &mut context, gen, constructor_bindings)?;
            let (default_type, default_constraints) = get_constraints(*default, &mut context, gen, constructor_bindings)?;
            constraints.extend(default_constraints);

            for (pattern, arm) in cases {
                let (binding_types, pat_type, pat_constraints) = get_pattern_constraints(&pattern, constructor_bindings, gen)?;
                constraints.extend(pat_constraints);
                constraints.push((arg_type.clone(), pat_type));

                let mut counter = vec![]; // silly
                for bt in binding_types {
                    context.push((bt, HashSet::new()));
                    counter.push(0);
                }

                let (arm_type, arm_constraints) = get_constraints(arm, &mut context, gen, constructor_bindings)?;
                constraints.extend(arm_constraints);
                constraints.push((arm_type, default_type.clone()));
                for _ in counter { context.pop(); }
            }

            Ok((default_type, constraints))
        },
        Term::Let(_, value, body) => {
            context.push((gen.next().unwrap(), HashSet::new()));
            match body {
                None => get_constraints(*value, &mut context, gen, constructor_bindings),
                Some(b) => {
                    let (mut value_type, value_constraints) = get_constraints(*value, &mut context, gen, constructor_bindings)?;
                    apply_substitution(&unify(value_constraints)?, &mut value_type);

                    let universals = type_vars_free_in(&value_type);
                    context.push((value_type, universals));
                    let result = get_constraints(*b, &mut context, gen, constructor_bindings);
                    context.pop();
                    result
                }
            }
        },
        Term::Sum(n, c, values) => {
            let (ref cb, ref universals) = constructor_bindings[n];
            let mut constraints = vec![];
            let mut arg_types = cb.arg_types.iter().rev().cloned().collect();
            let mut return_type = Type::Sum(cb.result_type.clone());
            // instantiate both return_type and arg_types at once to preserve relationship
            instantiate(&mut return_type, &mut arg_types, universals, gen);

            for val in values {
                let (value_type, value_constraints) = get_constraints(val, &mut context, gen, constructor_bindings)?;
                constraints.extend(value_constraints);
                match arg_types.pop().clone() {
                    Some(mut arg_type) => {
                        instantiate(&mut arg_type, &mut vec![], universals, gen);
                        constraints.push((value_type, arg_type));
                    },
                    _ => { return Err(format!("Too many arguments given to constructor {}", c)); }
                }
            }

            let typ = arg_types.into_iter().fold(return_type, |acc, t| arrow(t, acc));
            Ok((typ, constraints))
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

    if let Some((s, t)) = constraints.pop() {
        if s == t { return unify(constraints); }
        match (s, t) {
            (Sum(st1), Sum(st2)) => {
                if st1.name == st2.name {
                    for (t1, t2) in st1.params.iter().zip(st2.params.iter()) {
                        constraints.push((t1.clone(), t2.clone()));
                    }
                    unify(constraints)
                } else {
                    Err(String::from(format!("Type error: {} != {}", st1.name, st2.name)))
                }
            },
            (Arrow(s1, s2), Arrow(t1, t2)) => {
                constraints.push((*s1, *t1));
                constraints.push((*s2, *t2));
                unify(constraints)
            },
            (s, t) => {
                if let Some((n, mut typ)) = match_constraint_with_variable(&s, &t) {
                    let sub = vec![(n, typ.clone())].into_iter().collect();

                    for &mut (ref mut k, ref mut v) in &mut constraints {
                        apply_substitution(&sub, k);
                        apply_substitution(&sub, v);
                    };

                    let mut substitution = unify(constraints)?;
                    apply_substitution(&substitution, &mut typ);
                    substitution.insert(n, typ);
                    Ok(substitution)
                } else {
                    Err(String::from(format!("Type error: {:?} != {:?}", s, t)))
                }
            }
        }
    } else { Ok(HashMap::new()) }
}

//(types of bound vars, type of whole pattern, constraints introduced)
fn get_pattern_constraints(pattern: &Pattern, constructor_bindings: &[(ConstructorBinding, HashSet<usize>)], gen: &mut GenTypeVar) -> Result<(Vec<Type>, Type, Vec<(Type, Type)>), String> {
    match *pattern {
        Pattern::Atom(ref atom) => Ok((vec![], base_type(atom), vec![])),
        Pattern::Sum(ref n, _, ref patterns) => {
            let (ref cb, ref universals) = constructor_bindings[*n];
            let mut bound_types = vec![];
            let mut constraints = vec![];
            let mut return_type = Type::Sum(cb.result_type.clone());
            instantiate(&mut return_type, &mut vec![], universals, gen);

            let arg_types = match return_type {
                Type::Sum(ref t) => {
                    t.variants.iter()
                        .find(|variant| variant.0 == cb.tag)
                        .cloned().unwrap().1
                },
                _ => panic!("return_type was explicitly constructed as a sum type but could not be unwrapped. This should never happen.")
            };
            if arg_types.len() != patterns.len() {
                return Err(format!("Constructor {} should have {} argument(s) but instead got {}", cb.tag, arg_types.len(), patterns.len()));
            }

            for (pat, arg_type) in patterns.iter().zip(arg_types.into_iter()) {
                let (bts, inner_type, inner_constraints) = get_pattern_constraints(pat, constructor_bindings, gen)?;
                constraints.extend(inner_constraints);
                constraints.push((arg_type, inner_type));
                bound_types.extend(bts);
            }
            Ok((bound_types, return_type, constraints))
        },
        Pattern::Var(_) => {
            let typ = gen.next().unwrap();
            Ok((vec![typ.clone()], typ, vec![]))
        },
        Pattern::Wildcard => {
            let typ = gen.next().unwrap();
            Ok((vec![], typ, vec![]))
        }
    }
}

/* Helpers for unification etc */

fn occurs_in(n: usize, t: &Type) -> bool {
    match *t {
        TypeVar(m) => m == n,
        Arrow(ref t1, ref t2) => occurs_in(n, t1) || occurs_in(n, t2),
        _ => false
    }
}

// optionally instantiate a whole vector of other types with the same substitution
fn instantiate(typ: &mut Type, types: &mut Vec<Type>, universals: &HashSet<usize>, gen: &mut GenTypeVar) {
    let mut sorted : Vec<usize> = universals.iter().cloned().collect(); // for determinism in tests
    sorted.sort();
    let sub = sorted.into_iter().zip(gen).collect();
    apply_substitution(&sub, typ);
    for x in types.iter_mut() { apply_substitution(&sub, x); }
}

fn universalize(mut typ: &mut Type) {
    // very similar to type_vars_free_in
    fn ordered_type_vars_free_in(typ: &Type) -> Vec<usize> {
        match *typ {
            TypeVar(n) => vec![n],
            Arrow(ref t1, ref t2) => {
                let mut tvars = ordered_type_vars_free_in(t1);
                tvars.append(&mut ordered_type_vars_free_in(t2));
                tvars
            },
            Sum(ref sum) => {
                sum.params.iter().fold(vec![], |mut tvars, param| {
                    tvars.append(&mut ordered_type_vars_free_in(param));
                    tvars
                })
            },
            _ => vec![]
        }
    }

    // unique elements
    let tvars = ordered_type_vars_free_in(typ).into_iter()
        .fold((HashSet::new(), vec![]), |(mut set, mut tvars), n| {
            if !set.contains(&n) { tvars.push(n); }
            set.insert(n);
            (set, tvars)
    }).1;

    let substitution = tvars.iter().enumerate()
        .map(|(i, n)| (*n, TypeVar(i + 1))).collect();

    apply_substitution(&substitution, &mut typ);
}

/* Helpers for constraints */

#[derive(Default)]
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
        TypeVar(n) => { tvars.insert(n); },
        Arrow(ref t1, ref t2) => {
            tvars.extend(type_vars_free_in(t1));
            tvars.extend(type_vars_free_in(t2));
        },
        Sum(ref sum) => {
            for param in &sum.params {
                tvars.extend(type_vars_free_in(param));
            }
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
    use GenTypeVar as GV;
    use SumTypeDefs as SDs;
    use structures::sums::SumType;
    use self::LetBinding;
    use self::Op::*;

    const FIVE : Term = Term::Atom(Atom::Int(5));

    fn assert_type(expr: &Term, t: &Type, bs: &[LetBinding], gen: &mut GV, sum_types: &SDs) {
        match infer_type(expr, bs, gen, sum_types) {
            Ok(t1) => assert_eq!(t1, *t),
            Err(msg) => {
                println!("Expected type {:?} but got error {:?}", t, msg);
                panic!()
            }
        }
    }

    fn assert_type_err(expr: &Term, msg: &str, bs: &[LetBinding], gen: &mut GV, sums: &SDs) {
        match infer_type(expr, bs, gen, sums) {
            Ok(t1) => {
                println!("Expected type error {:?} but got type {:?}", msg, t1);
                panic!()
            },
            Err(err) => assert_eq!(&err, msg)
        }
    }

    fn apply(func: Term, arg: Term) -> Term {
        Term::App(Box::new(func), Box::new(arg))
    }

    fn lambda(body: Term, name: &str) -> Term {
        Term::Lambda(Box::new(body), String::from(name))
    }

    fn cond(pred: Term, tru: Term, fls: Term) -> Term {
        Term::Conditional(Box::new(pred), Box::new(tru), Box::new(fls))
    }

    fn case(arg: Term, arms: Vec<(Pattern, Term)>, default: Term) -> Term {
        Term::Case(Box::new(arg), arms, Box::new(default))
    }

    fn var(n: usize, name: &str) -> Term {
        Term::Var(n, String::from(name))
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
    fn test_instantiate() {
        let mut gen = GV::new();
        let t = arrow(TypeVar(10), TypeVar(10));
        let universals = type_vars_free_in(&t);
        for n in vec![1, 2, 3] {
            let mut t1 = t.clone();
            instantiate(&mut t1, &mut vec![], &universals, &mut gen);
            assert_eq!(&t1, &arrow(TypeVar(n), TypeVar(n)));
        }
    }

    #[test]
    fn test_type_vars_free_in() {
        let (t0, t1) = (TypeVar(5), TypeVar(2));

        for t in vec![Int, Bool, Unit] {
            assert_eq!(type_vars_free_in(&t), HashSet::new());
        }

        let expected = vec![2, 5].into_iter().collect();
        let actual = type_vars_free_in(&arrow(t0.clone(), t1.clone()));
        assert_eq!(actual, expected);

        let variants = vec![
            (String::from("Left"), vec![t0.clone()]),
            (String::from("Right"), vec![t1.clone()])
        ];
        let actual = type_vars_free_in(&Sum(SumType::new("Either", variants, vec![t0, t1])));
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_universalize_type() {
        fn universalized(t: &Type) -> Type {
            let mut typ = t.clone();
            universalize(&mut typ);
            typ
        }

        for t in vec![Unit, Bool, Int] {
            assert_eq!(universalized(&t), t);
        }

        let expected = TypeVar(1);
        let actual = universalized(&TypeVar(5));
        assert_eq!(actual, expected);

        let expected = arrow(TypeVar(1), TypeVar(1));
        let actual = universalized(&arrow(TypeVar(5), TypeVar(5)));
        assert_eq!(actual, expected);

        let expected = arrow(TypeVar(1), TypeVar(2));
        let actual = universalized(&arrow(TypeVar(5), TypeVar(3)));
        assert_eq!(actual, expected);

        let t0 = TypeVar(5);
        let t1 = TypeVar(2);
        let expected_variants = vec![
            (String::from("Left"), vec![TypeVar(1)]),
            (String::from("Right"), vec![TypeVar(2)])
        ];
        let expected = SumType::new("Either", expected_variants, vec![TypeVar(1), TypeVar(2)]);
        let variants = vec![
            (String::from("Left"), vec![t0.clone()]),
            (String::from("Right"), vec![t1.clone()])
        ];
        let actual = universalized(&Sum(SumType::new("Either", variants, vec![t0, t1])));
        assert_eq!(actual, Sum(expected));

        let t0 = arrow(TypeVar(3), Bool);
        let t1 = arrow(TypeVar(2), TypeVar(3));
        let expected_variants = vec![
            (String::from("Left"), vec![arrow(TypeVar(1), Bool)]),
            (String::from("Right"), vec![arrow(TypeVar(2), TypeVar(1))])
        ];
        let expected_params = vec![
            arrow(TypeVar(1), Bool),
            arrow(TypeVar(2), TypeVar(1))
        ];
        let expected = Sum(SumType::new("Either", expected_variants, expected_params));
        let variants = vec![
            (String::from("Left"), vec![t0.clone()]),
            (String::from("Right"), vec![t1.clone()])
        ];
        let actual = universalized(&Sum(SumType::new("Either", variants, vec![t0, t1])));
        assert_eq!(actual, expected);
    }

    /* Test type inference */

    #[test]
    fn test_infer_base_types() {
        let examples = vec![
            (int_to_term(5), Int),
            (bool_to_term(false), Bool),
            (op_to_term(Add), arrow(Int, arrow(Int, Int))),
            (op_to_term(Eql), arrow(Int, arrow(Int, Bool))),
            (unit(), Unit)
        ];
        for (term, typ) in examples {
            assert_type(&term, &typ, &vec![], &mut GV::new(), &mut SDs::new());
        }
    }

    #[test]
    fn test_application_of_builtins() {
        let eql_5 = apply(op_to_term(Op::Eql), FIVE);
        let eql_5_4 = apply(eql_5.clone(), int_to_term(4));
        let add_5 = apply(op_to_term(Op::Add), FIVE);
        let add_5_4 = apply(add_5.clone(), int_to_term(4));

        let examples = vec![
            (eql_5, arrow(Int, Bool)),
            (eql_5_4, Bool),
            (add_5, arrow(Int, Int)),
            (add_5_4, Int)
        ];
        for (term, typ) in examples {
            assert_type(&term, &typ, &vec![], &mut GV::new(), &mut SDs::new());
        }
    }

    #[test]
    fn test_identity() {
        let id = lambda(var(0, "x"), "x");
        let typ = arrow(TypeVar(1), TypeVar(1));
        assert_type(&id, &typ, &vec![], &mut GV::new(), &mut SDs::new());
        let x = apply(id.clone(), FIVE);
        assert_type(&x, &Int, &vec![], &mut GV::new(), &mut SDs::new());
        let x = apply(id.clone(), bool_to_term(true));
        assert_type(&x, &Bool, &vec![], &mut GV::new(), &mut SDs::new());
    }

    #[test]
    fn test_type_mismatch() {
        let eql = Term::Atom(Atom::BuiltIn(Op::Eql));
        let tru = Term::Atom(Atom::Bool(true));
        let x = apply(eql, tru);
        let msg = "Type error: Bool != Int";
        assert_type_err(&x, msg, &vec![], &mut GV::new(), &mut SDs::new());
    }

    #[test]
    fn test_application_of_non_function() {
        let x = apply(FIVE, FIVE);
        let msg = "Type error: Int -> t2 != Int";
        assert_type_err(&x, msg, &vec![], &mut GV::new(), &mut SDs::new());
    }

    #[test]
    fn test_no_recursive_function_types() {
        let x = Term::Var(0, String::from("x"));
        let untypable = lambda(apply(x.clone(), x), "x");
        let msg = "Type error: t1 -> t3 != t1";
        assert_type_err(&untypable, msg, &vec![], &mut GV::new(), &mut SDs::new());
    }

    #[test]
    fn test_valid_conditional() {
        let cond = cond(bool_to_term(true), FIVE, int_to_term(4));
        assert_type(&cond, &Int, &vec![], &mut GV::new(), &mut SDs::new());
    }

    #[test]
    fn test_conditional_with_invalid_predicate() {
        let cond = cond(int_to_term(5), FIVE, int_to_term(4));
        let msg = "Type error: Int != Bool";
        assert_type_err(&cond, msg, &vec![], &mut GV::new(), &mut SDs::new());
    }

    #[test]
    fn test_conditional_with_mismatched_arms() {
        let cond = cond(bool_to_term(false), FIVE, bool_to_term(true));
        let msg = "Type error: Int != Bool";
        assert_type_err(&cond, msg, &vec![], &mut GV::new(), &mut SDs::new());
    }

    #[test]
    fn test_polymorphic_let() {
        let id = lambda(var(0, "x"), "x");
        let v = var(0, "id");
        let poly = apply(apply(v.clone(), op_to_term(Gt)), apply(v, FIVE));
        let term = Term::Let(String::from("id"), Box::new(id), Some(Box::new(poly)));
        assert_type(&term, &arrow(Int, Bool), &vec![], &mut GV::new(), &mut SDs::new());
    }

    #[test]
    fn test_ill_typed_let_value_not_used_in_body() {
        let invalid = apply(op_to_term(Add), bool_to_term(false));
        let term = Term::Let(String::from("invalid"), Box::new(invalid), Some(Box::new(FIVE)));
        let msg = "Type error: Bool != Int";
        assert_type_err(&term, msg, &vec![], &mut GV::new(), &mut SDs::new());
    }

    mod test_sum_types {
        use super::*;
        use structures::patterns::Pattern;

        const LEFT: usize = 0;
        const RIGHT: usize = 1;
        const JUST: usize = 2;
        const NONE: usize = 3;
        const FOO: usize = 4;

        fn make_sum_types() -> (SDs, GV) {
            let mut gen = GV::new();
            let mut sum_types = SDs::new();

            let (t0, t1) = (gen.next().unwrap(), gen.next().unwrap());
            let variants = vec![
                (String::from("Left"), vec![t0.clone()]),
                (String::from("Right"), vec![t1.clone()])
            ];
            sum_types.add_type("Either", variants, vec![t0.clone(), t1.clone()])
                .unwrap();

            let t = gen.next().unwrap();
            let variants = vec![
                (String::from("Just"), vec![t.clone()]),
                (String::from("Nothing"), vec![])
            ];
            sum_types.add_type("Maybe", variants, vec![t.clone()]).unwrap();

            let variants = vec![(String::from("Foo"), vec![])];
            sum_types.add_type("Baz", variants, vec![]).unwrap();

            (sum_types, gen)
        }

        fn left() -> Term {
            let x = String::from("x");
            lambda(Term::Sum(LEFT, String::from("Left"), vec![Term::Var(0, x)]), "x")
        }

        fn left_sum(term: &Term) -> Term {
            Term::Sum(LEFT, String::from("Left"), vec![term.clone()])
        }

        fn right() -> Term {
            lambda(Term::Sum(RIGHT, String::from("Right"), vec![Term::Var(0, String::from("x"))]), "x")
        }

        fn right_sum(term: &Term) -> Term {
            Term::Sum(RIGHT, String::from("Right"), vec![term.clone()])
        }

        #[test]
        fn test_pattern_with_wrong_number_of_args() {
            let (sum_types, mut gen) = make_sum_types();

            let too_many_args = Pattern::Sum(
                RIGHT,
                String::from("Right"),
                vec![Pattern::Wildcard, Pattern::Wildcard])
            ;
            let term = case(right_sum(&FIVE), vec![(too_many_args, FIVE)], FIVE);
            let msg = "Constructor Right should have 1 argument(s) but instead got 2";
            assert_type_err(&term, &msg, &vec![], &mut gen, &sum_types);

            let too_few_args = Pattern::Sum(RIGHT, String::from("Right"), vec![]);
            let term = case(right_sum(&FIVE), vec![(too_few_args, FIVE)], FIVE);
            let msg = "Constructor Right should have 1 argument(s) but instead got 0";
            assert_type_err(&term, &msg, &vec![], &mut gen, &sum_types);
        }

        #[test]
        fn test_unary_constructor() {
            let (sum_types, mut gen) = make_sum_types();

            let term = apply(left(), FIVE);
            let left_concrete_variants = vec![
                (String::from("Left"), vec![Int]),
                (String::from("Right"), vec![TypeVar(1)])
            ];
            let expected = SumType::new(
                "Either",
                left_concrete_variants,
                vec![Int, TypeVar(1)]
            );
            assert_type(&term, &Sum(expected), &vec![], &mut gen, &sum_types);

            let term = apply(right(), FIVE);
            let right_concrete_variants = vec![
                (String::from("Left"), vec![TypeVar(1)]),
                (String::from("Right"), vec![Int])
            ];
            let expected = SumType::new(
                "Either",
                right_concrete_variants,
                vec![TypeVar(1), Int]
            );
            assert_type(&term, &Sum(expected), &vec![], &mut gen, &sum_types);

            //fully concrete
            let pred = bool_to_term(false);
            let true_case = apply(left(), FIVE);
            let false_case = apply(right(), bool_to_term(true));
            let term = cond(pred, true_case, false_case);
            let concrete_variants = vec![
                (String::from("Left"), vec![Int]),
                (String::from("Right"), vec![Bool])
            ];
            let expected = SumType::new("Either", concrete_variants, vec![Int, Bool]);
            assert_type(&term, &Sum(expected), &vec![], &mut gen, &sum_types);
        }

        #[test]
        fn test_sum_from_nullary_constructor() {
            let (sum_types, mut gen) = make_sum_types();

            let term = Term::Sum(FOO, String::from("Foo"), vec![]);
            let typ = SumType::new("Baz", vec![(String::from("Foo"), vec![])], vec![]);
            assert_type(&term, &Sum(typ), &vec![], &mut gen, &sum_types);

            let invalid_1 = Term::Sum(FOO, String::from("Foo"), vec![FIVE]);
            let expected = "Too many arguments given to constructor Foo";
            assert_type_err(&invalid_1, &expected, &vec![], &mut gen, &sum_types);

        }

        #[test]
        fn test_sum_from_unary_constructor() {
            let (sum_types, mut gen) = make_sum_types();

            let term = left_sum(&FIVE);
            let left_concrete_variants = vec![
                (String::from("Left"), vec![Int]),
                (String::from("Right"), vec![TypeVar(1)])
            ];
            let expected = SumType::new(
                "Either",
                left_concrete_variants,
                vec![Int, TypeVar(1)]
            );
            assert_type(&term, &Sum(expected), &vec![], &mut gen, &sum_types);

            let term = right_sum(&FIVE);
            let right_concrete_variants = vec![
                (String::from("Left"), vec![TypeVar(1)]),
                (String::from("Right"), vec![Int])
            ];
            let expected = SumType::new(
                "Either",
                right_concrete_variants,
                vec![TypeVar(1), Int]
            );
            assert_type(&term, &Sum(expected), &vec![], &mut gen, &sum_types);

            //fully concrete
            let pred = bool_to_term(false);
            let true_case = left_sum(&FIVE);
            let false_case = right_sum(&bool_to_term(true));
            let term = cond(pred, true_case, false_case);
            let concrete_variants = vec![
                (String::from("Left"), vec![Int]),
                (String::from("Right"), vec![Bool])
            ];
            let expected = SumType::new("Either", concrete_variants, vec![Int, Bool]);
            assert_type(&term, &Sum(expected), &vec![], &mut gen, &sum_types);
        }

        #[test]
        fn test_sum_from_binary_constructor() {
            use declare_sum_type;
            let mut gen = GV::new();
            let mut sum_types = SDs::new();
            declare_sum_type("Pair = Pair Int Bool", &mut sum_types).unwrap();
            let term = Term::Sum(0, String::from("Pair"), vec![FIVE, FIVE]);
            let expected = "Type error: Int != Bool";
            assert_type_err(&term, &expected, &vec![], &mut gen, &sum_types);
        }

        #[test]
        fn test_case_with_no_patterns() {
            let term = case(unit(), vec![], FIVE);
            assert_type(&term, &Int, &vec![], &mut GV::new(), &mut SDs::new());
        }

        #[test]
        fn test_valid_case_expression() {
            let (sum_types, mut gen) = make_sum_types();

            let pat0 = Pattern::Sum(LEFT, String::from("Left"), vec![Pattern::Wildcard]);
            let pat1 = Pattern::Sum(RIGHT, String::from("Right"), vec![Pattern::Wildcard]);
            let pat2 = Pattern::Var(String::from("x"));
            let pat3 = Pattern::Wildcard;
            let cases = vec![
                (pat0, int_to_term(0)),
                (pat1, int_to_term(1)),
                (pat2, int_to_term(2)),
                (pat3, int_to_term(3))
            ];

            let term = case(left_sum(&unit()), cases, FIVE);
            assert_type(&term, &Int, &vec![], &mut gen, &sum_types);
        }

        #[test]
        fn test_infer_case_arg_type_from_patterns() {
            let (sum_types, mut gen) = make_sum_types();

            let pat0 = Pattern::Sum(JUST, String::from("Just"), vec![Pattern::Wildcard]);

            let cases = vec![(pat0, int_to_term(0))];
            let term = lambda(case(var(0, "x"), cases, FIVE), "x");
            let variants = vec![
                (String::from("Just"), vec![TypeVar(1)]),
                (String::from("Nothing"), vec![])
            ];
            let typ = Sum(SumType::new("Maybe", variants, vec![TypeVar(1)]));
            let expected_type = arrow(typ, Int);
            assert_type(&term, &expected_type, &vec![], &mut gen, &sum_types);
        }

        #[test]
        fn test_infer_case_arg_type_from_nested_patterns() {
            let (sum_types, mut gen) = make_sum_types();

            let pat0 = Pattern::Sum(JUST, String::from("Just"), vec![Pattern::Wildcard]);
            let pat1 = Pattern::Sum(
                JUST,
                String::from("Just"),
                vec![Pattern::Atom(Atom::Bool(true))]
            );
            let cases = vec![(pat0, int_to_term(0)), (pat1, int_to_term(1))];
            let term = lambda(case(var(0, "x"), cases, FIVE), "x");
            let sum = SumType::new(
                "Maybe",
                vec![(String::from("Just"), vec![Bool]), (String::from("Nothing"), vec![])],
                vec![Bool]
            );
            let expected_type = arrow(Sum(sum), Int);
            assert_type(&term, &expected_type, &vec![], &mut gen, &sum_types);
        }

        #[test]
        fn test_match_arm_using_bound_variable() {
            let (sum_types, mut gen) = make_sum_types();

            let pat0 = Pattern::Sum(
                LEFT,
                String::from("Left"),
                vec![Pattern::Var(String::from("x"))]
            );
            let pat1 = Pattern::Sum(RIGHT, String::from("Right"), vec![Pattern::Wildcard]);
            let id = lambda(var(0, "x"), "x");
            let cases = vec![(pat0.clone(), apply(id, var(0, "x"))), (pat1.clone(), FIVE)];

            let term = case(left_sum(&FIVE), cases.clone(), FIVE);
            assert_type(&term, &Int, &vec![], &mut gen, &sum_types);
        }

        #[test]
        fn test_atom_pattern() {
            let pat0 = Pattern::Atom(Atom::Int(0));
            let pat1 = Pattern::Atom(Atom::Int(1));
            let cases = vec![(pat0, bool_to_term(true)), (pat1, bool_to_term(false))];
            let term = case(FIVE, cases, bool_to_term(true));
            assert_type(&term, &Bool, &vec![], &mut GV::new(), &mut SDs::new());
        }

        #[test]
        fn test_nested_patterns() {
            // Either (Maybe Int) Int -> Int
            let (sum_types, mut gen) = make_sum_types();

            let pat0 = Pattern::Sum(
                LEFT,
                String::from("Left"),
                vec![
                    Pattern::Sum(JUST, String::from("Just"),
                    vec![Pattern::Var(String::from("x"))])
                ]
            );
            let pat1 = Pattern::Sum(
                LEFT,
                String::from("Left"),
                vec![Pattern::Sum(NONE, String::from("None"), vec![])]
            );
            let pat2 = Pattern::Sum(
                RIGHT,
                String::from("Right"),
                vec![Pattern::Var(String::from("x"))]
            );

            let x = var(0, "x");
            let cases = vec![(pat0, x.clone()), (pat1, FIVE), (pat2, x)];
            let arg = left_sum(&Term::Sum(JUST, String::from("Just"), vec![FIVE]));
            let term = case(arg, cases, FIVE);
            assert_type(&term, &Int, &vec![], &mut gen, &sum_types);
        }

        #[test]
        fn test_mismatched_argument() {
            let pat0 = Pattern::Atom(Atom::Int(0));
            let cases = vec![(pat0, bool_to_term(true))];
            let term = case(bool_to_term(true), cases, bool_to_term(true));
            let err = "Type error: Bool != Int";
            assert_type_err(&term, err, &vec![], &mut GV::new(), &mut SDs::new());
        }

        #[test]
        fn test_mismatched_arms() {
            let (sum_types, mut gen) = make_sum_types();

            let pat0 = Pattern::Sum(LEFT, String::from("Left"), vec![Pattern::Wildcard]);
            let pat1 = Pattern::Sum(RIGHT, String::from("Right"), vec![Pattern::Wildcard]);
            let cases = vec![(pat0.clone(), unit()), (pat1.clone(), bool_to_term(true))];

            let term = case(right_sum(&unit()), cases.clone(), unit());
            let expected = "Type error: Bool != ()";
            assert_type_err(&term, &expected, &vec![], &mut gen, &sum_types);

            // mismatched default case
            let cases = vec![(pat0, bool_to_term(false)), (pat1, bool_to_term(true))];
            let term = case(right_sum(&unit()), cases, unit());
            let expected = "Type error: Bool != ()";
            assert_type_err(&term, &expected, &vec![], &mut gen, &sum_types);
        }

        #[test]
        fn test_mismatched_patterns() {
            let (sum_types, mut gen) = make_sum_types();

            let pat0 = Pattern::Sum(LEFT, String::from("Left"), vec![Pattern::Wildcard]);
            let pat1 = Pattern::Sum(JUST, String::from("Just"), vec![Pattern::Wildcard]);
            let cases = vec![(pat0, unit()), (pat1, unit())];

            let term = case(right_sum(&unit()), cases, unit());
            let expected = "Type error: Either != Maybe";
            assert_type_err(&term, &expected, &vec![], &mut gen, &sum_types);
        }
    }
}
