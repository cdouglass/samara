use interpret::declare_sum_type;
use interpret::evaluate;
use interpret::type_of;
use interpret::GenTypeVar;
use interpret::SumTypeDefs;
use interpret::structures::Atom;
use interpret::structures::LetBinding;
use interpret::structures::Op;
use interpret::structures::Term;
use interpret::structures::Type;

/* Test helpers */

fn assert_evaluation_err(expr: &str, mut bindings: &mut Vec<LetBinding>, mut gen: &mut GenTypeVar, msg: &str) {
    match evaluate(expr, bindings, gen, &SumTypeDefs::new()) {
        Err(m) => { assert_eq!(m, msg) },
        result => {
            println!("Expected Err({}). Instead got {:?}", msg, result);
            panic!()
        }
    }
}

fn assert_evaluates_to_atom_with_context(expr: &str, mut bindings: &mut Vec<LetBinding>, mut gen: &mut GenTypeVar, sum_types: &SumTypeDefs, expected: Atom) {
    match evaluate(expr, &mut bindings, gen, &sum_types).unwrap() {
        Term::Atom(a) => assert_eq!(a, expected),
        result => {
            println!("Expected {:?}. Instead got {:?}", expected, result);
            panic!()
        }
    }
}

fn assert_evaluates_to_atom(expr: &str, mut bindings: &mut Vec<LetBinding>, mut gen: &mut GenTypeVar, expected: Atom) {
    assert_evaluates_to_atom_with_context(expr, bindings, gen, &SumTypeDefs::new(), expected);
}

#[test]
fn test_too_many_arguments() {
    assert_evaluation_err("(* 1 2 3)", &mut vec![], &mut GenTypeVar::new(), "Type error: Int -> t6 != Int");
}

/* Evaluating valid input */

#[test]
fn test_evaluate_int() {
    assert_evaluates_to_atom("42", &mut vec![], &mut GenTypeVar::new(), Atom::Int(42));
}

#[test]
fn test_evaluate_op() {
    assert_evaluates_to_atom("+", &mut vec![], &mut GenTypeVar::new(), Atom::BuiltIn(Op::Add));
    assert_evaluates_to_atom("//", &mut vec![], &mut GenTypeVar::new(), Atom::BuiltIn(Op::Div));
}

#[test]
fn test_partially_apply_op() {
    let mut gen = GenTypeVar::new();
    let sum_types = SumTypeDefs::new();
    let result = evaluate("(% 10)", &mut vec![], &mut gen, &sum_types).unwrap();
    match result {
        Term::App(a, b) => {
            match (*a, *b) {
                (Term::Atom(Atom::BuiltIn(op)), Term::Atom(Atom::Int(n))) => assert_eq!((op, n), (Op::Mod, 10)),
                _ => panic!()
            }
        },
        _ => panic!()
    }
}

/*
Requires recursive types

#[test]
fn test_anonymous_factorial() {
    let expr = "((\\f -> (\\x -> f (\\y -> x x y)) (\\x -> f (\\y -> x x y))) (\\fct -> (\\n -> if (< n 2) then 1 else (* n (fct (- n 1)))))) 8";
    assert_evaluates_to_atom(expr, &mut vec![], &mut GenTypeVar::new(), Atom::Int(40320));
}

*/

#[test]
fn test_factorial_with_let() {
    let expr = "let fact = (\\n -> if (< n 2) then 1 else (* n (fact (- n 1)))) in fact 8";
    assert_evaluates_to_atom(expr, &mut vec![], &mut GenTypeVar::new(), Atom::Int(40320));
}

#[test]
fn test_polymorphic_let() {
    let mut gen = GenTypeVar::new();
    assert_evaluates_to_atom("let id = (\\x -> x) in (if id True then id 5 else id 10)", &mut vec![], &mut gen, Atom::Int(5));

    assert_evaluates_to_atom(
        "let
            compose = (\\f -> (\\g -> (\\x -> f (g x))))
        in
            if (compose (\\x -> (== (% x 2) 0)) (\\b -> if b then 1 else 0) False)
            then (compose (\\x -> (- x 1)) (\\x -> (* x 3)) 5)
            else 0",
        &mut vec![], &mut gen, Atom::Int(14));
}

#[test]
fn test_polymorphic_session_let() {
    let mut bindings = vec![];
    let mut gen = GenTypeVar::new();
    let sum_types = SumTypeDefs::new();
    evaluate("let id = (\\x -> x)", &mut bindings, &mut gen, &sum_types).unwrap();

    assert_evaluates_to_atom("id 5", &mut bindings, &mut gen, Atom::Int(5));
    assert_evaluates_to_atom("id False", &mut bindings, &mut gen, Atom::Bool(false));

    // (b -> c) -> (a -> b) -> a -> c
    evaluate("let compose = (\\f -> (\\g -> (\\x -> f (g x))))", &mut bindings, &mut gen, &sum_types).unwrap();
    // (Int -> Int) -> (Int -> Int) -> Int -> Int
    assert_evaluates_to_atom("compose (\\x -> (- x 1)) (\\x -> (* x 3)) 5", &mut bindings, &mut gen, Atom::Int(14));
    // (Int -> Bool) -> (Bool -> Int) -> Bool -> Bool
    assert_evaluates_to_atom("compose (\\x -> (== (% x 2) 0)) (\\b -> if b then 1 else 0) True", &mut bindings, &mut gen, Atom::Bool(false));
}

#[test]
fn test_fully_apply_op() {
    assert_evaluates_to_atom("(// 12 3)", &mut vec![], &mut GenTypeVar::new(), Atom::Int(4));
}

#[test]
fn test_missing_outer_parens() {
    assert_evaluates_to_atom("+ 10 10", &mut vec![], &mut GenTypeVar::new(), Atom::Int(20));
}

#[test]
fn test_save_session_bindings() {
    let expected = Ok(Term::Atom(Atom::Int(50)));

    let mut bindings = vec![];
    let mut gen = GenTypeVar::new();
    let sum_types = SumTypeDefs::new();
    evaluate("let x = (* 5 10)", &mut bindings, &mut gen, &sum_types).unwrap();

    let reuse = evaluate("x", &mut bindings, &mut gen, &sum_types);
    assert_eq!(reuse, expected);
}

#[test]
fn test_recursive_session_bindings() {
    let mut bindings = vec![];
    let mut gen = GenTypeVar::new();
    let sum_types = SumTypeDefs::new();
    evaluate("let fact = (\\n -> if (< n 2) then 1 else (* n (fact (- n 1))))", &mut bindings, &mut gen, &sum_types).unwrap();
    assert_evaluates_to_atom("fact 8", &mut bindings, &mut GenTypeVar::new(), Atom::Int(40320));
}

#[test]
fn test_type_of_using_session_bindings() {
    let mut bindings = vec![];
    let mut gen = GenTypeVar::new();
    let sum_types = SumTypeDefs::new();
    evaluate("let x = (* 5 10)", &mut bindings, &mut gen, &sum_types).unwrap();
    let typ = type_of("x", &bindings, &mut gen, &sum_types).1.unwrap();
    assert_eq!(typ, Type::Int);
}

#[test]
fn test_evaluate_case_expression() {
    let mut bindings = vec![];
    let mut gen = GenTypeVar::new();
    let mut sum_types = SumTypeDefs::new();
    declare_sum_type("Maybe a = Just a | None", &mut gen, &mut sum_types).unwrap();
    assert_evaluates_to_atom_with_context("case Just 10 of 100; Just 10 -> 42; Just x -> x; None -> 0", &mut bindings, &mut gen, &sum_types, Atom::Int(42));
    assert_evaluates_to_atom_with_context("case Just 9 of 100; Just 10 -> 42; Just x -> x; None -> 0", &mut bindings, &mut gen, &sum_types, Atom::Int(9));
    assert_evaluates_to_atom_with_context("case None of 100; Just 10 -> 42; Just x -> x; None -> 0", &mut bindings, &mut gen, &sum_types, Atom::Int(0));
    assert_evaluates_to_atom_with_context("case None of 100; Just 10 -> 42; Just x -> x", &mut bindings, &mut gen, &sum_types, Atom::Int(100));

    assert_evaluates_to_atom_with_context("case Just(Just 4) of 100; Just Just x -> (* x x); Just None -> 5", &mut bindings, &mut gen, &sum_types, Atom::Int(16));
    assert_evaluates_to_atom_with_context("case Just(Just 4) of 100; Just (Just x) -> (* x x); Just None -> 5", &mut bindings, &mut gen, &sum_types, Atom::Int(16));
}
//}

#[test]
fn test_binding_in_case_expression() {
    let mut bindings = vec![];
    let mut gen = GenTypeVar::new();
    let sum_types = SumTypeDefs::new();
    evaluate("let x = 5", &mut bindings, &mut gen, &sum_types).unwrap();

    let exprs = vec!["case 1 of 0; x -> x", "case x of 0; _ -> 1"];
    for e in exprs {
        assert_evaluates_to_atom_with_context(e, &mut bindings, &mut gen, &sum_types, Atom::Int(1));
    }

    let exprs = vec!["case x of 0; y -> x","case 1 of 0; y -> x", "case x of 0; _ -> x", "case x of 0; x -> x"];

    for e in exprs {
        let typ = type_of(e, &bindings, &mut gen, &sum_types).1.unwrap();
        assert_eq!(typ, Type::Int);
        assert_evaluates_to_atom_with_context(e, &mut bindings, &mut gen, &sum_types, Atom::Int(5));
    }
}

#[test]
fn test_pair_type() {
    let mut bindings: Vec<LetBinding> = vec![];
    let mut gen = GenTypeVar::new();
    let mut sum_types = SumTypeDefs::new();
    declare_sum_type("Pair a b = Pair a b", &mut gen, &mut sum_types).unwrap();
    evaluate("Pair True 5", &mut bindings, &mut gen, &sum_types).unwrap();
    assert_evaluates_to_atom_with_context("(\\x -> 5) (Pair 0 10)", &mut bindings, &mut gen, &sum_types, Atom::Int(5));
    assert_evaluates_to_atom_with_context("case (Pair 0 10) of 5; Pair _ x -> x", &mut bindings, &mut gen, &sum_types, Atom::Int(10));
    assert_evaluates_to_atom_with_context("case (Pair 0 10) of 5; Pair x _ -> x", &mut bindings, &mut gen, &sum_types, Atom::Int(0));
    assert_evaluates_to_atom_with_context("case (Pair 7 6) of 5; Pair y z -> - y z", &mut bindings, &mut gen, &sum_types, Atom::Int(1));
}
