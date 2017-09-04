use interpret::evaluate;
use interpret::type_of;
use interpret::GenTypeVar;
use interpret::types::Atom;
use interpret::types::LetBinding;
use interpret::types::Op;
use interpret::types::Term;
use interpret::types::Type;

/* Test helpers */

fn assert_evaluation_err(expr: &str, mut bindings: &mut Vec<LetBinding>, mut gen: &mut GenTypeVar, msg: &str) {
    match evaluate(expr, bindings, gen) {
        Err(m) => { assert_eq!(m, msg) },
        result => {
            println!("Expected Err({}). Instead got {:?}", msg, result);
            panic!()
        }
    }
}

fn assert_evaluates_to_atom(expr: &str, mut bindings: &mut Vec<LetBinding>, mut gen: &mut GenTypeVar, expected: Atom) {
    match evaluate(expr, &mut bindings, gen).unwrap() {
        Term::Atom(a) => assert_eq!(a, expected),
        result => {
            println!("Expected {:?}. Instead got {:?}", expected, result);
            panic!()
        }
    }
}

fn make_gen() -> GenTypeVar {
    GenTypeVar{n: 0}
}

#[test]
fn test_too_many_arguments() {
    assert_evaluation_err("(* 1 2 3)", &mut vec![], &mut make_gen(), "Type error: Int -> t6 != Int");
}

/* Evaluating valid input */

#[test]
fn test_evaluate_int() {
    assert_evaluates_to_atom("42", &mut vec![], &mut make_gen(), Atom::Int(42));
}

#[test]
fn test_evaluate_op() {
    assert_evaluates_to_atom("+", &mut vec![], &mut make_gen(), Atom::BuiltIn(Op::Add));
    assert_evaluates_to_atom("//", &mut vec![], &mut make_gen(), Atom::BuiltIn(Op::Div));
}

#[test]
fn test_partially_apply_op() {
    let mut gen = GenTypeVar{n: 0};
    let result = evaluate("(% 10)", &mut vec![], &mut gen).unwrap();
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
    assert_evaluates_to_atom(expr, &mut vec![], &mut make_gen(), Atom::Int(40320));
}

*/

#[test]
fn test_factorial_with_let() {
    let expr = "let fact = (\\n -> if (< n 2) then 1 else (* n (fact (- n 1)))) in fact 8";
    assert_evaluates_to_atom(expr, &mut vec![], &mut make_gen(), Atom::Int(40320));
}

#[test]
fn test_polymorphic_let() {
    let mut gen = make_gen();
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
    let mut gen = make_gen();
    evaluate("let id = (\\x -> x)", &mut bindings, &mut gen).unwrap();

    assert_evaluates_to_atom("id 5", &mut bindings, &mut gen, Atom::Int(5));
    assert_evaluates_to_atom("id False", &mut bindings, &mut gen, Atom::Bool(false));

    // (b -> c) -> (a -> b) -> a -> c
    evaluate("let compose = (\\f -> (\\g -> (\\x -> f (g x))))", &mut bindings, &mut gen).unwrap();
    // (Int -> Int) -> (Int -> Int) -> Int -> Int
    assert_evaluates_to_atom("compose (\\x -> (- x 1)) (\\x -> (* x 3)) 5", &mut bindings, &mut gen, Atom::Int(14));
    // (Int -> Bool) -> (Bool -> Int) -> Bool -> Bool
    assert_evaluates_to_atom("compose (\\x -> (== (% x 2) 0)) (\\b -> if b then 1 else 0) True", &mut bindings, &mut gen, Atom::Bool(false));
}

#[test]
fn test_fully_apply_op() {
    assert_evaluates_to_atom("(// 12 3)", &mut vec![], &mut make_gen(), Atom::Int(4));
}

#[test]
fn test_missing_outer_parens() {
    assert_evaluates_to_atom("+ 10 10", &mut vec![], &mut make_gen(), Atom::Int(20));
}

#[test]
fn test_save_session_bindings() {
    let expected = Ok(Term::Atom(Atom::Int(50)));

    let mut bindings = vec![];
    let mut gen = make_gen();
    evaluate("let x = (* 5 10)", &mut bindings, &mut gen).unwrap();

    let reuse = evaluate("x", &mut bindings, &mut gen);
    assert_eq!(reuse, expected);
}

#[test]
fn test_recursive_session_bindings() {
    let mut bindings = vec![];
    let mut gen = make_gen();
    evaluate("let fact = (\\n -> if (< n 2) then 1 else (* n (fact (- n 1))))", &mut bindings, &mut gen).unwrap();
    assert_evaluates_to_atom("fact 8", &mut bindings, &mut make_gen(), Atom::Int(40320));
}

#[test]
fn test_type_of_using_session_bindings() {
    let mut bindings = vec![];
    let mut gen = make_gen();
    evaluate("let x = (* 5 10)", &mut bindings, &mut gen).unwrap();
    let typ = type_of("x", &bindings, &mut gen).1.unwrap();
    assert_eq!(typ, Type::Int);
}
