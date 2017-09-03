use interpret::evaluate;
use interpret::parse;
use interpret::type_of;
use interpret::GenTypeVar;
use interpret::tokenize::build_lexer;
use interpret::tokenize::Token;
use interpret::types::Atom;
use interpret::types::Op;
use interpret::types::Term;
use interpret::types::Type;

#[test]
fn test_lex() {
    let input = "(5  5 * / // +34()";
    let expected = [Token::Open, Token::Number(String::from("5")), Token::Number(String::from("5")), Token::Operator(String::from("*")), Token::Operator(String::from("/")), Token::Operator(String::from("//")), Token::Operator(String::from("+")), Token::Number(String::from("34")), Token::Open, Token::Close];
    let actual : Vec<Token>  = build_lexer(input).collect();
    println!("expected: {:?}", expected);
    println!("actual: {:?}", actual);
    assert!(actual == expected);
}

/* Test helpers */

fn assert_evaluation_err(expr: &str, mut bindings: &mut Vec<(String, Term)>, msg: &str) {
    match evaluate(expr, bindings) {
        Err(m) => { assert_eq!(m, msg) },
        result => {
            println!("Expected Err({}). Instead got {:?}", msg, result);
            panic!()
        }
    }
}

fn assert_evaluates_to_atom(expr: &str, mut bindings: &mut Vec<(String, Term)>, expected: Atom) {
    match evaluate(expr, &mut bindings).unwrap() {
        Term::Atom(a) => assert_eq!(a, expected),
        result => {
            println!("Expected {:?}. Instead got {:?}", expected, result);
            panic!()
        }
    }
}

/* Parsing */

#[test]
fn test_parses_lambda_application() {
    let ast = parse(&mut build_lexer("(\\x -> (\\y -> 3)) 2"), &mut vec![]);
    let expected =
        Term::App(
            Box::new(Term::Lambda(
                Box::new(Term::Lambda(Box::new(Term::Atom(Atom::Int(3))), String::from("y"))),
                "x".to_string()
            )),
            Box::new(Term::Atom(Atom::Int(2)))
            );

    assert_eq!(ast, Ok(expected));
}

    /* Syntax errors */

#[test]
fn test_empty_input() {
    assert_evaluation_err("", &mut vec![], "Unexpected end of input");
    assert_evaluation_err("()", &mut vec![], "Unexpected end of input");
}

#[test]
fn test_unbalanced_delimiters() {
    assert_evaluation_err("(+ 5 8))", &mut vec![], "Unexpected CLOSE delimiter");
    assert_evaluation_err("(+ 5 (8)", &mut vec![], "Unexpected end of input");
}

/* Type errors */

#[test]
fn test_too_many_arguments() {
    assert_evaluation_err("(* 1 2 3)", &mut vec![], "Type error");
}

/* Evaluating valid input */

#[test]
fn test_evaluate_int() {
    assert_evaluates_to_atom("42", &mut vec![], Atom::Int(42));
}

#[test]
fn test_evaluate_op() {
    assert_evaluates_to_atom("+", &mut vec![], Atom::BuiltIn(Op::Add));
    assert_evaluates_to_atom("//", &mut vec![], Atom::BuiltIn(Op::Div));
}

#[test]
fn test_partially_apply_op() {
    let result = evaluate("(% 10)", &mut vec![]).unwrap();
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

#[test]
fn test_anonymous_factorial() {
    let expr = "((\\f -> (\\x -> f (\\y -> x x y)) (\\x -> f (\\y -> x x y))) (\\fct -> (\\n -> if (< n 2) then 1 else (* n (fct (- n 1)))))) 8";
    assert_evaluates_to_atom(expr, &mut vec![], Atom::Int(40320));
}

#[test]
fn test_factorial_with_let() {
    let expr = "let fact = (\\n -> if (< n 2) then 1 else (* n (fact (- n 1)))) in fact 8";
    assert_evaluates_to_atom(expr, &mut vec![], Atom::Int(40320));
}

#[test]
fn test_polymorphic_let() {
    assert_evaluates_to_atom("let id = (\\x -> x) in (if id True then id 5 else id 10)", &mut vec![], Atom::Int(5));

    assert_evaluates_to_atom(
        "let
            compose = (\\f -> (\\g -> (\\x -> f (g x))))
        in
            if (compose (\\x -> (== (% x 2) 0)) (\\b -> if b then 1 else 0) False)
            then (compose (\\x -> (- x 1)) (\\x -> (* x 3)) 5)
            else 0",
        &mut vec![], Atom::Int(14));
}

#[test]
fn test_polymorphic_session_let() {
    let mut bindings = vec![];
    evaluate("let id = (\\x -> x)", &mut bindings).unwrap();

    assert_evaluates_to_atom("id 5", &mut bindings, Atom::Int(5));
    assert_evaluates_to_atom("id False", &mut bindings, Atom::Bool(false));

    // (b -> c) -> (a -> b) -> a -> c
    evaluate("let compose = (\\f -> (\\g -> (\\x -> f (g x))))", &mut bindings).unwrap();
    // (Int -> Int) -> (Int -> Int) -> Int -> Int
    assert_evaluates_to_atom("compose (\\x -> (- x 1)) (\\x -> (* x 3)) 5", &mut bindings, Atom::Int(14));
    // (Int -> Bool) -> (Bool -> Int) -> Bool -> Bool
    assert_evaluates_to_atom("compose (\\x -> (== (% x 2) 0)) (\\b -> if b then 1 else 0) True", &mut bindings, Atom::Bool(false));
}

#[test]
fn test_fully_apply_op() {
    assert_evaluates_to_atom("(// 12 3)", &mut vec![], Atom::Int(4));
}

#[test]
fn test_missing_outer_parens() {
    assert_evaluates_to_atom("+ 10 10", &mut vec![], Atom::Int(20));
}

#[test]
fn test_save_session_bindings() {
    let expected = Ok(Term::Atom(Atom::Int(50)));

    let mut bindings = vec![];
    evaluate("let x = (* 5 10)", &mut bindings).unwrap();

    let reuse = evaluate("x", &mut bindings);
    assert_eq!(reuse, expected);
}

#[test]
fn test_recursive_session_bindings() {
    let mut bindings = vec![];
    evaluate("let fact = (\\n -> if (< n 2) then 1 else (* n (fact (- n 1))))", &mut bindings).unwrap();
    assert_evaluates_to_atom("fact 8", &mut bindings, Atom::Int(40320));
}

#[test]
fn test_type_of_using_session_bindings() {
    let mut bindings = vec![];
    evaluate("let x = (* 5 10)", &mut bindings).unwrap();
    let typ = type_of("x", &bindings, &mut GenTypeVar{n: 0}).1.unwrap();
    assert_eq!(typ, Type::Int);
}
