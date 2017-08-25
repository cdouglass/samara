use interpret::evaluate;
use interpret::parse;
use interpret::tokenize::build_lexer;
use interpret::tokenize::Token;
use interpret::types::Atom;
use interpret::types::Op;
use interpret::types::Term;

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

fn assert_evaluation_err(expr: &str, msg: &str) {
    match evaluate(expr) {
        Err(m) => { assert_eq!(m, msg) },
        result => {
            println!("Expected Err({}). Instead got {:?}", msg, result);
            panic!()
        }
    }
}

fn assert_evaluates_to_atom(expr: &str, expected: Atom) {
    match evaluate(expr).unwrap() {
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
    let ast = parse(&mut build_lexer("(\\x -> (\\y -> 3)) 2"));
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
    assert_evaluation_err("", "Unexpected end of input");
    assert_evaluation_err("()", "Unexpected end of input");
}

#[test]
fn test_unbalanced_delimiters() {
    assert_evaluation_err("(+ 5 8))", "Unexpected CLOSE delimiter");
    assert_evaluation_err("(+ 5 (8)", "Unexpected end of input");
}

/* Type errors */

#[test]
fn test_too_many_arguments() {
    assert_evaluation_err("(* 1 2 3)", "Type error");
}

/* Evaluating valid input */

#[test]
fn test_evaluate_int() {
    assert_evaluates_to_atom("42", Atom::Int(42));
}

#[test]
fn test_evaluate_op() {
    assert_evaluates_to_atom("+", Atom::BuiltIn(Op::Add));
    assert_evaluates_to_atom("//", Atom::BuiltIn(Op::Div));
}

#[test]
fn test_partially_apply_op() {
    let result = evaluate("(% 10)").unwrap();
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
    let expr = "((\\f -> (\\x -> f (\\y -> x x y)) (\\x -> f (\\y -> x x y))) (\\fct -> (\\n -> if (< n 3) then n else (* n (fct (- n 1)))))) 8";
    assert_evaluates_to_atom(expr, Atom::Int(40320));
}

#[test]
fn test_factorial_with_let() {
    let expr = "let fact (\\n -> if (< n 3) then n else (* n (fact (- n 1)))) in fact 8";
    assert_evaluates_to_atom(expr, Atom::Int(40320));
}

#[test]
fn test_fully_apply_op() {
    assert_evaluates_to_atom("(// 12 3)", Atom::Int(4));
}

#[test]
fn test_missing_outer_parens() {
    assert_evaluates_to_atom("+ 10 10", Atom::Int(20));
}
