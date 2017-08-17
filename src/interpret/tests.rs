use interpret::evaluate;
use interpret::tokenize::build_lexer;
use interpret::tokenize::Token;
use interpret::types::Atom;
use interpret::types::Term;

#[test]
fn test_lex() {
    let input = "(5  5 * / // +34()";
    let expected = [Token::Open, Token::Int(String::from("5")), Token::Int(String::from("5")), Token::Op(String::from("*")), Token::Op(String::from("/")), Token::Op(String::from("//")), Token::Op(String::from("+")), Token::Int(String::from("34")), Token::Open, Token::Close];
    let actual : Vec<Token>  = build_lexer(input).collect();
    assert!(actual == expected);
}

fn assert_evaluation_err(expr: &str, msg: &str) {
    match evaluate(expr) {
        Err(m) => { assert_eq!(m, msg) },
        //Err(m) | Ok((Err(m), _)) | Ok((Err(m), _)) => { assert_eq!(m, msg) },
        result => {
            println!("Expected Err({}). Instead got {:?}", msg, result);
            panic!()
        }
    }
}

fn assert_evaluates_to_atom(expr: &str, expected: Atom) {
    let result = evaluate(expr).unwrap();
    match (result, expected) {
        (Term::Atom(Atom::Int(i), _),   Atom::Int(j))     => assert_eq!(i, j),
        (Term::Atom(Atom::Func1(_), _), Atom::Func1(_))   => (),
        (Term::Atom(Atom::Func2(_), _), Atom::Func2(_))   => (),
        (r, e) => {
            println!("Expected {:?}. Instead got {:?}", r, e);
            panic!()
        }
    }
}

/* Syntax errors */

#[test]
fn test_empty_input() {
    assert_evaluation_err("", "Empty expression");
    assert_evaluation_err("()", "Empty expression");
}

#[test]
fn test_unbalanced_delimiters() {
    assert_evaluation_err("(+ 5 8))", "Unexpected CLOSE delimiter");
    assert_evaluation_err("(+ 5 (8)", "Unexpected end of input");
}

#[test]
fn test_missing_outer_parens() {
    assert_evaluates_to_atom("+ 10 10", Atom::Int(20));
}

/* Evaluating valid input */

#[test]
fn test_evaluate_int() {
    assert_evaluates_to_atom("42", Atom::Int(42));
}

#[test]
fn test_evaluate_op() {
    let add = evaluate("+");
    let div = evaluate("//");
    match (add, div) {
        (Ok(Term::Atom(Atom::Func2(_), _)), Ok(Term::Atom(Atom::Func2(_), _))) => {},
        _ => panic!()
    }
}

#[test]
fn test_partially_apply_op() {
    assert_evaluates_to_atom("(+ 10)", Atom::Func1(Box::new(|y| {y + 10})));
}

#[test]
fn test_fully_apply_op() {
    assert_evaluates_to_atom("(// 12 3)", Atom::Int(4));
}

/* Type errors */

#[test]
fn test_too_many_arguments() {
    assert_evaluation_err("(* 1 2 3)", "Type error: Int is not a function");
}
