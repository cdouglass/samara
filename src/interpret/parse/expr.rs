use std::iter::Iterator;
use std::iter::Peekable;
use std::str::FromStr;

use interpret::lex::expr::TokenStream;
use interpret::lex::expr::Token;
use interpret::lex::expr::Keyword::*;

use interpret::structures::Atom;
use interpret::structures::Op;
use interpret::structures::Term;

pub fn parse(tokens: &mut Peekable<TokenStream>, mut token_stack: &mut Vec<Token>, mut identifier_stack: &mut Vec<String>) -> Result<Term, String> {
    let close_err = Err(String::from("Unexpected CLOSE delimiter"));
    let end_of_input_err = Err(String::from("Unexpected end of input"));
    let syntax_err = Err(String::from("Syntax error"));
    let lambda_syntax_err = Err(String::from("Syntax error in lambda expression"));
    let mut term_so_far = None;

    loop {
        let next_term_result = match tokens.peek().cloned() {
            Some(Token::Unit) => {
                tokens.next();
                return Ok(Term::Atom(Atom::Unit));
            },
            Some(Token::Open) => {
                tokens.next();
                token_stack.push(Token::Open);
                parse(tokens, &mut token_stack, &mut identifier_stack)
            },
            Some(Token::Close) => {
                match token_stack.last() {
                    Some(&Token::Open) => {
                        token_stack.pop();
                        tokens.next();
                        break;
                    },
                    Some(&Token::Keyword(Arrow)) => { break; },
                    _ => { return close_err; }
                }
            },
            Some(Token::Lambda) => {
                tokens.next();
                match (tokens.next(), tokens.next()) {
                    (Some(Token::Identifier(s)), Some(Token::Keyword(Arrow))) => {
                        identifier_stack.push(s.clone());
                        token_stack.push(Token::Keyword(Arrow));
                        let body = parse(tokens, &mut token_stack, &mut identifier_stack);
                        identifier_stack.pop();
                        let result = body.map(|b| Term::Lambda(Box::new(b), s));
                        match token_stack.pop() {
                            Some(Token::Keyword(Arrow)) => { result },
                            _ => { return lambda_syntax_err; }
                        }
                    },
                    _ => { return lambda_syntax_err; }
                }
            },
            Some(Token::Keyword(k)) => {
                tokens.next();
                match k {
                    Arrow => { return lambda_syntax_err; },
                    If    => parse_conditional(tokens, &mut token_stack, &mut identifier_stack),
                    True  => { Ok(Term::Atom(Atom::Bool(true))) },
                    False => { Ok(Term::Atom(Atom::Bool(false))) },
                    Let   => parse_let(tokens, &mut token_stack, &mut identifier_stack),
                    k    => {
                        if token_stack.pop() == Some(Token::Keyword(k)) {
                            break;
                        } else { return syntax_err; }
                    },
                }
            },
            Some(Token::Constructor(s)) => {
                tokens.next();
                //TODO look up
                //TODO hold a value
                Ok(Term::Sum(s))
            },
            Some(Token::Identifier(s)) => {
                tokens.next();
                let mut stack = identifier_stack.iter().rev();
                match stack.position(|x| x == &s) {
                    Some(k) => Ok(Term::Var(k, s)),
                    None => Err(String::from(format!("Error: Undefined variable {}", s)))
                }
            },
            Some(Token::Number(s)) => {
                tokens.next();
                s.parse::<i64>()
                    .map_err(|_| String::from(format!("Invalid integer: {}", s)))
                    .map(|n| Term::Atom(Atom::Int(n)))
            },
            Some(Token::Operator(s)) => {
                tokens.next();
                Op::from_str(&s).map(|op| Term::Atom(Atom::BuiltIn(op)))
            },
            None => {
                if !token_stack.is_empty() && *token_stack != vec![Token::Keyword(In)] {
                    return end_of_input_err;
                } else { break; }
            }
        };

        match next_term_result {
            Ok(nt) => match term_so_far {
                Some(t) => { term_so_far = Some(Term::App(Box::new(t), Box::new(nt))); },
                None => { term_so_far = Some(nt); }
            },
            Err(msg) => { return Err(msg); }
        }
    }

    match term_so_far {
        Some(term) => Ok(term),
        None => end_of_input_err
    }
}

fn parse_conditional(tokens: &mut Peekable<TokenStream>, mut token_stack: &mut Vec<Token>, mut identifier_stack: &mut Vec<String>) -> Result<Term, String> {
    token_stack.push(Token::Keyword(Then));
    let predicate = parse(tokens, token_stack, identifier_stack);

    token_stack.push(Token::Keyword(Else));
    let true_case = parse(tokens, token_stack, identifier_stack);

    let false_case = parse(tokens, token_stack, identifier_stack);
    match (predicate, true_case, false_case) {
        (Ok(p), Ok(t), Ok(f)) => Ok(Term::Conditional(Box::new(p), Box::new(t), Box::new(f))),
        (Err(msg), _, _) | (_, Err(msg), _) | (_, _, Err(msg)) => Err(msg)
    }
}

fn parse_let(tokens: &mut Peekable<TokenStream>, mut token_stack: &mut Vec<Token>, mut identifier_stack: &mut Vec<String>) -> Result<Term, String> {
    let syntax_err = Err(String::from("Syntax error"));

    match (tokens.next(), tokens.next()) {
        (Some(Token::Identifier(s)), Some(Token::Keyword(Assign))) => {
            identifier_stack.push(s.clone());
            token_stack.push(Token::Keyword(In));
            let value = parse(tokens, &mut token_stack, &mut identifier_stack)?;

            if *token_stack == vec![Token::Keyword(In)] {
                Ok(Term::Let(s, Box::new(value), None))
            } else {
                let body = Some(parse(tokens, &mut token_stack, &mut identifier_stack)?);
                identifier_stack.pop();
                Ok(Term::Let(s, Box::new(value), body.map(Box::new)))
            }
        },
        _ => syntax_err
    }
}

#[cfg(test)]
mod tests {
    use super::parse;
    use interpret::lex::build_lexer;
    use interpret::lex::TokenStream as TS;
    use interpret::lex::expr::TokenStream;
    use interpret::structures::Atom;
    use interpret::structures::Term;

    fn assert_parse(expr: &str, expected: Term) {
        let mut token_stack = vec![];
        let mut ids = vec![];
        let mut tokens = match build_lexer(expr) {
            TS::Expr(ts) => ts,
            _ => panic!()
        };
        match parse(&mut tokens, &mut token_stack, &mut ids) {
            Err(msg) => {
                println!("Expected term {:?} but got error {}", expected, msg);
                panic!()
            },
            Ok(term) => assert_eq!(term, expected)
        }
    }

    fn assert_parse_err(expr: &str, msg: &str) {
        let mut token_stack = vec![];
        let mut ids = vec![];
        let mut tokens = match build_lexer(expr) {
            TS::Expr(ts) => ts,
            _ => panic!()
        };
        match parse(&mut tokens, &mut token_stack, &mut ids) {
            Err(m) => assert_eq!(m, msg),
            Ok(term) => {
                println!("Expected parse error {} but got success {:?}", msg, term);
                panic!()
            }
        }
    }

    #[test]
    fn test_parses_lambda_application() {
        let mut token_stream = match build_lexer("(\\x -> (\\y -> 3)) 2") {
            TS::Expr(ts) => ts,
            _ => panic!()
        };
        let ast = parse(&mut token_stream, &mut vec![], &mut vec![]);
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

    #[test]
    fn test_parses_unit() {
        assert_parse("()", Term::Atom(Atom::Unit));
    }

    #[test]
    fn test_empty_input() {
        assert_parse_err("", "Unexpected end of input");
    }

    #[test]
    fn test_unbalanced_delimiters() {
        assert_parse_err("(+ 5 8))", "Unexpected CLOSE delimiter");
        assert_parse_err("(+ 5 (8)", "Unexpected end of input");
    }
}