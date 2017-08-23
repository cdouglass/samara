use std::iter::Iterator;
use std::str::FromStr;

use interpret::tokenize::Lexer;
use interpret::tokenize::Token;

use interpret::types::Atom;
use interpret::types::Op;
use interpret::types::Term;

pub fn parse(mut tokens: &mut Lexer) -> Result<Term, String> {
    let mut paren_depth = 0;
    let mut context = vec![];
    parse_term(&mut tokens, &mut paren_depth, &mut context)
}

fn parse_term(tokens: &mut Lexer, mut paren_depth: &mut i64, mut context: &mut Vec<String>) -> Result<Term, String> {
    let close_msg = String::from("Unexpected CLOSE delimiter");
    let end_of_input_msg = String::from("Unexpected end of input");
    let mut term_so_far = None;

    loop {
        let next_term_result = match tokens.next() {
            Some(Token::Open) => {
                *paren_depth += 1;
                parse_term(tokens, &mut paren_depth, &mut context)
            },
            Some(Token::Close) => {
                if *paren_depth > 0 {
                    *paren_depth -= 1;
                    break;
                } else {
                    return Err(close_msg);
                }
            },
            Some(Token::Lambda) => {
                match (tokens.next(), tokens.next()) {
                    (Some(Token::Identifier(s)), Some(Token::Arrow)) => {
                        context.push(s.clone());
                        let pd0 = paren_depth.clone();
                        let body = parse_term(tokens, &mut paren_depth, &mut context);
                        context.pop();
                        let result = body.map(|b| Term::Lambda(Box::new(b), s));
                        if pd0 > *paren_depth {
                            // ensure that, on CLOSE, recursive call from OPEN
                            // case returns all the way to matching level
                            // instead of stopping here
                            return result;
                        } else { result }
                    },
                    _ => Err(String::from("Syntax error in lambda expression"))
                }
            },
            Some(Token::Arrow) => Err(String::from("Syntax error: -> found outside lambda expression")),
            Some(Token::Identifier(ref s)) => {
                if s == "if" {
                    parse_conditional(tokens, &mut paren_depth, &mut context)
                } else if s == "then" || s == "else" {
                        break;
                } else if s == "True" {
                    Ok(Term::Atom(Atom::Bool(true)))
                } else if s == "False" {
                        Ok(Term::Atom(Atom::Bool(false)))
                } else {
                    let mut stack = context.iter().rev();
                    match stack.position(|x| x == s) {
                        Some(k) => Ok(Term::Var(k, s.clone())),
                        None => Err(String::from(format!("Error: Undefined variable {}", s)))
                    }
                }
            },
            Some(Token::Number(s)) => {
                s.parse::<i64>()
                    .map_err(|_| String::from(format!("Invalid integer: {}", s)))
                    .map(|n| Term::Atom(Atom::Int(n)))
            },
            Some(Token::Operator(s)) => {
                Op::from_str(&s).map(|op| Term::Atom(Atom::BuiltIn(op)))
            },
            None => {
                if *paren_depth > 0 {
                    return Err(end_of_input_msg);
                } else { break; }
            }
        };

        match next_term_result {
            Ok(nt) => match term_so_far {
                Some(t) => {
                    println!("Application: Combining old term {:?} with new term {:?}", t, nt);
                    term_so_far = Some(Term::App(Box::new(t), Box::new(nt)));
                },
                None => { term_so_far = Some(nt); }
            },
            Err(msg) => { return Err(msg); }
        }
    }

    match term_so_far {
        Some(term) => Ok(term),
        None => Err(end_of_input_msg)
    }
}

fn parse_conditional(tokens: &mut Lexer, mut paren_depth: &mut i64, mut context: &mut Vec<String>) -> Result<Term, String> {
    let predicate = parse_term(tokens, paren_depth, context);
    let true_case = parse_term(tokens, paren_depth, context);
    let false_case = parse_term(tokens, paren_depth, context);
    match (predicate, true_case, false_case) {
        (Ok(p), Ok(t), Ok(f)) => Ok(Term::Conditional(Box::new(p), Box::new(t), Box::new(f))),
        (Err(msg), _, _) | (_, Err(msg), _) | (_, _, Err(msg)) => Err(msg)
    }
}
