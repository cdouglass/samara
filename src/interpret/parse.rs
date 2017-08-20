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
                        let body = parse_term(tokens, &mut paren_depth, &mut context);
                        context.pop();
                        body.map(|b| Term::Lambda(Box::new(b), s))
                    },
                    _ => Err(String::from("Syntax error in lambda expression"))
                }
            },
            Some(Token::Arrow) => Err(String::from("Syntax error: -> found outside lambda expression")),
            Some(Token::Identifier(ref s)) => {
                let mut stack = context.iter().rev();
                match stack.position(|x| x == s) {
                    Some(k) => Ok(Term::Var(k, s.clone())),
                    None => Err(String::from(format!("Error: Undefined variable {}", s)))
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
                Some(t) => { term_so_far = Some(Term::App(Box::new(t), Box::new(nt))); },
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
