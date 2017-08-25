use std::iter::Iterator;
use std::iter::Peekable;
use std::str::FromStr;

use interpret::tokenize::Lexer;
use interpret::tokenize::Token;
use interpret::tokenize::Keyword::*;

use interpret::types::Atom;
use interpret::types::Op;
use interpret::types::Term;

pub fn parse(mut tokens: &mut Peekable<Lexer>) -> Result<Term, String> {
    let mut stack = vec![];
    let mut context = vec![];
    parse_term(&mut tokens, &mut stack, &mut context)
}

fn parse_term(tokens: &mut Peekable<Lexer>, mut stack: &mut Vec<Token>, mut context: &mut Vec<String>) -> Result<Term, String> {
    let close_err = Err(String::from("Unexpected CLOSE delimiter"));
    let end_of_input_err = Err(String::from("Unexpected end of input"));
    let syntax_err = Err(String::from("Syntax error"));
    let lambda_syntax_err = Err(String::from("Syntax error in lambda expression"));
    let mut term_so_far = None;

    loop {
        let next_term_result = match tokens.peek().cloned() {
            Some(Token::Open) => {
                tokens.next();
                stack.push(Token::Open);
                parse_term(tokens, &mut stack, &mut context)
            },
            Some(Token::Close) => {
                match stack.last().cloned() {
                    Some(Token::Open) => {
                        stack.pop();
                        tokens.next();
                        break;
                    },
                    Some(Token::Keyword(Arrow)) => { break; },
                    _ => { return close_err; }
                }
            },
            Some(Token::Lambda) => {
                tokens.next();
                match (tokens.next(), tokens.next()) {
                    (Some(Token::Identifier(s)), Some(Token::Keyword(Arrow))) => {
                        context.push(s.clone());
                        stack.push(Token::Keyword(Arrow));
                        let body = parse_term(tokens, &mut stack, &mut context);
                        context.pop();
                        let result = body.map(|b| Term::Lambda(Box::new(b), s));
                        match stack.pop() {
                            Some(Token::Keyword(Arrow)) => { result },
                            _ => { return lambda_syntax_err; }
                        }
                    },
                    _ => lambda_syntax_err.clone()
                }
            },
            Some(Token::Keyword(k)) => {
                tokens.next();
                match k {
                    Arrow => { return lambda_syntax_err; },
                    If    => parse_conditional(tokens, &mut stack, &mut context),
                    True  => { Ok(Term::Atom(Atom::Bool(true))) },
                    False => { Ok(Term::Atom(Atom::Bool(false))) },
                    Let   => parse_let(tokens, &mut stack, &mut context),
                    k    => {
                        if stack.pop() == Some(Token::Keyword(k)) {
                            break;
                        } else { return syntax_err; }
                    },
                }
            },
            Some(Token::Identifier(ref s)) => {
                tokens.next();
                let mut stack = context.iter().rev();
                match stack.position(|x| x == s) {
                    Some(k) => Ok(Term::Var(k, s.clone())),
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
                if !stack.is_empty() {
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

fn parse_conditional(tokens: &mut Peekable<Lexer>, mut stack: &mut Vec<Token>, mut context: &mut Vec<String>) -> Result<Term, String> {
    stack.push(Token::Keyword(Then));
    let predicate = parse_term(tokens, stack, context);

    stack.push(Token::Keyword(Else));
    let true_case = parse_term(tokens, stack, context);

    let false_case = parse_term(tokens, stack, context);
    match (predicate, true_case, false_case) {
        (Ok(p), Ok(t), Ok(f)) => Ok(Term::Conditional(Box::new(p), Box::new(t), Box::new(f))),
        (Err(msg), _, _) | (_, Err(msg), _) | (_, _, Err(msg)) => Err(msg)
    }
}


fn parse_let(tokens: &mut Peekable<Lexer>, mut stack: &mut Vec<Token>, mut context: &mut Vec<String>) -> Result<Term, String> {
    let syntax_err = Err(String::from("Syntax error"));

    match tokens.next() {
        Some(Token::Identifier(s)) => {
            let name = s.clone();
            context.push(s.clone());

            stack.push(Token::Keyword(In));
            let value = parse_term(tokens, &mut stack, &mut context);

            let body = parse_term(tokens, &mut stack, &mut context);
            context.pop();
            match (value, body) {
                (Ok(v), Ok(b)) => Ok(Term::Let(name, Box::new(v), Box::new(b))),
                (Err(msg), _) | (_, Err(msg)) => Err(msg)
            }
        },
        _ => { return syntax_err; }
    }
}
