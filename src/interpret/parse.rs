use std::iter::Iterator;
use std::str::FromStr;

use interpret::tokenize::Lexer;
use interpret::tokenize::Token;

use interpret::types::Atom;
use interpret::types::Term;

pub fn parse(mut tokens: &mut Lexer) -> Result<Term, String> {
    parse_term(&mut tokens, false)
}

fn parse_term(tokens: &mut Lexer, waiting_for_close: bool) -> Result<Term, String> {
    let close_msg = String::from("Unexpected CLOSE delimiter");
    let end_of_input_msg = String::from("Unexpected end of input");
    let mut term_so_far = None;

    loop {
        let next_term_result = match tokens.next() {
            Some(Token::Close) => {
                if waiting_for_close {
                    return finish_term(term_so_far);
                } else {
                    return Err(close_msg);
                }
            },
            Some(Token::Open) => parse_term(tokens, true),
            Some(Token::Atom(s)) => {
                Atom::from_str(&s).map(Term::Atom)
            },
            None => {
                if waiting_for_close {
                    return Err(end_of_input_msg);
                } else {
                    return finish_term(term_so_far);
                }
            },
        };

        match next_term_result {
            Ok(nt) => match term_so_far {
                Some(t) => { term_so_far = Some(Term::App(Box::new(t), Box::new(nt))); },
                None => { term_so_far = Some(nt); }
            },
            Err(msg) => { return Err(msg); }
        }
    }
}

fn finish_term(term: Option<Term>) -> Result<Term, String> {
    let empty_msg = String::from("Empty expression");
    match term {
        None => Err(empty_msg),
        Some(t) => Ok(t)
    }
}
