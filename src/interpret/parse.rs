use std::iter::Iterator;
use std::ops::Deref;

use interpret::tokenize::Lexer;
use interpret::tokenize::Token;

use interpret::types::Atom;
use interpret::types::Term;
use interpret::types::Type;

fn func2_from_string(s: &str) -> Result<Atom, String> {
    match s {
        "+"  => Ok(Atom::Func2( Box::new( move |x| { Box::new( move |y| {x + y})}))),
        "-"  => Ok(Atom::Func2( Box::new( move |x| { Box::new( move |y| {x - y})}))),
        "*"  => Ok(Atom::Func2( Box::new( move |x| { Box::new( move |y| {x * y})}))),
        "//" => Ok(Atom::Func2( Box::new( move |x| { Box::new( move |y| {x / y})}))),
        "%"  => Ok(Atom::Func2( Box::new( move |x| { Box::new( move |y| {x % y})}))),
        "^"  => Ok(Atom::Func2( Box::new( move |x| { Box::new( move |y| {x.pow(y.abs() as u32)})}))),
        _    => Err(format!("Unknown operator {}", s)) 
    }
}

pub fn parse(mut tokens: &mut Lexer) -> Result<Term, String> {

    fn parse_helper(tokens: &mut Lexer, is_subexpr: bool) -> Result<Term, String> {
        let close_msg = "Unexpected CLOSE delimiter";
        let mut term = None;
        let mut error = None;
        loop {
            let mut combined_term = None;
            let next_term = match tokens.next() {
                Some(Token::Open) => parse_helper(tokens, true),
                Some(Token::Int(s)) => {
                    s.parse::<i64>()
                        .map_err(|_| {String::from("Not a valid integer")})
                        .map(|i| Term::Atom(Atom::Int(i), Box::new(Type::Int)))
                },
                Some(Token::Op(s)) => {
                    func2_from_string(&s).map(|t| Term::Atom(t, Box::new(Type::Arrow(Box::new(Type::Int), Box::new(Type::Arrow(Box::new(Type::Int), Box::new(Type::Int)))))))
                },
                Some(Token::Close) => {
                    if !is_subexpr { error = Some(Err(String::from(close_msg))); }
                    break;
                },
                None => {
                    if is_subexpr { error = Some(Err(String::from("Unexpected end of input"))); }
                    break;
                },
            };

            match next_term {
                Ok(nt) => {
                    match term {
                        None => { combined_term = Some(nt); },
                        Some(tm) => {
                            // using clone to avoid borrowing tm
                            let term_typ = get_type(&tm);
                            let nt_typ = get_type(&nt);
                            match term_typ {
                                Type::Int => {
                                    error = Some(Err(String::from("Type error: Int is not a function")));
                                },
                                Type::Arrow(arg_type, output_type) => {
                                    if nt_typ == *arg_type {
                                        combined_term = Some(Term::App(
                                            Box::new(tm),
                                            Box::new(nt),
                                            output_type
                                        ));
                                    }
                                }
                            }
                        }
                    }
                    term = combined_term;
                },
                Err(msg) => { error = Some(Err(msg)); }
            }
        }

        match error {
            None => {
                match term {
                    Some(t) => Ok(t),
                    None => Err(String::from("Empty expression"))
                }
            }
            Some(err) => err
        }
    }

    parse_helper(&mut tokens, false)
}

fn get_type(term: &Term) -> Type {
    match *term {
        Term::Atom(_, ref t) | Term::App(_, _, ref t) => (*t.deref()).clone()
    }
}
