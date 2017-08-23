use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;

#[derive(PartialEq)]
#[derive(Debug)]
#[derive(Clone)]
pub enum Token {
    Open,
    Close,
    Lambda,
    Arrow,
    Identifier(String),
    Number(String),
    Operator(String)
}

pub struct Lexer<'a> {
    it: Peekable<Chars<'a>>
}

pub fn build_lexer(expr: &str) -> Lexer {
    Lexer {
        it: expr.chars().peekable(),
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        let mut token = None;

        fn is_operator(c: char) -> bool {
            match c {
                '+'|'-'|'*'|'/'|'^'|'%'|'<'|'>'|'=' => true,
                _ => false
            }
        }

        loop {
            // map to prevent borrow of self.it
            // https://stackoverflow.com/questions/26920789/unable-to-borrow-an-iterator-as-mutable-more-than-once-at-a-time
            let ch = self.it.peek().cloned();
            match token {
                Some(Token::Open) | Some(Token::Close) | Some(Token::Lambda) | Some(Token::Arrow) => {
                    return token;
                },
                Some(Token::Identifier(s)) => {
                    match ch {
                        Some(c) => {
                            if c.is_alphabetic() {
                                self.it.next();
                                token = Some(Token::Identifier(s + &c.to_string()));
                            } else { return Some(Token::Identifier(s)); }
                        },
                        None => { return Some(Token::Identifier(s)); }
                    }
                },
                Some(Token::Number(s)) => {
                    match ch {
                        Some(c) => {
                            if c.is_numeric() {
                                self.it.next();
                                token = Some(Token::Number(s + &c.to_string()));
                            } else { return Some(Token::Number(s)); }
                        },
                        None => { return Some(Token::Number(s)); }
                    }
                },
                Some(Token::Operator(s)) => {
                    if s == "->" {
                        token = Some(Token::Arrow);
                    } else {
                        match ch {
                            Some(c) => {
                                if is_operator(c) {
                                    self.it.next();
                                    token = Some(Token::Operator(s + &c.to_string()));
                                } else { return Some(Token::Operator(s)); }
                            },
                            None => { return Some(Token::Operator(s)); }
                        }
                    }
                },
                None => {
                    match ch {
                        Some('(')  => { token = Some(Token::Open) },
                        Some(')')  => { token = Some(Token::Close) },
                        Some('\\') => { token = Some(Token::Lambda) },
                        Some(c) => {
                            if is_operator(c) {
                                token = Some(Token::Operator(c.to_string()));
                            } else if c.is_alphabetic() {
                                token = Some(Token::Identifier(c.to_string()));
                            } else if c.is_numeric() {
                                token = Some(Token::Number(c.to_string()));
                            }
                        },
                        None => { return None; }
                    }
                    self.it.next();
                }
            }
        }
    }
}
