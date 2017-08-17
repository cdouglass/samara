use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;

#[derive(PartialEq)]
#[derive(Debug)]
#[derive(Clone)]
pub enum Token {
    Open,
    Close,
    Atom(String)
}

#[derive(PartialEq)]
enum TokenType {
    Int,
    Op,
    Var,
    Open,
    Close
}

fn token_type_from_char(&c: &char) -> Option<TokenType> {
    if c.is_numeric() {
        Some(TokenType::Int)
    } else if c.is_alphabetic() {
        Some(TokenType::Var)
    } else {
        match c {
            '('                     => Some(TokenType::Open),
            ')'                     => Some(TokenType::Close),
            '+'|'-'|'*'|'/'|'^'|'%' => Some(TokenType::Op),
            _                       => None
        }
    }
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
        let mut token_type = None;
        let mut token_so_far = None;
        loop {
            // map to prevent borrow of self.it
            // https://stackoverflow.com/questions/26920789/unable-to-borrow-an-iterator-as-mutable-more-than-once-at-a-time
            let c = self.it.peek().map(|c| {*c});
            match c {
                Some(ref ch) => {
                    match token_so_far {
                        Some(Token::Open) | Some(Token::Close) => { return token_so_far; },
                        Some(Token::Atom(s)) => {
                            if token_type == token_type_from_char(ch) {
                                token_so_far = Some(Token::Atom(s + &ch.to_string()));
                                self.it.next();
                            } else {
                                return Some(Token::Atom(s));
                            }
                        },
                        None => {
                            self.it.next();
                            match token_type_from_char(ch) {
                                Some(TokenType::Open) => { token_so_far = Some(Token::Open); },
                                Some(TokenType::Close) => { return Some(Token::Close); },
                                Some(tt) => {
                                    token_type = Some(tt);
                                    token_so_far = Some(Token::Atom(ch.to_string()));
                                },
                                None => { }
                            }
                        }
                    }
                },
                None => { return token_so_far; }
            }
        }
    }
}
