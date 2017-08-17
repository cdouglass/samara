use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;

#[derive(PartialEq)]
#[derive(Debug)]
#[derive(Clone)]
pub enum Token {
    Open,
    Close,
    Int(String),
    Op(String)
}

fn token_from_char(&c: &char) -> Option<Token> {
    let s = c.to_string();
    if c.is_numeric() {
        Some(Token::Int(s))
    } else {
        match c {
            '('                     => Some(Token::Open),
            ')'                     => Some(Token::Close),
            '+'|'-'|'*'|'/'|'^'|'%' => Some(Token::Op(s)),
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
        let mut next_tok = None;
        let mut advance = false;
        loop {
            if advance { self.it.next(); }
            let c = self.it.peek();
            if c == None { break; }
            let partial_tok = c.and_then(token_from_char);
            match (next_tok.clone(), partial_tok.clone()) {
                (None, Some(Token::Open)) | (None, Some(Token::Close)) => {
                    next_tok = partial_tok;
                    advance = true;
                    break;
                },
                (None, Some(Token::Op(_))) | (None, Some(Token::Int(_))) => {
                    next_tok = partial_tok;
                    advance = true;
                },

                (Some(Token::Op(s)), Some(Token::Op(c))) => {
                    advance = true;
                    next_tok = Some(Token::Op(s + &c.to_string()));
                },
                (Some(Token::Int(s)), Some(Token::Int(c))) => {
                    advance = true;
                    next_tok = Some(Token::Int(s + &c.to_string()));
                },

                (None, None) => {
                    advance = true;
                },
                (_, None) => {
                    advance = true;
                    break;
                },
                _ => {
                    advance = false;
                    break;
                },
            }
        }
        if advance { self.it.next(); }
        next_tok
    }
}
