use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;
use std::str::FromStr;

#[derive(PartialEq)]
#[derive(Debug)]
#[derive(Clone)]
pub enum Token {
    Open,
    Close,
    Lambda,
    Keyword(Keyword),
    Identifier(String),
    Number(String),
    Operator(String)
}

#[derive(PartialEq)]
#[derive(Debug)]
#[derive(Clone)]
pub enum Keyword {
    Arrow,
    Assign,
    Let,
    In,
    If,
    Then,
    Else,
    True,
    False
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "->"    => Ok(Keyword::Arrow),
            "="     => Ok(Keyword::Assign),
            "let"   => Ok(Keyword::Let),
            "in"    => Ok(Keyword::In),
            "if"    => Ok(Keyword::If),
            "then"  => Ok(Keyword::Then),
            "else"  => Ok(Keyword::Else),
            "True"  => Ok(Keyword::True),
            "False" => Ok(Keyword::False),
            _       => Err(())
        }
    }
}

pub struct Lexer<'a> {
    it: Peekable<Chars<'a>>
}

pub fn build_lexer(expr: &str) -> Peekable<Lexer> {
    Lexer {
        it: expr.chars().peekable(),
    }.peekable()
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

        fn is_identifier(c: char) -> bool {
            c.is_alphabetic() || c == '_'
        }

        loop {
            // map to prevent borrow of self.it
            // https://stackoverflow.com/questions/26920789/unable-to-borrow-an-iterator-as-mutable-more-than-once-at-a-time
            let ch = self.it.peek().cloned();
            match token.clone() {
                Some(Token::Open) | Some(Token::Close) | Some(Token::Lambda) | Some(Token::Keyword(_)) => {
                    return token;
                },
                Some(Token::Identifier(s)) => {
                    if update_if_match(s, ch, &Token::Identifier, &is_identifier, &mut token) {
                        self.it.next();
                    } else { break; }
                },
                Some(Token::Number(s)) => {
                    if update_if_match(s, ch, &Token::Number, &(|x| x.is_numeric()), &mut token) {
                        self.it.next();
                    } else { break; }
                },
                Some(Token::Operator(s)) => {
                    if update_if_match(s, ch, &Token::Operator, &is_operator, &mut token) {
                        self.it.next();
                    } else { break; }
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
                        None => { break; }
                    }
                    self.it.next();
                }
            }
        }
        match token.clone() {
            Some(Token::Identifier(ref s)) | Some(Token::Operator(ref s)) => {
                match Keyword::from_str(s) {
                    Ok(k) => Some(Token::Keyword(k)),
                    _ => token
                }
            }
            _ => token
        }
    }
}

fn update_if_match(s: String, ch: Option<char>, constructor: &Fn(String) -> Token, predicate: &Fn(char) -> bool, token: &mut Option<Token>) -> bool {
    ch.map(|c| {
        let p = predicate(c);
        if p { *token = Some(constructor(s + &c.to_string())); }
        p
    }).unwrap_or(false)
}
