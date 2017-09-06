use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;

#[derive(PartialEq)]
#[derive(Debug)]
#[derive(Clone)]
pub enum Token {
    Open,
    Close,
    Separator,
    Eql,
    Arrow,
    Bool,
    Int,
    Unit,
    Sum(String),
    Var(String)
}

pub struct TokenStream<'a> {
    it: Peekable<Chars<'a>>
}

pub fn build_lexer(expr: &str) -> Peekable<TokenStream> {
    TokenStream {
        it: expr.chars().peekable(),
    }.peekable()
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        fn is_valid(c: char) -> bool {
            c.is_alphabetic() || c == '_' || c == '-' || c == '>'
        }

        let mut token = None;

        loop {
            let ch = self.it.peek().cloned();
            match token {
                Some(Token::Close) | Some(Token::Separator) | Some(Token::Eql) | Some(Token::Arrow) | Some(Token::Bool) | Some(Token::Int) | Some(Token::Unit) => {
                    return token;
                },
                Some(Token::Open) => {
                    match ch {
                        Some(')') => {
                            token = Some(Token::Unit);
                            self.it.next();
                        },
                        _ => {  return token; }
                    }
                },
                Some(Token::Sum(ref mut s)) | Some(Token::Var(ref mut s)) => {
                    match ch {
                        Some(c) if is_valid(c) => {
                            s.push(c);
                            self.it.next();
                        },
                        _ => { break; }
                    }
                },
                None => {
                    match ch {
                        Some('(')  => { token = Some(Token::Open) },
                        Some(')')  => { token = Some(Token::Close) },
                        Some('-') => {
                            self.it.next();
                            if let Some(&'>') = self.it.peek() {
                                self.it.next();
                                token = Some(Token::Arrow);
                            }
                        }
                        Some(c) => {
                            if c.is_uppercase() {
                                token = Some(Token::Sum(c.to_string()));
                            } else if is_valid(c) {
                                token = Some(Token::Var(c.to_string()));
                            }
                        },
                        None => { break; }
                    }
                    self.it.next();
                }
            }
        }
        token
    }
}

#[cfg(test)]
mod tests {
    //TODO
}
