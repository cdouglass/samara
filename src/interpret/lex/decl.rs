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

use self::Token::*;

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
                    break;
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
                        Some('=')  => { token = Some(Token::Eql) },
                        Some('|')  => { token = Some(Token::Separator) },
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
        if let Some(Token::Sum(ref s)) = token {
            if s == "Bool" {
                return Some(Token::Bool);
            } else if s == "Int" {
                return Some(Token::Int);
            } else if s == "Unit" {
                return Some(Token::Unit);
            }
        }
        token
    }
}

#[cfg(test)]
mod tests {
    use super::build_lexer;
    use super::Token;
    use super::Token::*;


    /* Helpers */
    fn assert_tokens(decl: &str, expected: Vec<Token>) {
        let tokens : Vec<Token> = build_lexer(decl).collect();
        assert_eq!(tokens, expected);
    }

    /* Tests */

    #[test]
    fn test_individual_tokens() {
        assert_tokens("(", vec![Open]);
        assert_tokens(")", vec![Close]);
        assert_tokens("|", vec![Separator]);
        assert_tokens("=", vec![Eql]);
        assert_tokens("->", vec![Arrow]);
        assert_tokens("Bool", vec![Bool]);
        assert_tokens("Int", vec![Int]);
        assert_tokens("Unit", vec![Unit]);
        assert_tokens("Foo", vec![Sum(String::from("Foo"))]);
        assert_tokens("foo", vec![Var(String::from("foo"))]);
    }
}
