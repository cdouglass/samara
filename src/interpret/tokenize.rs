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
    Keyword(Keyword),
    Constructor(String),
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
            match token {
                Some(Token::Open) | Some(Token::Close) | Some(Token::Lambda) | Some(Token::Keyword(_)) => {
                    return token;
                },
                Some(Token::Constructor(ref mut s)) => {
                    if update_if_match(s, ch, &is_identifier, &mut self.it) {
                    } else { break; }
                },
                Some(Token::Identifier(ref mut s)) => {
                    if update_if_match(s, ch, &is_identifier, &mut self.it) {
                    } else { break; }
                },
                Some(Token::Number(ref mut s)) => {
                    if update_if_match(s, ch, &(|x| x.is_numeric()), &mut self.it) {
                    } else { break; }
                },
                Some(Token::Operator(ref mut s)) => {
                    if update_if_match(s, ch, &is_operator, &mut self.it) {
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
                            } else if c.is_uppercase() {
                                token = Some(Token::Constructor(c.to_string()));
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
        match token {
            Some(Token::Identifier(ref s)) | Some(Token::Operator(ref s)) | Some(Token::Constructor(ref s)) => {
                use self::Keyword::*;
                use self::Token::*;
                match s.as_ref() {
                    "->"    => { return Some(Keyword(Arrow)); },
                    "="     => { return Some(Keyword(Assign)); },
                    "let"   => { return Some(Keyword(Let)); },
                    "in"    => { return Some(Keyword(In)); },
                    "if"    => { return Some(Keyword(If)); },
                    "then"  => { return Some(Keyword(Then)); },
                    "else"  => { return Some(Keyword(Else)); },
                    "True"  => { return Some(Keyword(True)); },
                    "False" => { return Some(Keyword(False)); },
                    _       => { }
                }
            },
            _ => { }
        }
        token
    }
}

fn update_if_match(s: &mut String, ch: Option<char>, predicate: &Fn(char) -> bool, it: &mut Peekable<Chars>) -> bool {
    ch.map(|c| {
        let p = predicate(c);
        if p {
            s.push(c);
            it.next();
        }
        p
    }).unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::build_lexer;
    use super::Token;

    #[test]
    fn test_lex() {
        let input = "(5  5 * / // +34()";
        let expected = [Token::Open, Token::Number(String::from("5")), Token::Number(String::from("5")), Token::Operator(String::from("*")), Token::Operator(String::from("/")), Token::Operator(String::from("//")), Token::Operator(String::from("+")), Token::Number(String::from("34")), Token::Open, Token::Close];
        let actual : Vec<Token>  = build_lexer(input).collect();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_constructors() {
        let input = "foo Bar bAZ";
        let expected = [Token::Identifier(String::from("foo")), Token::Constructor(String::from("Bar")), Token::Identifier(String::from("bAZ"))];
        let actual : Vec<Token> = build_lexer(input).collect();
        assert_eq!(actual, expected);
    }
}
