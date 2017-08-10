use std::io;
use std::io::Write;
use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;


const GREETING : &str = "---- samara 0.1.0 --------------------------------------------------------------";
const PROMPT : &str = "> ";
const CMD_MARKER : char = ':';
const USAGE : &str = "exit with :exit";

pub enum Command {
    Exit,
    Help,
    Unknown
}

#[derive(PartialEq)]
#[derive(Debug)]
pub enum Op {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // //
    Mod, // %
    Exp, // ^
    Other
}

#[derive(PartialEq)]
#[derive(Debug)]
pub enum Token {
    Open,
    Close,
    Int(i64),
    Op(Op)
}

#[derive(PartialEq)]
#[derive(Debug)]
enum TokenType {
    Int,
    Op,
    Open,
    Close,
    Invalid
}

pub enum Type {
    Int,
    Func(Box<Type>, Box<Type>)
}

pub enum Term {
    Int(i64),
    Func1(Box<Fn(i64)->i64>),
    Func2(Box<Fn(i64)->(Fn(i64)->i64)>),
    App(Box<Term>, Box<Term>)
}

impl ToString for Term {
    fn to_string(&self) -> String {
        //TODO
        match *self {
            Term::Int(i) => String::from(format!("{}", i)),
            Term::Func1(_) => String::from("<function> : Int -> Int"),
            Term::Func2(_) => String::from("<function> : Int -> Int -> Int"),
            _ => String::from("Oops!") // TODO should never happen
        }
    }
}

fn get_command(input: &str) -> Option<Command> {
    if input.chars().nth(0) == Some(CMD_MARKER) {
        match input.trim().trim_matches(CMD_MARKER) {
            "exit" => Some(Command::Exit),
            "help" => Some(Command::Help),
            _ => Some(Command::Unknown)
        }
    } else {
        None
    }
}

fn evaluate(expr: &str) -> String {
    let tokens = build_lexer(expr.trim());
    let ast = parse(tokens);
    let result = execute(ast);
    result.to_string()
}

fn token_type(&c: &char) -> TokenType {
    if c.is_numeric() {
        TokenType::Int
    } else {
        match c {
            '('                     => TokenType::Open,
            ')'                     => TokenType::Close,
            '+'|'-'|'*'|'/'|'^'|'%' => TokenType::Op,
            _                       => TokenType::Invalid
        }
    }
}

fn op_from_string(s: &str) -> Op {
    match s {
        "+"  => Op::Add,
        "-"  => Op::Sub,
        "*"  => Op::Mul,
        "//" => Op::Div,
        "%"  => Op::Mod,
        "^"  => Op::Exp,
        _    => Op::Other // TODO this shouldn't happen
    }
}

pub struct Lexer<'a> {
    it: Peekable<Chars<'a>>,
}

pub fn build_lexer(expr: &str) -> Lexer {
    Lexer {
        it: expr.chars().peekable(),
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        let mut tok_typ = TokenType::Invalid;
        let mut collected = String::new();
        let mut next_tok = None;
        let mut advance = false; // can't borrow self.it within the match on peek
        loop {
            if advance {
                self.it.next();
            }
            let c_ = self.it.peek();
            match c_ {
                None      => {
                    advance = true;
                    break;
                },
                Some(c)   => {
                        let tok_typ_new = token_type(c);
                        match tok_typ_new {
                            TokenType::Open => {
                                if tok_typ == TokenType::Invalid {
                                    next_tok = Some(Token::Open);
                                    advance = true;
                                } else {
                                    advance = false; // just flushing previous value
                                }
                                break;
                            },
                            TokenType::Close => {
                                if tok_typ == TokenType::Invalid {
                                    next_tok = Some(Token::Close);
                                    advance = true;
                                } else {
                                    advance = false;
                                }
                                break;
                            },
                            TokenType::Op | TokenType::Int => {
                                if tok_typ == TokenType::Invalid || tok_typ == tok_typ_new {
                                    tok_typ = tok_typ_new;
                                    advance = true;
                                    collected += &c.to_string();
                                } else {
                                    advance = false;
                                    break;
                                }
                            },
                            TokenType::Invalid => {
                                advance = true;
                                if tok_typ != TokenType::Invalid {
                                    break;
                                }
                            }
                        }
                    }
            }
        }
        if advance {
            self.it.next();
        }
        match tok_typ {
            TokenType::Int => {
                let n = collected.parse::<i64>().unwrap(); // already know all chars are numeric
                next_tok = Some(Token::Int(n));
            },
            TokenType::Op => {
                next_tok = Some(Token::Op(op_from_string(&collected)));
            },
            _ => {}
        }
        next_tok
    }
}

fn parse(tokens: Lexer) -> Term {
    let _ = tokens.last(); // will consume later
    // TODO
    Term::Int(0)
}

fn execute(term: Term) -> Term {
    // TODO
    term
}

fn main() {
    println!("{}", GREETING);

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut expr = String::new();
        io::stdin().read_line(&mut expr).unwrap();

        let cmd = get_command(&expr);
        match cmd {
            Some(Command::Exit) => break,
            Some(Command::Help) => println!("{}", USAGE),
            Some(Command::Unknown) => println!("Unknown command"),
            None => {
                let result = evaluate(&expr);
                println!("{}", result);
            }
        }

    }
}


#[cfg(test)]
mod tests {
    use super::Token;
    use super::Op;
    use super::build_lexer;
    #[test]
    fn test_lex() {
        let input = "(5  5 * / // +34()";
        println!("{}", input);
        let expected = [Token::Open, Token::Int(5), Token::Int(5), Token::Op(Op::Mul), Token::Op(Op::Other), Token::Op(Op::Div), Token::Op(Op::Add), Token::Int(34), Token::Open, Token::Close];

        let actual : Vec<Token>  = build_lexer(input).collect();
        assert!(actual == expected);
    }

}
