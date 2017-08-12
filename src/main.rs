use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
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
#[derive(Clone)]
pub enum Token {
    Open,
    Close,
    Int(String),
    Op(String)
}

pub enum Type {
    Int,
    Func(Box<Type>, Box<Type>)
}

pub enum Term {
    Int(i64),
    Func1(Box<Fn(i64)->i64>),
    Func2(Box<Fn(i64)->Box<Fn(i64)->i64>>)
}

impl Debug for Term {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Term::Int(i)   => write!(f, "{}", i),
            Term::Func1(_) => write!(f, "{}", "<function> : Int -> Int"),
            Term::Func2(_) => write!(f, "{}", "<function> : Int -> Int -> Int"),
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

fn evaluate(expr: &str) -> Result<Term, String> {
    let mut tokens = build_lexer(expr.trim());
    parse(&mut tokens).and_then(execute)
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

fn func2_from_string(s: &str) -> Result<Term, String> {
    match s {
        "+"  => Ok(Term::Func2( Box::new( move |x| { Box::new( move |y| {x + y})}))),
        "-"  => Ok(Term::Func2( Box::new( move |x| { Box::new( move |y| {x - y})}))),
        "*"  => Ok(Term::Func2( Box::new( move |x| { Box::new( move |y| {x * y})}))),
        "//" => Ok(Term::Func2( Box::new( move |x| { Box::new( move |y| {x / y})}))),
        "%"  => Ok(Term::Func2( Box::new( move |x| { Box::new( move |y| {x % y})}))),
        "^"  => Ok(Term::Func2( Box::new( move |x| { Box::new( move |y| {x.pow(y.abs() as u32)})}))),
        _    => Err(format!("Unknown operator {}", s))
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

pub enum Tree<T> {
    Leaf(T),
    Branch(Vec<Tree<T>>)
}

fn parse(mut tokens: &mut Lexer) -> Result<Tree<Term>, String> {

    fn parse_helper(tokens: &mut Lexer) -> Result<Tree<Term>, String> {
        let close_msg = String::from("Unexpected CLOSE delimiter");
        match tokens.next() {
            Some(Token::Open)   => {
                let mut subtrees : Vec<Tree<Term>> = vec![];
                loop {
                    let subtree = parse_helper(tokens);
                    match subtree {
                        // I don't like this way of doing it
                        Err(msg) => {
                            if msg == close_msg {
                                break;
                            } else {
                                return Err(msg);
                            }
                        },
                        Ok(st) => {
                            subtrees.push(st);
                        }
                    }
                }
                Ok(Tree::Branch(subtrees))
            },
            Some(Token::Int(s)) => {
                s.parse::<i64>()
                    .map_err(|_| {String::from("Not a valid integer")})
                    .map(Term::Int)
                    .map(Tree::Leaf)
            },
            Some(Token::Op(s))  => {
                func2_from_string(&s).map(Tree::Leaf)
            },
            Some(Token::Close)  => Err(close_msg),
            None                => Err(String::from("Unexpected end of input. Unclosed delimiter?"))
        }
    }

    let ast = parse_helper(&mut tokens);
    match tokens.next() {
        None => ast,
        _    => Err(String::from("Characters left over"))
    }
}

fn apply(func: Result<Term, String>, arg: Result<Term, String>) -> Result<Term, String> {
    let i = match arg {
        Ok(Term::Int(j)) => Ok(j),
        _ => Err(format!("Type error: expected Int, got {:?} ", arg))
    };
    match (func, i) {
        (Ok(_),              Err(msg)) => Err(msg),
        (Ok(Term::Func1(f)), Ok(n))    => Ok(Term::Int((*f)(n))),
        (Ok(Term::Func2(f)), Ok(n))    => Ok(Term::Func1((*f)(n))),
        (Ok(f), _)    => Err(format!("Type error: {:?} is not a function", f)),
        (Err(msg), _) => Err(msg)
    }
}

fn execute(ast: Tree<Term>) -> Result<Term, String> {
    match ast {
        Tree::Leaf(term) => Ok(term),
        Tree::Branch(ts) => {
            let subtrees = ts.into_iter();
            let mut terms = subtrees.map(execute);
            let init = match terms.next() {
                Some(term) => term,
                None       => Err(String::from("Empty expression"))
            };
            terms.fold(init, apply)
        }
    }
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
                match result {
                    Ok(term) => println!("{:?}", term),
                    Err(msg) => println!("{}", msg)
                }
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::Term;
    use super::Token;
    use super::build_lexer;
    use super::evaluate;

    #[test]
    fn test_lex() {
        let input = "(5  5 * / // +34()";
        println!("{}", input);
        let expected = [Token::Open, Token::Int(String::from("5")), Token::Int(String::from("5")), Token::Op(String::from("*")), Token::Op(String::from("/")), Token::Op(String::from("//")), Token::Op(String::from("+")), Token::Int(String::from("34")), Token::Open, Token::Close];
        let actual : Vec<Token>  = build_lexer(input).collect();
        assert!(actual == expected);
    }

    fn assert_evaluation_err(expr: &str, msg: &str) {
        match evaluate(expr) {
            Err(m) => { assert_eq!(m, msg) },
            _        => panic!()
        }
    }

    /* Syntax errors */

    #[test]
    fn test_empty_input() {
        assert_evaluation_err("", "Unexpected end of input. Unclosed delimiter?");
        assert_evaluation_err("()", "Empty expression");
    }

    #[test]
    fn test_extra_close_delimiter() {
        assert_evaluation_err("(+ 5 8))", "Characters left over");
    }

    #[test]
    fn test_missing_outer_parens() {
        assert_evaluation_err("+ 10 10", "Characters left over");
    }

    /* Evaluating valid input */

    #[test]
    fn test_evaluate_int() {
        let result = evaluate("42");
        match result {
            Ok(Term::Int(i)) => assert_eq!(i, 42),
            _                => panic!()
        }
    }

    #[test]
    fn test_evaluate_op() {
        let add = evaluate("+");
        let div = evaluate("//");
        match (add, div) {
            (Ok(Term::Func2(_)), Ok(Term::Func2(_))) => {},
            _                                        => panic!()
        }
    }

    #[test]
    fn test_partially_apply_op() {
        let result = evaluate("(+ 10)");
        match result {
            Ok(Term::Func1(_)) => {},
            _                  => panic!()
        }
    }

    #[test]
    fn test_fully_apply_op() {
        let result = evaluate("(// 12 3)");
        match result {
            Ok(Term::Int(i)) => assert_eq!(i, 4),
            _                => panic!()
        }
    }

    /* Type errors */

    #[test]
    fn test_too_many_arguments() {
        assert_evaluation_err("(* 1 2 3)", "Type error: 2 is not a function");
    }
}
