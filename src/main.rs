use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::io;
use std::io::Write;
use std::iter::Iterator;
use std::iter::Peekable;
use std::ops::Deref;
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

#[derive(PartialEq)]
#[derive(Clone)]
pub enum Type {
    Int,
    Arrow(Box<Type>, Box<Type>)
}

pub enum Atom {
    Int(i64),
    Func1(Box<Fn(i64)->i64>),
    Func2(Box<Fn(i64)->Box<Fn(i64)->i64>>)
}

#[derive(Debug)]
pub enum Term {
    Atom(Atom, Box<Type>),
    App(Box<Term>, Box<Term>, Box<Type>)
}

impl Debug for Atom {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Atom::Int(i)   => write!(f, "{}", i),
            Atom::Func1(_) => write!(f, "{}", "<function> : Int -> Int"),
            Atom::Func2(_) => write!(f, "{}", "<function> : Int -> Int -> Int"),
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Type::Int => write!(f, "Int"),
            Type::Arrow(ref a, ref b) => write!(f, "{:?} -> ({:?})", *a, *b)
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
    parse(&mut tokens).and_then(reduce)
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

fn parse(mut tokens: &mut Lexer) -> Result<Term, String> {

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

fn reduce(ast: Term) -> Result<Term, String> {
    match ast {
        Term::Atom(_, _) => Ok(ast),
        // would only have gotten here if types already validated
        Term::App(func, arg, typ) => {
            let f = reduce(*func);
            let a = reduce(*arg);
            match (f, a) {
                (Ok(Term::Atom(Atom::Func1(f), _)), Ok(Term::Atom(Atom::Int(n), _))) => Ok(Term::Atom(Atom::Int((*f)(n)), typ)),
                (Ok(Term::Atom(Atom::Func2(f), _)), Ok(Term::Atom(Atom::Int(n), _))) => Ok(Term::Atom(Atom::Func1((*f)(n)), typ)),
                _ => Err(String::from("Not implemented"))
            }
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
    use super::Atom;
    use super::Term;
    use super::Token;
    use super::build_lexer;
    use super::evaluate;

    #[test]
    fn test_lex() {
        let input = "(5  5 * / // +34()";
        let expected = [Token::Open, Token::Int(String::from("5")), Token::Int(String::from("5")), Token::Op(String::from("*")), Token::Op(String::from("/")), Token::Op(String::from("//")), Token::Op(String::from("+")), Token::Int(String::from("34")), Token::Open, Token::Close];
        let actual : Vec<Token>  = build_lexer(input).collect();
        assert!(actual == expected);
    }

    fn assert_evaluation_err(expr: &str, msg: &str) {
        match evaluate(expr) {
            Err(m) => { assert_eq!(m, msg) },
            //Err(m) | Ok((Err(m), _)) | Ok((Err(m), _)) => { assert_eq!(m, msg) },
            result => {
                println!("Expected Err({}). Instead got {:?}", msg, result);
                panic!()
            }
        }
    }

    fn assert_evaluates_to_atom(expr: &str, expected: Atom) {
        let result = evaluate(expr).unwrap();
        match (result, expected) {
            (Term::Atom(Atom::Int(i), _),   Atom::Int(j))     => assert_eq!(i, j),
            (Term::Atom(Atom::Func1(_), _), Atom::Func1(_))   => (),
            (Term::Atom(Atom::Func2(_), _), Atom::Func2(_))   => (),
            (r, e) => {
                println!("Expected {:?}. Instead got {:?}", r, e);
                panic!()
            }
        }
    }

    /* Syntax errors */

    #[test]
    fn test_empty_input() {
        assert_evaluation_err("", "Empty expression");
        assert_evaluation_err("()", "Empty expression");
    }

    #[test]
    fn test_unbalanced_delimiters() {
        assert_evaluation_err("(+ 5 8))", "Unexpected CLOSE delimiter");
        assert_evaluation_err("(+ 5 (8)", "Unexpected end of input");
    }

    #[test]
    fn test_missing_outer_parens() {
        assert_evaluates_to_atom("+ 10 10", Atom::Int(20));
    }

    /* Evaluating valid input */

    #[test]
    fn test_evaluate_int() {
        assert_evaluates_to_atom("42", Atom::Int(42));
    }

    #[test]
    fn test_evaluate_op() {
        let add = evaluate("+");
        let div = evaluate("//");
        match (add, div) {
            (Ok(Term::Atom(Atom::Func2(_), _)), Ok(Term::Atom(Atom::Func2(_), _))) => {},
            _ => panic!()
        }
    }

    #[test]
    fn test_partially_apply_op() {
        assert_evaluates_to_atom("(+ 10)", Atom::Func1(Box::new(|y| {y + 10})));
    }

    #[test]
    fn test_fully_apply_op() {
        assert_evaluates_to_atom("(// 12 3)", Atom::Int(4));
    }

    /* Type errors */

    #[test]
    fn test_too_many_arguments() {
        assert_evaluation_err("(* 1 2 3)", "Type error: Int is not a function");
    }
}
