use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::str::FromStr;

use self::Op::*;

#[derive(Clone)]
#[derive(PartialEq)]
pub enum Atom {
    BuiltIn(Op),
    Int(i64),
}

#[derive(Clone)]
#[derive(PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp
}

#[derive(Clone)]
#[derive(PartialEq)]
pub enum Term {
    Atom(Atom),
    App(Box<Term>, Box<Term>),
    Lambda(Box<Term>, String), // uses de Bruijn indices internally, but keeps name for debugging
    Var(usize, String)
}

#[derive(Clone)]
#[derive(PartialEq)]
pub enum Type {
    Int,
    Arrow(Box<Type>, Box<Type>)
}

impl Debug for Op {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let s = match *self {
            Add => "+" ,
            Sub => "-" ,
            Mul => "*" ,
            Div => "//",
            Mod => "%" ,
            Exp => "^"
        };
        write!(f, "{}", s)
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Atom::Int(n) => write!(f, "{}", n),
            Atom::BuiltIn(ref op) => write!(f, "{:?}", op)
        }
    }
}

impl Debug for Term {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Term::Atom(ref a) => write!(f, "{:?}", a),
            Term::App(ref a, ref b) => write!(f, "{:?} {{{:?}}}", a, b),
            Term::Lambda(ref t, ref name) => write!(f, "\\{} -> ({:?})", name, t),
            Term::Var(_, ref name) => write!(f, "{}", name)
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Type::Int => write!(f, "Int"),
            Type::Arrow(ref a, ref b) => write!(f, "{:?} -> {:?}", *a, *b)
        }
    }
}

impl FromStr for Op {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+"  => Ok(Add),
            "-"  => Ok(Sub),
            "*"  => Ok(Mul),
            "//" => Ok(Div),
            "%"  => Ok(Mod),
            "^"  => Ok(Exp),
            _    => Err(format!("Unknown operator {}", s))
        }
    }
}
