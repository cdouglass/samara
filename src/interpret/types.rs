use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::str::FromStr;

use self::Op::*;

#[derive(Debug)]
#[derive(PartialEq)]
pub enum Atom {
    BuiltIn(Op),
    Int(i64),
}

#[derive(Debug)]
#[derive(PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp
}

#[derive(Debug)]
pub enum Term {
    Atom(Atom),
    App(Box<Term>, Box<Term>),
    Lambda(Box<Term>), // using de Bruijn indices instead of names
    Var(usize)
}

#[derive(Clone)]
#[derive(PartialEq)]
pub enum Type {
    Int,
    Arrow(Box<Type>, Box<Type>)
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Type::Int => write!(f, "Int"),
            Type::Arrow(ref a, ref b) => write!(f, "{:?} -> ({:?})", *a, *b)
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
