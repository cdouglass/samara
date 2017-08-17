use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;

#[derive(PartialEq)]
#[derive(Clone)]
pub enum Type {
    Int,
    Arrow(Box<Type>, Box<Type>)
}

//#[derive(Clone)]
pub enum Atom {
    Int(i64),
    Func1(Box<Fn(i64)->i64>),
    Func2(Box<Fn(i64)->Box<Fn(i64)->i64>>)
}

#[derive(Debug)]
//#[derive(Clone)]
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
