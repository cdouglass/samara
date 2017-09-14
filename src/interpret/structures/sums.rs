use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;

use interpret::structures::arrow;
use interpret::structures::Atom;
use interpret::structures::Term;
use interpret::structures::Type;

#[derive(Debug)]
pub struct ConstructorBinding {
    pub tag: String,
    pub term: Term,
    arg_types: Vec<Type>,
    result_type: Type
}

impl ConstructorBinding {
    pub fn typ(&self) -> Type {
        let mut t = self.result_type.clone();
        for a in self.arg_types.iter().rev() {
            t = arrow(a.clone(), t.clone());
        }
        t
    }
}

#[derive(Debug)]
pub struct SumTypeDefs {
    pub bindings: Vec<ConstructorBinding>,
    types: HashMap<String, SumType>,
    constructors: HashMap<String, String> // key: constructor tag; value: type name. just for checking existence
}

impl SumTypeDefs {
    pub fn type_from_name(&self, t: &str) -> Option<SumType> {
        self.types.get(t).cloned()
    }

    pub fn add_type(&mut self, name: &str, constructors: Vec<(String, Vec<Type>)>, params: Vec<Type>) -> Result<(), String> {

        for typ in self.types.values() {
            if typ.name == name {
                return Err(String::from(format!("Type {} already exists", name)));
            }
        }

        for &(ref c, _) in &constructors {
            if let Some(t) = self.constructors.get(c) {
                return Err(String::from(format!("Ambiguous constructor: {} is already defined for type {}", c, t)));
            }
        }

        let new_typ = SumType::new(name, constructors.clone(), params.clone());
        self.types.insert(String::from(name), new_typ.clone());

        for (c, arg_types) in constructors {
            self.constructors.insert(c.clone(), String::from(name));
            let n = self.bindings.len();

            // still only uses first argument
            let term = if arg_types.is_empty() {
                Term::Sum(n, c.clone(), Box::new(Term::Atom(Atom::Unit)))
            } else {
                Term::Lambda(Box::new(Term::Sum(n, c.clone(), Box::new(Term::Var(0, String::from("x"))))), String::from("x"))
            };
            let binding = ConstructorBinding{tag: c, term: term, arg_types: arg_types, result_type: Type::Sum(new_typ.clone())};
            self.bindings.push(binding);
        }

        Ok(())
    }

    pub fn new() -> SumTypeDefs {
        SumTypeDefs{bindings: vec![], constructors: HashMap::new(), types: HashMap::new()}
    }
}

#[derive(Clone)]
#[derive(Eq)]
#[derive(PartialEq)]
#[derive(Hash)]
pub struct SumType {
    pub name: String,
    pub variants: Vec<(String, Vec<Type>)>, // constructor name, argument types
    pub params: Vec<Type>
}

impl SumType {
    pub fn new(name: &str, constructors: Vec<(String, Vec<Type>)>, params: Vec<Type>) -> SumType {
        let mut ctor_vec = constructors.clone();
        ctor_vec.sort_by_key(|x| x.0.clone());
        SumType{name: String::from(name), variants: ctor_vec, params: params}
    }
}

impl Debug for SumType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", Type::Sum(self.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use interpret::structures::Type::*;

    /* Helpers */
    fn maybe() -> Vec<(String, Vec<Type>)> {
        let mut variants = vec![];
        variants.push((String::from("Just"), vec![TypeVar(0)]));
        variants.push((String::from("None"), vec![Unit]));
        variants
    }

    /* Test SumTypeDefs */

    #[test]
    fn test_insert_valid_type() {
        let mut defs = SumTypeDefs::new();
        match defs.add_type("Maybe", maybe(), vec![]) {
            Err(msg) => {
                println!("Error: {}", msg);
                panic!()
            },
            Ok(()) => { }
        }
    }

    #[test]
    fn test_constructor_names_must_be_unique() {
        let mut defs = SumTypeDefs::new();
        defs.add_type("Maybe", maybe(), vec![]).unwrap();
        let mut dup = vec![];
        dup.push((String::from("Foo"), vec![Int]));
        dup.push((String::from("None"), vec![Bool]));
        match defs.add_type("Foo", dup, vec![]) {
            Ok(()) => panic!("Should not allow new sum type that reuses an existing constructor name!"),
            Err(msg) => assert_eq!(&msg, "Ambiguous constructor: None is already defined for type Maybe")
        }
    }
}
