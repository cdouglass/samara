use std::collections::HashMap;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Formatter;

use structures::Term;
use structures::Type;

#[derive(Debug)]
#[derive(Clone)]
pub struct ConstructorBinding {
    pub tag: String,
    pub arg_types: Vec<Type>,
    pub result_type: SumType
}

impl ConstructorBinding {
    // silly
    pub fn term(&self, index: usize) -> Term {
        let mut values = vec![];
        let x = String::from("x");
        for (i, _) in self.arg_types.iter().enumerate().rev() {
            values.push(Term::Var(i, x.clone()));
        }
        let mut term = Term::Sum(index, self.tag.clone(), values);
        for _ in &self.arg_types {
            term = Term::Lambda(Box::new(term), x.clone());
        }
        term
    }
}

#[derive(Debug)]
#[derive(Default)]
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
        for (tag, arg_types) in constructors {
            self.constructors.insert(tag.clone(), String::from(name));
            let binding = ConstructorBinding{tag: tag, arg_types: arg_types, result_type: new_typ.clone()};
            self.bindings.push(binding);
        }

        self.types.insert(String::from(name), new_typ);

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
    use structures::Type::*;

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
