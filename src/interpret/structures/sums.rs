use std::collections::HashMap;

use interpret::structures::arrow;
use interpret::structures::Atom;
use interpret::structures::Term;
use interpret::structures::Type;
use interpret::structures::Type::*;

#[derive(Debug)]
pub struct ConstructorBinding {
    pub tag: String,
    pub term: Term,
    pub typ: Type
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

    pub fn type_info(&self, constr: String) -> Result<(SumType, Type), String> {
        for binding in &self.bindings {
            if binding.tag == constr {
                let typ = binding.typ.clone();
                //TODO don't unwrap
                let type_name = &self.constructors[&constr];
                let scheme = &self.types[type_name];
                return Ok((scheme.clone(), typ));
            }
        }
        Err(String::from(format!("Constructor {} does not exist", constr)))
    }

    pub fn add_type(&mut self, name: &str, constructors: Vec<(String, Type)>, params: Vec<Type>) -> Result<(), String> {

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

        for (c, t) in constructors {
            self.constructors.insert(c.clone(), String::from(name));

            if let Unit = t {
                let term = Term::Sum(c.clone(), Box::new(Term::Atom(Atom::Unit)));
                //TODO don't unwrap
                let binding = ConstructorBinding{tag: c, term: term, typ: Type::Sum(new_typ.clone())};
                self.bindings.push(binding);
            } else {
                let term = Term::Lambda(Box::new(Term::Sum(c.clone(), Box::new(Term::Var(0, String::from("x"))))), c.clone());
                let typ = arrow(t, Type::Sum(new_typ.clone()));
                let binding = ConstructorBinding{tag: c, term: term, typ: typ};
                self.bindings.push(binding);
            }
        }

        Ok(())
    }

    pub fn new() -> SumTypeDefs {
        SumTypeDefs{bindings: vec![], constructors: HashMap::new(), types: HashMap::new()}
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(Eq)]
#[derive(PartialEq)]
#[derive(Hash)]
pub struct SumType {
    pub name: String,
    pub variants: Vec<(String, Type)>,
    pub params: Vec<Type>
}

impl SumType {
    pub fn new(name: &str, constructors: Vec<(String, Type)>, params: Vec<Type>) -> SumType {
        let mut ctor_vec = constructors.clone();
        ctor_vec.sort_by_key(|x| x.0.clone());
        SumType{name: String::from(name), variants: ctor_vec, params: params}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /* Helpers */
    fn maybe() -> Vec<(String, Type)> {
        let mut variants = vec![];
        variants.push((String::from("Just"), TypeVar(0)));
        variants.push((String::from("None"), Unit));
        variants
    }

    fn bar() -> Vec<(String, Type)> {
        let mut variants = vec![];
        variants.push((String::from("Bar"), Bool));
        variants.push((String::from("Foo"), Int));
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
        dup.push((String::from("Foo"), Int));
        dup.push((String::from("None"), Bool));
        match defs.add_type("Foo", dup, vec![]) {
            Ok(()) => panic!("Should not allow new sum type that reuses an existing constructor name!"),
            Err(msg) => assert_eq!(&msg, "Ambiguous constructor: None is already defined for type Maybe")
        }
    }

    #[test]
    fn test_find_type_of_constructor() {
        let mut defs = SumTypeDefs::new();
        defs.add_type("Maybe", maybe(), vec![TypeVar(0)]).unwrap();
        defs.add_type("Bar", bar(), vec![]).unwrap();

        let maybe_type = SumType::new("Maybe", maybe(), vec![TypeVar(0)]);
        let bar_type = SumType::new("Bar", bar(), vec![]);
        assert_eq!(defs.type_info(String::from("Just")), Ok((maybe_type.clone(), arrow(TypeVar(0), Type::Sum(maybe_type)))));
        assert_eq!(defs.type_info(String::from("Foo")), Ok((bar_type.clone(), arrow(Int, Type::Sum(bar_type)))));

        match defs.type_info(String::from("Baz")) {
            Ok(_) => panic!(),
            Err(msg) => assert_eq!(&msg, "Constructor Baz does not exist")
        }
    }
}
