use std::collections::HashMap;
use std::collections::HashSet;

use interpret::structures::Type;
use interpret::structures::Type::*;

pub struct SumTypeDefs {
    constructors: HashMap<String, (Constructor, Type)>,
    types: HashMap<String, String> //key: constructor name, value: type name
}

impl SumTypeDefs {
    pub fn type_of(&self, constr: Constructor) -> Result<Type, String> {
        match self.types.get(&constr.name) {
            Some(type_name) => Ok(Sum(type_name.clone())),
            None => Err(String::from(format!("Constructor {} does not exist", constr.name)))
        }
    }

    pub fn all_constructors(&self, typ: &str) -> Result<HashSet<(Constructor, Type)>, String> {
        //TODO
        Ok(HashSet::new())
    }

    pub fn insert(&mut self, name: &str, constructors: Vec<(Constructor, Type)>) -> Result<(), String> {
        for (c, typ) in constructors {
            if let Ok(t) = self.type_of(c.clone()) {
                    return Err(String::from(format!("Ambiguous constructor: {} is already defined for type {}", c.name, t.name)));
            }
            self.types.insert(c.name.clone(), String::from(name));
            self.constructors.insert(c.name.clone(), (c, typ));
        }
        Ok(())
    }

    pub fn new() -> SumTypeDefs {
        SumTypeDefs{constructors: HashMap::new(), types: HashMap::new()}
    }
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(Eq)]
#[derive(PartialEq)]
#[derive(Hash)]
pub struct Constructor {
    pub name: String
}

impl Constructor {
    pub fn new(name: &str) -> Constructor {
        Constructor{name: String::from(name)}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /* Helpers */
    fn maybe() -> Vec<(Constructor, Type)> {
        vec![(Constructor::new("Just"), TypeVar(0)), (Constructor::new("None"), Unit)]
    }

    fn bar() -> Vec<(Constructor, Type)> {
        vec![(Constructor::new("Foo"), Int), (Constructor::new("Bar"), Bool)]
    }

    /* Tests */

    #[test]
    fn test_insert_valid_type() {
        let mut defs = SumTypeDefs::new();
        match defs.insert("Maybe", maybe()) {
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
        defs.insert("Maybe", maybe()).unwrap();
        let dup = vec![(Constructor::new("Foo"), Int), (Constructor::new("None"), Bool)];
        match defs.insert("Maybe", dup) {
            Ok(()) => panic!("Should not allow new sum type that reuses an existing constructor name!"),
            Err(msg) => assert_eq!(&msg, "Ambiguous constructor: None is already defined for type Maybe")
        }
    }

    #[test]
    fn test_find_type_of_constructor() {
        let mut defs = SumTypeDefs::new();
        defs.insert("Maybe", maybe()).unwrap();
        defs.insert("Bar", bar()).unwrap();

        let maybe_type = Sum(String::from("Maybe"));
        let bar_type = Sum(String::from("Bar"));
        assert_eq!(defs.type_of(Constructor::new("Just")), Ok(maybe_type));
        assert_eq!(defs.type_of(Constructor::new("Foo")), Ok(bar_type));

        match defs.type_of(Constructor::new("Baz")) {
            Ok(_) => panic!(),
            Err(msg) => assert_eq!(&msg, "Constructor Baz does not exist")
        }
    }

    #[test]
    fn test_get_all_constructors_for_type() {
        let mut defs = SumTypeDefs::new();
        defs.insert("Maybe", maybe()).unwrap();
        defs.insert("Bar", bar()).unwrap();

        let mut expected = HashSet::new();
        expected.insert((Constructor::new("Just"), TypeVar(0)));
        expected.insert((Constructor::new("None"), Unit));
        assert_eq!(Ok(expected), defs.all_constructors("Maybe"));

        let mut expected = HashSet::new();
        expected.insert((Constructor::new("Foo"), Int));
        expected.insert((Constructor::new("Bar"), Bool));
        assert_eq!(Ok(expected), defs.all_constructors("Baz"));

        assert_eq!(Err(String::from("No type Invalid")), defs.all_constructors("Invalid"));
    }
}

/*
 Next:
 - test_get_all_constructors_for_type
 - don't include contained type inside Constructor structure
 */
