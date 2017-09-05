use std::collections::HashMap;
use std::collections::HashSet;

use interpret::structures::Type;
use interpret::structures::Type::*;

pub struct SumTypeDefs {
    constructors: HashMap<String, Constructor>,
    types: HashMap<String, String> //key: constructor name, value: type name
}

impl SumTypeDefs {
    pub fn type_of(&self, constr: Constructor) -> Result<Type, String> {
        match self.types.get(&constr.name) {
            Some(type_name) => Ok(Sum(type_name.clone())),
            None => Err(String::from(format!("Constructor {} does not exist", constr.name)))
        }
    }

    pub fn all_constructors(&self, typ: Type) -> HashSet<Constructor> {
        //TODO
        HashSet::new()
    }

    pub fn insert(&mut self, name: &str, constructors: Vec<(Constructor)>) -> Result<(), String> {
        /*
         * Failure cases:
         *   -there is already a constructor with that name
         * OK if type name already exists
         */
        for c in constructors {
            if let Ok(t) = self.type_of(c.clone()) {
                    return Err(String::from(format!("Ambiguous constructor: {} is already defined for type {}", c.name, t.name)));
            }
            self.types.insert(c.name.clone(), String::from(name));
            self.constructors.insert(c.name.clone(), c);
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
    pub name: String,
    pub typ: Box<Type>
}

impl Constructor {
    pub fn new(name: &str, typ: Type) -> Constructor {
        Constructor{name: String::from(name), typ: Box::new(typ)}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /* Helpers */
    fn maybe() -> Vec<Constructor> {
        vec![Constructor::new("Just", TypeVar(0)), Constructor::new("None", Unit)]
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
        let dup = vec![Constructor::new("Foo", Int), Constructor::new("None", Bool)];
        match defs.insert("Maybe", dup) {
            Ok(()) => panic!("Should not allow new sum type that reuses an existing constructor name!"),
            Err(msg) => assert_eq!(&msg, "Ambiguous constructor: None is already defined for type Maybe")
        }
    }

    #[test]
    fn test_find_type_of_constructor() {
        let mut defs = SumTypeDefs::new();
        defs.insert("Maybe", maybe()).unwrap();
        let bar = vec![Constructor::new("Foo", Int), Constructor::new("Bar", Bool)];
        defs.insert("Bar", bar).unwrap();

        let maybe_type = Sum(String::from("Maybe"));
        let bar_type = Sum(String::from("Bar"));
        assert_eq!(defs.type_of(Constructor::new("Just", TypeVar(0))), Ok(maybe_type));
        assert_eq!(defs.type_of(Constructor::new("Foo", Int)), Ok(bar_type));

        match defs.type_of(Constructor::new("Baz", Int)) {
            Ok(_) => panic!(),
            Err(msg) => assert_eq!(&msg, "Constructor Baz does not exist")
        }
    }

    #[test]
    fn test_get_all_constructors_for_type() {
        //TODO
    }
}
