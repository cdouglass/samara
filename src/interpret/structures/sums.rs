use std::collections::HashMap;
use std::collections::HashSet;

use interpret::structures::Type;
use interpret::structures::Type::*;

pub struct SumTypeDefs {
    constructors: HashMap<String, Constructor>
}

impl SumTypeDefs {
    pub fn type_of(&self, constr: Constructor) -> Type {
        //TODO
        Unit
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
            match self.constructors.get(&c.name) {
                Some(_) => { return Err(String::from(format!("Ambiguous constructor: {} is already defined for another type", c.name)));},
                None => { self.constructors.insert(c.name.clone(), c); }
            }
        }
        Ok(())
    }

    pub fn new() -> SumTypeDefs {
        SumTypeDefs{constructors: HashMap::new()}
    }
}

#[derive(Debug)]
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
            Err(msg) => assert_eq!(&msg, "Ambiguous constructor: None is already defined for another type")
        }
    }

    #[test]
    fn test_find_type_of_constructor() {
        //TODO
    }

    #[test]
    fn test_get_all_constructors_for_type() {
        //TODO
    }
}
