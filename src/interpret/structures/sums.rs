use std::collections::HashSet;

use interpret::structures::Type;
use interpret::structures::Type::*;

pub struct SumTypeDefs {
    //TODO
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
        Ok(())
    }

    pub fn new() -> SumTypeDefs {
        SumTypeDefs{}
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

    #[test]
    fn test_insert_valid_type() {
        let mut defs = SumTypeDefs::new();
        let constructors = vec![Constructor::new("Just", TypeVar(0)), Constructor::new("None", Unit)];
        match defs.insert("Maybe", constructors) {
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
        let constructors = vec![Constructor::new("Just", TypeVar(0)), Constructor::new("None", Unit)];
        defs.insert("Maybe", constructors).unwrap();
        let dup = vec![Constructor::new("Foo", Int), Constructor::new("None", Bool)];
        match defs.insert("Maybe", dup) {
            Ok(()) => panic!("Should not allow new sum type that reuses an existing constructor name!"),
            Err(msg) => assert_eq!(&msg, "Ambiguous constructor: None is already defined in type Maybe!")
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
