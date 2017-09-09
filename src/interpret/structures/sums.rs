use std::collections::HashMap;
use std::collections::HashSet;
use std::slice::Iter;

use interpret::structures::Type;
use interpret::structures::Type::*;

#[derive(Debug)]
pub struct SumTypeDefs {
    types: HashMap<String, SumType>,
    by_constructor: HashMap<Constructor, SumType>
}

impl SumTypeDefs {
    pub fn type_from_name(&self, t: &str) -> Option<Type> {
        self.types.get(t).cloned().map(Type::Sum)
    }

    pub fn type_of(&self, constr: Constructor) -> Result<SumType, String> {
        match self.by_constructor.get(&constr) {
            Some(typ) => Ok(typ.clone()),
            None => Err(String::from(format!("Constructor {} does not exist", constr.name)))
        }
    }

    pub fn all_constructors(&self, type_name: &str) -> Result<HashSet<(Constructor, Type)>, String> {
        match self.types.get(type_name) {
            Some(typ) => {
                let vs: Iter<(Constructor, Type)> = typ.variants.iter();
                let variants: HashSet<(Constructor, Type)> = vs.cloned().collect();
                Ok(variants)
            },
            None => Err(String::from(format!("Type {} does not exist", type_name)))
        }
    }

    pub fn add_type(&mut self, name: &str, constructors: HashSet<(Constructor, Type)>, universals: Vec<usize>) -> Result<(), String> {
        let new_typ = SumType::new(name, constructors.clone(), universals);
        let mut new_by_constructor = HashMap::new();

        for typ in self.types.values() {
            if typ.name == name {
                return Err(String::from(format!("Type {} already exists", name)));
            }
        }

        for (c, typ) in constructors {
            if let Ok(t) = self.type_of(c.clone()) {
                    return Err(String::from(format!("Ambiguous constructor: {} is already defined for type {}", c.name, t.name)));
            }
            new_by_constructor.insert(c, new_typ.clone());
        }

        self.types.insert(String::from(name), new_typ);
        for (c, typ) in new_by_constructor {
            self.by_constructor.insert(c, typ);
        }

        Ok(())
    }

    pub fn new() -> SumTypeDefs {
        SumTypeDefs{by_constructor: HashMap::new(), types: HashMap::new()}
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

#[derive(Debug)]
#[derive(Clone)]
#[derive(Eq)]
#[derive(PartialEq)]
#[derive(Hash)]
// won't be hashable if any fields are hashsets
// TODO redo this completely more efficiently
pub struct SumType {
    pub name: String,
    pub universals: Vec<usize>,
    pub variants: Vec<(Constructor, Type)>
}

impl SumType {
    pub fn new(name: &str, constructors: HashSet<(Constructor, Type)>, universals: Vec<usize>) -> SumType {
        let mut ctor_vec: Vec<(Constructor, Type)> = constructors.iter().cloned().collect();
        ctor_vec.sort_by_key(|x| x.0.name.clone());
        SumType{name: String::from(name), variants: ctor_vec, universals: universals}
    }

    pub fn universals(&self) -> HashSet<usize> {
        self.universals.iter().cloned().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /* Helpers */
    fn maybe() -> HashSet<(Constructor, Type)> {
        let mut variants = HashSet::new();
        variants.insert((Constructor::new("Just"), TypeVar(0)));
        variants.insert((Constructor::new("None"), Unit));
        variants
    }

    fn bar() -> HashSet<(Constructor, Type)> {
        let mut variants = HashSet::new();
        variants.insert((Constructor::new("Bar"), Bool));
        variants.insert((Constructor::new("Foo"), Int));
        variants
    }

    /* Tests */

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
        let mut dup = HashSet::new();
        dup.insert((Constructor::new("Foo"), Int));
        dup.insert((Constructor::new("None"), Bool));
        match defs.add_type("Foo", dup, vec![]) {
            Ok(()) => panic!("Should not allow new sum type that reuses an existing constructor name!"),
            Err(msg) => assert_eq!(&msg, "Ambiguous constructor: None is already defined for type Maybe")
        }
    }

    #[test]
    fn test_find_type_of_constructor() {
        let mut defs = SumTypeDefs::new();
        defs.add_type("Maybe", maybe(), vec![]).unwrap();
        defs.add_type("Bar", bar(), vec![]).unwrap();

        let maybe_type = SumType::new("Maybe", maybe(), vec![]);
        let bar_type = SumType::new("Bar", bar(), vec![]);
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
        defs.add_type("Maybe", maybe(), vec![]).unwrap();
        defs.add_type("Baz", bar(), vec![]).unwrap();

        let mut expected = HashSet::new();
        expected.insert((Constructor::new("Just"), TypeVar(0)));
        expected.insert((Constructor::new("None"), Unit));
        assert_eq!(defs.all_constructors("Maybe"), Ok(expected));

        let mut expected = HashSet::new();
        expected.insert((Constructor::new("Foo"), Int));
        expected.insert((Constructor::new("Bar"), Bool));
        assert_eq!(defs.all_constructors("Baz"), Ok(expected));

        assert_eq!(Err(String::from("Type Invalid does not exist")), defs.all_constructors("Invalid"));
    }
}
