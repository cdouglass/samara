use std::collections::HashMap;
use std::collections::HashSet;
use std::slice::Iter;

use interpret::infer::apply_substitution;
use interpret::structures::Type;
use interpret::structures::Type::*;

#[derive(Debug)]
pub struct SumTypeDefs {
    types: HashMap<String, SumTypeScheme>,
    by_constructor: HashMap<Constructor, (SumTypeScheme, Type)>
}

impl SumTypeDefs {
    pub fn type_from_name(&self, t: &str) -> Option<SumTypeScheme> {
        self.types.get(t).cloned()
    }

    pub fn type_info(&self, constr: Constructor) -> Result<(SumTypeScheme, Type), String> {
        match self.by_constructor.get(&constr) {
            Some(&(ref scheme, ref typ)) => Ok((scheme.clone(), typ.clone())),
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
        let new_typ = SumTypeScheme::new(name, constructors.clone(), universals);
        let mut new_by_constructor = HashMap::new();

        for typ in self.types.values() {
            if typ.name == name {
                return Err(String::from(format!("Type {} already exists", name)));
            }
        }

        for (c, typ) in constructors {
            if let Some(&(ref t, _)) = self.by_constructor.get(&c) {
                return Err(String::from(format!("Ambiguous constructor: {} is already defined for type {}", c.name, t.name)));
            }
            new_by_constructor.insert(c, (new_typ.clone(), typ.clone()));
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
pub struct SumTypeScheme {
    pub name: String,
    pub universals: Vec<usize>,
    pub variants: Vec<(Constructor, Type)>
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(Eq)]
#[derive(PartialEq)]
#[derive(Hash)]
pub struct SumType {
    pub name: String,
    pub variants: Vec<(Constructor, Type)>
}

impl SumTypeScheme {
    pub fn new(name: &str, constructors: HashSet<(Constructor, Type)>, universals: Vec<usize>) -> SumTypeScheme {
        let mut ctor_vec: Vec<(Constructor, Type)> = constructors.iter().cloned().collect();
        ctor_vec.sort_by_key(|x| x.0.name.clone());
        SumTypeScheme{name: String::from(name), variants: ctor_vec, universals: universals}
    }

    pub fn apply(&self, parameters: Vec<Type>) -> Result<Type, String> {
        if self.universals.len() != parameters.len() {
            return Err(String::from(format!("Type operator {} requires exactly {} parameter(s), got {}: {:?}", self.name, self.universals.len(), parameters.len(), parameters)));
        } else {
            let substitution = self.universals.iter().cloned().zip(parameters.iter().cloned()).collect();
            let mut variants = vec![];
            for &(ref constructor, ref typ) in self.variants.iter() {
                let mut t = typ.clone();
                apply_substitution(&substitution, &mut t);
                variants.push((constructor.clone(), t));
            }
            Ok(Type::Sum(SumType{name: self.name.clone(), variants: variants}))
        }
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

    /* Test SumTypeScheme */

    #[test]
    fn test_apply_sum_type_to_wrong_number_of_args() {
        let sum_type = SumTypeScheme::new("Maybe", maybe(), vec![0]);
        let err = sum_type.apply(vec![Type::Int, Type::Bool]).unwrap_err();
        assert_eq!(&err, "Type operator Maybe requires exactly 1 parameter(s), got 2: [Int, Bool]");

        let sum_type = SumTypeScheme::new("Bar", bar(), vec![]);
        let err = sum_type.apply(vec![Type::Int, Type::Bool]).unwrap_err();
        assert_eq!(&err, "Type operator Bar requires exactly 0 parameter(s), got 2: [Int, Bool]");
    }

    #[test]
    fn test_apply_nullary_sum_type() {
        let typ = SumTypeScheme::new("Bar", bar(), vec![]).apply(vec![]).unwrap();
        let mut variants = vec![];
        variants.push((Constructor::new("Bar"), Bool));
        variants.push((Constructor::new("Foo"), Int));
        let expected = Type::Sum(SumType{name: String::from("Bar"), variants: variants});
        assert_eq!(typ, expected);
    }

    #[test]
    fn test_apply_unary_sum_type() {
        let typ = SumTypeScheme::new("Maybe", maybe(), vec![0]).apply(vec![Type::Int]).unwrap();
        let mut variants = vec![(Constructor::new("Just"), Type::Int), (Constructor::new("None"), Type::Unit)];
        let expected = Type::Sum(SumType{name: String::from("Maybe"), variants: variants});
        assert_eq!(typ, expected);
    }

    #[test]
    fn test_apply_binary_sum_type() {
        let mut variants = HashSet::new();
        variants.insert((Constructor::new("Left"), Type::TypeVar(0)));
        variants.insert((Constructor::new("Right"), Type::TypeVar(1)));
        let scheme = SumTypeScheme::new("Either", variants, vec![0, 1]);

        let typ = scheme.apply(vec![Type::Int, Type::Bool]).unwrap();
        let expected_variants = vec![(Constructor::new("Left"), Type::Int), (Constructor::new("Right"), Type::Bool)];
        let expected = Type::Sum(SumType{name: String::from("Either"), variants: expected_variants});
        assert_eq!(typ, expected);
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

        let maybe_type = SumTypeScheme::new("Maybe", maybe(), vec![]);
        let bar_type = SumTypeScheme::new("Bar", bar(), vec![]);
        assert_eq!(defs.type_info(Constructor::new("Just")), Ok((maybe_type, TypeVar(0))));
        assert_eq!(defs.type_info(Constructor::new("Foo")), Ok((bar_type, Int)));

        match defs.type_info(Constructor::new("Baz")) {
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
