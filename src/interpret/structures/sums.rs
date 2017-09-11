use std::collections::HashMap;
use std::collections::HashSet;
use std::slice::Iter;

use interpret::infer::apply_substitution;
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
    types: HashMap<String, SumTypeScheme>,
    constructors: HashMap<String, String> // key: constructor tag; value: type name. just for checking existence
}

impl SumTypeDefs {
    pub fn type_from_name(&self, t: &str) -> Option<SumTypeScheme> {
        self.types.get(t).cloned()
    }

    pub fn type_info(&self, constr: String) -> Result<(SumTypeScheme, Type), String> {
        for ref binding in self.bindings.iter() {
            if binding.tag == constr {
                let typ = binding.typ.clone();
                //TODO don't unwrap
                let type_name = self.constructors.get(&constr).unwrap();
                let ref scheme = self.types.get(type_name).unwrap();
                return Ok(((*scheme).clone(), typ));
            }
        }
        Err(String::from(format!("Constructor {} does not exist", constr)))
    }

    pub fn add_type(&mut self, name: &str, constructors: HashSet<(String, Type)>, universals: Vec<usize>) -> Result<(), String> {

        for typ in self.types.values() {
            if typ.name == name {
                return Err(String::from(format!("Type {} already exists", name)));
            }
        }

        for &(ref c, _) in constructors.iter() {
            if let Some(ref s) = self.constructors.get(c) {
                return Err(String::from(format!("Ambiguous constructor: {} is already defined for type {}", c, s)));
            }
        }

        let new_typ = SumTypeScheme::new(name, constructors.clone(), universals.clone());
        self.types.insert(String::from(name), new_typ.clone());

        for (c, t) in constructors {
            self.constructors.insert(c.clone(), String::from(name));

            if let Unit = t {
                let term = Term::Sum(c.clone(), Box::new(Term::Atom(Atom::Unit)));
                //TODO don't unwrap
                let typ = new_typ.apply(universals.iter().map(|n| TypeVar(*n)).collect()).unwrap();
                let binding = ConstructorBinding{tag: c, term: term, typ: typ};
                self.bindings.push(binding);
            } else {
                let term = Term::Lambda(Box::new(Term::Sum(c.clone(), Box::new(Term::Atom(Atom::Unit)))), c.clone());
                let typ = arrow(t, new_typ.apply(universals.iter().map(|n| TypeVar(*n)).collect()).unwrap());
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
// won't be hashable if any fields are hashsets
// TODO redo this completely more efficiently
pub struct SumTypeScheme {
    pub name: String,
    pub universals: Vec<usize>,
    pub variants: Vec<(String, Type)>
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(Eq)]
#[derive(PartialEq)]
#[derive(Hash)]
pub struct SumType {
    pub name: String,
    pub variants: Vec<(String, Type)>
}

impl SumTypeScheme {
    pub fn new(name: &str, constructors: HashSet<(String, Type)>, universals: Vec<usize>) -> SumTypeScheme {
        let mut ctor_vec: Vec<(String, Type)> = constructors.iter().cloned().collect();
        ctor_vec.sort_by_key(|x| x.0.clone());
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
    fn maybe() -> HashSet<(String, Type)> {
        let mut variants = HashSet::new();
        variants.insert((String::from("Just"), TypeVar(0)));
        variants.insert((String::from("None"), Unit));
        variants
    }

    fn bar() -> HashSet<(String, Type)> {
        let mut variants = HashSet::new();
        variants.insert((String::from("Bar"), Bool));
        variants.insert((String::from("Foo"), Int));
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
        variants.push((String::from("Bar"), Bool));
        variants.push((String::from("Foo"), Int));
        let expected = Type::Sum(SumType{name: String::from("Bar"), variants: variants});
        assert_eq!(typ, expected);
    }

    #[test]
    fn test_apply_unary_sum_type() {
        let typ = SumTypeScheme::new("Maybe", maybe(), vec![0]).apply(vec![Type::Int]).unwrap();
        let mut variants = vec![(String::from("Just"), Type::Int), (String::from("None"), Type::Unit)];
        let expected = Type::Sum(SumType{name: String::from("Maybe"), variants: variants});
        assert_eq!(typ, expected);
    }

    #[test]
    fn test_apply_binary_sum_type() {
        let mut variants = HashSet::new();
        variants.insert((String::from("Left"), Type::TypeVar(0)));
        variants.insert((String::from("Right"), Type::TypeVar(1)));
        let scheme = SumTypeScheme::new("Either", variants, vec![0, 1]);

        let typ = scheme.apply(vec![Type::Int, Type::Bool]).unwrap();
        let expected_variants = vec![(String::from("Left"), Type::Int), (String::from("Right"), Type::Bool)];
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
        dup.insert((String::from("Foo"), Int));
        dup.insert((String::from("None"), Bool));
        match defs.add_type("Foo", dup, vec![]) {
            Ok(()) => panic!("Should not allow new sum type that reuses an existing constructor name!"),
            Err(msg) => assert_eq!(&msg, "Ambiguous constructor: None is already defined for type Maybe")
        }
    }

    #[test]
    fn test_find_type_of_constructor() {
        let mut defs = SumTypeDefs::new();
        defs.add_type("Maybe", maybe(), vec![0]).unwrap();
        defs.add_type("Bar", bar(), vec![]).unwrap();

        let maybe_type = SumTypeScheme::new("Maybe", maybe(), vec![0]);
        let bar_type = SumTypeScheme::new("Bar", bar(), vec![]);
        assert_eq!(defs.type_info(String::from("Just")), Ok((maybe_type.clone(), arrow(TypeVar(0), maybe_type.apply(vec![TypeVar(0)]).unwrap()))));
        assert_eq!(defs.type_info(String::from("Foo")), Ok((bar_type.clone(), arrow(Int, bar_type.apply(vec![]).unwrap()))));

        match defs.type_info(String::from("Baz")) {
            Ok(_) => panic!(),
            Err(msg) => assert_eq!(&msg, "Constructor Baz does not exist")
        }
    }
}
