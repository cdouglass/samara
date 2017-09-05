use std::collections::HashSet;

use interpret::structures::Type;
use interpret::structures::Type::*;

pub struct SumTypeDefs {
    //TODO
}

impl SumTypeDefs {
    pub fn type_of(constr: Constructor) -> Type {
        //TODO
        Unit
    }

    pub fn all_constructors(typ: Type) -> HashSet<Constructor> {
        //TODO
        HashSet::new()
    }
}

#[derive(Eq)]
#[derive(PartialEq)]
#[derive(Hash)]
pub struct Constructor {
    pub name: String,
    pub typ: Box<Type>
}
