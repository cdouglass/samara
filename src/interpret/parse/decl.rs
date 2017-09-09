use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::Iterator;
use std::iter::Peekable;

use interpret::infer::GenTypeVar;
use interpret::lex::decl::Token;
use interpret::lex::decl::TokenStream;
use interpret::structures::arrow;
use interpret::structures::Type;
use interpret::structures::sums::Constructor;
use interpret::structures::sums::SumType;

pub fn parse(mut tokens: &mut Peekable<TokenStream>, gen: &mut GenTypeVar) -> Result<SumType, String> {
    let name = get_sum(tokens, "Type declaration must begin with an uppercase name")?;
    let mut type_vars = HashMap::new();
    let mut variants = HashSet::new();

    loop {
        match tokens.next() {
            Some(Token::Var(name)) => {
                type_vars.insert(name, gen.next().unwrap());
            },
            Some(Token::Eql) => {
                break;
            },
            _ => {
                return Err(String::from("Invalid token in left-hand side of type declaration"));
            }
        }
    }

    loop {
        let variant = parse_variant(tokens, &type_vars)?;
        variants.insert(variant);
        if let None = tokens.peek() { break; }
    }

    let mut universals = HashSet::new();
    for typ in type_vars.values() {
        if let Type::TypeVar(n) = *typ {
            universals.insert(n);
        }
    }

    Ok(SumType::new(&name, variants, universals))
}

// eventually must also get SumTypeDefs as arg
// expects, in order:
// * Sum(s) - the name of the type
// * any number of Var(s) - type variables that declared for use on the right-hand side
// * Eq
// * any number of types separated by Separator

// use of undeclared typevar on right-hand side of declaration is an error
// returns at Token::Close, end of input, or Separator
// caller should then check token stack is empty - if it's not, this is always an error
// Eq is an error as well
//
// trickiness: infix arrow
// luckily it's the only infix we need here...
fn parse_variant(mut tokens: &mut Peekable<TokenStream>, vars: &HashMap<String, Type>) -> Result<(Constructor, Type), String> {
    let name = get_sum(tokens, "Missing constructor in right-hand side of type declaration")?;
    let mut token_stack = vec![];
    let typ = parse_type(tokens, &mut token_stack, vars)?;

    if !token_stack.is_empty() {
        return Err(String::from("Unexpected end of input"));
    }

    Ok((Constructor{name: name}, typ))
}

fn parse_type(mut tokens: &mut Peekable<TokenStream>, mut token_stack: &mut Vec<Token>, vars: &HashMap<String, Type>) -> Result<Type, String> {

    let mut typ = None;

    loop {
        match tokens.next() {
            Some(Token::Open) => {
                token_stack.push(Token::Open);
            },
            Some(Token::Close) => {
                match token_stack.pop() {
                    Some(_) => { break; },
                    None => {
                        return Err(String::from("Unexpected CLOSE delimiter"));
                    }
                }
            },
            Some(Token::Separator) => { break; },
            Some(Token::Eql) => {
                return Err(String::from("Unexpected token = in right-hand side of type declaration"));
            },
            Some(Token::Arrow) => {
                match typ {
                    Some(input_type) => {
                        let output_type = parse_type(tokens, token_stack, vars)?;
                        typ = Some(arrow(input_type, output_type));
                    },
                    None => {
                        return Err(String::from("Unexpected token ->. Must come between input and output types."));
                    }
                }

            },
            Some(Token::Bool) => {
                typ = Some(Type::Bool);
            },
            Some(Token::Int) => {
                typ = Some(Type::Int);
            },
            Some(Token::Unit) => {
                typ = Some(Type::Unit);
            },
            Some(Token::Sum(s)) => {
            },
            Some(Token::Var(ref s)) => {
                match vars.get(s) {
                    Some(t) => {
                        typ = Some(t.clone());
                    },
                    None => {
                        return Err(String::from(format!("Undeclared type variable {} in right-hand side of type declaration", s)));
                    }
                }
            },
            None => { break; },
        }
    }

    match typ {
        Some(t) => Ok(t),
        None => Ok(Type::TypeVar(0)) // for now, so tests of other parts pass
            //Err(String::from("Failed to parse type"))
    }
}

fn get_sum(mut tokens: &mut Peekable<TokenStream>, msg: &str) -> Result<String, String> {
    println!("{:?}", tokens.peek());
    match tokens.next() {
        Some(Token::Sum(s)) => Ok(s),
        _ => Err(String::from(msg))
    }
}

/*
 * Test cases to consider
 *
 * Arrow case analogous to bug for expression parsing: Foo = x -> x
 */

#[cfg(test)]
mod tests {
    use super::*;
    use interpret::structures::arrow;
    use interpret::structures::sums::SumType;
    use interpret::infer::GenTypeVar;
    use interpret::lex::decl::build_lexer;

    /* Test parse_type */

    fn assert_parses_type(s: &str, typ: Type) {
        match parse_type(&mut build_lexer(s), &mut vec![], &HashMap::new()) {
            Ok(t) => {
                assert_eq!(t, typ)
            },
            Err(msg) => {
                panic!("Expected successful parse giving type {:?}, instead got error {}", typ, msg)
            }
        }
    }

    #[test]
    fn test_parses_base_types() {
        assert_parses_type("Bool", Type::Bool);
        assert_parses_type("Int", Type::Int);
        assert_parses_type("()", Type::Unit);
    }

    #[test]
    fn test_parses_type_var() {
        let mut vars = HashMap::new();
        vars.insert(String::from("a"), Type::TypeVar(1));
        vars.insert(String::from("b"), Type::TypeVar(2));
        assert_eq!(parse_type(&mut build_lexer("a"), &mut vec![], &vars).unwrap(), Type::TypeVar(1));
        assert_eq!(parse_type(&mut build_lexer("b"), &mut vec![], &vars).unwrap(), Type::TypeVar(2));

        let msg = "Undeclared type variable c in right-hand side of type declaration";
        let err = parse_type(&mut build_lexer("c"), &mut vec![], &vars).unwrap_err();
        assert_eq!(&err, msg)
    }

    #[test]
    fn test_parses_function_type() {
        assert_parses_type("Int -> Bool", arrow(Type::Int, Type::Bool));
        assert_parses_type("Int -> Bool -> Int", arrow(Type::Int, (arrow(Type::Bool, Type::Int))));
        assert_parses_type("(Int -> Bool) -> Int", arrow(arrow(Type::Int, Type::Bool), Type::Int));
        assert_parses_type("((Int -> Bool) -> ()) -> Int", arrow(arrow(arrow(Type::Int, Type::Bool), Type::Unit), Type::Int));
    }

    #[test]
    fn test_parses_monomorphic_sum_type() {
        //TODO send in dict
        let maybe_int = SumType::new("MaybeInt", vec![(Constructor::new("JustInt"), Type::Int), (Constructor::new("Nothing"), Type::Unit)], HashSet::new());
        assert_parses_type("MaybeInt", Type::Sum(maybe_int));
    }

    /*
    #[test]
    fn test_freshly_instantiates_polymorphic_sum_type() {
    //TODO is this really what I want?
    }

    #[test]
    fn test_parses_nested_type() {
        //TODO
    }
    */

    #[test]
    fn test_rejects_extra_eq() {
        let msg = "Unexpected token = in right-hand side of type declaration";
        let err = parse_type(&mut build_lexer("Foo = Foo = |"), &mut vec![], &HashMap::new()).unwrap_err();
        assert_eq!(&err, msg)
    }

    #[test]
    fn test_extra_close_paren() {
        let err = parse_type(&mut build_lexer("Foo ())"), &mut vec![], &HashMap::new()).unwrap_err();
        assert_eq!(&err, "Unexpected CLOSE delimiter");
    }

    /* Test parse_variant */

    #[test]
    fn test_parses_constructor_name() {
        let mut tokens = build_lexer("Empty | Full");
        let variant = parse_variant(&mut tokens, &HashMap::new()).unwrap();
        assert_eq!(&variant.0.name, "Empty");

        let variant = parse_variant(&mut tokens, &HashMap::new()).unwrap();
        assert_eq!(&variant.0.name, "Full");
    }

    /* Test parse */

    fn assert_parse_err(decl: &str, msg: &str) {
        let mut tokens = build_lexer(decl);
        match parse(&mut tokens, &mut GenTypeVar::new()) {
            Ok(_) => {
                panic!("Expected error {} but got success", msg)
            },
            Err(m) => assert_eq!(&m, msg)
        }
    }

    #[test]
    fn test_parses_type_name() {
        let mut tokens = build_lexer("Tree a = Empty | Foo a");
        let sum_type = parse(&mut tokens, &mut GenTypeVar::new()).unwrap();
        assert_eq!(&sum_type.name, "Tree");
    }

    #[test]
    fn test_missing_type_name() {
        let msg = "Type declaration must begin with an uppercase name";
        assert_parse_err("foo a = Foo", msg);
        assert_parse_err("= Bar", msg);
    }

    #[test]
    fn test_invalid_left_side() {
        let msg = "Invalid token in left-hand side of type declaration";
        assert_parse_err("Tree a | b = Bar", msg);
        assert_parse_err("Tree X = Bar", msg);
    }

    #[test]
    fn test_parses_type_vars() {
        let mut gen = GenTypeVar::new();

        let mut tokens = build_lexer("Foo a b c = Bar");
        let sum_type = parse(&mut tokens, &mut gen).unwrap();
        let mut vars = HashSet::new();
        for n in vec![1, 2, 3] {
            vars.insert(n);
        }
        assert_eq!(sum_type.universals, vars);

        let mut tokens = build_lexer("Baz a b = Quux");
        let sum_type = parse(&mut tokens, &mut gen).unwrap();
        let mut vars = HashSet::new();
        for n in vec![4, 5] {
            vars.insert(n);
        }
        assert_eq!(sum_type.universals, vars);
    }

    /*TODO
    #[test]
    fn test_unbalanced_parens() {
        assert_parse_err("Foo = Bar(", "Unexpected end of input");
    }
    */

}
