use std::collections::HashMap;
use std::iter::Iterator;
use std::iter::Peekable;

use infer::GenTypeVar;
use lex::decl::Token;
use lex::decl::TokenStream;
use structures::arrow;
use structures::Type;
use structures::sums::SumType;
use structures::sums::SumTypeDefs;

pub fn parse(tokens: &mut Peekable<TokenStream>, sum_types: &SumTypeDefs) -> Result<SumType, String> {
    let name = get_sum(tokens, "Type declaration must begin with an uppercase name")?;
    let mut type_vars = HashMap::new();
    let mut params = vec![];
    let mut gen = GenTypeVar::new();

    loop {
        match tokens.next() {
            Some(Token::Var(name)) => {
                let tv = gen.next().unwrap();
                params.push(tv.clone());
                type_vars.insert(name, tv);
            },
            Some(Token::Eql) => { break; },
            _ => {
                return Err(String::from("Invalid token in left-hand side of type declaration"));
            }
        }
    }

    let mut variants = vec![];
    while tokens.peek() != None {
        variants.push(parse_variant(tokens, &type_vars, sum_types)?);
    }

    Ok(SumType::new(&name, variants, params))
}

fn parse_variant(tokens: &mut Peekable<TokenStream>, vars: &HashMap<String, Type>, sum_types: &SumTypeDefs) -> Result<(String, Vec<Type>), String> {
    let name = get_sum(tokens, "Missing constructor in right-hand side of type declaration")?;
    let mut token_stack = vec![];
    let mut arg_types = vec![];

    loop {
        match tokens.peek() {
            Some(&Token::Separator) => {
                tokens.next();
                break;
            },
            None => { break; },
            _ => { }
        }

        let typ = parse_type(tokens, &mut token_stack, vars, sum_types)?;
        arg_types.push(typ);
    }

    if !token_stack.is_empty() {
        return Err(String::from("Unexpected end of input111"));
    }

    Ok((String::from(name), arg_types))
}

fn parse_type(tokens: &mut Peekable<TokenStream>, mut token_stack: &mut Vec<Token>, vars: &HashMap<String, Type>, sum_types: &SumTypeDefs) -> Result<Type, String> {
    let mut typ : Type;

    match tokens.next() {
        Some(Token::Open) => {
            token_stack.push(Token::Open);
            typ = parse_type(tokens, &mut token_stack, vars, sum_types)?;

            match (tokens.peek().cloned(), token_stack.last().cloned()) {
                (Some(Token::Close), Some(Token::Open)) => {
                    token_stack.pop();
                    tokens.next();
                },
                (Some(Token::Close), _) => {
                    return Err(String::from(format!("Unexpected token {:?} in right-hand side of type declaration", Token::Close)));
                },
                _ => { }
            }
        },
        Some(Token::Bool) => { typ = Type::Bool; },
        Some(Token::Int) => { typ = Type::Int; },
        Some(Token::Unit) => { typ = Type::Unit; },
        Some(Token::Separator) | None => { return Err(String::from("Unexpected end of input")); },
        Some(Token::Sum(ref s)) => {
            match sum_types.type_from_name(s) {
                Some(t) => {
                    let mut parameters = vec![];
                    while parameters.len() < t.params.len() {
                        match tokens.peek() {
                            Some(&Token::Separator) | None => { break; },
                            _ => { }
                        }
                        let param = parse_type(tokens, token_stack, vars, sum_types)?;
                        parameters.push(param);
                    }
                    let mut new_typ = t.clone();
                    new_typ.params = parameters;
                    typ = Type::Sum(new_typ);
                },
                None => {
                    return Err(String::from(format!("Undeclared sum type {}", s)));
                }
            }
        },
        Some(Token::Var(ref s)) => {
            match vars.get(s) {
                Some(t) => {
                    typ = t.clone();
                },
                None => {
                    return Err(String::from(format!("Undeclared type variable {} in right-hand side of type declaration", s)));
                }
            }
        },
        Some(tok) => {
            return Err(String::from(format!("Unexpected token {:?} in right-hand side of type declaration", tok)));
        }
    }

    match tokens.peek() {
        Some(&Token::Arrow) => {
            tokens.next();
            let input_type = typ.clone();
            let output_type = parse_type(tokens, &mut token_stack, vars, sum_types)?;
            typ = arrow(input_type, output_type);
        },
        Some(&Token::Close) => {
            if let Some(Token::Open) = token_stack.pop() {
                tokens.next();
            } else {
                return Err(String::from(format!("Unexpected token {:?} in right-hand side of type declaration", Token::Close)));
            }
        },
        _ => { }
    }

    Ok(typ)
}

fn get_sum(tokens: &mut Peekable<TokenStream>, msg: &str) -> Result<String, String> {
    match tokens.next() {
        Some(Token::Sum(s)) => Ok(s),
        _ => Err(String::from(msg))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use structures::arrow;
    use structures::Type::TypeVar;
    use structures::sums::SumType;
    use lex::decl::build_lexer;

    /* Test parse_type */

    fn assert_parses_type(s: &str, typ: Type) {
        assert_parses_type_with_context(s, typ, &SumTypeDefs::new())
    }

    fn assert_parses_type_with_context(s: &str, typ: Type, sum_types: &SumTypeDefs) {
        match parse_type(&mut build_lexer(s), &mut vec![], &HashMap::new(), sum_types) {
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
        let vars = vec![(String::from("a"), Type::TypeVar(1)), (String::from("b"), Type::TypeVar(2))].into_iter().collect();
        assert_eq!(parse_type(&mut build_lexer("a"), &mut vec![], &vars, &SumTypeDefs::new()).unwrap(), Type::TypeVar(1));
        assert_eq!(parse_type(&mut build_lexer("b"), &mut vec![], &vars, &SumTypeDefs::new()).unwrap(), Type::TypeVar(2));

        let msg = "Undeclared type variable c in right-hand side of type declaration";
        let err = parse_type(&mut build_lexer("c"), &mut vec![], &vars, &SumTypeDefs::new()).unwrap_err();
        assert_eq!(&err, msg)
    }

    #[test]
    fn test_parses_function_type() {
        assert_parses_type("Int -> Bool", arrow(Type::Int, Type::Bool));
        assert_parses_type("Int -> Bool -> Int", arrow(Type::Int, (arrow(Type::Bool, Type::Int))));
        assert_parses_type("(Int -> Bool) -> Int", arrow(arrow(Type::Int, Type::Bool), Type::Int));
        assert_parses_type("Int -> (Int -> Bool) -> Int", arrow(Type::Int, arrow(arrow(Type::Int, Type::Bool), Type::Int)));
        assert_parses_type("((Int -> Bool) -> ()) -> Int", arrow(arrow(arrow(Type::Int, Type::Bool), Type::Unit), Type::Int));
    }

    #[test]
    fn test_parses_nullary_sum_type() {
        let variants = vec![(String::from("JustInt"), vec![Type::Int]), (String::from("Nothing"), vec![Type::Unit])];
        let maybe_int = SumType::new("MaybeInt", variants.clone(), vec![]);

        let mut sum_types = SumTypeDefs::new();
        sum_types.add_type("MaybeInt", variants, vec![]).unwrap();
        assert_parses_type_with_context("MaybeInt", Type::Sum(maybe_int), &sum_types);
    }

    #[test]
    fn test_unknown_sum_type() {
        let msg = "Undeclared sum type Foo";
        let err = parse_type(&mut build_lexer("Foo"), &mut vec![], &HashMap::new(), &SumTypeDefs::new()).unwrap_err();
        assert_eq!(&err, msg)
    }

    #[test]
    fn test_parses_nested_type() {
        let variants = vec![(String::from("Just"), vec![Type::TypeVar(0)]), (String::from("Nothing"), vec![Type::Unit])];
        let mut sum_types = SumTypeDefs::new();
        sum_types.add_type("Maybe", variants.clone(), vec![TypeVar(0)]).unwrap();

        let maybe = SumType::new("Maybe", variants, vec![TypeVar(0)]);
        let mut maybe_int = maybe.clone();
        maybe_int.params = vec![Type::Int];
        let mut expected = maybe.clone();
        expected.params = vec![Type::Sum(maybe_int)];
        assert_parses_type_with_context("Maybe (Maybe Int)", Type::Sum(expected), &sum_types);
    }

    #[test]
    fn test_extra_close_paren() {
        let mut sum_types = SumTypeDefs::new();
        sum_types.add_type("Foo", vec![], vec![]).unwrap();
        let err = parse_type(&mut build_lexer("Int -> (Int -> Int)) -> Int"), &mut vec![], &HashMap::new(), &sum_types).unwrap_err();
        assert_eq!(&err, "Unexpected token Close in right-hand side of type declaration");
    }

    /* Test parse_variant */

    #[test]
    fn test_extra_close_paren_in_variant() {
        let mut sum_types = SumTypeDefs::new();
        sum_types.add_type("Foo", vec![], vec![]).unwrap();
        let err = parse_variant(&mut build_lexer("Foo ())"), &HashMap::new(), &sum_types).unwrap_err();
        assert_eq!(&err, "Unexpected token Close in right-hand side of type declaration");
    }

    #[test]
    fn test_rejects_extra_eq() {
        let mut sum_types = SumTypeDefs::new();
        sum_types.add_type("Foo", vec![], vec![]).unwrap();
        let msg = "Unexpected token Eql in right-hand side of type declaration";
        let err = parse_variant(&mut build_lexer("Foo = Foo = |"), &HashMap::new(), &sum_types).unwrap_err();
        assert_eq!(&err, msg)
    }

    #[test]
    fn test_parses_constructor_name() {
        let mut tokens = build_lexer("Empty | Full");
        let variant = parse_variant(&mut tokens, &HashMap::new(), &SumTypeDefs::new()).unwrap();
        assert_eq!(&variant.0, "Empty");

        let variant = parse_variant(&mut tokens, &HashMap::new(), &SumTypeDefs::new()).unwrap();
        assert_eq!(&variant.0, "Full");
    }

    /* Test parse */

    fn assert_parse_err(decl: &str, msg: &str) {
        let mut tokens = build_lexer(decl);
        match parse(&mut tokens, &SumTypeDefs::new()) {
            Ok(_) => {
                panic!("Expected error {} but got success", msg)
            },
            Err(m) => assert_eq!(&m, msg)
        }
    }

    #[test]
    fn test_parses_type_name() {
        let mut tokens = build_lexer("Tree a = Empty | Foo a");
        let sum_type = parse(&mut tokens, &SumTypeDefs::new()).unwrap();
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
        let mut tokens = build_lexer("Foo a b c = Bar");
        let sum_type = parse(&mut tokens, &SumTypeDefs::new()).unwrap();
        assert_eq!(sum_type.params, vec![TypeVar(1), TypeVar(2), TypeVar(3)]);

        let mut tokens = build_lexer("Baz a b = Quux");
        let sum_type = parse(&mut tokens, &SumTypeDefs::new()).unwrap();
        assert_eq!(sum_type.params, vec![TypeVar(1), TypeVar(2)]);
    }
}

