use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::Iterator;
use std::iter::Peekable;

use interpret::infer::GenTypeVar;
use interpret::lex::decl::Token;
use interpret::lex::decl::TokenStream;
use interpret::structures::Type;
use interpret::structures::sums::Constructor;
use interpret::structures::sums::SumType;

pub fn parse(mut tokens: &mut Peekable<TokenStream>, gen: &mut GenTypeVar) -> Result<SumType, String> {
    let name = if let Some(Token::Sum(s)) = tokens.next() {
        s
    } else {
        return Err(String::from("Type declaration must begin with an uppercase name"));
    };
    let mut vars = HashMap::new();
    let mut variants = HashSet::new();
    let mut token_stack = vec![];

    loop {
        match tokens.next() {
            Some(Token::Var(name)) => {
                vars.insert(name, gen.next().unwrap());
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
        let variant = parse_variant(tokens, &mut token_stack, &vars)?;
        variants.insert(variant);

        if !token_stack.is_empty() {
            return Err(String::from("Unexpected end of input"));
        }
        if let None = tokens.peek() { break; }
    }

    Ok(SumType{name: name, variants: variants})
}

// TODO SumType should include set of type variables
// TODO GenTypeVar should give a TypeVar, not an unwrapped usize

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
fn parse_variant(mut tokens: &mut Peekable<TokenStream>, mut token_stack: &mut Vec<Token>, vars: &HashMap<String, Type>) -> Result<(Constructor, Type), String> {
    tokens.next();
    let c = Constructor::new("Hello");
    let t = Type::Unit;
    Ok((c, t))
}

/*
 * Test cases to consider
 *
 * Arrow case analogous to bug for expression parsing: Foo = x -> x
 */

#[cfg(test)]
mod tests {
    use super::*;
    use interpret::infer::GenTypeVar;
    use interpret::lex::decl::build_lexer;

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
        let mut tokens = build_lexer("Tree a = Empty | Tree a (Tree a) (Tree a)");
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
    }
}
