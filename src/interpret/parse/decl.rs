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
    match tokens.next() {
        Some(Token::Sum(s)) => {
            let name = s;
            Ok(SumType{name: name, variants: HashSet::new()})
        },
        _ => Err(String::from("Type declaration must begin with an uppercase name"))
    }
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
fn parse_variant(mut tokens: &mut Peekable<TokenStream>, token_stack: Vec<Token>, vars: HashMap<String, usize>) -> (Constructor, Type) {
    let c = Constructor::new("Hello");
    let t = Type::Unit;
    (c, t)
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

    #[test]
    fn test_parses_type_name() {
        let mut tokens = build_lexer("Tree a = Empty | Tree a (Tree a) (Tree a)");
        let sum_type = parse(&mut tokens, &mut GenTypeVar::new()).unwrap();
        assert_eq!(&sum_type.name, "Tree");
    }
}
