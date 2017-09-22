use std::iter::Peekable;

use SumTypeDefs;
use lex::expr::TokenStream as ETokenStream;
use structures::Term;

mod decl;
mod expr;

pub use self::decl::parse as parse_decl;

pub fn parse_expr(tokens: &mut Peekable<ETokenStream>, mut identifier_stack: &mut Vec<String>, sum_types: &SumTypeDefs) -> Result<Term, String> {
    let mut token_stack = vec![];
    expr::parse(tokens, &mut token_stack, &mut identifier_stack, sum_types)
}
