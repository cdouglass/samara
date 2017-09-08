use std::iter::Iterator;
use std::iter::Peekable;

use interpret::lex::expr::TokenStream;
use interpret::structures::Term;
use interpret::structures::Type;

mod expr;

pub fn parse_expr(mut tokens: &mut Peekable<TokenStream>, mut identifier_stack: &mut Vec<String>) -> Result<Term, String> {
    let mut token_stack = vec![];
    expr::parse(&mut tokens, &mut token_stack, &mut identifier_stack)
}
