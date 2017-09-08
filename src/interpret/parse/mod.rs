use std::iter::Iterator;
use std::iter::Peekable;

use interpret::infer::GenTypeVar;
use interpret::lex::decl::TokenStream as DTokenStream;
use interpret::lex::expr::TokenStream as ETokenStream;
use interpret::structures::Term;
use interpret::structures::Type;

mod decl;
mod expr;

pub fn parse_decl(mut tokens: &mut Peekable<DTokenStream>, mut gen: &mut GenTypeVar) -> Result<Type, String> {
    let mut token_stack = vec![];
    decl::parse(tokens, &mut token_stack, &mut gen)
}

pub fn parse_expr(mut tokens: &mut Peekable<ETokenStream>, mut identifier_stack: &mut Vec<String>) -> Result<Term, String> {
    let mut token_stack = vec![];
    expr::parse(tokens, &mut token_stack, &mut identifier_stack)
}
