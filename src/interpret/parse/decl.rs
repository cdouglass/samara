use std::iter::Iterator;
use std::iter::Peekable;

use interpret::infer::GenTypeVar;
use interpret::lex::decl::Token;
use interpret::lex::decl::TokenStream;
use interpret::structures::Type;

pub fn parse(mut tokens: &mut Peekable<TokenStream>, token_stack: &mut Vec<Token>, gen: &mut GenTypeVar) -> Result<Type, String> {
    Err(String::new())
    //TODO
}

