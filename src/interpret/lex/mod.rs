use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Chars;

pub mod expr;
use self::expr::build_lexer as e_build_lexer;
pub use self::expr::Keyword;
pub use self::expr::Token;
use self::expr::TokenStream as ETokenStream;

pub mod decl;
use self::decl::TokenStream as DTokenStream;
//pub use self::decl::Keyword;
//pub use self::decl::Token;

pub fn build_lexer(expr: &str) -> TokenStream {
    let mut ts = e_build_lexer(expr);
    TokenStream::Expr(ts)
}
pub enum TokenStream<'a> {
    Expr(Peekable<ETokenStream<'a>>),
    Decl(Peekable<DTokenStream<'a>>)
}
