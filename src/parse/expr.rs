use std::iter::Iterator;
use std::iter::Peekable;
use std::str::FromStr;

use lex::expr::TokenStream;
use lex::expr::Token;
use lex::expr::Keyword::*;

use SumTypeDefs;
use structures::Atom;
use structures::Op;
use structures::Term;
use structures::patterns::Pattern;
use structures::sums::ConstructorBinding;

pub fn parse(tokens: &mut Peekable<TokenStream>, mut token_stack: &mut Vec<Token>, mut identifier_stack: &mut Vec<String>, sum_types: &SumTypeDefs) -> Result<Term, String> {
    let close_err = Err(String::from("Unexpected CLOSE delimiter"));
    let end_of_input_err = Err(String::from("Unexpected end of input"));
    let syntax_err = Err(String::from("Syntax error"));
    let lambda_syntax_err = Err(String::from("Syntax error in lambda expression"));
    let mut term_so_far = None;

    loop {
        let next_term_result = match tokens.peek().cloned() {
            Some(Token::Unit) => {
                tokens.next();
                Ok(Term::Atom(Atom::Unit))
            },
            Some(Token::Open) => {
                tokens.next();
                token_stack.push(Token::Open);
                parse(tokens, &mut token_stack, &mut identifier_stack, sum_types)
            },
            Some(Token::Close) => {
                match token_stack.last() {
                    Some(&Token::Open) => {
                        token_stack.pop();
                        tokens.next();
                        break;
                    },
                    Some(&Token::Keyword(Arrow)) => { break; },
                    _ => { return close_err; }
                }
            },
            Some(Token::Lambda) => {
                tokens.next();
                match (tokens.next(), tokens.next()) {
                    (Some(Token::Identifier(s)), Some(Token::Keyword(Arrow))) => {
                        identifier_stack.push(s.clone());
                        token_stack.push(Token::Keyword(Arrow));
                        let body = parse(tokens, &mut token_stack, &mut identifier_stack, sum_types);
                        identifier_stack.pop();
                        let result = body.map(|b| Term::Lambda(Box::new(b), s));
                        match token_stack.pop() {
                            Some(Token::Keyword(Arrow)) => { result },
                            _ => { return lambda_syntax_err; }
                        }
                    },
                    _ => { return lambda_syntax_err; }
                }
            },
            Some(Token::Keyword(k)) => {
                tokens.next();
                match k {
                    If    => parse_conditional(tokens, &mut token_stack, &mut identifier_stack, sum_types),
                    True  => { Ok(Term::Atom(Atom::Bool(true))) },
                    False => { Ok(Term::Atom(Atom::Bool(false))) },
                    Let   => parse_let(tokens, &mut token_stack, &mut identifier_stack, sum_types),
                    Case  => parse_case(tokens, &mut token_stack, &mut identifier_stack, sum_types),
                    k    => {
                        let tok = token_stack.pop();
                        if tok == Some(Token::Keyword(k.clone())) {
                            break;
                        } else {
                            return syntax_err;
                        }
                    },
                }
            },
            Some(Token::Constructor(s)) => {
                tokens.next();
                let (n, cb) = identify_constructor(&s, sum_types)?;
                Ok(cb.term(n))
            },
            Some(Token::Identifier(s)) => {
                tokens.next();
                if identifier_stack.last() == Some(&String::from("_")) {
                    identifier_stack.pop();
                    identifier_stack.push(s.clone());
                    Ok(Term::Var(0, s))
                } else {
                    let mut stack = identifier_stack.iter().rev();
                    match stack.position(|x| x == &s) {
                        Some(k) => Ok(Term::Var(k, s)),
                        None => Err(String::from(format!("Error: Undefined variable {}", s)))
                    }
                }
            },
            Some(Token::Number(ref s)) => {
                tokens.next();
                parse_int(s).map(Term::Atom)
            },
            Some(Token::Operator(ref s)) => {
                tokens.next();
                parse_operator(s).map(Term::Atom)
            },
            None => {
                if *token_stack == vec![Token::Keyword(In)] || token_stack.is_empty() {
                    break;
                } else {
                    match token_stack.last() {
                        Some(&Token::Keyword(Semicolon)) | Some(&Token::Keyword(Arrow)) => { break; },
                        _ => { return end_of_input_err; }
                    }
                }
            }
        };

        match next_term_result {
            Ok(nt) => match term_so_far {
                Some(t) => { term_so_far = Some(Term::App(Box::new(t), Box::new(nt))); },
                None => { term_so_far = Some(nt); }
            },
            Err(msg) => { return Err(msg); }
        }
    }

    match term_so_far {
        Some(term) => Ok(term),
        None => {
            end_of_input_err
        }
    }
}

fn parse_conditional(tokens: &mut Peekable<TokenStream>, token_stack: &mut Vec<Token>, identifier_stack: &mut Vec<String>, sum_types: &SumTypeDefs) -> Result<Term, String> {
    token_stack.push(Token::Keyword(Then));
    let predicate = parse(tokens, token_stack, identifier_stack, sum_types);

    token_stack.push(Token::Keyword(Else));
    let true_case = parse(tokens, token_stack, identifier_stack, sum_types);

    let false_case = parse(tokens, token_stack, identifier_stack, sum_types);
    match (predicate, true_case, false_case) {
        (Ok(p), Ok(t), Ok(f)) => Ok(Term::Conditional(Box::new(p), Box::new(t), Box::new(f))),
        (Err(msg), _, _) | (_, Err(msg), _) | (_, _, Err(msg)) => Err(msg)
    }
}

fn parse_let(tokens: &mut Peekable<TokenStream>, mut token_stack: &mut Vec<Token>, mut identifier_stack: &mut Vec<String>, sum_types: &SumTypeDefs) -> Result<Term, String> {
    let syntax_err = Err(String::from("Syntax error"));

    match (tokens.next(), tokens.next()) {
        (Some(Token::Identifier(s)), Some(Token::Keyword(Assign))) => {
            identifier_stack.push(s.clone());
            token_stack.push(Token::Keyword(In));
            let value = parse(tokens, &mut token_stack, &mut identifier_stack, sum_types)?;

            if *token_stack == vec![Token::Keyword(In)] {
                Ok(Term::Let(s, Box::new(value), None))
            } else {
                let body = Some(parse(tokens, &mut token_stack, &mut identifier_stack, sum_types)?);
                identifier_stack.pop();
                Ok(Term::Let(s, Box::new(value), body.map(Box::new)))
            }
        },
        _ => syntax_err
    }
}

fn parse_case(tokens: &mut Peekable<TokenStream>, token_stack: &mut Vec<Token>, mut identifier_stack: &mut Vec<String>, sum_types: &SumTypeDefs) -> Result<Term, String> {
    token_stack.push(Token::Keyword(Of));
    let arg = parse(tokens, token_stack, identifier_stack, sum_types)?;

    token_stack.push(Token::Keyword(Semicolon));
    let default = parse(tokens, token_stack, identifier_stack, sum_types)?;
    let mut cases = vec![];

    loop {
        if Some(&Token::Keyword(Semicolon)) == token_stack.last()
            || tokens.peek().is_none() { break; }

        token_stack.push(Token::Keyword(Semicolon));
        token_stack.push(Token::Keyword(Arrow));

        let pattern = parse_pattern(tokens, token_stack, sum_types)?;
        for var in &pattern.identifiers() { identifier_stack.push((*var).clone()); }

        let tok = tokens.next();
        if tok != Some(Token::Keyword(Arrow)) {
            return Err(format!("Expecting {:?} but instead got {:?}", Token::Keyword(Arrow), tok));
        }
        token_stack.pop();

        let arm = parse(tokens, token_stack, &mut identifier_stack, sum_types)?;
        for _ in pattern.identifiers() { identifier_stack.pop(); }
        cases.push((pattern, arm));
    }

    Ok(Term::Case(Box::new(arg), cases, Box::new(default)))
}

fn parse_pattern(tokens: &mut Peekable<TokenStream>, token_stack: &mut Vec<Token>, sum_types: &SumTypeDefs) -> Result<Pattern, String> {
    match tokens.next() {
        Some(Token::Open) => {
            token_stack.push(Token::Open);
            parse_pattern(tokens, token_stack, sum_types)
        },
        Some(Token::Unit) => Ok(Pattern::Atom(Atom::Unit)),
        Some(Token::Number(ref s)) => parse_int(s).map(Pattern::Atom),
        Some(Token::Operator(ref s)) => parse_operator(s).map(Pattern::Atom),
        Some(Token::Identifier(s)) => {
            if &s == "_" {
                Ok(Pattern::Wildcard)
            } else {
                Ok(Pattern::Var(s))
            }
        },
        Some(Token::Constructor(s)) => {
            let k = identify_constructor(&s, sum_types)?.0;
            let mut patterns = vec![];
            while tokens.peek() != Some(&Token::Keyword(Arrow)) {
                if tokens.peek() == Some(&Token::Close) {
                    if token_stack.pop() == Some(Token::Open) {
                        tokens.next();
                        break;
                    } else {
                        return Err(format!("Unexpected token {:?} in pattern", Token::Close));
                    }
                }
                patterns.push(parse_pattern(tokens, token_stack, sum_types)?);
            }
            Ok(Pattern::Sum(k, s, patterns))
        },
        Some(t) => Err(format!("Unexpected token {:?} in pattern", t)),
        None => Err(String::from("Unexpected end of input"))
    }
}

fn parse_int(s: &str) -> Result<Atom, String> {
    s.parse::<i64>()
        .map_err(|_| String::from(format!("Invalid integer: {}", s)))
        .map(Atom::Int)
}

fn parse_operator(s: &str) -> Result<Atom, String> {
    Op::from_str(s).map(Atom::BuiltIn)
}

fn identify_constructor<'a>(s: &str, sum_types: &'a SumTypeDefs) -> Result<(usize, &'a ConstructorBinding), String> {
    let mut binding_iter = sum_types.bindings.iter();
    match binding_iter.position(|x| x.tag == s) {
        Some(k) => Ok((k, &sum_types.bindings[k])),
        None => Err(String::from(format!("Unknown constructor {}", s)))
    }
}

#[cfg(test)]
mod tests {
    use std::iter::Peekable;

    use super::parse;
    use SumTypeDefs;
    use lex::expr::TokenStream;
    use structures::Atom;
    use structures::Term;
    use structures::Type;

    fn get_tokens(expr: &str) -> Peekable<TokenStream> {
        use lex::build_lexer;
        use lex::TokenStream as TS;
        match build_lexer(expr) {
            TS::Expr(ts) => ts,
            _ => panic!()
        }
    }

    fn assert_parse(expr: &str, expected: &Term) {
        let mut tokens = get_tokens(expr);
        match parse(&mut tokens, &mut vec![], &mut vec![], &SumTypeDefs::new()) {
            Err(msg) => {
                panic!("Expected term {:?} but got error {}", expected, msg);
            },
            Ok(term) => assert_eq!(term, *expected)
        }
    }

    fn assert_parse_err(expr: &str, msg: &str) {
        let mut tokens = get_tokens(expr);
        match parse(&mut tokens, &mut vec![], &mut vec![], &SumTypeDefs::new()) {
            Err(m) => assert_eq!(m, msg),
            Ok(term) => {
                panic!("Expected parse error {} but got success {:?}", msg, term);
            }
        }
    }

    #[test]
    fn test_lambda_without_parens() {
        let expected = Term::Lambda(Box::new(Term::Var(0, String::from("x"))), String::from("x"));
        assert_parse("(\\x -> x)", &expected);
        assert_parse("\\x -> x", &expected);
    }

    #[test]
    fn test_parses_lambda_application() {
        let mut tokens = get_tokens("(\\x -> (\\y -> 3)) 2");
        let ast = parse(&mut tokens, &mut vec![], &mut vec![], &SumTypeDefs::new());
        let expected = Term::App(
            Box::new(Term::Lambda(
                Box::new(Term::Lambda(Box::new(Term::Atom(Atom::Int(3))), String::from("y"))),
                "x".to_string()
            )),
            Box::new(Term::Atom(Atom::Int(2)))
        );

        assert_eq!(ast, Ok(expected));
    }

    #[test]
    fn test_parses_unit() {
        assert_parse("()", &Term::Atom(Atom::Unit));
    }

    #[test]
    fn test_unit_as_argument() {
        let expected = Term::App(Box::new(Term::Lambda(Box::new(Term::Atom(Atom::Int(5))), String::from("x"))), Box::new(Term::Atom(Atom::Unit)));
        assert_parse("(\\x -> 5) ()", &expected);
    }

    #[test]
    fn test_parses_constructor() {
        let mut sum_types = SumTypeDefs::new();
        let constructors = vec![(String::from("Foo"), vec![Type::Unit])];
        sum_types.add_type("Bar", constructors, vec![]).unwrap();
        let mut tokens = get_tokens("Foo");
        let ast = parse(&mut tokens, &mut vec![], &mut vec![], &sum_types).unwrap();
        let expected = Term::Lambda(Box::new(Term::Sum(0, String::from("Foo"), vec![Term::Var(0, String::from("x"))])), String::from("x"));
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_invalid_constructor() {
        assert_parse_err("Foo", "Unknown constructor Foo");
    }

    #[test]
    fn test_empty_input() {
        assert_parse_err("", "Unexpected end of input");
    }

    #[test]
    fn test_unbalanced_delimiters() {
        assert_parse_err("(+ 5 8))", "Unexpected CLOSE delimiter");
        assert_parse_err("(+ 5 (8)", "Unexpected end of input");
    }

    #[test]
    fn test_parses_case() {
        use declare_sum_type;
        use structures::patterns::Pattern;

        let mut sum_types = SumTypeDefs::new();
        declare_sum_type("Maybe a = Just a | None", &mut sum_types).unwrap();
        let expr = "case Just 10 of 5; Just 0 -> 42; Just x -> x; None -> 100; _ -> 777";
        let mut tokens = get_tokens(expr);
        let ast = parse(&mut tokens, &mut vec![], &mut vec![], &sum_types).unwrap();
        let expected_cases = vec![
            (Pattern::Sum(0, String::from("Just"), vec![Pattern::Atom(Atom::Int(0))]), Term::Atom(Atom::Int(42))),
            (Pattern::Sum(0, String::from("Just"), vec![Pattern::Var(String::from("x"))]), Term::Var(0, String::from("x"))),
            (Pattern::Sum(1, String::from("None"), vec![]), Term::Atom(Atom::Int(100))),
            (Pattern::Wildcard, Term::Atom(Atom::Int(777)))];
        match ast {
            Term::Case(arg, cases, default) => {
                assert_eq!(*arg, Term::App(Box::new(Term::Lambda(Box::new(Term::Sum(0, String::from("Just"), vec![Term::Var(0, String::from("x"))])), String::from("x"))), Box::new(Term::Atom(Atom::Int(10)))));
                assert_eq!(*default, Term::Atom(Atom::Int(5)));
                assert_eq!(cases, expected_cases);
            },
            x => panic!("Expected case expression but got {:?}", x)
        }
    }
}
