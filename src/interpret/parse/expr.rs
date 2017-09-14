use std::iter::Iterator;
use std::iter::Peekable;
use std::str::FromStr;

use interpret::lex::expr::TokenStream;
use interpret::lex::expr::Token;
use interpret::lex::expr::Keyword::*;

use interpret::SumTypeDefs;
use interpret::structures::Atom;
use interpret::structures::Op;
use interpret::structures::Term;
use interpret::structures::Type;
use interpret::structures::patterns::Pattern;

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
                return Ok(Term::Atom(Atom::Unit));
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
                let mut binding_iter = sum_types.bindings.iter();
                match binding_iter.position(|x| x.tag == s) {
                    Some(k) => Ok(Term::Constructor(k, s)),
                    None => Err(String::from(format!("Unknown constructor {}", s)))
                }
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
            Some(Token::Number(s)) => {
                tokens.next();
                s.parse::<i64>()
                    .map_err(|_| String::from(format!("Invalid integer: {}", s)))
                    .map(|n| Term::Atom(Atom::Int(n)))
            },
            Some(Token::Operator(s)) => {
                tokens.next();
                Op::from_str(&s).map(|op| Term::Atom(Atom::BuiltIn(op)))
            },
            None => {
                if *token_stack == vec![Token::Keyword(In)] || token_stack.is_empty() {
                    break;
                } else if let Some(&Token::Keyword(Semicolon)) = token_stack.last() {
                    break;
                } else {
                    return end_of_input_err;
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

fn parse_conditional(tokens: &mut Peekable<TokenStream>, mut token_stack: &mut Vec<Token>, mut identifier_stack: &mut Vec<String>, sum_types: &SumTypeDefs) -> Result<Term, String> {
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

fn parse_case(tokens: &mut Peekable<TokenStream>, mut token_stack: &mut Vec<Token>, mut identifier_stack: &mut Vec<String>, sum_types: &SumTypeDefs) -> Result<Term, String> {
    token_stack.push(Token::Keyword(Of));
    let arg = parse(tokens, token_stack, identifier_stack, sum_types)?;

    token_stack.push(Token::Keyword(Semicolon));
    let default = parse(tokens, token_stack, identifier_stack, sum_types)?;

    let mut cases = vec![];

    loop {
        if let Some(&Token::Keyword(Semicolon)) = token_stack.last() {
            break;
        } else if let None = tokens.peek() {
            break;
        }
        token_stack.push(Token::Keyword(Semicolon));
        token_stack.push(Token::Keyword(Arrow));

        /*
         * Push wildcard marker as identifier, then get pattern as term (and when spotting a
         * variable, check for '_' on top of ID stack and fill it in if so instead of potentially
         * erroring because it's undefined), then turn term into pattern.
         *
         * What will happen if I try to name an actual variable '_'?
         *
         * TODO Clean this up later.
         */
        identifier_stack.push(String::from("_"));
        let pattern_term = parse(tokens, token_stack, identifier_stack, sum_types)?;
        let pattern = term_to_pattern(&pattern_term, sum_types)?;

        // consume -> if above didn't already
        match tokens.peek().cloned() {
            Some(Token::Keyword(Arrow)) => {
                if token_stack.last() == Some(&Token::Keyword(Arrow)) {
                    token_stack.pop();
                    tokens.next();
                }
            },
            _ => { }
        }

        let arm = parse(tokens, token_stack, identifier_stack, sum_types)?;
        identifier_stack.pop();

        cases.push((pattern, arm));
    }

    Ok(Term::Case(Box::new(arg), cases, Box::new(default)))
}

fn term_to_pattern(term: &Term, sum_types: &SumTypeDefs) -> Result<Pattern, String> {
    match *term {
        Term::Atom(ref atom) => Ok(Pattern::Atom(atom.clone())),
        Term::Var(ref n, ref s) => {
            if s == "_" {
                Ok(Pattern::Wildcard)
            } else {
                Ok(Pattern::Var(*n, s.clone()))
            }
        },
        Term::Sum(ref n, ref s, ref value) => {
            let inner_pattern = term_to_pattern(value, sum_types)?;
            Ok(Pattern::Sum(*n, s.clone(), Box::new(inner_pattern)))
        },
        // TODO clean this up, check whether constructor actually takes argument
        // needs to look at constructor instead of sum because it doesn't actually parse sums directly now
        Term::Constructor(ref n, ref s) => {
            let constructor_binding = &sum_types.bindings[*n];
            if let Type::Arrow(ref left, _) = constructor_binding.typ() {
                if *left.clone() != Type::Unit {
                    return Err(format!("Invalid pattern {:?}", term));
                }
            }
            Ok(Pattern::Sum(*n, s.clone(), Box::new(Pattern::Atom(Atom::Unit))))
        },
        Term::App(ref left, ref value) => {
            if let Term::Constructor(ref n, ref s) = *left.clone() {
                let inner_pattern = term_to_pattern(value, sum_types)?;
                Ok(Pattern::Sum(*n, s.clone(), Box::new(inner_pattern)))
            } else {
                Err(format!("Invalid pattern {:?}", term))
            }
        },
        _ => Err(format!("Invalid pattern {:?}", term))
    }
}

#[cfg(test)]
mod tests {
    use super::parse;
    use interpret::SumTypeDefs;
    use interpret::lex::build_lexer;
    use interpret::lex::TokenStream as TS;
    use interpret::structures::Atom;
    use interpret::structures::Term;
    use interpret::structures::Type;

    fn assert_parse(expr: &str, expected: Term) {
        let mut token_stack = vec![];
        let mut ids = vec![];
        let sum_types = SumTypeDefs::new();
        let mut tokens = match build_lexer(expr) {
            TS::Expr(ts) => ts,
            _ => panic!()
        };
        match parse(&mut tokens, &mut token_stack, &mut ids, &sum_types) {
            Err(msg) => {
                println!("Expected term {:?} but got error {}", expected, msg);
                panic!()
            },
            Ok(term) => assert_eq!(term, expected)
        }
    }

    fn assert_parse_err(expr: &str, msg: &str) {
        let mut token_stack = vec![];
        let mut ids = vec![];
        let sum_types = SumTypeDefs::new();
        let mut tokens = match build_lexer(expr) {
            TS::Expr(ts) => ts,
            _ => panic!()
        };
        match parse(&mut tokens, &mut token_stack, &mut ids, &sum_types) {
            Err(m) => assert_eq!(m, msg),
            Ok(term) => {
                println!("Expected parse error {} but got success {:?}", msg, term);
                panic!()
            }
        }
    }

    #[test]
    fn test_parses_lambda_application() {
        let mut token_stream = match build_lexer("(\\x -> (\\y -> 3)) 2") {
            TS::Expr(ts) => ts,
            _ => panic!()
        };
        let ast = parse(&mut token_stream, &mut vec![], &mut vec![], &SumTypeDefs::new());
        let expected =
            Term::App(
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
        assert_parse("()", Term::Atom(Atom::Unit));
    }

    #[test]
    fn test_parses_constructor() {
        let mut sum_types = SumTypeDefs::new();
        let constructors = vec![(String::from("Foo"), Type::Unit)];
        sum_types.add_type("Bar", constructors, vec![]).unwrap();
        let mut token_stream = match build_lexer("Foo") {
            TS::Expr(ts) => ts,
            _ => panic!()
        };
        let ast = parse(&mut token_stream, &mut vec![], &mut vec![], &sum_types).unwrap();
        let expected = Term::Constructor(0, String::from("Foo"));
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
        use interpret::declare_sum_type;
        use interpret::infer::GenTypeVar;
        use interpret::structures::patterns::Pattern;

        let mut sum_types = SumTypeDefs::new();
        declare_sum_type("Maybe a = Just a | None", &mut GenTypeVar::new(), &mut sum_types).unwrap();
        let mut tokens = match build_lexer("case Just 10 of 5; Just 0 -> 42; Just x -> x; None -> 100; _ -> 777"){
            TS::Expr(ts) => ts,
            _ => panic!()
        };
        let ast = parse(&mut tokens, &mut vec![], &mut vec![], &sum_types).unwrap();
        match ast {
            Term::Case(arg, cases, default) => {
                assert_eq!(*arg, Term::App(Box::new(Term::Constructor(0, String::from("Just"))), Box::new(Term::Atom(Atom::Int(10)))));
                assert_eq!(*default, Term::Atom(Atom::Int(5)));
                assert_eq!(cases, vec![(Pattern::Sum(0, String::from("Just"), Box::new(Pattern::Atom(Atom::Int(0)))), Term::Atom(Atom::Int(42))), (Pattern::Sum(0, String::from("Just"), Box::new(Pattern::Var(0, String::from("x")))), Term::Var(0, String::from("x"))), (Pattern::Sum(1, String::from("None"), Box::new(Pattern::Atom(Atom::Unit))), Term::Atom(Atom::Int(100))), (Pattern::Wildcard, Term::Atom(Atom::Int(777)))]);
            },
            x => panic!("Expected case expression but got {:?}", x)
        }
    }
}
