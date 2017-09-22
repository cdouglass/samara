# Templates

```rust,skt-repl
extern crate samara;
use samara::evaluate;
use samara::SumTypeDefs;
use samara::infer::GenTypeVar;

use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Lines;

/*
const CMD_MARKER : char = ':';
const CONTINUE_LINE : char = '\\';
const CONTINUE_PROMPT : &str = "| ";
*/
const PROMPT : &str = "> ";

struct Inputs<'a> {{
    lines: Peekable<Lines<'a>>
}}

impl<'a> Inputs<'a> {{
    pub fn new(s: &str) -> Inputs {{
        Inputs{{lines: s.lines().peekable()}}
    }}
}}

#[derive(Clone)]
enum Command<'a> {{
    Eval(&'a str),
    Expected(&'a str)
}}

//TODO don't duplicate logic from main (reuse this there???)
impl<'a> Iterator for Inputs<'a> {{
    type Item = Command<'a>;
    fn next(&mut self) -> Option<Command<'a>> {{
        match self.lines.next() {{
            None => None,
            Some(line) => {{
                if let Some(expr) = strip_prefix(line, PROMPT) {{
                    return Some(Command::Eval(expr));
                }} else {{
                    return Some(Command::Expected(line));
                }}
            }}
        }}
    }}
}}

fn strip_prefix<'a>(s: &'a str, p: &str) -> Option<&'a str> {{
    if s.starts_with(p) {{
        Some(&s[p.len()..])
    }} else {{
        None
    }}
}}

fn main() {{
    let mut sum_types = SumTypeDefs::new();
    let mut gen = GenTypeVar::new();
    let mut session_bindings = vec![];
    let mut inputs = Inputs::new(r"{}").peekable();

    // mutable because refers to things that will mutate
    let mut eval = |x| {{
        evaluate(x, &mut session_bindings, &mut gen, &sum_types)
    }};

    loop {{
        if let Some(Command::Eval(expr)) = inputs.next() {{
            if let Some(Command::Expected(expected)) = inputs.peek().cloned() {{
                inputs.next();
                match eval(expr) {{
                    Err(err) => {{
                        assert_eq!(err, expected);
                    }},
                    Ok(t) => assert_eq!(format!("{{:?}}", t), expected)
                }}
            }} else {{
                eval(expr).unwrap();
            }}
        }} else {{
            break;
        }}
    }}
}}

```
