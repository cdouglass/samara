# Templates

```rust,skt-repl
extern crate samara;
use samara::declare_sum_type;
use samara::evaluate;
use samara::SumTypeDefs;
use samara::infer::GenTypeVar;

use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Lines;

/*
const CMD_MARKER : char = ':';
*/
const CONTINUE_PROMPT : &str = "| ";
const CONTINUE_LINE : &str = "\\";
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
    Decl(&'a str),
    Eval(String),
    Expected(&'a str)
}}

impl<'a> Iterator for Inputs<'a> {{
    type Item = Command<'a>;
    fn next(&mut self) -> Option<Command<'a>> {{
        if let Some(line) = self.lines.next() {{
            if let Some(expr) = strip_prefix(line, &format!("{{}}type", PROMPT)) {{
                Some(Command::Decl(expr))
            }} else if let Some(expr) = strip_prefix(line, PROMPT) {{
                let mut partial = String::new();
                let mut e = expr;
                loop {{
                    if let Some(p) = strip_suffix(e, CONTINUE_LINE) {{
                        partial += p;
                        match self.lines.peek().cloned().and_then(|x| strip_prefix(x, CONTINUE_PROMPT)) {{
                            Some(n) => {{
                                e = n;
                                self.lines.next();
                            }},
                            None => {{ break; }}
                        }}
                    }} else {{
                        partial += e;
                        break;
                    }}
                }}
                Some(Command::Eval(partial))
            }} else {{
                Some(Command::Expected(line))
            }}
        }} else {{
            None
        }}
    }}
}}

fn strip_suffix<'a>(s: &'a str, p: &str) -> Option<&'a str> {{
    if s.ends_with(p) {{
        let index = s.len() - p.len();
        Some(&s[..index])
    }} else {{
        None
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

    loop {{
        match inputs.next() {{
            Some(Command::Decl(typ)) => {{
                declare_sum_type(typ, &mut gen, &mut sum_types).unwrap();
            }},
            Some(Command::Eval(expr)) => {{
                let mut eval = |x: String| {{
                    evaluate(&x, &mut session_bindings, &mut gen, &sum_types)
                }};

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
            }},
            Some(Command::Expected(_)) => panic!(),
            None => {{ break; }}
        }}
    }}
}}

```
