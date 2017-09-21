# Templates

```rust,skt-repl
extern crate samara;
use samara::evaluate;
use samara::SumTypeDefs;
use samara::infer::GenTypeVar;

use std::iter::Iterator;
use std::str::Split;

/*
const CMD_MARKER : char = ':';
const CONTINUE_LINE : char = '\\';
const CONTINUE_PROMPT : &str = "| ";
const PROMPT : &str = "> ";
*/

struct Inputs<'a> {{
    lines: Split<'a, &'static str>
}}

impl<'a> Inputs<'a> {{
    pub fn new(s: &str) -> Inputs {{
        Inputs{{lines: s.split("\n")}}
    }}
}}

enum Command<'a> {{
    Eval(&'a str)
}}

//TODO don't duplicate logic from main (reuse this there???)
impl<'a> Iterator for Inputs<'a> {{
// TODO this will change
    type Item = Command<'a>;
    fn next(&mut self) -> Option<Command<'a>> {{
        match self.lines.next() {{
            None => None,
            Some(line) => {{
                 let cmd = Command::Eval(line);
                 return Some(cmd);
            }}
        }}
    }}
}}

fn main() {{
    let mut sum_types = SumTypeDefs::new();
    let mut gen = GenTypeVar::new();
    let mut session_bindings = vec![];
    let mut inputs = Inputs::new(r"{}");

    // mutable because refers to things that will mutate
    let mut eval = |x| {{
        match x {{
            Command::Eval(s) => evaluate(&s, &mut session_bindings, &mut gen, &sum_types)
        }}
    }};

    loop {{
        if let (Some(x), Some(y)) = (inputs.next(), inputs.next()) {{
            assert_eq!(eval(x), eval(y))
        }} else {{
            break;
        }}
    }}
}}

```
