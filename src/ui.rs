use declare_sum_type;
use evaluate;
use SumTypeDefs;
use infer::GenTypeVar;

use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Lines;

const CONTINUE_PROMPT : &str = "| ";
const CONTINUE_LINE : &str = "\\";
const PROMPT : &str = "> ";

pub struct Inputs<'a> {
    lines: Peekable<Lines<'a>>
}

impl<'a> Inputs<'a> {
    pub fn new(s: &str) -> Inputs {
        Inputs{lines: s.lines().peekable()}
    }
}

#[derive(Clone)]
pub enum Command<'a> {
    Decl(&'a str),
    Eval(String),
    Expected(&'a str)
}

impl<'a> Iterator for Inputs<'a> {
    type Item = Command<'a>;
    fn next(&mut self) -> Option<Command<'a>> {
        if let Some(line) = self.lines.next() {
            if let Some(expr) = strip_prefix(line, &format!("{}type", PROMPT)) {
                Some(Command::Decl(expr))
            } else if let Some(expr) = strip_prefix(line, PROMPT) {
                let mut partial = String::new();
                let mut e = expr;
                loop {
                    if let Some(p) = strip_suffix(e, CONTINUE_LINE) {
                        partial += p;
                        match self.lines.peek().cloned().and_then(|x| strip_prefix(x, CONTINUE_PROMPT)) {
                            Some(n) => {
                                e = n;
                                self.lines.next();
                            },
                            None => { break; }
                        }
                    } else {
                        partial += e;
                        break;
                    }
                }
                Some(Command::Eval(partial))
            } else {
                Some(Command::Expected(line))
            }
        } else {
            None
        }
    }
}

fn strip_suffix<'a>(s: &'a str, p: &str) -> Option<&'a str> {
    if s.ends_with(p) {
        let index = s.len() - p.len();
        Some(&s[..index])
    } else {
        None
    }
}

fn strip_prefix<'a>(s: &'a str, p: &str) -> Option<&'a str> {
    if s.starts_with(p) {
        Some(&s[p.len()..])
    } else {
        None
    }
}
