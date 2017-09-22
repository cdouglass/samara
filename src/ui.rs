use std::io::BufRead;
use std::io::StdinLock;
use std::io::StdoutLock;
use std::io::Write;
use std::iter::Iterator;
use std::iter::Peekable;
use std::str::Lines as StrLines;

pub struct Inputs<'a> {
    lines: Peekable<Ipts<'a>>,
    cmd_marker: &'static str,
    continue_prompt: &'static str,
    continue_line: &'static str,
    prompt: &'static str,
    out: Option<StdoutLock<'a>>
}

enum Ipts<'a> {
    Io(StdinLock<'a>),
    Str(StrLines<'a>)
}

impl<'a> Inputs<'a> {
    pub fn new_str(s: &'a str) -> Inputs<'a> {
        Inputs {
            lines: Ipts::Str(s.lines()).peekable(),
            cmd_marker: ":",
            continue_prompt: "| ",
            continue_line: "\\",
            prompt: "> ",
            out: None
        }
    }

    pub fn new_io(stdin: StdinLock<'a>, stdout: StdoutLock<'a>, cm: &'static str, cp: &'static str, cl: &'static str, p: &'static str) -> Inputs<'a> {
        Inputs {
            lines: Ipts::Io(stdin).peekable(),
            cmd_marker: cm,
            continue_prompt: cp,
            continue_line: cl,
            prompt: p,
            out: Some(stdout)
        }
    }
}

impl<'a> Iterator for Ipts<'a> {
    type Item = String;

    fn next(&mut self) -> Option<String> {
        match *self {
            Ipts::Io(ref mut stdin) => {
                let mut s = String::new();
                stdin.read_line(&mut s).unwrap();
                Some(s.trim().to_string())
            },
            Ipts::Str(ref mut lines) => {
                lines.next().map(|x| x.to_owned())
            }
        }
    }
}

#[derive(Clone)]
#[derive(Debug)]
pub enum Command {
    Decl(String),
    Eval(String),
    Expected(String),
    Command(String, Option<String>) // command, optional parameters
}

impl<'a> Iterator for Inputs<'a> {
    type Item = Command;
    fn next(&mut self) -> Option<Command> {
        if let Some(line) = self.lines.next() {
            if let Some(ref expr) = strip_prefix(&line, &format!("{}type", self.prompt)) {
                Some(Command::Decl(expr.to_string()))
            } else if let Some(expr) = print_or_strip_prefix(&line, self.prompt, &mut self.out) {
                if let Some(cmd) = strip_prefix(&expr, self.cmd_marker) {
                    let mut split = cmd.splitn(2, ' ').map(|x| x.to_string());
                    return Some(Command::Command(split.next().unwrap(), split.next()));
                }
                let mut partial = String::new();
                let mut e = expr;
                loop {
                    if let Some(p) = strip_suffix(&e, self.continue_line) {
                        partial += p;
                    } else {
                        partial += &e;
                        break;
                    }
                    match self.lines.peek().cloned().and_then(|x| print_or_strip_prefix(&x, self.continue_prompt, &mut self.out)) {
                        Some(n) => {
                            e = n.clone();
                            self.lines.next();
                        },
                        None => { break; }
                    }
                }
                Some(Command::Eval(partial))
            } else {
                Some(Command::Expected(line.clone()))
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

fn strip_prefix(s: &str, p: &str) -> Option<String> {
    if s.starts_with(p) {
        Some(s[p.len()..].to_string())
    } else {
        None
    }
}

fn print_or_strip_prefix(s: &str, p: &str, handle: &mut Option<StdoutLock>) -> Option<String> {
    match *handle {
        Some(ref mut out) => {
            out.write(p.as_bytes()).unwrap();
            out.flush().unwrap();
            None
        },
        None => strip_prefix(s, p)
    }
}
