use std::collections::HashMap;
use std::io;
use std::io::Write;
use std::iter::Iterator;

mod interpret;

const GREETING : &str = "---- samara 0.1.0 --------------------------------------------------------------";
const PROMPT : &str = "> ";
const CMD_MARKER : char = ':';
const USAGE : &str = "Available commands:\n:exit        - exit\n:help        - help\n:type <expr> - show type of <expr>";

pub enum Command {
    Exit,
    Help,
    TypeOf(String),
    Unknown
}

fn get_command(input: &str) -> Option<Command> {
    if input.chars().nth(0) == Some(CMD_MARKER) {
        let mut it = input.trim().trim_matches(CMD_MARKER).splitn(2, ' ');
        match it.next() {
            Some("exit") => Some(Command::Exit),
            Some("help") => Some(Command::Help),
            Some("type") => {
                let expr = it.next();
                expr.map(|s| Command::TypeOf(String::from(s)))
            },
            _ => Some(Command::Unknown)
        }
    } else {
        None
    }
}

fn main() {
    println!("{}", GREETING);
    let mut bindings = HashMap::new();

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut expr = String::new();
        io::stdin().read_line(&mut expr).unwrap();

        let cmd = get_command(&expr);
        match cmd {
            Some(Command::Exit) => break,
            Some(Command::Help) => println!("{}", USAGE),
            Some(Command::TypeOf(s)) => {
                let result = interpret::type_of(&s);
                match result {
                    (Ok(term), Ok(typ)) => println!("{:?} : {:?}", term, typ),
                    (Err(msg), _) | (_, Err(msg)) => println!("{}", msg)
                }
            }
            Some(Command::Unknown) => println!("Unknown command"),
            None => {
                let result = interpret::evaluate(&expr, &mut bindings);
                match result {
                    Ok(term) => println!("{:?}", term),
                    Err(msg) => println!("{}", msg)
                }
            }
        }
    }
}
