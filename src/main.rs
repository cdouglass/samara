use std::io;
use std::io::Write;
use std::iter::Iterator;

mod interpret;

const GREETING : &str = "---- samara 0.1.0 --------------------------------------------------------------";
const PROMPT : &str = "> ";
const CMD_MARKER : char = ':';
const USAGE : &str = "exit with :exit";

pub enum Command {
    Exit,
    Help,
    Unknown
}

fn get_command(input: &str) -> Option<Command> {
    if input.chars().nth(0) == Some(CMD_MARKER) {
        match input.trim().trim_matches(CMD_MARKER) {
            "exit" => Some(Command::Exit),
            "help" => Some(Command::Help),
            _ => Some(Command::Unknown)
        }
    } else {
        None
    }
}

fn main() {
    println!("{}", GREETING);

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut expr = String::new();
        io::stdin().read_line(&mut expr).unwrap();

        let cmd = get_command(&expr);
        match cmd {
            Some(Command::Exit) => break,
            Some(Command::Help) => println!("{}", USAGE),
            Some(Command::Unknown) => println!("Unknown command"),
            None => {
                let result = interpret::evaluate(&expr);
                match result {
                    Ok(term) => println!("{:?}", term),
                    Err(msg) => println!("{}", msg)
                }
            }
        }
    }
}
