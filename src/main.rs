use std::io;
use std::io::Write;
use std::iter::Iterator;

mod interpret;

const CMD_MARKER : char = ':';
const CONTINUE_LINE : char = '\\';
const CONTINUE_PROMPT : &str = "| ";
const PROMPT : &str = "> ";
const GREETING : &str = "---- samara 0.1.0 --------------------------------------------------------------";
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
    use interpret::infer::GenTypeVar;
    println!("{}", GREETING);
    let mut bindings = vec![];
    let mut gen_type_var = GenTypeVar{n: 0};

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut expr = String::new();
        io::stdin().read_line(&mut expr).unwrap();

        loop {
            expr.pop();
            if expr.chars().last() == Some(CONTINUE_LINE) {
                expr.pop();

                print!("{}", CONTINUE_PROMPT);
                io::stdout().flush().unwrap();

                io::stdin().read_line(&mut expr).unwrap();
            } else {
                break;
            }
        }

        let cmd = get_command(&expr);
        match cmd {
            Some(Command::Exit) => break,
            Some(Command::Help) => println!("{}", USAGE),
            Some(Command::TypeOf(s)) => {
                let result = interpret::type_of(&s, &bindings, &mut gen_type_var);
                match result {
                    (Ok(term), Ok(typ)) => println!("{:?} : {:?}", term, typ),
                    (Err(msg), _) | (_, Err(msg)) => println!("{}", msg)
                }
            }
            Some(Command::Unknown) => println!("Unknown command"),
            None => {

                let result = interpret::evaluate(&expr, &mut bindings, &mut gen_type_var);
                match result {
                    Ok(term) => println!("{:?}", term),
                    Err(msg) => println!("{}", msg)
                }
            }
        }
    }
}
