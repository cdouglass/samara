use std::io;
use std::io::Write;

const GREETING : &str = "---- samara 0.1.0 --------------------------------------------------------------";
const PROMPT : &str = "> ";

fn main() {
    println!("{}", GREETING);

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut expr = String::new();
        io::stdin().read_line(&mut expr).unwrap();

        let result = evaluate(&expr);
        println!("{}", result);
    }
}

fn evaluate(s: &str) -> String {
    s.trim().chars().rev().collect()
}
