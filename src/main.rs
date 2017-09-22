extern crate samara;

use samara::declare_sum_type;
use samara::evaluate;
use samara::type_of;
use samara::SumTypeDefs;
use samara::infer::GenTypeVar;
use samara::ui::Command as UiCmd;
use samara::ui::Inputs;

use std::io;
use std::io::Write;
use std::iter::Iterator;

const PROMPT : &str = "> ";
const GREETING : &str = "---- samara 0.1.0 --------------------------------------------------------------";
const USAGE : &str = "Available commands:\n:exit        - exit\n:help        - help\n:type <expr> - show type of <expr>";

fn main() {
    println!("{}", GREETING);
    let mut bindings = vec![];
    let mut gen_type_var = GenTypeVar::new();
    let mut sum_type_defs = SumTypeDefs::new();

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let stdin = io::stdin();
        let stdout = io::stdout();
        let mut inputs = Inputs::new_io(stdin.lock(), stdout.lock(), ":", "| ", "\\", "").peekable();

        match inputs.next().unwrap() {
            UiCmd::Command(cmd, arg) => {
                if &cmd == "exit" {
                    break;
                } else if &cmd == "help" {
                    println!("{}", USAGE)
                } else if &cmd == "type" {
                    let expr = &arg.unwrap_or(String::new());
                    let result = type_of(&expr, &bindings, &mut gen_type_var, &sum_type_defs);
                    match result {
                        (Ok(term), Ok(typ)) => println!("{:?} : {:?}", term, typ),
                        (Err(msg), _) | (_, Err(msg)) => println!("{}", msg)
                    }
                } else {
                    println!("Unknown command: {}", cmd)
                }
            }
            UiCmd::Decl(expr) => {
                let new_sum_type = declare_sum_type(&expr, &mut sum_type_defs);
                match new_sum_type {
                    Ok(sum) => println!("{:?}", sum),
                    Err(msg) => println!("{:?}", msg)
                }
            },
            UiCmd::Eval(expr) | UiCmd::Expected(expr) => {
               let result = evaluate(&expr, &mut bindings, &mut gen_type_var, &sum_type_defs);
               match result {
                   Ok(term) => println!("{:?}", term),
                   Err(msg) => println!("{}", msg)
               }
            }
        }
    }
}
