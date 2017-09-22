# Templates

```rust,skt-repl
extern crate samara;

use samara::declare_sum_type;
use samara::evaluate;
use samara::SumTypeDefs;
use samara::infer::GenTypeVar;
use samara::ui::Command;
use samara::ui::Inputs;

fn main() {{
    let mut sum_types = SumTypeDefs::new();
    let mut gen = GenTypeVar::new();
    let mut session_bindings = vec![];
    let mut inputs = Inputs::new_str(r"{}").peekable();

    loop {{
        match inputs.next() {{
            Some(Command::Decl(typ)) => {{
                declare_sum_type(&typ, &mut gen, &mut sum_types).unwrap();
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
            Some(Command::Command(_, _)) => panic!(),
            None => {{ break; }}
        }}
    }}
}}

```
