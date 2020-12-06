mod orefs;
mod gc;
mod mutator;
mod object;
mod int;
mod bool;
mod tuple;
mod string;
mod lexer;
mod parser;
mod eval;

use rustyline::error::ReadlineError;

use mutator::{KyyMutator, KyyType};
use eval::exec;
use int::Int;
use self::bool::Bool;

fn read_lines(repl: &mut rustyline::Editor<()>) -> Result<String, ReadlineError> {
    let mut input = repl.readline(">>> ")?;
    input.push('\n');
    if input.ends_with(":\n") { // TODO: Newline escape \
        loop {
            let line = repl.readline("... ")?;
            if line.len() > 0 {
                input.push_str(&line);
                input.push('\n');
            } else {
                input.push('\n');
                break;
            }
        }
    }
    Ok(input)
}

fn main() {
    let mut repl = rustyline::Editor::new();
    let mut km = KyyMutator::new(1 << 22 /* 4 MiB */).expect("Kyy out of memory: could not create GC heap");
    let mut env = eval::new_global_env();

    loop {
        match read_lines(&mut repl) {
            Ok(lines) => {
                let lexer = lexer::KyyLexer::new(&lines, None);
                match parser::parse(&mut km, lexer) {
                    Ok(ref stmt) => match exec(&mut km, &mut env, stmt) {
                        Ok(Some(v)) => if let Some(n) = Int::downcast(&mut km, v.clone()) {
                            println!("=> {}", isize::from(n));
                        } else if let Some(b) = Bool::downcast(&mut km, v) {
                            println!("=> {}", if bool::from(b) { "True" } else { "False" });
                        } else {
                            todo!()
                        },
                        Ok(None) => (),
                        Err(err) => todo!()
                    },
                    Err(err) => println!("Syntax error: {:?}", err)
                }
            },
            Err(ReadlineError::Eof) => break,
            Err(ReadlineError::Interrupted) => continue,
            Err(err) => println!("Readline error: {}", err)
        }
    }
}

