mod lexer;
mod parser;
mod eval;

use rustyline::error::ReadlineError;

use eval::exec;

const PROMPT: &'static str = ">>> ";

fn main() {
    let mut repl = rustyline::Editor::<()>::new();
    let mut env = eval::new_global_env();

    loop {
        match repl.readline(PROMPT) {
            Ok(line) => {
                let lexer = lexer::KyyLexer::new(&line, None);
                match parser::parse(lexer) {
                    Ok(stmt) => {
                        println!("{:#?}", stmt);
                        println!("=> {:?}", exec(&mut env, stmt));
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

