mod lexer;
mod parser;
mod eval;

use rustyline::error::ReadlineError;

use eval::eval;

const PROMPT: &'static str = ">>> ";

fn main() {
    let mut repl = rustyline::Editor::<()>::new();

    loop {
        match repl.readline(PROMPT) {
            Ok(line) => {
                let lexer = lexer::KyyLexer::new(&line, None);
                match parser::parse(lexer) {
                    Ok(expr) => {
                        println!("{:#?}", expr);
                        println!("=> {:?}", eval(expr));
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

