mod lexer;

use lexer::{Located, SyntaxError, KyyLexer};

use rustyline::error::ReadlineError;

const PROMPT: &'static str = ">>> ";

fn main() {
    let mut repl = rustyline::Editor::<()>::new();

    loop {
        match repl.readline(PROMPT) {
            Ok(line) => {
                let lexer = KyyLexer::new(&line, None);
                match lexer.collect::<Result<Vec<_>, Located<SyntaxError>>>() {
                    Ok(tokens) => println!("{:#?}", tokens),
                    Err(err) => println!("Syntax error: {:?}", err)
                }
            },
            Err(ReadlineError::Eof) => break,
            Err(ReadlineError::Interrupted) => continue,
            Err(err) => println!("Readline error: {}", err)
        }
    }
}

