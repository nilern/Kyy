mod gc;
mod mutator;
mod int;
mod bool;
mod tuple;
mod string;
mod lexer;
mod parser;
mod eval;

use rustyline::error::ReadlineError;

use eval::exec;

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
    let mut env = eval::new_global_env();

    loop {
        match read_lines(&mut repl) {
            Ok(lines) => {
                let lexer = lexer::KyyLexer::new(&lines, None);
                match parser::parse(lexer) {
                    Ok(ref stmt) => {
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

