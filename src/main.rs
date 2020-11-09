const PROMPT: &'static str = ">>> ";

fn main() {
    let mut repl = rustyline::Editor::<()>::new();

    loop {
        match repl.readline(PROMPT) {
            Ok(line) => println!("{}", line),
            Err(err) => {
                println!("Readline error: {:?}", err);
                break;
            }
        }
    }
}

