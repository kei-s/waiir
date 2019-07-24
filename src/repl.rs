use std::io;
use std::io::prelude::*;
use super::lexer;

pub fn start() {
    const PROMPT: &str = ">> ";
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let l = lexer::Lexer::new(&input);

        for token in l {
            println!("{:?}", token);
        }
    }
}
