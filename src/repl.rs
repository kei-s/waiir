use std::io;
use std::io::prelude::*;
use super::lexer;
use super::token::TokenType;

pub fn start() {
    const PROMPT: &str = ">> ";
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let mut l = lexer::Lexer::new(&input);

        loop {
            let token = l.next_token();
            match token.t {
                TokenType::Eof => break,
                _ => { println!("{:?}", token); }
            }
        }
    }
}
