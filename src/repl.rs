use super::evaluator::{Eval, Environment};
use super::{lexer, parser};
use std::io;
use std::io::prelude::*;

pub fn start() {
    const PROMPT: &str = ">> ";
    let mut env = Environment::new();
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let l = lexer::Lexer::new(&input);
        let mut p = parser::Parser::new(l);

        let program = p.parse_program();

        let errors = p.errors();
        if errors.len() != 0 {
            print_parser_errors(errors);
            continue;
        }

        let evaluated = program.eval(&mut env);

        println!("{}", evaluated);
    }
}

fn print_parser_errors(errors: std::vec::Vec<String>) {
    for msg in errors {
        println!("{}", msg)
    }
}
