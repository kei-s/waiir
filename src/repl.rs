use super::evaluator::{define_macros, expand_macros, Environment, Eval};
use super::{lexer, parser};
use std::io;
use std::io::prelude::Write;
use std::rc::Rc;

pub fn start() {
    const PROMPT: &str = ">> ";
    let mut env = Environment::new();
    let macro_env = Environment::new();
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let l = lexer::Lexer::new(&input);
        let mut p = parser::Parser::new(l);

        let mut program = p.parse_program();

        let errors = p.errors();
        if !errors.is_empty() {
            print_parser_errors(errors);
            continue;
        }

        define_macros(&mut program, Rc::clone(&macro_env));
        let expanded = expand_macros(program, &mut Rc::clone(&macro_env));

        let evaluated = expanded.eval(&mut env);

        println!("{}", evaluated);
    }
}

fn print_parser_errors(errors: std::vec::Vec<String>) {
    for msg in errors {
        println!("{}", msg)
    }
}
