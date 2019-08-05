use super::ast::{BlockStatement, Identifier};
use super::enum_with_fmt;
use super::evaluator::Environment;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

enum_with_fmt!(
    #[derive(Debug,Clone,PartialEq,Eq)]
    pub enum Object {
        Integer(i64),
        Boolean(bool),
        ReturnValue(Box<Object>),
        Function(Function),
        => // custom format
        String(String) => "\"{}\"",
        Error(String) => "Error: {}",
        ;=> // without data and custom format
        Null => "null",
    }
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn({}) {{{}}}", // {{ => {
            self.parameters
                .iter()
                .map(|p| format!("{}", p))
                .collect::<Vec<_>>()
                .join(", "),
            self.body
        )
    }
}
