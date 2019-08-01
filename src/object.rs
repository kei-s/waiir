use super::enum_with_fmt;
use std::fmt;

enum_with_fmt!(
    #[derive(Debug,PartialEq,Eq)]
    pub enum Object {
        Integer(i64),
        Boolean(bool),
        ReturnValue(Box<Object>),
        => // custom format
        Error(String) => "Error: {}",
        ;=> // without data and custom format
        Null => "null",
    }
);
