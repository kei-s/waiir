use super::enum_with_fmt;
use std::fmt;

enum_with_fmt!(Object;
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>)
    ;
    Null => "null"
);
