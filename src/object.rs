use std::fmt;

pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(o) => write!(f, "{}", o),
            Object::Boolean(o) => write!(f, "{}", o),
            Object::Null => write!(f, "null"),
        }
    }
}
