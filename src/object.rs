use self::hash::HashKey;
use super::ast::{BlockStatement, Expression, Identifier};
use super::enum_with_fmt;
use super::evaluator::Environment;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

enum_with_fmt!(
    #[derive(Debug,Clone,PartialEq,Eq)]
    pub enum Object {
        Integer(i64),
        Boolean(bool),
        ReturnValue(Box<Object>),
        Function(Function),
        Builtin(Builtin),
        Array(Array),
        Hash(Hash),
        String(String),
        Quote(Quote),
        Macro(Macro),
        => // custom format
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Array {
    pub elements: Vec<Object>,
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.elements
                .iter()
                .map(|p| format!("{}", p))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Builtin {
    pub func: fn(Vec<Object>) -> Object,
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "builtin function")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashPair {
    pub key: Object,
    pub value: Object,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hash {
    pub pairs: HashMap<HashKey, HashPair>,
}

impl fmt::Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.pairs
                .iter()
                .map(|(_, pair)| format!("{}: {}", pair.key, pair.value))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Quote {
    pub node: Expression,
}

impl fmt::Display for Quote {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "QUOTE({})", self.node)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Macro {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

impl fmt::Display for Macro {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "macro({}) {{{}}}", // {{ => {
            self.parameters
                .iter()
                .map(|p| format!("{}", p))
                .collect::<Vec<_>>()
                .join(", "),
            self.body
        )
    }
}

pub mod hash {
    use super::Object;
    use std::collections::hash_map::DefaultHasher;
    use std::hash::Hasher;

    pub fn hash_key_of(object: &Object) -> Result<HashKey, String> {
        Ok(match object {
            Object::String(string) => string.hash_key(),
            Object::Integer(integer) => integer.hash_key(),
            Object::Boolean(boolean) => boolean.hash_key(),
            _ => return Err(format!("unusable as hash key: {}", object)),
        })
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub struct HashKey {
        t: HashType,
        value: u64,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    enum HashType {
        Integer,
        Boolean,
        String,
    }

    pub trait Hashable {
        fn hash_key(&self) -> HashKey;
    }

    impl Hashable for String {
        fn hash_key(&self) -> HashKey {
            let mut hasher = DefaultHasher::new();
            hasher.write(self.as_bytes());
            HashKey {
                t: HashType::String,
                value: hasher.finish(),
            }
        }
    }

    impl Hashable for i64 {
        fn hash_key(&self) -> HashKey {
            HashKey {
                t: HashType::Integer,
                value: *self as u64,
            }
        }
    }

    impl Hashable for bool {
        fn hash_key(&self) -> HashKey {
            let value = if *self { 1 } else { 0 };

            HashKey {
                t: HashType::Boolean,
                value,
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::Hashable;

        #[test]
        fn test_string_hash_key() {
            let hello1 = "Hello World".to_string();
            let hello2 = "Hello World".to_string();
            let diff1 = "My name is johnny".to_string();
            let diff2 = "My name is johnny".to_string();

            assert_eq!(hello1.hash_key(), hello2.hash_key());
            assert_eq!(diff1.hash_key(), diff2.hash_key());
            assert_ne!(hello1.hash_key(), diff2.hash_key());
        }

        #[test]
        fn test_boolean_hash_key() {
            let true1 = true;
            let true2 = true;
            let false1 = false;
            let false2 = false;

            assert_eq!(true1.hash_key(), true2.hash_key());
            assert_eq!(false1.hash_key(), false2.hash_key());
            assert_ne!(true1.hash_key(), false1.hash_key());
        }

        #[test]
        fn test_integer_hash_key() {
            let one1: i64 = 1;
            let one2: i64 = 1;
            let two1: i64 = 2;
            let two2: i64 = 2;

            assert_eq!(one1.hash_key(), one2.hash_key());
            assert_eq!(two1.hash_key(), two2.hash_key());
            assert_ne!(one1.hash_key(), two1.hash_key());
        }
    }
}
