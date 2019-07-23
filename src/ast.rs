use std::fmt;

pub struct Program {
    pub statements: Vec<Statement>
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str_iter = self.statements.iter().map(|stmt| format!("{}", stmt));
        write!(f, "{}", str_iter.collect::<Vec<String>>().join(""))
    }
}

/*
Enum generation macro

This macro generates below code.

```
#[derive(Debug,PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ...
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::LetStatement(x) => write!(f, "{}", x),
            Statement::ReturnStatement(x) => write!(f, "{}", x),
            ...
        }
    }
}
```
*/
macro_rules! gen_enum {
    ($name:ident, $($var:ident($ty:ty)),+) => {
        #[derive(Debug,PartialEq)]
        pub enum $name {
            $(
                $var($ty),
            )*
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        $name::$var(x) => write!(f, "{}", x),
                    )+
                }
            }
        }
    };
}

// Statement
gen_enum!(Statement,
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement)
);

#[derive(Debug,PartialEq)]
pub struct LetStatement {
    pub name: Identifier,
    // value: Expression
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = ;", self.name)
    }
}

#[derive(Debug,PartialEq)]
pub struct ReturnStatement {
    // value: Expression
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return {};", "DUMMY")
    }
}

#[derive(Debug,PartialEq)]
pub struct ExpressionStatement {
    value: Expression
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

// Expression
gen_enum!(Expression,
    Identifier(Identifier)
);

#[derive(Debug,PartialEq)]
pub struct Identifier {
    pub value: String
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value,)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let program = Program{
            statements: vec![
                Statement::LetStatement(LetStatement {
                    name: Identifier {
                    value: "myVar".to_string()
                }})
            ]
        };

        assert_eq!(format!("{}", program), "let myVar = ;");
    }
}
