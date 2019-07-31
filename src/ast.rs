use std::fmt;

pub struct Program {
    pub statements: Vec<Statement>,
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
gen_enum!(
    Statement,
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement)
);

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub return_value: Expression,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return {};", self.return_value)
    }
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.statements.iter().map(|s| write!(f, "{}", s)).collect()
    }
}

// Expression
gen_enum!(
    Expression,
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(Boolean),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression)
);

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub value: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct IntegerLiteral {
    pub value: i64,
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    pub operator: String,
    pub right: Box<Expression>,
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug, PartialEq)]
pub struct Boolean {
    pub value: bool,
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub consequence: Box<BlockStatement>,
    pub alternative: Option<Box<BlockStatement>>,
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(alternative) = &self.alternative {
            write!(
                f,
                "if{} {}else {}",
                self.condition, self.consequence, alternative
            )
        } else {
            write!(f, "if{} {}", self.condition, self.consequence)
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionLiteral {
    pub parameters: Vec<Identifier>,
    pub body: Box<BlockStatement>,
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn({}) {}",
            self.parameters
                .iter()
                .map(|p| format!("{}", p))
                .collect::<Vec<_>>()
                .join(", "),
            self.body
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.function,
            self.arguments
                .iter()
                .map(|p| format!("{}", p))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![
                Statement::LetStatement(LetStatement {
                    name: Identifier {
                        value: "myVar".to_string(),
                    },
                    value: Expression::Identifier(Identifier {
                        value: "anotherVar".to_string(),
                    }),
                }),
                Statement::ReturnStatement(ReturnStatement {
                    return_value: Expression::Identifier(Identifier {
                        value: "returnVar".to_string(),
                    }),
                }),
            ],
        };

        assert_eq!(
            format!("{}", program),
            "let myVar = anotherVar;return returnVar;"
        );
    }
}
