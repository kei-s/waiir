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

// Statement
#[derive(Debug,PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::LetStatement(x) => write!(f, "{}", x),
            _ => panic!()
            // Statement::ReturnStatement(x) => write!(f, "{}", x),
            // Statement::ExpressionStatement(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug,PartialEq)]
pub struct LetStatement {
    pub name: Identifier,
    // value: Expression
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = ;", self.name,)
    }
}

#[derive(Debug,PartialEq)]
pub struct ReturnStatement {
    // value: Expression
}

#[derive(Debug,PartialEq)]
pub struct ExpressionStatement {
    value: Expression
}

// Expression
#[derive(Debug,PartialEq)]
pub enum Expression {
    Identifier(Identifier)
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(x) => write!(f, "{}", x),
        }
    }
}

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
