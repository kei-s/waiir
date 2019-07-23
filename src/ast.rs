pub struct Program {
    pub statements: Vec<Statement>
}

// Statement
#[derive(Debug,PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
}

#[derive(Debug,PartialEq)]
pub struct LetStatement {
    pub identifier: Identifier,
    // value: Expression
}

#[derive(Debug,PartialEq)]
pub struct ReturnStatement {
    // value: Expression
}

// Expression
pub enum Expression {
    Identifier(Identifier)
}

#[derive(Debug,PartialEq)]
pub struct Identifier {
    pub value: String
}
