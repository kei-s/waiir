use super::token;

trait Node {
    fn token_literal(&self) -> &String;
}

pub trait Statement: Node {}

trait Expression: Node {}

// Program
pub struct Program {
    pub statements: Vec<Box<Statement>>
}

impl Statement for Program {}

impl Node for Program {
    fn token_literal(&self) -> &String {
        self.statements[0].token_literal()
    }
}

// LetStatement
struct LetStatement {
    token: token::Token,
    name: Identifier,
    value: Expression
}

impl Statement for LetStatement {}

impl Node for LetStatement {
    fn token_literal(&self) -> &String { &self.token.literal }
}

// Identifier
struct Identifier {
    token: token::Token,
    value: String
}

impl Expression for Identifier {}

impl Node for Identifier {
    fn token_literal(&self) -> &String { &self.token.literal }
}
