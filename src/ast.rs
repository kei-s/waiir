use super::enum_with_fmt;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str_iter = self.statements.iter().map(|stmt| format!("{}", stmt));
        write!(f, "{}", str_iter.collect::<Vec<String>>().join(""))
    }
}

// Statement
enum_with_fmt!(
    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum Statement {
        LetStatement(LetStatement),
        ReturnStatement(ReturnStatement),
        ExpressionStatement(ExpressionStatement),
    }
);

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ReturnStatement {
    pub return_value: Expression,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return {};", self.return_value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.statements.iter().map(|s| write!(f, "{}", s)).collect()
    }
}

// Expression
enum_with_fmt!(
    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum Expression {
        Identifier(Identifier),
        IntegerLiteral(IntegerLiteral),
        PrefixExpression(PrefixExpression),
        InfixExpression(InfixExpression),
        Boolean(Boolean),
        IfExpression(IfExpression),
        FunctionLiteral(FunctionLiteral),
        CallExpression(CallExpression),
        StringLiteral(StringLiteral),
        ArrayLiteral(ArrayLiteral),
        IndexExpression(IndexExpression),
        HashLiteral(HashLiteral),
        MacroLiteral(MacroLiteral),
    }
);

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Identifier {
    pub value: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct IntegerLiteral {
    pub value: i64,
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PrefixExpression {
    pub operator: String,
    pub right: Box<Expression>,
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Boolean {
    pub value: bool,
}

impl fmt::Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StringLiteral {
    pub value: String,
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, r#""{}""#, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}

impl fmt::Display for ArrayLiteral {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct IndexExpression {
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl fmt::Display for IndexExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HashLiteral {
    pub pairs: BTreeMap<Expression, Expression>,
}

impl fmt::Display for HashLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.pairs
                .iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MacroLiteral {
    pub parameters: Vec<Identifier>,
    pub body: Box<BlockStatement>,
}

impl fmt::Display for MacroLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "macro({}) {}",
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
pub enum Node {
    Program(Program),
    Statement(Statement),
    BlockStatement(BlockStatement),
    Expression(Expression),
}

pub fn modify<P: FnMut(Node) -> Node>(target: Node, modifier: Rc<RefCell<P>>) -> Node {
    match target {
        Node::Program(mut program) => {
            for i in 0..program.statements.len() {
                let statement = program.statements.swap_remove(i);
                if let Node::Statement(stmt) =
                    modify(Node::Statement(statement), Rc::clone(&modifier))
                {
                    program.statements.insert(i, stmt);
                }
            }
            (&mut *modifier.borrow_mut())(Node::Program(program))
        }
        Node::BlockStatement(mut node) => {
            for i in 0..node.statements.len() {
                let statement = node.statements.swap_remove(i);
                if let Node::Statement(stmt) =
                    modify(Node::Statement(statement), Rc::clone(&modifier))
                {
                    node.statements.insert(i, stmt)
                }
            }
            (&mut *modifier.borrow_mut())(Node::BlockStatement(node))
        }
        Node::Statement(Statement::ExpressionStatement(node)) => {
            if let Node::Expression(expression) =
                modify(Node::Expression(node.expression), Rc::clone(&modifier))
            {
                (&mut *modifier.borrow_mut())(Node::Statement(Statement::ExpressionStatement(
                    ExpressionStatement { expression },
                )))
            } else {
                unreachable!()
            }
        }
        Node::Expression(Expression::InfixExpression(node)) => {
            if let Node::Expression(left) =
                modify(Node::Expression(*node.left), Rc::clone(&modifier))
            {
                if let Node::Expression(right) =
                    modify(Node::Expression(*node.right), Rc::clone(&modifier))
                {
                    (&mut *modifier.borrow_mut())(Node::Expression(Expression::InfixExpression(
                        InfixExpression {
                            left: Box::new(left),
                            right: Box::new(right),
                            operator: node.operator,
                        },
                    )))
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            }
        }
        Node::Expression(Expression::PrefixExpression(node)) => {
            if let Node::Expression(right) =
                modify(Node::Expression(*node.right), Rc::clone(&modifier))
            {
                (&mut *modifier.borrow_mut())(Node::Expression(Expression::PrefixExpression(
                    PrefixExpression {
                        right: Box::new(right),
                        operator: node.operator,
                    },
                )))
            } else {
                unreachable!()
            }
        }
        Node::Expression(Expression::IndexExpression(node)) => {
            if let Node::Expression(left) =
                modify(Node::Expression(*node.left), Rc::clone(&modifier))
            {
                if let Node::Expression(index) =
                    modify(Node::Expression(*node.index), Rc::clone(&modifier))
                {
                    (&mut *modifier.borrow_mut())(Node::Expression(Expression::IndexExpression(
                        IndexExpression {
                            left: Box::new(left),
                            index: Box::new(index),
                        },
                    )))
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            }
        }
        Node::Expression(Expression::IfExpression(node)) => {
            if let Node::Expression(condition) =
                modify(Node::Expression(*node.condition), Rc::clone(&modifier))
            {
                if let Node::BlockStatement(consequence) = modify(
                    Node::BlockStatement(*node.consequence),
                    Rc::clone(&modifier),
                ) {
                    if node.alternative.is_some() {
                        if let Node::BlockStatement(alternative) = modify(
                            Node::BlockStatement(*node.alternative.unwrap()),
                            Rc::clone(&modifier),
                        ) {
                            (&mut *modifier.borrow_mut())(Node::Expression(
                                Expression::IfExpression(IfExpression {
                                    condition: Box::new(condition),
                                    consequence: Box::new(consequence),
                                    alternative: Some(Box::new(alternative)),
                                }),
                            ))
                        } else {
                            unreachable!()
                        }
                    } else {
                        (&mut *modifier.borrow_mut())(Node::Expression(Expression::IfExpression(
                            IfExpression {
                                condition: Box::new(condition),
                                consequence: Box::new(consequence),
                                alternative: None,
                            },
                        )))
                    }
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            }
        }
        Node::Expression(Expression::FunctionLiteral(mut node)) => {
            for i in 0..node.parameters.len() {
                let identifier = node.parameters.swap_remove(i);
                if let Node::Expression(Expression::Identifier(ident)) = modify(
                    Node::Expression(Expression::Identifier(identifier)),
                    Rc::clone(&modifier),
                ) {
                    node.parameters.insert(i, ident);
                }
            }
            if let Node::BlockStatement(body) =
                modify(Node::BlockStatement(*node.body), Rc::clone(&modifier))
            {
                (&mut *modifier.borrow_mut())(Node::Expression(Expression::FunctionLiteral(
                    FunctionLiteral {
                        parameters: node.parameters,
                        body: Box::new(body),
                    },
                )))
            } else {
                unreachable!()
            }
        }
        Node::Expression(Expression::ArrayLiteral(mut node)) => {
            for i in 0..node.elements.len() {
                let element = node.elements.swap_remove(i);
                if let Node::Expression(elem) =
                    modify(Node::Expression(element), Rc::clone(&modifier))
                {
                    node.elements.insert(i, elem);
                }
            }
            (&mut *modifier.borrow_mut())(Node::Expression(Expression::ArrayLiteral(node)))
        }
        Node::Expression(Expression::HashLiteral(node)) => {
            let mut new_pairs = BTreeMap::new();
            for (key, value) in node.pairs {
                if let Node::Expression(k) = modify(Node::Expression(key), Rc::clone(&modifier)) {
                    if let Node::Expression(v) =
                        modify(Node::Expression(value), Rc::clone(&modifier))
                    {
                        new_pairs.insert(k, v);
                    }
                }
            }
            (&mut *modifier.borrow_mut())(Node::Expression(Expression::HashLiteral(HashLiteral {
                pairs: new_pairs,
            })))
        }
        Node::Statement(Statement::ReturnStatement(node)) => {
            if let Node::Expression(return_value) =
                modify(Node::Expression(node.return_value), Rc::clone(&modifier))
            {
                (&mut *modifier.borrow_mut())(Node::Statement(Statement::ReturnStatement(
                    ReturnStatement { return_value },
                )))
            } else {
                unreachable!()
            }
        }
        Node::Statement(Statement::LetStatement(node)) => {
            if let Node::Expression(value) =
                modify(Node::Expression(node.value), Rc::clone(&modifier))
            {
                (&mut *modifier.borrow_mut())(Node::Statement(Statement::LetStatement(
                    LetStatement {
                        name: node.name,
                        value,
                    },
                )))
            } else {
                unreachable!()
            }
        }
        _ => (&mut *modifier.borrow_mut())(target),
    }
}

#[cfg(test)]
mod tests {
    use super::{
        modify, ArrayLiteral, BTreeMap, BlockStatement, Expression, ExpressionStatement,
        FunctionLiteral, HashLiteral, Identifier, IfExpression, IndexExpression, InfixExpression,
        IntegerLiteral, LetStatement, Node, PrefixExpression, Program, ReturnStatement, Statement,
    };
    use std::cell::RefCell;
    use std::rc::Rc;

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

    #[test]
    fn test_modify() {
        fn one() -> Expression {
            Expression::IntegerLiteral(IntegerLiteral { value: 1 })
        }
        fn two() -> Expression {
            Expression::IntegerLiteral(IntegerLiteral { value: 2 })
        }
        let turn_one_into_two = |node: Node| -> Node {
            if let Node::Expression(Expression::IntegerLiteral(integer)) = &node {
                if integer.value != 1 {
                    return node;
                }
                Node::Expression(Expression::IntegerLiteral(IntegerLiteral { value: 2 }))
            } else {
                node
            }
        };

        let mut input_map = BTreeMap::new();
        input_map.insert(one(), one());
        input_map.insert(one(), one());
        let mut expected_map = BTreeMap::new();
        expected_map.insert(two(), two());
        expected_map.insert(two(), two());

        let mut tests = vec![
            (Node::Expression(one()), Node::Expression(two())),
            (
                Node::Program(Program {
                    statements: vec![Statement::ExpressionStatement(ExpressionStatement {
                        expression: one(),
                    })],
                }),
                Node::Program(Program {
                    statements: vec![Statement::ExpressionStatement(ExpressionStatement {
                        expression: two(),
                    })],
                }),
            ),
            (
                Node::Expression(Expression::InfixExpression(InfixExpression {
                    left: Box::new(one()),
                    operator: "+".to_string(),
                    right: Box::new(two()),
                })),
                Node::Expression(Expression::InfixExpression(InfixExpression {
                    left: Box::new(two()),
                    operator: "+".to_string(),
                    right: Box::new(two()),
                })),
            ),
            (
                Node::Expression(Expression::InfixExpression(InfixExpression {
                    left: Box::new(two()),
                    operator: "+".to_string(),
                    right: Box::new(one()),
                })),
                Node::Expression(Expression::InfixExpression(InfixExpression {
                    left: Box::new(two()),
                    operator: "+".to_string(),
                    right: Box::new(two()),
                })),
            ),
            (
                Node::Expression(Expression::PrefixExpression(PrefixExpression {
                    operator: "-".to_string(),
                    right: Box::new(one()),
                })),
                Node::Expression(Expression::PrefixExpression(PrefixExpression {
                    operator: "-".to_string(),
                    right: Box::new(two()),
                })),
            ),
            (
                Node::Expression(Expression::IndexExpression(IndexExpression {
                    left: Box::new(one()),
                    index: Box::new(one()),
                })),
                Node::Expression(Expression::IndexExpression(IndexExpression {
                    left: Box::new(two()),
                    index: Box::new(two()),
                })),
            ),
            (
                Node::Expression(Expression::IfExpression(IfExpression {
                    condition: Box::new(one()),
                    consequence: Box::new(BlockStatement {
                        statements: vec![Statement::ExpressionStatement(ExpressionStatement {
                            expression: one(),
                        })],
                    }),
                    alternative: Some(Box::new(BlockStatement {
                        statements: vec![Statement::ExpressionStatement(ExpressionStatement {
                            expression: one(),
                        })],
                    })),
                })),
                Node::Expression(Expression::IfExpression(IfExpression {
                    condition: Box::new(two()),
                    consequence: Box::new(BlockStatement {
                        statements: vec![Statement::ExpressionStatement(ExpressionStatement {
                            expression: two(),
                        })],
                    }),
                    alternative: Some(Box::new(BlockStatement {
                        statements: vec![Statement::ExpressionStatement(ExpressionStatement {
                            expression: two(),
                        })],
                    })),
                })),
            ),
            (
                Node::Statement(Statement::ReturnStatement(ReturnStatement {
                    return_value: one(),
                })),
                Node::Statement(Statement::ReturnStatement(ReturnStatement {
                    return_value: two(),
                })),
            ),
            (
                Node::Statement(Statement::LetStatement(LetStatement {
                    name: Identifier {
                        value: "value".to_string(),
                    },
                    value: one(),
                })),
                Node::Statement(Statement::LetStatement(LetStatement {
                    name: Identifier {
                        value: "value".to_string(),
                    },
                    value: two(),
                })),
            ),
            (
                Node::Expression(Expression::FunctionLiteral(FunctionLiteral {
                    parameters: vec![],
                    body: Box::new(BlockStatement {
                        statements: vec![Statement::ExpressionStatement(ExpressionStatement {
                            expression: one(),
                        })],
                    }),
                })),
                Node::Expression(Expression::FunctionLiteral(FunctionLiteral {
                    parameters: vec![],
                    body: Box::new(BlockStatement {
                        statements: vec![Statement::ExpressionStatement(ExpressionStatement {
                            expression: two(),
                        })],
                    }),
                })),
            ),
            (
                Node::Expression(Expression::ArrayLiteral(ArrayLiteral {
                    elements: vec![one(), one()],
                })),
                Node::Expression(Expression::ArrayLiteral(ArrayLiteral {
                    elements: vec![two(), two()],
                })),
            ),
            (
                Node::Expression(Expression::HashLiteral(HashLiteral { pairs: input_map })),
                Node::Expression(Expression::HashLiteral(HashLiteral {
                    pairs: expected_map,
                })),
            ),
        ];

        while !tests.is_empty() {
            let (input, expected) = tests.pop().unwrap();
            let modified = modify(input, Rc::new(RefCell::new(turn_one_into_two)));
            assert_eq!(modified, expected);
        }
    }
}
