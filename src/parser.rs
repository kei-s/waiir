use super::ast::{
    ArrayLiteral, BlockStatement, Boolean, CallExpression, Expression, ExpressionStatement,
    FunctionLiteral, HashLiteral, Identifier, IfExpression, IndexExpression, InfixExpression,
    IntegerLiteral, LetStatement, MacroLiteral, PrefixExpression, Program, ReturnStatement,
    Statement, StringLiteral,
};
use super::lexer::Lexer;
use super::token::{Token, TokenType};
use std::collections::BTreeMap;
use std::fmt;

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > OR <
    Sum,         // +
    Product,     // *
    Prefix,      // -X OR !X
    Call,        // myFunction(x)
    Index,       // array[index]
}

#[derive(Debug)]
struct ParseError {
    message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ParseError {}

pub struct Parser {
    l: Lexer,
    _cur_token: Option<Token>,
    _peek_token: Option<Token>,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(mut l: Lexer) -> Parser {
        let _cur_token = l.next();
        let _peek_token = l.next();

        Parser {
            l,
            _cur_token,
            _peek_token,
            errors: vec![],
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = vec![];

        while self._cur_token.is_some() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(error) => self.errors.push(error),
            }
            self.next_token();
        }
        Program { statements }
    }

    pub fn errors(&self) -> std::vec::Vec<String> {
        self.errors.iter().map(|e| format!("{}", e)).collect()
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        Ok(match self.cur_token().t {
            TokenType::Let => Statement::LetStatement(self.parse_let_statement()?),
            TokenType::Return => Statement::ReturnStatement(self.parse_return_statement()?),
            _ => Statement::ExpressionStatement(self.parse_expression_statement()?),
        })
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, ParseError> {
        self.expect_peek(&TokenType::Ident);

        let name = Identifier {
            value: self.cur_token().literal.clone(),
        };

        self.expect_peek(&TokenType::Assign);

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(LetStatement { name, value })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ParseError> {
        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }

        Ok(ReturnStatement { return_value })
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, ParseError> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token()
        }

        Ok(ExpressionStatement { expression })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let mut left_exp = self.parse_prefix()?;

        loop {
            if !self.peek_token_is(&TokenType::Semicolon) && precedence < self.peek_precedence() {
                left_exp = match self.peek_token().unwrap().t {
                    TokenType::Plus
                    | TokenType::Minus
                    | TokenType::Slash
                    | TokenType::Asterisk
                    | TokenType::Eq
                    | TokenType::NotEq
                    | TokenType::Lt
                    | TokenType::Gt => {
                        self.next_token();
                        Expression::InfixExpression(self.parse_infix_expression(left_exp)?)
                    }
                    TokenType::LBracket => {
                        self.next_token();
                        Expression::IndexExpression(self.parse_index_expression(left_exp)?)
                    }
                    TokenType::LParen => {
                        self.next_token();
                        Expression::CallExpression(self.parse_call_expression(left_exp)?)
                    }
                    _ => return Ok(left_exp),
                };
            } else {
                break;
            }
        }

        Ok(left_exp)
    }

    fn parse_prefix(&mut self) -> Result<Expression, ParseError> {
        Ok(match self.cur_token().t {
            TokenType::Ident => Expression::Identifier(self.parse_identifier()),
            TokenType::Int => Expression::IntegerLiteral(self.parse_integer_literal()?),
            TokenType::Bang | TokenType::Minus => {
                Expression::PrefixExpression(self.parse_prefix_expression()?)
            }
            TokenType::True | TokenType::False => Expression::Boolean(self.parse_boolean()),
            TokenType::LParen => self.parse_grouped_expression()?,
            TokenType::If => Expression::IfExpression(self.parse_if_expression()?),
            TokenType::Function => Expression::FunctionLiteral(self.parse_function_literal()?),
            TokenType::String => Expression::StringLiteral(self.parse_string_literal()?),
            TokenType::LBracket => Expression::ArrayLiteral(self.parse_array_literal()?),
            TokenType::LBrace => Expression::HashLiteral(self.parse_hash_literal()?),
            TokenType::Macro => Expression::MacroLiteral(self.parse_macro_literal()?),
            _ => {
                return Err(ParseError {
                    message: String::from("not implemented"),
                })
            }
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<InfixExpression, ParseError> {
        let operator = self.cur_token().literal.clone();
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Ok(InfixExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<IndexExpression, ParseError> {
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(&TokenType::RBracket) {
            Err(ParseError {
                message: String::from("Missing RBracket"),
            })
        } else {
            Ok(IndexExpression {
                left: Box::new(left),
                index: Box::new(index),
            })
        }
    }

    fn parse_identifier(&self) -> Identifier {
        Identifier {
            value: self.cur_token().literal.clone(),
        }
    }

    fn parse_integer_literal(&self) -> Result<IntegerLiteral, ParseError> {
        match self.cur_token().literal.parse() {
            Ok(value) => Ok(IntegerLiteral { value }),
            Err(_) => Err(ParseError {
                message: format!("could not parse {} as integer", self.cur_token().literal),
            }),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<PrefixExpression, ParseError> {
        let operator = self.cur_token().literal.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(PrefixExpression {
            operator,
            right: Box::new(right),
        })
    }

    fn parse_boolean(&mut self) -> Boolean {
        Boolean {
            value: self.cur_token_is(&TokenType::True),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(&TokenType::RParen) {
            Err(ParseError {
                message: String::from("Missing RParen"),
            })
        } else {
            Ok(exp)
        }
    }

    fn parse_if_expression(&mut self) -> Result<IfExpression, ParseError> {
        if !self.expect_peek(&TokenType::LParen) {
            return Err(ParseError {
                message: String::from("Missing LParen"),
            });
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&TokenType::RParen) {
            return Err(ParseError {
                message: String::from("Missing RParen"),
            });
        }
        if !self.expect_peek(&TokenType::LBrace) {
            return Err(ParseError {
                message: String::from("Missing LBrace"),
            });
        }

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token_is(&TokenType::Else) {
            self.next_token();
            if !self.expect_peek(&TokenType::LBrace) {
                return Err(ParseError {
                    message: String::from("Missing LBrace"),
                });
            }
            Some(Box::new(self.parse_block_statement()?))
        } else {
            None
        };

        Ok(IfExpression {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative,
        })
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let mut statements: Vec<Statement> = vec![];

        self.next_token();

        while !self.cur_token_is(&TokenType::RBrace) && self._cur_token.is_some() {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(BlockStatement { statements })
    }

    fn parse_function_literal(&mut self) -> Result<FunctionLiteral, ParseError> {
        if !self.expect_peek(&TokenType::LParen) {
            return Err(ParseError {
                message: String::from("Missing LParen"),
            });
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(&TokenType::LBrace) {
            return Err(ParseError {
                message: String::from("Missing LBrace"),
            });
        }

        let body = self.parse_block_statement()?;

        Ok(FunctionLiteral {
            parameters,
            body: Box::new(body),
        })
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, ParseError> {
        let mut identifiers: Vec<Identifier> = vec![];

        if self.peek_token_is(&TokenType::RParen) {
            self.next_token();
            return Ok(identifiers);
        }

        self.next_token();

        identifiers.push(Identifier {
            value: self.cur_token().literal.clone(),
        });

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            identifiers.push(Identifier {
                value: self.cur_token().literal.clone(),
            });
        }

        if !self.expect_peek(&TokenType::RParen) {
            return Err(ParseError {
                message: String::from("Missing RParen"),
            });
        }

        Ok(identifiers)
    }

    fn parse_call_expression(
        &mut self,
        function: Expression,
    ) -> Result<CallExpression, ParseError> {
        let arguments = self.parse_expression_list(TokenType::RParen)?;
        Ok(CallExpression {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Result<Vec<Expression>, ParseError> {
        let mut args: Vec<Expression> = vec![];

        if self.peek_token_is(&end) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.expect_peek(&end) {
            return Err(ParseError {
                message: String::from("Missing RParen"),
            });
        }

        Ok(args)
    }

    fn parse_string_literal(&self) -> Result<StringLiteral, ParseError> {
        Ok(StringLiteral {
            value: self.cur_token().literal.clone(),
        })
    }

    fn parse_array_literal(&mut self) -> Result<ArrayLiteral, ParseError> {
        let elements = self.parse_expression_list(TokenType::RBracket)?;
        Ok(ArrayLiteral { elements })
    }

    fn parse_hash_literal(&mut self) -> Result<HashLiteral, ParseError> {
        let mut pairs = BTreeMap::new();

        while !self.peek_token_is(&TokenType::RBrace) {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;

            if !self.expect_peek(&TokenType::Colon) {
                return Err(ParseError {
                    message: String::from("Missing Colon"),
                });
            }

            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;

            pairs.insert(key, value);

            if !self.peek_token_is(&TokenType::RBrace) && !self.expect_peek(&TokenType::Comma) {
                return Err(ParseError {
                    message: String::from("Missing RBrace or Comma"),
                });
            }
        }

        if !self.expect_peek(&TokenType::RBrace) {
            return Err(ParseError {
                message: String::from("Missing RBrace"),
            });
        }

        Ok(HashLiteral { pairs })
    }

    fn parse_macro_literal(&mut self) -> Result<MacroLiteral, ParseError> {
        if !self.expect_peek(&TokenType::LParen) {
            return Err(ParseError {
                message: String::from("Missing LParen"),
            });
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(&TokenType::LBrace) {
            return Err(ParseError {
                message: String::from("Missing LBrace"),
            });
        }

        let body = self.parse_block_statement()?;

        Ok(MacroLiteral {
            parameters,
            body: Box::new(body),
        })
    }
    fn next_token(&mut self) {
        self._cur_token = self._peek_token.take();
        self._peek_token = self.l.next();
    }

    fn cur_token(&self) -> &Token {
        self._cur_token.as_ref().unwrap()
    }

    fn peek_token(&self) -> Option<&Token> {
        self._peek_token.as_ref()
    }

    fn cur_token_is(&self, t: &TokenType) -> bool {
        &self.cur_token().t == t
    }

    fn peek_token_is(&self, t: &TokenType) -> bool {
        self.peek_token().is_some() && &self.peek_token().unwrap().t == t
    }

    fn expect_peek(&mut self, t: &TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.errors.push(ParseError {
                message: format!(
                    "expected next token to be {:?}, got {:?} instead",
                    t,
                    self.peek_token().unwrap().t
                ),
            });
            false
        }
    }

    fn cur_precedence(&self) -> Precedence {
        Parser::precedence(&self.cur_token().t)
    }

    fn peek_precedence(&self) -> Precedence {
        if let Some(peek) = self.peek_token() {
            Parser::precedence(&peek.t)
        } else {
            Precedence::Lowest
        }
    }

    fn precedence(t: &TokenType) -> Precedence {
        match t {
            TokenType::Eq => Precedence::Equals,
            TokenType::NotEq => Precedence::Equals,
            TokenType::Lt => Precedence::LessGreater,
            TokenType::Gt => Precedence::LessGreater,
            TokenType::Plus => Precedence::Sum,
            TokenType::Minus => Precedence::Sum,
            TokenType::Slash => Precedence::Product,
            TokenType::Asterisk => Precedence::Product,
            TokenType::LParen => Precedence::Call,
            TokenType::LBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::ast::{Expression, Statement};
    use super::{Lexer, Parser};
    use std::collections::HashMap;
    use std::vec::Vec;

    #[test]
    fn test_let_statements() {
        test(vec![("let x = 5;", "x", 5)]);
        test(vec![("let y = true;", "y", true)]);
        test(vec![("let foobar = y;", "foobar", "y".to_string())]);

        fn test<T: AssertWithExpression>(tests: Vec<(&str, &str, T)>) {
            for (input, expected_identifier, expected_value) in tests {
                let l = Lexer::new(input);
                let mut p = Parser::new(l);
                let program = p.parse_program();
                check_parse_errors(p);

                assert_eq!(program.statements.len(), 1);
                assert_let_statement(&program.statements[0], expected_identifier);

                if let Statement::LetStatement(stmt) = &program.statements[0] {
                    assert_literal_expression(&stmt.value, expected_value);
                }
            }
        }
    }

    #[test]
    fn test_return_statements() {
        test(vec![("return 5;", 5)]);
        test(vec![("return true;", true)]);
        test(vec![("return foobar", "foobar".to_string())]);

        fn test<T: AssertWithExpression>(tests: Vec<(&str, T)>) {
            for (input, expected_value) in tests {
                let l = Lexer::new(input);
                let mut p = Parser::new(l);
                let program = p.parse_program();
                check_parse_errors(p);

                assert_eq!(program.statements.len(), 1);
                if let Statement::ReturnStatement(stmt) = &program.statements[0] {
                    assert_literal_expression(&stmt.return_value, expected_value)
                } else {
                    assert!(false, "stmt not ast::ReturnStatement")
                }
            }
        }
    }

    #[test]
    fn test_identifier_expressions() {
        let input = "foobar;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 1);

        if let Statement::ExpressionStatement(exp_stmt) = &program.statements[0] {
            if let Expression::Identifier(ident) = &exp_stmt.expression {
                assert_eq!(ident.value, "foobar");
            } else {
                assert!(false, "program.statements[0] is not ast::Identifier")
            }
        } else {
            assert!(
                false,
                "program.statements[0] is not ast::ExpressionStatement"
            )
        }
    }

    #[test]
    fn test_integer_literal_expressions() {
        let input = "5;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 1);

        if let Statement::ExpressionStatement(exp_stmt) = &program.statements[0] {
            if let Expression::IntegerLiteral(literal) = &exp_stmt.expression {
                assert_eq!(literal.value, 5);
            } else {
                assert!(false, "program.statements[0] is not ast::IntegerLiteral")
            }
        } else {
            assert!(
                false,
                "program.statements[0] is not ast::ExpressionStatement"
            )
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        test(vec![("!5;", "!", 5), ("-15;", "-", 15)]);
        test(vec![("!true;", "!", true), ("!false;", "!", false)]);

        fn test<T: AssertWithExpression>(tests: Vec<(&str, &str, T)>) {
            for (input, expected_operator, expected_value) in tests {
                let l = Lexer::new(input);
                let mut p = Parser::new(l);
                let program = p.parse_program();
                check_parse_errors(p);

                assert_eq!(program.statements.len(), 1);
                if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
                    if let Expression::PrefixExpression(exp) = &stmt.expression {
                        assert_eq!(exp.operator, expected_operator);
                        assert_literal_expression(&exp.right, expected_value);
                    } else {
                        assert!(false, "stmt is not ast::PrefixExpression");
                    }
                } else {
                    assert!(
                        false,
                        "program.statements[0] is not ast::ExpressionStatement"
                    );
                }
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        test(vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ]);

        test(vec![
            ("true == true", true, "==", true),
            ("true != false", true, "!=", false),
            ("false == false", false, "==", false),
        ]);

        fn test<T: AssertWithExpression, U: AssertWithExpression>(tests: Vec<(&str, T, &str, U)>) {
            for (input, expected_left_value, expected_operator, expected_right_value) in tests {
                let l = Lexer::new(input);
                let mut p = Parser::new(l);
                let program = p.parse_program();
                check_parse_errors(p);

                assert_eq!(program.statements.len(), 1);
                if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
                    assert_infix_expression(
                        &stmt.expression,
                        expected_left_value,
                        expected_operator,
                        expected_right_value,
                    );
                } else {
                    assert!(
                        false,
                        "program.statements[0] is not ast::ExpressionStatement"
                    );
                }
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, expected) in tests.iter() {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parse_errors(p);

            assert_eq!(format!("{}", program), *expected);
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = [("true;", true), ("false;", false)];

        for (input, expected) in tests.iter() {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parse_errors(p);
            assert_eq!(program.statements.len(), 1);

            if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
                if let Expression::Boolean(boolean) = &stmt.expression {
                    assert_eq!(boolean.value, *expected);
                } else {
                    assert!(false, "program.statements[0] is not ast::BooleanExpression")
                }
            } else {
                assert!(
                    false,
                    "program.statements[0] is not ast::ExpressionStatement"
                )
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 1);

        if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let Expression::IfExpression(exp) = &stmt.expression {
                assert_infix_expression(&exp.condition, "x".to_string(), "<", "y".to_string());
                assert_eq!(exp.consequence.statements.len(), 1);
                if let Statement::ExpressionStatement(consequence) = &exp.consequence.statements[0]
                {
                    assert_identifier(&consequence.expression, "x");
                } else {
                    assert!(false, "statements[0] is not ast::ExpressionStatement")
                }
                assert_eq!(exp.alternative, None);
            } else {
                assert!(false, "stmt.expression is not ast::IfExpression")
            }
        } else {
            assert!(
                false,
                "program.statements[0] is not ast::ExpressionStatement"
            )
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 1);

        if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let Expression::IfExpression(exp) = &stmt.expression {
                assert_infix_expression(&exp.condition, "x".to_string(), "<", "y".to_string());
                assert_eq!(exp.consequence.statements.len(), 1);
                if let Statement::ExpressionStatement(consequence) = &exp.consequence.statements[0]
                {
                    assert_identifier(&consequence.expression, "x");
                } else {
                    assert!(false, "statements[0] is not ast::ExpressionStatement")
                }
                if let Some(exp_alternative) = &exp.alternative {
                    assert_eq!(exp_alternative.statements.len(), 1);
                    if let Statement::ExpressionStatement(alternative) =
                        &(*exp_alternative).statements[0]
                    {
                        assert_identifier(&alternative.expression, "y");
                    } else {
                        assert!(false, "statements[0] is not ast::ExpressionStatement")
                    }
                } else {
                    assert!(false, "exp.alternative is None")
                }
            } else {
                assert!(false, "stmt.expression is not ast::IfExpression")
            }
        } else {
            assert!(
                false,
                "program.statements[0] is not ast::ExpressionStatement"
            )
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 1);

        if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let Expression::FunctionLiteral(function) = &stmt.expression {
                assert_eq!(function.parameters[0].value, "x".to_string());
                assert_eq!(format!("{}", function.parameters[0].value), "x");
                assert_eq!(function.parameters[1].value, "y".to_string());
                assert_eq!(format!("{}", function.parameters[1].value), "y");
            } else {
                assert!(false, "stmt.expression is not ast::FunctionLiteral")
            }
        } else {
            assert!(
                false,
                "program.statements[0] is not ast::ExpressionStatement"
            )
        }
    }

    #[test]
    fn test_function_parameter_parsing() {
        let tests = [
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x, y, z) {};", vec!["x", "y", "z"]),
        ];

        for (input, expected) in tests.iter() {
            let l = Lexer::new(input);
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parse_errors(p);

            if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
                if let Expression::FunctionLiteral(function) = &stmt.expression {
                    assert_eq!(function.parameters.len(), expected.len());

                    for (i, ident) in expected.iter().enumerate() {
                        assert_eq!(function.parameters[i].value, ident.to_string());
                        assert_eq!(
                            format!("{}", function.parameters[i].value),
                            ident.to_string()
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);

        assert_eq!(program.statements.len(), 1);
        if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let Expression::CallExpression(exp) = &stmt.expression {
                assert_identifier(&exp.function, "add");
                assert_eq!(exp.arguments.len(), 3);
                assert_literal_expression(&exp.arguments[0], 1);
                assert_infix_expression(&exp.arguments[1], 2, "*", 3);
                assert_infix_expression(&exp.arguments[2], 4, "+", 5);
            } else {
                assert!(false, "stmt.expression is not ast::CallExpression")
            }
        } else {
            assert!(false, "stmt is not ast::ExpressionStatement")
        }
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world";"#;

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);

        if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let Expression::StringLiteral(literal) = &stmt.expression {
                assert_eq!(literal.value, "hello world")
            } else {
                assert!(false, "exp not ast::StringLiteral")
            }
        }
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);

        if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let Expression::ArrayLiteral(array) = &stmt.expression {
                assert_eq!(array.elements.len(), 3);
                assert_integer_literal(&array.elements[0], 1);
                assert_infix_expression(&array.elements[1], 2, "*", 2);
                assert_infix_expression(&array.elements[2], 3, "+", 3);
            } else {
                assert!(false, "exp not ast::ArrayLiteral")
            }
        }
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1];";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);

        if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let Expression::IndexExpression(index_exp) = &stmt.expression {
                assert_identifier(&index_exp.left, "myArray");
                assert_infix_expression(&index_exp.index, 1, "+", 1);
            } else {
                assert!(false, "exp not ast::IndexExpression")
            }
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);

        if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let Expression::HashLiteral(hash) = &stmt.expression {
                assert_eq!(hash.pairs.len(), 0);
            } else {
                assert!(false, "exp is not ast::HashLiteral")
            }
        }
    }

    #[test]
    fn test_parsing_hash_literal_string_keys() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);

        let mut expected = HashMap::new();
        expected.insert("one".to_string(), 1);
        expected.insert("two".to_string(), 2);
        expected.insert("three".to_string(), 3);

        if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let Expression::HashLiteral(hash) = &stmt.expression {
                assert_eq!(hash.pairs.len(), 3);
                for (key, value) in &hash.pairs {
                    if let Expression::StringLiteral(literal) = key {
                        let expected_value = expected.get(&literal.value).unwrap();
                        assert_integer_literal(value, *expected_value as i64);
                    } else {
                        assert!(false, "key is not ast::StringLiteral")
                    }
                }
            } else {
                assert!(false, "exp is not ast::HashLiteral")
            }
        }
    }

    #[test]
    fn test_parsing_hash_literal_boolean_keys() {
        let input = "{true: 1, false: 2}";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);

        let mut expected = HashMap::new();
        expected.insert(true, 1);
        expected.insert(false, 2);

        if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let Expression::HashLiteral(hash) = &stmt.expression {
                assert_eq!(hash.pairs.len(), 2);
                for (key, value) in &hash.pairs {
                    if let Expression::Boolean(literal) = key {
                        let expected_value = expected.get(&literal.value).unwrap();
                        assert_integer_literal(value, *expected_value as i64);
                    } else {
                        assert!(false, "key is not ast::Boolean")
                    }
                }
            } else {
                assert!(false, "exp is not ast::HashLiteral")
            }
        }
    }

    #[test]
    fn test_parsing_hash_literal_integer_keys() {
        let input = "{1: 1, 2: 2, 3: 3}";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);

        let mut expected = HashMap::new();
        expected.insert(1, 1);
        expected.insert(2, 2);
        expected.insert(3, 3);

        if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let Expression::HashLiteral(hash) = &stmt.expression {
                assert_eq!(hash.pairs.len(), 3);
                for (key, value) in &hash.pairs {
                    if let Expression::IntegerLiteral(literal) = key {
                        let expected_value = expected.get(&literal.value).unwrap();
                        assert_integer_literal(value, *expected_value as i64);
                    } else {
                        assert!(false, "key is not ast::Integer")
                    }
                }
            } else {
                assert!(false, "exp is not ast::HashLiteral")
            }
        }
    }

    #[test]
    fn test_parsing_hash_literal_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);

        let mut expected = HashMap::new();
        expected.insert("one".to_string(), (0, "+", 1));
        expected.insert("two".to_string(), (10, "-", 8));
        expected.insert("three".to_string(), (15, "/", 5));

        if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let Expression::HashLiteral(hash) = &stmt.expression {
                assert_eq!(hash.pairs.len(), 3);
                for (key, value) in &hash.pairs {
                    if let Expression::StringLiteral(literal) = key {
                        let (left, operator, right) = expected.get(&literal.value).unwrap();
                        assert_infix_expression(value, *left as i64, operator, *right as i64)
                    } else {
                        assert!(false, "key is not ast::StringLiteral")
                    }
                }
            } else {
                assert!(false, "exp is not ast::HashLiteral")
            }
        }
    }

    #[test]
    fn test_macro_literal_parsing() {
        let input = "macro(x, y) { x + y; }";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parse_errors(p);
        assert_eq!(program.statements.len(), 1);

        if let Statement::ExpressionStatement(stmt) = &program.statements[0] {
            if let Expression::MacroLiteral(macro_literal) = &stmt.expression {
                assert_eq!(macro_literal.parameters.len(), 2);
                assert_eq!(macro_literal.parameters[0].value, "x".to_string());
                assert_eq!(format!("{}", macro_literal.parameters[0].value), "x");
                assert_eq!(macro_literal.parameters[1].value, "y".to_string());
                assert_eq!(format!("{}", macro_literal.parameters[1].value), "y");
                assert_eq!(macro_literal.body.statements.len(), 1);
                if let Statement::ExpressionStatement(body_stmt) = &macro_literal.body.statements[0]
                {
                    assert_infix_expression(
                        &body_stmt.expression,
                        "x".to_string(),
                        "+",
                        "y".to_string(),
                    )
                } else {
                    assert!(false, "macro body stmt is not ast::ExpressionStatement")
                }
            } else {
                assert!(false, "stmt.expression is not ast::MacroLiteral")
            }
        } else {
            assert!(
                false,
                "program.statements[0] is not ast::ExpressionStatement"
            )
        }
    }

    fn check_parse_errors(p: Parser) {
        let errors = p.errors;
        let len = errors.len();

        if len == 0 {
            return;
        }
        for e in errors {
            eprintln!("parser error: {}", e);
        }
        assert!(false, "parser has {} errors.", len);
    }

    fn assert_let_statement(statement: &Statement, expected: &str) {
        if let Statement::LetStatement(stmt) = statement {
            assert_eq!(stmt.name.value, expected)
        } else {
            assert!(false, "statement not ast::LetStatement")
        }
    }

    fn assert_integer_literal(il: &Expression, value: i64) {
        if let Expression::IntegerLiteral(integ) = il {
            assert_eq!(integ.value, value);
            assert_eq!(format!("{}", integ.value), format!("{}", value));
        } else {
            assert!(false, "il not ast::IntegerLiteral");
        }
    }

    fn assert_identifier(exp: &Expression, value: &str) {
        if let Expression::Identifier(ident) = exp {
            assert_eq!(&ident.value, value);
            assert_eq!(format!("{}", ident.value), format!("{}", value));
        } else {
            assert!(false, "exp not ast::Identifier");
        }
    }

    fn assert_boolean_literal(exp: &Expression, value: bool) {
        if let Expression::Boolean(boolean) = exp {
            assert_eq!(boolean.value, value);
            assert_eq!(format!("{}", boolean.value), format!("{}", value));
        } else {
            assert!(false, "exp not ast::Boolean");
        }
    }

    trait AssertWithExpression {
        fn assert_with(self, exp: &Expression);
    }

    impl AssertWithExpression for i64 {
        fn assert_with(self, exp: &Expression) {
            assert_integer_literal(exp, self);
        }
    }

    impl AssertWithExpression for String {
        fn assert_with(self, exp: &Expression) {
            assert_identifier(exp, &self);
        }
    }

    impl AssertWithExpression for bool {
        fn assert_with(self, exp: &Expression) {
            assert_boolean_literal(exp, self);
        }
    }

    fn assert_literal_expression<T: AssertWithExpression>(exp: &Expression, expected: T) {
        expected.assert_with(exp);
    }

    fn assert_infix_expression<T: AssertWithExpression, U: AssertWithExpression>(
        exp: &Expression,
        left: T,
        operator: &str,
        right: U,
    ) {
        if let Expression::InfixExpression(op_exp) = exp {
            assert_literal_expression(&op_exp.left, left);
            assert_eq!(&op_exp.operator, operator);
            assert_literal_expression(&op_exp.right, right);
        } else {
            assert!(false, "exp not ast::InfixExpression");
        }
    }
}
