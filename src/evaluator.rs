use super::ast::*;
use super::object::*;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

pub trait Eval {
    fn eval(&self) -> Object;
}

macro_rules! impl_eval {
    ($ty:ty, $self:ident, $block:block) => {
        impl Eval for $ty {
            fn eval(&$self) -> Object {
                $block
            }
        }
    };
}

impl_eval!(Program, self, {
    let mut result = NULL;
    for statement in &self.statements {
        result = statement.eval();

        match result {
            Object::ReturnValue(return_value) => return *return_value,
            Object::Error(_) => return result,
            _ => (),
        }
    }
    result
});

impl_eval!(Vec<Statement>, self, {
    let mut result = NULL;
    for stmt in self {
        result = stmt.eval();

        if let Object::ReturnValue(return_value) = result {
            return *return_value;
        }
    }
    result
});

impl_eval!(Statement, self, {
    match self {
        Statement::ExpressionStatement(stmt) => stmt.expression.eval(),
        Statement::ReturnStatement(stmt) => {
            let val = stmt.return_value.eval();
            if is_error(&val) {
                return val;
            }
            Object::ReturnValue(Box::new(val))
        }
        _ => panic!(),
    }
});

impl_eval!(Expression, self, {
    match self {
        Expression::IntegerLiteral(exp) => exp.eval(),
        Expression::Boolean(exp) => exp.eval(),
        Expression::PrefixExpression(exp) => exp.eval(),
        Expression::InfixExpression(exp) => exp.eval(),
        Expression::IfExpression(exp) => exp.eval(),
        _ => panic!(),
    }
});

impl_eval!(IntegerLiteral, self, { Object::Integer(self.value) });

impl_eval!(Boolean, self, { native_bool_to_boolean_object(self.value) });

fn native_bool_to_boolean_object(input: bool) -> Object {
    if input {
        TRUE
    } else {
        FALSE
    }
}

impl_eval!(PrefixExpression, self, {
    let right = self.right.eval();
    if is_error(&right) {
        return right;
    }
    eval_prefix_expression(&self.operator, right)
});

fn eval_prefix_expression(operator: &String, right: Object) -> Object {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => new_error(format!("unknown operator: {}{:?}", operator, right)),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    if let Object::Integer(value) = right {
        Object::Integer(-value)
    } else {
        new_error(format!("unknown operator: -{:?}", right))
    }
}

impl_eval!(InfixExpression, self, {
    let left = self.left.eval();
    if is_error(&left) {
        return left;
    }
    let right = self.right.eval();
    if is_error(&right) {
        return right;
    }
    eval_infix_expression(&self.operator, left, right)
});

fn eval_infix_expression(operator: &String, left: Object, right: Object) -> Object {
    if operator.as_str() == "==" {
        return native_bool_to_boolean_object(left == right);
    }
    if operator.as_str() == "!=" {
        return native_bool_to_boolean_object(left != right);
    }
    if let Object::Integer(l) = left {
        if let Object::Integer(r) = right {
            return eval_integer_infix_expression(operator, l, r);
        }
        return new_error(format!(
            "type mismatch: {:?} {} {:?}",
            left, operator, right
        ));
    }
    return new_error(format!(
        "unknown operator: {:?} {} {:?}",
        left, operator, right
    ));
}

fn eval_integer_infix_expression(operator: &String, left_val: i64, right_val: i64) -> Object {
    match operator.as_str() {
        "+" => Object::Integer(left_val + right_val),
        "-" => Object::Integer(left_val - right_val),
        "*" => Object::Integer(left_val * right_val),
        "/" => Object::Integer(left_val / right_val),
        "<" => native_bool_to_boolean_object(left_val < right_val),
        ">" => native_bool_to_boolean_object(left_val > right_val),
        "==" => native_bool_to_boolean_object(left_val == right_val),
        "!=" => native_bool_to_boolean_object(left_val != right_val),
        _ => new_error(format!(
            "unknown operator: {:?} {} {:?}",
            left_val, operator, right_val
        )),
    }
}

impl_eval!(IfExpression, self, {
    let condition = self.condition.eval();
    if is_error(&condition) {
        return condition;
    }

    return if is_truthy(condition) {
        self.consequence.eval()
    } else if let Some(alternative) = &self.alternative {
        alternative.eval()
    } else {
        NULL
    };
});

impl_eval!(BlockStatement, self, {
    let mut result = NULL;
    for statement in &self.statements {
        result = statement.eval();

        match result {
            Object::ReturnValue(_) => return result,
            Object::Error(_) => return result,
            _ => (),
        }
    }
    result
});

fn is_truthy(obj: Object) -> bool {
    match obj {
        NULL => false,
        TRUE => true,
        FALSE => false,
        _ => true,
    }
}

fn new_error(message: String) -> Object {
    Object::Error(message)
}

fn is_error(obj: &Object) -> bool {
    if let Object::Error(_) = obj {
        true
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::super::lexer::Lexer;
    use super::super::object::*;
    use super::super::parser::Parser;
    use super::{Eval, NULL};

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for tt in tests {
            let input = tt.0.to_string();
            let expected = tt.1;
            let evaluated = test_eval(&input);
            assert_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
        ];

        for tt in tests {
            let input = tt.0.to_string();
            let expected = tt.1;
            let evaluated = test_eval(&input);
            assert_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for tt in tests {
            let input = tt.0.to_string();
            let expected = tt.1;
            let evaluated = test_eval(&input);
            assert_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            ("if (true) { 10 }", 10),
            ("if (1) { 10 }", 10),
            ("if (1 < 2) { 10 }", 10),
            ("if (1 > 2) { 10 } else { 20 }", 20),
            ("if (1 < 2) { 10 } else { 20 }", 10),
        ];

        for tt in tests {
            let input = tt.0.to_string();
            let expected = tt.1;
            let evaluated = test_eval(&input);
            assert_integer_object(evaluated, expected);
        }

        let nil_tests = vec!["if (false) { 10 }", "if (1 > 2) { 10 }"];

        for input in nil_tests {
            let evaluated = test_eval(&input.to_string());
            assert_null_object(evaluated);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("(; return 2 * 5; 9;", 10),
            (
                r#"
            if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            }
            "#,
                10,
            ),
        ];

        for tt in tests {
            let input = tt.0.to_string();
            let expected = tt.1;
            let evaluated = test_eval(&input);
            assert_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: Integer(5) + Boolean(true)"),
            ("5 + true; 5;", "type mismatch: Integer(5) + Boolean(true)"),
            ("-true", "unknown operator: -Boolean(true)"),
            (
                "true + false;",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
            (
                "5; true + false; 5",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
            (
                r#"
            if (10 > 1) {
                if (10 > 1) {
                    return true + false;
                }
            }
            "#,
                "unknown operator: Boolean(true) + Boolean(false)",
            ),
        ];

        for tt in tests {
            let input = tt.0.to_string();
            let expected = tt.1;
            let evaluated = test_eval(&input);

            if let Object::Error(message) = evaluated {
                assert_eq!(message, expected)
            } else {
                assert!(false, "no error object returned.")
            }
        }
    }

    fn test_eval(input: &String) -> Object {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        program.eval()
    }

    fn assert_integer_object(obj: Object, expected: i64) {
        if let Object::Integer(result) = obj {
            assert_eq!(result, expected)
        } else {
            assert!(false, "object is not Integer")
        }
    }

    fn assert_boolean_object(obj: Object, expected: bool) {
        if let Object::Boolean(result) = obj {
            assert_eq!(result, expected)
        } else {
            assert!(false, "object is not Boolean")
        }
    }

    fn assert_null_object(obj: Object) {
        assert_eq!(obj, NULL);
    }
}
