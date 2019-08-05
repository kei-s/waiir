use super::ast::*;
use super::object::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
const NULL: Object = Object::Null;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Environment>> {
        let store = HashMap::new();
        let env = Environment { store, outer: None };
        Rc::new(RefCell::new(env))
    }

    pub fn new_enclosed(outer: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        let store = HashMap::new();
        let env = Environment {
            store,
            outer: Some(outer),
        };
        Rc::new(RefCell::new(env))
    }

    fn get(&self, name: &String) -> Option<Object> {
        if let Some(o) = self.store.get(name) {
            Some(o.clone())
        } else {
            self.outer
                .as_ref()
                .and_then(|outer| outer.borrow().get(name))
        }
    }

    fn set(&mut self, name: &String, val: &Object) {
        self.store.insert(name.clone(), val.clone());
    }
}

pub trait Eval {
    fn eval(&self, env: &mut Rc<RefCell<Environment>>) -> Object;
}

macro_rules! impl_eval {
    ($ty:ty => ($self:ident, $env:ident) $block:block) => {
        impl Eval for $ty {
            fn eval(&$self, $env: &mut Rc<RefCell<Environment>>) -> Object {
                $block
            }
        }
    };
}

impl_eval!(Program => (self, env) {
    let mut result = NULL;
    for statement in &self.statements {
        result = statement.eval(env);

        match result {
            Object::ReturnValue(return_value) => return *return_value,
            Object::Error(_) => return result,
            _ => (),
        }
    }
    result
});

impl_eval!(Vec<Statement> => (self, env) {
    let mut result = NULL;
    for stmt in self {
        result = stmt.eval(env);

        if let Object::ReturnValue(return_value) = result {
            return *return_value;
        }
    }
    result
});

impl_eval!(Statement => (self, env) {
    match self {
        Statement::ExpressionStatement(stmt) => stmt.expression.eval(env),
        Statement::ReturnStatement(stmt) => {
            let val = stmt.return_value.eval(env);
            if is_error(&val) {
                return val;
            }
            Object::ReturnValue(Box::new(val))
        }
        Statement::LetStatement(stmt) => {
            let val = stmt.value.eval(env);
            if is_error(&val) {
                return val;
            }
            env.borrow_mut().set(&stmt.name.value, &val);
            val
        }
    }
});

impl_eval!(Expression => (self, env) {
    match self {
        Expression::IntegerLiteral(exp) => exp.eval(env),
        Expression::Boolean(exp) => exp.eval(env),
        Expression::PrefixExpression(exp) => exp.eval(env),
        Expression::InfixExpression(exp) => exp.eval(env),
        Expression::IfExpression(exp) => exp.eval(env),
        Expression::Identifier(exp) => exp.eval(env),
        Expression::FunctionLiteral(exp) => exp.eval(env),
        Expression::CallExpression(exp) => exp.eval(env),
    }
});

impl_eval!(IntegerLiteral => (self, _env) { Object::Integer(self.value) });

impl_eval!(Boolean => (self, _env) {
    native_bool_to_boolean_object(self.value)
});

fn native_bool_to_boolean_object(input: bool) -> Object {
    if input {
        TRUE
    } else {
        FALSE
    }
}

impl_eval!(PrefixExpression => (self, env) {
    let right = self.right.eval(env);
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

impl_eval!(InfixExpression => (self, env) {
    let left = self.left.eval(env);
    if is_error(&left) {
        return left;
    }
    let right = self.right.eval(env);
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

impl_eval!(IfExpression => (self, env) {
    let condition = self.condition.eval(env);
    if is_error(&condition) {
        return condition;
    }

    return if is_truthy(condition) {
        self.consequence.eval(env)
    } else if let Some(alternative) = &self.alternative {
        alternative.eval(env)
    } else {
        NULL
    };
});

impl_eval!(BlockStatement => (self, env) {
    let mut result = NULL;
    for statement in &self.statements {
        result = statement.eval(env);

        match result {
            Object::ReturnValue(_) => return result,
            Object::Error(_) => return result,
            _ => (),
        }
    }
    result
});

impl_eval!(Identifier => (self, env) {
    if let Some(val) = env.borrow().get(&self.value) {
        return val;
    }
    new_error(format!("identifier not found: {}", self.value))
});

impl_eval!(FunctionLiteral => (self, env) {
    Object::Function(Function {
        parameters: self.parameters.clone(),
        body: *self.body.clone(),
        env: Rc::clone(&env),
    })
});

impl_eval!(CallExpression => (self, env) {
    let function = self.function.eval(env);
    if is_error(&function) {
        return function;
    }
    let mut args: Vec<Object> = vec![];
    for argument in &self.arguments {
        let evaluated = argument.eval(env);
        if is_error(&evaluated) {
            return evaluated;
        }
        args.push(evaluated);
    }
    apply_function(function, args)
});

fn apply_function(func: Object, args: Vec<Object>) -> Object {
    if let Object::Function(function) = func {
        let mut extended_env = Environment::new_enclosed(function.env);
        for (i, param) in function.parameters.iter().enumerate() {
            extended_env.borrow_mut().set(&param.value, &args[i]);
        }
        let evaluated = function.body.eval(&mut extended_env);
        unwrap_return_value(evaluated)
    } else {
        new_error(format!("not a function: {:?}", func))
    }
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::ReturnValue(return_value) = obj {
        return *return_value;
    }
    obj
}

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
    use super::{Environment, Eval, NULL};

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
            (
                r#"
                let f = fn(x) {
                    return x;
                    x + 10;
                };
                f(10);
                "#,
                10,
            ),
            (
                r#"
                let f = fn(x) {
                    let result = x + 10;
                    return result;
                    return 10;
                };
                f(10);
                "#,
                20,
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
            ("foobar", "identifier not found: foobar"),
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

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for tt in tests {
            let input = tt.0.to_string();
            let expected = tt.1;
            assert_integer_object(test_eval(&input), expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; }".to_string();

        let evaluated = test_eval(&input);
        if let Object::Function(func) = &evaluated {
            assert_eq!(func.parameters.len(), 1);
            assert_eq!(format!("{}", func.parameters[0]), "x");
            assert_eq!(format!("{}", func.body), "(x + 2)");
        } else {
            assert!(false, "object is not Function.")
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for tt in tests {
            assert_integer_object(test_eval(&tt.0.to_string()), tt.1);
        }
    }

    #[test]
    fn test_closures() {
        let input = r#"
            let newAdder = fn(x) {
                fn(y) { x + y };
            };
            let addTwo = newAdder(2);
            addTwo(2);
        "#
        .to_string();
        assert_integer_object(test_eval(&input), 4);
    }

    #[test]
    fn test_recursive() {
        let input = r#"
            let factorial = fn(n) {
                if (n == 0) {
                    1;
                } else {
                    n * factorial(n - 1);
                }
            };
            factorial(5);
        "#
        .to_string();

        let evaluated = test_eval(&input);
        if let Object::Error(message) = evaluated {
            assert!(false, message);
        } else {
            assert_integer_object(test_eval(&input), 120);
        }
    }

    fn test_eval(input: &String) -> Object {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let mut env = Environment::new();
        program.eval(&mut env)
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
