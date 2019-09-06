use super::ast::{
    modify, ArrayLiteral, BlockStatement, Boolean, CallExpression, Expression, FunctionLiteral,
    HashLiteral, Identifier, IfExpression, IndexExpression, InfixExpression, IntegerLiteral, Node,
    PrefixExpression, Program, Statement, StringLiteral,
};
use super::object::hash::hash_key_of;
use super::object::{Array, Builtin, Function, Hash, HashPair, Macro, Object, Quote};
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

    fn get(&self, name: &str) -> Option<Object> {
        if let Some(o) = self.store.get(name) {
            Some(o.clone())
        } else {
            self.outer
                .as_ref()
                .and_then(|outer| outer.borrow().get(name))
        }
    }

    fn set(&mut self, name: &str, val: &Object) {
        self.store.insert(name.to_string(), val.clone());
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
        Expression::StringLiteral(exp) => exp.eval(env),
        Expression::ArrayLiteral(exp) => exp.eval(env),
        Expression::IndexExpression(exp) => exp.eval(env),
        Expression::HashLiteral(exp) => exp.eval(env),
        _ => unimplemented!()
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

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
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

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    if operator == "==" {
        return native_bool_to_boolean_object(left == right);
    }
    if operator == "!=" {
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
    if let Object::String(l) = &left {
        if let Object::String(r) = &right {
            return eval_string_infix_expression(operator, l, r);
        }
        return new_error(format!(
            "type mismatch: {:?} {} {:?}",
            left, operator, right
        ));
    }
    new_error(format!(
        "unknown operator: {:?} {} {:?}",
        left, operator, right
    ))
}

fn eval_integer_infix_expression(operator: &str, left_val: i64, right_val: i64) -> Object {
    match operator {
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

    if is_truthy(condition) {
        self.consequence.eval(env)
    } else if let Some(alternative) = &self.alternative {
        alternative.eval(env)
    } else {
        NULL
    }
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
    match self.value.as_str() {
        "len" => Object::Builtin(Builtin{func: builtin::len}),
        "first" => Object::Builtin(Builtin{func: builtin::first}),
        "last" => Object::Builtin(Builtin{func: builtin::last}),
        "rest" => Object::Builtin(Builtin{func: builtin::rest}),
        "push" => Object::Builtin(Builtin{func: builtin::push}),
        "puts" => Object::Builtin(Builtin{func: builtin::puts}),
        _ => new_error(format!("identifier not found: {}", self.value))
    }
});

impl_eval!(FunctionLiteral => (self, env) {
    Object::Function(Function {
        parameters: self.parameters.clone(),
        body: *self.body.clone(),
        env: Rc::clone(&env),
    })
});

impl_eval!(CallExpression => (self, env) {
    if let Expression::Identifier(identifier) = &*self.function {
        if identifier.value == "quote" {
            return quote(&self.arguments[0], env)
        }
    }
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
    match func {
        Object::Function(function) => {
            let mut extended_env = Environment::new_enclosed(function.env);
            for (i, param) in function.parameters.iter().enumerate() {
                extended_env.borrow_mut().set(&param.value, &args[i]);
            }
            let evaluated = function.body.eval(&mut extended_env);
            unwrap_return_value(evaluated)
        }
        Object::Builtin(builtin) => {
            let builtin_function = builtin.func;
            builtin_function(args)
        }
        _ => new_error(format!("not a function: {:?}", func)),
    }
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::ReturnValue(return_value) = obj {
        return *return_value;
    }
    obj
}

impl_eval!(StringLiteral => (self, _env) {
    Object::String(self.value.clone())
});

fn eval_string_infix_expression(operator: &str, left: &str, right: &str) -> Object {
    if operator != "+" {
        return new_error(format!(
            "unknown operator: {:?} {} {:?}",
            left, operator, right
        ));
    }
    Object::String([left, right].join(""))
}

impl_eval!(ArrayLiteral => (self, env) {
    let mut elements: Vec<Object> = vec![];
    for element in &self.elements {
        let evaluated = element.eval(env);
        if is_error(&evaluated) {
            return evaluated;
        }
        elements.push(evaluated);
    }
    Object::Array(Array{ elements })
});

impl_eval!(IndexExpression => (self, env) {
    let left = self.left.eval(env);
    if is_error(&left) {
        return left
    }
    let index = self.index.eval(env);
    if is_error(&index) {
        return index
    }
    eval_index_expression(left, index)
});

fn eval_index_expression(left: Object, index: Object) -> Object {
    if let Object::Array(array) = &left {
        if let Object::Integer(idx) = index {
            return eval_array_index_expression(&array.elements, idx);
        }
    }
    if let Object::Hash(hash) = &left {
        return eval_hash_index_expression(hash, index);
    }
    new_error(format!("index operator not supported: {:?}", left))
}

fn eval_array_index_expression(elements: &[Object], idx: i64) -> Object {
    let max = elements.len() - 1;

    if idx < 0 || idx as usize > max {
        return NULL;
    }
    elements[idx as usize].clone()
}

impl_eval!(HashLiteral => (self, env) {
    let mut pairs = HashMap::new();

    for (key_node, value_node) in &self.pairs {
        let key = key_node.eval(env);
        if is_error(&key) {
            return key;
        }

        let hashed = match hash_key_of(&key) {
            Ok(k) => k,
            Err(message) => return new_error(message),
        };

        let value = value_node.eval(env);

        pairs.insert(hashed, HashPair{key, value});
    }

    Object::Hash(Hash{pairs})
});

fn eval_hash_index_expression(hash_object: &Hash, index: Object) -> Object {
    let key = match hash_key_of(&index) {
        Ok(k) => k,
        Err(message) => return new_error(message),
    };

    if let Some(pair) = hash_object.pairs.get(&key) {
        pair.value.clone()
    } else {
        NULL
    }
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

fn quote(node: &Expression, env: &mut Rc<RefCell<Environment>>) -> Object {
    let n = eval_unquote_calls(node, env);
    Object::Quote(Quote { node: n })
}

fn eval_unquote_calls(quoted: &Expression, env: &mut Rc<RefCell<Environment>>) -> Expression {
    if let Node::Expression(exp) = modify(
        Node::Expression(quoted.clone()),
        Rc::new(RefCell::new(|node: Node| -> Node {
            if !is_unquote_call(&node) {
                return node;
            }

            if let Node::Expression(Expression::CallExpression(call)) = &node {
                if call.arguments.len() != 1 {
                    return node;
                }
                let unquoted = call.arguments[0].eval(env);
                Node::Expression(convert_object_to_ast_node(unquoted))
            } else {
                node
            }
        })),
    ) {
        exp
    } else {
        unreachable!()
    }
}

fn convert_object_to_ast_node(obj: Object) -> Expression {
    match obj {
        Object::Integer(int) => Expression::IntegerLiteral(IntegerLiteral { value: int }),
        Object::Boolean(boolean) => Expression::Boolean(Boolean { value: boolean }),
        Object::Quote(quote) => quote.node,
        _ => unimplemented!(),
    }
}

fn is_unquote_call(node: &Node) -> bool {
    if let Node::Expression(Expression::CallExpression(call_expression)) = node {
        if let Expression::Identifier(identifier) = &*call_expression.function {
            return identifier.value == "unquote";
        }
        false
    } else {
        false
    }
}

pub fn define_macros(program: &mut Program, env: Rc<RefCell<Environment>>) {
    let mut definition = vec![];
    for (i, statement) in program.statements.iter().enumerate() {
        if is_macro_definition(statement) {
            add_macro(statement, Rc::clone(&env));
            definition.push(i);
        }
    }

    definition.reverse();
    for definition_index in definition.iter() {
        program.statements.remove(*definition_index);
    }
}

fn is_macro_definition(node: &Statement) -> bool {
    if let Statement::LetStatement(let_statement) = node {
        if let Expression::MacroLiteral(_) = let_statement.value {
            true
        } else {
            false
        }
    } else {
        false
    }
}

fn add_macro(stmt: &Statement, env: Rc<RefCell<Environment>>) {
    if let Statement::LetStatement(let_statement) = stmt {
        if let Expression::MacroLiteral(macro_literal) = &let_statement.value {
            let macro_o = Object::Macro(Macro {
                parameters: macro_literal.parameters.clone(),
                env: Rc::clone(&env),
                body: *macro_literal.body.clone(),
            });
            env.borrow_mut().set(&let_statement.name.value, &macro_o);
        } else {
            unreachable!()
        }
    } else {
        unreachable!()
    }
}

pub fn expand_macros(program: Program, env: &mut Rc<RefCell<Environment>>) -> Program {
    if let Node::Program(p) = modify(
        Node::Program(program),
        Rc::new(RefCell::new(|node: Node| -> Node {
            if let Node::Expression(Expression::CallExpression(call_expression)) = &node {
                if let Some(macro_o) = is_macro_call(call_expression, env) {
                    let args = quote_args(call_expression);
                    let mut eval_env = Environment::new_enclosed(macro_o.env);
                    for (i, param) in macro_o.parameters.iter().enumerate() {
                        eval_env.borrow_mut().set(&param.value, &args[i]);
                    }
                    let evaluated = macro_o.body.eval(&mut eval_env);
                    if let Object::Quote(quote) = evaluated {
                        Node::Expression(quote.node)
                    } else {
                        panic!("we only support returning AST-nodes from macros");
                    }
                } else {
                    node
                }
            } else {
                node
            }
        })),
    ) {
        p
    } else {
        unreachable!()
    }
}

fn is_macro_call(exp: &CallExpression, env: &Rc<RefCell<Environment>>) -> Option<Macro> {
    if let Expression::Identifier(identifier) = &*exp.function {
        if let Some(obj) = env.borrow().get(&identifier.value) {
            if let Object::Macro(macro_o) = obj {
                Some(macro_o)
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn quote_args(exp: &CallExpression) -> Vec<Object> {
    let mut args = vec![];

    for a in &exp.arguments {
        args.push(Object::Quote(Quote { node: a.clone() }));
    }
    args
}

mod builtin {
    use super::new_error;
    use super::{Array, Object, NULL};

    pub fn len(args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return new_error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            ));
        }
        match &args[0] {
            Object::String(string) => Object::Integer(string.len() as i64),
            Object::Array(array) => Object::Integer(array.elements.len() as i64),
            _ => new_error(format!(
                "argument to `len` not supported, got {:?}",
                args[0]
            )),
        }
    }

    pub fn first(args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return new_error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            ));
        }
        if let Object::Array(array) = &args[0] {
            if !array.elements.is_empty() {
                array.elements.first().unwrap().clone()
            } else {
                NULL
            }
        } else {
            new_error(format!(
                "argument to `first` must be ARRAY, got {:?}",
                args[0]
            ))
        }
    }

    pub fn last(args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return new_error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            ));
        }
        if let Object::Array(array) = &args[0] {
            if !array.elements.is_empty() {
                array.elements.last().unwrap().clone()
            } else {
                NULL
            }
        } else {
            new_error(format!(
                "argument to `last` must be ARRAY, got {:?}",
                args[0]
            ))
        }
    }

    pub fn rest(args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return new_error(format!(
                "wrong number of arguments. got={}, want=1",
                args.len()
            ));
        }
        if let Object::Array(array) = &args[0] {
            if !array.elements.is_empty() {
                Object::Array(Array {
                    elements: array.elements[1..].to_vec(),
                })
            } else {
                NULL
            }
        } else {
            new_error(format!(
                "argument to `rest` must be ARRAY, got {:?}",
                args[0]
            ))
        }
    }

    pub fn push(args: Vec<Object>) -> Object {
        if args.len() != 2 {
            return new_error(format!(
                "wrong number of arguments. got={}, want=2",
                args.len()
            ));
        }
        if let Object::Array(array) = &args[0] {
            let mut elements = array.elements[..].to_vec();
            elements.push(args[1].clone());
            Object::Array(Array { elements })
        } else {
            new_error(format!(
                "argument to `push` must be ARRAY, got {:?}",
                args[0]
            ))
        }
    }

    pub fn puts(args: Vec<Object>) -> Object {
        for arg in args {
            println!("{}", arg);
        }
        NULL
    }
}

#[cfg(test)]
mod tests {
    use super::super::ast::Program;
    use super::super::lexer::Lexer;
    use super::super::object::hash::Hashable;
    use super::super::object::Object;
    use super::super::parser::Parser;
    use super::{define_macros, expand_macros, Environment, Eval, NULL};
    use std::collections::HashMap;
    use std::rc::Rc;

    #[test]
    fn test_eval_integer_expression() {
        let tests = [
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

        for (input, expected) in tests.iter() {
            let evaluated = test_eval(input);
            assert_integer_object(&evaluated, *expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = [
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

        for (input, expected) in tests.iter() {
            let evaluated = test_eval(input);
            assert_boolean_object(evaluated, *expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = [
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

        for (input, expected) in tests.iter() {
            let evaluated = test_eval(input);
            assert_boolean_object(evaluated, *expected);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = [
            ("if (true) { 10 }", 10),
            ("if (1) { 10 }", 10),
            ("if (1 < 2) { 10 }", 10),
            ("if (1 > 2) { 10 } else { 20 }", 20),
            ("if (1 < 2) { 10 } else { 20 }", 10),
        ];

        for (input, expected) in tests.iter() {
            let evaluated = test_eval(input);
            assert_integer_object(&evaluated, *expected);
        }

        let nil_tests = ["if (false) { 10 }", "if (1 > 2) { 10 }"];

        for input in nil_tests.iter() {
            let evaluated = test_eval(input);
            assert_null_object(evaluated);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = [
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

        for (input, expected) in tests.iter() {
            let evaluated = test_eval(input);
            assert_integer_object(&evaluated, *expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = [
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
            (
                r#""Hello" - "World""#,
                r#"unknown operator: "Hello" - "World""#,
            ),
            (
                r#"{"name": "Monkey"}[fn(x) { x }];"#,
                "unusable as hash key: fn(x) {x}",
            ),
        ];

        for (input, expected) in tests.iter() {
            let evaluated = test_eval(input);

            if let Object::Error(message) = evaluated {
                assert_eq!(message, *expected)
            } else {
                assert!(false, "no error object returned.")
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests.iter() {
            assert_integer_object(&test_eval(input), *expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; }";

        let evaluated = test_eval(input);
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
        let tests = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in tests.iter() {
            assert_integer_object(&test_eval(input), *expected);
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
        "#;
        assert_integer_object(&test_eval(input), 4);
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
        "#;

        let evaluated = test_eval(input);
        if let Object::Error(message) = evaluated {
            assert!(false, message);
        } else {
            assert_integer_object(&test_eval(input), 120);
        }
    }

    #[test]
    fn test_string_literal() {
        let input = r#""Hello World!""#;
        let evaluated = test_eval(input);
        if let Object::String(string) = evaluated {
            assert_eq!(string, "Hello World!")
        } else {
            assert!(false, "object is not String")
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#""Hello" + " " + "World!""#;
        let evaluated = test_eval(input);
        if let Object::String(string) = &evaluated {
            assert_eq!(string, "Hello World!")
        } else {
            assert!(false, "object is not String")
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tests = [
            (r#"len("")"#, 0),
            (r#"len("four")"#, 4),
            (r#"len("hello world")"#, 11),
            ("len([1, 2, 3])", 3),
            ("len([])", 0),
            ("first([1, 2, 3])", 1),
            ("last([1, 2, 3])", 3),
        ];

        for (input, expected) in tests.iter() {
            let evaluated = test_eval(input);
            assert_integer_object(&evaluated, *expected);
        }

        let null_tests = ["first([])", "last([])", "rest([])"];

        for tt in null_tests.iter() {
            assert_null_object(test_eval(tt));
        }

        let array_tests = [("rest([1, 2, 3])", vec![2, 3]), ("push([], 1)", vec![1])];

        for (input, expected) in array_tests.iter() {
            let evaluated = test_eval(input);
            if let Object::Array(array) = &evaluated {
                assert_eq!(array.elements.len(), expected.len());
                for (r, i) in array.elements.iter().zip(expected.iter()) {
                    assert_integer_object(r, *i as i64);
                }
            } else {
                assert!(false, "obj not Array")
            }
        }

        let error_tests = [
            ("len(1)", "argument to `len` not supported, got Integer(1)"),
            (
                r#"len("one", "two")"#,
                "wrong number of arguments. got=2, want=1",
            ),
            (
                "first(1)",
                "argument to `first` must be ARRAY, got Integer(1)",
            ),
            (
                "last(1)",
                "argument to `last` must be ARRAY, got Integer(1)",
            ),
            (
                "push(1, 1)",
                "argument to `push` must be ARRAY, got Integer(1)",
            ),
        ];

        for (input, expected) in error_tests.iter() {
            let evaluated = test_eval(input);
            if let Object::Error(message) = evaluated {
                assert_eq!(message, *expected)
            } else {
                assert!(false, "object is not Error")
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";

        let evaluated = test_eval(input);
        if let Object::Array(result) = evaluated {
            assert_eq!(result.elements.len(), 3);
            for (r, i) in result.elements.iter().zip([1, 4, 6].iter()) {
                assert_integer_object(r, *i as i64);
            }
        } else {
            assert!(false, "object is not array")
        }
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = [
            ("[1, 2, 3][0]", 1),
            ("[1, 2, 3][1]", 2),
            ("[1, 2, 3][2]", 3),
            ("let i = 0; [1][i]", 1),
            ("[1, 2, 3][1 + 1]", 3),
            ("let myArray = [1, 2, 3]; myArray[2]", 3),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]",
                6,
            ),
            ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", 2),
        ];

        for (input, expected) in tests.iter() {
            let evaluated = test_eval(input);
            assert_integer_object(&evaluated, *expected);
        }

        let null_tests = ["[1, 2, 3][3]", "[1, 2, 3][-1]"];

        for tt in null_tests.iter() {
            let input = tt;
            let evaluated = test_eval(input);
            assert_null_object(evaluated);
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"
            let two = "two";
            {
                "one": 10 - 9,
                two: 1 + 1,
                "thr" + "ee": 6 / 2,
                4: 4,
                true: 5,
                false: 6
            }"#;

        let mut expected = HashMap::new();
        expected.insert("one".to_string().hash_key(), 1);
        expected.insert("two".to_string().hash_key(), 2);
        expected.insert("three".to_string().hash_key(), 3);
        expected.insert((4 as i64).hash_key(), 4);
        expected.insert(true.hash_key(), 5);
        expected.insert(false.hash_key(), 6);

        let evaluated = test_eval(input);
        if let Object::Hash(result) = evaluated {
            assert_eq!(result.pairs.len(), expected.len());

            for (expected_key, expected_value) in expected.iter() {
                let pair = result.pairs.get(expected_key).unwrap();
                assert_integer_object(&pair.value, *expected_value as i64);
            }
        } else {
            assert!(false, "Eval didn't return Hash")
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        let tests = [
            (r#"{"foo": 5}["foo"]"#, 5),
            (r#"let key = "foo"; {"foo": 5}[key]"#, 5),
            ("{5: 5}[5]", 5),
            ("{true: 5}[true]", 5),
            ("{false: 5}[false]", 5),
        ];

        for (input, expected) in tests.iter() {
            let evaluated = test_eval(input);
            assert_integer_object(&evaluated, *expected);
        }

        let null_tests = [r#"{"foo": 5}["bar"]"#, r#"{}["foo"]"#];

        for tt in null_tests.iter() {
            let input = tt;
            let evaluated = test_eval(input);
            assert_null_object(evaluated);
        }
    }

    #[test]
    fn test_quote() {
        let tests = [
            ("quote(5)", "5"),
            ("quote(5 + 8)", "(5 + 8)"),
            ("quote(foobar + barfoo)", "(foobar + barfoo)"),
        ];

        for (input, expected) in tests.iter() {
            let evaluated = test_eval(input);
            if let Object::Quote(quote) = evaluated {
                assert_eq!(format!("{}", quote.node), expected as &str)
            } else {
                assert!(false, "expected Quote")
            }
        }
    }

    #[test]
    fn test_quote_unquote() {
        let tests = [
            ("quote(unquote(4))", "4"),
            ("quote(unquote(4 + 4))", "8"),
            ("quote(8 + unquote(4 + 4))", "(8 + 8)"),
            ("quote(unquote(4 + 4) + 8)", "(8 + 8)"),
            ("let foobar = 8; quote(foobar)", "foobar"),
            ("let foobar = 8; quote(unquote(foobar))", "8"),
            ("quote(unquote(true))", "true"),
            ("quote(unquote(true == false))", "false"),
            ("quote(unquote(quote(4 + 4)))", "(4 + 4)"),
            (
                r#"let quotedInfixExpression = quote(4 + 4);
            quote(unquote(4 + 4) + unquote(quotedInfixExpression))"#,
                "(8 + (4 + 4))",
            ),
        ];

        for (input, expected) in tests.iter() {
            let evaluated = test_eval(input);
            if let Object::Quote(quote) = evaluated {
                assert_eq!(format!("{}", quote.node), expected as &str)
            } else {
                assert!(false, "expected Quote")
            }
        }
    }

    #[test]
    fn test_define_macros() {
        let input = r#"
        let number = 1;
        let funciton = fn(x, y) { x + y; };
        let mymacro = macro(x, y) { x + y; };
        "#;

        let env = Environment::new();
        let mut program = test_parse_program(input);

        define_macros(&mut program, Rc::clone(&env));

        assert_eq!(program.statements.len(), 2);

        let borrowed_env = env.borrow();
        assert!(borrowed_env.get("number").is_none());
        assert!(borrowed_env.get("function").is_none());

        if let Some(obj) = borrowed_env.get("mymacro") {
            if let Object::Macro(macro_o) = obj {
                assert_eq!(macro_o.parameters.len(), 2);
                assert_eq!(macro_o.parameters[0].value, "x".to_string());
                assert_eq!(macro_o.parameters[1].value, "y".to_string());
                assert_eq!(format!("{}", macro_o.body), "(x + y)");
            } else {
                assert!(false, "object is not Macro")
            }
        } else {
            assert!(false, "macro not in environment.")
        }
    }

    #[test]
    fn test_expand_macros() {
        let tests = [
            (
                r#"
            let infixExpression = macro() { quote(1 + 2); };
            infixExpression();
            "#,
                "(1 + 2)",
            ),
            (
                r#"
            let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); };
            reverse(2 + 2, 10 - 5);
            "#,
                "(10 - 5) - (2 + 2)",
            ),
            (
                r#"
            let unless = macro(condition, consequence, alternative) {
                quote(if (!(unquote(condition))){
                    unquote(consequence);
                } else {
                    unquote(alternative);
                })
            };
            unless(10 > 5, puts("not greater"), puts("greater"));
            "#,
                r#"if (!(10 > 5)) { puts("not greater") } else { puts("greater") }"#,
            ),
        ];

        for (input, expected_str) in tests.iter() {
            let expected = test_parse_program(expected_str);
            let mut program = test_parse_program(input);

            let env = Environment::new();
            define_macros(&mut program, Rc::clone(&env));
            let expanded = expand_macros(program, &mut Rc::clone(&env));

            assert_eq!(format!("{}", expanded), format!("{}", expected));
        }
    }

    fn test_parse_program(input: &str) -> Program {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        p.parse_program()
    }

    fn test_eval(input: &str) -> Object {
        let l = Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();
        let mut env = Environment::new();
        program.eval(&mut env)
    }

    fn assert_integer_object(obj: &Object, expected: i64) {
        if let Object::Integer(result) = obj {
            assert_eq!(result, &expected)
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
