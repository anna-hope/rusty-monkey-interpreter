use std::mem;
use std::sync::Arc;

use slotmap::DefaultKey;

use crate::ast::{BlockStatement, Expression, Identifier, Program, Statement};
use crate::object::{add_environment, get_environment, BoxedObject, Environment, Function, Object};

const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

pub fn eval_program(program: &Program, environment_key: DefaultKey) -> BoxedObject {
    let mut result = Arc::new(NULL);

    for statement in &program.statements {
        result = eval(statement, environment_key);

        match result.as_ref() {
            Object::ReturnValue(value) => return Arc::clone(&value),
            Object::Error(_) => return Arc::clone(&result),
            _ => continue,
        }
    }

    Arc::clone(&result)
}

fn eval(statement: &Statement, environment_key: DefaultKey) -> BoxedObject {
    match statement {
        Statement::ExpressionStatement { expression, .. } => {
            eval_expression(expression, environment_key)
        }
        Statement::Block(block) => eval_block_statement(block, environment_key),
        Statement::Return { value, .. } => {
            let value = eval_expression(value, environment_key);
            if value.is_error() {
                value
            } else {
                Arc::new(Object::ReturnValue(Arc::clone(&value)))
            }
        }
        Statement::Let { value, name, .. } => {
            let value = eval_expression(value, environment_key);
            if value.is_error() {
                return value;
            }
            let environment = get_environment(environment_key).unwrap();
            environment.set(name.value.clone(), Arc::clone(&value));
            value
        }
    }
}

fn eval_block_statement(block: &BlockStatement, environment_key: DefaultKey) -> BoxedObject {
    let mut result = Arc::new(NULL);

    for statement in &block.statements {
        result = eval(statement, environment_key);

        if matches!(result.as_ref(), Object::ReturnValue(_))
            || matches!(result.as_ref(), Object::Error(_))
        {
            return result;
        }
    }

    result
}

fn eval_expression(expression: &Expression, environment_key: DefaultKey) -> BoxedObject {
    match expression {
        Expression::IntegerLiteral { value, .. } => Arc::new(Object::from(*value)),
        Expression::Boolean { value, .. } => Arc::new(native_bool_to_boolean_object(*value)),
        Expression::Prefix {
            operator, right, ..
        } => {
            let right = eval_expression(right, environment_key);
            if right.is_error() {
                right
            } else {
                Arc::clone(&eval_prefix_expression(operator, &right))
            }
        }
        Expression::Infix {
            left,
            operator,
            right,
            ..
        } => {
            let left = eval_expression(left, environment_key);
            if left.is_error() {
                return left;
            }

            let right = eval_expression(right, environment_key);
            if right.is_error() {
                return right;
            }

            Arc::new(eval_infix_expression(operator, &left, &right))
        }
        Expression::If {
            condition,
            consequence,
            alternative,
            ..
        } => {
            let condition = eval_expression(condition, environment_key);

            if condition.is_error() {
                return condition;
            }

            if is_truthy(&condition) {
                Arc::clone(&eval_block_statement(consequence, environment_key))
            } else if let Some(alternative) = alternative {
                Arc::clone(&eval_block_statement(alternative, environment_key))
            } else {
                Arc::new(NULL)
            }
        }
        Expression::Identifier(identifier) => eval_identifier(identifier, environment_key),
        Expression::FunctionLiteral {
            parameters, body, ..
        } => {
            let function = Function {
                parameters: parameters.clone(),
                body: body.clone(),
                environment_key,
            };
            Arc::new(Object::Function(function))
        }
        Expression::Call {
            function,
            arguments,
            ..
        } => {
            let function = eval_expression(function, environment_key);
            if function.is_error() {
                return function;
            }

            let args = eval_expressions(arguments, environment_key);
            if args.len() == 1 && args[0].is_error() {
                return Arc::clone(&args[0]);
            }

            if let Object::Function(function) = function.as_ref() {
                apply_function(function, &args)
            } else {
                Arc::new(Object::Error(format!(
                    "not a function: {}",
                    function.type_string()
                )))
            }
        }
        _ => todo!(),
    }
}

fn eval_expressions(expressions: &[Expression], environment_key: DefaultKey) -> Vec<BoxedObject> {
    let mut result = vec![];

    for expression in expressions {
        let evaluated = eval_expression(expression, environment_key);
        if evaluated.is_error() {
            return vec![evaluated];
        }
        result.push(evaluated);
    }

    result
}

fn eval_identifier(node: &Identifier, environment_key: DefaultKey) -> BoxedObject {
    let environment = get_environment(environment_key).unwrap();
    if let Some(value) = environment.get(&node.value) {
        value
    } else {
        Arc::new(Object::Error(format!(
            "identifier not found: {}",
            node.value
        )))
    }
}

fn is_truthy(object: &Object) -> bool {
    match object {
        Object::Null => false,
        Object::Boolean(value) => *value,
        _ => true,
    }
}

fn native_bool_to_boolean_object(input: bool) -> Object {
    input.into()
}

fn eval_prefix_expression(operator: &str, right: &Object) -> BoxedObject {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => Arc::new(eval_minus_prefix_operator_expression(right)),
        _ => Arc::new(Object::Error(format!(
            "unknown operator {operator}{}",
            right.type_string()
        ))),
    }
}

fn eval_bang_operator_expression(right: &Object) -> BoxedObject {
    let object = match right {
        Object::Boolean(value) => {
            if *value {
                FALSE
            } else {
                TRUE
            }
        }
        Object::Null => TRUE,
        _ => FALSE,
    };
    Arc::new(object)
}

fn eval_minus_prefix_operator_expression(right: &Object) -> Object {
    match right {
        Object::Integer(value) => Object::from(-value),
        _ => Object::Error(format!("unknown operator: -{}", right.type_string())),
    }
}

fn eval_infix_expression(operator: &str, left: &Object, right: &Object) -> Object {
    if mem::discriminant(left) != mem::discriminant(right) {
        return Object::Error(format!(
            "type mismatch: {} {operator} {}",
            left.type_string(),
            right.type_string()
        ));
    }

    match (operator, left, right) {
        (_, Object::Integer(left_val), Object::Integer(right_val)) => {
            eval_integer_infix_expression(operator, *left_val, *right_val)
        }
        ("==", _, _) => native_bool_to_boolean_object(left == right),
        ("!=", _, _) => native_bool_to_boolean_object(left != right),
        _ => Object::Error(format!(
            "unknown operator: {} {operator} {}",
            left.type_string(),
            right.type_string()
        )),
    }
}

fn eval_integer_infix_expression(operator: &str, left: i64, right: i64) -> Object {
    match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => native_bool_to_boolean_object(left < right),
        ">" => native_bool_to_boolean_object(left > right),
        "==" => native_bool_to_boolean_object(left == right),
        "!=" => native_bool_to_boolean_object(left != right),
        _ => Object::Error(format!("unknown operator: INTEGER {operator} INTEGER")),
    }
}

fn apply_function(function: &Function, args: &[BoxedObject]) -> BoxedObject {
    let extended_env = Arc::new(Environment::new_enclosed(function.environment_key));
    let extended_env_key = add_environment(&extended_env);

    for (param, arg) in function.parameters.iter().zip(args) {
        extended_env.set(param.value.clone(), Arc::clone(arg));
    }

    let evaluated = eval_block_statement(&function.body, extended_env_key);
    match evaluated.as_ref() {
        Object::ReturnValue(value) => Arc::clone(value),
        _ => evaluated,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn run_eval(input: &str) -> BoxedObject {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program().unwrap();
        let environment = Arc::new(Environment::new());
        let environment_key = add_environment(&environment);
        eval_program(&program, environment_key)
    }

    fn test_integer_object(object: &Object, expected: i64) {
        match object {
            Object::Integer(value) => assert_eq!(*value, expected),
            _ => panic!("Object is not Integer"),
        }
    }

    fn test_boolean_object(object: &Object, expected: bool) {
        match object {
            Object::Boolean(value) => assert_eq!(*value, expected),
            _ => panic!("Object is not Boolean"),
        }
    }

    fn test_null_object(object: &Object) {
        assert!(matches!(object, Object::Null));
    }

    #[test]
    fn integer_expression() {
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

        for (input, expected) in tests {
            let evaluated = run_eval(input);
            test_integer_object(&evaluated, expected);
        }
    }

    #[test]
    fn boolean_expression() {
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
        for (input, expected) in tests {
            let evaluated = run_eval(input);
            test_boolean_object(&evaluated, expected);
        }
    }

    #[test]
    fn bang_operator() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = run_eval(input);
            test_boolean_object(&evaluated, expected);
        }
    }

    #[test]
    fn if_else_expressions() {
        let tests = [
            ("if (true) { 10 }", Some(10)),
            ("if (false) { 10 }", None),
            ("if (1) { 10 }", Some(10)),
            ("if (1 < 2) { 10 }", Some(10)),
            ("if (1 > 2) { 10 }", None),
            ("if (1 > 2) { 10 } else { 20 }", Some(20)),
            ("if (1 < 2) { 10 } else { 20 }", Some(10)),
        ];

        for (input, expected) in tests {
            let evaluated = run_eval(input);
            if let Some(value) = expected {
                test_integer_object(&evaluated, value);
            } else {
                test_null_object(&evaluated);
            }
        }
    }

    #[test]
    fn return_statements() {
        let tests = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
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

        for (input, expected) in tests {
            let evaluated = run_eval(input);
            test_integer_object(&evaluated, expected);
        }
    }

    #[test]
    fn error_handling() {
        let tests = [
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                r#"
if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }
  return 1; 
}"#,
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, expected) in tests {
            let evaluated = run_eval(input);

            match evaluated.as_ref() {
                Object::Error(message) => assert_eq!(message, expected),
                _ => panic!("No error object returned! Got {evaluated:?}"),
            }
        }
    }

    #[test]
    fn let_statements() {
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            let evaluated = run_eval(input);
            test_integer_object(&evaluated, expected);
        }
    }

    #[test]
    fn function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = run_eval(input);

        match evaluated.as_ref() {
            Object::Function(function) => {
                assert_eq!(function.parameters.len(), 1);
                assert_eq!(function.parameters[0].to_string(), "x");

                let expected_body = "(x + 2)";
                assert_eq!(function.body.to_string(), expected_body);
            }
            _ => panic!("Object is not Function. Got {evaluated:?}"),
        }
    }

    #[test]
    fn function_application() {
        let tests = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in tests {
            let evaluated = run_eval(input);
            test_integer_object(&evaluated, expected);
        }
    }

    #[test]
    fn closures() {
        let input = r#"
let newAdder = fn(x) {
     fn(y) { x + y };
};
   let addTwo = newAdder(2);
   addTwo(2);
"#;
        let evaluated = run_eval(input);
        test_integer_object(&evaluated, 4);
    }
}
