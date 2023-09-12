use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::{Rc, Weak};

use crate::ast::{BlockStatement, Identifier};

pub type BoxedObject = Rc<Object>;

#[derive(Debug, Clone, Default)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    #[default]
    Null,
    ReturnValue(BoxedObject),
    Error(String),
    Function {
        parameters: Vec<Identifier>,
        body: BlockStatement,
        environment: Weak<Environment>,
    },
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(value) => format!("{value}"),
            Self::Boolean(value) => format!("{value}"),
            Self::Null => "null".to_string(),
            Self::ReturnValue(object) => object.inspect(),
            Self::Error(message) => format!("ERROR: {message}"),
            Self::Function {
                parameters, body, ..
            } => {
                let parameters = parameters
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn ({parameters}) {{\n{body}\n}}")
            }
        }
    }

    pub fn type_string(&self) -> String {
        match self {
            Self::Integer(_) => "INTEGER",
            Self::Boolean(_) => "BOOLEAN",
            Self::Null => "NULL",
            Self::ReturnValue(_) => "RETURN_VALUE",
            Self::Error(_) => "ERROR",
            Self::Function { .. } => "FUNCTION",
        }
        .to_string()
    }

    /// Returns `true` if the `Object` is an instance of `Object::Error`.
    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error(_))
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inspect())
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<i64> for Object {
    fn from(value: i64) -> Self {
        Self::Integer(value)
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(val_left), Self::Integer(val_right)) => val_left == val_right,
            (Self::Boolean(val_left), Self::Boolean(val_right)) => val_left == val_right,
            (Self::Null, Self::Null) => true,
            (Self::ReturnValue(obj_left), Self::ReturnValue(obj_right)) => obj_left == obj_right,
            (Self::Error(msg_left), Self::Error(msg_right)) => msg_left == msg_right,
            (
                Self::Function {
                    parameters: params_left,
                    body: body_left,
                    environment: env_left,
                },
                Self::Function {
                    parameters: params_right,
                    body: body_right,
                    environment: env_right,
                },
            ) => {
                let params_eq = params_left == params_right;
                let body_eq = body_left == body_right;
                let env_eq = {
                    if let (Some(env_left), Some(env_right)) =
                        (env_left.upgrade(), env_right.upgrade())
                    {
                        env_left == env_right
                    } else {
                        false
                    }
                };
                params_eq && body_eq && env_eq
            }
            (_, _) => false,
        }
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct Environment {
    store: RefCell<HashMap<String, BoxedObject>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: RefCell::new(HashMap::new()),
        }
    }

    pub fn get(&self, name: &str) -> Option<BoxedObject> {
        self.store.borrow().get(name).map(Rc::clone)
    }

    pub fn set(&self, name: String, value: BoxedObject) {
        self.store.borrow_mut().insert(name, value);
    }
}

pub type BoxedEnvironment = Rc<Environment>;
