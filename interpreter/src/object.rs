use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};

use lazy_static::lazy_static;
use slotmap::{DefaultKey, SlotMap};

use crate::ast::{BlockStatement, Identifier};

lazy_static! {
    static ref ENVIRONMENTS: Mutex<SlotMap<DefaultKey, BoxedEnvironment>> =
        Mutex::new(SlotMap::new());
}

pub type BoxedObject = Arc<Object>;
type BoxedEnvironment = Arc<Environment>;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub environment_key: DefaultKey,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    #[default]
    Null,
    ReturnValue(BoxedObject),
    Error(String),
    Function(Function),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(value) => format!("{value}"),
            Self::Boolean(value) => format!("{value}"),
            Self::Null => "null".to_string(),
            Self::ReturnValue(object) => object.inspect(),
            Self::Error(message) => format!("ERROR: {message}"),
            Self::Function(function) => {
                let parameters = function
                    .parameters
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn ({parameters}) {{\n{}\n}}", function.body)
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

#[derive(Debug, Default)]
pub struct Environment {
    id: usize,
    store: Mutex<HashMap<String, BoxedObject>>,
    outer_key: Option<DefaultKey>,
}

impl Environment {
    pub fn new() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let id = COUNTER.fetch_add(1, Ordering::Relaxed);
        Self {
            id,
            store: Mutex::new(HashMap::new()),
            outer_key: None,
        }
    }

    pub fn new_enclosed(outer_key: DefaultKey) -> Self {
        let mut environment = Self::new();
        environment.outer_key = Some(outer_key);
        environment
    }

    pub fn get(&self, name: &str) -> Option<BoxedObject> {
        if let Some(object) = self.store.lock().unwrap().get(name) {
            Some(Arc::clone(object))
        } else {
            let outer = get_environment(self.outer_key?)
                .expect("Outer environment should exist if the key exists");
            outer.get(name)
        }
    }

    pub fn set(&self, name: String, value: BoxedObject) {
        self.store.lock().unwrap().insert(name, value);
    }
}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

pub fn add_environment(environment: &BoxedEnvironment) -> DefaultKey {
    ENVIRONMENTS.lock().unwrap().insert(Arc::clone(environment))
}

pub fn get_environment(key: DefaultKey) -> Option<BoxedEnvironment> {
    ENVIRONMENTS.lock().unwrap().get(key).map(Arc::clone)
}
