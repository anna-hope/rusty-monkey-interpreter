use std::collections::HashMap;
use std::rc::Rc;

pub type BoxedObject = Rc<Object>;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    #[default]
    Null,
    ReturnValue(BoxedObject),
    Error(String),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(value) => format!("{value}"),
            Self::Boolean(value) => format!("{value}"),
            Self::Null => "null".to_string(),
            Self::ReturnValue(object) => object.inspect(),
            Self::Error(message) => format!("ERROR: {message}"),
        }
    }

    pub fn type_string(&self) -> String {
        match self {
            Self::Integer(_) => "INTEGER",
            Self::Boolean(_) => "BOOLEAN",
            Self::Null => "NULL",
            Self::ReturnValue(_) => "RETURN_VALUE",
            Self::Error(_) => "ERROR",
        }
        .to_string()
    }

    /// Returns `true` if the `Object` is an instance of `Object::Error`.
    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error(_))
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
    store: HashMap<String, BoxedObject>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<BoxedObject> {
        self.store.get(name).map(Rc::clone)
    }

    pub fn set(&mut self, name: String, value: BoxedObject) {
        self.store.insert(name, value);
    }
}
