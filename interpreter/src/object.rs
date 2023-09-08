#[derive(Debug, Clone, Default)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    #[default]
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(value) => format!("{value}"),
            Self::Boolean(value) => format!("{value}"),
            Self::Null => "null".to_string(),
        }
    }
}
