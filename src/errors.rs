use thiserror::Error;

#[derive(Debug, Error)]
pub enum SerError {
    #[error("too many schema nodes for u32")]
    TooManySchemaNodes,

    #[error("too many schema node lists for u32")]
    TooManySchemaNodeLists,

    #[error("too many fields for u32")]
    TooManyFields,

    #[error("too many names for u32")]
    TooManyNames,

    #[error("too many field lists for u32")]
    TooManyNameLists,

    #[error("too many values for u32")]
    TooManyValues,

    #[error("custom: {0}")]
    Custom(String),
}

impl serde::ser::Error for SerError {
    #[inline]
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        SerError::Custom(msg.to_string())
    }
}
