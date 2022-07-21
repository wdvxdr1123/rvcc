use std::{fmt, error::Error};

#[derive(Debug)]
pub struct SyntaxError {
    pub line: usize, 
    pub col: usize,
    pub msg: String,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.col, self.msg)
    }
}

impl Error for SyntaxError{}

pub type Result<T> = std::result::Result<T,SyntaxError>;