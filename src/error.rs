use std::{error::Error, fmt};

use crate::position::Position;

#[derive(Debug)]
pub struct SyntaxError {
    pub pos: Position,
    pub msg: String,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}: {}", self.pos.line, self.pos.column, self.msg)
    }
}

impl Error for SyntaxError {}

pub type Result<T> = std::result::Result<T, SyntaxError>;

pub fn error_at(file: String, pos: Position, msg: String) {
    eprintln!("{}", file);
    eprintln!(
        "{delim:>width$} {msg}",
        delim = "^",
        msg = msg,
        width = pos.column
    );
}
