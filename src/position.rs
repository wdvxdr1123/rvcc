#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn no_postion() -> Self {
        Position { line: 0, column: 0 }
    }
}
