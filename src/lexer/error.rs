use std::fmt;

#[derive(Debug)]
pub struct LexerError {
    pub message: String,
    pub line: usize,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Lexer error on line {}: {}", self.line, self.message)
    }
}

impl std::error::Error for LexerError {}
