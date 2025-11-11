pub mod error;
pub mod lexer;
pub mod token;

#[cfg(test)]
mod tests;

pub use self::error::LexerError;
pub use self::lexer::Lexer;
pub use self::token::{Token, TokenType};

pub fn tokenize(source: &str) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::new(source);
    lexer.tokenize()
}