//! Parser utility functions module
//! 
//! This module contains helper functions and utilities used throughout the parser.

use crate::lexer::{Token, TokenType};
use crate::ast::{BinaryOperator, UnaryOperator};
use super::ParseError;

/// Converts a token to a binary operator
pub fn binary_operator_from_token(token: &Token) -> Result<BinaryOperator, ParseError> {
    match &token.token_type {
        TokenType::Plus => Ok(BinaryOperator::Plus),
        TokenType::Minus => Ok(BinaryOperator::Minus),
        TokenType::Star => Ok(BinaryOperator::Star),
        TokenType::Slash => Ok(BinaryOperator::Slash),
        TokenType::Percent => Ok(BinaryOperator::Percent),
        TokenType::EqualEqual => Ok(BinaryOperator::EqualEqual),
        TokenType::NotEqual => Ok(BinaryOperator::NotEqual),
        TokenType::Less => Ok(BinaryOperator::Less),
        TokenType::LessEqual => Ok(BinaryOperator::LessEqual),
        TokenType::Greater => Ok(BinaryOperator::Greater),
        TokenType::GreaterEqual => Ok(BinaryOperator::GreaterEqual),
        TokenType::And => Ok(BinaryOperator::And),
        TokenType::Or => Ok(BinaryOperator::Or),
        _ => Err(ParseError {
            message: format!("Invalid binary operator: {:?}", token.token_type),
            line: token.line,
        }),
    }
}

/// Converts a token to a unary operator
pub fn unary_operator_from_token(token: &Token) -> Result<UnaryOperator, ParseError> {
    match &token.token_type {
        TokenType::Minus => Ok(UnaryOperator::Negate),
        TokenType::Not => Ok(UnaryOperator::Not),
        _ => Err(ParseError {
            message: format!("Invalid unary operator: {:?}", token.token_type),
            line: token.line,
        }),
    }
}

/// Checks if the current token matches the expected type
pub fn match_token<'a>(parser: &'a mut super::Parser, token_type: &TokenType) -> bool {
    if check(parser, token_type) {
        parser.advance();
        return true;
    }
    false
}

/// Checks if the current token matches the expected type without consuming it
pub fn check(parser: &super::Parser, token_type: &TokenType) -> bool {
    if parser.is_at_end() {
        return false;
    }
    std::mem::discriminant(&parser.peek().token_type) == std::mem::discriminant(token_type)
}

/// Advances to the next token and returns the previous one
pub fn advance<'a>(parser: &'a mut super::Parser) -> &'a Token {
    if !parser.is_at_end() {
        parser.current += 1;
    }
    parser.previous()
}

/// Checks if the parser has reached the end of tokens
pub fn is_at_end(parser: &super::Parser) -> bool {
    parser.current >= parser.tokens.len() || matches!(parser.peek().token_type, TokenType::Eof)
}

/// Returns the current token without consuming it
pub fn peek<'a>(parser: &'a super::Parser) -> &'a Token {
    &parser.tokens[parser.current]
}

/// Returns the previous token
pub fn previous<'a>(parser: &'a super::Parser) -> &'a Token {
    &parser.tokens[parser.current - 1]
}

/// Consumes a token of the expected type or returns an error
pub fn consume<'a>(parser: &'a mut super::Parser, token_type: &TokenType, message: &str) -> Result<&'a Token, ParseError> {
    if check(parser, token_type) {
        return Ok(advance(parser));
    }
    
    Err(ParseError {
        message: message.to_string(),
        line: peek(parser).line,
    })
}