//! TODO:
//!  - Add more error types
//!  - Implement error recovery (Synchronization) - https://craftinginterpreters.com/parsing-expressions.html#panic-mode-error-recovery

use crate::rslox::scanner;

use super::{scanner::Token, ParserError};

/// Parser

pub struct Parser {
    tokens: Vec<scanner::Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<scanner::Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<ParserError>> {
        let mut statements: Vec<Stmt> = Vec::new();
        let mut errors: Vec<ParserError> = Vec::new();
        while !self.is_at_end() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(error) => {
                    errors.push(error);
                    self.synchronize();
                }
            }
        }
        if errors.is_empty() {
            Err(errors)
        } else {
            Ok(statements)
        }
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        let result = match self.peek().token_type {
            scanner::TokenType::Print => {
                self.advance();
                self.parse_print_statement()
            }
            _ => self.parse_expression_statement(),
        };
        self.expect_token(scanner::TokenType::Semicolon)?;
        result
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt, ParserError> {
        Ok(Stmt::Expression {
            expression: self.parse_expression()?,
        })
    }

    fn parse_print_statement(&mut self) -> Result<Stmt, ParserError> {
        Ok(Stmt::Print {
            expression: self.parse_expression()?,
        })
    }

    fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_comparison()?;
        while let Some(operator) = self.match_tokens(&[
            scanner::TokenType::BangEqual,
            scanner::TokenType::EqualEqual,
        ]) {
            let right = self.parse_comparison()?;
            let operator = LoxBinaryOperator::try_from(operator)?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_term()?;
        while let Some(operator) = self.match_tokens(&[
            scanner::TokenType::Greater,
            scanner::TokenType::GreaterEqual,
            scanner::TokenType::Less,
            scanner::TokenType::LessEqual,
        ]) {
            let right = self.parse_term().unwrap();
            let operator = LoxBinaryOperator::try_from(operator)?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_factor()?;
        while let Some(operator) =
            self.match_tokens(&[scanner::TokenType::Plus, scanner::TokenType::Minus])
        {
            let right = self.parse_factor()?;

            let operator = LoxBinaryOperator::try_from(operator)?;

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_unary()?;
        while let Some(operator) =
            self.match_tokens(&[scanner::TokenType::Star, scanner::TokenType::Slash])
        {
            let right = self.parse_unary()?;

            let operator = LoxBinaryOperator::try_from(operator)?;

            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(operator) =
            self.match_tokens(&[scanner::TokenType::Bang, scanner::TokenType::Minus])
        {
            let right = self.parse_unary()?;

            let operator = LoxUnaryOperator::try_from(operator)?;

            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        if self.match_token(scanner::TokenType::False).is_some() {
            return Ok(Expr::Literal {
                value: LoxParserValue::Boolean(false),
            });
        }
        if self.match_token(scanner::TokenType::True).is_some() {
            return Ok(Expr::Literal {
                value: LoxParserValue::Boolean(true),
            });
        }
        if self.match_token(scanner::TokenType::Nil).is_some() {
            return Ok(Expr::Literal {
                value: LoxParserValue::Nil,
            });
        }
        if let Some(token) = self.match_token(scanner::TokenType::Number) {
            return Ok(Expr::Literal {
                value: LoxParserValue::Number(token.literal.unwrap().parse().unwrap()),
            });
        }
        if let Some(token) = self.match_token(scanner::TokenType::String) {
            return Ok(Expr::Literal {
                value: LoxParserValue::String(token.literal.unwrap()),
            });
        }

        if self.match_token(scanner::TokenType::LeftParen).is_some() {
            let expr = self.parse_expression()?;
            self.expect_token(scanner::TokenType::RightParen)?;
            return Ok(Expr::Grouping {
                expression: Box::new(expr),
            });
        }

        let &Token {
            token_type,
            line,
            column,
            ..
        } = self.peek();
        Err(ParserError::UnexpectedTokenNoExpected(
            token_type, line, column,
        ))
    }

    // Utils

    fn advance(&mut self) -> Option<scanner::Token> {
        self.current += 1;
        if self.is_at_end() {
            None
        } else {
            Some(self.tokens[self.current - 1].clone())
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() - 1
    }

    fn peek(&self) -> &scanner::Token {
        &self.tokens[self.current]
    }

    fn expect_token(&mut self, token_type: scanner::TokenType) -> Result<(), ParserError> {
        if self.peek().token_type == token_type {
            self.advance();
            return Ok(());
        }
        Err(ParserError::UnexpectedToken(
            self.peek().token_type,
            token_type,
            self.peek().line,
            self.peek().column,
        ))
    }

    fn match_token(&mut self, token_type: scanner::TokenType) -> Option<scanner::Token> {
        if self.is_at_end() {
            return None;
        }
        if self.peek().token_type != token_type {
            return None;
        }
        let token = self.peek().clone();
        self.advance();
        Some(token)
    }

    // The name of this function is unclear. `match_any_token` maybe? At least a good doc comment to explain the function is in order
    fn match_tokens(&mut self, tokens: &[scanner::TokenType]) -> Option<scanner::Token> {
        if self.is_at_end() {
            return None;
        }
        for token_type in tokens {
            if &self.peek().token_type == token_type {
                return self.advance();
            }
        }
        None
    }

    fn synchronize(&mut self) {
        // TODO: Fix synchronize when the error is unexpected semicolon (should just skip the semicolon)

        self.advance();
        while !self.is_at_end() {
            if self.peek().token_type == scanner::TokenType::Semicolon {
                self.advance();
                return;
            }
            match self.peek().token_type {
                scanner::TokenType::Class
                | scanner::TokenType::Fun
                | scanner::TokenType::Var
                | scanner::TokenType::For
                | scanner::TokenType::If
                | scanner::TokenType::While
                | scanner::TokenType::Print
                | scanner::TokenType::Return => return,
                _ => {}
            }
            self.advance();
        }
    }
}

/// Statement

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression { expression: Expr },
    Print { expression: Expr },
}

/// Expression

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: LoxBinaryOperator,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: LoxParserValue,
    },
    Unary {
        operator: LoxUnaryOperator,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum LoxParserValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug, Clone, Copy)]
pub enum LoxBinaryOperator {
    Plus,
    Minus,
    Star,
    Slash,
    EqualEqual,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl TryFrom<scanner::Token> for LoxBinaryOperator {
    type Error = ParserError;
    fn try_from(value: scanner::Token) -> Result<Self, ParserError> {
        match value.token_type {
            scanner::TokenType::Plus => Ok(LoxBinaryOperator::Plus),
            scanner::TokenType::Minus => Ok(LoxBinaryOperator::Minus),
            scanner::TokenType::Star => Ok(LoxBinaryOperator::Star),
            scanner::TokenType::Slash => Ok(LoxBinaryOperator::Slash),
            scanner::TokenType::EqualEqual => Ok(LoxBinaryOperator::EqualEqual),
            scanner::TokenType::BangEqual => Ok(LoxBinaryOperator::BangEqual),
            scanner::TokenType::Greater => Ok(LoxBinaryOperator::Greater),
            scanner::TokenType::GreaterEqual => Ok(LoxBinaryOperator::GreaterEqual),
            scanner::TokenType::Less => Ok(LoxBinaryOperator::Less),
            scanner::TokenType::LessEqual => Ok(LoxBinaryOperator::LessEqual),
            _ => Err(ParserError::UnexpectedTokenNoExpected(
                value.token_type,
                value.line,
                value.column,
            )),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LoxUnaryOperator {
    Minus,
    Bang,
}

impl TryFrom<scanner::Token> for LoxUnaryOperator {
    type Error = ParserError;
    fn try_from(value: scanner::Token) -> Result<Self, ParserError> {
        match value.token_type {
            scanner::TokenType::Minus => Ok(LoxUnaryOperator::Minus),
            scanner::TokenType::Bang => Ok(LoxUnaryOperator::Bang),
            _ => Err(ParserError::UnexpectedTokenNoExpected(
                value.token_type,
                value.line,
                value.column,
            )),
        }
    }
}
