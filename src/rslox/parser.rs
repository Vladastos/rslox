//! TODO:
//!  - Add more error types
//!  - Improve error messages

use ordered_float::OrderedFloat;

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

    /// Parses all statements in the source code.
    ///
    /// On error, returns a vector of `ParserError`.
    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<ParserError>> {
        let mut statements: Vec<Stmt> = Vec::new();
        let mut errors: Vec<ParserError> = Vec::new();
        while !self.is_at_end() {
            match self.parse_declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(error) => {
                    errors.push(error);
                    self.synchronize();
                }
            }
        }
        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(statements)
        }
    }

    fn parse_declaration(&mut self) -> Result<Stmt, ParserError> {
        let result = match self.peek().token_type {
            scanner::TokenType::Var => self.parse_var_declaration()?,
            _ => self.parse_statement()?,
        };
        self.expect_token(scanner::TokenType::Semicolon)?;
        Ok(result)
    }

    fn parse_var_declaration(&mut self) -> Result<Stmt, ParserError> {
        let token = self.expect_token(scanner::TokenType::Var)?;
        self.expect_token(scanner::TokenType::Identifier)?;

        let initializer = if self.peek().token_type == scanner::TokenType::Equal {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        Ok(Stmt::VarDeclaration {
            name: token.lexeme,
            initializer,
        })
    }

    /// Parses a statement.
    ///
    /// This is the entry point for parsing a statement. It will parse an expression statement or a print statement.
    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        let result = match self.peek().token_type {
            scanner::TokenType::Print => self.parse_print_statement(),
            scanner::TokenType::LeftBrace => self.parse_block_statement(),
            _ => self.parse_expression_statement(),
        };
        result
    }

    /// Parses an expression statement.
    ///
    /// This function parses an expression and constructs a `Stmt::Expression` node containing the parsed expression.
    /// Returns an error if the expression cannot be parsed.
    fn parse_expression_statement(&mut self) -> Result<Stmt, ParserError> {
        Ok(Stmt::Expression {
            expression: self.parse_expression()?,
        })
    }

    /// Parses a print statement.
    ///
    /// This function assumes that the current token is a 'print' keyword and
    /// advances the token stream. It then parses the following expression
    /// and constructs a `Stmt::Print` node containing the parsed expression.
    ///
    /// Returns an error if the expression cannot be parsed.
    fn parse_print_statement(&mut self) -> Result<Stmt, ParserError> {
        self.advance();
        Ok(Stmt::Print {
            expression: self.parse_expression()?,
        })
    }

    fn parse_block_statement(&mut self) -> Result<Stmt, ParserError> {
        self.advance();
        let mut statements: Vec<Stmt> = Vec::new();
        while self.peek().token_type != scanner::TokenType::RightBrace {
            if self.is_at_end() {
                return Err(ParserError::UnterminatedBlock {
                    line: self.peek().line,
                    column: self.peek().column,
                });
            }
            statements.push(self.parse_declaration()?);
        }
        self.expect_token(scanner::TokenType::RightBrace)?;
        Ok(Stmt::Block { statements })
    }

    /// Parses an expression.
    ///
    /// This is the entry point for parsing an expression. It will parse a logical expression.
    fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParserError> {
        let token = self.peek().clone();
        let expr = self.parse_logical_or()?;
        if self.match_token(scanner::TokenType::Equal).is_some() {
            let value = self.parse_expression()?;
            if let Expr::Variable { name } = expr {
                return Ok(Expr::Assignment {
                    name,
                    value: Box::new(value),
                });
            } else {
                return Err(ParserError::InvalidAssignmentTarget {
                    line: token.line,
                    column: token.column,
                });
            }
        }
        Ok(expr)
    }

    fn parse_logical_or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_logical_and()?;
        while let Some(operator) = self.match_token(scanner::TokenType::Or) {
            let right = self.parse_logical_and()?;
            let operator = LoxBinaryOperator::try_from(operator)?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_comparison()?;
        while let Some(operator) = self.match_token(scanner::TokenType::And) {
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
    /// Parses a comparison expression.
    ///
    /// This handles comparison operations by first parsing a term expression.
    /// Then, it checks for any comparison operators, and recursively parses
    /// the right-hand side as another term expression. The result is a binary expression node
    /// representing the comparison.
    ///
    /// If no comparison operator is found, returns the result of the term expression.
    fn parse_comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_term()?;
        if let Some(operator) = self.match_any_token(&[
            scanner::TokenType::Greater,
            scanner::TokenType::GreaterEqual,
            scanner::TokenType::Less,
            scanner::TokenType::LessEqual,
            scanner::TokenType::BangEqual,
            scanner::TokenType::EqualEqual,
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

    /// Parses a term expression.
    ///
    /// This handles addition and subtraction operations by first parsing a factor expression.
    /// Then, it checks for any addition or subtraction operators, and recursively parses
    /// the right-hand side as another factor expression. The result is a binary expression node
    /// representing the term.
    ///
    /// If no addition or subtraction operator is found, returns the result of the factor expression.
    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_factor()?;
        while let Some(operator) =
            self.match_any_token(&[scanner::TokenType::Plus, scanner::TokenType::Minus])
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

    /// Parses a factor expression.
    ///
    /// This handles multiplication and division operations by first parsing a unary expression.
    /// Then, it checks for any multiplication or division operators, and recursively parses
    /// the right-hand side as another unary expression. The result is a binary expression node
    /// representing the factor.
    ///
    /// If no multiplication or division operator is found, returns the result of the unary expression.
    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_unary()?;
        while let Some(operator) =
            self.match_any_token(&[scanner::TokenType::Star, scanner::TokenType::Slash])
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

    /// Parses a unary expression.
    ///
    /// This is either a negation prefix (e.g. `-1`), or a primary expression.
    ///
    /// If the current token is not a negation prefix, returns the result of parsing a primary
    /// expression.
    ///
    /// If the current token is a negation prefix, it is consumed and the right operand is
    /// recursively parsed.
    fn parse_unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(operator) =
            self.match_any_token(&[scanner::TokenType::Bang, scanner::TokenType::Minus])
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

    /// Parses a primary expression.
    ///
    /// This is either a literal value (false, true, nil, a number, or a string), or a grouping
    /// expression, which is a parenthesized expression.
    ///
    /// If the current token is not a primary expression, returns an `UnexpectedTokenNoExpected` error.
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
        if let Some(token) = self.match_token(scanner::TokenType::Identifier) {
            return Ok(Expr::Variable { name: token.lexeme });
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
        Err(ParserError::UnexpectedTokenNoExpected {
            token_type,
            line,
            column,
        })
    }

    // Utils

    /// Consumes the current token.
    /// Returns None if the parser is at the end of the tokens.
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

    /// Consumes the current token if it matches the given `token_type`.
    /// Returns an error if the current token does not match the given `token_type`.
    fn expect_token(&mut self, token_type: scanner::TokenType) -> Result<Token, ParserError> {
        if self.peek().token_type == token_type {
            self.advance();
            return Ok(self.peek().clone());
        }
        let &scanner::Token {
            token_type: found,
            line,
            column,
            ..
        } = self.peek();
        Err(ParserError::UnexpectedToken {
            found,
            expected: token_type,
            line,
            column,
        })
    }

    /// If the current token matches the given `token_type`, returns the matched token and consumes
    /// it. Otherwise, returns `None`.
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

    /// If the current token matches any of the given `token_type`s, returns the matched token and
    /// consumes it. Otherwise, returns `None`.
    fn match_any_token(&mut self, tokens: &[scanner::TokenType]) -> Option<scanner::Token> {
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

    /// Consumes tokens until it reaches the start of the next statement, or the end of the file.
    /// Used to recover from a parse error.
    ///
    /// The list of "statement start" tokens is based on the Lox language definition.
    ///
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
    Expression {
        expression: Expr,
    },
    Print {
        expression: Expr,
    },
    VarDeclaration {
        name: String,
        initializer: Option<Expr>,
    },
    Block {
        statements: Vec<Stmt>,
    },
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
    Variable {
        name: String,
    },
    Assignment {
        name: String,
        value: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum LoxParserValue {
    Number(OrderedFloat<f64>),
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
    And,
    Or,
}
impl std::fmt::Display for LoxBinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxBinaryOperator::Plus => write!(f, "+"),
            LoxBinaryOperator::Minus => write!(f, "-"),
            LoxBinaryOperator::Star => write!(f, "*"),
            LoxBinaryOperator::Slash => write!(f, "/"),
            LoxBinaryOperator::EqualEqual => write!(f, "=="),
            LoxBinaryOperator::BangEqual => write!(f, "!="),
            LoxBinaryOperator::Greater => write!(f, ">"),
            LoxBinaryOperator::GreaterEqual => write!(f, ">="),
            LoxBinaryOperator::Less => write!(f, "<"),
            LoxBinaryOperator::LessEqual => write!(f, "<="),
            LoxBinaryOperator::And => write!(f, "&&"),
            LoxBinaryOperator::Or => write!(f, "||"),
        }
    }
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
            scanner::TokenType::And => Ok(LoxBinaryOperator::And),
            scanner::TokenType::Or => Ok(LoxBinaryOperator::Or),
            _ => Err(ParserError::UnexpectedTokenNoExpected {
                token_type: value.token_type,
                line: value.line,
                column: value.column,
            }),
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
            _ => Err(ParserError::UnexpectedTokenNoExpected {
                token_type: value.token_type,
                line: value.line,
                column: value.column,
            }),
        }
    }
}
