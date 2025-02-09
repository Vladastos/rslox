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

    /// Parses a declaration.
    ///
    /// This is the entry point for parsing a declaration. It will parse either a variable declaration or a statement.
    /// Expects a semicolon at the end.
    fn parse_declaration(&mut self) -> Result<Stmt, ParserError> {
        let result = match self.peek().token_type {
            scanner::TokenType::Var => self.parse_var_declaration()?,
            scanner::TokenType::Fun => self.parse_fun_declaration()?,
            _ => self.parse_statement()?,
        };
        self.expect_token(scanner::TokenType::Semicolon)?;
        Ok(result)
    }

    /// Parses a variable declaration.
    ///
    /// Expects the current token to be the 'var' keyword.
    /// Parses the variable name and optional initializer.
    ///
    /// Returns a `Stmt::VarDeclaration` with the parsed variable name and initializer.
    fn parse_var_declaration(&mut self) -> Result<Stmt, ParserError> {
        self.expect_token(scanner::TokenType::Var)?;
        let name = self.expect_token(scanner::TokenType::Identifier)?.lexeme;

        let initializer = if self.check(scanner::TokenType::Equal) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };

        Ok(Stmt::VarDeclaration { name, initializer })
    }

    /// Parses a function declaration.
    ///
    /// Expects the current token to be the 'fun' keyword.
    /// Parses the function name, parameters, and body.
    ///
    /// Returns a `Stmt::Function` with the parsed function name, parameters, and body.
    fn parse_fun_declaration(&mut self) -> Result<Stmt, ParserError> {
        self.expect_token(scanner::TokenType::Fun)?;
        let name = self.expect_token(scanner::TokenType::Identifier)?.lexeme;
        self.expect_token(scanner::TokenType::LeftParen)?;
        let params = self.parse_fun_params()?;
        self.expect_token(scanner::TokenType::RightParen)?;
        let body = Box::from(self.parse_block_statement()?);
        Ok(Stmt::Function { name, params, body })
    }

    /// Parses function parameters.
    ///
    /// Assumes the current token to be a '(' and expects the next token to be a ')'.
    /// If the next token is not a ')', then this function parses a comma-separated list of identifiers,
    /// and returns the list of identifiers.
    /// If the next token is a ')', then this function returns an empty vector.
    ///
    fn parse_fun_params(&mut self) -> Result<Vec<String>, ParserError> {
        let mut params: Vec<String> = Vec::new();
        if !self.check(scanner::TokenType::RightParen) {
            params.push(
                self.expect_token(scanner::TokenType::Identifier)
                    .unwrap()
                    .lexeme,
            );
            while self.check(scanner::TokenType::Comma) {
                self.advance();
                params.push(self.expect_token(scanner::TokenType::Identifier)?.lexeme);
            }
        }
        Ok(params)
    }

    /// Parses a statement.
    ///
    /// This is the entry point for parsing a statement. It will parse one of the following:
    ///   - A print statement
    ///   - A block statement
    ///   - An if statement
    ///   - A while statement
    ///   - A for statement
    ///   - A return statement
    ///   - An expression statement
    ///
    /// Returns a `Stmt` with the parsed statement.
    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        let result = match self.peek().token_type {
            scanner::TokenType::Print => self.parse_print_statement(),
            scanner::TokenType::LeftBrace => self.parse_block_statement(),
            scanner::TokenType::If => self.parse_if_statement(),
            scanner::TokenType::While => self.parse_while_statement(),
            scanner::TokenType::For => self.parse_for_statement(),
            scanner::TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        };
        result
    }

    /// Parses an if statement.
    ///
    /// This function assumes that the current token is an 'if' keyword and advances the token stream.
    /// It then parses the condition enclosed in parentheses, followed by the then-branch statement.
    /// If an 'else' keyword is present, it also parses the else-branch statement.
    ///
    /// Returns a `Stmt::If` node containing the parsed condition, then-branch, and optional else-branch.
    /// Returns an error if the condition or branches cannot be parsed.

    fn parse_if_statement(&mut self) -> Result<Stmt, ParserError> {
        self.advance();
        self.expect_token(scanner::TokenType::LeftParen)?;
        let condition = self.parse_expression()?;
        self.expect_token(scanner::TokenType::RightParen)?;
        let then_branch = Box::from(self.parse_statement()?);
        let else_branch = if self.match_token(scanner::TokenType::Else).is_some() {
            Some(Box::from(self.parse_statement()?))
        } else {
            None
        };
        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    /// Parses a while statement.
    ///
    /// This function assumes that the current token is a 'while' keyword and advances the token stream.
    /// It then parses the condition enclosed in parentheses, followed by the loop body statement.
    ///
    /// Returns a `Stmt::While` node containing the parsed condition and body.
    /// Returns an error if the condition or body cannot be parsed.
    fn parse_while_statement(&mut self) -> Result<Stmt, ParserError> {
        self.expect_token(scanner::TokenType::While)?;
        self.expect_token(scanner::TokenType::LeftParen)?;
        let condition = self.parse_expression()?;
        self.expect_token(scanner::TokenType::RightParen)?;
        let body = Box::from(self.parse_statement()?);
        Ok(Stmt::While { condition, body })
    }

    /// Parses a for statement.
    ///
    /// This function assumes that the current token is a 'for' keyword and advances the token stream.
    /// It then parses the initializer, condition, and increment parts of the for loop.
    /// The initializer is either a variable declaration or an expression statement.
    /// The condition is an expression, or the value true if not present.
    /// The increment is an expression, or the value nil if not present.
    ///
    /// Returns a `Stmt::Block` node containing a while loop statement with the parsed parts.
    /// Returns an error if any of the parts cannot be parsed.
    fn parse_for_statement(&mut self) -> Result<Stmt, ParserError> {
        self.expect_token(scanner::TokenType::For)?;
        self.expect_token(scanner::TokenType::LeftParen)?;

        let initializer = if self.match_token(scanner::TokenType::Semicolon).is_some() {
            None
        } else if self.check(scanner::TokenType::Var) {
            Some(Box::from(self.parse_var_declaration()?))
        } else {
            Some(Box::from(self.parse_expression_statement()?))
        };
        if initializer.is_some() {
            self.expect_token(scanner::TokenType::Semicolon)?;
        }
        let condition = if self.match_token(scanner::TokenType::Semicolon).is_some() {
            None
        } else {
            Some(self.parse_expression()?)
        };

        if condition.is_some() {
            self.expect_token(scanner::TokenType::Semicolon)?;
        }
        let increment = if self.match_token(scanner::TokenType::RightParen).is_some() {
            None
        } else {
            Some(self.parse_expression()?)
        };
        if increment.is_some() {
            self.expect_token(scanner::TokenType::RightParen)?;
        }
        let body = Box::from(Stmt::Block {
            statements: vec![
                self.parse_statement()?,
                Stmt::Expression {
                    expression: increment.unwrap_or(Expr::Literal {
                        value: LoxParserValue::Nil,
                    }),
                },
            ],
        });

        return Ok(Stmt::Block {
            statements: vec![
                *initializer.unwrap_or(Box::from(Stmt::Expression {
                    expression: Expr::Literal {
                        value: LoxParserValue::Nil,
                    },
                })),
                Stmt::While {
                    condition: condition.unwrap_or(Expr::Literal {
                        value: LoxParserValue::Boolean(true),
                    }),
                    body,
                },
            ],
        });
    }

    /// Parses a return statement.
    ///
    /// This function assumes that the current token is a 'return' keyword and advances the token stream.
    /// It then parses an optional expression, and constructs a `Stmt::Return` node containing the parsed expression.
    /// Returns an error if the expression cannot be parsed.
    fn parse_return_statement(&mut self) -> Result<Stmt, ParserError> {
        self.expect_token(scanner::TokenType::Return)?;
        let value = if self.check(scanner::TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        Ok(Stmt::Return { value })
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
        while !self.check(scanner::TokenType::RightBrace) {
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
    /// Does not expect a semicolon at the end.
    fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        self.parse_assignment()
    }

    /// Parses an assignment expression.
    ///
    /// This function parses a logical expression and checks if the next token is an equal sign.
    /// If it is, it parses the following expression and constructs an assignment expression.
    /// Otherwise, it returns the parsed logical expression.
    ///
    /// Returns an error if the expression cannot be parsed, or if the assignment target is invalid.
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

    /// Parses a logical or expression.
    ///
    /// This function parses a logical and expression and checks if the next token is an 'or' keyword.
    /// If it is, it parses the following logical and expression and constructs a binary expression node
    /// representing the logical or. Otherwise, it returns the parsed logical and expression.
    ///
    /// Returns an error if the expression cannot be parsed.
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

    /// Parses a logical and expression.
    ///
    /// This function parses a comparison expression and checks if the next token is an 'and' keyword.
    /// If it is, it parses the following comparison expression and constructs a binary expression node
    /// representing the logical and. Otherwise, it returns the parsed comparison expression.
    ///
    /// Returns an error if the expression cannot be parsed.
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
        self.parse_call()
    }

    /// Parses a call expression.
    ///
    /// This is either a primary expression, or a call expression.
    fn parse_call(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_primary()?;
        if self.match_token(scanner::TokenType::LeftParen).is_some() {
            let mut arguments = Vec::new();
            if !self.check(scanner::TokenType::RightParen) {
                arguments.push(self.parse_expression()?);
                while self.match_token(scanner::TokenType::Comma).is_some() {
                    arguments.push(self.parse_expression()?);
                }
            }
            self.expect_token(scanner::TokenType::RightParen)?;
            if arguments.len() > 255 {
                return Err(ParserError::TooManyArguments {
                    line: self.peek().line,
                    column: self.peek().column,
                });
            }
            return Ok(Expr::Call {
                callee: Box::new(expr),
                arguments,
            });
        }
        Ok(expr)
    }

    /// Parses a primary expression.
    ///
    /// This can be one of the following:
    ///   - A literal value
    ///   - A variable
    ///   - A grouping expression (i.e. a parenthesized expression)
    ///
    /// If the current token is not one of the above, returns an error.
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

    /// Returns true if the current token matches the given `token_type`, false otherwise. Does not consume the token.
    fn check(&self, token_type: scanner::TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == token_type
    }
    /// Consumes the current token if it matches the given `token_type` and returns it
    /// Returns an error if the current token does not match the given `token_type`.
    fn expect_token(&mut self, token_type: scanner::TokenType) -> Result<Token, ParserError> {
        if self.peek().token_type == token_type {
            let token = self.peek().clone();
            self.advance();
            return Ok(token);
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
            if self.check(scanner::TokenType::Semicolon) {
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
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Function {
        name: String,
        params: Vec<String>,
        body: Box<Stmt>,
    },
    Return {
        value: Option<Expr>,
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
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
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
