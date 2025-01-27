use log::trace;
use thiserror::Error;

use crate::rslox::scanner;

//
// Parser
//

pub struct Parser {
    tokens: Vec<scanner::Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<scanner::Token>) -> Parser {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Expr, ParserError> {
        trace!("Parsing tokens: {:#?}", self.tokens);
        return self.parse_expression();
    }

    fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        return self.parse_equality();
    }

    fn parse_equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_comparison()?;
        while self.match_token(scanner::TokenType::BangEqual)
            || self.match_token(scanner::TokenType::EqualEqual)
        {
            let operator_result = self.previous();
            if operator_result.is_err() {
                return Err(ParserError::InternalError(
                    "Could not get operator".to_string(),
                ));
            }
            let operator = operator_result.unwrap();

            let right = self.parse_comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        return Ok(expr);
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_term()?;
        while self.match_token(scanner::TokenType::Greater)
            || self.match_token(scanner::TokenType::GreaterEqual)
            || self.match_token(scanner::TokenType::Less)
            || self.match_token(scanner::TokenType::LessEqual)
        {
            let operator = self.previous().unwrap();
            let right = self.parse_term().unwrap();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        return Ok(expr);
    }

    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_factor()?;
        while self.match_token(scanner::TokenType::Minus)
            || self.match_token(scanner::TokenType::Plus)
        {
            let operator = self.previous().unwrap();
            let right = self.parse_factor().unwrap();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        return Ok(expr);
    }

    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_unary()?;
        while self.match_token(scanner::TokenType::Slash)
            || self.match_token(scanner::TokenType::Star)
        {
            let operator = self.previous().unwrap();
            let right = self.parse_unary().unwrap();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        return Ok(expr);
    }

    fn parse_unary(&mut self) -> Result<Expr, ParserError> {
        if self.match_token(scanner::TokenType::Bang) || self.match_token(scanner::TokenType::Minus)
        {
            let operator = self.previous().unwrap();
            let right = self.parse_unary().unwrap();
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }
        return self.parse_primary();
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        if self.match_token(scanner::TokenType::False) {
            return Ok(Expr::Literal {
                value: String::from("false"),
            });
        }
        if self.match_token(scanner::TokenType::True) {
            return Ok(Expr::Literal {
                value: String::from("true"),
            });
        }
        if self.match_token(scanner::TokenType::Nil) {
            return Ok(Expr::Literal {
                value: String::from("nil"),
            });
        }
        if self.match_token(scanner::TokenType::Number) {
            return Ok(Expr::Literal {
                value: self.previous().unwrap().literal.unwrap(),
            });
        }
        if self.match_token(scanner::TokenType::String) {
            return Ok(Expr::Literal {
                value: self.previous().unwrap().literal.unwrap(),
            });
        }

        return Err(ParserError::InternalError(format!(
            "Could not parse token : {} ",
            self.peek()
        )));
    }
    // Utils
    fn advance(&mut self) -> scanner::Token {
        self.current += 1;
        self.tokens[self.current - 1].clone()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> scanner::Token {
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> Result<scanner::Token, ()> {
        if self.current == 0 {
            return Err(());
        }
        Ok(self.tokens[self.current - 1].clone())
    }

    fn match_token(&mut self, token_type: scanner::TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek().token_type != token_type {
            return false;
        }
        self.advance();
        return true;
    }
}

//
// Expression
//

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: scanner::Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: String,
    },
    Unary {
        operator: scanner::Token,
        right: Box<Expr>,
    },
}

//
// Error
//

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Unexpected token: {0}")]
    UnexpectedToken(scanner::Token),
    #[error("Internal error: {0}")]
    InternalError(String),
}
