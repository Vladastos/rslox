// TODO:
//  - Add more error types
//  - Implement error recovery (Synchronization) - https://craftinginterpreters.com/parsing-expressions.html#panic-mode-error-recovery

use log::trace;

use crate::rslox::scanner;

use super::ParserError;

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

    pub fn parse(&mut self) -> Result<Expr, Vec<ParserError>> {
        trace!("Parsing tokens: {:#?}", self.tokens);
        return self.parse_expression();
    }

    fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        return self.parse_equality();
    }

    fn parse_equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_comparison()?;
        while let Some(operator) = self.match_tokens(vec![
            scanner::TokenType::BangEqual,
            scanner::TokenType::EqualEqual,
        ]) {
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
        while let Some(operator) = self.match_tokens(vec![
            scanner::TokenType::Greater,
            scanner::TokenType::GreaterEqual,
            scanner::TokenType::Less,
            scanner::TokenType::LessEqual,
        ]) {
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
        while let Some(operator) =
            self.match_tokens(vec![scanner::TokenType::Plus, scanner::TokenType::Minus])
        {
            let right = self.parse_factor()?;
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
        while let Some(operator) =
            self.match_tokens(vec![scanner::TokenType::Star, scanner::TokenType::Slash])
        {
            let right = self.parse_unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        return Ok(expr);
    }

    fn parse_unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(operator) =
            self.match_tokens(vec![scanner::TokenType::Bang, scanner::TokenType::Minus])
        {
            let right = self.parse_unary()?;

            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }
        return self.parse_primary();
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        if self.match_token(scanner::TokenType::False).is_some() {
            return Ok(Expr::Literal {
                value: String::from("false"),
            });
        }
        if self.match_token(scanner::TokenType::True).is_some() {
            return Ok(Expr::Literal {
                value: String::from("true"),
            });
        }
        if self.match_token(scanner::TokenType::Nil).is_some() {
            return Ok(Expr::Literal {
                value: String::from("nil"),
            });
        }
        if let Some(token) = self.match_token(scanner::TokenType::Number) {
            return Ok(Expr::Literal {
                value: token.literal.unwrap(),
            });
        }
        if let Some(token) = self.match_token(scanner::TokenType::String) {
            return Ok(Expr::Literal {
                value: token.literal.unwrap(),
            });
        }

        if self.match_token(scanner::TokenType::LeftParen).is_some() {
            let expr = self.parse_expression()?;
            self.expect_token(scanner::TokenType::RightParen)?;
            return Ok(Expr::Grouping {
                expression: Box::new(expr),
            });
        }

        return Err(ParserError::UnexpectedTokenNoExpected(
            self.peek().token_type,
            self.peek().line,
        ));
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

    fn expect_token(&mut self, token_type: scanner::TokenType) -> Result<(), ParserError> {
        if self.peek().token_type == token_type {
            self.advance();
            return Ok(());
        }
        return Err(ParserError::UnexpectedToken(
            self.peek().clone().token_type,
            token_type,
            self.peek().line,
        ));
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
        return Some(token);
    }

    fn match_tokens(&mut self, tokens: Vec<scanner::TokenType>) -> Option<scanner::Token> {
        if self.is_at_end() {
            return None;
        }
        for token_type in tokens {
            if self.peek().token_type == token_type {
                return Some(self.advance());
            }
        }
        return None;
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
