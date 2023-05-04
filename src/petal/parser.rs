use std::{collections::HashMap, rc::Rc};

use super::{
    ast::Expr,
    lexer::Lexer,
    token::{Literal, Token, TokenPos, TokenRange, TokenType},
};

#[derive(PartialOrd, PartialEq, Clone, Debug)]
pub enum Precedence {
    Lowest,
    Assign,     // =
    LogicalOr,  // ||
    LogicalAnd, // &&
    Equality,   // ==, !=
    Comparison, // <, >, <=, >=
    Term,       // +, -
    Factor,     // *, /
    Unary,      // !, -
    Call,       // my_function()
    Highest,
}

impl Precedence {
    fn next(self) -> Self {
        use Precedence::*;
        match self {
            Lowest => Assign,
            Assign => LogicalOr,
            LogicalOr => LogicalAnd,
            LogicalAnd => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Highest,
            Highest => Highest,
        }
    }
}

pub trait PrefixParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<Expr>;
}

pub trait InfixParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, token: Token) -> ParserResult<Expr>;
    fn precedence(&self) -> Precedence;
}

#[derive(Debug, Clone)]
pub struct ParserError {
    pub range: Option<TokenRange>,
    pub msg: String,
}

impl ParserError {
    pub fn new(msg: String) -> ParserError {
        ParserError { range: None, msg }
    }

    pub fn with_range(self, range: TokenRange) -> ParserError {
        let mut e = self;
        e.range = Some(range);
        e
    }
}

type ParserResult<T> = Result<T, ParserError>;

struct IntegerParselet;
impl PrefixParselet for IntegerParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> ParserResult<Expr> {
        match token.literal {
            Some(Literal::Number(value)) => Ok(Expr::Number(value)),
            _ => Err(
                ParserError::new(format!("Expected a number, but found {}", token))
                    .with_range(token.clone().range),
            ),
        }
    }
}

struct GroupParseLet;
impl PrefixParselet for GroupParseLet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<Expr> {
        let expr = parser.parse_expression(Precedence::Lowest)?;
        parser.expect_token(TokenType::RightParen)?;
        Ok(expr)
    }
}

pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    current_token: Option<Token>,
    prefix_parselets: HashMap<TokenType, Rc<dyn PrefixParselet>>,
    infix_parselets: HashMap<TokenType, Rc<dyn InfixParselet>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Parser<'a> {
        let mut parser = Parser {
            lexer,
            current_token: None,
            prefix_parselets: HashMap::new(),
            infix_parselets: HashMap::new(),
        };

        // Register prefix parselets
        parser
            .prefix_parselets
            .insert(TokenType::Number, Rc::new(IntegerParselet));
        parser
            .prefix_parselets
            .insert(TokenType::LeftParen, Rc::new(GroupParseLet));

        parser
    }

    pub fn parse(&mut self) -> ParserResult<Expr> {
        self.parse_expression(Precedence::Lowest)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParserResult<Expr> {
        let token = self
            .consume()?
            .ok_or_else(|| ParserError::new("Unexpected EOF".to_string()))?;

        let prefix_parselet = self
            .prefix_parselets
            .get(&token.token_type)
            .ok_or_else(|| {
                ParserError::new(format!(
                    "No prefix parselet found for token {}",
                    token.clone()
                ))
                .with_range(token.clone().range)
            })?
            .clone();

        let mut left = prefix_parselet.parse(self, token)?;

        while precedence < self.get_precedence() {
            let token = self.consume()?.ok_or_else(|| {
                ParserError::new("Unexpected EOF. Expected infix operator".to_string())
            })?;
            let infix_parselet = self
                .infix_parselets
                .get(&token.token_type)
                .ok_or_else(|| {
                    ParserError::new(format!(
                        "No infix parselet found for token {}",
                        token.clone()
                    ))
                    .with_range(token.clone().range)
                })?
                .clone();

            left = infix_parselet.parse(self, left, token)?;
        }

        Ok(left)
    }

    fn get_precedence(&self) -> Precedence {
        if let Some(token) = &self.current_token {
            self.infix_parselets
                .get(&token.token_type)
                .map_or(Precedence::Lowest, |parselet| parselet.precedence())
        } else {
            Precedence::Lowest
        }
    }

    fn consume(&mut self) -> ParserResult<Option<Token>> {
        match self.lexer.next() {
            Some(Ok(token)) => {
                self.current_token = Some(token.clone());
                Ok(Some(token))
            }
            Some(Err(e)) => Err(ParserError::new(e.msg).with_range((e.pos.clone(), e.pos))),
            None => Ok(None),
        }
    }

    fn expect_token(&mut self, token_type: TokenType) -> ParserResult<Token> {
        if let Some(token) = self.consume()? {
            if token.token_type == token_type {
                return Ok(token.clone());
            }

            return Err(ParserError::new(format!(
                "Expected token {:?}, but found {:?}",
                token_type, self.current_token
            ))
            .with_range(token.range));
        }

        Err(ParserError::new(format!(
            "Expected token {:?}, but found {:?}",
            token_type, self.current_token
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(s: String) -> Expr {
        let mut lexer = Lexer::new(&s);
        let mut parser = Parser::new(&mut lexer);
        parser.parse().unwrap()
    }

    #[test]
    fn test_literals() {
        let expr = parse("1".to_string());
        insta::assert_debug_snapshot!(expr);
    }

    #[test]
    fn test_groups() {
        let expr = parse("(1)".to_string());
        insta::assert_debug_snapshot!(expr);
    }
}
