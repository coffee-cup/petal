use std::{collections::HashMap, rc::Rc};
use thiserror::Error;

use super::{
    ast::Expr,
    errors::CompilerError,
    lexer::{Lexer, LexerErrorKind},
    positions::{HasSpan, Pos, Span},
    token::{Literal, Token, TokenType},
};

type TT = TokenType;

#[derive(Error, Clone, Debug)]
pub enum ParserErrorKind {
    #[error("{0}")]
    LexerError(#[from] LexerErrorKind),

    #[error("Unexpected token {0}")]
    UnexpectedToken(Token),
}

#[derive(Debug, Clone)]
pub struct ParserError {
    pub span: Option<Span>,
    pub kind: ParserErrorKind,
}

impl ParserError {
    pub fn new(kind: ParserErrorKind) -> Self {
        ParserError { span: None, kind }
    }

    pub fn with_span(&self, span: Span) -> Self {
        ParserError {
            span: Some(span),
            ..self.clone()
        }
    }
}

impl CompilerError for ParserError {
    fn span(&self) -> Option<Span> {
        self.span.clone()
    }

    fn msg(&self) -> String {
        self.kind.to_string()
    }
}

type ParserResult<T> = Result<T, ParserError>;

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

trait PrefixParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<Expr>;
}

trait InfixParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, token: Token) -> ParserResult<Expr>;
    fn precedence(&self) -> Precedence;
}

macro_rules! register {
    ($map:expr, $token_type:path, $parselet:ident) => {
        $map.insert($token_type, std::rc::Rc::new($parselet))
    };
}

struct NumberParselet;
impl PrefixParselet for NumberParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> ParserResult<Expr> {
        match token.literal {
            Some(Literal::Number(value)) => Ok(Expr::Number {
                value,
                span: token.span,
            }),
            _ => Err(ParserError::new(ParserErrorKind::UnexpectedToken(token))),
        }
    }
}

struct PrefixOperatorParselet;
impl PrefixParselet for PrefixOperatorParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<Expr> {
        let operand = parser.parse_expression()?;
        Ok(Expr::PrefixOp {
            op: token.clone(),
            right: Box::new(operand),
            span: token.span,
        })
    }
}

struct BinaryOperatorParselet;
impl InfixParselet for BinaryOperatorParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, token: Token) -> ParserResult<Expr> {
        let right = parser.parse_expression()?;
        let span = left.span().clone().merge(right.span().clone());

        Ok(Expr::BinaryOp {
            left: Box::new(left),
            op: token,
            right: Box::new(right),
            span,
        })
    }

    fn precedence(&self) -> Precedence {
        todo!()
    }
}

pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    current_token: Token,
    next_token: Token,
    prefix_parselets: HashMap<TokenType, Rc<dyn PrefixParselet>>,
    infix_parselets: HashMap<TokenType, Rc<dyn InfixParselet>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> Parser<'a> {
        let mut parser = Parser {
            lexer,
            current_token: Token::new(TT::Eof),
            next_token: Token::new(TT::Eof),
            prefix_parselets: HashMap::new(),
            infix_parselets: HashMap::new(),
        };

        // Prime the next_token
        parser.consume().unwrap();

        // Prefix parselets
        register!(parser.prefix_parselets, TT::Number, NumberParselet);

        // Infix parselets
        register!(parser.infix_parselets, TT::Plus, BinaryOperatorParselet);
        register!(parser.infix_parselets, TT::Minus, BinaryOperatorParselet);
        register!(parser.infix_parselets, TT::Star, BinaryOperatorParselet);
        register!(parser.infix_parselets, TT::Slash, BinaryOperatorParselet);

        parser
    }

    pub fn parse(&mut self) -> ParserResult<Expr> {
        self.parse_expression()
    }

    fn parse_expression(&mut self) -> ParserResult<Expr> {
        println!("\n\n--- Parse expression ---");
        let token = self.consume()?;

        println!("Token: {}", token);

        let prefix_parselet = self
            .prefix_parselets
            .get(&token.token_type)
            .ok_or_else(|| {
                ParserError::new(ParserErrorKind::UnexpectedToken(token.clone()))
                    .with_span(token.span.clone())
            })?
            .clone();

        println!("Found prefix parselet");

        let mut left = prefix_parselet.parse(self, token.clone())?;

        println!("\nLeft: {:?}", left);

        let token = self.next_token.clone();
        println!("Next token: {}", token);

        let infix_parselet = self
            .infix_parselets
            .get(&token.token_type)
            .map(|x| x.clone());

        if let Some(infix_parselet) = infix_parselet.clone() {
            println!("Found infix parselet");
            self.consume()?;
            left = infix_parselet.clone().parse(self, left, token.clone())?;
        }

        println!("\nReturning: {:?}", left);

        Ok(left)
    }

    fn consume(&mut self) -> ParserResult<Token> {
        self.current_token = self.next_token.clone();
        self.next_token = self
            .lexer
            .next()
            .unwrap_or_else(|| Ok(Token::new(TT::Eof)))
            .map_err(|lexer_error| {
                let mut pe = ParserError::new(ParserErrorKind::LexerError(lexer_error.kind));
                pe.span = lexer_error.span;
                pe
            })?;

        Ok(self.current_token.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // fn parse(s: String) -> Expr {
    //     let mut lexer = Lexer::new(&s);
    //     let mut parser = Parser::new(&mut lexer);
    //     parser.parse().unwrap()
    // }

    // #[test]
    // fn test_literals() {
    //     let expr = parse("1".to_string());
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_groups() {
    //     let expr = parse("(1)".to_string());
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_math() {
    //     let expr = parse("1 + 2 * 3".to_string());
    //     insta::assert_debug_snapshot!(expr);

    //     assert_eq!(1, 2);
    // }
}
