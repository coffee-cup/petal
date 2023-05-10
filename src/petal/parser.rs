use std::{collections::HashMap, rc::Rc};
use thiserror::Error;

use super::{
    ast::{Decl, Expr, Stmt},
    errors::CompilerError,
    lexer::{Lexer, LexerErrorKind},
    positions::{HasSpan, Span},
    precedence::Precedence,
    token::{Literal, Token, TokenType},
};

type TT = TokenType;

#[derive(Error, Clone, Debug)]
pub enum ParserErrorKind {
    #[error("{0}")]
    LexerError(#[from] LexerErrorKind),

    #[error("Unexpected token {0}")]
    UnexpectedToken(String),

    #[error("Expected {0}, found {1}")]
    ExpectedToken(TokenType, String),
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

trait PrefixParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<Expr>;
}

trait InfixParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, token: Token) -> ParserResult<Expr>;
    fn precedence(&self) -> Precedence;
}

struct NumberParselet;
impl PrefixParselet for NumberParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> ParserResult<Expr> {
        match token.literal {
            Some(Literal::Number(value)) => Ok(Expr::Number {
                value,
                span: token.span,
            }),
            _ => Err(ParserError::new(ParserErrorKind::UnexpectedToken(
                token.to_string(),
            ))),
        }
    }
}

struct StringParselet;
impl PrefixParselet for StringParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> ParserResult<Expr> {
        match token.literal {
            Some(Literal::String(value)) => Ok(Expr::String {
                value,
                span: token.span,
            }),
            _ => Err(ParserError::new(ParserErrorKind::UnexpectedToken(
                token.to_string(),
            ))),
        }
    }
}

struct IdentParselet;
impl PrefixParselet for IdentParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> ParserResult<Expr> {
        match token.literal {
            Some(Literal::Identifier(name)) => Ok(Expr::Ident {
                name,
                span: token.span,
            }),
            _ => Err(ParserError::new(ParserErrorKind::UnexpectedToken(
                token.to_string(),
            ))),
        }
    }
}

struct CommentParselet;
impl PrefixParselet for CommentParselet {
    fn parse(&self, _parser: &mut Parser, token: Token) -> ParserResult<Expr> {
        match token.literal {
            Some(Literal::Comment(value)) => Ok(Expr::Comment {
                value,
                span: token.span,
            }),
            _ => Err(ParserError::new(ParserErrorKind::UnexpectedToken(
                token.to_string(),
            ))),
        }
    }
}

struct GroupParselet;
impl PrefixParselet for GroupParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> ParserResult<Expr> {
        let expr = parser.parse_expression(Precedence::Lowest)?;
        parser.consume_expected(TT::RightParen)?;
        Ok(expr)
    }
}

struct PrefixOperatorParselet {
    precedence: Precedence,
}
impl PrefixParselet for PrefixOperatorParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<Expr> {
        let operand = parser.parse_expression(self.precedence.clone())?;
        let span = token.span().merge(operand.span());

        Ok(Expr::PrefixOp {
            op: token,
            right: Box::new(operand),
            span,
        })
    }
}

struct CallParselet;
impl InfixParselet for CallParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, _token: Token) -> ParserResult<Expr> {
        let mut args: Vec<Expr> = Vec::new();
        let mut span = left.span().clone();

        loop {
            if parser.peek().token_type == TT::RightParen {
                break;
            }

            let arg = parser.parse_expression(Precedence::Lowest)?;
            span = span.merge(arg.span());

            args.push(arg);
            parser.match_expected(TT::Comma)?;
        }

        parser.consume_expected(TT::RightParen)?;

        Ok(Expr::Call {
            callee: Box::new(left),
            args,
            span,
        })
    }

    fn precedence(&self) -> Precedence {
        Precedence::Call
    }
}

struct BinaryOperatorParselet {
    precedence: Precedence,
    is_right: bool,
}
impl InfixParselet for BinaryOperatorParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, token: Token) -> ParserResult<Expr> {
        let parse_right_prec = if self.is_right {
            self.precedence().prev()
        } else {
            self.precedence()
        };

        let right = parser.parse_expression(parse_right_prec)?;
        let span = left.span().merge(right.span());

        Ok(Expr::BinaryOp {
            left: Box::new(left),
            op: token,
            right: Box::new(right),
            span,
        })
    }

    fn precedence(&self) -> Precedence {
        self.precedence.clone()
    }
}

struct PostfixOperatorParselet {
    precedence: Precedence,
}
impl PrefixParselet for PostfixOperatorParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<Expr> {
        let operand = parser.parse_expression(self.precedence.clone())?;
        let span = token.span.merge(operand.span());

        Ok(Expr::PostfixOp {
            op: token,
            left: Box::new(operand),
            span,
        })
    }
}

struct ConditionalParselet;
impl InfixParselet for ConditionalParselet {
    fn parse(&self, parser: &mut Parser, left: Expr, _token: Token) -> ParserResult<Expr> {
        let then_branch = parser.parse_expression(Precedence::Lowest)?;
        parser.consume_expected(TT::Colon)?;
        let else_branch = parser.parse_expression(self.precedence().prev())?;
        let span = left.span().merge(else_branch.span());

        Ok(Expr::Conditional {
            condition: Box::new(left),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
            span,
        })
    }

    fn precedence(&self) -> Precedence {
        Precedence::Conditional
    }
}

macro_rules! register {
    ($map:expr, $token_type:path, $parselet:ident) => {
        $map.insert($token_type, std::rc::Rc::new($parselet))
    };
}

macro_rules! prefix {
    ($map:expr, $token_type:path, $precedence:expr) => {
        $map.insert(
            $token_type,
            std::rc::Rc::new(PrefixOperatorParselet {
                precedence: $precedence,
            }),
        )
    };
}

macro_rules! infix_left {
    ($map:expr, $token_type:path, $precedence:expr) => {
        $map.insert(
            $token_type,
            std::rc::Rc::new(BinaryOperatorParselet {
                precedence: $precedence,
                is_right: false,
            }),
        )
    };
}

macro_rules! infix_right {
    ($map:expr, $token_type:path, $precedence:expr) => {
        $map.insert(
            $token_type,
            std::rc::Rc::new(BinaryOperatorParselet {
                precedence: $precedence,
                is_right: true,
            }),
        )
    };
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

        // Custom parselets
        register!(parser.prefix_parselets, TT::Number, NumberParselet);
        register!(parser.prefix_parselets, TT::String, StringParselet);
        register!(parser.prefix_parselets, TT::Identifier, IdentParselet);
        register!(parser.prefix_parselets, TT::Comment, CommentParselet);
        register!(parser.prefix_parselets, TT::LeftParen, GroupParselet);
        register!(
            parser.infix_parselets,
            TT::QuestionMark,
            ConditionalParselet
        );
        register!(parser.infix_parselets, TT::LeftParen, CallParselet);

        // Prefix parselets
        prefix!(parser.prefix_parselets, TT::Minus, Precedence::Unary);
        prefix!(parser.prefix_parselets, TT::Bang, Precedence::Unary);

        // Infix parselets
        infix_left!(parser.infix_parselets, TT::Minus, Precedence::Sum);
        infix_left!(parser.infix_parselets, TT::Plus, Precedence::Sum);
        infix_left!(parser.infix_parselets, TT::Star, Precedence::Product);
        infix_left!(parser.infix_parselets, TT::Slash, Precedence::Product);
        infix_right!(parser.infix_parselets, TT::Caret, Precedence::Exponent);

        parser
    }

    pub fn parse(&mut self) -> ParserResult<Stmt> {
        let result = self.parse_declaration()?;
        self.consume_expected(TT::Eof)?;
        Ok(result)
    }

    fn parse_declaration(&mut self) -> ParserResult<Stmt> {
        let stmt = match self.peek().token_type {
            TT::Let => self.parse_let_declaration()?,
            _ => self.parse_statement()?,
        };

        Ok(stmt)
    }

    fn parse_let_declaration(&mut self) -> ParserResult<Stmt> {
        let token = self.consume()?;
        let mut span = token.span().clone();

        let name = match self.consume_expected(TT::Identifier)?.literal {
            Some(Literal::Identifier(name)) => name,
            _ => unreachable!(),
        };

        self.match_expected(TT::Equal)?;
        let init = self.parse_expression(Precedence::Lowest)?;

        span = span.merge(init.span().clone());

        Ok(Stmt::Let { name, init, span })
    }

    fn parse_statement(&mut self) -> ParserResult<Stmt> {
        let stmt = match self.peek().token_type {
            TT::If => self.parse_if_statement()?,
            _ => {
                let expr = self.parse_expression(Precedence::Lowest)?;
                let span = expr.span().clone();

                // TODO: consume newline or semicolon?
                Stmt::ExprStmt {
                    expr: Box::new(expr),
                    span,
                }
            }
        };

        Ok(stmt)
    }

    fn parse_if_statement(&mut self) -> ParserResult<Stmt> {
        let token = self.consume()?;
        let mut span = token.span().clone();

        let condition = self.parse_expression(Precedence::Lowest)?;
        self.consume_expected(TT::LeftBrace)?;

        let then_block = self.parse_statement()?;
        self.consume_expected(TT::RightBrace)?;
        span = span.merge(then_block.span());

        let else_block = if self.match_expected(TT::Else)? {
            self.consume_expected(TT::LeftBrace)?;
            let else_block = self.parse_statement()?;
            self.consume_expected(TT::RightBrace)?;
            span = span.merge(else_block.span());
            Some(Box::new(else_block))
        } else {
            None
        };

        Ok(Stmt::IfStmt {
            condition: condition,
            then_block: Box::new(then_block),
            else_block,
            span,
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParserResult<Expr> {
        let token = self.consume()?;

        let prefix_parselet = self
            .prefix_parselets
            .get(&token.token_type)
            .ok_or_else(|| {
                ParserError::new(ParserErrorKind::UnexpectedToken(token.to_string()))
                    .with_span(token.span.clone())
            })?
            .clone();

        let mut left = prefix_parselet.parse(self, token.clone())?;

        while precedence < self.get_precedence() {
            let token = self.consume()?;

            let infix_parselet = self
                .infix_parselets
                .get(&token.token_type)
                .ok_or_else(|| {
                    ParserError::new(ParserErrorKind::UnexpectedToken(token.to_string()))
                        .with_span(token.span.clone())
                })?
                .clone();

            left = infix_parselet.parse(self, left, token.clone())?;
        }

        Ok(left)
    }

    fn get_precedence(&self) -> Precedence {
        self.infix_parselets
            .get(&self.next_token.token_type)
            .map(|p| p.precedence())
            .unwrap_or(Precedence::Lowest)
    }

    fn peek(&self) -> &Token {
        &self.next_token
    }

    fn match_expected(&mut self, token_type: TokenType) -> ParserResult<bool> {
        if self.next_token.token_type == token_type {
            self.consume()?;
            return Ok(true);
        }

        Ok(false)
    }

    fn consume_expected(&mut self, token_type: TokenType) -> ParserResult<Token> {
        if self.next_token.token_type != token_type {
            return Err(ParserError::new(ParserErrorKind::ExpectedToken(
                token_type,
                self.next_token.to_string(),
            ))
            .with_span(self.next_token.span.clone()));
        }

        self.consume()
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

    fn parse_stmt(s: &str) -> Stmt {
        let mut lexer = Lexer::new(s);
        let mut parser = Parser::new(&mut lexer);
        parser.parse_declaration().unwrap()
    }

    fn parse_expr(s: &str) -> Expr {
        let mut lexer = Lexer::new(s);
        let mut parser = Parser::new(&mut lexer);
        parser.parse_expression(Precedence::Lowest).unwrap()
    }

    #[test]
    fn test_literals() {
        insta::assert_debug_snapshot!(parse_expr("1"));
        insta::assert_debug_snapshot!(parse_expr("123.456"));
        insta::assert_debug_snapshot!(parse_expr("\"hello\""));
    }

    #[test]
    fn test_identifiers() {
        insta::assert_debug_snapshot!(parse_expr("a"));
        insta::assert_debug_snapshot!(parse_expr("foo"));
    }

    #[test]
    fn test_unary_prec() {
        insta::assert_debug_snapshot!(parse_expr("-ab"));
        insta::assert_debug_snapshot!(parse_expr("!foo"));
        insta::assert_debug_snapshot!(parse_expr("!-a"));
    }

    #[test]
    fn test_binary_prec() {
        insta::assert_debug_snapshot!(parse_expr("1 + 2 + 3"));
        insta::assert_debug_snapshot!(parse_expr("1 + 2 * 3"));
        insta::assert_debug_snapshot!(parse_expr("1 * 2 + 3"));
        insta::assert_debug_snapshot!(parse_expr("1 ^ 2"));
    }

    #[test]
    fn test_unary_binary_prec() {
        insta::assert_debug_snapshot!(parse_expr("-a * b"));
        insta::assert_debug_snapshot!(parse_expr("!a ^ b"));
    }

    #[test]
    fn test_binary_associativity() {
        insta::assert_debug_snapshot!(parse_expr("a + b - c"));
        insta::assert_debug_snapshot!(parse_expr("a * b / c"));
        insta::assert_debug_snapshot!(parse_expr("a ^ b ^ c"));
    }

    #[test]
    fn test_conditionals() {
        insta::assert_debug_snapshot!(parse_expr("1 ? 2 : 3"));
        insta::assert_debug_snapshot!(parse_expr("1 ? 2 : 3 ? 4 : 5"));
        insta::assert_debug_snapshot!(parse_expr("a + b ? c * d : e / f",));
    }

    #[test]
    fn test_groups() {
        insta::assert_debug_snapshot!(parse_expr("(foo)"));
        insta::assert_debug_snapshot!(parse_expr("(1 + 2) * 3"));
        insta::assert_debug_snapshot!(parse_expr("1 * (2 - 3)"));
        insta::assert_debug_snapshot!(parse_expr("a ^ (b + c)"));
        insta::assert_debug_snapshot!(parse_expr("(a ^ b) ^ c"));
    }

    #[test]
    fn test_calls() {
        insta::assert_debug_snapshot!(parse_expr("foo()"));
        insta::assert_debug_snapshot!(parse_expr("foo(a, 1, \"hello\")"));
        insta::assert_debug_snapshot!(parse_expr("a(b) + c(d)"));
        insta::assert_debug_snapshot!(parse_expr("a(b)(c)"));
    }

    #[test]
    fn test_let_declarations() {
        insta::assert_debug_snapshot!(parse_stmt("let a = b"));
        insta::assert_debug_snapshot!(parse_stmt("let a = b + c * d"));
    }

    #[test]
    fn test_ifs() {
        insta::assert_debug_snapshot!(parse_stmt(
            "
            if cond {
                a
            }
        "
        ));
        insta::assert_debug_snapshot!(parse_stmt(
            "
            if cond {
                a
            } else {
                b
            }
        "
        ));
    }
}
