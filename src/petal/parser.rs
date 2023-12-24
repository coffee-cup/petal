use miette::{Diagnostic, SourceSpan};
use std::{collections::HashMap, rc::Rc};
use thiserror::Error;

use super::{
    ast::{
        BinaryOp, BinaryOpType, Block, Expr, ExprId, FuncArg, FuncDecl, IdentId, LetDecl, PrefixOp,
        PrefixOpType, Program, Stmt, StmtId, StructDecl, StructField, TypeAnnotation,
    },
    lexer::{Lexer, LexerError},
    precedence::Precedence,
    source_info::Span,
    token::{Literal, Token, TokenType},
};

type TT = TokenType;

#[derive(Diagnostic, Error, Clone, Debug)]
pub enum ParserError {
    #[error("{0}")]
    LexerError(#[from] LexerError),

    #[error("Unexpected token")]
    UnexpectedToken {
        token: String,

        #[label("we came across the token `{token}` and don't know how to parse it")]
        span: Span,
    },

    #[error("Missing type annotation")]
    #[diagnostic(help("You can add a type annotation like this: `{name}: Int`"))]
    MissingTypeAnnotation {
        #[label("all function parameters need a type annotation")]
        span: Span,

        name: String,
    },

    #[error("Expected {expected}, found the end of the file")]
    UnexpectedEof {
        expected: TokenType,

        #[label = "we expected to find `{expected}`, but found the end of the file instead"]
        span: Span,
    },

    #[error("Expected {expected}, found {found}")]
    ExpectedToken {
        expected: TokenType,

        found: String,

        #[label = "we expected to find `{expected}`, but found `{found}` instead"]
        span: SourceSpan,
    },
}

type ParserResult<T> = Result<T, ParserError>;

trait PrefixParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<ExprId>;
}

trait InfixParselet {
    fn parse(&self, parser: &mut Parser, left: ExprId, token: Token) -> ParserResult<ExprId>;
    fn precedence(&self) -> Precedence;
}

struct NumberParselet;
impl PrefixParselet for NumberParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<ExprId> {
        match token.literal {
            Some(Literal::Integer(value)) => Ok(parser
                .program
                .new_expression(Expr::Integer(value), token.span)),
            Some(Literal::Float(value)) => Ok(parser
                .program
                .new_expression(Expr::Float(value), token.span)),
            _ => Err(ParserError::UnexpectedToken {
                token: token.to_string(),
                span: token.span(),
            }),
        }
    }
}

struct BooleanParselet;
impl PrefixParselet for BooleanParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<ExprId> {
        match token.literal {
            Some(Literal::Boolean(value)) => {
                Ok(parser.program.new_expression(Expr::Bool(value), token.span))
            }
            _ => Err(ParserError::UnexpectedToken {
                token: token.to_string(),
                span: token.span(),
            }),
        }
    }
}

struct StringParselet;
impl PrefixParselet for StringParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<ExprId> {
        match token.literal {
            Some(Literal::String(value)) => Ok(parser
                .program
                .new_expression(Expr::String(value), token.span)),
            _ => Err(ParserError::UnexpectedToken {
                token: token.to_string(),
                span: token.span(),
            }),
        }
    }
}

struct IdentParselet;
impl PrefixParselet for IdentParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<ExprId> {
        match token.literal {
            Some(Literal::Identifier(name)) => {
                let ident = parser.program.new_identifier(name, token.span.clone());

                Ok(parser
                    .program
                    .new_expression(Expr::Ident(ident), token.span))
            }
            _ => Err(ParserError::UnexpectedToken {
                token: token.to_string(),
                span: token.span(),
            }),
        }
    }
}

// struct CommentParselet;
// impl PrefixParselet for CommentParselet {
//     fn parse(&self, _parser: &mut Parser, token: Token) -> ParserResult<ExprId> {
//         match token.literal {
//             Some(Literal::Comment(value)) => Ok(Expr::Comment {
//                 value,
//                 span: token.span,
//             }),
//             _ => Err(ParserError::new(ParserErrorKind::UnexpectedToken(
//                 token.to_string(),
//             ))),
//         }
//     }
// }

struct GroupParselet;
impl PrefixParselet for GroupParselet {
    fn parse(&self, parser: &mut Parser, _token: Token) -> ParserResult<ExprId> {
        println!("Parsing group! {:?}", _token);

        let expr = parser.parse_expression(Precedence::Lowest)?;
        parser.consume_expected(TT::RightParen)?;
        Ok(expr)
    }
}

struct PrefixOperatorParselet {
    precedence: Precedence,
}
impl PrefixParselet for PrefixOperatorParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<ExprId> {
        let operand = parser.parse_expression(self.precedence.clone())?;

        let span = parser.program.ast.expressions[operand]
            .span
            .merge(token.span());

        let prefix_op_type = match token.token_type {
            TT::Minus => PrefixOpType::Neg,
            TT::Bang => PrefixOpType::Not,
            _ => unreachable!("Invalid prefix operator, {token}"),
        };

        Ok(parser.program.new_expression(
            Expr::PrefixOp {
                op: PrefixOp {
                    prefix_type: prefix_op_type,
                    span: token.span(),
                },
                right: operand,
            },
            span,
        ))
    }
}

struct CallParselet;
impl InfixParselet for CallParselet {
    fn parse(&self, parser: &mut Parser, left: ExprId, _token: Token) -> ParserResult<ExprId> {
        let mut args: Vec<ExprId> = Vec::new();
        let mut span = parser.program.ast.expressions[left].span.clone();

        loop {
            if parser.peek().token_type == TT::RightParen {
                break;
            }

            let arg = parser.parse_expression(Precedence::Lowest)?;
            span = span.merge(parser.program.ast.expressions[arg].span.clone());

            args.push(arg);
            parser.match_expected(TT::Comma)?;
        }

        let t = parser.consume_expected(TT::RightParen)?;
        span = span.merge(t.span());

        Ok(parser
            .program
            .new_expression(Expr::Call { callee: left, args }, span))
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
    fn parse(&self, parser: &mut Parser, left: ExprId, token: Token) -> ParserResult<ExprId> {
        let parse_right_prec = if self.is_right {
            self.precedence().prev()
        } else {
            self.precedence()
        };

        let right = parser.parse_expression(parse_right_prec)?;
        let span = parser.program.ast.expressions[left]
            .span
            .merge(parser.program.ast.expressions[right].span.clone());

        let binary_type = match token.token_type {
            TT::Plus => BinaryOpType::Add,
            TT::Minus => BinaryOpType::Subtract,
            TT::Star => BinaryOpType::Multiply,
            TT::Slash => BinaryOpType::Divide,
            // TT::Caret => BinaryOpType::Power,
            _ => unreachable!("Invalid binary operator, {token}"),
        };

        Ok(parser.program.new_expression(
            Expr::BinaryOp {
                left,
                op: BinaryOp {
                    binary_type,
                    span: token.span(),
                },
                right,
            },
            span,
        ))
    }

    fn precedence(&self) -> Precedence {
        self.precedence.clone()
    }
}

struct PostfixOperatorParselet {
    precedence: Precedence,
}
impl PrefixParselet for PostfixOperatorParselet {
    fn parse(&self, parser: &mut Parser, token: Token) -> ParserResult<ExprId> {
        let operand = parser.parse_expression(self.precedence.clone())?;
        let span = parser.program.ast.expressions[operand]
            .span
            .merge(token.span());

        Ok(parser.program.new_expression(
            Expr::PostfixOp {
                op: token,
                left: operand,
            },
            span,
        ))
    }
}

// struct ConditionalParselet;
// impl InfixParselet for ConditionalParselet {
//     fn parse(&self, parser: &mut Parser, left: Expr, _token: Token) -> ParserResult<Expr> {
//         let then_branch = parser.parse_expression(Precedence::Lowest)?;
//         parser.consume_expected(TT::Colon)?;
//         let else_branch = parser.parse_expression(self.precedence().prev())?;
//         let span = left.span().merge(else_branch.span());

//         Ok(Expr::Conditional {
//             condition: Box::new(left),
//             then_branch: Box::new(then_branch),
//             else_branch: Box::new(else_branch),
//             span,
//         })
//     }

//     fn precedence(&self) -> Precedence {
//         Precedence::Conditional
//     }
// }

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
    program: Program,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer<'a>) -> ParserResult<Parser<'a>> {
        let mut parser = Parser {
            lexer,
            current_token: Token::new(TT::Eof),
            next_token: Token::new(TT::Eof),
            prefix_parselets: HashMap::new(),
            infix_parselets: HashMap::new(),
            program: Program::new(),
        };

        // Prime the next_token
        parser.consume()?;

        // Custom parselets
        register!(parser.prefix_parselets, TT::Integer, NumberParselet);
        register!(parser.prefix_parselets, TT::Float, NumberParselet);
        register!(parser.prefix_parselets, TT::String, StringParselet);
        register!(parser.prefix_parselets, TT::Identifier, IdentParselet);
        register!(parser.prefix_parselets, TT::Boolean, BooleanParselet);
        // register!(parser.prefix_parselets, TT::Comment, CommentParselet);
        register!(parser.prefix_parselets, TT::LeftParen, GroupParselet);
        // register!(parser.infix_parselets, TT::QuestionMark, ConditionalParselet);
        register!(parser.infix_parselets, TT::LeftParen, CallParselet);

        // Prefix parselets
        prefix!(parser.prefix_parselets, TT::Minus, Precedence::Unary);
        prefix!(parser.prefix_parselets, TT::Bang, Precedence::Unary);

        // Infix parselets
        infix_left!(parser.infix_parselets, TT::Minus, Precedence::Sum);
        infix_left!(parser.infix_parselets, TT::Plus, Precedence::Sum);
        infix_left!(parser.infix_parselets, TT::Star, Precedence::Product);
        infix_left!(parser.infix_parselets, TT::Slash, Precedence::Product);
        // infix_right!(parser.infix_parselets, TT::Caret, Precedence::Exponent);

        Ok(parser)
    }

    pub fn parse(&mut self) -> ParserResult<Program> {
        self.parse_program()?;
        self.consume_expected(TT::Eof)?;

        Ok(self.program.clone())
    }

    fn parse_program(&mut self) -> ParserResult<()> {
        while !self.is_at_end() {
            match self.peek().token_type {
                TT::Struct => {
                    self.parse_struct_decl()?;
                }
                TT::Fun | TT::Export => {
                    self.parse_function()?;
                }
                _ => {
                    let stmt = self.parse_declaration()?;
                    self.program.main_stmts.push(stmt);
                }
            };
        }

        Ok(())
    }

    fn parse_struct_decl(&mut self) -> ParserResult<()> {
        let token = self.consume()?;
        let mut span = token.span();

        let ident = self.parse_identifier()?;
        self.consume_expected(TT::LeftBrace)?;

        let mut fields = Vec::new();
        while self.peek().is(TT::Identifier) {
            let field = self.parse_struct_field()?;
            fields.push(field);

            if self.peek().is(TT::RightBrace) {
                break;
            }

            self.consume_expected(TT::Comma)?;
        }

        let token = self.consume_expected(TT::RightBrace)?;

        span = span.merge(token.span());

        self.program.add_struct(StructDecl {
            ident,
            fields,
            span,
        });

        Ok(())
    }

    fn parse_struct_field(&mut self) -> ParserResult<StructField> {
        let token = self.consume_expected(TT::Identifier)?;
        let mut span = token.span();

        let ident = self.parse_identifier()?;
        self.consume_expected(TT::Colon)?;

        let ty = self.parse_type()?;
        span = span.merge(ty.span.clone());

        Ok(StructField { ident, ty, span })
    }

    fn parse_function(&mut self) -> ParserResult<()> {
        let token = self.consume()?;
        let mut span = token.span();

        // Export keyword
        let is_exported = token.is(TT::Export);
        if is_exported {
            self.consume_expected(TT::Fun)?;
        }

        let ident = self.parse_identifier()?;

        // Generic type parameters
        let mut type_params = Vec::new();
        if self.match_expected(TT::Less)? {
            while !self.peek().is(TT::Greater) && !self.is_at_end() {
                let ty = self.parse_type()?;
                type_params.push(ty);

                if self.peek().is(TT::Comma) {
                    self.consume()?;
                }
            }

            self.consume_expected(TT::Greater)?;
        }

        // Function args
        self.consume_expected(TT::LeftParen)?;
        let mut args: Vec<FuncArg> = Vec::new();
        while !self.peek().is(TT::RightParen) && !self.is_at_end() {
            args.push(self.parse_function_arg()?);
            if self.peek().is(TT::Comma) {
                self.consume()?;
            }
        }
        self.consume_expected(TT::RightParen)?;

        // Return type
        let mut return_ty = None;
        if self.match_expected(TT::Colon)? {
            return_ty = Some(self.parse_type()?);
        }

        // Function body
        let block = self.parse_block()?;
        span = span.merge(self.program.ast.statements[block].span.clone());

        self.program.add_function(FuncDecl {
            ident,
            is_exported,
            type_params,
            args,
            return_ty,
            body: block,
            span,
        });

        Ok(())
    }

    fn parse_identifier(&mut self) -> ParserResult<IdentId> {
        let token = self.consume_expected(TT::Identifier)?;
        let span = token.span();

        let name = match token.literal {
            Some(Literal::Identifier(name)) => name,
            _ => unreachable!(),
        };

        let ident = self.program.new_identifier(name, span);
        Ok(ident)
    }

    fn parse_function_arg(&mut self) -> ParserResult<FuncArg> {
        let ident = self.parse_identifier()?;
        let span = self.program.span_for_ident(ident);

        self.consume_expected(TT::Colon)
            .map_err(|_e| ParserError::MissingTypeAnnotation {
                span: span.clone(),
                name: self.program.get_ident_name(ident),
            })?;

        let ty = self.parse_type()?;

        let span = span.merge(ty.span.clone());

        Ok(FuncArg { ident, span, ty })
    }

    fn parse_type(&mut self) -> ParserResult<TypeAnnotation> {
        // TODO: map error to expected type
        let token = self.consume_expected(TT::Identifier)?;
        let span = token.span();

        let name = match token.literal {
            Some(Literal::Identifier(name)) => name,
            _ => unreachable!(),
        };

        // TODO: ensure that the type starts with a capital letter
        Ok(TypeAnnotation { name, span })
    }

    fn parse_block(&mut self) -> ParserResult<StmtId> {
        let token = self.consume_expected(TT::LeftBrace)?;
        let mut span = token.span();

        let mut statements: Vec<StmtId> = Vec::new();
        while !self.peek().is(TT::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_declaration()?);
        }

        let token = self.consume_expected(TT::RightBrace)?;
        span = span.merge(token.span());

        Ok(self
            .program
            .new_statement(Stmt::BlockStmt(Block { statements }), span))
    }

    fn parse_declaration(&mut self) -> ParserResult<StmtId> {
        match self.peek().token_type {
            TT::Let => self.parse_let_declaration(),
            TT::Return => self.parse_return(),
            _ => self.parse_statement(),
        }
    }

    fn parse_return(&mut self) -> ParserResult<StmtId> {
        let token = self.consume()?;
        let mut span = token.span();

        let expr = self
            .parse_expression(Precedence::Lowest)
            .map(|e| Some(e))
            .unwrap_or(None);

        if let Some(expr) = expr {
            span = span.merge(self.program.ast.expressions[expr].span.clone());
        }

        Ok(self.program.new_statement(Stmt::Return(expr), span))
    }

    fn parse_let_declaration(&mut self) -> ParserResult<StmtId> {
        let token = self.consume()?;
        let mut span = token.span();

        let name = self.parse_identifier()?;

        let mut ty = None;
        if self.match_expected(TT::Colon)? {
            ty = Some(self.parse_type()?);
        }

        self.consume_expected(TT::Equal)?;
        let init = self.parse_expression(Precedence::Lowest)?;

        span = span.merge(self.program.ast.expressions[init].span.clone());

        Ok(self.program.new_statement(
            Stmt::Let(LetDecl {
                ident: name,
                ty,
                init,
            }),
            span,
        ))
    }

    fn parse_statement(&mut self) -> ParserResult<StmtId> {
        let stmt = match self.peek().token_type {
            TT::If => self.parse_if_statement()?,
            _ => {
                let expr = self.parse_expression(Precedence::Lowest)?;
                let span = self.program.ast.expressions[expr].span.clone();

                // TODO: consume newline or semicolon?
                self.program.new_statement(Stmt::ExprStmt(expr), span)
            }
        };

        Ok(stmt)
    }

    fn parse_if_statement(&mut self) -> ParserResult<StmtId> {
        let token = self.consume()?;
        let mut span = token.span();

        let condition = self.parse_expression(Precedence::Lowest)?;

        let then_block = self.parse_block()?;
        span = span.merge(self.program.ast.statements[then_block].span.clone());

        let else_block = if self.match_expected(TT::Else)? {
            let else_block = self.parse_block()?;
            span = span.merge(self.program.ast.statements[else_block].span.clone());
            Some(else_block)
        } else {
            None
        };

        Ok(self.program.new_statement(
            Stmt::IfStmt {
                condition,
                then_block,
                else_block,
            },
            span,
        ))
    }

    /// Parse the next expression using the provided precedence
    /// This is the core of the Pratt parser
    pub fn parse_expression(&mut self, precedence: Precedence) -> ParserResult<ExprId> {
        let token = self.consume()?;

        let prefix_parselet = self
            .prefix_parselets
            .get(&token.token_type)
            .ok_or_else(|| ParserError::UnexpectedToken {
                token: token.to_string(),
                span: token.span(),
            })?
            .clone();

        let mut left = prefix_parselet.parse(self, token.clone())?;

        while precedence < self.get_precedence() {
            let token = self.consume()?;

            let infix_parselet = self
                .infix_parselets
                .get(&token.token_type)
                .ok_or_else(|| ParserError::UnexpectedToken {
                    token: token.to_string(),
                    span: token.span(),
                })?
                .clone();

            left = infix_parselet.parse(self, left, token.clone())?;
        }

        Ok(left)
    }

    /// Get the infix precedence of the next token
    fn get_precedence(&self) -> Precedence {
        self.infix_parselets
            .get(&self.next_token.token_type)
            .map(|p| p.precedence())
            .unwrap_or(Precedence::Lowest)
    }

    /// Returns the next token without consuming it
    fn peek(&self) -> &Token {
        &self.next_token
    }

    /// Returns whether or not we are at the end of the token stream
    fn is_at_end(&self) -> bool {
        self.next_token.token_type == TT::Eof
    }

    /// Consumes the next token if it matches the expected token type.
    /// Returns true if the token was consumed, false otherwise.
    fn match_expected(&mut self, token_type: TokenType) -> ParserResult<bool> {
        if self.next_token.is(token_type) {
            self.consume()?;
            return Ok(true);
        }

        Ok(false)
    }

    /// Consumes the next token if it matches the expected token type.
    /// Returns the consumed token if it was consumed, an error otherwise.
    fn consume_expected(&mut self, token_type: TokenType) -> ParserResult<Token> {
        if self.next_token.token_type != token_type {
            if self.next_token.token_type == TT::Eof {
                return Err(ParserError::UnexpectedEof {
                    expected: token_type,
                    span: self.next_token.span.clone(),
                });
            }

            // TODO: Figure out how to add extra context to this
            return Err(ParserError::ExpectedToken {
                expected: token_type,
                found: self.next_token.to_string(),
                span: self.next_token.span.clone().into(),
            });
        }

        self.consume()
    }

    /// Consumes the next token and returns it
    fn consume(&mut self) -> ParserResult<Token> {
        self.current_token = self.next_token.clone();
        self.next_token = self
            .lexer
            .next()
            .unwrap_or_else(|| {
                let offset = self.lexer.pos().offset();
                Ok(Token::new(TT::Eof).with_span(Span::new((offset - 1).into(), None)))
            })
            .map_err(ParserError::LexerError)?;

        Ok(self.current_token.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::petal::ast::{ExprNode, Identifier, StmtNode};

    use super::*;

    fn parse_expr(s: &str) -> (Vec<Identifier>, Vec<ExprNode>) {
        let mut lexer = Lexer::new(s);
        let mut parser = Parser::new(&mut lexer).unwrap();
        parser.parse_expression(Precedence::Lowest).unwrap();

        let mut idents = Vec::new();
        for (_, ident) in parser.program.ast.identifiers.iter() {
            idents.push(ident.clone())
        }

        let mut exprs = Vec::new();
        for (_, expr) in parser.program.ast.expressions.iter() {
            exprs.push(expr.clone())
        }

        (idents, exprs)
    }

    fn parse_stmt(s: &str) -> (Vec<Identifier>, Vec<ExprNode>, Vec<StmtNode>, Vec<FuncDecl>) {
        let mut lexer = Lexer::new(s);
        let mut parser = Parser::new(&mut lexer).unwrap();
        parser.parse_program().unwrap();

        let mut idents = Vec::new();
        for (_, ident) in parser.program.ast.identifiers.iter() {
            idents.push(ident.clone())
        }

        let mut exprs = Vec::new();
        for (_, expr) in parser.program.ast.expressions.iter() {
            exprs.push(expr.clone())
        }

        let mut stmts = Vec::new();
        for (_, stmt) in parser.program.ast.statements.iter() {
            stmts.push(stmt.clone())
        }

        let mut funcs = Vec::new();
        for func in parser.program.functions.iter() {
            funcs.push(func.clone())
        }

        (idents, exprs, stmts, funcs)
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

    // #[test]
    // fn test_conditionals() {
    //     insta::assert_debug_snapshot!(parse_expr("1 ? 2 : 3"));
    //     insta::assert_debug_snapshot!(parse_expr("1 ? 2 : 3 ? 4 : 5"));
    //     insta::assert_debug_snapshot!(parse_expr("a + b ? c * d : e / f",));
    // }

    #[test]
    fn test_groups() {
        insta::assert_debug_snapshot!(parse_expr("(foo)"));
        insta::assert_debug_snapshot!(parse_expr("(1 + 2) * 3"));
        insta::assert_debug_snapshot!(parse_expr("1 * (2 - 3)"));
        // insta::assert_debug_snapshot!(parse_expr("a ^ (b + c)"));
        // insta::assert_debug_snapshot!(parse_expr("(a ^ b) ^ c"));
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
        insta::assert_debug_snapshot!(parse_stmt("let a: Float = b"));
    }

    // #[test]
    // fn test_ifs() {
    //     insta::assert_debug_snapshot!(parse_stmt(
    //         "
    //         if cond {
    //             a
    //         }
    //     "
    //     ));
    //     insta::assert_debug_snapshot!(parse_stmt(
    //         "
    //         if cond {
    //             a
    //         } else {
    //             b
    //         }
    //     "
    //     ));
    // }

    // #[test]
    // fn test_structs() {
    //     insta::assert_debug_snapshot!(parse_stmt("struct Foo {}"));
    //     insta::assert_debug_snapshot!(parse_stmt("struct Foo { hello: Int }"));
    //     insta::assert_debug_snapshot!(parse_stmt(
    //         "struct Foo {
    //         hello: Int,
    //         world: String
    //     }"
    //     ));
    // }

    #[test]
    fn test_functions() {
        insta::assert_debug_snapshot!(parse_stmt("fn foo() {}"));
        insta::assert_debug_snapshot!(parse_stmt("fn foo(a: Int) {}"));
        insta::assert_debug_snapshot!(parse_stmt("fn foo(a: Int, b: String) {}"));
        insta::assert_debug_snapshot!(parse_stmt(
            "
            fn foo(foo: Int, bar: Int) {
                let a = foo + bar
            }"
        ));
        insta::assert_debug_snapshot!(parse_stmt("export fn foo() {}"));

        //     insta::assert_debug_snapshot!(parse_stmt("fn foo<T>() {}"));
        //     insta::assert_debug_snapshot!(parse_stmt("fn foo<T>(a: T) {}"));
    }
}
