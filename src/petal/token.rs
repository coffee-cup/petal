use core::fmt;

use super::positions::{Pos, Span};

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    QuestionMark,
    Colon,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    If,
    Else,
    True,
    False,

    Comment,
    Eof,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    Identifier(String),
    String(String),
    Number(f64),
    Comment(String),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub span: Span,
}

impl Token {
    pub fn new(token_type: TokenType) -> Token {
        Token {
            token_type,
            lexeme: "".to_string(),
            literal: None,
            span: Pos::new(0, 0).into(),
        }
    }

    pub fn with_literal(self, literal: Literal) -> Token {
        let mut t = self;
        t.literal = Some(literal);
        t
    }

    pub fn with_lexeme(self, lexeme: String) -> Token {
        let mut t = self;
        t.lexeme = lexeme;
        t
    }

    pub fn with_span(self, span: Span) -> Token {
        let mut t = self;
        t.span = span;
        t
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}
