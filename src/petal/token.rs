use core::fmt;

use super::source_info::{Pos, Span};

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
    Caret,

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
    Integer,
    Float,

    // Keywords.
    Struct,
    Fun,
    Export,
    Let,
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
    Integer(i64),
    Float(f64),
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
            span: Pos::new(0).into(),
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

    pub fn is(&self, token_type: TokenType) -> bool {
        self.token_type == token_type
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.token_type == TokenType::Eof {
            return write!(f, "<EOF>");
        }

        write!(f, "{}", self.lexeme)
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TokenType::*;

        match self {
            LeftParen => write!(f, "("),
            RightParen => write!(f, ")"),
            LeftBrace => write!(f, "{{"),
            RightBrace => write!(f, "}}"),
            Comma => write!(f, ","),
            Dot => write!(f, "."),
            Minus => write!(f, "-"),
            Plus => write!(f, "+"),
            Semicolon => write!(f, ";"),
            Slash => write!(f, "/"),
            Star => write!(f, "*"),
            QuestionMark => write!(f, "?"),
            Colon => write!(f, ":"),
            Caret => write!(f, "^"),
            Bang => write!(f, "!"),
            BangEqual => write!(f, "!="),
            Equal => write!(f, "="),
            EqualEqual => write!(f, "=="),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            Less => write!(f, "<"),
            LessEqual => write!(f, "<="),
            Identifier => write!(f, "identifier"),
            String => write!(f, "string"),
            Integer => write!(f, "integer"),
            Float => write!(f, "float"),
            Struct => write!(f, "struct"),
            Fun => write!(f, "fn"),
            Export => write!(f, "export"),
            Let => write!(f, "let"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            True => write!(f, "true"),
            False => write!(f, "false"),
            Comment => write!(f, "comment"),
            Eof => write!(f, "EOF"),
        }
    }
}
