use core::fmt;

use super::positions::{HasSpan, Pos, Span};

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
    Number,

    // Keywords.
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

    pub fn is(&self, token_type: TokenType) -> bool {
        self.token_type == token_type
    }
}

impl HasSpan for Token {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "("),
            TokenType::RightParen => write!(f, ")"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::Comma => write!(f, ","),
            TokenType::Dot => write!(f, "."),
            TokenType::Minus => write!(f, "-"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Slash => write!(f, "/"),
            TokenType::Star => write!(f, "*"),
            TokenType::QuestionMark => write!(f, "?"),
            TokenType::Colon => write!(f, ":"),
            TokenType::Caret => write!(f, "^"),
            TokenType::Bang => write!(f, "!"),
            TokenType::BangEqual => write!(f, "!="),
            TokenType::Equal => write!(f, "="),
            TokenType::EqualEqual => write!(f, "=="),
            TokenType::Greater => write!(f, ">"),
            TokenType::GreaterEqual => write!(f, ">="),
            TokenType::Less => write!(f, "<"),
            TokenType::LessEqual => write!(f, "<="),
            TokenType::Identifier => write!(f, "identifier"),
            TokenType::String => write!(f, "string"),
            TokenType::Number => write!(f, "number"),
            TokenType::Fun => write!(f, "fn"),
            TokenType::Export => write!(f, "export"),
            TokenType::Let => write!(f, "let"),
            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),
            TokenType::True => write!(f, "true"),
            TokenType::False => write!(f, "false"),
            TokenType::Comment => write!(f, "comment"),
            TokenType::Eof => write!(f, "EOF"),
        }
    }
}
