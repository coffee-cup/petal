use core::fmt;

#[derive(PartialEq, Debug, Clone)]
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
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    Identifier(String),
    String(String),
    Number(f64),
    Comment(String),
}

#[derive(PartialEq, Debug, Clone)]
pub struct TokenPos {
    pub line: usize,
    pub col: usize,
}

impl TokenPos {
    pub fn new(line: usize, col: usize) -> Self {
        TokenPos { line, col }
    }
}

pub type TokenRange = (TokenPos, TokenPos);

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub range: TokenRange,
}

impl Token {
    pub fn new(token_type: TokenType) -> Token {
        Token {
            token_type,
            lexeme: "".to_string(),
            literal: None,
            range: (TokenPos::new(0, 0), TokenPos::new(0, 0)),
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

    pub fn with_range(self, range: TokenRange) -> Token {
        let mut t = self;
        t.range = range;
        t
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}
