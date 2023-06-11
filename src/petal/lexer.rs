use std::{collections::HashMap, str::Chars};
use thiserror::Error;

use super::{
    source_info::{Pos, Span},
    token::{Literal, Token, TokenType},
};

type TT = TokenType;

pub struct Lexer<'a> {
    chars: Chars<'a>,
    current: Option<char>,

    // line: usize,
    // col: usize,
    offset: usize,
    current_lexeme: String,
}

#[derive(Error, Clone, Debug)]
pub enum LexerErrorKind {
    #[error("Unexpected character {0}")]
    UnexpectedChar(char),

    #[error("Unterminated string")]
    UnterminatedString,

    #[error("Invalid number")]
    InvalidNumber,
}

#[derive(Debug, Clone)]
pub struct LexerError {
    pub span: Option<Span>,
    pub kind: LexerErrorKind,
}

impl LexerError {
    pub fn new(kind: LexerErrorKind) -> Self {
        LexerError { span: None, kind }
    }

    pub fn with_span(&self, span: Span) -> Self {
        LexerError {
            span: Some(span),
            ..self.clone()
        }
    }
}

type LexerResult<T> = Result<T, LexerError>;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("struct", TT::Struct);
        m.insert("fn", TT::Fun);
        m.insert("export", TT::Export);
        m.insert("let", TT::Let);
        m.insert("if", TT::If);
        m.insert("else", TT::Else);
        m.insert("false", TT::False);
        m.insert("true", TT::True);
        m
    };
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            chars: source.chars(),
            current: None,
            current_lexeme: "".to_string(),
            // line: 0,
            // col: 0,
            offset: 0,
        };

        lexer.advance();
        lexer
    }

    pub fn pos(&self) -> Pos {
        Pos::new(self.offset)
    }

    fn identifier_or_reserved(&mut self) -> Token {
        let mut s = String::new();

        loop {
            match self.current {
                Some(c) if c.is_alphanumeric() => s.push(c),
                _ => break,
            }

            match self.peek() {
                Some(c) if c.is_alphanumeric() => self.advance(),
                _ => break,
            }
        }

        match KEYWORDS.get(s.as_str()) {
            Some(token_type) => Token::new(token_type.clone()),
            None => Token::new(TT::Identifier).with_literal(Literal::Identifier(s)),
        }
    }

    fn string(&mut self) -> LexerResult<Token> {
        let mut s = String::new();
        let start_pos = Pos::new(self.offset);

        self.advance();

        loop {
            match self.current {
                Some('"') => break,
                Some(c) => s.push(c),
                _ => {
                    return Err(LexerError::new(LexerErrorKind::UnterminatedString)
                        .with_span(start_pos.into()))
                }
            }

            self.advance();
        }

        Ok(Token::new(TT::String).with_literal(Literal::String(s)))
    }

    fn number(&mut self) -> LexerResult<Token> {
        let mut number = String::new();
        let mut seen_dot = false;

        loop {
            match self.current {
                Some(c) if c.is_ascii_digit() => number.push(c),
                Some('.') => {
                    if seen_dot {
                        return Err(LexerError::new(LexerErrorKind::InvalidNumber)
                            .with_span(Pos::new(self.offset).into()));
                    }

                    seen_dot = true;
                    number.push('.');
                }
                _ => break,
            }

            // Advance if the next character belongs to the number
            match self.peek() {
                Some(c) if c.is_ascii_digit() || c == '.' => self.advance(),
                _ => break,
            }
        }

        if seen_dot {
            let value = number.parse::<f64>().map_err(|_e| {
                LexerError::new(LexerErrorKind::InvalidNumber)
                    .with_span(Pos::new(self.offset).into())
            })?;

            Ok(Token::new(TT::Float).with_literal(Literal::Float(value)))
        } else {
            let value = number.parse::<i64>().map_err(|_e| {
                LexerError::new(LexerErrorKind::InvalidNumber)
                    .with_span(Pos::new(self.offset).into())
            })?;

            Ok(Token::new(TT::Integer).with_literal(Literal::Integer(value)))
        }
    }

    fn match_result(&mut self, expected: char, left: TokenType, right: TokenType) -> Token {
        if self.peek() == Some(expected) {
            self.advance();
            Token::new(left)
        } else {
            Token::new(right)
        }
    }

    fn peek(&self) -> Option<char> {
        let mut iter = self.chars.clone().peekable();
        let r = iter.peek();
        r.copied()
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current {
            if !c.is_whitespace() {
                break;
            }
            self.advance();
        }
    }

    fn advance_while(&mut self, pred: &dyn Fn(char) -> bool) -> String {
        let mut s = String::new();
        while let Some(c) = self.current {
            if !pred(c) {
                break;
            }
            s.push(c);
            self.advance();
        }

        s
    }

    fn advance(&mut self) {
        // if self.current == Some('\n') {
        //     self.line += 1;
        //     self.col = 1;
        // } else {
        //     self.col += 1;
        // }

        self.offset += 1;
        self.current = self.chars.next();

        if let Some(c) = self.current {
            self.current_lexeme.push(c);
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        self.current_lexeme.clear();
        if let Some(c) = self.current {
            self.current_lexeme.push(c);
        }

        let start_offset = self.offset;
        // let start_line = self.line;
        // let start_col = self.col;

        let token = match self.current {
            Some('(') => Token::new(TT::LeftParen),
            Some(')') => Token::new(TT::RightParen),
            Some('{') => Token::new(TT::LeftBrace),
            Some('}') => Token::new(TT::RightBrace),
            Some(',') => Token::new(TT::Comma),
            Some('.') => Token::new(TT::Dot),
            Some('?') => Token::new(TT::QuestionMark),
            Some(':') => Token::new(TT::Colon),
            Some('^') => Token::new(TT::Caret),
            Some(';') => Token::new(TT::Semicolon),
            Some('+') => Token::new(TT::Plus),
            Some('-') => Token::new(TT::Minus),
            Some('*') => Token::new(TT::Star),
            Some('/') => Token::new(TT::Slash),
            Some('!') => self.match_result('=', TT::BangEqual, TT::Bang),
            Some('=') => self.match_result('=', TT::EqualEqual, TT::Equal),
            Some('<') => self.match_result('=', TT::LessEqual, TT::Less),
            Some('>') => self.match_result('=', TT::GreaterEqual, TT::Greater),
            Some('#') => {
                let comment = self.advance_while(&|c| c != '\n');
                Token::new(TT::Comment).with_literal(Literal::Comment(
                    comment
                        .trim_start_matches(|c| c == '#' || c == ' ')
                        .to_string(),
                ))
            }
            Some(c) if c.is_ascii_digit() => match self.number() {
                Ok(token) => token,
                Err(e) => return Some(Err(e)),
            },
            Some('"') => match self.string() {
                Ok(token) => token,
                Err(e) => return Some(Err(e)),
            },
            Some(c) if c.is_alphabetic() => self.identifier_or_reserved(),

            Some(_) => {
                return Some(Err(LexerError::new(LexerErrorKind::UnexpectedChar(
                    self.current.unwrap(),
                ))
                .with_span(Pos::new(self.offset).into())))
            }

            None => return None,
        };

        let token = token
            .with_lexeme(self.current_lexeme.clone())
            .with_span(Span::new(start_offset.into(), self.offset.into()));

        self.advance();

        Some(Ok(token))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(s: String) -> Vec<Token> {
        let lexer = Lexer::new(&s);
        lexer.map(Result::unwrap).collect::<Vec<_>>()
    }

    #[test]
    fn test_whitespace() {
        let tokens = lex(" \t\r\n\t\r  ".to_string());
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn test_comments() {
        let tokens = lex("# hello world".to_string());
        insta::assert_debug_snapshot!(tokens);
    }

    #[test]
    fn test_single_letter_operators() {
        insta::assert_debug_snapshot!(lex("( ) { } . , ? : ^ ; + - * / ! = < >".to_string()));
        insta::assert_debug_snapshot!(lex("   ()\n(\n    (".to_string()));
    }

    #[test]
    fn test_equal_operators() {
        insta::assert_debug_snapshot!(lex("== != <= >=".to_string()));
    }

    #[test]
    fn test_strings() {
        insta::assert_debug_snapshot!(lex("\"hello world\" \"123 -+() . '\"".to_string()));
        insta::assert_debug_snapshot!(lex("\"multi\nline\nstring\"".to_string()));
    }

    #[test]
    fn test_numbers() {
        insta::assert_debug_snapshot!(lex("1 123 123.456".to_string()));
    }

    #[test]
    fn test_keywords() {
        insta::assert_debug_snapshot!(lex("let if else false true".to_string()));
    }

    #[test]
    fn test_identifiers() {
        insta::assert_debug_snapshot!(lex("a boop hello world".to_string()));
    }
}
