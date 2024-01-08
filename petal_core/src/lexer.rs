use colored::Colorize;
use miette::Diagnostic;
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

    offset: usize,
    current_lexeme: String,
}

#[derive(Diagnostic, Error, Clone, Debug)]
pub enum LexerError {
    #[error("Unexpected character")]
    #[diagnostic()]
    UnexpectedChar {
        c: char,

        #[label("The character `{c}` was found but not expected here")]
        span: Span,
    },

    #[error("Unterminated string")]
    #[diagnostic(help("Strings must be closed with a double quote `{}`", "\"".bold()))]
    UnterminatedString {
        #[label("This string is unterminated")]
        span: Span,
    },

    #[error("Invalid number")]
    InvalidNumber {
        #[label("This number is invalid")]
        span: Span,

        #[help("{0}")]
        help: Option<String>,
    },
}

type LexerResult<T> = Result<T, LexerError>;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("struct", TT::Struct);
        m.insert("fn", TT::Fun);
        m.insert("export", TT::Export);
        m.insert("import", TT::Import);
        m.insert("let", TT::Let);
        m.insert("if", TT::If);
        m.insert("while", TT::While);
        m.insert("else", TT::Else);
        m.insert("true", TT::Boolean);
        m.insert("false", TT::Boolean);
        m.insert("return", TT::Return);
        m
    };
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            chars: source.chars(),
            current: None,
            current_lexeme: "".to_string(),
            offset: 0,
        };

        lexer.advance();

        // The first call to advance() will load the first character into current as well as increase the offset
        // However, we want to the first character to be at offset 0, so we reset the offset to 0
        lexer.offset = 0;

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
            Some(TT::Boolean) => {
                if s == "true" {
                    Token::new(TT::Boolean).with_literal(Literal::Boolean(true))
                } else {
                    Token::new(TT::Boolean).with_literal(Literal::Boolean(false))
                }
            }
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
                    return Err(LexerError::UnterminatedString {
                        span: start_pos.into(),
                    })
                }
            }

            self.advance();
        }

        Ok(Token::new(TT::String).with_literal(Literal::String(s)))
    }

    fn number(&mut self) -> LexerResult<Token> {
        let mut number = String::new();
        let mut seen_dot = false;
        let start_pos = (self.offset).into();

        loop {
            match self.current {
                Some(c) if c.is_ascii_digit() => number.push(c),
                Some('.') => {
                    if seen_dot {
                        return Err(LexerError::InvalidNumber {
                            span: Span::new(start_pos, Some((self.offset + 1).into())),
                            help: Some("A number cannot have more than one decimal point".into()),
                        });
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

        let t = if seen_dot {
            let value = number
                .parse::<f64>()
                .map_err(|_e| LexerError::InvalidNumber {
                    span: Pos::new(self.offset).into(),
                    help: None,
                })?;

            Token::new(TT::Float).with_literal(Literal::Float(value))
        } else {
            let value = number
                .parse::<i64>()
                .map_err(|_e| LexerError::InvalidNumber {
                    span: Pos::new(self.offset).into(),
                    help: None,
                })?;

            Token::new(TT::Integer).with_literal(Literal::Integer(value))
        };

        Ok(t)
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
                return Some(Err(LexerError::UnexpectedChar {
                    c: self.current.unwrap(),
                    span: Pos::new(self.offset).into(),
                }))
            }

            None => return None,
        };

        let token = token
            .with_lexeme(self.current_lexeme.clone())
            .with_span(Span::new(start_offset.into(), Some(self.offset.into())));

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
        insta::assert_debug_snapshot!(lex(
            "let if while else false true return export import".to_string()
        ));
    }

    #[test]
    fn test_identifiers() {
        insta::assert_debug_snapshot!(lex("a boop hello world".to_string()));
    }

    #[test]
    fn test_groups() {
        insta::assert_debug_snapshot!(lex("(a) (1) (1 + 2)".to_string()));
    }
}
