use core::fmt;
use std::{collections::HashMap, str::Chars};

use super::token::{Literal, Token, TokenPos, TokenType};

type TT = TokenType;

pub struct Lexer<'a> {
    chars: Chars<'a>,
    current: Option<char>,

    line: usize,
    col: usize,
    current_lexeme: String,
}

#[derive(Debug, Clone)]
pub struct LexerError {
    pub pos: TokenPos,
    pub msg: String,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("else", TT::Else);
        m.insert("false", TT::False);
        m.insert("if", TT::If);
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
            line: 0,
            col: 0,
        };

        lexer.advance();
        lexer
    }

    pub fn lex(source: &str) -> Result<Vec<Token>, LexerError> {
        let lexer = Lexer::new(source);

        let mut tokens: Vec<Token> = Vec::new();
        for t in lexer {
            match t {
                Ok(token) => tokens.push(token),
                Err(e) => return Err(e),
            }
        }

        Ok(tokens)
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

    fn string(&mut self) -> Result<Token, LexerError> {
        let mut s = String::new();
        let start_pos = TokenPos::new(self.line, self.col);

        self.advance();

        loop {
            match self.current {
                Some('"') => break,
                Some(c) => s.push(c),
                _ => {
                    return Err(LexerError::new(
                        start_pos,
                        "Unterminated string".to_string(),
                    ))
                }
            }

            self.advance();
        }

        Ok(Token::new(TT::String).with_literal(Literal::String(s)))
    }

    fn number(&mut self) -> Result<Token, LexerError> {
        let mut number = String::new();
        let mut seen_dot = false;

        loop {
            match self.current {
                Some(c) if c.is_ascii_digit() => number.push(c),
                Some('.') => {
                    if seen_dot {
                        return Err(LexerError::new(
                            TokenPos::new(self.line, self.col),
                            "Invalid number".to_string(),
                        ));
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

        let value = number.parse::<f64>().map_err(|_e| {
            LexerError::new(
                TokenPos::new(self.line, self.col),
                "Invalid number".to_string(),
            )
        })?;

        Ok(Token::new(TT::Number).with_literal(Literal::Number(value)))
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
        if self.current == Some('\n') {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }

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

        let start_line = self.line;
        let start_col = self.col;

        let token = match self.current {
            Some('(') => Token::new(TT::LeftParen),
            Some(')') => Token::new(TT::RightParen),
            Some('{') => Token::new(TT::LeftBrace),
            Some('}') => Token::new(TT::RightBrace),
            Some(',') => Token::new(TT::Comma),
            Some('.') => Token::new(TT::Dot),
            Some('?') => Token::new(TT::QuestionMark),
            Some(':') => Token::new(TT::Colon),
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
                return Some(Err(LexerError::new(
                    TokenPos::new(self.line, self.col),
                    format!("Unexpected character '{}'", self.current.unwrap()),
                )))
            }

            None => return None,
        };

        let token = token.with_lexeme(self.current_lexeme.clone()).with_range((
            TokenPos::new(start_line, start_col),
            TokenPos::new(self.line, self.col),
        ));

        self.advance();

        Some(Ok(token))
    }
}

impl LexerError {
    pub fn new(pos: TokenPos, msg: String) -> LexerError {
        LexerError { pos, msg }
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}:{}] Error {}", self.pos.line, self.pos.col, self.msg)
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
        insta::assert_debug_snapshot!(lex("( ) { } . , ? : ; + - * / ! = < >".to_string()));
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
        insta::assert_debug_snapshot!(lex("if else false true".to_string()));
    }

    #[test]
    fn test_identifiers() {
        insta::assert_debug_snapshot!(lex("a boop hello world".to_string()));
    }
}
