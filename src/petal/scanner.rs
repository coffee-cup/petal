use core::fmt;
use std::collections::HashMap;

use super::token::{Literal, Token, TokenType};

type TT = TokenType;

pub struct Scanner<'a> {
    source: &'a str,
    chars: Vec<char>,

    tokens: Vec<Token>,
    errors: Vec<ScannerError>,
    start: usize,
    current: usize,
    line: usize,
}

#[derive(Debug, Clone)]
pub struct ScannerError {
    pub line: usize,
    pub msg: String,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("else", TT::Else);
        m.insert("false", TT::False);
        m.insert("if", TT::If);
        m.insert("return", TT::Return);
        m.insert("true", TT::True);
        m.insert("let", TT::Var);
        m
    };
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Scanner<'a> {
        Scanner {
            source,
            chars: source.chars().collect(),

            tokens: Vec::new(),
            errors: Vec::new(),
            start: 0,
            current: 0,
            line: 0,
        }
    }

    pub fn lex(source: &'a str) -> Result<Vec<Token>, Vec<ScannerError>> {
        let mut lexer = Scanner::new(source);
        lexer.scan_tokens()
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, Vec<ScannerError>> {
        while !self.is_at_end() {
            // We are at the beginning of the next lexeme
            self.start = self.current;

            self.scan_token()
        }

        self.tokens
            .push(Token::new(TT::Eof, "".to_string(), self.line));

        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        }

        Ok(self.tokens.clone())
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(self.next_token(TT::LeftParen)),
            ')' => self.add_token(self.next_token(TT::RightParen)),
            '{' => self.add_token(self.next_token(TT::LeftBrace)),
            '}' => self.add_token(self.next_token(TT::RightBrace)),
            ',' => self.add_token(self.next_token(TT::Comma)),
            '.' => self.add_token(self.next_token(TT::Dot)),
            '?' => self.add_token(self.next_token(TT::QuestionMark)),
            ':' => self.add_token(self.next_token(TT::Colon)),
            '-' => self.add_token(self.next_token(TT::Minus)),
            '+' => self.add_token(self.next_token(TT::Plus)),
            ';' => self.add_token(self.next_token(TT::Semicolon)),
            '*' => self.add_token(self.next_token(TT::Star)),
            '!' => self.match_result('=', TT::BangEqual, TT::Bang),
            '=' => self.match_result('=', TT::EqualEqual, TT::Equal),
            '<' => self.match_result('=', TT::LessEqual, TT::Less),
            '>' => self.match_result('=', TT::GreaterEqual, TT::Greater),
            '/' => self.add_token(self.next_token(TT::Slash)),
            '#' => {
                let text = self.advance_while(&|c| c != '\n');
                self.add_token(
                    Token::new(TT::Comment, text.clone(), self.line)
                        .with_literal(Literal::Comment(text.trim().to_string())),
                )
            }
            '"' => self.string(),
            ' ' => (),
            '\r' => (),
            '\t' => (),
            '\n' => {
                self.line += 1;
            }
            _ => {
                if is_digit(c) {
                    self.number();
                } else if is_alpha(c) {
                    self.identifier_or_reserved();
                } else {
                    self.error("Unexpected character")
                }
            }
        }
    }

    fn identifier_or_reserved(&mut self) {
        self.advance_while(&is_alpha_numeric);

        let text = self.source[self.start..self.current].to_string();

        match KEYWORDS.get(text.as_str()) {
            Some(token_type) => self.add_token(self.next_token(token_type.clone())),
            None => self.add_token(
                self.next_token(TT::Identifier)
                    .with_literal(Literal::Identifier(text)),
            ),
        }
    }

    fn number(&mut self) {
        self.advance_while(&is_digit);

        // Look for a fractional part
        let next_c = self.peek_next();
        if self.peek() == '.' && is_digit(next_c) {
            // Consume the "."
            self.advance();

            self.advance_while(&is_digit);
        }

        let text = self.source[self.start..self.current].to_string();
        let num: f64 = text.parse().unwrap();
        self.add_token(
            self.next_token(TT::Number)
                .with_literal(Literal::Number(num)),
        )
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            self.error("Unterminated string");
        } else {
            self.advance();

            let text = self.source[self.start + 1..self.current - 1].to_string();
            self.add_token(
                self.next_token(TT::String)
                    .with_literal(Literal::String(text)),
            )
        }
    }

    fn match_result(&mut self, expected: char, left: TokenType, right: TokenType) {
        let matches = self.check_match(expected);
        let t = if matches {
            self.next_token(left)
        } else {
            self.next_token(right)
        };

        self.add_token(t);
    }

    fn check_match(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn peek(&mut self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.chars[self.current]
        }
    }

    fn peek_next(&mut self) -> char {
        if self.current + 1 >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.current + 1]
        }
    }

    fn advance_while(&mut self, pred: &dyn Fn(char) -> bool) -> String {
        let mut text: String = "".to_string();
        while pred(self.peek()) && !self.is_at_end() {
            text.push(self.advance());
        }

        text
    }

    fn advance(&mut self) -> char {
        let c = self.chars[self.current];
        self.current += 1;
        c
    }

    fn add_token(&mut self, token: Token) {
        self.tokens.push(token)
    }

    fn next_token(&self, token_type: TokenType) -> Token {
        let text = self.source[self.start..self.current].to_string();
        Token::new(token_type, text, self.line)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn error(&mut self, msg: &str) {
        self.errors
            .push(ScannerError::new(self.line, msg.to_string()))
    }
}

fn is_alpha_numeric(c: char) -> bool {
    is_digit(c) || is_alpha(c)
}

fn is_digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}

fn is_alpha(c: char) -> bool {
    ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_'
}

impl ScannerError {
    pub fn new(line: usize, msg: String) -> ScannerError {
        ScannerError { line, msg }
    }
}

impl fmt::Display for ScannerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[line {}] Error {}", self.line, self.msg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(s: String) -> Vec<Token> {
        Scanner::lex(&s).unwrap()
    }

    #[test]
    fn test_whitespace() {
        let tokens = lex(" \t\r\n\t\r  ".to_string());
        assert_eq!(tokens, vec![Token::new(TT::Eof, "".to_string(), 1)]);
    }

    #[test]
    fn test_comments() {
        let tokens = lex("# hello world".to_string());
        assert_eq!(
            tokens,
            vec![
                Token::new(TT::Comment, " hello world".to_string(), 0,)
                    .with_literal(Literal::Comment("hello world".to_string())),
                Token::new(TT::Eof, "".to_string(), 0)
            ]
        );
    }

    #[test]
    fn test_single_letter_operators() {
        let tokens = lex("+ - * / ( ) { } . , ! ; = < >".to_string());
        assert_eq!(
            tokens,
            vec![
                Token::new(TT::Plus, "+".to_string(), 0),
                Token::new(TT::Minus, "-".to_string(), 0),
                Token::new(TT::Star, "*".to_string(), 0),
                Token::new(TT::Slash, "/".to_string(), 0),
                Token::new(TT::LeftParen, "(".to_string(), 0),
                Token::new(TT::RightParen, ")".to_string(), 0),
                Token::new(TT::LeftBrace, "{".to_string(), 0),
                Token::new(TT::RightBrace, "}".to_string(), 0),
                Token::new(TT::Dot, ".".to_string(), 0),
                Token::new(TT::Comma, ",".to_string(), 0),
                Token::new(TT::Bang, "!".to_string(), 0),
                Token::new(TT::Semicolon, ";".to_string(), 0),
                Token::new(TT::Equal, "=".to_string(), 0),
                Token::new(TT::Less, "<".to_string(), 0),
                Token::new(TT::Greater, ">".to_string(), 0),
                Token::new(TT::Eof, "".to_string(), 0)
            ]
        );
    }

    #[test]
    fn test_equal_operators() {
        let tokens = lex("== != <= >=".to_string());
        assert_eq!(
            tokens,
            vec![
                Token::new(TT::EqualEqual, "==".to_string(), 0),
                Token::new(TT::BangEqual, "!=".to_string(), 0),
                Token::new(TT::LessEqual, "<=".to_string(), 0),
                Token::new(TT::GreaterEqual, ">=".to_string(), 0),
                Token::new(TT::Eof, "".to_string(), 0)
            ]
        );
    }

    #[test]
    fn test_strings() {
        let tokens = lex("\"hello world\" \"123 -+() . '\"".to_string());
        assert_eq!(
            tokens,
            vec![
                Token::new(TT::String, "\"hello world\"".to_string(), 0)
                    .with_literal(Literal::String("hello world".to_string())),
                Token::new(TT::String, "\"123 -+() . '\"".to_string(), 0)
                    .with_literal(Literal::String("123 -+() . '".to_string())),
                Token::new(TT::Eof, "".to_string(), 0)
            ]
        );
    }

    #[test]
    fn test_numbers() {
        let tokens = lex("1 123 123.456".to_string());
        assert_eq!(
            tokens,
            vec![
                Token::new(TT::Number, "1".to_string(), 0).with_literal(Literal::Number(1.0)),
                Token::new(TT::Number, "123".to_string(), 0).with_literal(Literal::Number(123.0)),
                Token::new(TT::Number, "123.456".to_string(), 0)
                    .with_literal(Literal::Number(123.456)),
                Token::new(TT::Eof, "".to_string(), 0)
            ]
        );
    }

    #[test]
    fn test_keywords() {
        let tokens = lex(
            "and class else false for fun if nil or print return super this true var while"
                .to_string(),
        );
        assert_eq!(
            tokens,
            vec![
                Token::new(TT::And, "and".to_string(), 0),
                Token::new(TT::Class, "class".to_string(), 0),
                Token::new(TT::Else, "else".to_string(), 0),
                Token::new(TT::False, "false".to_string(), 0),
                Token::new(TT::For, "for".to_string(), 0),
                Token::new(TT::Fun, "fun".to_string(), 0),
                Token::new(TT::If, "if".to_string(), 0),
                Token::new(TT::Nil, "nil".to_string(), 0),
                Token::new(TT::Or, "or".to_string(), 0),
                Token::new(TT::Print, "print".to_string(), 0),
                Token::new(TT::Return, "return".to_string(), 0),
                Token::new(TT::Super, "super".to_string(), 0),
                Token::new(TT::This, "this".to_string(), 0),
                Token::new(TT::True, "true".to_string(), 0),
                Token::new(TT::Var, "var".to_string(), 0),
                Token::new(TT::While, "while".to_string(), 0),
                Token::new(TT::Eof, "".to_string(), 0)
            ]
        );
    }

    #[test]
    fn test_identifiers() {
        let tokens = lex("a boop hello world".to_string());
        assert_eq!(
            tokens,
            vec![
                Token::new(TT::Identifier, "a".to_string(), 0)
                    .with_literal(Literal::Identifier("a".to_string())),
                Token::new(TT::Identifier, "boop".to_string(), 0)
                    .with_literal(Literal::Identifier("boop".to_string())),
                Token::new(TT::Identifier, "hello".to_string(), 0)
                    .with_literal(Literal::Identifier("hello".to_string())),
                Token::new(TT::Identifier, "world".to_string(), 0)
                    .with_literal(Literal::Identifier("world".to_string())),
                Token::new(TT::Eof, "".to_string(), 0)
            ]
        );
    }
}
