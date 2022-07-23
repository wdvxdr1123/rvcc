use std::{fmt, iter::Peekable, str::Chars};

use crate::{error::Result, position::Position};

use phf::phf_map;

static KEYWORDS: phf::Map<&'static str, TokenKind> = phf_map! {
    "return" => TokenKind::RETURN,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Invalid,

    IDENT,
    NUMBER,

    Add, // +
    Sub, // -
    Mul, // *
    Div, // /

    LPAREN, // (
    RPAREN, // )
    LBRACK, // [
    RBRACK, // ]
    LBRACE, // {
    RBRACE, // }

    EQ,  // ==
    NEQ, // !=
    GEQ, // >=
    LEQ, // <=

    GTR, // >
    LSS, // <

    Assign, // =

    NOT,       // !
    SEMICOLON, // ;

    // keywords
    RETURN,

    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
    pub literal: String,
}

pub struct Scanner<'a> {
    p: Peekable<Chars<'a>>,
    line: usize,
    col: usize,

    tok0: TokenKind,
    pub literal: String,
}

impl<'a> Scanner<'a> {
    pub fn new(p: Peekable<Chars<'a>>) -> Self {
        Scanner {
            p,
            line: 1,
            col: 0,
            tok0: TokenKind::Invalid,
            literal: "".to_string(),
        }
    }

    pub fn scan(&mut self) -> Result<Vec<Token>> {
        let mut tokens = vec![];
        loop {
            self.step();
            let tok = Token {
                kind: self.tok0,
                position: self.pos(),
                literal: self.literal.clone(),
            };
            let b = tok.kind == TokenKind::Invalid || tok.kind == TokenKind::EOF;
            tokens.push(tok);
            if b {
                break;
            }
        }
        Ok(tokens)
    }

    fn step(&mut self) {
        use TokenKind::*;
        let tok: TokenKind = loop {
            let ch = self.p.peek();
            if ch.is_none() {
                break EOF;
            }
            match ch.unwrap() {
                '\n' => {
                    self.next_char();
                    self.line = self.line + 1;
                    self.col = 0;
                    continue;
                }
                ' ' | '\t' => {
                    // skip whitespace
                    self.next_char();
                    continue;
                }
                'a'..='z' | 'A'..='Z' => {
                    break self.ident();
                }
                c @ ('(' | ')' | '[' | ']' | '{' | '}') => {
                    let tok = match *c {
                        '(' => LPAREN,
                        ')' => RPAREN,
                        '[' => LBRACK,
                        ']' => RBRACK,
                        '{' => LBRACE,
                        '}' => RBRACE,
                        _ => unreachable!(),
                    };
                    self.next_char();
                    break tok;
                }
                c @ ('+' | '-' | '*' | '/' | ';') => {
                    let tok = match *c {
                        '+' => Add,
                        '-' => Sub,
                        '*' => Mul,
                        '/' => Div,
                        ';' => SEMICOLON,
                        _ => unreachable!(),
                    };
                    self.next_char();
                    break tok;
                }
                '=' => {
                    self.next_char();
                    break self.switch2(Assign, '=', EQ);
                }
                '>' => {
                    self.next_char();
                    break self.switch2(GTR, '=', GEQ);
                }
                '<' => {
                    self.next_char();
                    break self.switch2(LSS, '=', LEQ);
                }
                '!' => {
                    self.next_char();
                    break self.switch2(NOT, '=', NEQ);
                }
                '0'..='9' => break self.number(),
                _ => {
                    break EOF;
                }
            }
        };
        self.tok0 = tok;
    }

    pub fn next_char(&mut self) -> Option<char> {
        let ch = self.p.next();
        if ch.is_some() {
            self.col = self.col + 1;
        }
        ch
    }

    fn number(&mut self) -> TokenKind {
        let mut literal = vec![];
        while let Some(&ch) = self.p.peek() {
            match ch {
                '0'..='9' => {
                    literal.push(ch);
                    self.next_char();
                }
                _ => break,
            }
        }
        self.literal = literal.iter().collect();
        TokenKind::NUMBER
    }

    fn ident(&mut self) -> TokenKind {
        let mut literal = vec![];
        while let Some(&ch) = self.p.peek() {
            match ch {
                '0'..='9' | 'a'..='z' | 'A'..='Z' => {
                    literal.push(ch);
                    self.next_char();
                }
                _ => break,
            }
        }
        let lit = literal.iter().collect::<String>();
        if let Some(keyword) = KEYWORDS.get(lit.as_str()) {
            return *keyword;
        }
        self.literal = lit;
        TokenKind::IDENT
    }

    pub fn pos(&self) -> Position {
        Position {
            line: self.line,
            column: self.col,
        }
    }

    fn switch2(&mut self, tok0: TokenKind, c: char, tok1: TokenKind) -> TokenKind {
        match self.p.peek() {
            Some(&_c) if _c == c => {
                self.next_char();
                tok1
            }
            _ => tok0,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)?;
        Ok(())
    }
}
