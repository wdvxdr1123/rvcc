use std::{fmt, iter::Peekable, str::Chars};

use crate::{error::Result, position::Position};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    Invalid,
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /

    LParen, // (
    RParen, // )

    EQ,  // ==
    NEQ, // !=
    GEQ, // >=
    LEQ, // <=

    GTR, // >
    LSS, // <

    Assign, // =

    NOT, // !

    Number,
    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
    pub num: usize,
}

pub struct Scanner<'a> {
    p: Peekable<Chars<'a>>,
    line: usize,
    col: usize,

    tok0: TokenKind,

    pub number: usize,
    pub string: String,
}

impl<'a> Scanner<'a> {
    pub fn new(p: Peekable<Chars<'a>>) -> Self {
        Scanner {
            p,
            line: 1,
            col: 0,
            tok0: TokenKind::Invalid,
            number: 0,
            string: "".to_string(),
        }
    }

    pub fn scan(&mut self) -> Result<Vec<Token>> {
        let mut tokens = vec![];
        loop {
            self.step();
            let mut tok = Token {
                kind: self.tok0,
                position: self.pos(),
                num: 0,
            };
            if tok.kind == TokenKind::Number {
                tok.num = self.number;
            }
            tokens.push(tok.clone());
            if tok.kind == TokenKind::Invalid || tok.kind == TokenKind::EOF {
                break;
            }
        }
        Ok(tokens)
    }

    fn step(&mut self) {
        use TokenKind::*;
        let mut num: usize = 0;
        let tok: TokenKind = loop {
            let ch = self.p.peek();
            if ch.is_none() {
                break Invalid;
            }
            match ch.unwrap() {
                '\n' => {
                    self.next_char();
                    self.line = self.line +1;
                    self.col = 0;
                    continue;
                }
                ' ' | '\t' => {
                    // skip whitespace
                    self.next_char();
                    continue;
                }
                c @ ('+' | '-' | '*' | '/' | '(' | ')') => {
                    let tok = match *c {
                        '+' => Add,
                        '-' => Sub,
                        '*' => Mul,
                        '/' => Div,
                        '(' => LParen,
                        ')' => RParen,
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
                '0'..='9' => {
                    while let Some(ch) = self.p.peek() {
                        match ch {
                            '0'..='9' => {
                                num = num * 10 + *ch as usize - '0' as usize;
                                self.next_char();
                            }
                            _ => {
                                break;
                            }
                        }
                    }
                    break Number;
                }
                _ => {
                    break EOF;
                }
            }
        };

        self.tok0 = tok;
        self.number = num;
    }

    pub fn next_char(&mut self) -> Option<char> {
        let ch = self.p.next();
        if ch.is_some() {
            self.col = self.col + 1;
        }
        ch
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
