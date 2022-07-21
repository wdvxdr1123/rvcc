use core::fmt;
use std::{iter::Peekable, str::Chars};

use crate::error::{SyntaxError, Result};

pub enum Token {
    Punct(char),
    Num(usize),
    EOF,
}

pub struct Tokenizer<'a> {
    p: Peekable<Chars<'a>>,
    line: usize,
    col: usize,
}

impl Iterator for Tokenizer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(ch) = self.p.peek() {
            match ch {
                ' ' | '\t' => {
                    // skip whitespace
                    self.next_char();
                    continue;
                }
                '+' | '-' => {
                    let tok = Token::Punct(*ch);
                    self.next_char();
                    return Some(tok);
                }
                '0'..='9' => {
                    let mut num: usize = 0;
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
                    return Some(Token::Num(num));
                }
                _ => {
                    return Some(Token::EOF);
                }
            }
        }
        None
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(p: Peekable<Chars<'a>>) -> Self {
        Tokenizer {
            p: p,
            line: 1,
            col: 0,
        }
    }

    pub fn next_char(&mut self) -> Option<char> {
        let ch = self.p.next();
        if ch.is_some() {
            self.col = self.col + 1;
        }
        ch
    }

    fn error(&self, msg:String) -> SyntaxError {
        SyntaxError{
            line: self.line,
            col: self.col,
            msg
        }
    }

    pub fn expect_num(&mut self) -> Result<usize> {
        let tok = self.next().unwrap();
        if let Token::Num(i)= tok {
            Ok(i)
        } else {
            Err(self.error(format!("expect number but got {}", tok)))
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Punct(c) => write!(f, "{}", *c),
            Token::Num(i) => write!(f, "{}", i),
            Token::EOF => write!(f, "EOF"),
        }
    }
}
