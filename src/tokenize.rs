use std::{iter::Peekable, str::Chars};

pub enum Token {
    Punct(char),
    Num(usize),
    EOF,
}

impl Token {
    pub fn num(&self) -> Option<usize> {
        if let Token::Num(i) = self {
            return Some(*i);
        }
        None
    }
}

pub struct Tokenizer<'a> {
    p: Peekable<Chars<'a>>,
}

impl Iterator for Tokenizer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(ch) = self.p.peek() {
            match ch {
                ' ' | '\t' => {
                    // skip whitespace
                    self.p.next();
                    continue;
                }
                '+' | '-' => {
                    let tok = Token::Punct(*ch);
                    self.p.next();
                    return Some(tok);
                }
                '0'..='9' => {
                    let mut num: usize = 0;
                    while let Some(ch) = self.p.peek() {
                        match ch {
                            '0'..='9' => {
                                num = num * 10 + *ch as usize - '0' as usize;
                                self.p.next();
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
        Tokenizer { p: p }
    }
}
