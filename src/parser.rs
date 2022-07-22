use std::iter::Peekable;

use crate::error::{Result, SyntaxError};
use crate::scanner::Token;
use crate::scanner::TokenKind::{self, *};

pub enum Node {
    BinaryExpr {
        op: BinaryOperator,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Unay {
        op: BinaryOperator,
        expr: Box<Node>,
    },
    Number(usize),
}

pub enum BinaryOperator {
    ADD,
    SUB,
    MUL,
    DIV,
}

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(tokens: Peekable<T>) -> Self {
        Parser { tokens }
    }

    fn peek<'a>(&mut self) -> Result<Token> {
        if let Some(tok) = self.tokens.peek() {
            Ok(tok.clone())
        } else {
            Err(SyntaxError {
                pos: crate::position::Position { line: 0, column: 0 },
                msg: "unexpected eof".to_string(),
            })
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        let tok = self.peek()?;
        if tok.kind == kind {
            self.tokens.next();
            return Ok(());
        }
        return Err(SyntaxError {
            pos: tok.position,
            msg: format!("expected {}", kind),
        });
    }

    pub fn expr(&mut self) -> Result<Node> {
        let mut node = self.mul()?;

        loop {
            let tok = self.peek()?;
            match tok.kind {
                Add => {
                    self.expect(Add)?;
                    node = Node::BinaryExpr {
                        op: BinaryOperator::ADD,
                        lhs: node.into(),
                        rhs: self.mul()?.into(),
                    };
                }
                Sub => {
                    self.expect(Sub)?;
                    node = Node::BinaryExpr {
                        op: BinaryOperator::SUB,
                        lhs: node.into(),
                        rhs: self.mul()?.into(),
                    };
                }
                _ => break Ok(node),
            }
        }
    }

    fn unary(&mut self) -> Result<Node> {
        let tok = self.peek()?;
        match tok.kind {
            Add => {
                self.expect(Add)?;
                return self.unary();
            }
            Sub => {
                self.expect(Sub)?;
                return Ok(Node::Unay {
                    op: BinaryOperator::SUB,
                    expr: self.unary()?.into(),
                });
            }
            _ => {}
        };
        self.primary_expr()
    }

    fn primary_expr(&mut self) -> Result<Node> {
        let tok = self.peek()?;
        if tok.kind == LParen {
            self.expect(LParen)?;
            let expr = self.expr()?;
            self.expect(RParen)?;
            return Ok(expr);
        }

        if tok.kind == Number {
            //"".parse::<usize>().map_err(|_err| {
            //   SyntaxError{
            //        pos: tok.position,
            //        msg: "invalid number".to_string(),
            //    }
            //})?
            let num = tok.num;
            self.expect(Number)?;
            return Ok(Node::Number(num));
        }

        Err(self.error(format!("expected expresssion")))
    }

    fn mul(&mut self) -> Result<Node> {
        let mut node = self.unary()?;

        loop {
            let tok = self.peek()?;
            match tok.kind {
                Mul => {
                    self.expect(Mul)?;
                    node = Node::BinaryExpr {
                        op: BinaryOperator::MUL,
                        lhs: node.into(),
                        rhs: self.unary()?.into(),
                    };
                }
                Div => {
                    self.expect(Div)?;
                    node = Node::BinaryExpr {
                        op: BinaryOperator::DIV,
                        lhs: node.into(),
                        rhs: self.unary()?.into(),
                    };
                }
                _ => break Ok(node),
            }
        }
    }

    fn error(&self, msg: String) -> SyntaxError {
        todo!()
    }
}
