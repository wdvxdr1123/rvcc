use std::iter::Peekable;

use crate::error::{Result, SyntaxError};
use crate::scanner::Token;
use crate::scanner::{
    TokenKind::{self, *},
};

pub enum Node {
    BinaryExpr {
        op: BinaryOperator,
        lhs: Box<Node>,
        rhs: Box<Node>,
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
        Parser {
            tokens,
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        if let Some(tok) = self.tokens.next() {
            if tok.kind == kind {
                return Ok(());
            }
            return Err(SyntaxError {
                pos: tok.position,
                msg: format!("expected {}", kind),
            });
        }
        Err(SyntaxError {
            pos: crate::position::Position { line: 0, column: 0 },
            msg: "unexpected eof".to_string(),
        })
    }

    pub fn expr(&mut self) -> Result<Node> {
        let mut node = self.mul()?;

        loop {
            let tok = self.tokens.peek().unwrap();
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

    fn primary_expr(&mut self) -> Result<Node> {
        let tok = self.tokens.peek().unwrap();
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
        let mut node = self.primary_expr()?;

        loop {
            let tok = self.tokens.peek().unwrap();
            match tok.kind {
                Mul => {
                    self.expect(Mul)?;
                    node = Node::BinaryExpr {
                        op: BinaryOperator::MUL,
                        lhs: node.into(),
                        rhs: self.primary_expr()?.into(),
                    };
                }
                Div => {
                    self.expect(Div)?;
                    node = Node::BinaryExpr {
                        op: BinaryOperator::DIV,
                        lhs: node.into(),
                        rhs: self.primary_expr()?.into(),
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
