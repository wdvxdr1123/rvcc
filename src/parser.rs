use std::iter::Peekable;

use crate::error::{Result, SyntaxError};
use crate::position::Position;
use crate::scanner::Token;
use crate::scanner::TokenKind::{self, *};

pub enum Expr {
    Binary {
        // lhs op rhs
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unay {
        op: BinOp,
        expr: Box<Expr>,
    },
    Number(usize),
}

pub enum Stmt {
    Expr(Box<Expr>),
}

pub enum BinOp {
    ADD,
    SUB,
    MUL,
    DIV,
    LT,
    LE,
    EQ,
    NE,
}

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
    position: Position,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(tokens: Peekable<T>) -> Self {
        Parser {
            tokens,
            position: Position { line: 0, column: 0 },
        }
    }

    fn peek<'a>(&mut self) -> Result<Token> {
        if let Some(tok) = self.tokens.peek() {
            self.position = tok.position;
            Ok(tok.clone())
        } else {
            Err(self.error("unexpected eof".to_string()))
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        let tok = self.peek()?;
        if tok.kind == kind {
            self.tokens.next();
            return Ok(());
        }
        return Err(self.error(format!("expected {}", kind)));
    }

    pub fn stmts(&mut self) -> Result<Vec<Stmt>> {
        let mut s = vec![];
        while self.peek()?.kind != EOF {
            s.push(self.stmt()?);
        }
        Ok(s)
    }

    // expr-statement =  expr ';'
    fn stmt(&mut self) -> Result<Stmt> {
        let expr = self.expr()?;
        self.expect(SEMICOLON)?;
        Ok(Stmt::Expr(expr.into()))
    }

    fn expr(&mut self) -> Result<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut node = self.relational()?;

        loop {
            let tok = self.peek()?;
            match tok.kind {
                EQ => {
                    self.expect(EQ)?;
                    node = Expr::Binary {
                        op: BinOp::EQ,
                        lhs: node.into(),
                        rhs: self.add()?.into(),
                    };
                }
                NEQ => {
                    self.expect(NEQ)?;
                    node = Expr::Binary {
                        op: BinOp::NE,
                        lhs: node.into(),
                        rhs: self.add()?.into(),
                    };
                }
                _ => break Ok(node),
            }
        }
    }

    fn relational(&mut self) -> Result<Expr> {
        let mut node = self.add()?;

        loop {
            let tok = self.peek()?;
            match tok.kind {
                LSS => {
                    self.expect(LSS)?;
                    node = Expr::Binary {
                        op: BinOp::LT,
                        lhs: node.into(),
                        rhs: self.add()?.into(),
                    };
                }
                LEQ => {
                    self.expect(LEQ)?;
                    node = Expr::Binary {
                        op: BinOp::LE,
                        lhs: node.into(),
                        rhs: self.add()?.into(),
                    };
                }
                GTR => {
                    self.expect(GTR)?;
                    node = Expr::Binary {
                        op: BinOp::LT,
                        lhs: self.add()?.into(),
                        rhs: node.into(),
                    };
                }
                GEQ => {
                    self.expect(GEQ)?;
                    node = Expr::Binary {
                        op: BinOp::LE,
                        lhs: self.add()?.into(),
                        rhs: node.into(),
                    };
                }
                _ => break Ok(node),
            }
        }
    }

    pub fn add(&mut self) -> Result<Expr> {
        let mut node = self.mul()?;

        loop {
            let tok = self.peek()?;
            match tok.kind {
                Add => {
                    self.expect(Add)?;
                    node = Expr::Binary {
                        op: BinOp::ADD,
                        lhs: node.into(),
                        rhs: self.mul()?.into(),
                    };
                }
                Sub => {
                    self.expect(Sub)?;
                    node = Expr::Binary {
                        op: BinOp::SUB,
                        lhs: node.into(),
                        rhs: self.mul()?.into(),
                    };
                }
                _ => break Ok(node),
            }
        }
    }

    fn unary(&mut self) -> Result<Expr> {
        let tok = self.peek()?;
        match tok.kind {
            Add => {
                self.expect(Add)?;
                return self.unary();
            }
            Sub => {
                self.expect(Sub)?;
                return Ok(Expr::Unay {
                    op: BinOp::SUB,
                    expr: self.unary()?.into(),
                });
            }
            _ => {}
        };
        self.primary_expr()
    }

    fn primary_expr(&mut self) -> Result<Expr> {
        let tok = self.peek()?;
        match tok.kind {
            LParen => {
                self.expect(LParen)?;
                let expr = self.expr()?;
                self.expect(RParen)?;
                return Ok(expr);
            }
            Number => {
                //"".parse::<usize>().map_err(|_err| {
                //   SyntaxError{
                //        pos: tok.position,
                //        msg: "invalid number".to_string(),
                //    }
                //})?
                let num = tok.num;
                self.expect(Number)?;
                return Ok(Expr::Number(num));
            }
            _ => Err(self.error(format!("expected expresssion"))),
        }
    }

    fn mul(&mut self) -> Result<Expr> {
        let mut node = self.unary()?;

        loop {
            let tok = self.peek()?;
            match tok.kind {
                Mul => {
                    self.expect(Mul)?;
                    node = Expr::Binary {
                        op: BinOp::MUL,
                        lhs: node.into(),
                        rhs: self.unary()?.into(),
                    };
                }
                Div => {
                    self.expect(Div)?;
                    node = Expr::Binary {
                        op: BinOp::DIV,
                        lhs: node.into(),
                        rhs: self.unary()?.into(),
                    };
                }
                _ => break Ok(node),
            }
        }
    }

    fn error(&self, msg: String) -> SyntaxError {
        SyntaxError {
            pos: self.position,
            msg: msg,
        }
    }
}
