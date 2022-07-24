use std::iter::Peekable;

use crate::compiler::{Func, Object};
use crate::error::{Result, SyntaxError};
use crate::position::Position;
use crate::scanner::Token;
use crate::scanner::TokenKind::{self, *};

pub trait Node {
    fn pos(&self) -> Position;
}

#[derive(Clone)]
pub enum Expr {
    Binary {
        // lhs op rhs
        op: BinOp,
        pos: Position,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        op: BinOp,
        pos: Position,
        expr: Box<Expr>,
    },
    Number {
        pos: Position,
        value: usize,
    },
    Ident {
        pos: Position,
        name: String,
    },
    Assign {
        lhs: Box<Expr>,
        pos: Position,
        rhs: Box<Expr>,
    },
}

impl Node for Expr {
    fn pos(&self) -> Position {
        match self {
            Self::Binary { pos, .. } => pos.clone(),
            Self::Unary { pos, .. } => pos.clone(),
            Self::Number { pos, .. } => pos.clone(),
            Self::Ident { pos, .. } => pos.clone(),
            Self::Assign { pos, .. } => pos.clone(),
        }
    }
}

#[derive(Clone)]
pub enum Stmt {
    Expr {
        expr: Box<Expr>,
        semicolon: Position,
    },
    Return {
        pos: Position,
        expr: Box<Expr>,
        semicolon: Position,
    },
    Block {
        lbrace: Position,
        rbrace: Position,
        body: Vec<Stmt>,
    },
    If {
        pos: Position,
        lparen: Position,
        cond: Box<Expr>,
        rparen: Position,
        then: Box<Stmt>,
        r#else: Option<Box<Stmt>>,
    },
    For {
        pos: Position,
        lparen: Position,
        init: Option<Box<Stmt>>,
        cond: Option<Box<Expr>>,
        post: Option<Box<Expr>>,
        rparen: Position,
        body: Box<Stmt>,
    },
    None(Position),
}

impl Node for Stmt {
    fn pos(&self) -> Position {
        match self {
            Stmt::Expr { expr, .. } => expr.pos(),
            Stmt::Block { lbrace, .. } => lbrace.clone(),
            Stmt::Return { pos, .. } => pos.clone(),
            Stmt::If { pos, .. } => pos.clone(),
            Stmt::For { pos, .. } => pos.clone(),
            Stmt::None(pos) => pos.clone(),
        }
    }
}

#[derive(Clone)]
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
    objs: Vec<String>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(tokens: Peekable<T>) -> Self {
        Parser {
            tokens,
            position: Position { line: 0, column: 0 },
            objs: vec![],
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

    fn expect(&mut self, kind: TokenKind) -> Result<Position> {
        let tok = self.peek()?;
        if tok.kind == kind {
            self.tokens.next();
            return Ok(tok.position);
        }
        return Err(self.error(format!("expected {}", kind)));
    }

    pub fn function(&mut self) -> Result<Func> {
        let mut func = Func {
            ..Default::default()
        };
        func.body = self.stmts()?;

        for o in self.objs.iter() {
            func.objs.push(Object {
                name: o.clone(),
                offset: 0,
            })
        }

        Ok(func)
    }

    fn ident(&mut self, name: &String) {
        if !self.objs.contains(name) {
            self.objs.push(name.clone())
        }
    }

    pub fn stmts(&mut self) -> Result<Vec<Stmt>> {
        let mut s = vec![];
        while self.peek()?.kind != EOF {
            s.push(self.stmt()?);
        }
        Ok(s)
    }

    // stmt = "return" expr ";"
    //      | "if" "(" expr ")" stmt ("else" stmt)?
    //      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
    //      | "while" "(" expr ")" stmt
    //      | compound-stmt
    //      | expr-stmt
    fn stmt(&mut self) -> Result<Stmt> {
        let tok = self.peek()?;
        match tok.kind {
            // "return" expr ";"
            RETURN => {
                let pos = self.expect(RETURN)?;
                let expr = Box::new(self.expr()?);
                let semi = self.expect(SEMICOLON)?;
                return Ok(Stmt::Return {
                    pos,
                    expr,
                    semicolon: semi,
                });
            }
            // "if" "(" expr ")" stmt ("else" stmt)?
            IF => {
                let pos = self.expect(IF)?;

                let lparen = self.expect(LPAREN)?;
                let cond = Box::new(self.expr()?);
                let rparen = self.expect(RPAREN)?;

                let then = Box::new(self.stmt()?);

                let els = if self.peek()?.kind == ELSE {
                    self.expect(ELSE)?;
                    Some(Box::new(self.stmt()?))
                } else {
                    None
                };

                Ok(Stmt::If {
                    pos,
                    lparen,
                    cond,
                    rparen,
                    then,
                    r#else: els,
                })
            }
            // "for" "(" expr-stmt expr? ";" expr? ")" stmt
            FOR => {
                let pos = self.expect(FOR)?;
                let lparen = self.expect(LPAREN)?;
                let init = Box::new(self.expr_stmt()?);

                let cond = if self.peek()?.kind != SEMICOLON {
                    Some(Box::new(self.expr()?))
                } else {
                    None
                };
                self.expect(SEMICOLON)?;

                let post = if self.peek()?.kind != RPAREN {
                    Some(Box::new(self.expr()?))
                } else {
                    None
                };
                let rparen = self.expect(RPAREN)?;

                let body = Box::new(self.stmt()?);

                Ok(Stmt::For {
                    pos,
                    lparen,
                    init: Some(init.into()),
                    cond,
                    post,
                    rparen,
                    body,
                })
            }
            // "while" "(" expr ")" stmt
            WHILE => {
                let pos = self.expect(WHILE)?;

                let lparen = self.expect(LPAREN)?;
                let cond = self.expr()?;
                let rparen = self.expect(RPAREN)?;

                let body = Box::new(self.stmt()?);

                Ok(Stmt::For {
                    pos,
                    lparen,
                    init: None,
                    cond: Some(cond.into()),
                    post: None,
                    rparen,
                    body,
                })
            }
            LBRACE => self.compound_stmt(),
            _ => self.expr_stmt(),
        }
    }

    // compound-stmt = "{" stmt* "}"
    fn compound_stmt(&mut self) -> Result<Stmt> {
        let lbrace = self.expect(LBRACE)?;

        let mut stmt = vec![];
        loop {
            match self.peek()?.kind {
                RBRACE => break,
                _ => stmt.push(self.stmt()?),
            }
        }
        let rbrace = self.expect(RBRACE)?;
        Ok(Stmt::Block {
            lbrace,
            rbrace,
            body: stmt,
        })
    }

    // expr-stmt =  expr? ';'
    fn expr_stmt(&mut self) -> Result<Stmt> {
        if self.peek()?.kind == SEMICOLON {
            let pos = self.expect(SEMICOLON)?;
            return Ok(Stmt::None(pos));
        }

        let expr = self.expr()?;
        Ok(Stmt::Expr {
            expr: expr.into(),
            semicolon: self.expect(SEMICOLON)?,
        })
    }

    fn expr(&mut self) -> Result<Expr> {
        self.assign()
    }

    fn assign(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;
        if self.peek()?.kind == Assign {
            let pos = self.expect(Assign)?;
            expr = Expr::Assign {
                lhs: expr.into(),
                pos,
                rhs: self.assign()?.into(),
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut node = self.relational()?;

        loop {
            let tok = self.peek()?;
            match tok.kind {
                EQ => {
                    let pos = self.expect(EQ)?;
                    node = Expr::Binary {
                        op: BinOp::EQ,
                        pos,
                        lhs: node.into(),
                        rhs: self.add()?.into(),
                    };
                }
                NEQ => {
                    let pos = self.expect(NEQ)?;
                    node = Expr::Binary {
                        op: BinOp::NE,
                        pos,
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
                    let pos = self.expect(LSS)?;
                    node = Expr::Binary {
                        op: BinOp::LT,
                        pos,
                        lhs: node.into(),
                        rhs: self.add()?.into(),
                    };
                }
                LEQ => {
                    let pos = self.expect(LEQ)?;
                    node = Expr::Binary {
                        op: BinOp::LE,
                        pos,
                        lhs: node.into(),
                        rhs: self.add()?.into(),
                    };
                }
                GTR => {
                    let pos = self.expect(GTR)?;
                    node = Expr::Binary {
                        op: BinOp::LT,
                        pos,
                        lhs: self.add()?.into(),
                        rhs: node.into(),
                    };
                }
                GEQ => {
                    let pos = self.expect(GEQ)?;
                    node = Expr::Binary {
                        op: BinOp::LE,
                        pos,
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
                    let pos = self.expect(Add)?;
                    node = Expr::Binary {
                        op: BinOp::ADD,
                        pos,
                        lhs: node.into(),
                        rhs: self.mul()?.into(),
                    };
                }
                Sub => {
                    let pos = self.expect(Sub)?;
                    node = Expr::Binary {
                        op: BinOp::SUB,
                        pos,
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
                let pos = self.expect(Add)?;
                return Ok(Expr::Unary {
                    op: BinOp::ADD,
                    pos,
                    expr: self.unary()?.into(),
                });
            }
            Sub => {
                let pos = self.expect(Sub)?;
                return Ok(Expr::Unary {
                    op: BinOp::SUB,
                    pos,
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
            LPAREN => {
                self.expect(LPAREN)?;
                let expr = self.expr()?;
                self.expect(RPAREN)?;
                return Ok(expr);
            }
            NUMBER => {
                let pos = self.expect(NUMBER)?;
                let value = tok
                    .literal
                    .parse::<usize>()
                    .map_err(|_err| self.error("invalid number".to_string()))?;
                return Ok(Expr::Number { pos, value });
            }
            IDENT => {
                let pos = self.expect(IDENT)?;
                self.ident(&tok.literal);
                return Ok(Expr::Ident {
                    pos,
                    name: tok.literal,
                });
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
                    let pos = self.expect(Mul)?;
                    node = Expr::Binary {
                        op: BinOp::MUL,
                        pos,
                        lhs: node.into(),
                        rhs: self.unary()?.into(),
                    };
                }
                Div => {
                    let pos = self.expect(Div)?;
                    node = Expr::Binary {
                        op: BinOp::DIV,
                        pos,
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
