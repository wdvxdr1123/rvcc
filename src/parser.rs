use std::collections::HashMap;
use std::iter::Peekable;

use crate::compiler::Func;
use crate::error::{Result, SyntaxError};
use crate::position::Position;
use crate::scanner::Token;
use crate::scanner::TokenKind::{self, *};
use crate::typecheck::Type;

pub trait Node {
    fn pos(&self) -> Position;
}

#[derive(Clone, Debug)]
pub enum Expr {
    Binary {
        // lhs op rhs
        op: BinOp,
        pos: Position,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        ty: Type,
    },
    Unary {
        op: UnaryOp,
        pos: Position,
        expr: Box<Expr>,

        ty: Type,
    },
    Number {
        pos: Position,
        value: usize,
        ty: Type,
    },
    Ident {
        pos: Position,
        name: String,
        ty: Type,
    },
    Assign {
        lhs: Box<Expr>,
        pos: Position,
        rhs: Box<Expr>,
        ty: Type,
    },
    Call {
        pos: Position,
        name: String,
        ty: Type,
    },
}

impl Expr {
    pub fn typecheck(&self) -> &Type {
        match self {
            Self::Binary { ty, .. } => ty,
            Self::Unary { ty, .. } => ty,
            Self::Number { ty, .. } => ty,
            Self::Ident { ty, .. } => ty,
            Self::Assign { ty, .. } => ty,
            Self::Call { ty, .. } => ty,
        }
    }
}

impl Node for Expr {
    fn pos(&self) -> Position {
        match self {
            Self::Binary { pos, .. } => pos.clone(),
            Self::Unary { pos, .. } => pos.clone(),
            Self::Number { pos, .. } => pos.clone(),
            Self::Ident { pos, .. } => pos.clone(),
            Self::Assign { pos, .. } => pos.clone(),
            Self::Call { pos, .. } => pos.clone(),
        }
    }
}

#[derive(Clone, Debug)]
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
    Declaration {
        pos: Position,
        decls: Vec<Declarator>,
    },
    None(Position),
}

#[derive(Clone, Debug)]
pub struct Declarator {
    pub name: String,
    pub init: Option<Box<Expr>>,
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
            Stmt::Declaration { pos, .. } => pos.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    POSITIVE,
    NEGATIVE,
    DEREF,
    ADDR,
}

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
    position: Position,

    pub ty_ident: HashMap<String, Type>,
}

impl<T> Parser<T>
where
    T: Iterator<Item = Token>,
{
    pub fn new(tokens: Peekable<T>) -> Self {
        Parser {
            tokens,
            position: Position { line: 0, column: 0 },
            ty_ident: HashMap::new(),
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
        self.expect_token(kind).and_then(|x| Ok(x.position))
    }

    fn expect_token(&mut self, kind: TokenKind) -> Result<Token> {
        let tok = self.peek()?;
        if tok.kind == kind {
            self.tokens.next();
            return Ok(tok);
        }
        return Err(self.error(format!("expected {}", kind)));
    }

    pub fn function(&mut self) -> Result<Func> {
        let mut func = Func {
            ..Default::default()
        };
        func.body = self.stmts()?;
        Ok(func)
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
    //      | declaration
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
            INT => {
                let pos = self.expect(INT)?;
                let ty = Type::Int;
                let mut decls = vec![];
                while self.peek()?.kind != SEMICOLON {
                    if !decls.is_empty() {
                        self.expect(COMMA)?;
                    }
                    decls.push(self.declator(ty.clone())?)
                }
                self.expect(SEMICOLON)?;
                Ok(Stmt::Declaration { pos, decls })
            }
            _ => self.expr_stmt(),
        }
    }

    fn declator(&mut self, mut ty: Type) -> Result<Declarator> {
        while self.peek()?.kind == MUL {
            self.expect(MUL)?;
            ty = ty.pointer_to();
        }

        let ident = self.expect_token(IDENT)?;

        self.ty_ident.insert(ident.literal.clone(), ty);

        let init = if self.peek()?.kind == ASSIGN {
            self.expect(ASSIGN)?;
            Some(self.expr()?.into())
        } else {
            None
        };

        Ok(Declarator {
            name: ident.literal,
            init,
        })
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
        if self.peek()?.kind == ASSIGN {
            let pos = self.expect(ASSIGN)?;
            expr = Expr::Assign {
                lhs: expr.into(),
                pos,
                rhs: self.assign()?.into(),
                ty: Type::Unchecked,
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut node = self.relational()?;

        loop {
            let tok = self.peek()?;
            match tok.kind {
                EQ | NEQ => {
                    let pos = self.expect(tok.kind)?;
                    node = Expr::Binary {
                        op: if tok.kind == EQ { BinOp::EQ } else { BinOp::NE },
                        pos,
                        lhs: node.into(),
                        rhs: self.add()?.into(),
                        ty: Type::Unchecked,
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
                LSS | LEQ | GTR | GEQ => {
                    let pos = self.expect(tok.kind)?;
                    let (op, swap) = match tok.kind {
                        LSS => (BinOp::LT, false),
                        LEQ => (BinOp::LE, false),
                        GTR => (BinOp::LT, true),
                        GEQ => (BinOp::LE, true),
                        _ => unreachable!(),
                    };

                    if !swap {
                        node = Expr::Binary {
                            op,
                            pos,
                            lhs: node.into(),
                            rhs: self.add()?.into(),
                            ty: Type::Unchecked,
                        };
                    } else {
                        node = Expr::Binary {
                            op,
                            pos,
                            lhs: self.add()?.into(),
                            rhs: node.into(),
                            ty: Type::Unchecked,
                        };
                    }
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
                ADD | SUB => {
                    let pos = self.expect(tok.kind)?;
                    node = Expr::Binary {
                        op: if tok.kind == ADD {
                            BinOp::ADD
                        } else {
                            BinOp::SUB
                        },
                        pos,
                        lhs: node.into(),
                        rhs: self.mul()?.into(),
                        ty: Type::Unchecked,
                    };
                }
                _ => break Ok(node),
            }
        }
    }

    fn unary(&mut self) -> Result<Expr> {
        let tok = self.peek()?;
        match tok.kind {
            k @ (ADD | SUB | MUL | AND) => {
                let pos = self.expect(k)?;
                let op = match k {
                    ADD => UnaryOp::POSITIVE,
                    SUB => UnaryOp::NEGATIVE,
                    MUL => UnaryOp::DEREF,
                    AND => UnaryOp::ADDR,
                    _ => unreachable!(),
                };

                return Ok(Expr::Unary {
                    op,
                    pos,
                    expr: self.unary()?.into(),
                    ty: Type::Unchecked,
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
                return Ok(Expr::Number {
                    pos,
                    value,
                    ty: Type::Unchecked,
                });
            }
            IDENT => {
                let pos = self.expect(IDENT)?;
                let name = tok.literal;

                if self.peek()?.kind == LPAREN {
                    self.expect(LPAREN)?;
                    // todo arguments
                    self.expect(RPAREN)?;
                    return Ok(Expr::Call {
                        pos,
                        name,
                        ty: Type::Unchecked,
                    });
                }

                return Ok(Expr::Ident {
                    pos,
                    name,
                    ty: Type::Unchecked,
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
                MUL | DIV => {
                    let pos = self.expect(tok.kind)?;
                    node = Expr::Binary {
                        op: if tok.kind == MUL {
                            BinOp::MUL
                        } else {
                            BinOp::DIV
                        },
                        pos,
                        lhs: node.into(),
                        rhs: self.unary()?.into(),
                        ty: Type::Unchecked,
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
