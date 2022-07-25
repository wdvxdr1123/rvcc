use std::{collections::binary_heap, rc::Rc};

use crate::{
    error::SyntaxError,
    parser::{BinOp, Expr, Node, Stmt, UnaryOp},
};

#[derive(Debug)]
pub struct TypecheckError {} // todo: fill error msg

type Result<T> = std::result::Result<T, SyntaxError>;

#[derive(Debug, PartialEq)]
pub enum Kind {
    Int,
    Ptr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Pointer(Rc<Type>),
}

impl Type {
    pub fn kind(&self) -> Kind {
        match self {
            Type::Int => Kind::Int,
            Type::Pointer(_) => Kind::Ptr,
        }
    }

    pub fn is(&self, kind: Kind) -> bool {
        return self.kind() == kind;
    }
}

pub struct Context {
    ty_int: Rc<Type>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            ty_int: Rc::new(Type::Int),
        }
    }

    fn int(&self) -> Rc<Type> {
        self.ty_int.clone()
    }

    fn pointer_to(&self, to: &Expr) -> Option<Rc<Type>> {
        if let Some(typ) = to.typecheck() {
            Some(Rc::new(Type::Pointer(typ)))
        } else {
            None
        }
    }

    fn elem(&self, expr: &Expr) -> Option<Rc<Type>> {
        match expr.typecheck() {
            Some(inner) => {
                if let Type::Pointer(typ) = inner.as_ref() {
                    Some(typ.clone())
                } else {
                    Some(self.int())
                }
            }
            _ => None,
        }
    }

    pub fn typecheck(&self, stmts: &mut Vec<Stmt>) -> Result<()> {
        self.check_stmts(stmts)
    }

    pub fn check_expr(&self, expr: &mut Expr) -> Result<()> {
        if expr.typecheck().is_some() {
            return Ok(());
        }

        match expr {
            Expr::Binary {
                op, lhs, rhs, ty, ..
            } => {
                self.check_expr(lhs)?;
                self.check_expr(rhs)?;

                *ty = match op {
                    BinOp::SUB => {
                        let (lty, rty) = (lhs.typecheck().unwrap(), rhs.typecheck().unwrap());
                        if lty.is(Kind::Ptr) && rty.is(Kind::Ptr) {
                            Some(self.int())
                        } else {
                            Some(lty)
                        }
                    },
                    _ => lhs.typecheck(),
                };
            }
            Expr::Unary { op, expr, ty, .. } => {
                self.check_expr(expr)?;
                match op {
                    UnaryOp::ADDR => {
                        *ty = self.pointer_to(expr);
                    }
                    UnaryOp::DEREF => {
                        *ty = self.elem(expr);
                    }
                    _ => *ty = expr.typecheck(),
                }
            }
            Expr::Number { ty, .. } => {
                *ty = Some(self.int());
            }
            Expr::Ident { ty, .. } => {
                *ty = Some(self.int());
            }
            Expr::Assign { lhs, rhs, ty, .. } => {
                self.check_expr(rhs)?;
                self.check_expr(lhs)?;
                *ty = lhs.typecheck();
            }
        }
        Ok(())
    }

    fn check_stmts(&self, stmts: &mut Vec<Stmt>) -> Result<()> {
        for s in stmts {
            self.check_stmt(s)?;
        }
        Ok(())
    }

    fn check_stmt(&self, stmt: &mut Stmt) -> Result<()> {
        match stmt {
            Stmt::Expr { expr, .. } => self.check_expr(expr),
            Stmt::Return { expr, .. } => self.check_expr(expr),
            Stmt::Block { body, .. } => self.check_stmts(body),
            Stmt::If {
                cond, then, r#else, ..
            } => {
                self.check_expr(cond)?;
                self.check_stmt(then)?;
                if let Some(s) = r#else {
                    self.check_stmt(s)
                } else {
                    Ok(())
                }
            }
            Stmt::For {
                init,
                cond,
                post,
                body,
                ..
            } => {
                if let Some(init) = init {
                    self.check_stmt(init)?;
                }
                if let Some(cond) = cond {
                    self.check_expr(cond)?;
                }
                if let Some(post) = post {
                    self.check_expr(post)?;
                }
                self.check_stmt(body)
            }
            Stmt::None(_) => Ok(()),
        }
    }

    fn check_add(&self, lhs: &mut Expr, rhs: &mut Expr) -> Result<()> {
        let (lty, rty) = (lhs.typecheck().unwrap(), rhs.typecheck().unwrap());
        // num + num
        if lty.is(Kind::Int) && rty.is(Kind::Int) {
            return Ok(());
        }
        // ptr + ptr
        if lty.is(Kind::Ptr) && rty.is(Kind::Ptr) {
            return Ok(()); // todo: error
        }

        if lty.is(Kind::Ptr) && rty.is(Kind::Int) {
            *rhs = {
                Expr::Binary {
                    op: BinOp::MUL,
                    pos: rhs.pos(),
                    rhs: Expr::Number {
                        pos: rhs.pos(),
                        value: 8,
                        ty: None,
                    }
                    .into(),
                    lhs: Box::new(rhs.clone()),
                    ty: None,
                }
            };
            self.check_expr(lhs)?;
        }

        if lty.is(Kind::Int) && rty.is(Kind::Ptr) {
            *lhs = {
                Expr::Binary {
                    op: BinOp::MUL,
                    pos: lhs.pos(),
                    rhs: Expr::Number {
                        pos: lhs.pos(),
                        value: 8,
                        ty: None,
                    }
                    .into(),
                    lhs: Box::new(lhs.clone()),
                    ty: None,
                }
            };
            self.check_expr(lhs)?;
        }

        Ok(())
    }

    fn check_sub(&self, lhs: &mut Expr, rhs: &mut Expr) -> Result<()> {
        let (lty, rty) = (lhs.typecheck().unwrap(), rhs.typecheck().unwrap());
        // num + num
        if lty.is(Kind::Int) && rty.is(Kind::Int) {
            return Ok(());
        }
        // ptr - ptr
        if lty.is(Kind::Ptr) && rty.is(Kind::Ptr) {
            *lhs = {
                Expr::Binary {
                    op: BinOp::DIV,
                    pos: lhs.pos(),
                    rhs: Expr::Number {
                        pos: lhs.pos(),
                        value: 8,
                        ty: None,
                    }
                    .into(),
                    lhs: Box::new(lhs.clone()),
                    ty: None,
                }
            };
            self.check_expr(lhs)?;
            *rhs = {
                Expr::Binary {
                    op: BinOp::DIV,
                    pos: rhs.pos(),
                    rhs: Expr::Number {
                        pos: rhs.pos(),
                        value: 8,
                        ty: None,
                    }
                    .into(),
                    lhs: Box::new(rhs.clone()),
                    ty: None,
                }
            };
            self.check_expr(rhs)?;
            return Ok(()); // todo: fix
        }

        // ptr - num
        if lty.is(Kind::Ptr) && rty.is(Kind::Int) {
            *rhs = {
                Expr::Binary {
                    op: BinOp::MUL,
                    pos: rhs.pos(),
                    rhs: Expr::Number {
                        pos: rhs.pos(),
                        value: 8,
                        ty: None,
                    }
                    .into(),
                    lhs: Box::new(rhs.clone()),
                    ty: None,
                }
            };
            self.check_expr(rhs)?;
        }

        Ok(())
    }
}
