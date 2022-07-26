use std::collections::HashMap;

use crate::{
    error::SyntaxError,
    parser::{BinOp, Expr, Stmt, UnaryOp},
};

#[derive(Debug)]
pub struct TypecheckError {} // todo: fill error msg

type Result<T> = std::result::Result<T, SyntaxError>;

#[derive(Debug, PartialEq)]
pub enum Kind {
    Unchecked,
    Int,
    Ptr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unchecked,
    Int,
    Pointer(Box<Type>),
}

impl Type {
    pub fn kind(&self) -> Kind {
        match self {
            Type::Int => Kind::Int,
            Type::Pointer(_) => Kind::Ptr,
            _ => Kind::Unchecked,
        }
    }

    pub fn is(&self, kind: Kind) -> bool {
        return self.kind() == kind;
    }

    pub fn checked(&self) -> bool {
        match self {
            Self::Unchecked => false,
            _ => true,
        }
    }

    pub fn pointer_to(&self) -> Type {
        if self != &Type::Unchecked {
            Type::Pointer(Box::new(self.clone()))
        } else {
            Type::Unchecked
        }
    }
}

pub struct Context {
    ty_ident: HashMap<String, Type>,
    ty_int: Type,
}

impl Context {
    pub fn new(tys: HashMap<String, Type>) -> Self {
        Context {
            ty_ident: tys,
            ty_int: Type::Int,
        }
    }

    fn int(&self) -> Type {
        self.ty_int.clone()
    }

    fn elem(&self, expr: &Expr) -> Type {
        if let Type::Pointer(x) = expr.typecheck() {
            *x.clone()
        } else {
            Type::Unchecked
        }
    }

    pub fn typecheck(&self, stmts: &mut Vec<Stmt>) -> Result<()> {
        self.check_stmts(stmts)
    }

    pub fn check_expr(&self, expr: &mut Expr) -> Result<()> {
        if expr.typecheck().checked() {
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
                        let (lty, rty) = (lhs.typecheck(), rhs.typecheck());
                        if lty.is(Kind::Ptr) && rty.is(Kind::Ptr) {
                            self.int()
                        } else {
                            lty.clone()
                        }
                    }
                    _ => lhs.typecheck().clone(),
                };
            }
            Expr::Unary { op, expr, ty, .. } => {
                self.check_expr(expr)?;
                match op {
                    UnaryOp::ADDR => {
                        *ty = expr.typecheck().pointer_to();
                    }
                    UnaryOp::DEREF => {
                        *ty = self.elem(expr);
                    }
                    _ => *ty = expr.typecheck().clone(),
                }
            }
            Expr::Number { ty, .. } => {
                *ty = self.int();
            }
            Expr::Ident { ty, name, .. } => {
                *ty = self.ty_ident.get(name).unwrap().clone();
            }
            Expr::Assign { lhs, rhs, ty, .. } => {
                self.check_expr(rhs)?;
                self.check_expr(lhs)?;
                *ty = lhs.typecheck().clone();
            }
            Expr::Call { ty, .. } => *ty = self.int(),
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
            Stmt::Declaration { decls, .. } => {
                for decl in decls {
                    if let Some(ref mut init) = decl.init {
                        self.check_expr(init)?;
                    }
                }
                Ok(())
            }
            Stmt::None(_) => Ok(()),
        }
    }
}
