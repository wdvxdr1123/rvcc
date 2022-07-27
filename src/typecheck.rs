use std::rc::Rc;

#[derive(Debug)]
pub struct TypecheckError {} // todo: fill error msg

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Unchecked,
    Int,
    Ptr,
    Func,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unchecked,
    Int,
    Pointer(Rc<Type>),
    Func(Rc<Type>),
}

impl Default for Type {
    fn default() -> Self {
        Type::Unchecked
    }
}

impl Type {
    pub fn kind(&self) -> Kind {
        match self {
            Type::Int => Kind::Int,
            Type::Pointer(_) => Kind::Ptr,
            Type::Func(_) => Kind::Func,
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
            Type::Pointer(Rc::new(self.clone()))
        } else {
            Type::Unchecked
        }
    }

    pub fn elem(&self) -> Type {
        if let Type::Pointer(x) = self {
            x.as_ref().clone()
        } else {
            Type::Unchecked
        }
    }
}
