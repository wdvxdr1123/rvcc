use std::fmt;

/// comparision
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Cmp {
    /// Returns 1 if first value is less than second, respecting signedness
    LT,
    /// Returns 1 if first value is less than or equal to second, respecting signedness
    LE,
    /// Returns 1 if values are equal
    EQ,
    /// Returns 1 if values are not equal
    NE,
}

/// instruction
#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(dead_code)]
pub enum Inst {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Cmp(Cmp),
    And,
    Or,
    Ret,
    Jz(String),
    /// Unconditionally jumps to a label
    Jmp(String),
    /// Push a value to stack
    Push,
    /// Pop a value from stack
    Pop(String),
    /// Immediate
    Imm(usize),
    /// Neg
    Neg,
    /// Deref
    Load,
    /// Store
    Store(String),

    Shl(isize),
    Shr(isize),

    LocalVariable(String),

    Call(String),
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(f, "add"),
            Self::Sub => write!(f, "sub"),
            Self::Mul => write!(f, "mul"),
            Self::Div => write!(f, "div"),
            Self::Rem => write!(f, "rem"),
            Self::Cmp(cmp) => {
                write!(
                    f,
                    "c{}",
                    match cmp {
                        Cmp::LT => "lt",
                        Cmp::LE => "le",
                        // Cmp::GT => "gt",
                        // Cmp::GE => "ge",
                        Cmp::EQ => "eq",
                        Cmp::NE => "ne",
                    },
                )
            }
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Ret => write!(f, "ret"),
            Self::Jz(if_zero) => {
                write!(f, "jz @{}", if_zero)
            }
            Self::Jmp(label) => write!(f, "jmp @{}", label),
            Self::Push => write!(f, "push"),
            Self::Pop(reg) => write!(f, "pop {}", reg),
            Self::Imm(imm) => write!(f, "imm {}", imm),
            Self::Neg => write!(f, "neg"),
            Self::Load => write!(f, "load"),
            Self::Store(dst) => write!(f, "store {}", dst),
            Self::Shl(i) => write!(f, "shl {}", i),
            Self::Shr(i) => write!(f, "shr {}", i),
            Self::LocalVariable(name) => write!(f, "local {}", name),
            Self::Call(name) => write!(f, "call {}", name),
        }
    }
}
