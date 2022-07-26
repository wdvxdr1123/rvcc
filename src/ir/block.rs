use std::fmt;

use super::Inst;

/// Function block with a label
#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Block {
    /// Label before the block
    pub label: String,

    /// A list of statements in the block
    pub statements: Vec<Inst>,
}

impl Block {
    /// Adds a new instruction to the block
    pub fn add_instr(&mut self, instr: Inst) {
        self.statements.push(instr);
    }

    /// Returns true if the block's last instruction is a jump
    pub fn jumps(&self) -> bool {
        let last = self.statements.last();

        if let Some(instr) = last {
            matches!(instr, Inst::Ret | Inst::Jmp(_) | Inst::Jz(..))
        } else {
            false
        }
    }
}

impl<'a> fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "@{}", self.label)?;

        write!(
            f,
            "{}",
            self.statements
                .iter()
                .map(|instr| format!("\t{}", instr))
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}
