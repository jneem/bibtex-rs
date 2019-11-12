use crate::executor::{FnRef, Instruction};
use crate::intern::{InternedString, InternTable};

use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct BstFile {
    pub wizard_functions: Vec<Vec<Instruction>>,

    // The number of possible fields in an entry. *Each* entry has up to this many fields.
    pub num_entry_fields: usize,
    // The number of entry int variables.
    pub num_entry_ints: usize,
    // The number of entry string variables.
    pub num_entry_strings: usize,

    // Looks up a function index by its name.
    pub function_by_name: HashMap<InternedString, FnRef>,

    pub intern: InternTable,

    macros: HashSet<()>, // TODO
}

#[derive(Clone, Debug, PartialEq)]
pub enum StackSlot {
    Integer(i32),
    String(Vec<u8>),
    Symbol(InternedString),
    Entry(InternedString),
    FnRef(FnRef),
    // This is never actually stored on the stack, but it might be returned from popping if the
    // stack is empty.
    Empty,
}

impl StackSlot {
    pub fn same_type_as(&self, other: &StackSlot) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }

    pub fn int_or_string_or_empty(&self) -> bool {
        use StackSlot::*;
        match self {
            Integer(_) | String(_) => true,
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            StackSlot::Integer(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            StackSlot::String(_) => true,
            _ => false,
        }
    }
}

/// This struct contains the global state that is mutated while executing the BST file.
#[derive(Default)]
pub struct BstState {
    /// The current program stack.
    pub stack: Vec<StackSlot>,
    /// The current values of all the global string variables. These variables all have names. The
    /// data for looking up the index by name is stored in the [`BstFile`] struct; at this point,
    /// for efficiency, we only refer to them by index.
    pub global_strings: Vec<Vec<u8>>,
    /// The current values of all the global integer variables.
    pub global_ints: Vec<i32>,
}


