use crate::bst::{BstFile, BstState, StackSlot};
use crate::intern::InternedString;

#[derive(Clone)]
pub enum Instruction {
    IntegerLit(i32),
    StringLit(InternedString),
    FnRef(FnRef),
    FnCall(FnRef),
}

// TODO: this is a bit heavyweight because StackSlot can contain a Vec
#[derive(Clone, PartialEq)]
pub enum ExecutionError {
    EmptyStack,
    DifferentTypes(StackSlot, StackSlot),
    NotIntOrString(StackSlot),
    ExpectedInt(StackSlot),
    ExpectedString(StackSlot),
    ExpectedFunction(StackSlot),
}

pub trait ErrorReporter {
    fn report(&mut self, e: ExecutionError) {
    }
}

impl ErrorReporter for Vec<ExecutionError> {
    fn report(&mut self, e: ExecutionError) {
        self.push(e);
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BuiltInFunction {
    Equals,
    GreaterThan,
    LessThan,
    Plus,
    Minus,
    Concatenate,
    Assign,
    AddPeriod,
    CallType,
    ChangeCase,
    ChrToInt,
    Cite,
    Duplicate,
    Empty,
    FormatName,
    If,
    IntToChr,
    IntToStr,
    Missing,
    Newline,
    NumNames,
    Pop,
    Preamble,
    Purify,
    Quote,
    Skip,
    SortKey,
    Stack,
    Substring,
    Swap,
    TextLength,
    TextPrefix,
    Top,
    Type,
    Warning,
    While,
    Width,
    Write,
    // TODO: where are entry.max$ and global.max$? They're defined in bibtex with pre_define
    // instead of build_in, so what's the difference?
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FnRef {
    Field(usize),
    EntryInt(usize),
    EntryString(usize),
    GlobalString(usize),
    GlobalInt(usize),
    WizardDefined(usize),
    BuiltIn(BuiltInFunction),
}

#[derive(Clone)]
// It might be more efficient to store all of the code in one giant array, and have function
// definitions contain indices into it. But this is easier, for now.
pub struct FnDef {
    pub insts: Vec<Instruction>,
}

pub struct Executor<'bst, 'state, 'err> {
    file: &'bst BstFile,
    state: &'state mut BstState,
    error: Box<dyn ErrorReporter + 'err>,
}

impl<'bst, 'state, 'err> Executor<'bst, 'state, 'err> {
    fn pop(&mut self) -> StackSlot {
        if let Some(s) = self.state.stack.pop() {
            s
        } else {
            self.error.report(ExecutionError::EmptyStack);
            StackSlot::Empty
        }
    }

    fn push(&mut self, val: StackSlot) {
        self.state.stack.push(val);
    }

    // For example, this should print a warning unless the variables are either both integers or
    // both strings.
    // Also, figure out what to do if the stack is empty.
    fn equals(&mut self) {
        use StackSlot::*;

        let a = self.pop();
        let b = self.pop();

        if !a.same_type_as(&b) {
            self.error.report(ExecutionError::DifferentTypes(a.clone(), b.clone()));
        } else if !a.int_or_string_or_empty() {
            self.error.report(ExecutionError::NotIntOrString(a.clone()));
        }

        let ret = match (a, b) {
            (Integer(i), Integer(j)) => if i == j { 1 } else { 0 },
            (String(s), String(t)) => if s == t { 1 } else { 0 },
            _ => 0,
        };
        self.push(Integer(ret));
    }

    fn binary_int_fn<F: Fn(i32, i32) -> i32>(&mut self, f: F) {
        use StackSlot::*;
        let a = self.pop();
        let b = self.pop();
        let ret = if let (Integer(i), Integer(j)) = (&a, &b) {
            f(*i, *j)
        } else {
            if !a.is_int() {
                self.error.report(ExecutionError::ExpectedInt(a.clone()));
            } else {
                self.error.report(ExecutionError::ExpectedInt(b.clone()));
            }
            0
        };
        self.push(Integer(ret));
    }

    fn greater_than(&mut self) {
        self.binary_int_fn(|i, j| if i > j { 1 } else { 0 });
    }

    fn less_than(&mut self) {
        self.binary_int_fn(|i, j| if i > j { 1 } else { 0 });
    }

    fn plus(&mut self) {
        self.binary_int_fn(|i, j| i.wrapping_add(j));
    }

    fn minus(&mut self) {
        self.binary_int_fn(|i, j| i.wrapping_sub(j));
    }

    fn expect_function(&mut self, f: StackSlot) -> Option<FnRef> {
        if let StackSlot::FnRef(f) = f {
            Some(f)
        } else {
            self.error.report(ExecutionError::ExpectedFunction(f));
            None
        }
    }

    fn expect_integer(&mut self, i: StackSlot) -> Option<i32> {
        if let StackSlot::Integer(i) = i {
            Some(i)
        } else {
            self.error.report(ExecutionError::ExpectedInt(i));
            None
        }
    }

    // `if' is a reserved keyword
    fn ifx(&mut self) {
        let f1 = self.pop();
        let f2 = self.pop();
        let b = self.pop();

        let _ = (move || {
            let f1 = self.expect_function(f1)?;
            let f2 = self.expect_function(f2)?;
            let b = self.expect_integer(b)?;
            if b > 0 {
                self.execute_function(f2);
            } else {
                self.execute_function(f1);
            }
            Some(())
        })();
    }

    pub fn execute_built_in(&mut self, f: BuiltInFunction) {
        use BuiltInFunction::*;

        match f {
            Equals => self.equals(),
            GreaterThan => self.greater_than(),
            LessThan => self.less_than(),
            Plus => self.plus(),
            Minus => self.minus(),
            If => self.ifx(),
            _ => unimplemented!(),
        }
    }

    pub fn execute_wizard_function(&mut self, idx: usize) {
        let insts = &self.file.wizard_functions[idx];
        for inst in insts {
            self.execute_instruction(inst);
        }
    }

    pub fn execute_function(&mut self, f: FnRef) {
        // TODO: could we avoid some cloning of strings? Cows?
        match f {
            FnRef::BuiltIn(f) => self.execute_built_in(f),
            // Field, EntryInt, and EntryString only make sense in the context of modifying an
            // entry.
            FnRef::GlobalString(i) => self.push(StackSlot::String(self.state.global_strings[i].clone())),
            FnRef::GlobalInt(i) => self.push(StackSlot::Integer(self.state.global_ints[i])),
            FnRef::WizardDefined(i) => self.execute_wizard_function(i),
            _ => unimplemented!(),
        }
    }

    pub fn execute_instruction(&mut self, inst: &Instruction) {
        match *inst {
            Instruction::IntegerLit(i) => self.push(StackSlot::Integer(i)),
            Instruction::StringLit(s) => self.push(StackSlot::String(self.file.intern.string(&s).to_owned())),
            Instruction::FnRef(i) => self.push(StackSlot::FnRef(i)),
            Instruction::FnCall(fn_ref) => self.execute_function(fn_ref),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::Instruction::*;
    use super::FnRef::*;
    use super::BuiltInFunction::*;
    use crate::bst::BstFile;
    use crate::bst::StackSlot::*;

    fn check_stack(init_stack: Vec<StackSlot>, insts: Vec<Instruction>, expected_stack: Vec<StackSlot>) {
        let mut state = BstState {
            stack: init_stack,
            global_strings: vec![],
            global_ints: vec![],
        };
        let file = BstFile::default();
        let mut executor = Executor {
            file: &file,
            state: &mut state,
            error: Box::new(vec![]),
        };

        for inst in insts {
            executor.execute_instruction(&inst);
        }
        assert_eq!(executor.state.stack, expected_stack);
    }

    macro_rules! test_stack {
        ($name:ident, $init:expr, $insts:expr, $expected:expr) => {
            #[test]
            fn $name() {
                check_stack($init, $insts, $expected);
            }
        }
    }

    test_stack!(
        equals_int,
        vec![Integer(5), Integer(5)],
        vec![FnCall(BuiltIn(Equals))],
        vec![Integer(1)]
    );

    test_stack!(
        not_equals_int,
        vec![Integer(5), Integer(6)],
        vec![FnCall(BuiltIn(Equals))],
        vec![Integer(0)]
    );

    test_stack!(
        plus,
        vec![Integer(5), Integer(6)],
        vec![FnCall(BuiltIn(Plus))],
        vec![Integer(11)]
    );
}
