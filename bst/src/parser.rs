use crate::bst::{BstFile, BstState};
use crate::executor::{FnDef, FnRef, Instruction};
use crate::intern::{InternedString, InternTable};
use crate::lexer::{is_white, Lexer, LexError};

use common::Input;
use std::collections::{HashMap, HashSet};
use std::io;


#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FnClass {
    BuiltIn,
    WizardDefined,
    IntegerLiteral,
    StringLiteral,
    Field,
    IntegerEntryVariable,
    StringEntryVariable,
    IntegerGlobalVariable,
    StringGlobalVariable,
}

#[derive(Debug)]
pub enum Error {
    RepeatedEntry,
    UnexpectedEOF,
    UnexpectedEOFIn(&'static str),
    Expected(u8),
    ExpectedIn(u8, &'static str),
    // Expected an identifier, but found an illegal id character.
    // TODO: needs a "contextual" version
    IdBegin(u8),
    LexError(LexError),
    // Found something unexpected immediately after an identifier.
    // TODO: needs a "contextual" version
    AfterId(u8),
    Io(io::Error),
    AlreadySeenFunction(InternedString, FnClass),
    Recursion(InternedString),
    UnknownFunction(InternedString),
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::Io(e)
    }
}

impl From<LexError> for Error {
    fn from(e: LexError) -> Error {
        Error::LexError(e)
    }
}

impl Error {
    fn add_context(self, ctxt: &'static str) -> Error {
        use Error::*;

        match self {
            UnexpectedEOF => UnexpectedEOFIn(ctxt),
            Expected(c) => ExpectedIn(c, ctxt),
            e => e,
        }
    }
}

pub trait ErrorReporter {
    fn report(&mut self, error: Error);
}

pub struct Parser<'read> {
    input: Input<'read>,
    file: BstFile,
    state: BstState,
    entry_seen: bool,
    errors: Box<dyn ErrorReporter>,

    implicit_function_num: u32,
}

impl<'read> Parser<'read> {
    pub fn new<E: ErrorReporter + 'static>(input: Input<'read>, errors: E) -> Parser<'read> {
        Parser {
            input,
            file: BstFile::default(),
            state: BstState::default(),
            entry_seen: false,
            errors: Box::new(errors),
            implicit_function_num: 0,
        }
    }

    // Current error handling strategy (subject to revision): in all of the parsing routines, if an
    // EOF is encountered then we return an Err(UnexpectedEOF)
    fn advance_to_blank_line(&mut self) -> Result<(), Error> {
        unimplemented!();
    }

    /// Skips over white space and comments, returning an error if EOF is reached before a
    /// non-whitespace character.
    fn skip_white_space(&mut self) -> Result<(), Error> {
        loop {
            if self.input.skip_white_space() {
                if self.input.current() != b'%' {
                    return Ok(());
                }
            }

            if !self.input.input_line()? {
                return Err(Error::UnexpectedEOF);
            }
        }
    }

    fn skip_char(&mut self, c: u8) -> Result<(), Error> {
        if self.input.current() == c {
            self.input.advance();
            Ok(())
        } else {
            Err(Error::Expected(c))
        }
    }

    /// Parses an identifier, expecting that the following character will be either
    /// a b'{', a b'%', a whitespace, of the end of file.
    fn scan_identifier_before_rbrace(&mut self) -> Result<InternedString, Error> {
        let id = self.input.scan_identifier();
        if id.is_empty() {
            Err(Error::IdBegin(self.input.current()))
        } else if !self.input.is_eol() {
            let next = self.input.current();
            if is_white(next) || next == b'{' || next == b'%' {
                Ok(self.file.intern.insert(&id))
            } else {
                Err(Error::AfterId(next))
            }
        } else {
            Ok(self.file.intern.insert(&id))
        }
    }

    /// Parses a space-separated list of identifiers, delimited by braces.
    fn parse_brace_delimited_list(&mut self) -> Result<Vec<InternedString>, Error> {
        let mut ret = Vec::new();

        self.skip_white_space()?;
        self.skip_char(b'{')?;
        self.skip_white_space()?;

        while self.input.current() != b'}' {
            ret.push(self.scan_identifier_before_rbrace()?);
            self.skip_white_space()?;
        }
        self.input.advance(); // advance past the }
        Ok(ret)
    }

    /// Parses an identifier, surrounded by braces.
    fn parse_braced_identifier(&mut self) -> Result<InternedString, Error> {
        self.skip_char(b'{')?;
        self.skip_white_space()?;
        let ret = self.scan_identifier_before_rbrace()?;
        self.skip_white_space()?;
        self.skip_char(b'}')?;
        self.input.advance();
        Ok(ret)
    }

    /// Assuming that the string "entry" has already been consumed, parse the rest of an "entry"
    /// command.
    ///
    /// An entry command consists of three brace-delimited, whitespace-separated lists of strings:
    /// the first is the list of entry types (e.g. book, article, etc.); the second is the list of
    /// private (to the .bst file) integer variables; the third is the list of private string
    /// variables.
    fn parse_entries(&mut self) -> Result<(), Error> {
        if self.entry_seen {
            self.errors.report(Error::RepeatedEntry);
            self.advance_to_blank_line()?;
            return Ok(());
        }

        self.entry_seen = true;
        self.skip_white_space()?;

        for field in self.parse_brace_delimited_list()? {
            self.add_function(field, FnRef::Field(self.file.num_entry_fields), FnClass::Field)?;
            self.file.num_entry_fields += 1;
        }
        for int in self.parse_brace_delimited_list()? {
            self.add_function(int, FnRef::EntryInt(self.file.num_entry_ints), FnClass::IntegerEntryVariable)?;
            self.file.num_entry_ints += 1;
        }
        for string in self.parse_brace_delimited_list()? {
            self.add_function(string, FnRef::EntryString(self.file.num_entry_strings), FnClass::StringEntryVariable)?;
            self.file.num_entry_strings += 1;
        }

        Ok(())
    }

    fn add_function(&mut self, name: InternedString, val: FnRef, class: FnClass) -> Result<(), Error> {
        if self.file.function_by_name.contains_key(&name) {
            Err(Error::AlreadySeenFunction(name, class))
        } else {
            self.file.function_by_name.insert(name, val);
            Ok(())
        }
    }

    /// Assuming that the string "integers" has just been consumed, parse the rest of an "integers"
    /// command (which consists of a brace-delimited list of identifiers).
    ///
    /// TODO: make the error handling more consistent. Where (i.e. at what level) are the errors being handled?
    fn parse_integers(&mut self) -> Result<(), Error> {
        self.skip_white_space()?;
        for int in self.parse_brace_delimited_list()? {
            self.add_function(int, FnRef::GlobalInt(self.state.global_ints.len()), FnClass::IntegerGlobalVariable)?;
            self.state.global_ints.push(0);
        }

        Ok(())
    }

    fn parse_strings(&mut self) -> Result<(), Error> {
        self.skip_white_space()?;
        for string in self.parse_brace_delimited_list()? {
            self.add_function(string, FnRef::GlobalString(self.state.global_strings.len()), FnClass::StringGlobalVariable)?;
            self.state.global_strings.push(vec![]);
        }

        Ok(())
    }

    /// Assuming that the string "function" has just been consumed, parse the rest of a "function"
    /// command.
    ///
    /// This consists of two brace-delimited arguments: the first is the function name, the second
    /// is the function body.
    fn parse_function(&mut self) -> Result<(), Error> {
        self.skip_white_space()?;
        let name = self.parse_braced_identifier()?;

        if self.file.function_by_name.contains_key(&name) {
            // FIXME: don't hardcode the function class
            return Err(Error::AlreadySeenFunction(name, FnClass::WizardDefined));
        }

        self.skip_white_space()?;
        self.skip_char(b'{')?;

        let f = self.parse_function_def(name)?;
        self.file.function_by_name.insert(name, FnRef::WizardDefined(self.file.wizard_functions.len()));
        self.file.wizard_functions.push(f);
        Ok(())
    }

    /// Assuming that a left brace has just been consumed, parse the rest of the body of a
    /// function (including the closing right brace).
    ///
    /// Function definitions have five different kinds of elements, which can be distinguished by
    /// their first byte:
    ///
    ///  - a numeric literal starts with a '#',
    ///  - a string literal starts with a '"',
    ///  - a quoted function starts with a '\'',
    ///  - an anonymous function (or, in bibtex terminology, an implicit function) starts with '{',
    ///  - anything else is interpreted as a function call.
    ///
    /// This function essentially handles its own errors: it only returns UnexpectedEOF or I/O
    /// errors. TODO: see if we can use the type-system to do better with our error handling
    /// strategy.
    fn parse_function_def(&mut self, name: InternedString) -> Result<Vec<Instruction>, Error> {
        let mut insts = Vec::new();

        self.skip_white_space()?;

        while self.input.current() != b'}' {
            match self.input.current() {
                b'#' => {
                    self.input.advance();
                    match self.input.scan_integer() {
                        Ok(i) => insts.push(Instruction::IntegerLit(i)),
                        Err(e) => self.errors.report(e.into()),
                    }
                }
                b'"' => {
                    match self.input.scan_string() {
                        Ok(s) => insts.push(Instruction::StringLit(self.file.intern.insert(&s))),
                        Err(e) => self.errors.report(e.into()),
                    }
                }
                b'\'' => {
                    let fn_name = self.input.scan_function_name();
                    let fn_name = self.file.intern.insert(&fn_name);
                    if let Some(fn_ref) = self.file.function_by_name.get(&fn_name) {
                        insts.push(Instruction::FnRef(*fn_ref));
                    } else if fn_name == name {
                        self.errors.report(Error::Recursion(fn_name));
                    } else {
                        self.errors.report(Error::UnknownFunction(fn_name));
                    }
                }
                b'{' => {
                    // To parse an anonymous function, we simply recurse this function. First,
                    // however, we need to synthesize a unique name for the recursive function. We
                    // use the same scheme as the original bibtex: a name of the form 'n for
                    // sequential integers n. (This is guaranteed not to clash with any
                    // user-defined function, since ' is an illegal character in those.)
                    let name = format!("\'{}", self.implicit_function_num).into_bytes();
                    self.implicit_function_num += 1;
                    let name = self.file.intern.insert(&name);
                    let f = self.parse_function_def(name)?;

                    self.file.function_by_name.insert(name, FnRef::WizardDefined(self.file.wizard_functions.len()));
                    self.file.wizard_functions.push(f);
                }
                _ => {
                    let fn_name = self.input.scan_function_name();
                    let fn_name = self.file.intern.insert(&fn_name);
                    if let Some(idx) = self.file.function_by_name.get(&fn_name) {
                        insts.push(Instruction::FnCall(*idx));
                    } else if fn_name == name {
                        self.errors.report(Error::Recursion(fn_name));
                    } else {
                        self.errors.report(Error::UnknownFunction(fn_name));
                    }
                }
            }

            self.skip_white_space()?;
        }

        debug_assert!(self.input.current() == b'}');
        self.input.advance();
        Ok(insts)
    }
    
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! is_match {
        ($p:pat, $e:expr) => {
            match $e {
                $p => true,
                _ => false,
            }
        }
    }

    struct ErrorCollector(Vec<Error>);

    impl ErrorReporter for ErrorCollector {
        fn report(&mut self, e: Error) {
            self.0.push(e);
        }
    }

    fn parser(s: &'static [u8]) -> Parser {
        let input = Input::from_reader(s);
        let error = ErrorCollector(Vec::new());
        Parser::new(input, error)
    }

    #[test]
    fn parse_brace_delimited_list() {
        let mut p = parser(b"{}");
        let result = p.parse_brace_delimited_list().unwrap();
        assert_eq!(result, vec![]);

        let mut p = parser(b"{ foo foo bar }");
        let result = p.parse_brace_delimited_list().unwrap();
        let expected = vec![p.file.intern.insert(b"foo"), p.file.intern.insert(b"foo"), p.file.intern.insert(b"bar")];
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_brace_delimited_list_errors() {
        macro_rules! expect_error {
            ($input:expr, $expected:pat) => {
                let mut p = parser($input);
                let result = p.parse_brace_delimited_list().unwrap_err();
                assert!(is_match!($expected, result), "unexpected error {:?} for \"{:?}\"", result, $input);
            }
        };

        expect_error!(b"blah { foo }", Error::Expected(b'{'));
        expect_error!(b"{ foo", Error::UnexpectedEOF);
        expect_error!(b"{ f#oo", Error::AfterId(b'#'));
    }
}

