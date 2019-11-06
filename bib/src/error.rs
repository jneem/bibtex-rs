//! This module provides various types and methods for reporting errors in `.bib` files. The main
//! struct is [`Error`].

use common::Input;
use common::input::is_white;
use std::io;
use std::sync::Arc;

fn xchr(c: u8) -> char {
    match c {
        127 => ' ', // DEL
        b'\t' => '\t',
        0..=31 => ' ',
        x => x.into(),
    }
}

/// This struct represents an error that occurred while parsing a `.bib` file. It also stores the
/// location of the error in that file.
///
/// An `Error` is capable of producing BibTex-compatible error messages, with the
/// [`Error::write_comwrite_compatible_errmsg`] function.
// TODO: should this implement std::Error?
#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    /// What kind of error is this?
    pub kind: ErrorKind,
    /// In which context did this error occur?
    pub context: Option<ErrorContext>,
    /// The line of input that was being parsed when the error occurred.
    pub current_input: Vec<u8>,
    /// The line number of the current line in the `.bib` file (starting from 1).
    pub input_line: usize,
    /// The column number at which the error occurred (starting from 1).
    pub input_col: usize,
}

impl Error {
    pub(crate) fn new(kind: ErrorKind, input: &Input<'_>) -> Error {
        Error {
            kind,
            context: None,
            input_line: input.cur_line_num(),
            input_col: input.cur_col_num(),
            current_input: input.cur_line().to_owned(),
        }
    }

    pub(crate) fn with_context(kind: ErrorKind, context: ErrorContext, input: &Input<'_>) -> Error {
        Error {
            kind,
            context: Some(context),
            input_line: input.cur_line_num(),
            input_col: input.cur_col_num(),
            current_input: input.cur_line().to_owned(),
        }
    }

    // Prints the current line, divided into two at the current column. Whitespace characters are
    // replaced by spaces.
    fn print_input_line(&self, write: &mut dyn io::Write) -> io::Result<()> {
        fn map_space(buf: &[u8]) -> Vec<u8> {
            buf.iter().map(|&c| if is_white(c) { b' ' } else { c }).collect()
        }

        let before: Vec<u8> = map_space(&self.current_input[..self.input_col]);
        let spaces = vec![b' '; before.len()];
        let after: Vec<u8> = map_space(&self.current_input[self.input_col..]);
        write.write_all(b" : ")?;
        write.write_all(&before)?;
        write.write_all(b"\n : ")?;
        write.write_all(&spaces)?;
        write.write_all(&after)?;
        write.write_all(b"\n")?;

        if before.iter().all(|&c| is_white(c)) {
            write.write_all(b"(Error may have been on previous line)\n")?;
        }
        Ok(())
    }

    // This corresponds to WEB section 221 in BibTex. It prints the line number, the contents of
    // the bad line, and the text "skipping whatever remains".
    fn common_err_msg(&self, write: &mut dyn io::Write, filename: &str) -> io::Result<()> {
        writeln!(write, "---line {} of file {}", self.input_line, filename)?;
        self.print_input_line(write)?;
        let context = match self.context {
            Some(ErrorContext::Command) => "command",
            Some(ErrorContext::Entry) => "entry",
            None => "unknown thing",
        };
        writeln!(write, "I'm skipping whatever remains of this {}", context)?;
        Ok(())
    }


    fn write_compatible_errmsg_dyn(&self, write: &mut dyn io::Write, filename: &str) -> io::Result<()> {
        use ErrorKind::*;

        macro_rules! err {
            ($($args:expr),*) => {
                {
                    write!(write, $($args,)*)?;
                    self.common_err_msg(write, filename)?;
                }
            }
        };

        match self.kind {
            UnexpectedEOF => err!("Illegal end of database file"),
            ExpectedEquals => err!("I was expecting an \"=\""),
            ExpectedEither(a, b) => err!("I was expecting a `{}' or a `{}'", xchr(a), xchr(b)),
            UnterminatedPreamble(a) => err!("Missing \"{}\" in preamble command", xchr(a)),
            UnterminatedString(a) => err!("Missing \"{}\" in string command", xchr(a)),
            EmptyId(kind) => err!("You're missing {}", kind),
            InvalidIdChar(kind) => err!("\"{}\" immediately follows {}", xchr(self.current_input[self.input_col]), kind),
            UnbalancedBraces => err!("Unbalanced braces"),
            Io(ref e) => err!("I/O error {}", e),
        }
        Ok(())
    }

    /// Writes (to `write`) an error message identical to one that `BibTex` would produce.
    ///
    /// Because BibTex's error messages can include the name of the input file, you need to tell us
    /// what it is.
    pub fn write_compatible_errmsg<W: io::Write>(&self, mut write: W, filename: &str) -> io::Result<()> {
        self.write_compatible_errmsg_dyn(&mut write, filename)
    }
}

/// These are the various places that identifiers can occur in a `.bib` file.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IdentifierKind {
    /// Identifiers can appear on the right hand side of a field (where they are interpreted as
    /// macro invocations). For example, in `@string{ janfeb = jan # "or February" }` then `jan`
    /// is an identifier.
    FieldPart,
    /// The kind of an entry is an identifier, like "article" in `@article{key}`.
    EntryKind,
    /// In a string command like `@string{key = "value"}`, the key is an identifier.
    StringName,
    /// The left-hand-side of an entry field is an identifier, like "title" in
    /// `@article{key, title="My Title"}`.
    FieldName,
}

// TODO: this might not be the best, since the code here is really specific to the
// bibtex-compatible error printer.
impl std::fmt::Display for IdentifierKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use IdentifierKind::*;
        match self {
            FieldPart => f.write_str("a field part"),
            EntryKind => f.write_str("an entry type"),
            StringName => f.write_str("a string name"),
            FieldName => f.write_str("a field name"),
        }
    }
}

/// These are the kinds of errors that might be encountered in a `.bib` file.
///
/// Most of these error kinds correspond to a particular error message that you might get from
/// BibTex. The exception is `Io`, since BibTex doesn't check for I/O errors (it treats them as an
/// EOF).
#[derive(Clone, Debug)]
pub enum ErrorKind {
    /// We encountered the end of the file before we expected to.
    UnexpectedEOF,
    /// We were expecting to see an identifier, but instead we found a character that isn't allowed
    /// in an identifier.
    EmptyId(IdentifierKind),
    /// While in the middle of reading an identifier, we found a character that was unexpected.
    InvalidIdChar(IdentifierKind),
    /// We wanted an equals sign, but didn't get it.
    ExpectedEquals,
    /// We were expecting one of two characters, but didn't get either one.
    ExpectedEither(u8, u8),
    /// While reading a string, we found a right brace without a corresponding left brace.
    UnbalancedBraces,
    /// A `@preamble` command was missing its closing delimiter.
    UnterminatedPreamble(u8),
    /// A `@string` command was missing its closing delimiter.
    UnterminatedString(u8),
    /// An I/O error occurred while reading the `.bib` file.
    Io(Arc<io::Error>),
}

/// Some of the error kinds in [`ErrorKind`] can happen either in different contexts within the
/// `.bib` file. We keep track of the context, because it's sometimes needed for the error message.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ErrorContext {
    /// The error was encountered while parsing a @string or @preamble command.
    Command,
    /// The error was encountered while parsing an entry (i.e. something like @article{ }).
    Entry,
}

impl From<io::Error> for ErrorKind {
    fn from(e: io::Error) -> ErrorKind {
        ErrorKind::Io(Arc::new(e))
    }
}

// The presence of io::Error prevents us from automatically deriving this.
impl PartialEq for ErrorKind {
    fn eq(&self, other: &ErrorKind) -> bool {
        use ErrorKind::*;

        match (self, other) {
            (UnexpectedEOF, UnexpectedEOF) => true,
            (UnbalancedBraces, UnbalancedBraces) => true,
            (EmptyId(a), EmptyId(b)) => a == b,
            (InvalidIdChar(a), InvalidIdChar(b)) => a == b,
            (ExpectedEither(a, b), ExpectedEither(a1, b1)) => a == a1 && b == b1,
            (ExpectedEquals, ExpectedEquals) => true,
            (UnterminatedString(a), UnterminatedString(b)) => a == b,
            (UnterminatedPreamble(a), UnterminatedPreamble(b)) => a == b,
            _ => false,
        }
    }
}

