//! This module provides various types and methods for reporting errors in `.bib` files. The main
//! struct is [`Error`].

use common::Input;
use common::input::is_white;
use std::io;
use std::sync::Arc;

use crate::BString;

fn xchr(c: u8) -> char {
    match c {
        127 => ' ', // DEL
        b'\t' => '\t',
        0..=31 => ' ',
        x => x.into(),
    }
}

fn xchrs(cs: &[u8]) -> String {
    cs.into_iter().cloned().map(xchr).collect()
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
    /// We found two entries with the same name (up to case differences, which don't count).
    RepeatedEntry,
    /// While reading a string, we found a right brace without a corresponding left brace.
    UnbalancedBraces,
    /// A `@preamble` command was missing its closing delimiter.
    UnterminatedPreamble(u8),
    /// A `@string` command was missing its closing delimiter.
    UnterminatedString(u8),
    /// An I/O error occurred while reading the `.bib` file.
    Io(Arc<io::Error>),
}

impl From<io::Error> for ErrorKind {
    fn from(e: io::Error) -> ErrorKind {
        ErrorKind::Io(Arc::new(e))
    }
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

/// These are the kinds of warnings that might be encountered while parsing a `.bib` file.
///
/// The dividing line between errors and warnings seems to be that upon an error, skip over the
/// rest of the input until a blank line is encountered.
#[derive(Clone, Debug)]
pub enum WarningKind {
    /// We tried to expand a string during its own definition.
    RecursiveString(Vec<u8>),
    /// We tried to expand a string that wasn't defined.
    UndefinedString(Vec<u8>),
    /// We encountered an unknown entry type. (The payload here is the key of the entry, not the
    /// type.)
    UnknownEntryType(Vec<u8>),
}

/// These are the kinds of warnings that might be encountered while resolving cross-references
/// in a `.bib` file.
#[derive(Clone, Debug)]
pub enum DatabaseWarningKind {
    /// We were expecting to find an entry that wasn't there.
    MissingEntry(BString),
    /// We encountered a nested cross-reference: an entry that was referenced in a `crossref` field
    /// has a `crossref` field of its own.
    NestedCrossRef {
        /// The key of the entry that was cross-referenced and contained a cross-reference.
        child: BString,
        /// The key that `child` referenced.
        parent: BString
    },
}

// This corresponds to WEB section 221 in BibTex. It prints the line number, the contents of
// the bad line, and the text "skipping whatever remains".
fn common_err_msg(write: &mut dyn io::Write, ctxt: ErrorContext, input: &Input, filename: &str) -> io::Result<()> {
    writeln!(write, "---line {} of file {}", input.cur_line_num(), filename)?;

    // Print the current line, divided into two at the current column. Whitespace characters are
    // replaced by spaces.
    fn map_space(buf: &[u8]) -> Vec<u8> {
        buf.iter().map(|&c| if is_white(c) { b' ' } else { c }).collect()
    }

    let col_num = input.cur_col_num();
    let before: Vec<u8> = map_space(&input.cur_line()[..col_num]);
    let spaces = vec![b' '; before.len()];
    let after: Vec<u8> = map_space(&input.cur_line()[col_num..]);
    write.write_all(b" : ")?;
    write.write_all(&before)?;
    write.write_all(b"\n : ")?;
    write.write_all(&spaces)?;
    write.write_all(&after)?;
    write.write_all(b"\n")?;

    if before.iter().all(|&c| is_white(c)) {
        write.write_all(b"(Error may have been on previous line)\n")?;
    }
    let context = match ctxt {
        ErrorContext::Command => "command",
        ErrorContext::Entry => "entry",
    };
    writeln!(write, "I'm skipping whatever remains of this {}", context)?;
    Ok(())
}

fn common_warn_msg(write: &mut dyn io::Write, input: &Input, filename: &str) -> io::Result<()> {
    writeln!(write, "--line {} of file {}", input.cur_line_num(), filename)?;
    Ok(())
}

fn write_compatible_err(write: &mut dyn io::Write, kind: &ErrorKind, ctxt: ErrorContext, input: &Input, filename: &str) -> io::Result<()> {
    use ErrorKind::*;

    macro_rules! err {
        ($($args:expr),*) => {
            {
                write!(write, $($args,)*)?;
                common_err_msg(write, ctxt, input, filename)?;
            }
        }
    };

    match *kind {
        UnexpectedEOF => err!("Illegal end of database file"),
        ExpectedEquals => err!("I was expecting an \"=\""),
        ExpectedEither(a, b) => err!("I was expecting a `{}' or a `{}'", xchr(a), xchr(b)),
        UnterminatedPreamble(a) => err!("Missing \"{}\" in preamble command", xchr(a)),
        UnterminatedString(a) => err!("Missing \"{}\" in string command", xchr(a)),
        EmptyId(kind) => err!("You're missing {}", kind),
        InvalidIdChar(kind) => err!("\"{}\" immediately follows {}", xchr(input.cur_line()[input.cur_col_num()]), kind),
        RepeatedEntry => err!("Repeated entry"),
        UnbalancedBraces => err!("Unbalanced braces"),
        Io(ref e) => err!("I/O error {}", e),
    }
    Ok(())
}

fn write_crossref_err(write: &mut dyn io::Write, child: &[u8], parent: &[u8]) -> io::Result<()> {
    writeln!(
        write,
        "A bad cross reference---entry \"{}\"\nrefers to entry \"{}\", which doesn't exist",
        xchrs(child),
        xchrs(parent))
}

fn write_compatible_warning(write: &mut dyn io::Write, kind: &WarningKind, input: &Input, filename: &str) -> io::Result<()> {
    use WarningKind::*;

    macro_rules! warn {
        ($($args:expr),*) => {
            {
                writeln!(write, $($args,)*)?;
                common_warn_msg(write, input, filename)?;
            }
        }
    };

    match kind {
        RecursiveString(ref name) => warn!("Warning--string name \"{}\" is used in its own definition", xchrs(name)),
        UndefinedString(ref name) => warn!("Warning--string name \"{}\" is undefined", xchrs(name)),
        UnknownEntryType(ref key) => warn!("Warning--entry type for \"{}\" isn't style-file defined", xchrs(key)),
    }

    Ok(())
}

fn write_compatible_db_warning(write: &mut dyn io::Write, kind: &DatabaseWarningKind) -> io::Result<()> {
    use DatabaseWarningKind::*;

    match kind {
        MissingEntry(ref name) => writeln!(write, "Warning--I didn't find a database entry for \"{}\"", xchrs(name)),
        NestedCrossRef { ref child, ref parent } =>
            writeln!(
                write,
                "Warning--you've nested cross references--entry \"{}\"\nrefers to entry \"{}\", which also refers to something",
                xchrs(child),
                xchrs(parent)),
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


/// An `ProblemReporter` provides hooks for the parser to call when it encounters an error or a
/// warning.
pub trait ProblemReporter {
    /// Reports an error that occurred while parsing a `.bib` file.
    fn parse_error(&mut self, error: &ErrorKind, ctxt: ErrorContext, input: &Input);

    /// Reports an invalid crossreference that occurred in a `.bib` file (this isn't one of the error
    /// types handled by `parse_error` because invalid crossreferences are detected after the input
    /// is completely read, so there is no corresponding input location).
    fn crossref_error(&mut self, child: &[u8], parent: &[u8]);

	/// Reports a warning that occurred while parsing a `.bib` file.
    fn parse_warning(&mut self, warning: &WarningKind, input: &Input);

    /// Reports a warning that occurred while post-processing a `.bib` file.
    fn database_warning(&mut self, warning: &DatabaseWarningKind);
}

/// The `ProblemReporter` impl for `()` simply ignores all errors and warnings.
impl ProblemReporter for () {
    fn parse_error(&mut self, _error: &ErrorKind, _ctxt: ErrorContext, _input: &Input) {}
    fn crossref_error(&mut self, _child: &[u8], _parent: &[u8]) {}

    fn parse_warning(&mut self, _warning: &WarningKind, _input: &Input) {}
    fn database_warning(&mut self, _warning: &DatabaseWarningKind) {}
}

/// `CompatibleProblemReporter` implements `ProblemReporter` by writing all error messages
/// to `write`, in a bibtex-compatible format. It also sets `found_error` to `true` if any
/// error was reported.
pub struct CompatibleProblemReporter<'a, W: io::Write> {
    /// All error messages will be written here.
    pub write: W,

    /// The name of the `.bib` file (used for writing the error messages).
    pub filename: &'a str,

    /// This will be set to `true` if an error is reported.
    pub found_error: bool,
}

impl<'a, 'b, W: io::Write> ProblemReporter for &'a mut CompatibleProblemReporter<'b, W> {
    fn parse_error(&mut self, error: &ErrorKind, ctxt: ErrorContext, input: &Input) {
        self.found_error = true;
        let _ = write_compatible_err(&mut self.write, error, ctxt, input, &self.filename);
    }

    fn crossref_error(&mut self, child: &[u8], parent: &[u8]) {
        self.found_error = true;
        let _ = write_crossref_err(&mut self.write, child, parent);
    }

    fn parse_warning(&mut self, warning: &WarningKind, input: &Input) {
        let _ = write_compatible_warning(&mut self.write, warning, input, &self.filename);
    }

    fn database_warning(&mut self, warning: &DatabaseWarningKind) {
        let _ = write_compatible_db_warning(&mut self.write, warning);
    }
}

