#![deny(missing_docs)]

//! This is a crate for parsing BibTex's `.bib` files. It aims to support behavior that is
//! identical to that of BibTex.
//!
//! The main entry point of this crate is [`Parser`], which allows iteration over entries in a
//! `.bib` file.
//!
//! # Example
//!
//! ```
//! use bib::{Entry, Parser};
//! let bib_data = r#"
//! @book{Melville:51,
//!     title = "Moby Dick",
//!     author = "Herman Melville",
//! }
//!
//! @article{Shannon:48,
//!     title = "A Mathematical Theory of Communication",
//!     author = "Shannon, C. E.",
//! }
//! "#;
//!
//! let parser = Parser::new(bib_data.as_bytes(), ());
//! let entries = parser.entries().collect::<Vec<_>>();
//!
//! assert_eq!(&entries[0].fields[&b"author"[..]], b"Herman Melville");
//! assert_eq!(&entries[1].fields[&b"author"[..]], b"Shannon, C. E.");
//! ```
//!
//! # Compatibility
//!
//! This crate aims to be 100% compatible with standard BibTex, including all the weird corner
//! cases. It is capable of producing error messages that are identical to BibTex's, and in the
//! future will also support identical warning messages and other terminal output.
//!
//! There is one family of caveats regarding BibTex compatibility: BibTex *always* reads a `.bib`
//! file in the context of a `.aux` file and a `.bst` file; it has no standalone parser. This
//! context provides BibTex with extra information that we don't have access to in a standalone
//! parser, specifically:
//! 
//! - Predefined macros: `.bst` files can define macros that can be encountered and expanded within
//!     the `.bib` file. For example, many standard `.bst` files the macro `jan`, which expands
//!     to `"January"`. Since our parser doesn't know which macros should be predefined, you will
//!     need to tell it. (This is not yet supported, but it will be.)
//! - Entry filters: the `.aux` file tells BibTex which entries are required (usually, only the
//!     ones that were actually cited). We do not yet have such filtering capability.
//! - Entry kinds and field names: the `.bst` file tells BibTex which entry kinds (e.g. `article`,
//!     `book`) are supported, and which field names (e.g. `author`, `title`) are supported. This
//!     crate just accepts all entry kinds and field names.

use std::collections::HashMap;

pub mod error;
mod parser;

pub use parser::Parser;

/// For most purposes, you can think of a .bib file as a collection of entries.
///
/// (This isn't quite true -- there are other kinds of commands -- but the most interesting ones
/// are entries.) An entry looks roughly like this:
///
/// ```text
/// @article{citationkey,
///     title = {The cool article I wrote},
///     author = {me},
/// }
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct Entry {
    /// The kind of publication this entry refers to. In the example above, this is "article".
    pub kind: Vec<u8>,
    /// The citation key of this entry. In the example above, this is "citationkey".
    pub key: Vec<u8>,
    /// The collection of key/value pairs making up the rest of the entry. In the example above,
    /// there are two of them. The values do not contain their outermost delimiters, so for the
    /// example above `field[b"author"]` will return `b"me"`, not `b"{me}"`.
    pub fields: HashMap<Vec<u8>, Vec<u8>>,
}


