#![deny(missing_docs)]

//! This is a crate for parsing BibTex's `.bib` files. It aims to support behavior that is
//! identical to that of BibTex.
//!
//! The main entry point of this crate is [`Parser`], which allows iteration over entries in a
//! `.bib` file.
//!
//! # Examples
//!
//! The simplest interface to this crate is to use [`Parser`] as an iterator over [`Entry`]s.
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
//! let mut parser = Parser::new(bib_data.as_bytes(), ());
//! let entries = parser.entries().collect::<Vec<_>>();
//!
//! assert_eq!(&entries[0].fields[&b"author"[..]], b"Herman Melville");
//! assert_eq!(&entries[1].fields[&b"author"[..]], b"Shannon, C. E.");
//! ```
//!
//! For a slightly more powerful interface, [`DatabaseBuilder`] is capable of reading and merging
//! multiple `.bib` files, and it can also do some postprocessing (such as merging in
//! cross-references) afterwards.
//!
//! ```
//! use bib::DatabaseBuilder;
//! let bib_data1 = r#"
//! @book{Melville:51,
//!     title = "Moby Dick",
//!     author = "Herman Melville",
//! }"#;
//! let bib_data2 = r#"
//! @article{Shannon:48,
//!     title = "A Mathematical Theory of Communication",
//!     author = "Shannon, C. E.",
//! }"#;
//!
//! let mut db = DatabaseBuilder::with_all_citations();
//! db.merge_bib_file(bib_data1.as_bytes(), ());
//! db.merge_bib_file(bib_data2.as_bytes(), ());
//! let entries = db.into_entries(());
//! assert_eq!(&entries[0].fields[&b"author"[..]], b"Herman Melville");
//! assert_eq!(&entries[1].fields[&b"author"[..]], b"Shannon, C. E.");
//! ```
//!
//! # Compatibility
//!
//! This crate aims to be compatible with standard BibTex, including all the weird corner cases. It
//! is capable of producing error messages that are identical to BibTex's, and in the future will
//! also support identical warning messages and other terminal output.
//!
//! There is one family of caveats regarding BibTex compatibility: BibTex *always* reads a `.bib`
//! file in the context of a `.aux` file and a `.bst` file; it has no standalone parser. This
//! context provides BibTex with extra information that we don't have access to in a standalone
//! parser, specifically:
//! 
//! - Predefined macros: `.bst` files can define macros that can be encountered and expanded within
//!     the `.bib` file. For example, many standard `.bst` files the macro `jan`, which expands
//!     to `"January"`. Since our parser doesn't know which macros should be predefined, you will
//!     need to tell it.
//! - Entry filters: the `.aux` file tells BibTex which entries are required (usually, only the
//!     ones that were actually cited). By default, we just read all the entries (but filtering is
//!     supported).
//! - Entry kinds and field names: the `.bst` file tells BibTex which entry kinds (e.g. `article`,
//!     `book`) are supported, and which field names (e.g. `author`, `title`) are supported. If you
//!     want this crate to filter out unsupported entries and field names, you'll need to tell it.

use std::collections::HashMap;

mod citation_list;
mod database;
pub mod error;
mod parser;

pub use database::DatabaseBuilder;
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

impl Entry {
    pub(crate) fn empty() -> Entry {
        Entry {
            kind: Vec::new(),
            key: Vec::new(),
            fields: HashMap::new(),
        }
    }
}

/// A string of bytes.
pub type BString = Vec<u8>;

/// A byte string that's guaranteed to be in ASCII lowercase.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct BStringLc(BString);

impl BStringLc {
    /// Creates a new, empty byte string.
    pub fn new() -> BStringLc {
        BStringLc(Vec::new())
    }

    /// Converts from (a possibly non-lowercased) byte string to a lowercased byte string.
    pub fn from_bytes<I: AsRef<[u8]>>(bytes: I) -> BStringLc {
        BStringLc(bytes.as_ref().to_ascii_lowercase())
    }

    /// Replaces the bytes in this string with some new ones.
    ///
    /// `self.set_from_bytes(..)` is equivalent to `*self = from_bytes(..)`, except that it will
    /// try to reuse its allocation.
    pub fn set_from_bytes<I: AsRef<[u8]>>(&mut self, bytes: I) {
        self.0.clear();
        self.0.extend_from_slice(bytes.as_ref());
        self.0.make_ascii_lowercase();
    }
}

impl std::ops::Deref for BStringLc {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        &self.0
    }
}
