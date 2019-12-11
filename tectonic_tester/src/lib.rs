//! This crate contains some utilities to verify the behavior of our bibtex implementation by
//! comparing it to the reference implementation contained in tectonic.
//!
//! Note that the `system_bibtex_tester` crate contains utilities for comparing to the reference
//! implementation in the bibtex binary installed on the current system. The main advantage of this
//! crate over that one is that it's faster, and it compares warning messages in addition to error
//! messages. On the other hand, tectonic has some known memory leaks.  Overall, this one is
//! intended for handwritten tests whereas the other one is intended for fuzzing.

use bstr::{BStr, BString, ByteSlice};
use std::collections::HashSet;
use std::ffi::OsStr;
use std::ops::DerefMut;
use tectonic::BibtexEngine;
use tectonic::engines::NoopIoEventBackend;
use tectonic::io::stack::IoStack;
use tectonic::status::NoopStatusBackend;
use tectonic::io::memory::MemoryIo;

use bib::Parser;

pub struct BibtexRunner {
    bib_data: Vec<u8>,
    io: MemoryIo,
    engine: BibtexEngine,
}

impl BibtexRunner {
    pub fn new() -> BibtexRunner {
        let mut io = MemoryIo::new(true); // stdout is allowed
        io.create_entry("min.bst".as_ref(), include_bytes!("min.bst").to_vec());
        io.create_entry("min.bib".as_ref(), Vec::new());
        io.create_entry("min.aux".as_ref(), b"\\bibstyle{min}\n\\bibdata{min}\n\\citation{*}".to_vec());

        BibtexRunner {
            bib_data: Vec::new(),
            io,
            engine: BibtexEngine::new(),
        }
    }

    pub fn set_bib_input(&mut self, data: &[u8]) {
        self.bib_data = data.to_vec();
        self.io.create_entry("min.bib".as_ref(), data.to_vec());
    }

    pub fn reference_output(&mut self) -> BibtexOutput {
        let mut io_stack = IoStack::new(vec![&mut self.io]);
        let _ = self.engine.process(
            &mut io_stack,
            &mut NoopIoEventBackend::new(),
            &mut NoopStatusBackend::new(),
            "min.aux",
        );

        let stdout_data = BString::from(self.io.files.borrow()[self.io.stdout_key()].clone());
        let bbl_data = BString::from(self.io.files.borrow()[OsStr::new("min.bbl")].clone());

        BibtexOutput {
            // FIXME: for some reason I don't understand, our bst file is producing lots of blank
            // lines. So we filter them out, but this means we're also filtering out any entries
            // that truly have an empty key. So that isn't great.
            bbl_lines: bbl_data.lines().map(BString::from).filter(|s| !s.is_empty()).collect(),
            stdout: trim_stdout(stdout_data.as_ref()),
        }
    }

    // Asserts that the reference bibtex and our implementation give the same parse errors. Returns
    // true if the parse succeeded with no errors.
    pub fn check_ours(&mut self) -> bool {
        let reference_output = self.reference_output();
        let mut errors = Vec::new();
        let mut error_buf = BString::from("");

        // These need to match the ENTRY command in the included .bst file.
        let mut known_fields = HashSet::new();
        known_fields.insert(&b"field"[..]);
        known_fields.insert(b"title");
        known_fields.insert(b"author");

        let parser = Parser::new(&self.bib_data[..], &mut errors)
            .with_entry_type_checker(|s| s == b"article")
            .with_field_name_checker(move |s| known_fields.get(s).is_some());

        let mut keys = Vec::new();
        for entry in parser.entries() {
            if !entry.key.is_empty() { 
                // because of the FIXME above, we have to filter out empty keys here too.
                keys.push(String::from_utf8(entry.key).unwrap())
            }
        }
        for err in errors {
            err.write_compatible_errmsg(error_buf.deref_mut(), "min.bib").unwrap();
        }
        let ret = error_buf.is_empty();
        println!("{}", error_buf);
        assert_eq!(keys, reference_output.bbl_lines);
        assert_eq!(BString::from(error_buf.to_ascii_lowercase()), BString::from(reference_output.stdout.to_ascii_lowercase()));
        ret
    }
}

pub struct BibtexOutput {
    pub bbl_lines: Vec<BString>,
    pub stdout: BString,
}

// Trim uninteresting parts from the stdout (namely, version printing and total number of errors)
fn trim_stdout(bibtex_stdout: &BStr) -> BString {
    let mut ret = BString::from("");

    for line in bibtex_stdout.lines() {
        if line.starts_with(b"(There was") || line.starts_with(b"(There were") {
            // We've reached the end of the error messages, so return what we have.
            return ret;
        }

        ret.extend_from_slice(line);
        ret.push(b'\n');
    }

    ret
}

#[macro_export]
macro_rules! list_entries_success {
    ($test_name:ident, $bib_file:expr) => {
        #[test]
        fn $test_name() {
            let mut runner = $crate::BibtexRunner::new();
            runner.set_bib_input(include_bytes!($bib_file));
            assert!(runner.check_ours());
        }
    }
}

#[macro_export]
macro_rules! list_entries_failure {
    ($test_name:ident, $bib_file:expr) => {
        #[test]
        fn $test_name() {
            let mut runner = $crate::BibtexRunner::new();
            runner.set_bib_input(include_bytes!($bib_file));
            assert!(!runner.check_ours());
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runner()
    {
        let mut runner = BibtexRunner::new();
        runner.set_bib_input(b"@article{mykey, title={mytitle},}");
        let output = runner.reference_output();
        assert_eq!(output.bbl_lines, vec!["mykey".to_owned()]);

        runner.set_bib_input(b"@article{mykey, title={mytitle},");
        let output = runner.reference_output();
        assert_eq!(output.stdout, "Illegal end of database file---line 1 of file min.bib\n : @article{mykey, title={mytitle},\n :                                 \nI\'m skipping whatever remains of this entry\n");
    }
}
