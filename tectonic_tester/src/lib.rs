use std::ffi::OsStr;
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

        let stdout_data = String::from_utf8(self.io.files.borrow()[self.io.stdout_key()].clone()).unwrap();
        let bbl_data = String::from_utf8(self.io.files.borrow()[OsStr::new("min.bbl")].clone()).unwrap();

        BibtexOutput {
            bbl_lines: bbl_data.lines().map(|s| s.to_owned()).filter(|s| !s.is_empty()).collect(),
            warnings: Vec::new(), // FIXME
            errors: parse_errors(&stdout_data),
        }
    }

    // Asserts that the reference bibtex and our implementation give the same parse errors. Returns
    // true if the parse succeeded with no errors.
    pub fn check_ours(&mut self) -> bool {
        let reference_output = self.reference_output();
        let mut error_buf = Vec::new();
        let parser = Parser::new(&self.bib_data[..]);
        let mut keys = Vec::new();
        for entry in parser.entries() {
            match entry {
                Ok(entry) => keys.push(String::from_utf8(entry.key).unwrap()),
                Err(e) => e.write_compatible_errmsg(&mut error_buf, "min.bib").unwrap(),
            }
        }
        let ret = error_buf.is_empty();
        assert_eq!(keys, reference_output.bbl_lines);
        assert_eq!(String::from_utf8(error_buf).unwrap(), reference_output.errors);
        ret
    }
}

pub struct BibtexOutput {
    pub bbl_lines: Vec<String>,
    pub warnings: Vec<String>,
    pub errors: String,
}

// I'm not too sure how accurate this bibtex error parsing is, but we can refine it when we find
// spuriously failing tests.
//
// The current algorithm is: include everything between the first occurrence of --- and the message
// "(There was 1 error message)" (or similar).
fn parse_errors(bibtex_stdout: &str) -> String {
    let mut ret = String::new();

    for line in bibtex_stdout.lines() {
        if (line.starts_with("(There was") && line.ends_with("error message)"))
            || (line.starts_with("(There were") && line.ends_with("error messages)")) {
                // We've reached the end of the error messages, so return what we have.
                return ret;
        }

        if !ret.is_empty() || line.find("---").is_some() {
            ret.push_str(line);
            ret.push('\n');
        }
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
        assert_eq!(output.errors, "Illegal end of database file---line 1 of file min.bib\n : @article{mykey, title={mytitle},\n :                                 \nI\'m skipping whatever remains of this entry\n");
    }
}
