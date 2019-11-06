use std::ffi::OsStr;
use tectonic::BibtexEngine;
use tectonic::engines::NoopIoEventBackend;
use tectonic::io::stack::IoStack;
use tectonic::status::NoopStatusBackend;
use tectonic::io::memory::MemoryIo;

use bib::Parser;

// Creates a memory-backed I/O environment with three files: an aux file that just includes all
// references, a bst file that outputs to the bbl file all of the citation keys, and a bib file
// containing the given data.
fn create_io_environment(bib_data: String) -> MemoryIo {
    let mut ret = MemoryIo::new(true); // stdout is allowed
    ret.create_entry("min.bst".as_ref(), include_bytes!("min.bst").to_vec());
    ret.create_entry("min.bib".as_ref(), bib_data.into_bytes());
    ret.create_entry("min.aux".as_ref(), b"\\bibstyle{min}\n\\bibdata{min}\n\\citation{*}".to_vec());
    ret
}

pub struct BibtexOutput {
    pub bbl_lines: Vec<String>,
    pub warnings: Vec<String>,
    pub errors: String,
}

pub fn run_bibtex(bib_data: &str) -> BibtexOutput {
    let mut io = create_io_environment(bib_data.to_owned());
    let mut io_stack = IoStack::new(vec![&mut io]);
    let mut engine = BibtexEngine::new();

    let _ = engine.process(
        &mut io_stack,
        &mut NoopIoEventBackend::new(),
        &mut NoopStatusBackend::new(),
        "min.aux",
    );

    let stdout_data = String::from_utf8(io.files.borrow()[io.stdout_key()].clone()).unwrap();
    println!("{}", stdout_data);
    let bbl_data = String::from_utf8(io.files.borrow()[OsStr::new("min.bbl")].clone()).unwrap();

    BibtexOutput {
        bbl_lines: bbl_data.lines().map(|s| s.to_owned()).filter(|s| !s.is_empty()).collect(),
        warnings: Vec::new(), // FIXME
        errors: parse_errors(&stdout_data),
    }
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

// Asserts that the reference bibtex and our implementation give the same parse errors. Returns
// true if the parse succeeded with no errors.
pub fn check_keys_and_errors(bib_data: &str) -> bool {
    let reference_output = run_bibtex(bib_data);
    let mut error_buf = Vec::new();
    let parser = Parser::new(bib_data.as_bytes());
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

#[test]
fn test_runner()
{
    let output = run_bibtex("@article{mykey, title={mytitle},}");
    assert_eq!(output.bbl_lines, vec!["mykey".to_owned()]);

    let output = run_bibtex("@article{mykey, title={mytitle},");
    assert_eq!(output.errors, "Illegal end of database file---line 1 of file min.bib\n : @article{mykey, title={mytitle},\n :                                 \nI\'m skipping whatever remains of this entry\n");

    check_keys_and_errors("@article{mykey, title={mytitle},}");
}

macro_rules! bibtex_success {
    ($test_name:ident, $bib_file:expr) => {
        #[test]
        fn $test_name() {
            assert!(check_keys_and_errors(include_str!($bib_file)));
        }
    }
}

macro_rules! bibtex_failure {
    ($test_name:ident, $bib_file:expr) => {
        #[test]
        fn $test_name() {
            assert!(!check_keys_and_errors(include_str!($bib_file)));
        }
    }
}

bibtex_success!(basic, "basic.bib");

bibtex_failure!(missing_entry_type, "missing_entry_type.bib");
bibtex_failure!(missing_field_name, "missing_field_name.bib");
bibtex_failure!(missing_string_name, "missing_string_name.bib");
bibtex_failure!(missing_field_part, "missing_field_part.bib");
bibtex_failure!(no_key, "no_key.bib");
bibtex_failure!(unexpected_eof, "unexpected_eof.bib");
bibtex_failure!(missing_equals, "missing_equals.bib");
bibtex_failure!(expecting_delimiter, "expecting_delimiter.bib");
bibtex_failure!(unterminated_preamble, "unterminated_preamble.bib");
bibtex_failure!(unterminated_string, "unterminated_string.bib");
bibtex_failure!(unbalanced_braces, "unbalanced_braces.bib");
