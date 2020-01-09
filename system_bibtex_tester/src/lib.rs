use bstr::{BStr, BString, ByteSlice};
use std::collections::HashSet;
use tempfile::{TempDir, tempdir};

use bib::Parser;
use bib::error::ProblemKind;

pub struct BibtexRunner {
    bib_data: Vec<u8>,
    dir: TempDir,
    citation_list: Option<Vec<Vec<u8>>>,
}

impl BibtexRunner {
    pub fn new(data: &[u8]) -> BibtexRunner {
        let dir = tempdir().unwrap();
        let bst_path = dir.path().join("min.bst");
        let bib_path = dir.path().join("min.bib");
        let aux_path = dir.path().join("min.aux");

        std::fs::write(&bst_path, &include_bytes!("min.bst")[..]).unwrap();
        std::fs::write(&bib_path, data).unwrap();
        std::fs::write(&aux_path, &b"\\bibstyle{min}\n\\bibdata{min}\n\\citation{*}"[..]).unwrap();

        BibtexRunner {
            bib_data: data.to_vec(),
            dir,
            citation_list: None,
        }
    }

    pub fn set_bib_input(&mut self, data: &[u8]) {
        self.bib_data = data.to_vec();
        let bib_path = self.dir.path().join("min.bib");
        std::fs::write(&bib_path, data).unwrap();
    }

    pub fn set_citation_list(&mut self, citations: &[&'static [u8]]) {
        self.citation_list = Some(citations.iter().map(|x| x.to_vec()).collect());
        let mut aux_data = Vec::new();
        let aux_path = self.dir.path().join("min.aux");

        use std::io::Write;
        writeln!(&mut aux_data, "\\bibstyle{{min}}").unwrap();
        writeln!(&mut aux_data, "\\bibdata{{min}}").unwrap();
        for c in self.citation_list.as_ref().unwrap() {
            // NOTE: this probably only works as expected if the keys are all ASCII. Since we
            // control the testing data, this is ok for now.
            writeln!(&mut aux_data, "\\citation{{{}}}", BString::from(&c[..])).unwrap();
        }
        std::fs::write(&aux_path, &aux_data).unwrap();
    }

    pub fn reference_output(&mut self) -> BibtexOutput {
        let output = std::process::Command::new("bibtex")
            .current_dir(self.dir.path())
            .arg("min")
            .output()
            .expect("failed to run bibtex");

        let bbl_path = self.dir.path().join("min.bbl");
        let bbl_data = BString::from(std::fs::read(&bbl_path).expect("couldn't read bbl"));

        println!("{}", BString::from(&output.stdout[..]));

        BibtexOutput {
            // FIXME: for some reason I don't understand, our bst file is producing lots of blank
            // lines. So we filter them out, but this means we're also filtering out any entries
            // that truly have an empty key. So that isn't great.
            bbl_lines: bbl_data.lines().map(BString::from).filter(|s| !s.is_empty()).collect(),
            stdout: trim_stdout((&output.stdout[..]).into()),
        }
    }

    /// Asserts that the reference bibtex and our implementation give the same parse errors (and,
    /// if `check_warnings` is true, also checks warning messages. Returns true if the parse
    /// succeeded with no errors.
    pub fn check_ours(&mut self, check_warnings: bool) -> bool {
        let reference_output = self.reference_output();
        let mut found_error = false;
        let mut errors = Vec::new();
        let mut error_buf = Vec::new();

        // These need to match the ENTRY command in the included .bst file, which is hard-coded for
        // now.
        let mut known_fields = HashSet::new();
        known_fields.insert(&b"field"[..]);
        known_fields.insert(b"title");
        known_fields.insert(b"author");

        let mut parser = Parser::new(&self.bib_data[..], &mut errors)
            .with_entry_type_checker(|s| s == b"article")
            .with_field_name_checker(move |s| known_fields.get(s).is_some());

        if let Some(ref citation_list) = self.citation_list {
            parser = parser.with_citation_list(citation_list, 2);
        }

        let mut keys = Vec::new();
        for entry in parser.entries() {
            if !entry.key.is_empty() { 
                // because of the FIXME above, we have to filter out empty keys here too.
                keys.push(String::from_utf8(entry.key).unwrap())
            }
        }
        drop(parser); // Because it's borrowing errors.
        for err in errors {
            // FIXME: We only check for equality of the error messages, not the warning messages.
            // That's because some of the warning messages that we want to test for don't show up
            // in bibtex. For example, bibtex doesn't do macro substitution for field types it
            // doesn't know, and so it doesn't raise warnings for those.
            if let ProblemKind::Error(_) = err.kind {
                err.write_compatible_errmsg(&mut error_buf, "min.bib").unwrap();
                found_error = true;
            } else if check_warnings {
                err.write_compatible_errmsg(&mut error_buf, "min.bib").unwrap();
            }
        }

        assert_eq!(keys, reference_output.bbl_lines);
        assert_eq!(BString::from(error_buf.to_ascii_lowercase()), BString::from(reference_output.stdout.to_ascii_lowercase()));
        !found_error
    }
}

pub struct BibtexOutput {
    pub bbl_lines: Vec<BString>,
    pub stdout: BString,
}

// Trim uninteresting parts from the stdout (namely, version printing, warnings, and total number of errors)
fn trim_stdout(bibtex_stdout: &BStr) -> BString {
    let mut ret = BString::from("");

    // This is pretty hacky, and only works if there's just one database file. We skip four lines:
    //
    // This is BibTeX, Version...
    // The top-level auxiliary file: ...
    // The style file: ...
    // Database file #1: ...
    for line in bibtex_stdout.lines().skip(4) {
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
            let mut runner = $crate::BibtexRunner::new(include_bytes!($bib_file));
            assert!(runner.check_ours(true));
        }
    };

    ($test_name:ident, $bib_file:expr, $citations:expr) => {
        #[test]
        fn $test_name() {
            let mut runner = $crate::BibtexRunner::new(include_bytes!($bib_file));
            runner.set_citation_list(&$citations[..]);
            assert!(runner.check_ours(true));
        }
    };
}

#[macro_export]
macro_rules! list_entries_failure {
    ($test_name:ident, $bib_file:expr) => {
        #[test]
        fn $test_name() {
            let mut runner = $crate::BibtexRunner::new(include_bytes!($bib_file));
            assert!(!runner.check_ours(true));
        }
    };

    ($test_name:ident, $bib_file:expr, $citations:expr) => {
        #[test]
        fn $test_name() {
            let mut runner = $crate::BibtexRunner::new(include_bytes!($bib_file));
            runner.set_citation_list(&$citations[..]);
            assert!(!runner.check_ours(true));
        }
    };
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runner()
    {
        let mut runner = BibtexRunner::new(b"@article{mykey, title={mytitle},}");
        let output = runner.reference_output();
        assert_eq!(output.bbl_lines, vec!["mykey".to_owned()]);

        runner.set_bib_input(b"@article{mykey, title={mytitle},");
        let output = runner.reference_output();
        assert_eq!(output.stdout, "Illegal end of database file---line 1 of file min.bib\n : @article{mykey, title={mytitle},\n :                                 \nI\'m skipping whatever remains of this entry\n");
    }
}
