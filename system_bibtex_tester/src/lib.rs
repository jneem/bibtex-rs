use bstr::{BStr, BString, ByteSlice};
use tempfile::{TempDir, tempdir};

use bib::Parser;
use bib::error::ProblemKind;

pub struct BibtexRunner {
    bib_data: Vec<u8>,
    dir: TempDir,
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
        }
    }

    pub fn set_bib_input(&mut self, data: &[u8]) {
        self.bib_data = data.to_vec();
        let bib_path = self.dir.path().join("min.bib");
        std::fs::write(&bib_path, data).unwrap();
    }

    pub fn reference_output(&mut self) -> BibtexOutput {
        let output = std::process::Command::new("bibtex")
            .current_dir(self.dir.path())
            .arg("min")
            .output()
            .expect("failed to run bibtex");

        let bbl_path = self.dir.path().join("min.bbl");
        let bbl_data = BString::from(std::fs::read(&bbl_path).expect("couldn't read bbl"));

        BibtexOutput {
            // FIXME: for some reason I don't understand, our bst file is producing lots of blank
            // lines. So we filter them out, but this means we're also filtering out any entries
            // that truly have an empty key. So that isn't great.
            bbl_lines: bbl_data.lines().map(BString::from).filter(|s| !s.is_empty()).collect(),
            stdout: trim_stdout((&output.stdout[..]).into()),
        }
    }

    // Asserts that the reference bibtex and our implementation give the same parse errors. Returns
    // true if the parse succeeded with no errors.
    pub fn check_ours(&mut self) -> bool {
        let reference_output = self.reference_output();
        let mut errors = Vec::new();
        let mut error_buf = Vec::new();
        let parser = Parser::new(&self.bib_data[..], &mut errors);

        let mut keys = Vec::new();
        for entry in parser.entries() {
            if !entry.key.is_empty() { 
                // because of the FIXME above, we have to filter out empty keys here too.
                keys.push(String::from_utf8(entry.key).unwrap())
            }
        }
        for err in errors {
            // FIXME: We only check for equality of the error messages, not the warning messages.
            // That's because some of the warning messages that we want to test for don't show up
            // in bibtex. For example, bibtex doesn't do macro substitution for field types it
            // doesn't know, and so it doesn't raise warnings for those.
            if let ProblemKind::Error(_) = err.kind {
                err.write_compatible_errmsg(&mut error_buf, "min.bib").unwrap();
            }
        }
        let ret = error_buf.is_empty();
        assert_eq!(keys, reference_output.bbl_lines);
        assert_eq!(BString::from(error_buf.to_ascii_lowercase()), BString::from(reference_output.stdout.to_ascii_lowercase()));
        ret
    }
}

pub struct BibtexOutput {
    pub bbl_lines: Vec<BString>,
    pub stdout: BString,
}

// Trim uninteresting parts from the stdout (namely, version printing, warnings, and total number of errors)
fn trim_stdout(bibtex_stdout: &BStr) -> BString {
    let mut ret = BString::from("");
    let mut in_error = false;

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

        if line.starts_with(b"Warning--") {
            in_error = false;
        } else if line.find(b"---").is_some()
            && (line.starts_with(b"Illegal")
                || line.starts_with(b"I was expecting")
                || line.starts_with(b"Missing \"")
                || line.starts_with(b"You're missing")
                || line.starts_with(b"Unbalanced braces")
                || line[2..].starts_with(b"\" immediately follows")
            )
        {
            in_error = true;
        }

        if in_error {
            ret.extend_from_slice(line);
            ret.push(b'\n');
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
        let mut runner = BibtexRunner::new(b"@article{mykey, title={mytitle},}");
        let output = runner.reference_output();
        assert_eq!(output.bbl_lines, vec!["mykey".to_owned()]);

        runner.set_bib_input(b"@article{mykey, title={mytitle},");
        let output = runner.reference_output();
        assert_eq!(output.stdout, "Illegal end of database file---line 1 of file min.bib\n : @article{mykey, title={mytitle},\n :                                 \nI\'m skipping whatever remains of this entry\n");
    }
}
