use std::io;

enum MessageHistory {
    Spotless,
    Warning,
    Error,
    Fatal,
}

pub struct BibFileState {
    bib_name: String,
    bib_line_num: u32,
    read_performed: bool,
    reading_completed: bool,
}

pub struct Messages {
    writer: Box<dyn io::Write>,
    err_count: u32,
    history: MessageHistory,
}

impl Messages {
    /// Write's BibTex's closing messages. The return value is BibTex's exit value.
    pub fn write_cleanup(&mut self, state: &BibFileState) -> io::Result<i32> {
        if state.read_performed && !state.reading_completed {
            writeln!(&mut self.writer, "Aborted at line {} of file", state.bib_line_num)?;
            writeln!(&mut self.writer, "{}", state.bib_name)?;
        }

        match self.history {
            MessageHistory::Spotless => {
                if self.err_count == 1 {
                    writeln!(&mut self.writer, "(There was 1 warning)")?;
                } else {
                    writeln!(&mut self.writer, "(There were {} warnings)", self.err_count)?;
                }
                Ok(0)
            }
            MessageHistory::Warning => {
                if self.err_count == 1 {
                    writeln!(&mut self.writer, "(There was 1 error message)")?;
                } else {
                    writeln!(&mut self.writer, "(There were {} error messages)", self.err_count)?;
                }
                Ok(1)
            }
            MessageHistory::Error => {
                writeln!(&mut self.writer, "(That was a fatal error)")?;
                Ok(2)
            }
            MessageHistory::Fatal => {
                Ok(3)
            }
        }
    }
}
