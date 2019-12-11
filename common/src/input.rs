use std::io;

/// A helper for reading input line-by-line.
pub struct Input<'read> {
    read: Box<dyn io::BufRead + 'read>,
    // A buffer containing the current line.
    line: Vec<u8>,
    line_num: usize,
    // The index of the next character to be read from the current line.
    col: usize,
    saw_eof: bool,
}

// This is Bibtex's definition of whitespace. It's a bit puzzling that it includes '\r' but not
// '\n'. It shouldn't make a difference, though, because either '\r' or '\n' is considered a line
// terminator, and gets trimmed in `Input::input_line`.
pub fn is_white(b: u8) -> bool {
    b == b'\t' || b == b'\r' || b == b' '
}

pub fn is_id(b: u8) -> bool {
    match b {
        0..=31 | b' ' | b'"' | b'#' | b'%' | b'\'' | b'(' | b')' | b',' | b'=' | b'{' | b'}' => false,
        _ => true,
    }
}

pub fn is_digit(b: u8) -> bool {
    b'0' <= b && b <= b'9'
}

impl<'read> Input<'read> {
    pub fn from_reader<R: io::BufRead + 'read>(read: R) -> Input<'read> {
        Input {
            read: Box::new(read),
            line: Vec::new(),
            // Line numbering starts from 1, so we initialize to zero and increment to one the
            // first time that input_line is called.
            line_num: 0,
            col: 0,
            saw_eof: false,
        }
    }

    pub fn cur_line_num(&self) -> usize {
        self.line_num
    }

    pub fn cur_col_num(&self) -> usize {
        self.col
    }

    /// Returns the entire contents of the current line (including the part before
    /// `self.cur_col_num()`).
    pub fn cur_line(&self) -> &[u8] {
        &self.line[..]
    }

    pub fn is_eof(&self) -> bool {
        self.saw_eof && self.col == self.line.len()
    }

    /// This returns true if the current line is not terminated by a newline character. In other
    /// words, this returns true if we are on the last line, *and* the last line is not
    /// newline-terminated.
    pub fn no_more_newlines(&self) -> bool {
        self.saw_eof
    }

    /// Reads a new line from the input, updating our internal line buffer. The buffer will be
    /// trimmed of whitespace on the right.
    ///
    /// Returns true if there was a line to read, and false if we have already reached EOF.
    //
    // bibtex.c hard-codes end-of-line as either '\r' or '\n'. Therefore, we can't use rust's
    // built-in `BufRead::read_until` function (which can only test for a single byte).
    // Instead, we roll our own, with inspiration from the stdlib's implementation.
    pub fn input_line(&mut self) -> io::Result<bool> {
        if self.saw_eof {
            return Ok(false);
        }
        
        self.line.clear();
        loop {
            let (done, used) = {
                let available = match self.read.fill_buf() {
                    Ok(n) => n,
                    Err(ref e) if e.kind() == io::ErrorKind::Interrupted => continue,
                    Err(e) => return Err(e),
                };
                match memchr::memchr2(b'\n', b'\r', available) {
                    Some(i) => {
                        // Note that we don't include the newline character in the line, but we do
                        // mark it as consumed from the input.
                        self.line.extend_from_slice(&available[..i]);
                        (true, i + 1)
                    }
                    None => {
                        self.line.extend_from_slice(available);
                        (false, available.len())
                    }
                }
            };

            self.read.consume(used);
            if done || used == 0 {
                break;
            }
        }

        let ws_count = self.line.iter().rev().take_while(|x| is_white(**x)).count();
        self.line.truncate(self.line.len() - ws_count);
        self.col = 0;
        self.line_num += 1;
        self.saw_eof = self.read.fill_buf().map(|xs| xs.len()).unwrap_or(0) == 0;
        Ok(true)
    }

    /// Advances past all white space, returning true if there is still something left on the
    /// current line.
    pub fn skip_white_space(&mut self) -> bool {
        let ws_count = self.line[self.col..].iter().take_while(|x| is_white(**x)).count();
        self.col += ws_count;
        self.col < self.line.len()
    }

    /// Returns true if the current position is at the end of a line.
    pub fn is_eol(&self) -> bool {
        self.col == self.line.len()
    }

    /// Returns the current byte, panicking if the line was already fully consumed.
    ///
    /// The panicking is a bit unforgiving, but it turns out to be useful for shaking out bugs in
    /// the parser. The point is that at most (all?) places where we want to inspect the current
    /// character, the parser should already know that there is one (usually, by calling
    /// [`skip_white_space`] first).
    pub fn current(&self) -> u8 {
        self.line[self.col]
    }

    /// Advances one byte.
    pub fn advance(&mut self) {
        self.col += 1;
    }

    /// Unadvances one byte, panicking if we're at the start of the line.
    pub fn unadvance(&mut self) {
        assert!(self.col > 0);
        self.col -= 1;
    }

    /// Advance by (potentially) multiple bytes.
    pub fn advance_by(&mut self, count: usize) {
        self.col += count;
    }

    /// Advances the input past all characters satisfying the predicate.
    pub fn skip_past<F: FnMut(u8) -> bool>(&mut self, f: F) {
        self.col += self.count(f);
    }

    /// Advances the input to the next character satisfying the predicate, returning true if it
    /// found one.
    pub fn skip_to<F: FnMut(u8) -> bool>(&mut self, mut f: F) -> bool {
        self.col += self.count(|c| !f(c));
        !self.is_eol()
    }

    /// Returns what is left of the current line.
    pub fn buffer(&self) -> &[u8] {
        &self.line[self.col..]
    }

    /// Returns what is left of the current line.
    pub fn buffer_mut(&mut self) -> &mut [u8] {
        &mut self.line[self.col..]
    }

    /// Counts the number of successive characters, starting from the current position, that satisfy
    /// the predicate `f`.
    pub fn count<F: FnMut(u8) -> bool>(&self, mut f: F) -> usize {
        self.buffer().iter().take_while(|c| f(**c)).count()
    }

    pub fn scan_identifier(&mut self) -> Vec<u8> {
        if is_digit(self.current()) {
            Vec::new()
        } else {
            let count = self.count(is_id);
            let mut id = self.buffer()[..count].to_owned();
            id.make_ascii_lowercase();
            self.advance_by(count);
            id
        }
    }

    pub fn scan_nonnegative_number(&mut self) -> Vec<u8> {
        self.scan_while(is_digit)
    }

    pub fn scan_while<F: FnMut(u8) -> bool>(&mut self, f: F) -> Vec<u8> {
        let count = self.count(f);
        let ret = self.buffer()[..count].to_owned();
        self.advance_by(count);
        ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input_line_and_eof() {
        // If there is a single line terminated by a '\n', we refuse to advance to an empty second
        // line.
        let mut input = Input::from_reader(&b"line 1\n"[..]);
        assert_eq!(input.input_line().unwrap(), true);
        assert_eq!(&input.line, b"line 1");
        assert_eq!(input.line_num, 1);
        assert!(input.saw_eof);

        assert_eq!(input.input_line().unwrap(), false);
        assert_eq!(&input.line, b"line 1");
        assert_eq!(input.line_num, 1);
        assert!(input.saw_eof);

        // If there is a second line that is empty, but terminated by a '\n', then we do advance to
        // it.
        let mut input = Input::from_reader(&b"line 1\n\n"[..]);
        input.input_line().unwrap();
        assert_eq!(&input.line, b"line 1");
        assert_eq!(input.line_num, 1);
        assert!(!input.saw_eof);

        input.input_line().unwrap();
        assert_eq!(&input.line, b"");
        assert_eq!(input.line_num, 2);
        assert!(input.saw_eof);

        // If the second line is non-empty but newline-terminated, that's also ok.
        let mut input = Input::from_reader(&b"line 1\nline 2"[..]);
        input.input_line().unwrap();
        assert_eq!(&input.line, b"line 1");
        assert_eq!(input.line_num, 1);
        assert!(!input.saw_eof);

        input.input_line().unwrap();
        assert_eq!(&input.line, b"line 2");
        assert_eq!(input.line_num, 2);
        assert!(input.saw_eof);
    }
}

