use crate::input::lex_class;

pub enum NameWarning {
    BracesUnbalanced,
    TooManyCommas,
    TrailingComma,
}

pub struct TokenizedName {
    bytes: Vec<u8>,
    seps: Vec<u8>,
    tokens: Vec<usize>,
    num_commas: u8,
    token_starting: bool,
    comma1_idx: usize,
    comma2_idx: usize,

	// These are indices into `tokens`, giving the positions of the various parts of the name.
	// All of these are half-open intervals (i.e., not including the endpoint).
	//
	// An interval of length zero signifies that that part of the name isn't present.
    first_pos: (usize, usize),
    von_pos: (usize, usize),
    last_pos: (usize, usize),
    jr_pos: (usize, usize),
}

impl TokenizedName {
    fn new() -> TokenizedName {
        TokenizedName {
            bytes: Vec::new(),
            seps: Vec::new(),
            tokens: Vec::new(),
            num_commas: 0,
            token_starting: true,
            comma1_idx: 0,
            comma2_idx: 0,
            first_pos: (0, 0),
            von_pos: (0, 0),
            last_pos: (0, 0),
            jr_pos: (0, 0),
        }
    }

    pub fn from_bytes<B: AsRef<[u8]>>(bytes: B) -> TokenizedName {
        let mut ret = TokenizedName::new();
        let bytes = bytes.as_ref();

        ret.tokenize_from_bytes(bytes);
        ret.find_names();

        ret
    }

    fn consume_comma(&mut self) {
        if self.num_commas == 2 {
            // TODO: issue a warning
        } else {
            self.num_commas += 1;
            if self.num_commas == 1 {
                self.comma1_idx = self.tokens.len();
            } else {
                self.comma2_idx = self.tokens.len();
            }
            self.token_starting = true;
        }
    }

    fn consume_lbrace(&mut self, buf: &mut &[u8]) {
        debug_assert!(buf[0] == b'{');

        if self.token_starting {
            self.tokens.push(self.bytes.len());
        }

        self.bytes.push(b'{');
        let mut brace_level = 1;

        // TODO: describe the buf-incrementing convention
        while brace_level > 0 && !buf.is_empty() {
            *buf = &buf[1..];
            let ch = buf[0];
            match ch {
                b'{' => brace_level += 1,
                b'}' => brace_level -= 1,
                ch => self.bytes.push(ch),
            }
        }
        self.token_starting = false;
    }

    fn consume_rbrace(&mut self) {
        // If we get here, it means that the name contained an unbalanced right brace.
        if self.token_starting {
            self.tokens.push(self.bytes.len());
        }
        // TODO: print a warning
    }

    fn consume_sep(&mut self, ch: u8) {
        if !self.token_starting {
            self.seps.push(ch);
        }
        self.token_starting = true;
    }

    fn consume_other(&mut self, ch: u8) {
        if self.token_starting {
            self.tokens.push(self.bytes.len());
        }
        self.bytes.push(ch);
        self.token_starting = false;
    }

    fn tokenize_from_bytes(&mut self, mut bytes: &[u8]) {
        // TODO: need to trim leading and trailing whitespace and sep_chars, and trailing commas. See module 388.
        while !bytes.is_empty() {
            let ch = bytes[0];

            match ch {
                b',' => self.consume_comma(),
                b'{' => self.consume_lbrace(&mut bytes),
                b'}' => self.consume_rbrace(),
                ch => {
                    use crate::input::LexClass::*;
                    match lex_class(ch) {
                        WhiteSpace | SepChar => self.consume_sep(ch),
                        _ => self.consume_other(ch),
                    }
                }
            }
            bytes = &bytes[1..];
        }
    }

    fn find_names(&mut self) {
        match self.num_commas {
            0 => {
                todo!()
            }
            1 => {
                // The name format is "von Last, First". There is no jr.
                self.first_pos = (self.comma1_idx, self.tokens.len());
                self.find_von_last(0, self.comma1_idx);
            }
            2 => {
                todo!()
            }
            // This should never be reached, because we cap the number of commas at 2
            // in `consume_comma`.
            _ => panic!("Unsupported number of commas")
        }
    }

    // The "von Last" part of the name occurs between tokens `start` (inclusive) and `end` (exclusive).
    // This figures out which part of that is "von" and which part is "Last", and initializes
    // `self.von_pos` and `self.last_pos` appropriately.
    fn find_von_last(&mut self, start: usize, end: usize) {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_basic() {
        let t = TokenizedName::from_bytes(b"first second");
        assert_eq!(t.bytes.len(), 11);
        assert_eq!(t.tokens, vec![0, 5]);
        assert_eq!(t.seps, vec![b' ']);
    }
}
