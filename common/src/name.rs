use crate::input::{lex_class, LexClass};

/// A struct for splitting a collection of names.
#[derive(Debug)]
pub struct SplitNames {
    bytes: Vec<u8>,

    // A list of indices into `bytes`. Each pair of indices points to the beginning and end of a name.
    names: Vec<(usize, usize)>,
}

impl SplitNames {
    pub fn from_bytes<B: AsRef<[u8]>>(bytes: B) -> SplitNames {
        let mut ret = SplitNames {
            bytes: bytes.as_ref().to_owned(),
            names: Vec::new(),
        };

        ret.find_names();
        ret
    }

    // Returns the index of the next "and".
    fn next_and(&self, start: usize) -> Option<usize> {
        let mut after_space = false;
        let mut i = start;
        while i < self.bytes.len() {
            let ch = self.bytes[i];
            if lex_class(ch) == LexClass::WhiteSpace {
                after_space = true;
                i += 1;
            } else {
                match ch {
                    b'a' | b'A' => {
                        if after_space
                                && self.bytes.len() > i + 3
                                && self.bytes[i + 1].to_ascii_lowercase() == b'n'
                                && self.bytes[i + 2].to_ascii_lowercase() == b'd'
                                && lex_class(self.bytes[i + 3]) == LexClass::WhiteSpace {
                            return Some(i);
                        } else {
                            i += 1;
                        }
                    },
                    b'{' => {
                        i += 1;
                        i += braced_string_end(&self.bytes[i..]).len();
                    }
                    _ => {
                        i += 1;
                    }
                }
                after_space = false;
            }
        }
        None
    }

    fn find_names(&mut self) {
        let mut start = 0;
        while let Some(and_idx) = self.next_and(start) {
            self.names.push((start, and_idx));
            start = and_idx + 3;
        }
        if start != self.bytes.len() {
            self.names.push((start, self.bytes.len()));
        }
    }

    /// The number of names that were found.
    pub fn num_names(&self) -> usize {
        self.names.len()
    }
}

/// Returns a slice containing the `idx`th name.
impl std::ops::Index<usize> for SplitNames {
    type Output = [u8];

    fn index(&self, idx: usize) -> &[u8] {
        let (start, end) = self.names[idx];
        &self.bytes[start..end]
    }
}

#[derive(Debug, Hash, PartialEq)]
pub enum NameWarning {
    BracesUnbalanced,
    TooManyCommas,
    TrailingComma,
}

#[derive(Debug)]
pub struct TokenizedName {
    // This vector of bytes includes the tokens and also exactly one "separator" byte between each
    // pair of tokens.
    bytes: Vec<u8>,

    // A vector of indices into bytes. Each index is the start of a token.
    tokens: Vec<usize>,

    // The number of commas in the name (zero, one, or two).
    num_commas: u8,

    // Positions of the commas, if present.
    comma1_idx: usize,
    comma2_idx: usize,

    // This is just used while parsing the name, to keep track of whether we're between tokens.
    token_starting: bool,

	// These are indices into `tokens`, giving the positions of the various parts of the name.
	// All of these are half-open intervals (i.e., not including the endpoint).
	//
	// An interval of length zero signifies that that part of the name isn't present.
    first_pos: (usize, usize),
    von_pos: (usize, usize),
    last_pos: (usize, usize),
    jr_pos: (usize, usize),

    // The collection of warnings that we encountered while parsing this name.
    warnings: Vec<NameWarning>,
}

#[derive(Debug, PartialEq)]
enum Case {
    Lower,
    Upper,
    Unknown,
}

impl TokenizedName {
    fn new() -> TokenizedName {
        TokenizedName {
            bytes: Vec::new(),
            tokens: Vec::new(),
            num_commas: 0,
            token_starting: true,
            comma1_idx: 0,
            comma2_idx: 0,
            first_pos: (0, 0),
            von_pos: (0, 0),
            last_pos: (0, 0),
            jr_pos: (0, 0),
            warnings: Vec::new(),
        }
    }

    pub fn from_bytes<B: AsRef<[u8]>>(bytes: B) -> TokenizedName {
        let mut ret = TokenizedName::new();
        let bytes = bytes.as_ref();

        ret.tokenize_from_bytes(bytes);
        ret.find_names();

        ret
    }

    // Returns the name that running from token tok_begin (inclusive) to token tok_end (exclusive).
    fn name_part(&self, tok_begin: usize, tok_end: usize) -> &[u8] {
        if tok_begin == tok_end {
            b""
        } else {
            let end = if tok_end == self.tokens.len() {
                self.bytes.len()
            } else {
                // Leave off the last byte of the last token, because it is a separator.
                self.tokens[tok_end] - 1
            };
            &self.bytes[self.tokens[tok_begin]..end]
        }
    }

    pub fn first(&self) -> &[u8] {
        self.name_part(self.first_pos.0, self.first_pos.1)
    }

    pub fn von(&self) -> &[u8] {
        self.name_part(self.von_pos.0, self.von_pos.1)
    }

    pub fn last(&self) -> &[u8] {
        self.name_part(self.last_pos.0, self.last_pos.1)
    }

    pub fn jr(&self) -> &[u8] {
        self.name_part(self.jr_pos.0, self.jr_pos.1)
    }

    // Strips leading and trailing whitespace and sepchars, and also trailing commas.
    // Corresponds to WEB module 388.
    fn trim_input<'a>(&mut self, mut buf: &'a [u8]) -> &'a [u8] {
        let white_sep = |c:u8| {
            lex_class(c) == LexClass::WhiteSpace || lex_class(c) == LexClass::SepChar
        };

        while let Some(&ch) = buf.first() {
            if white_sep(ch) {
                buf = &buf[1..];
            } else {
                break;
            }
        }

        while let Some(&ch) = buf.last() {
            if ch == b',' {
                self.warnings.push(NameWarning::TrailingComma);
            }
            if white_sep(ch) || ch == b',' {
                buf = &buf[..(buf.len() - 1)];
            } else {
                break;
            }
        }

        buf
    }

    fn token(&self, i: usize) -> &[u8] {
        let tok_start = self.tokens[i];
        let tok_end = self.tokens[i+1];
        &self.bytes[tok_start..tok_end]
    }

    fn consume_comma(&mut self) {
        if self.num_commas == 2 {
            self.warnings.push(NameWarning::TooManyCommas);
        } else {
            self.num_commas += 1;
            if self.num_commas == 1 {
                self.comma1_idx = self.tokens.len();
            } else {
                self.comma2_idx = self.tokens.len();
            }

            // A comma overrides the last separator byte, if there was one (if there was one, we add
            // a comma as a new separator char).
            if self.token_starting && !self.bytes.is_empty() {
                *self.bytes.last_mut().unwrap() = b',';
            } else if !self.token_starting {
                self.bytes.push(b',');
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
        *buf = &buf[1..];
        let braced_str = braced_string_end(buf);
        dbg!(braced_str);
        self.bytes.extend_from_slice(braced_str);
        *buf = &buf[(braced_str.len() - 1)..];
        dbg!(&buf);

        self.token_starting = false;
    }

    fn consume_rbrace(&mut self) {
        // If we get here, it means that the name contained an unbalanced right brace.
        if self.token_starting {
            self.tokens.push(self.bytes.len());
        }
        self.warnings.push(NameWarning::BracesUnbalanced);
    }

    fn consume_sep(&mut self, ch: u8) {
        if !self.token_starting {
            self.bytes.push(ch);
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
        bytes = self.trim_input(bytes);
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

    // This function finds and sets the variables `first_pos`, `von_pos`, `last_pos`, and `jr_pos`,
    // corresponding to the various parts of the name. We assume that all of these are initialized
    // to empty ranges, so if a part of the name is missing then we just don't need to initialize
    // the corresponding `xxx_pos` value.
    fn find_names(&mut self) {
        if self.tokens.is_empty() {
            return;
        }

        match self.num_commas {
            0 => {
                // The name format is "First von Last". There is no jr.

                // Look for the first lower-case token (not including the last token). If there is
                // one, it's the beginning of the "von" part.
                for i in 0..(self.tokens.len() - 1) {
                    if token_case(self.token(i)) == Case::Lower {
                        self.find_von_last(i, self.tokens.len());
                        self.first_pos = (0, i);
                        return;
                    }
                }

                // There is no "von" part. The "Last" part consists of however many trailing tokens
                // are joined by non-'~' sepchars.
                let nonsep = |c:u8| { lex_class(c) != LexClass::SepChar || c == b'~' };
                let last_nonsep_pos = self.tokens.iter()
                    // self.bytes[idx-1] is the separator before token number `idx`.
                    .rposition(|&idx| idx > 0 && nonsep(self.bytes[idx-1]))
                    // Even if all tokens are connected by separators, if there is more than one token then put
                    // the first token as the first name.
                    .unwrap_or(usize::min(1, self.tokens.len() - 1));

                self.first_pos = (0, last_nonsep_pos);
                self.last_pos = (last_nonsep_pos, self.tokens.len());
            }
            1 => {
                // The name format is "von Last, First". There is no jr.
                self.first_pos = (self.comma1_idx, self.tokens.len());
                self.find_von_last(0, self.comma1_idx);
            }
            2 => {
                // The name format is "von Last, jr, First".
                self.first_pos = (self.comma2_idx, self.tokens.len());
                self.jr_pos = (self.comma1_idx, self.comma2_idx);
                self.find_von_last(0, self.comma1_idx);
            }
            // This should never be reached, because we cap the number of commas at 2
            // in `consume_comma`.
            _ => panic!("Unsupported number of commas")
        }
    }

    // The "von Last" part of the name occurs between tokens `start` (inclusive) and `end`
    // (exclusive).  This figures out which part of that is "von" and which part is "Last", and
    // initializes `self.von_pos` and `self.last_pos` appropriately.
    fn find_von_last(&mut self, start: usize, end: usize) {
        for von_end in (start..(end-1)).rev() {
            if token_case(self.token(von_end)) == Case::Lower {
                self.von_pos = (start, von_end + 1);
                self.last_pos = (von_end + 1, end);
                return;
            }
        }
        self.last_pos = (start, end);
    }

}

// Finds the case of a token. The rules are a little complicated, and are contained in WEB module
// 397.  When there are no braces, the rule is simple: we look for the first ASCII letter, and
// return its case (or `Case::Unknown`) if there are no ASCII letters).
//
// When a braced string is encountered, there are two possibilities:
//  1) the braced string starts with a control sequence (e.g. {\'a}). In this case,
//    a) if the control sequence is one of a few special built-ins, we recognize its case in
//      `control_seq_case`;
//    b) otherwise, we skip the control sequence and get our case from the next ASCII letter in the
//      braced string.
//  2) if the braced string doesn't start with a control sequence, we ignore it.
fn token_case(mut buf: &[u8]) -> Case {
    while let Some((ch, rest)) = buf.split_first() {
        buf = rest;

        match ch {
            b'a'..=b'z' => return Case::Lower,
            b'A'..=b'Z' => return Case::Upper,
            b'{' => {
                let braced = braced_string_end(buf);
                if braced.len() >= 2 && buf[0] == b'\\' {
                    let cseq = control_seq(braced);
                    let mut case = control_seq_case(cseq);
                    if case == Case::Unknown {
                        case = braced_string_case(&braced[cseq.len()..]);
                    }
                    if case != Case::Unknown {
                        return case;
                    }
                }
                buf = &buf[braced.len()..];
            }
            _ => {},
        }
    }
    Case::Unknown
}

// Look at the balanced braced string at the beginning of `buf` (which does not include the opening
// '{'). If it has a letter in it, return that letter's case.
fn braced_string_case(buf: &[u8]) -> Case {
    let braced = braced_string_end(buf);
    for &b in braced {
        match b {
            b'a'..=b'z' => return Case::Lower,
            b'A'..=b'Z' => return Case::Upper,
            _ => {},
        }
    }
    Case::Unknown
}

// Finds a (balanced) braced string at the beginning of `buf`, which does not include the
// opening '{'. (If we run out of `buf` while searching for a closing brace, we just return the
// whole thing.)
fn braced_string_end(buf: &[u8]) -> &[u8] {
    let mut depth = 1;
    for (idx, &c) in buf.iter().enumerate() {
        if c == b'{' {
            depth += 1;
        } else if c == b'}' {
            depth -= 1;
        }
        if depth == 0 {
            return &buf[..=idx];
        }
    }
    buf
}

// The buffer starts with a backslash, and we return the control sequence: the backslash, plus the
// longest sequence of alphabetic characters following it.
fn control_seq(buf: &[u8]) -> &[u8] {
    let end = buf.iter()
        .skip(1)
        .position(|&c| lex_class(c) != LexClass::Alpha)
        .map(|x| x + 1)
        .unwrap_or(buf.len());
    &buf[..end]
}

// `buf` contains a control sequence, and we're checking its case. These rules are hard-coded
// into bibtex.
fn control_seq_case(buf: &[u8]) -> Case {
    match &buf[1..] {
        b"OE" | b"AE" | b"AA" | b"O" | b"L" => Case::Upper,
        b"i" | b"j" | b"oe" | b"ae" | b"aa" | b"o" | b"l" | b"ss" => Case::Lower,
        _ => Case::Unknown,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_basic() {
        let t = TokenizedName::from_bytes(b"first second");
        assert_eq!(t.bytes.len(), 12);
        assert_eq!(t.tokens, vec![0, 6]);

        let t = TokenizedName::from_bytes(b"first~second");
        assert_eq!(t.bytes.len(), 12);
        assert_eq!(t.tokens, vec![0, 6]);
    }

    #[test]
    fn test_braced_string_end() {
        assert_eq!(braced_string_end(b"foo} bar"), b"foo}");
        assert_eq!(braced_string_end(b"foo{nes{}ted}} bar"), b"foo{nes{}ted}}");
        assert_eq!(braced_string_end(b"foo{unterminated} bar"), b"foo{unterminated} bar");
    }

    #[test]
    fn test_token_case() {
        use Case::*;

        assert_eq!(token_case(b"Apple"), Upper);
        assert_eq!(token_case(b"apple"), Lower);
        assert_eq!(token_case(b"{A}pple"), Lower);
        assert_eq!(token_case(b"{\\A}pple"), Lower);
        assert_eq!(token_case(b"{\\AA}pple"), Upper);
        assert_eq!(token_case(b"{\\aa}pple"), Lower);
    }

    macro_rules! check_name_split {
        ($($input:expr => ($first:expr, $von:expr, $last:expr, $jr:expr)),* $(,)?) => {
            $(
            {
                let t = TokenizedName::from_bytes($input.as_bytes());
                dbg!(&t);
                assert_eq!((t.first(), t.von(), t.last(), t.jr()), ($first.as_bytes(), $von.as_bytes(), $last.as_bytes(), $jr.as_bytes()));
            }
            )*
        }
    }

    #[test]
    fn names() {
        check_name_split!(
            // These are the test cases from http://maverick.inria.fr/~Xavier.Decoret/resources/xdkbibtex/bibtex_summary.html
            "AA BB" => ("AA", "", "BB", ""),
            "AA" => ("", "", "AA", ""),
            "AA bb" => ("AA", "", "bb", ""),
            "aa" => ("", "", "aa", ""),
            "AA bb CC" => ("AA", "bb", "CC", ""),
            "AA bb CC dd EE" => ("AA", "bb CC dd", "EE", ""),
            "AA 1B cc dd" => ("AA 1B", "cc", "dd", ""),
            "AA 1b cc dd" => ("AA", "1b cc", "dd", ""),
            "AA {b}B cc dd" => ("AA {b}B", "cc", "dd", ""),
            "AA {b}b cc dd" => ("AA", "{b}b cc", "dd", ""),
            "AA {B}B cc dd" => ("AA {B}B", "cc", "dd", ""),
            "AA {B}b cc dd" => ("AA", "{B}b cc", "dd", ""),
            "AA \\BB{b} cc dd" => ("AA \\BB{b}", "cc", "dd", ""),
            "AA \\bb{b} cc dd" => ("AA", "\\bb{b} cc", "dd", ""),
            "AA {bb} cc DD" => ("AA {bb}", "cc", "DD", ""),
            "AA bb {cc} DD" => ("AA", "bb", "{cc} DD", ""),
            "AA {bb} CC" => ("AA {bb}", "", "CC", ""),
            "bb CC, AA" => ("AA", "bb", "CC", ""),
            "bb CC, aa" => ("aa", "bb", "CC", ""),
            "bb CC dd EE, AA" => ("AA", "bb CC dd", "EE", ""),
            "bb, AA" => ("AA", "", "bb", ""),
            "BB," => ("", "", "BB", ""),
            "bb CC, XX, AA" => ("AA", "bb", "CC", "XX"),
            "bb CC, xx, AA" => ("AA", "bb", "CC", "xx"),

            // Hyphens affect the split between first and last names. Ties don't.
            "AA BB CC" => ("AA BB", "", "CC", ""),
            "AA BB-CC" => ("AA", "", "BB-CC", ""),
            "AA-BB-CC" => ("AA", "", "BB-CC", ""),
            "AA BB~CC" => ("AA BB", "", "CC", ""),
        );
    }

    macro_rules! check_and_split {
        ($input:expr => $($name:expr),*) => {
            {
                let t = SplitNames::from_bytes($input.as_bytes());
                dbg!(&t);
                let expected: Vec<&[u8]> = vec![$($name.as_bytes()),*];
                let actual: Vec<&[u8]> = (0..t.num_names()).map(|i| &t[i]).collect();
                assert_eq!(actual, expected);
            }
        }
    }

    #[test]
    fn and_names() {
        check_and_split!("AA BB and CC DD" => "AA BB ", " CC DD");
        check_and_split!("AA Andy BB and CC DD" => "AA Andy BB ", " CC DD");
        check_and_split!("AA Sand BB and CC DD" => "AA Sand BB ", " CC DD");
        check_and_split!("AA {and} BB and CC DD" => "AA {and} BB ", " CC DD");
        check_and_split!("" => );
        check_and_split!("?" => "?");
    }
}
