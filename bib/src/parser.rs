use common::Input;
use common::input::is_white;
use std::collections::HashMap;

use crate::Entry;
use crate::error::{Error, ErrorContext, ErrorKind, IdentifierKind};

/// A parser for `.bib` files.
pub struct Parser<'read> {
    input: Input<'read>,

    // BibTex does space compression on macros at substitution time. We do it at declaration time,
    // so the values stored here have already had their spaces compressed.
    macros: HashMap<Vec<u8>, Vec<u8>>,

    // Recursive macros are not allowed. In order to output an informative error message, when we
    // are in the middle of parsing a macro we store its name here.
    cur_macro: Option<Vec<u8>>,
}

impl<'read> Parser<'read> {
    /// Creates a new [`Parser`] that's ready to parse the input given in `read`.
    pub fn new<R: std::io::BufRead + 'read>(read: R) -> Parser<'read> {
        Parser {
            input: Input::from_reader(read),
            macros: HashMap::new(),
            cur_macro: None,
        }
    }

    /// Returns an iterator over all the entries (and errors) in this `.bib` file.
    pub fn entries(self) -> EntriesIter<'read> {
        EntriesIter {
            parser: self,
            next_entry: None,
        }
    }

    // If the current character is the expected one, consumes it. Otherwise, returns an error.
    fn expect(&mut self, ch: u8, e: ErrorKind) -> Result<(), ErrorKind> {
        if self.input.current() != ch {
            Err(e)
        } else {
            self.input.advance();
            Ok(())
        }
    }

    // If the current character is one of the expected ones, consumes it. Otherwise, returns an
    // error.
    fn expect_one_of_two(&mut self, a: u8, b: u8) -> Result<(), ErrorKind> {
        let ch = self.input.current();
        if ch != a && ch != b {
            Err(ErrorKind::ExpectedEither(a, b))
        } else {
            self.input.advance();
            Ok(())
        }
    }

    fn scan_identifier<F: FnMut(u8) -> bool>(&mut self, mut next_char: F, kind: IdentifierKind) -> Result<Vec<u8>, ErrorKind> {
        let id = self.input.scan_identifier();
        if id.is_empty() {
            Err(ErrorKind::EmptyId(kind))
        } else if !self.input.is_eol() && !is_white(self.input.current()) && !next_char(self.input.current()) {
            Err(ErrorKind::InvalidIdChar(kind))
        } else {
            self.input.mark_last_lowercase(id.len());
            Ok(id)
        }
    }

    // Consumes and returns the next byte from the input, while ensuring that consecutive
    // whitespace characters are replaced by a single space.
    fn next_char_compressing_spaces(&mut self) -> Result<u8, ErrorKind> {
        if self.input.is_eol() || is_white(self.input.current()) {
            self.skip_white_space()?;
            Ok(b' ')
        } else {
            let ret = self.input.current();
            self.input.advance();
            Ok(ret)
        }
    }

    // Assuming that we have just consumed some opening delimiter, parses a balanced-brace string
    // up until the closing delimiter `end`. The string (but not including the closing delimiter)
    // is returned. Any consecutive whitespace characters in the string are replaced by a single
    // space.
    //
    // The implementation in BibTex starts at WEB section number 253. That implementation differs
    // from this one in two main respects: it uses a nested loop in order to avoid having to test
    // the ending delimiter during the nested loops, and it has an optimization for the case that
    // we only want to advance past and discard the string.
    //
    // Not counting I/O errors, there are two possible errors that can happen: if the input ends
    // before we see the closing delimiter, we will return `UnexpectedEOF`. If at any point we see
    // more right braces than left braces, we will return `UnbalancedBraces`.
    fn parse_balanced_brace_string(&mut self, end: u8) -> Result<Vec<u8>, ErrorKind> {
        let mut brace_level = 0;
        let mut ret = vec![];
        loop {
            match self.next_char_compressing_spaces()? {
                ch if ch == end && brace_level == 0 => {
                    return Ok(ret);
                }
                b'{' => {
                    brace_level += 1;
                    ret.push(b'{');
                }
                b'}' => {
                    if brace_level == 0 {
                        // Since we just read a '}', we can't be at the start of the line. It's
                        // safe to back up the input by one byte. This matches BibTex's behavior,
                        // which doesn't consume the unbalanced brace.
                        self.input.unadvance();
                        return Err(ErrorKind::UnbalancedBraces);
                    } else {
                        brace_level -= 1;
                        ret.push(b'}');
                    }
                }
                ch => ret.push(ch),
            }
        }
    }

    // Starting from the current token (which is assumed not to be whitespace), parses a file
    // token.
    //
    // A field token is either:
    // - a non-negative number (i.e. a string of digits),
    // - a macro name, in which case we return the expanded value of the macro, or
    // - a brace-balanced string delimited by either {} or "".
    //
    // This is specified starting at WEB section number 250 of the BibTex source.
    fn parse_field_token(&mut self, right_delim: u8) -> Result<Vec<u8>, ErrorKind> {
        match self.input.current() {
            b'{' => {
                self.input.advance();
                self.parse_balanced_brace_string(b'}')
            }
            b'"' => {
                self.input.advance();
                self.parse_balanced_brace_string(b'"')
            }
            b'0'..=b'9' => {
                Ok(self.input.scan_nonnegative_number())
            }
            _ => {
                let id = self.scan_identifier(|c| c == b',' || c == b'#' || c == right_delim, IdentifierKind::FieldPart)?;
                if Some(&id) == self.cur_macro.as_ref() {
                    // TODO: we need to emit an error, but without aborting the current function.
                    // What is the BibTex behavior in this case?
                    unimplemented!()
                }
                if let Some(val) = self.macros.get(&id) {
                    Ok(val.to_owned())
                } else {
                    // The macro hasn't been defined yet, but we don't error out (because BibTex
                    // doesn't). Just issue a warning and substitute the empty string.
                    // TODO: issue a warning
                    Ok(Vec::new())
                }
            }
        }
    }

    // Parses a field value, which can consist of multiple field tokens separated by #. These field
    // tokens will be concatenated to form the final value. Multiple whitespace characters will be
    // compressed into a single space.
    //
    // Field values appear in the @preamble command, the right hand side of a @string command, and
    // as the field values in an entry.
    fn parse_field_value(&mut self, right_delim: u8) -> Result<Vec<u8>, ErrorKind> {
        let mut ret = self.parse_field_token(right_delim)?;
        self.skip_white_space()?;
        while self.input.current() == b'#' {
            self.input.advance();
            self.skip_white_space()?;
            let next = self.parse_field_token(right_delim)?;
            ret.extend_from_slice(&next[..]);
        }
        Ok(ret)
    }

    // Consumes an opening delimiter (either '{' or '(') and returns the corresponding closing
    // delimiter.
    fn parse_open_delim(&mut self) -> Result<u8, ErrorKind> {
        let closing_delim = match self.input.current() {
            b'{' => b'}',
            b'(' => b')',
            _ => return Err(ErrorKind::ExpectedEither(b'{', b'(')),
        };
        self.input.advance();
        Ok(closing_delim)
    }

    fn parse_preamble(&mut self) -> Result<Item, ErrorKind> {
        // A preamble string is allowed to be enclosed in either {} or ().
        self.skip_white_space()?;
        let closing_delim = self.parse_open_delim()?;
        self.skip_white_space()?;
        let ret = self.parse_field_value(closing_delim)?;
        self.skip_white_space()?;
        self.expect(closing_delim, ErrorKind::UnterminatedPreamble(closing_delim))?;
        Ok(Item::Preamble(ret))
    }

    // Note that this routine not only parses a @string command, it stores the resulting macro
    // substition in `self.macros`.
    fn parse_string(&mut self) -> Result<Item, ErrorKind> {
        self.skip_white_space()?;
        let closing_delim = self.parse_open_delim()?;
        self.skip_white_space()?;
        let key = self.scan_identifier(|c| c == b'=', IdentifierKind::StringName)?;
        self.skip_white_space()?;
        self.expect(b'=', ErrorKind::ExpectedEquals)?;
        self.skip_white_space()?;
        let val = self.parse_field_value(closing_delim)?;
        self.skip_white_space()?;
        self.expect(closing_delim, ErrorKind::UnterminatedString(closing_delim))?;

        // TODO: do the bit about checking if it's already defined, or recursively defined, etc.
        self.macros.insert(key.clone(), val.clone());

        Ok(Item::String { _key: key, _val: val })
    }

    fn parse_comment(&mut self) -> Item {
        // There's no need to actually consume anything, because of the way `parse_item` skips to
        // the next '@' when looking for the next entry.
        Item::Comment
    }

    // Parses a single key-value pair, and adds it to the entry. We expect the current input
    // character to be a comma (the one coming after the previous key-value pair).
    fn parse_entry_field(&mut self, entry: &mut Entry, closing_delim: u8) -> Result<(), ErrorKind> {
        // Even though we expect a comma, let's pretend a closing delimiter would be ok too. This
        // is to match BibTex's error messages.
        self.expect_one_of_two(b',', closing_delim)?;
        self.skip_white_space()?;

        if self.input.current() == closing_delim {
            // The last field had a trailing comma. This isn't an error; we just don't add a new
            // key-value pair.
            return Ok(());
        }

        // TODO: BibTex checks whether the field name is a valid one before storing it. That
        // isn't really possible in a standalone .bib parser (because we have to parse the .bst
        // file to find the supported field names), but it might be worth supporting some sort
        // of field-filtering functionality.
        let field_name = self.scan_identifier(|c| c == b'=', IdentifierKind::FieldName)?;
        self.skip_white_space()?;
        self.expect(b'=', ErrorKind::ExpectedEquals)?;
        self.skip_white_space()?;
        let field_value = self.parse_field_value(closing_delim)?;

        // BibTex silently overwrites duplicate fields; see WEB section 245.
        entry.fields.insert(field_name, field_value);
        Ok(())
    }

    /// Parses an entry.
    ///
    /// If an error was encountered in the middle of reading an entry, we might return a partial
    /// entry along with the error.
    fn parse_entry(&mut self, kind: Vec<u8>) -> Result<Item, (Option<Item>, ErrorKind)> {
        self.skip_white_space().map_err(|e| (None, e))?;
        let closing_delim = self.parse_open_delim().map_err(|e| (None, e))?;
        self.skip_white_space().map_err(|e| (None, e))?;
        // The allowed characters in the key of a bibtex entry depend on the delimiter. When
        // using (), the key is even allowed to contain ')'.
        let key = if closing_delim == b')' {
            self.input.scan_while(|b| !is_white(b) && b != b',')
        } else {
            self.input.scan_while(|b| !is_white(b) && b != b',' && b != b'}')
        };

        // TODO: BibTex maintains a list of entries that were cited in the .aux file. While
        // parsing, it skips over unneeded entries. We should also have some way of doing that.

        // TODO: WEB sections 268 and 269 mention crossrefs, which I don't understand yet.

        let mut ret = Entry {
            kind,
            key,
            fields: HashMap::new(),
        };

        // We can't use map_err here and below, because borrowck would require us to clone `ret`.
        if let Err(e) = self.skip_white_space() {
            return Err((Some(Item::Entry(ret)), e))
        }
        while self.input.current() != closing_delim {
            if let Err(e) = self.parse_entry_field(&mut ret, closing_delim) {
                return Err((Some(Item::Entry(ret)), e));
            }
        }

        // Advance past the closing delimiter.
        self.input.advance();

        Ok(Item::Entry(ret))
    }

    // Having just consumed a '@', parse the rest of the item.
    fn try_parse_item(&mut self) -> Result<Item, (Option<Item>, Error)> {
        // BibTex treats an error here as occurring inside an entry, even though we haven't really
        // determined whether it's an entry or a command.
        self.skip_white_space().map_err(|kind| (None, Error::with_context(kind, ErrorContext::Entry, &self.input)))?;
        let kind = self
            .scan_identifier(|c| c == b'(' || c == b'{', IdentifierKind::EntryKind)
            // BibTex treats a missing entry type as being an error in an entry.
            .map_err(|k| (None, Error::with_context(k, ErrorContext::Entry, &self.input)))?;

        match &kind[..] {
            // TODO: check whether it's possible for bibtex to return partial preambles and strings
            // on error. Probably it is.
            b"preamble" => self.parse_preamble().map_err(|k| (None, Error::with_context(k, ErrorContext::Command, &self.input))),
            b"string" => self.parse_string().map_err(|k| (None, Error::with_context(k, ErrorContext::Command, &self.input))),
            b"comment" => Ok(self.parse_comment()),
            _ => self.parse_entry(kind).map_err(|(e, k)| (e, Error::with_context(k, ErrorContext::Entry, &self.input))),
        }
    }

    // TODO: fix the docs
    // Finds and parses the next item, returning `None` if there are no more items. Any errors that
    // are encountered will be reported using `self.error`, but then we will simply continue
    // looking for the next item.
    //
    // The beginning of an item is signified by an '@' character, and so we will silently consume
    // everything up to the next '@'.
    fn parse_item(&mut self) -> (Option<Item>, Option<Error>) {
        // This is for matching BibTex's behavior: it refuses to start parsing a new item if it
        // is on the last line of a file, and that line is not newline-terminated. This
        // behavior can be seen in WEB section 223, where it checks if the file has not hit EOF
        // before trying to start parsing the next entry.
        if self.input.no_more_newlines() {
            return (None, None);
        }

        while !self.input.skip_to(|c| c == b'@') {
            if let Err(e) = self.input.input_line() {
                return (None, Some(Error::new(e.into(), &self.input)));
            } else if self.input.is_eof() {
                return (None, None);
            }
        }

        debug_assert!(self.input.current() == b'@');
        self.input.advance();

        match self.try_parse_item() {
            Ok(it) => (Some(it), None),
            Err((maybe_item, err)) => (maybe_item, Some(err)),
        }
    }

    /// Skips over white space, returning an error if EOF is reached before a non-whitespace
    /// character.
    fn skip_white_space(&mut self) -> Result<(), ErrorKind> {
        loop {
            if self.input.skip_white_space() {
                return Ok(());
            }

            if !self.input.input_line()? {
                return Err(ErrorKind::UnexpectedEOF);
            }
        }
    }
}

/// An iterator over bibtex entries.
pub struct EntriesIter<'read> {
    parser: Parser<'read>,
    // The parser tends to produce errors at the same time as it produces partially valid entries. In
    // order to provide a more friendly iterator interface, we report the error first, and then the
    // entry after. This means we need to store the entry until the next call to `next()`.
    next_entry: Option<Entry>,
}

impl<'read> Iterator for EntriesIter<'read> {
    type Item = Result<Entry, Error>;

    fn next(&mut self) -> Option<Result<Entry, Error>> {
        if let Some(entry) = self.next_entry.take() {
            return Some(Ok(entry))
        }
        loop {
            match self.parser.parse_item() {
                (Some(Item::Entry(e)), None) => return Some(Ok(e)),
                (Some(Item::Entry(e)), Some(err)) => {
                    self.next_entry = Some(e);
                    return Some(Err(err));
                }
                (_, Some(err)) => return Some(Err(err)),
                (None, None) => return None,
                // If we get an item that isn't an entry, ignore it.
                (_, None) => continue,
            }
        }
    }
}

enum Item {
    Preamble(Vec<u8>),
    // These fields are currently unused, because we don't export anything except for entries.
    String { _key: Vec<u8>, _val: Vec<u8> },
    Entry(Entry),
    Comment,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parser(s: &'static [u8]) -> Parser<'static> {
        Parser::new(s)
    }

    #[test]
    fn parse_balanced_brace_string() {
        fn expect(input: &'static [u8], output: &[u8]) {
            let mut p = parser(input);
            p.skip_white_space().unwrap();
            // Even though we're testing balanced braces, it's more convenient to call
            // parse_field_token, because parse_balanced_brace_string requires the opening
            // delimiter to already be consumed.
            let out = p.parse_field_token(b'}').unwrap();
            assert_eq!(output, &out[..]);
        }

        fn expect_error(input: &'static [u8], err: ErrorKind) {
            let mut p = parser(input);
            p.skip_white_space().unwrap();
            assert_eq!(p.parse_field_token(b'}').unwrap_err(), err);
        }

        expect(b"{blah}after", b"blah");
        expect(b"{{blah}}after", b"{blah}");
        expect(b"{bl{ah}}after", b"bl{ah}");
        expect(b"{bl\"ah}after", b"bl\"ah");
        expect(b"\"blah\"after", b"blah");
        expect(b"\"{blah}\"after", b"{blah}");
        expect(b"\"bl{ah}\"after", b"bl{ah}");
        expect(b"\"bl\"ah\"after", b"bl");

        expect_error(b"{blah", ErrorKind::UnexpectedEOF);
        expect_error(b"\"blah}\"", ErrorKind::UnbalancedBraces);
    }
}

