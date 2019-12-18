use common::Input;
use common::input::is_white;
use std::collections::{HashMap, HashSet};

use crate::Entry;
use crate::citation_list::{CitationList, CrossrefList};
use crate::error::{ErrorContext, ErrorKind, IdentifierKind, Problem, ProblemReporter, WarningKind};

trait ResultExt<T> {
    fn or_bail(self, p: &mut Parser, ctxt: ErrorContext) -> Option<T>;
}

impl<T> ResultExt<T> for Result<T, ErrorKind> {
    fn or_bail(self, p: &mut Parser, ctxt: ErrorContext) -> Option<T> {
        match self {
            Ok(t) => Some(t),
            Err(e) => {
                p.report.report(&Problem::from_error_with_context(e, ctxt, &p.input));
                None
            }
        }
    }
}

/// A parser for `.bib` files.
pub struct Parser<'read, 'error> {
    input: Input<'read>,

    report: Box<dyn ProblemReporter + 'error>,

    // BibTex does space compression on macros at substitution time. We do it at declaration time,
    // so the values stored here have already had their spaces compressed.
    macros: HashMap<Vec<u8>, Vec<u8>>,

    entry_type_checker: Option<Box<dyn FnMut(&[u8]) -> bool>>,
    field_name_checker: Option<Box<dyn FnMut(&[u8]) -> bool>>,

    citation_list: CitationList,
    // Was the citation list explicitly initialized? If not, we default to allowing all citations.
    citation_list_initialized: bool,
    crossref_list: CrossrefList,

    // Entries can contain a `crossref` field, like
    // @article{article_key, title="Blah", crossref=book_key}
    // @book{book_key, editor="Me"}
    //
    // While processing the bibtex file, we keep track of all crossreferenced entries (i.e.
    // book_key in the example above). Those that are referenced more than `min_crossrefs` will
    // appear in the bibliography even if they were not otherwise included.
    min_crossrefs: usize,

    // We need to keep track of which entries we've already seen (in order to detect missing and
    // repeated entries). It may be better to fold this in with `CitationList` somehow, but for now
    // here it is. All of these entries are stored in ASCII lower case.
    seen_entries: HashSet<Vec<u8>>,
}

impl<'read, 'error> Parser<'read, 'error> {
    /// Creates a new [`Parser`] that's ready to parse the input given in `read`.
    pub fn new<R: std::io::BufRead + 'read, E: ProblemReporter + 'error>(read: R, report: E) -> Parser<'read, 'error> {
        Parser {
            input: Input::from_reader(read),
            report: Box::new(report),
            macros: HashMap::new(),
            entry_type_checker: None,
            field_name_checker: None,
            citation_list: Default::default(),
            citation_list_initialized: false,
            crossref_list: Default::default(),
            min_crossrefs: 0,
            seen_entries: HashSet::new(),
        }
    }

    /// Provides a function for checking whether the entries are of a known type.
    ///
    /// Whenever the provided function returns false, a warning will be issued.
    pub fn with_entry_type_checker<F: FnMut(&[u8]) -> bool + 'static>(mut self, f: F) -> Self {
        self.entry_type_checker = Some(Box::new(f));
        self
    }

    /// Provides a function for checking whether the field names are known.
    ///
    /// Whenever the provided function returns false, a warning will be issued.
    pub fn with_field_name_checker<F: FnMut(&[u8]) -> bool + 'static>(mut self, f: F) -> Self {
        self.field_name_checker = Some(Box::new(f));
        self
    }

    /// Provides this parser with a list of all interesting citation keys.
    ///
    /// When iterating over entries, the parser will ignore all entries belonging to this list,
    /// unless they were crossreferenced at least `min_crossrefs` times by something in this list.
    pub fn with_citation_list<I: AsRef<[u8]>, T: IntoIterator<Item=I>>(mut self, list: T, min_crossrefs: usize) -> Self {
        self.citation_list.initialize_with(list.into_iter().map(|xs| xs.as_ref().to_vec()).collect::<Vec<_>>());
        self.citation_list_initialized = true;
        self.min_crossrefs = min_crossrefs;
        self
    }

    /// Returns an iterator over all the entries in this `.bib` file.
    pub fn entries<'parser>(&'parser mut self) -> EntriesIter<'parser, 'read, 'error> {
        if !self.citation_list_initialized {
            self.citation_list_initialized = true;
            self.citation_list.initialize_with(vec![vec![b'*']]);
        }
        EntriesIter {
            parser: self,
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
    //
    // For more on `cur_macro` and `ignore_field`, see [`parse_field_value`].
    fn parse_field_token(&mut self, right_delim: u8, cur_macro: Option<&[u8]>, ignore_field: bool) -> Result<Vec<u8>, ErrorKind> {
        match self.input.current() {
            b'{' => {
                self.input.advance();
                // TODO: as an optimization, we could avoid allocating memory here (and below) when
                // ignore_field is true.
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
                if ignore_field {
                    Ok(Vec::new())
                } else if Some(&id[..]) == cur_macro {
                    // Someone attempted a recursive macro definition. Issue a warning and
                    // substitute the empty string.
                    self.report.report(&Problem::from_warning(WarningKind::RecursiveString(id), &self.input));
                    Ok(Vec::new())
                } else if let Some(val) = self.macros.get(&id) {
                    Ok(val.to_owned())
                } else {
                    // The macro hasn't been defined yet, but we don't error out (because BibTex
                    // doesn't). Just issue a warning and substitute the empty string.
                    self.report.report(&Problem::from_warning(WarningKind::UndefinedString(id), &self.input));
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
    //
    // If the `cur_macro` parameter is present, it means that we are currently defining that macro,
    // and that therefore this macro is not allowed to appear in the field value.
    //
    // If `ignore_field` is true, it means that we are parsing the field value for a field type
    // that we don't know about. This means that we need to parse it correctly, but we don't need
    // to return a reasonable value (so that, for example, we don't need to expand macros or
    // allocate memory).
    fn parse_field_value(&mut self, right_delim: u8, cur_macro: Option<&[u8]>, ignore_field: bool) -> Result<Vec<u8>, ErrorKind> {
        let mut ret = self.parse_field_token(right_delim, cur_macro, ignore_field)?;
        self.skip_white_space()?;
        while self.input.current() == b'#' {
            self.input.advance();
            self.skip_white_space()?;
            let next = self.parse_field_token(right_delim, cur_macro, ignore_field)?;
            ret.extend_from_slice(&next[..]);
            self.skip_white_space()?;
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
        let ret = self.parse_field_value(closing_delim, None, false)?;
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
        let val = self.parse_field_value(closing_delim, Some(&key), false)?;
        self.skip_white_space()?;
        self.expect(closing_delim, ErrorKind::UnterminatedString(closing_delim))?;

        // We don't need to check if the macro was already defined. BibTex just overwrites the
        // value silently.
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
    //
    // If `ignore_it` is true, we parse the field don't actually store it in the entry.
    fn parse_entry_field(&mut self, entry: &mut Entry, closing_delim: u8, ignore_it: bool) -> Result<(), ErrorKind> {
        // Even though we expect a comma, let's pretend a closing delimiter would be ok too. This
        // is to match BibTex's error messages.
        self.expect_one_of_two(b',', closing_delim)?;
        self.skip_white_space()?;

        if self.input.current() == closing_delim {
            // The last field had a trailing comma. This isn't an error; we just don't add a new
            // key-value pair.
            return Ok(());
        }

        let field_name = self.scan_identifier(|c| c == b'=', IdentifierKind::FieldName)?;
        let ignore_field = ignore_it || self.field_name_checker.as_mut().map(|f| f(&field_name)) == Some(false);

        self.skip_white_space()?;
        self.expect(b'=', ErrorKind::ExpectedEquals)?;
        self.skip_white_space()?;
        let field_value = self.parse_field_value(closing_delim, None, ignore_field)?;

        // BibTex silently overwrites duplicate fields; see WEB section 245.
        if !ignore_field {
            entry.fields.insert(field_name, field_value);
        }
        Ok(())
    }

    /// Parses an entry, reporting errors if there are any.
    ///
    /// Note that if an error occurs in the middle of an entry, we return whatever part of the
    /// entry that we successfully parsed.
    ///
    /// If the entry key is not on the citation list, we will return `None` even if there are no
    /// errors.
    fn parse_entry(&mut self, kind: Vec<u8>) -> Option<Item> {
        self.skip_white_space().or_bail(self, ErrorContext::Entry)?;
        let closing_delim = self.parse_open_delim().or_bail(self, ErrorContext::Entry)?;
        self.skip_white_space().or_bail(self, ErrorContext::Entry)?;

        // The allowed characters in the key of a bibtex entry depend on the delimiter. When
        // using (), the key is even allowed to contain ')'.
        let key = if closing_delim == b')' {
            self.input.scan_while(|b| !is_white(b) && b != b',')
        } else {
            self.input.scan_while(|b| !is_white(b) && b != b',' && b != b'}')
        };

        if let Some(ref mut check) = self.entry_type_checker {
            if !check(&kind) {
                self.report.report(&Problem::from_warning(WarningKind::UnknownEntryType(key.clone()), &self.input));
            }
        }

        let lc_key = key.to_ascii_lowercase();
        if self.seen_entries.contains(&lc_key) {
            self.report.report(&Problem::from_error_with_context(ErrorKind::RepeatedEntry, ErrorContext::Entry, &self.input));
            return None;
        }
        self.seen_entries.insert(lc_key);

        let mut ret = Entry {
            kind,
            key,
            fields: HashMap::new(),
        };

        if self.skip_white_space().or_bail(self, ErrorContext::Entry).is_none() {
            return Some(Item::Entry(ret));
        }
        let ignore_it = !self.citation_list.contains(&ret.key);
        while self.input.current() != closing_delim {
            if self.parse_entry_field(&mut ret, closing_delim, ignore_it)
                    .or_bail(self, ErrorContext::Entry)
                    .is_none() {
                return if ignore_it {
                    None
                } else {
                    Some(Item::Entry(ret))
                };
            }
        }

        // Advance past the closing delimiter.
        self.input.advance();

        if ignore_it {
            None
        } else {
            Some(Item::Entry(ret))
        }
    }

    // Having just consumed a '@', parse the rest of the item.
    fn try_parse_item(&mut self) -> Option<Item> {
        // BibTex treats an error here as occurring inside an entry, even though we haven't really
        // determined whether it's an entry or a command.
        self.skip_white_space().or_bail(self, ErrorContext::Entry)?;
        let kind = self.scan_identifier(|c| c == b'(' || c == b'{', IdentifierKind::EntryKind)
            .or_bail(self, ErrorContext::Entry)?;

        match &kind[..] {
            // TODO: check whether it's possible for bibtex to return partial preambles and strings
            // on error. Probably it is.
            b"preamble" => self.parse_preamble().or_bail(self, ErrorContext::Command),
            b"string" => self.parse_string().or_bail(self, ErrorContext::Command),
            b"comment" => Some(self.parse_comment()),
            _ => self.parse_entry(kind),
        }
    }

    // Finds and parses the next item, returning `None` if there are no more items. Any errors that
    // are encountered will be reported using `self.error`, but then we will simply continue
    // looking for the next item.
    //
    // The beginning of an item is signified by an '@' character, and so we will silently consume
    // everything up to the next '@'.
    fn parse_item(&mut self) -> Option<Item> {
        loop {
            // This is for matching BibTex's behavior: it refuses to start parsing a new item if it
            // is on the last line of a file, and that line is not newline-terminated. This
            // behavior can be seen in WEB section 223, where it checks if the file has not hit EOF
            // before trying to start parsing the next entry.
            if self.input.no_more_newlines() {
                return None;
            }

            while !self.input.skip_to(|c| c == b'@') {
                if let Err(e) = self.input.input_line() {
                    self.report.report(&Problem::from_error(e.into(), &self.input));
                    continue;
                } else if self.input.is_eof() {
                    return None;
                }
            }

            debug_assert!(self.input.current() == b'@');
            self.input.advance();

            match self.try_parse_item() {
                Some(it) => return Some(it),
                None => {},
            }
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
pub struct EntriesIter<'parser, 'read, 'error> {
    parser: &'parser mut Parser<'read, 'error>,
}

impl<'parser, 'read, 'error> Iterator for EntriesIter<'parser, 'read, 'error> {
    type Item = Entry;

    fn next(&mut self) -> Option<Entry> {
        while let Some(item) = self.parser.parse_item() {
            if let Item::Entry(e) = item {
                return Some(e)
            }
        }
        None
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

    fn parser(s: &'static [u8]) -> Parser<'static, 'static> {
        Parser::new(s, ())
    }

    #[test]
    fn parse_balanced_brace_string() {
        fn expect(input: &'static [u8], output: &[u8]) {
            let mut p = parser(input);
            p.skip_white_space().unwrap();
            // Even though we're testing balanced braces, it's more convenient to call
            // parse_field_token, because parse_balanced_brace_string requires the opening
            // delimiter to already be consumed.
            let out = p.parse_field_token(b'}', None, true).unwrap();
            assert_eq!(output, &out[..]);
        }

        fn expect_error(input: &'static [u8], err: ErrorKind) {
            let mut p = parser(input);
            p.skip_white_space().unwrap();
            assert_eq!(p.parse_field_token(b'}', None, true).unwrap_err(), err);
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

