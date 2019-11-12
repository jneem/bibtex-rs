use std::io;

use common::Input;

#[derive(Debug, PartialEq)]
pub enum LexClass {
    Illegal,
    WhiteSpace,
    SepChar,
    Numeric,
    Alpha,
    Other,
}

// TODO: record the position (line/col)
#[derive(Debug, PartialEq)]
pub enum LexError {
    IllegalStartOfIntLiteral(u8),
    UnterminatedStringLiteral,
    IllegalStuffAfterToken(u8),
}

pub fn lex_class(b: u8) -> LexClass {
    use LexClass::*;

    match b {
        // Strangely, bibtex.c doesn't seem to count '\n' as whitespace. Anyway, it doesn't matter
        // because `Input::input_line` always trims it.
        b'\t' | b'\r' | b' ' => WhiteSpace,
        b'-' | b'~' => SepChar,
        b'0'..=b'9' => Numeric,
        0..=31 | 127 => Illegal,
        b'A'..=b'Z' | b'a'..=b'z' | 128..=255 => Alpha,
        _ => Other,
    }
}

pub fn is_white(b: u8) -> bool {
    lex_class(b) == LexClass::WhiteSpace
}

pub fn is_numeric(b: u8) -> bool {
    b'0' <= b && b <= b'9'
}

pub fn is_id(b: u8) -> bool {
    match b {
        0..=31 | b' ' | b'"' | b'#' | b'%' | b'\'' | b'(' | b')' | b',' | b'=' | b'{' | b'}' => false,
        _ => true,
    }
}

pub trait Lexer {
    fn is_space_or_brace(&self) -> bool;
    fn skip_until_space_or_brace(&mut self);
    fn scan_identifier(&mut self) -> (Vec<u8>, u8);
    fn scan_integer(&mut self) -> Result<i32, LexError>;
    fn scan_function_name(&mut self) -> Vec<u8>;
    fn scan_string(&mut self) -> Result<Vec<u8>, LexError>;
}

impl<'read> Lexer for Input<'read> {
    fn is_space_or_brace(&self) -> bool {
        self.is_eol() || is_white(self.current()) || self.current() == b'}' || self.current() == b'%'
    }

    fn skip_until_space_or_brace(&mut self) {
        self.skip_past(|c| !is_white(c) && c != b'{' && c != b'%');
    }

    fn scan_identifier(&mut self) -> (Vec<u8>, u8) {
        if lex_class(self.current()) == LexClass::Numeric {
            (Vec::new(), self.current())
        } else {
            let count = self.count(is_id);
            let mut id = self.buffer()[..count].to_owned();
            for c in &mut id {
                c.make_ascii_lowercase();
            }
            self.advance_by(count);
            let next = if self.is_eol() { b'\n' } else { self.current() };
            (id, next)
        }
    }

    /// Scans a (possibly signed) integer from the buffer.
    ///
    /// In every case, this function will consume up to the next whitespace, right brace, or
    /// comment.
    fn scan_integer(&mut self) -> Result<i32, LexError> {
        let sign = if self.current() == b'-' {
            self.advance();
            -1
        } else {
            1
        };

        let len = self.count(is_numeric);
        // We don't use rust's built-in integer parsing, because the original bibtex implementation
        // has different overflow semantics.
        let mut val = 0i32;
        let buf = self.buffer();
        for i in 0..len {
            val = val.wrapping_mul(10).wrapping_add((buf[i] - b'0') as i32);
        }
        val *= sign;
        self.advance_by(len);

        if !self.is_space_or_brace() {
            let b = self.current();
            self.skip_until_space_or_brace();
            Err(LexError::IllegalStuffAfterToken(b))
        } else if len == 0 {
            let b = self.current();
            self.skip_until_space_or_brace();
            Err(LexError::IllegalStartOfIntLiteral(b))
        } else {
            Ok(val)
        }
    }

    fn scan_string(&mut self) -> Result<Vec<u8>, LexError> {
        debug_assert!(self.current() == b'"');

        self.advance();
        let len = self.count(|c| c != b'"');
        if len >= self.buffer().len() {
            // If the string is unterminated, skip the rest of the line.
            self.advance_by(len);
            Err(LexError::UnterminatedStringLiteral)
        } else {
            let ret = self.buffer()[..len].to_owned();
            self.advance_by(len + 1); // The +1 is to skip the "
            if !self.is_space_or_brace() {
                let b = self.current();
                self.skip_until_space_or_brace();
                Err(LexError::IllegalStuffAfterToken(b))
            } else {
                Ok(ret)
            }
        }
    }

    /// Scans the name of a function.
    ///
    /// A function name is allowed to contain anything except whitespace, right braces, or comment
    /// characters. An empty function name is not considered an error, so this function never
    /// triggers an error. Function names are case insensitive, and will be converted automatically
    /// to lower case.
    fn scan_function_name(&mut self) -> Vec<u8> {
        let len = self.count(|c| !is_white(c) && c != b'}' && c != b'%');
        let mut ret = self.buffer()[..len].to_owned();
        ret.make_ascii_lowercase();
        self.advance_by(len);
        ret
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_identifier() {
        let mut input = Input::from_reader(&b"id1 ID2 3"[..]);
        input.input_line().unwrap();

        let id = input.scan_identifier();
        assert_eq!(&id, b"id1");

        input.skip_white_space();
        let id = input.scan_identifier();
        assert_eq!(&id, b"id2");

        input.skip_white_space();
        let id = input.scan_identifier();
        assert_eq!(&id, b"");
    }

    #[test]
    fn scan_integer() {
        let mut input = Input::from_reader(&b"-34 46 - -blah 12345678901234567890"[..]);
        input.input_line().unwrap();

        assert_eq!(Ok(-34), input.scan_integer());
        input.skip_white_space();
        assert_eq!(Ok(46), input.scan_integer());
        input.skip_white_space();
        assert_eq!(Err(LexError::IllegalStartOfIntLiteral(b' ')), input.scan_integer());
        input.skip_white_space();
        assert_eq!(Err(LexError::IllegalStuffAfterToken(b'b')), input.scan_integer());
        input.skip_white_space();
        assert_eq!(Ok(-350287150), input.scan_integer()); // overflow is allowed
    }

    #[test]
    fn scan_string() {
        let mut input = Input::from_reader(&br#""foo" "FOO" "bar"blah "unterminated"#[..]);
        input.input_line().unwrap();

        assert_eq!(Ok(b"foo".to_vec()), input.scan_string());
        input.skip_white_space();
        assert_eq!(Ok(b"FOO".to_vec()), input.scan_string());
        input.skip_white_space();
        assert_eq!(Err(LexError::IllegalStuffAfterToken(b'b')), input.scan_string());
        input.skip_white_space();
        assert_eq!(Err(LexError::UnterminatedStringLiteral), input.scan_string());
        assert!(input.is_eol());
    }

    #[test]
    fn scan_function_name() {
        let mut input = Input::from_reader(&b"foo FOO 99x < > { }"[..]);
        input.input_line().unwrap();

        assert_eq!(b"foo".to_vec(), input.scan_function_name());
        input.skip_white_space();
        assert_eq!(b"foo".to_vec(), input.scan_function_name());
        input.skip_white_space();
        assert_eq!(b"99x".to_vec(), input.scan_function_name());
        input.skip_white_space();
        assert_eq!(b"<".to_vec(), input.scan_function_name());
        input.skip_white_space();
        assert_eq!(b">".to_vec(), input.scan_function_name());
        input.skip_white_space();
        assert_eq!(b"{".to_vec(), input.scan_function_name());
        input.skip_white_space();
        assert_eq!(b"".to_vec(), input.scan_function_name());
    }
}
