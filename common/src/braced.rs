/// Iterates over braced substrings of a string.
pub fn braced_tokens<'a>(input: &'a [u8]) -> impl Iterator<Item=Token<'a>> {
    Tokenizer {
        input,
        idx: 0,
    }
}

struct Tokenizer<'a> {
    input: &'a [u8],
    idx: usize,
}

#[derive(Debug, PartialEq)]
/// The different tokens that we can encounter while iterating over braced substrings.
pub enum Token<'a> {
    /// A single byte, which is guaranteed not to be `'{'` or `'}'`.
    Byte {
        ch: u8,
        idx: usize,
    },
    /// A substring delimited by braces. The substring might not be fully balanced, in the case that
    /// we ran out of input before we found enough closing braces.
    Braced {
        s: &'a [u8],
        idx: usize,
        closed: bool,
    },
    /// A right brace that was not matched by a preceding left brace.
    UnmatchedRBrace {
        idx: usize,
    },
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        if self.idx >= self.input.len() {
            None
        } else {
            let old_idx = self.idx;
            match self.input[self.idx] {
                b'}' => {
                    self.idx += 1;
                    Some(Token::UnmatchedRBrace { idx: old_idx })
                }
                b'{' => {
                    let mut depth = 1;
                    let mut end = self.idx + 1;
                    while end < self.input.len() {
                        match self.input[end] {
                            b'}' => {
                                depth -= 1;
                                if depth == 0 {
                                    end += 1;
                                    break;
                                }
                            }
                            b'{' => {
                                depth += 1;
                            }
                            _ => {}
                        }
                        end += 1;
                    }
                    let ret = &self.input[old_idx..end];
                    self.idx = end;
                    Some(Token::Braced {
                        s: ret,
                        idx: old_idx,
                        closed: depth == 0,
                    })
                }
                ch => {
                    self.idx += 1;
                    Some(Token::Byte { ch, idx: old_idx })
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn nested_braces() {
        assert_eq!(
            braced_tokens(b"F{Bar {Nested} Baz}").collect::<Vec<_>>(),
            vec![
                Byte { ch: b'F', idx: 0 },
                Braced { s: b"{Bar {Nested} Baz}", idx: 1, closed: true },
            ]
        );
    }

    #[test]
    fn unterminated() {
        assert_eq!(
            braced_tokens(b"{missing {closing} brace").collect::<Vec<_>>(),
            vec![
                Braced { s: b"{missing {closing} brace", idx: 0, closed: false },
            ]
        );
    }

    #[test]
    fn unbalanced() {
        assert_eq!(
            braced_tokens(b"}{").collect::<Vec<_>>(),
            vec![
                UnmatchedRBrace { idx: 0 },
                Braced { s: b"{", idx: 1, closed: false },
            ]
        );
    }
}
