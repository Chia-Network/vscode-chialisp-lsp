//! LSP uses UTF-16 code units; the compiler and document storage use UTF-8 bytes.
use std::rc::Rc;

use lsp_types::{Position, Range};

pub fn utf16_to_byte(line: &[u8], character: u32) -> usize {
    let text = std::str::from_utf8(line).unwrap_or("");
    let mut units = 0;
    for (offset, ch) in text.char_indices() {
        if units + ch.len_utf16() as u32 > character {
            return offset;
        }
        units += ch.len_utf16() as u32;
    }
    line.len()
}

pub fn byte_to_utf16(line: &[u8], byte: u32) -> u32 {
    let text = std::str::from_utf8(line).unwrap_or("");
    text.char_indices()
        .take_while(|(offset, _)| *offset < byte as usize)
        .map(|(_, ch)| ch.len_utf16() as u32)
        .sum()
}

pub fn position_to_byte(lines: &[Rc<Vec<u8>>], position: Position) -> Position {
    Position {
        line: position.line,
        character: lines
            .get(position.line as usize)
            .map(|line| utf16_to_byte(line, position.character) as u32)
            .unwrap_or(0),
    }
}

pub fn position_to_utf16(lines: &[Rc<Vec<u8>>], position: Position) -> Position {
    Position {
        line: position.line,
        character: lines
            .get(position.line as usize)
            .map(|line| byte_to_utf16(line, position.character))
            .unwrap_or(0),
    }
}

pub fn range_to_utf16(lines: &[Rc<Vec<u8>>], range: Range) -> Range {
    Range {
        start: position_to_utf16(lines, range.start),
        end: position_to_utf16(lines, range.end),
    }
}

/// Recognize comments and strings using the same quote and escape rules as the
/// compiler. State survives line boundaries because quoted strings may span lines.
#[derive(Default)]
pub(crate) struct LexicalState {
    quote: Option<u8>,
    escaped: bool,
    comment: bool,
    atom: bool,
}

impl LexicalState {
    /// True only for source bytes outside comments and quoted strings.
    pub(crate) fn code(&mut self, byte: u8) -> bool {
        if self.comment {
            if byte == b'\n' {
                self.comment = false;
            }
            return false;
        }
        if let Some(quote) = self.quote {
            if self.escaped {
                self.escaped = false;
            } else if byte == b'\\' {
                self.escaped = true;
            } else if byte == quote {
                self.quote = None;
            }
            return false;
        }
        match byte {
            b';' if !self.atom => {
                self.comment = true;
                false
            }
            b'"' | b'\'' if !self.atom => {
                self.quote = Some(byte);
                false
            }
            b'(' | b')' => {
                self.atom = false;
                true
            }
            byte if byte.is_ascii_whitespace() => {
                self.atom = false;
                true
            }
            _ => {
                self.atom = true;
                true
            }
        }
    }

    pub(crate) fn in_comment(&self) -> bool {
        self.comment
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn utf16_and_utf8_boundaries() {
        let line = "aé😀z".as_bytes();
        for (utf16, byte) in [(0, 0), (1, 1), (2, 3), (4, 7), (5, 8)] {
            assert_eq!(utf16_to_byte(line, utf16), byte);
            assert_eq!(byte_to_utf16(line, byte as u32), utf16);
        }
        assert_eq!(utf16_to_byte(line, 99), line.len());
        assert_eq!(utf16_to_byte(line, 3), 3); // Never split a surrogate pair.
    }
}
