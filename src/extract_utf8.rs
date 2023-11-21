use core::str::utf8_char_width;

use bytes::Buf as _;

use crate::repeatable::Repeatable;
use crate::{ByteStream, Extract, ParseAny, ParseResult, ParseWhen};

// 'a'
impl Extract for char {
    type State = (u8, ByteStream);

    fn extract(
        &self,
        mut input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, ByteStream> {
        let mut char_buf = [0u8; 4];
        let mut bytes = self.encode_utf8(&mut char_buf).as_bytes();
        let (mut seen, mut output) = state.unwrap_or((0, ByteStream::default()));
        bytes.advance(seen as usize);
        let matched = input.common_prefix_length(bytes);
        if matched == bytes.len() {
            output.extend(input.take_before(matched));
            ParseResult::Match(output, input)
        } else if !last && matched == input.remaining() {
            output.extend(input.take_before(matched));
            seen += matched as u8;
            ParseResult::Partial((seen, output))
        } else {
            ParseResult::NoMatch
        }
    }
}

// 'a'.optional()
impl Repeatable for char {}

// "abc"
impl Extract for &str {
    type State = (usize, ByteStream);

    fn extract(
        &self,
        mut input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, ByteStream> {
        let mut bytes = self.as_bytes();
        let (mut seen, mut output) = state.unwrap_or((0, ByteStream::default()));
        bytes.advance(seen);
        let matched = input.common_prefix_length(bytes);
        if matched == bytes.len() {
            output.extend(input.take_before(matched));
            ParseResult::Match(output, input)
        } else if !last && matched == input.remaining() {
            output.extend(input.take_before(matched));
            seen += matched;
            ParseResult::Partial((seen, output))
        } else {
            ParseResult::NoMatch
        }
    }
}

impl Repeatable for &str {}

pub struct AnyCharParser;

// char::any()
impl Extract for AnyCharParser {
    type State = (usize, ByteStream);

    fn extract(
        &self,
        mut input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, ByteStream> {
        let (mut required, mut output) = state.unwrap_or((0, ByteStream::default()));
        if required == 0 {
            match input.iter().next() {
                Some(&b) => {
                    required = utf8_char_width(b);
                    if required == 0 {
                        return ParseResult::NoMatch;
                    }
                }
                None if last => {
                    return ParseResult::NoMatch;
                }
                None => {
                    return ParseResult::Partial((required, output));
                }
            }
        }
        let input_len = input.remaining();
        if input_len < required {
            if last {
                ParseResult::NoMatch
            } else {
                required -= input_len;
                output.extend(input.take_before(input_len));
                ParseResult::Partial((required, output))
            }
        } else {
            output.extend(input.take_before(required));
            ParseResult::Match(output, input)
        }
    }
}

// char::any().optional()
impl Repeatable for AnyCharParser {}

impl ParseAny for char {
    type Parser = AnyCharParser;

    fn any() -> Self::Parser {
        AnyCharParser
    }
}

#[derive(Clone)]
pub struct CharWhenParser<F>(F);

// char::when(|c|...)
impl<F> Extract for CharWhenParser<F>
where
    F: Fn(char) -> bool,
{
    type State = (usize, ByteStream);

    fn extract(
        &self,
        mut input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, ByteStream> {
        let (mut required, mut output) = state.unwrap_or((0, ByteStream::default()));
        if required == 0 {
            match input.iter().next() {
                Some(&b) => {
                    required = utf8_char_width(b);
                    if required == 0 {
                        return ParseResult::NoMatch;
                    }
                }
                None if last => {
                    return ParseResult::NoMatch;
                }
                None => {
                    return ParseResult::Partial((required, output));
                }
            }
        }
        let input_len = input.remaining();
        if input_len < required {
            if last {
                ParseResult::NoMatch
            } else {
                output.extend(input.take_before(input_len));
                ParseResult::Partial((required - input_len, output))
            }
        } else {
            output.extend(input.take_before(required));
            let mut bytes = [0; 4];
            let len = output.fill_slice(&mut bytes);
            match std::str::from_utf8(&bytes[..len]) {
                Ok(s) => {
                    let Some(c) = s.chars().next() else {
                        return ParseResult::NoMatch;
                    };
                    if (self.0)(c) {
                        ParseResult::Match(output, input)
                    } else {
                        ParseResult::NoMatch
                    }
                }
                Err(_) => ParseResult::NoMatch,
            }
        }
    }
}

// char::when(|c|...).optional()
impl<F> Repeatable for CharWhenParser<F> where F: Fn(char) -> bool {}

impl<F> ParseWhen<char, F> for char
where
    F: Fn(char) -> bool,
{
    type Parser = CharWhenParser<F>;

    fn when(f: F) -> Self::Parser {
        CharWhenParser(f)
    }
}
