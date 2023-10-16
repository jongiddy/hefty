#![feature(assert_matches)]
#![feature(slice_take)]
#![feature(str_internals)]

use core::str::utf8_char_width;
use std::string::FromUtf8Error;

use bytes::{Buf, Bytes};
use either::Either;

#[derive(Debug, Default)]
enum OutputChunks {
    #[default]
    Empty,
    One(Bytes),
    Multiple(Vec<Bytes>),
}

#[derive(Debug, Default)]
struct Output {
    chunks: OutputChunks,
}

impl Output {
    fn push(&mut self, bytes: Bytes) {
        self.chunks = match std::mem::take(&mut self.chunks) {
            OutputChunks::Empty => OutputChunks::One(bytes),
            OutputChunks::One(first) => OutputChunks::Multiple(vec![first, bytes]),
            OutputChunks::Multiple(mut v) => {
                v.push(bytes);
                OutputChunks::Multiple(v)
            }
        }
    }

    fn copy_to_slice(&self, slice: &mut [u8]) -> usize {
        match &self.chunks {
            OutputChunks::Empty => 0,
            OutputChunks::One(bytes) => {
                let len = bytes.len();
                slice[..len].copy_from_slice(bytes);
                len
            }
            OutputChunks::Multiple(chunks) => {
                let mut copied = 0;
                for bytes in chunks {
                    let len = bytes.len();
                    slice[copied..][..len].copy_from_slice(bytes);
                    copied += len;
                }
                copied
            }
        }
    }

    fn into_iter(self) -> impl Iterator<Item = u8> {
        match self.chunks {
            OutputChunks::Empty => Either::Left(Bytes::new().into_iter()),
            OutputChunks::One(bytes) => Either::Left(bytes.into_iter()),
            OutputChunks::Multiple(v) => {
                Either::Right(v.into_iter().flat_map(|bytes| bytes.into_iter()))
            }
        }
    }

    fn into_vec(self) -> Vec<u8> {
        match self.chunks {
            OutputChunks::Empty => Vec::new(),
            OutputChunks::One(bytes) => bytes.into(),
            OutputChunks::Multiple(chunks) => {
                let mut bytes_iter = chunks.into_iter();
                let mut vec = Vec::from(bytes_iter.next().unwrap());
                for bytes in bytes_iter {
                    vec.extend_from_slice(&*bytes)
                }
                vec
            }
        }
    }

    fn into_string(self) -> Result<String, FromUtf8Error> {
        String::from_utf8(self.into_vec())
    }
}

impl ToString for Output {
    fn to_string(&self) -> String {
        match &self.chunks {
            OutputChunks::Empty => String::new(),
            OutputChunks::One(bytes) => String::from_utf8_lossy(bytes).into_owned(),
            OutputChunks::Multiple(chunks) => {
                let mut bytes_iter = chunks.iter();
                let mut vec = Vec::from(bytes_iter.next().unwrap().clone());
                for bytes in bytes_iter {
                    vec.extend_from_slice(&*bytes)
                }
                String::from_utf8_lossy(&vec).into_owned()
            }
        }
    }
}

#[derive(Debug)]
enum ParseResult<State, Output> {
    NoMatch,
    Partial(State),
    Match(Output),
}

trait Extract {
    type State;

    fn extract(
        &self,
        input: &mut Bytes,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Output>;
}

trait ParseAny {
    type Parser: Extract;

    fn any() -> Self::Parser;
}

trait ParseWhen<T, F>
where
    F: Fn(T) -> bool,
{
    type Parser: Extract;

    fn when(f: F) -> Self::Parser;
}

impl Extract for u8 {
    type State = ();

    fn extract(
        &self,
        input: &mut Bytes,
        _state: Option<Self::State>,
    ) -> ParseResult<Self::State, Output> {
        if input.has_remaining() {
            let bytes = input.copy_to_bytes(1);
            if &bytes[0] == self {
                ParseResult::Match(Output {
                    chunks: OutputChunks::One(bytes),
                })
            } else {
                ParseResult::NoMatch
            }
        } else {
            ParseResult::Partial(())
        }
    }
}

impl<const N: usize> Extract for [u8; N] {
    type State = (usize, Output);

    fn extract(
        &self,
        input: &mut Bytes,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Output> {
        let mut bytes = self.as_slice();
        let (mut seen, mut output) = state.unwrap_or((
            0,
            Output {
                chunks: OutputChunks::Empty,
            },
        ));
        bytes.advance(seen);
        if input.len() < bytes.len() {
            if bytes.starts_with(input) {
                seen += input.len();
                output.push(input.split_off(0));
                ParseResult::Partial((seen, output))
            } else {
                ParseResult::NoMatch
            }
        } else {
            if input.starts_with(bytes) {
                let bytes = input.split_to(bytes.len());
                output.push(bytes);
                ParseResult::Match(output)
            } else {
                ParseResult::NoMatch
            }
        }
    }
}

struct AnyByteParser;

impl Extract for AnyByteParser {
    type State = ();

    fn extract(
        &self,
        input: &mut Bytes,
        _state: Option<Self::State>,
    ) -> ParseResult<Self::State, Output> {
        if input.has_remaining() {
            let bytes = input.copy_to_bytes(1);
            ParseResult::Match(Output {
                chunks: OutputChunks::One(bytes),
            })
        } else {
            ParseResult::Partial(())
        }
    }
}

impl ParseAny for u8 {
    type Parser = AnyByteParser;

    fn any() -> Self::Parser {
        AnyByteParser
    }
}

struct ByteWhenParser<F>(F);

impl<F> Extract for ByteWhenParser<F>
where
    F: Fn(u8) -> bool,
{
    type State = ();

    fn extract(
        &self,
        input: &mut Bytes,
        _state: Option<Self::State>,
    ) -> ParseResult<Self::State, Output> {
        match input.first() {
            Some(&b) if (self.0)(b) => ParseResult::Match(Output {
                chunks: OutputChunks::One(input.split_to(1)),
            }),
            Some(_) => ParseResult::NoMatch,
            None => ParseResult::Partial(()),
        }
    }
}

impl<F> ParseWhen<u8, F> for u8
where
    F: Fn(u8) -> bool,
{
    type Parser = ByteWhenParser<F>;

    fn when(f: F) -> Self::Parser {
        ByteWhenParser(f)
    }
}

struct ByteWhenRefParser<F>(F);

impl<F> Extract for ByteWhenRefParser<F>
where
    F: Fn(&u8) -> bool,
{
    type State = ();

    fn extract(
        &self,
        input: &mut Bytes,
        _state: Option<Self::State>,
    ) -> ParseResult<Self::State, Output> {
        match input.first() {
            Some(b) if (self.0)(b) => ParseResult::Match(Output {
                chunks: OutputChunks::One(input.split_to(1)),
            }),
            Some(_) => ParseResult::NoMatch,
            None => ParseResult::Partial(()),
        }
    }
}

impl<F> ParseWhen<&u8, F> for u8
where
    F: Fn(&u8) -> bool,
{
    type Parser = ByteWhenRefParser<F>;

    fn when(f: F) -> Self::Parser {
        ByteWhenRefParser(f)
    }
}

impl Extract for char {
    type State = (u8, Output);

    fn extract(
        &self,
        input: &mut Bytes,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Output> {
        let mut char_buf = [0u8; 4];
        let mut char_as_bytes = self.encode_utf8(&mut char_buf).as_bytes();
        let (mut seen, mut output) = state.unwrap_or((
            0,
            Output {
                chunks: OutputChunks::Empty,
            },
        ));
        char_as_bytes.advance(seen as usize);
        if input.len() < char_as_bytes.len() {
            if char_as_bytes.starts_with(input) {
                seen += input.len() as u8;
                output.push(input.split_off(0));
                ParseResult::Partial((seen, output))
            } else {
                ParseResult::NoMatch
            }
        } else {
            if input.starts_with(char_as_bytes) {
                let bytes = input.split_to(char_as_bytes.len());
                output.push(bytes);
                ParseResult::Match(output)
            } else {
                ParseResult::NoMatch
            }
        }
    }
}

struct AnyCharParser;

impl Extract for AnyCharParser {
    type State = (usize, Output);

    fn extract(
        &self,
        input: &mut Bytes,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Output> {
        let (mut required, mut output) = state.unwrap_or((0, Output::default()));
        if required == 0 {
            match input.first() {
                Some(&b) => {
                    required = utf8_char_width(b);
                    if required == 0 {
                        return ParseResult::NoMatch;
                    }
                }
                None => {
                    return ParseResult::Partial((required, output));
                }
            }
        }
        if input.len() < required {
            required -= input.len();
            output.push(input.split_off(0));
            ParseResult::Partial((required, output))
        } else {
            output.push(input.split_to(required));
            ParseResult::Match(output)
        }
    }
}

impl ParseAny for char {
    type Parser = AnyCharParser;

    fn any() -> Self::Parser {
        AnyCharParser
    }
}

struct CharWhenParser<F>(F);

impl<F> Extract for CharWhenParser<F>
where
    F: Fn(char) -> bool,
{
    type State = (usize, Output);

    fn extract(
        &self,
        input: &mut Bytes,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Output> {
        let (mut required, mut output) = state.unwrap_or((0, Output::default()));
        if required == 0 {
            match input.first() {
                Some(&b) => {
                    required = utf8_char_width(b);
                    if required == 0 {
                        return ParseResult::NoMatch;
                    }
                }
                None => {
                    return ParseResult::Partial((required, output));
                }
            }
        }
        if input.len() < required {
            required -= input.len();
            output.push(input.split_off(0));
            ParseResult::Partial((required, output))
        } else {
            output.push(input.split_to(required));
            let mut bytes = [0; 4];
            let len = output.copy_to_slice(&mut bytes);
            match std::str::from_utf8(&bytes[..len]) {
                Ok(s) => {
                    let Some(c) = s.chars().next() else {
                        return ParseResult::NoMatch;
                    };
                    if (self.0)(c) {
                        ParseResult::Match(output)
                    } else {
                        ParseResult::NoMatch
                    }
                }
                Err(_) => ParseResult::NoMatch,
            }
        }
    }
}

impl<F> ParseWhen<char, F> for char
where
    F: Fn(char) -> bool,
{
    type Parser = CharWhenParser<F>;

    fn when(f: F) -> Self::Parser {
        CharWhenParser(f)
    }
}

struct CharWhenRefParser<F>(F);

impl<F> Extract for CharWhenRefParser<F>
where
    F: Fn(&char) -> bool,
{
    type State = (usize, Output);

    fn extract(
        &self,
        input: &mut Bytes,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Output> {
        let (mut required, mut output) = state.unwrap_or((0, Output::default()));
        if required == 0 {
            match input.first() {
                Some(&b) => {
                    required = utf8_char_width(b);
                    if required == 0 {
                        return ParseResult::NoMatch;
                    }
                }
                None => {
                    return ParseResult::Partial((required, output));
                }
            }
        }
        if input.len() < required {
            required -= input.len();
            output.push(input.split_off(0));
            ParseResult::Partial((required, output))
        } else {
            output.push(input.split_to(required));
            let mut bytes = [0; 4];
            let len = output.copy_to_slice(&mut bytes);
            match std::str::from_utf8(&bytes[..len]) {
                Ok(s) => {
                    let Some(c) = s.chars().next() else {
                        return ParseResult::NoMatch;
                    };
                    if (self.0)(&c) {
                        ParseResult::Match(output)
                    } else {
                        ParseResult::NoMatch
                    }
                }
                Err(_) => ParseResult::NoMatch,
            }
        }
    }
}

impl<F> ParseWhen<&char, F> for char
where
    F: Fn(&char) -> bool,
{
    type Parser = CharWhenRefParser<F>;

    fn when(f: F) -> Self::Parser {
        CharWhenRefParser(f)
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use bytes::Bytes;

    use super::{Extract, ParseAny, ParseResult, ParseWhen};

    #[test]
    fn test_byte_literal() {
        let buffer = Bytes::from_static("23".as_bytes());
        let mut input = buffer.clone();
        assert_eq!(input.len(), 2);
        let res = b'2'.extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "2");
        let res = b'3'.extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "3");
        let res = b'4'.extract(&mut input, None);
        assert_matches!(res, ParseResult::Partial(_));

        let mut input = buffer.clone();
        let res = b'3'.extract(&mut input, None);
        assert_matches!(res, ParseResult::NoMatch);
    }

    #[test]
    fn test_byte_array_literal() {
        let buffer = Bytes::from_static(b"hello, world!");
        let mut input = buffer.clone();
        let res = b"hello".extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "hello");
        let res = b", ".extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == ", ");
        let res = b" everyone!".extract(&mut input, None);
        assert_matches!(res, ParseResult::NoMatch);
    }

    #[test]
    fn test_byte_any() {
        let buffer = Bytes::from_static("A4".as_bytes());
        let mut input = buffer.clone();
        assert_eq!(input.len(), 2);
        let res = u8::any().extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "A");
        let res = u8::any().extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "4");
        let res = u8::any().extract(&mut input, None);
        assert_matches!(res, ParseResult::Partial(_state));
    }

    #[test]
    fn test_byte_when() {
        let buffer = Bytes::from_static("A4".as_bytes());
        let mut input = buffer.clone();
        assert_eq!(input.len(), 2);
        let res = u8::when(|b: u8| u8::is_ascii_alphabetic(&b)).extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "A");
        let res = u8::when(|b: u8| u8::is_ascii_alphabetic(&b)).extract(&mut input.clone(), None);
        assert_matches!(res, ParseResult::NoMatch);
        let res = u8::when(|b: u8| u8::is_ascii_digit(&b)).extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "4");
        let res = u8::when(|b: u8| u8::is_ascii_digit(&b)).extract(&mut input, None);
        assert_matches!(res, ParseResult::Partial(_state));
    }

    #[test]
    fn test_byte_when_ref() {
        let buffer = Bytes::from_static("A4".as_bytes());
        let mut input = buffer.clone();
        assert_eq!(input.len(), 2);
        let res = u8::when(u8::is_ascii_alphabetic).extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "A");
        let res = u8::when(u8::is_ascii_alphabetic).extract(&mut input.clone(), None);
        assert_matches!(res, ParseResult::NoMatch);
        let res = u8::when(u8::is_ascii_digit).extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "4");
        let res = u8::when(u8::is_ascii_digit).extract(&mut input, None);
        assert_matches!(res, ParseResult::Partial(_state));
    }

    #[test]
    fn test_1_byte_char() {
        let buffer = Bytes::from_static("23".as_bytes());
        let mut input = buffer.clone();
        assert_eq!(input.len(), 2);
        let res = '2'.extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "2");
        assert_eq!(&*input, &[b'3']);
        let res = '2'.extract(&mut input, None);
        assert_matches!(res, ParseResult::NoMatch);
    }

    #[test]
    fn test_multibyte_char() {
        let buffer = Bytes::from_static("€4".as_bytes());
        let mut input = buffer.clone();
        assert_eq!(input.len(), 4);
        let res = '€'.extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "€");
        assert_eq!(&*input, &[b'4']);
        let res = '€'.extract(&mut input, None);
        assert_matches!(res, ParseResult::NoMatch);

        let mut input = buffer.clone();
        let res = char::any().extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "€");
        let res = char::any().extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "4");
        let res = char::any().extract(&mut input, None);
        assert_matches!(res, ParseResult::Partial(_state));

        let mut input = buffer.clone();
        let res = char::when(char::is_alphabetic).extract(&mut input.clone(), None);
        assert_matches!(res, ParseResult::NoMatch);
        let res = char::when(|c: char| !c.is_alphabetic()).extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "€");
        let res = char::when(|c| char::is_digit(c, 10)).extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "4");
        let res = char::when(char::is_alphabetic).extract(&mut input, None);
        assert_matches!(res, ParseResult::Partial(_state));

        let mut input = buffer.clone();
        let res = char::when(char::is_ascii_alphabetic).extract(&mut input.clone(), None);
        assert_matches!(res, ParseResult::NoMatch);
        // closure with ref arg does not get inferred for<'a> lifetime if placed in when arg
        let not_alphabetic = |c: &char| -> bool { !c.is_ascii_alphabetic() };
        let res = char::when(not_alphabetic).extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "€");
        let res = char::when(char::is_ascii_digit).extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "4");
        let res = char::when(char::is_ascii_digit).extract(&mut input, None);
        assert_matches!(res, ParseResult::Partial(_state));
    }

    #[test]
    fn test_multibyte_char_byte_by_byte() {
        let buffer = Bytes::from_static("€4".as_bytes());
        let mut input = buffer.slice(..1);
        let res = '€'.extract(&mut input, None);
        let ParseResult::Partial(state) = res else {
            panic!("{res:?}");
        };
        assert!(input.is_empty());
        let mut input = buffer.slice(1..2);
        let res = '€'.extract(&mut input, Some(state));
        let ParseResult::Partial(state) = res else {
            panic!("{res:?}");
        };
        assert!(input.is_empty());
        let mut input = buffer.slice(2..);
        let res = '€'.extract(&mut input, Some(state));
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "€");
        assert_eq!(&*input, &[b'4']);
        let res = '€'.extract(&mut input, None);
        assert_matches!(res, ParseResult::NoMatch);
    }

    // A 3-byte symbol that matches until the last byte
    #[test]
    fn test_multibyte_char_nearly() {
        let buffer = Bytes::from_static("€4".as_bytes());
        let mut input = buffer.slice(..1);
        let res = '₭'.extract(&mut input, None);
        let ParseResult::Partial(state) = res else {
            panic!("{res:?}");
        };
        assert!(input.is_empty());
        let mut input = buffer.slice(1..2);
        let res = '₭'.extract(&mut input, Some(state));
        let ParseResult::Partial(state) = res else {
            panic!("{res:?}");
        };
        assert!(input.is_empty());
        let mut input = buffer.slice(2..);
        let res = '₭'.extract(&mut input, Some(state));
        assert_matches!(res, ParseResult::NoMatch);
    }
}
