#![feature(assert_matches)]
#![feature(associated_type_defaults)]
#![feature(str_internals)]

use core::str::utf8_char_width;
use std::collections::VecDeque;
use std::string::FromUtf8Error;

use bytes::{Buf, Bytes};
use either::Either;

#[derive(Debug, Default, Clone)]
enum ByteChunks {
    #[default]
    Empty,
    One(Bytes),
    Multiple(VecDeque<Bytes>),
}

#[derive(Debug, Default, Clone)]
struct ByteStream {
    chunks: ByteChunks,
}

impl ByteStream {
    fn push_bytes(&mut self, bytes: Bytes) {
        if bytes.is_empty() {
            return;
        }
        self.chunks = match std::mem::take(&mut self.chunks) {
            ByteChunks::Empty => ByteChunks::One(bytes),
            ByteChunks::One(first) => ByteChunks::Multiple(VecDeque::from([first, bytes])),
            ByteChunks::Multiple(mut v) => {
                v.push_back(bytes);
                ByteChunks::Multiple(v)
            }
        }
    }

    fn extend(&mut self, stream: ByteStream) {
        if stream.is_empty() {
            return;
        }
        match self.chunks {
            ByteChunks::Empty => {
                *self = stream;
            }
            _ => match stream.chunks {
                ByteChunks::Empty => {}
                ByteChunks::One(bytes) => {
                    self.push_bytes(bytes);
                }
                ByteChunks::Multiple(chunks) => {
                    chunks.into_iter().for_each(|bytes| self.push_bytes(bytes));
                }
            },
        }
    }

    fn append(&mut self, stream: &ByteStream) {
        match &stream.chunks {
            ByteChunks::Empty => {}
            ByteChunks::One(bytes) => {
                self.push_bytes(bytes.clone());
            }
            ByteChunks::Multiple(chunks) => {
                chunks
                    .iter()
                    .for_each(|bytes| self.push_bytes(bytes.clone()));
            }
        }
    }

    // Given a slice, how many bytes match?
    fn common_prefix_length(&self, slice: &[u8]) -> usize {
        let mut count = 0;
        for (b1, b2) in self.iter().zip(slice) {
            if b1 == b2 {
                count += 1;
            } else {
                break;
            }
        }
        count
    }

    fn get_before(&self, mut pos: usize) -> Self {
        assert!(pos <= self.remaining());
        let mut byte_stream = ByteStream::default();
        match &self.chunks {
            ByteChunks::Empty => {}
            ByteChunks::One(bytes) => {
                byte_stream.push_bytes(bytes.slice(..pos));
            }
            ByteChunks::Multiple(chunks) => {
                for bytes in chunks {
                    if bytes.len() < pos {
                        byte_stream.push_bytes(bytes.slice(..pos));
                        break;
                    } else {
                        byte_stream.push_bytes(bytes.clone());
                        pos -= bytes.len();
                    }
                }
            }
        }
        byte_stream
    }

    fn take_before(&mut self, pos: usize) -> Self {
        if pos == self.remaining() {
            return std::mem::take(self);
        }
        let byte_stream = self.get_before(pos);
        self.advance(pos);
        byte_stream
    }

    fn is_empty(&self) -> bool {
        matches!(self.chunks, ByteChunks::Empty)
    }

    fn copy_to_slice(&self, slice: &mut [u8]) -> usize {
        match &self.chunks {
            ByteChunks::Empty => 0,
            ByteChunks::One(bytes) => {
                let len = bytes.len();
                slice[..len].copy_from_slice(bytes);
                len
            }
            ByteChunks::Multiple(chunks) => {
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

    fn iter(&self) -> impl Iterator<Item = &u8> {
        match &self.chunks {
            ByteChunks::Empty => Either::Left(Either::Left(std::iter::empty())),
            ByteChunks::One(bytes) => Either::Left(Either::Right(bytes.iter())),
            ByteChunks::Multiple(v) => Either::Right(v.iter().flat_map(|bytes| bytes.iter())),
        }
    }

    fn into_iter(self) -> impl Iterator<Item = u8> {
        match self.chunks {
            ByteChunks::Empty => Either::Left(Bytes::new().into_iter()),
            ByteChunks::One(bytes) => Either::Left(bytes.into_iter()),
            ByteChunks::Multiple(v) => {
                Either::Right(v.into_iter().flat_map(|bytes| bytes.into_iter()))
            }
        }
    }

    fn into_vec(self) -> Vec<u8> {
        match self.chunks {
            ByteChunks::Empty => Vec::new(),
            ByteChunks::One(bytes) => bytes.into(),
            ByteChunks::Multiple(chunks) => {
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

impl From<Bytes> for ByteStream {
    fn from(bytes: Bytes) -> Self {
        ByteStream {
            chunks: ByteChunks::One(bytes),
        }
    }
}

impl From<&'static str> for ByteStream {
    fn from(s: &'static str) -> Self {
        ByteStream {
            chunks: ByteChunks::One(Bytes::from_static(s.as_bytes())),
        }
    }
}

impl Buf for ByteStream {
    fn remaining(&self) -> usize {
        match &self.chunks {
            ByteChunks::Empty => 0,
            ByteChunks::One(bytes) => bytes.len(),
            ByteChunks::Multiple(chunks) => chunks.iter().map(|bytes| bytes.len()).sum(),
        }
    }

    fn chunk(&self) -> &[u8] {
        match &self.chunks {
            ByteChunks::Empty => &[],
            ByteChunks::One(bytes) => &bytes,
            ByteChunks::Multiple(chunks) => chunks.front().unwrap(),
        }
    }

    fn advance(&mut self, mut cnt: usize) {
        assert!(cnt < self.remaining());
        match &mut self.chunks {
            ByteChunks::Empty => {}
            ByteChunks::One(bytes) => {
                bytes.advance(cnt);
                if bytes.is_empty() {
                    self.chunks = ByteChunks::Empty;
                }
            }
            ByteChunks::Multiple(chunks) => {
                loop {
                    let Some(bytes) = chunks.front_mut() else {
                        assert!(cnt == 0);
                        break;
                    };
                    if cnt < bytes.len() {
                        bytes.advance(cnt);
                        break;
                    } else {
                        cnt -= bytes.len();
                        chunks.pop_front();
                    }
                }
                match chunks.len() {
                    0 => {
                        self.chunks = ByteChunks::Empty;
                    }
                    1 => {
                        self.chunks = ByteChunks::One(chunks.pop_front().unwrap());
                    }
                    _ => {}
                }
            }
        }
    }
}

impl ToString for ByteStream {
    fn to_string(&self) -> String {
        match &self.chunks {
            ByteChunks::Empty => String::new(),
            ByteChunks::One(bytes) => String::from_utf8_lossy(bytes).into_owned(),
            ByteChunks::Multiple(chunks) => {
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

enum ParseResult<State, Output> {
    NoMatch,
    Partial(State),
    Match(Output),
}

impl<State, Output> std::fmt::Debug for ParseResult<State, Output> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoMatch => write!(f, "NoMatch"),
            Self::Partial(_arg0) => write!(f, "Partial"),
            Self::Match(_arg0) => write!(f, "Match"),
        }
    }
}

struct OptionalParser<InnerParser> {
    inner: InnerParser,
}

impl<InnerParser> OptionalParser<InnerParser> {
    fn new(inner: InnerParser) -> Self {
        Self { inner }
    }
}

impl<InnerParser> Extract for OptionalParser<InnerParser>
where
    InnerParser: Extract,
{
    type State = (InnerParser::State, ByteStream);

    fn extract(
        &self,
        input: &mut ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        let (inner_state, mut saved) = state
            .map(|(inner_state, saved)| (Some(inner_state), saved))
            .unwrap_or((None, ByteStream::default()));
        saved.append(input);
        match self.inner.extract(input, inner_state) {
            ParseResult::NoMatch => {
                *input = saved;
                ParseResult::Match(ByteStream {
                    chunks: ByteChunks::Empty,
                })
            }
            ParseResult::Partial(inner_state) => ParseResult::Partial((inner_state, saved)),
            ParseResult::Match(output) => ParseResult::Match(output),
        }
    }
}

trait Extract {
    type State;

    fn extract(
        &self,
        input: &mut ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream>;
}

trait Repeatable: Extract + Sized {
    fn optional(self) -> OptionalParser<Self> {
        OptionalParser::new(self)
    }
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
        input: &mut ByteStream,
        _state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        if !input.is_empty() {
            let output = input.take_before(1);
            if output.iter().next().unwrap() == self {
                ParseResult::Match(output)
            } else {
                ParseResult::NoMatch
            }
        } else {
            ParseResult::Partial(())
        }
    }
}

impl Repeatable for u8 {}

impl<const N: usize> Extract for [u8; N] {
    type State = (usize, ByteStream);

    fn extract(
        &self,
        input: &mut ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        let mut bytes = self.as_slice();
        let (seen, mut output) = state.unwrap_or((
            0,
            ByteStream {
                chunks: ByteChunks::Empty,
            },
        ));
        bytes.advance(seen);
        let matched = input.common_prefix_length(bytes);
        if matched == bytes.len() {
            output.extend(input.take_before(matched));
            ParseResult::Match(output)
        } else if matched == input.remaining() {
            output.extend(input.take_before(matched));
            ParseResult::Partial((seen + matched, output))
        } else {
            ParseResult::NoMatch
        }
    }
}

impl<const N: usize> Repeatable for [u8; N] {}

struct AnyByteParser;

impl Extract for AnyByteParser {
    type State = ();

    fn extract(
        &self,
        input: &mut ByteStream,
        _state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        if input.has_remaining() {
            ParseResult::Match(input.take_before(1))
        } else {
            ParseResult::Partial(())
        }
    }
}

impl Repeatable for AnyByteParser {}

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
        input: &mut ByteStream,
        _state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        let first = input.iter().next().cloned();
        match first {
            Some(b) if (self.0)(b) => ParseResult::Match(input.take_before(1)),
            Some(_) => ParseResult::NoMatch,
            None => ParseResult::Partial(()),
        }
    }
}

impl<F> Repeatable for ByteWhenParser<F> where F: Fn(u8) -> bool {}

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
        input: &mut ByteStream,
        _state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        let first = input.iter().next().cloned();
        match first {
            Some(b) if (self.0)(&b) => ParseResult::Match(input.take_before(1)),
            Some(_) => ParseResult::NoMatch,
            None => ParseResult::Partial(()),
        }
    }
}

impl<F> Repeatable for ByteWhenRefParser<F> where F: Fn(&u8) -> bool {}

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
    type State = (u8, ByteStream);

    fn extract(
        &self,
        input: &mut ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        let mut char_buf = [0u8; 4];
        let mut bytes = self.encode_utf8(&mut char_buf).as_bytes();
        let (mut seen, mut output) = state.unwrap_or((
            0,
            ByteStream {
                chunks: ByteChunks::Empty,
            },
        ));
        bytes.advance(seen as usize);
        let matched = input.common_prefix_length(bytes);
        if matched == bytes.len() {
            output.append(&input.get_before(matched));
            input.advance(matched);
            ParseResult::Match(output)
        } else if matched == input.remaining() {
            output.append(input);
            *input = ByteStream::default();
            seen += matched as u8;
            ParseResult::Partial((seen, output))
        } else {
            ParseResult::NoMatch
        }
    }
}

impl Repeatable for char {}

impl Extract for &str {
    type State = (usize, ByteStream);

    fn extract(
        &self,
        input: &mut ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        let mut bytes = self.as_bytes();
        let (mut seen, mut output) = state.unwrap_or((
            0,
            ByteStream {
                chunks: ByteChunks::Empty,
            },
        ));
        bytes.advance(seen);
        let matched = input.common_prefix_length(bytes);
        if matched == bytes.len() {
            output.extend(input.take_before(matched));
            ParseResult::Match(output)
        } else if matched == input.remaining() {
            output.extend(input.take_before(matched));
            seen += matched;
            ParseResult::Partial((seen, output))
        } else {
            ParseResult::NoMatch
        }
    }
}

impl Repeatable for &str {}

struct AnyCharParser;

impl Extract for AnyCharParser {
    type State = (usize, ByteStream);

    fn extract(
        &self,
        input: &mut ByteStream,
        state: Option<Self::State>,
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
                None => {
                    return ParseResult::Partial((required, output));
                }
            }
        }
        let input_len = input.remaining();
        if input_len < required {
            required -= input_len;
            output.append(&input.take_before(input_len));
            ParseResult::Partial((required, output))
        } else {
            output.append(&input.take_before(required));
            ParseResult::Match(output)
        }
    }
}

impl Repeatable for AnyCharParser {}

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
    type State = (usize, ByteStream);

    fn extract(
        &self,
        input: &mut ByteStream,
        state: Option<Self::State>,
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
                None => {
                    return ParseResult::Partial((required, output));
                }
            }
        }
        let input_len = input.remaining();
        if input_len < required {
            output.append(&input.take_before(input_len));
            ParseResult::Partial((required - input_len, output))
        } else {
            output.append(&input.take_before(required));
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

struct CharWhenRefParser<F>(F);

impl<F> Extract for CharWhenRefParser<F>
where
    F: Fn(&char) -> bool,
{
    type State = (usize, ByteStream);

    fn extract(
        &self,
        input: &mut ByteStream,
        state: Option<Self::State>,
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
                None => {
                    return ParseResult::Partial((required, output));
                }
            }
        }
        let input_len = input.remaining();
        if input_len < required {
            output.append(&input.take_before(input_len));
            ParseResult::Partial((required - input_len, output))
        } else {
            output.append(&input.take_before(required));
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

impl<F> Repeatable for CharWhenRefParser<F> where F: Fn(&char) -> bool {}

impl<F> ParseWhen<&char, F> for char
where
    F: Fn(&char) -> bool,
{
    type Parser = CharWhenRefParser<F>;

    fn when(f: F) -> Self::Parser {
        CharWhenRefParser(f)
    }
}

impl<F> Extract for F
where
    F: Fn(&mut ByteStream) -> ParseResult<ByteStream, ByteStream>,
{
    type State = ByteStream;

    fn extract(
        &self,
        input: &mut ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        (self)(input)
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;
    use std::collections::VecDeque;

    use bytes::Buf;

    use super::{ByteStream, Extract, ParseAny, ParseResult, ParseWhen, Repeatable};

    #[test]
    fn test_byte_literal() {
        let buffer = ByteStream::from("23");
        let mut input = buffer.clone();
        assert_eq!(input.remaining(), 2);
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
        let buffer = ByteStream::from("hello, world!");
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
        let buffer = ByteStream::from("A4");
        let mut input = buffer.clone();
        assert_eq!(input.remaining(), 2);
        let res = u8::any().extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "A");
        let res = u8::any().extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "4");
        let res = u8::any().extract(&mut input, None);
        assert_matches!(res, ParseResult::Partial(_state));
    }

    #[test]
    fn test_byte_when() {
        let buffer = ByteStream::from("A4");
        let mut input = buffer.clone();
        assert_eq!(input.remaining(), 2);
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
        let buffer = ByteStream::from("A4");
        let mut input = buffer.clone();
        assert_eq!(input.remaining(), 2);
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
    fn test_str_literal() {
        let buffer = ByteStream::from("hello, world!");
        let mut input = buffer.clone();
        let res = "hello".extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "hello");
        let res = ", ".extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == ", ");
        let res = " everyone!".extract(&mut input, None);
        assert_matches!(res, ParseResult::NoMatch);
    }

    #[test]
    fn test_1_byte_char() {
        let buffer = ByteStream::from("23");
        let mut input = buffer.clone();
        assert_eq!(input.remaining(), 2);
        let res = '2'.extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "2");
        assert_eq!(input.iter().next(), Some(&b'3'));
        let res = '2'.extract(&mut input, None);
        assert_matches!(res, ParseResult::NoMatch);
    }

    #[test]
    fn test_multibyte_char() {
        let buffer = ByteStream::from("€4");
        let mut input = buffer.clone();
        assert_eq!(input.remaining(), 4);
        let res = '€'.extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "€");
        assert_eq!(input.iter().next(), Some(&b'4'));
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
        let mut buffer = ByteStream::from("€4");
        let mut input = buffer.take_before(1);
        let res = '€'.extract(&mut input, None);
        let ParseResult::Partial(state) = res else {
            panic!("{res:?}");
        };
        assert!(input.is_empty());
        let mut input = buffer.take_before(1);
        let res = '€'.extract(&mut input, Some(state));
        let ParseResult::Partial(state) = res else {
            panic!("{res:?}");
        };
        assert!(input.is_empty());
        let mut input = buffer;
        let res = '€'.extract(&mut input, Some(state));
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "€");
        assert_eq!(input.iter().next(), Some(&b'4'));
        let res = '€'.extract(&mut input, None);
        assert_matches!(res, ParseResult::NoMatch);
    }

    // A 3-byte symbol that matches until the last byte
    #[test]
    fn test_multibyte_char_nearly() {
        let mut buffer = ByteStream::from("€4");
        let mut input = buffer.take_before(1);
        let res = '₭'.extract(&mut input, None);
        let ParseResult::Partial(state) = res else {
            panic!("{res:?}");
        };
        assert!(input.is_empty());
        let mut input = buffer.take_before(1);
        let res = '₭'.extract(&mut input, Some(state));
        let ParseResult::Partial(state) = res else {
            panic!("{res:?}");
        };
        assert!(input.is_empty());
        let mut input = buffer;
        let res = '₭'.extract(&mut input, Some(state));
        assert_matches!(res, ParseResult::NoMatch);
    }

    #[test]
    fn test_optional() {
        let mut input = ByteStream::from("helloworld");
        let res = "hello".optional().extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "hello");
        let res = ", ".optional().extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.is_empty());
        let res = "world".optional().extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "world");
        let res = "world".optional().extract(&mut input, None);
        assert_matches!(res, ParseResult::Partial(_state));
    }

    fn reverse(input: &mut ByteStream) -> ParseResult<ByteStream, ByteStream> {
        let mut d = VecDeque::new();
        for b in input.iter().cloned() {
            d.push_front(b);
        }
        let v = d.into_iter().collect::<Vec<_>>();
        let output = ByteStream::from(bytes::Bytes::from(v));
        ParseResult::Match(output)
    }

    fn make_reverse(i: usize) -> impl Fn(&mut ByteStream) -> ParseResult<ByteStream, ByteStream> {
        move |bs| {
            let mut bs1 = bs.take_before(i);
            reverse(&mut bs1)
        }
    }

    #[test]
    fn test_function() {
        let mut input = ByteStream::from("helloworld");
        let res = reverse.extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "dlrowolleh");

        let mut input = ByteStream::from("helloworld");
        let res = make_reverse(6).extract(&mut input, None);
        assert_matches!(res, ParseResult::Match(output) if output.to_string() == "wolleh");
    }
}
