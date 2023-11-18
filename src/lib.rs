#![feature(assert_matches)]
#![feature(associated_type_defaults)]
#![feature(str_internals)]

use core::str::utf8_char_width;
use std::marker::PhantomData;
use std::ops::RangeBounds;

use bytes::Buf;

mod byte_stream;
mod tuple;

pub use self::byte_stream::ByteStream;
pub use self::tuple::ExtractTuple;

pub enum ParseResult<State, Output> {
    NoMatch,
    Partial(State),
    Match(Output, ByteStream),
}

impl<State, Output> std::fmt::Debug for ParseResult<State, Output> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoMatch => write!(f, "NoMatch"),
            Self::Partial(_arg0) => write!(f, "Partial"),
            Self::Match(_arg0, _arg1) => write!(f, "Match"),
        }
    }
}

/// A trait to extract a type from a `ByteStream`. The `extract` method is initially called with a
/// state of `None`. If the type is extracted fully, the method returns `ParseResult::Match` with
/// the extracted type and the remaining `ByteStream`. If the type cannot be extracted because it
/// does not match, the `ParseResult::NoMatch` is returned. If the extraction reads to the end of
/// the `ByteStream` but needs more data to extract the type it returns `ParseResult::Partial` with
/// a state. The next call to `extract` with a new `ByteStream` must pass in the returned state to
/// continue parsing the type. If the end of the stream is reached while the state is `Partial` call
/// `stop` with the state as some extractors can return `ParseResult::Match` in this case. e.g.
/// `'a'.repeated(2..5)` will return Partial for "aaa" since it may be part of a longer pattern, but
/// on end of stream, will return the 3 characters as a match.
pub trait Extract {
    type State;
    type Output = ByteStream;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, Self::Output>;
}

pub trait Repeatable: Extract + Sized {
    fn optional(self) -> OptionalParser<Self> {
        OptionalParser::new(self)
    }

    fn times(self, n: usize) -> TimesParser<Self> {
        TimesParser::new(self, n)
    }

    fn repeated<R: RangeBounds<usize>>(self, r: R) -> RepeatedParser<Self> {
        RepeatedParser::new(self, r)
    }
}

pub trait ParseAny {
    type Parser: Extract;

    fn any() -> Self::Parser;
}

pub trait ParseWhen<T, F>
where
    F: Fn(T) -> bool,
{
    type Parser: Extract;

    fn when(f: F) -> Self::Parser;
}

pub struct OptionalParser<InnerParser> {
    inner: InnerParser,
}

#[allow(dead_code)]
impl<InnerParser> OptionalParser<InnerParser> {
    fn new(inner: InnerParser) -> Self {
        Self { inner }
    }
}

impl<InnerParser> Extract for OptionalParser<InnerParser>
where
    InnerParser: Extract,
    InnerParser::Output: Default,
{
    // The state saves the state of the inner parser and the consumed ByteStream. The consumed
    // ByteStream is kept so that it can be returned in a Match result for the None side of Option.
    type State = (InnerParser::State, ByteStream);
    type Output = InnerParser::Output;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, Self::Output> {
        let (inner_state, mut saved) = state
            .map(|(s, saved)| (Some(s), saved))
            .unwrap_or((None, ByteStream::default()));
        saved.append(&input);
        match self.inner.extract(input, inner_state, last) {
            ParseResult::NoMatch => ParseResult::Match(Self::Output::default(), saved),
            ParseResult::Partial(_) if last => ParseResult::Match(Self::Output::default(), saved),
            ParseResult::Partial(inner_state) => ParseResult::Partial((inner_state, saved)),
            ParseResult::Match(output, input) => ParseResult::Match(output, input),
        }
    }
}

pub struct TimesParser<InnerParser> {
    inner: InnerParser,
    times: usize,
}

#[allow(dead_code)]
impl<InnerParser> TimesParser<InnerParser> {
    fn new(inner: InnerParser, n: usize) -> Self {
        Self { inner, times: n }
    }
}

impl<InnerParser> Extract for TimesParser<InnerParser>
where
    InnerParser: Extract,
{
    type State = (InnerParser::State, Vec<InnerParser::Output>);
    type Output = Vec<InnerParser::Output>;

    fn extract(
        &self,
        mut input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, Self::Output> {
        let (mut inner_state, mut output) = state
            .map(|(s, o)| (Some(s), o))
            .unwrap_or((None, Vec::new()));
        while output.len() < self.times {
            match self.inner.extract(input, inner_state, last) {
                ParseResult::NoMatch => {
                    return ParseResult::NoMatch;
                }
                ParseResult::Partial(_) if last => {
                    return ParseResult::NoMatch;
                }
                ParseResult::Partial(inner_state) => {
                    return ParseResult::Partial((inner_state, output));
                }
                ParseResult::Match(inner_output, inner_input) => {
                    output.push(inner_output);
                    inner_state = None;
                    input = inner_input;
                }
            }
        }
        ParseResult::Match(output, input)
    }
}

pub struct RepeatedParser<InnerParser> {
    inner: InnerParser,
    min: usize,
    max: usize,
}

impl<InnerParser> RepeatedParser<InnerParser> {
    fn new<R: RangeBounds<usize>>(inner: InnerParser, range: R) -> Self {
        let min = match range.start_bound() {
            std::ops::Bound::Included(&n) => n,
            std::ops::Bound::Excluded(&n) => n + 1,
            std::ops::Bound::Unbounded => usize::MIN,
        };
        let max = match range.end_bound() {
            std::ops::Bound::Included(&n) => n,
            std::ops::Bound::Excluded(&n) => n - 1,
            std::ops::Bound::Unbounded => usize::MAX,
        };
        Self { inner, min, max }
    }
}

impl<InnerParser> Extract for RepeatedParser<InnerParser>
where
    InnerParser: Extract,
{
    type State = (InnerParser::State, ByteStream, Vec<InnerParser::Output>);
    type Output = Vec<InnerParser::Output>;

    fn extract(
        &self,
        mut input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, Self::Output> {
        let (mut inner_state, mut saved, mut output) = state
            .map(|(s, saved, output)| (Some(s), saved, output))
            .unwrap_or((None, ByteStream::default(), Vec::new()));
        while output.len() < self.min {
            match self.inner.extract(input, inner_state, last) {
                ParseResult::NoMatch => {
                    return ParseResult::NoMatch;
                }
                ParseResult::Partial(_) if last => {
                    return ParseResult::NoMatch;
                }
                ParseResult::Partial(inner_state) => {
                    return ParseResult::Partial((inner_state, saved, output));
                }
                ParseResult::Match(inner_output, inner_input) => {
                    output.push(inner_output);
                    inner_state = None;
                    input = inner_input;
                }
            }
        }
        while output.len() < self.max {
            saved.append(&input);
            match self.inner.extract(input, inner_state, last) {
                ParseResult::NoMatch => {
                    return ParseResult::Match(output, saved);
                }
                ParseResult::Partial(_) if last => {
                    return ParseResult::Match(output, saved);
                }
                ParseResult::Partial(inner_state) => {
                    return ParseResult::Partial((inner_state, saved, output));
                }
                ParseResult::Match(inner_output, inner_input) => {
                    output.push(inner_output);
                    inner_state = None;
                    saved = ByteStream::default();
                    input = inner_input;
                }
            }
        }
        ParseResult::Match(output, input)
    }
}

// A trait to merge a sequence of ByteStreams into a single ByteStream
pub trait Collectable: Extract + Sized {
    fn collect<Target>(self) -> CollectParser<Target, Self> {
        CollectParser::new(self)
    }
}

pub struct CollectParser<Target, InnerParser> {
    inner: InnerParser,
    target: PhantomData<Target>,
}

#[allow(dead_code)]
impl<Target, InnerParser> CollectParser<Target, InnerParser> {
    fn new(inner: InnerParser) -> Self {
        Self {
            inner,
            target: PhantomData,
        }
    }
}

impl<Target, InnerParser> Extract for CollectParser<Target, InnerParser>
where
    InnerParser: Extract,
    InnerParser::Output: IntoIterator,
    Target: FromIterator<<<InnerParser as Extract>::Output as std::iter::IntoIterator>::Item>,
{
    type State = InnerParser::State;
    type Output = Target;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, Self::Output> {
        match self.inner.extract(input, state, last) {
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Partial(state) => ParseResult::Partial(state),
            ParseResult::Match(output, input) => {
                ParseResult::Match(output.into_iter().collect(), input)
            }
        }
    }
}

impl<Parser> Collectable for Parser
where
    Parser: Extract,
    Parser::Output: IntoIterator<Item = ByteStream>,
{
}

// b'a'
impl Extract for u8 {
    type State = ();

    fn extract(
        &self,
        mut input: ByteStream,
        _state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, ByteStream> {
        if input.is_empty() {
            if last {
                ParseResult::NoMatch
            } else {
                ParseResult::Partial(())
            }
        } else {
            let output = input.take_before(1);
            if output.iter().next().unwrap() == self {
                ParseResult::Match(output, input)
            } else {
                ParseResult::NoMatch
            }
        }
    }
}

// b'a'.optional()
impl Repeatable for u8 {}

// b"abc".extract()
impl<const N: usize> Extract for [u8; N] {
    type State = (usize, ByteStream);

    fn extract(
        &self,
        mut input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, ByteStream> {
        let mut bytes = self.as_slice();
        let (seen, mut output) = state.unwrap_or((0, ByteStream::default()));
        bytes.advance(seen);
        let matched = input.common_prefix_length(bytes);
        if matched == bytes.len() {
            output.extend(input.take_before(matched));
            ParseResult::Match(output, input)
        } else if !last && matched == input.remaining() {
            output.extend(input.take_before(matched));
            ParseResult::Partial((seen + matched, output))
        } else {
            ParseResult::NoMatch
        }
    }
}

// b"abc".optional()
impl<const N: usize> Repeatable for [u8; N] {}

pub struct AnyByteParser;

#[allow(non_camel_case_types)]
pub struct byte {}

// byte::any()
impl Extract for AnyByteParser {
    type State = ();

    fn extract(
        &self,
        mut input: ByteStream,
        _state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, ByteStream> {
        if input.has_remaining() {
            ParseResult::Match(input.take_before(1), input)
        } else if last {
            ParseResult::NoMatch
        } else {
            ParseResult::Partial(())
        }
    }
}

// byte::any().optional()
impl Repeatable for AnyByteParser {}

impl ParseAny for byte {
    type Parser = AnyByteParser;

    fn any() -> Self::Parser {
        AnyByteParser
    }
}

pub struct ByteWhenParser<F>(F);

// byte::when(|u8|...)
impl<F> Extract for ByteWhenParser<F>
where
    F: Fn(u8) -> bool,
{
    type State = ();

    fn extract(
        &self,
        mut input: ByteStream,
        _state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, ByteStream> {
        let first = input.iter().next().cloned();
        match first {
            Some(b) if (self.0)(b) => ParseResult::Match(input.take_before(1), input),
            Some(_) => ParseResult::NoMatch,
            None if last => ParseResult::NoMatch,
            None => ParseResult::Partial(()),
        }
    }
}

// byte::when(|u8|...).optional()
impl<F> Repeatable for ByteWhenParser<F> where F: Fn(u8) -> bool {}

impl<F> ParseWhen<u8, F> for byte
where
    F: Fn(u8) -> bool,
{
    type Parser = ByteWhenParser<F>;

    fn when(f: F) -> Self::Parser {
        ByteWhenParser(f)
    }
}

pub struct ByteWhenRefParser<F>(F);

// byte::when(|&u8|...)
impl<F> Extract for ByteWhenRefParser<F>
where
    F: Fn(&u8) -> bool,
{
    type State = ();

    fn extract(
        &self,
        mut input: ByteStream,
        _state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, ByteStream> {
        let first = input.iter().next().cloned();
        match first {
            Some(b) if (self.0)(&b) => ParseResult::Match(input.take_before(1), input),
            Some(_) => ParseResult::NoMatch,
            None if last => ParseResult::NoMatch,
            None => ParseResult::Partial(()),
        }
    }
}

// byte::when(|&u8|...).optional()
impl<F> Repeatable for ByteWhenRefParser<F> where F: Fn(&u8) -> bool {}

impl<F> ParseWhen<&u8, F> for byte
where
    F: Fn(&u8) -> bool,
{
    type Parser = ByteWhenRefParser<F>;

    fn when(f: F) -> Self::Parser {
        ByteWhenRefParser(f)
    }
}

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

pub struct CharWhenRefParser<F>(F);

// char::when(|&c| ...)
impl<F> Extract for CharWhenRefParser<F>
where
    F: Fn(&char) -> bool,
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
                    if (self.0)(&c) {
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

// char::when(|&c|...).optional()
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

trait ExtractFunction:
    Fn(ByteStream, Option<ByteStream>, bool) -> ParseResult<ByteStream, ByteStream>
{
}

impl<T> ExtractFunction for T where
    T: Fn(ByteStream, Option<ByteStream>, bool) -> ParseResult<ByteStream, ByteStream>
{
}

impl<F> Extract for F
where
    F: ExtractFunction,
{
    type State = ByteStream;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, ByteStream> {
        (self)(input, state, last)
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;
    use std::collections::VecDeque;

    use bytes::Buf;

    use crate::{byte, ExtractFunction, ExtractTuple};

    use super::{ByteStream, Collectable, Extract, ParseAny, ParseResult, ParseWhen, Repeatable};

    #[test]
    fn test_byte_literal() {
        let buffer = ByteStream::from("23");
        let input = buffer.clone();
        assert_eq!(input.remaining(), 2);
        let ParseResult::Match(output, input) = b'2'.extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), "2");
        let ParseResult::Match(output, input) = b'3'.extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), "3");
        let ParseResult::Partial(_) = b'4'.extract(input, None, false) else {
            panic!()
        };

        let input = buffer.clone();
        let ParseResult::NoMatch = b'3'.extract(input, None, false) else {
            panic!()
        };
    }

    #[test]
    fn test_byte_array_literal() {
        let buffer = ByteStream::from("hello, world!");
        let input = buffer.clone();
        let ParseResult::Match(output, input) = b"hello".extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), "hello");
        let ParseResult::Match(output, input) = b", ".extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), ", ");
        let ParseResult::NoMatch = b" everyone!".extract(input, None, false) else {
            panic!()
        };
    }

    #[test]
    fn test_byte_any() {
        let buffer = ByteStream::from("A4");
        let input = buffer.clone();
        assert_eq!(input.remaining(), 2);
        let ParseResult::Match(output, input) = byte::any().extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), "A");
        let ParseResult::Match(output, input) = byte::any().extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), "4");
        let ParseResult::Partial(_) = byte::any().extract(input, None, false) else {
            panic!()
        };
    }

    #[test]
    fn test_byte_when() {
        let buffer = ByteStream::from("A4");
        let input = buffer.clone();
        assert_eq!(input.remaining(), 2);
        let ParseResult::Match(output, input) =
            byte::when(|b: u8| u8::is_ascii_alphabetic(&b)).extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "A");
        let ParseResult::NoMatch =
            byte::when(|b: u8| u8::is_ascii_alphabetic(&b)).extract(input.clone(), None, false)
        else {
            panic!()
        };
        let ParseResult::Match(output, input) =
            byte::when(|b: u8| u8::is_ascii_digit(&b)).extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "4");
        let ParseResult::Partial(_) =
            byte::when(|b: u8| u8::is_ascii_digit(&b)).extract(input, None, false)
        else {
            panic!()
        };
    }

    #[test]
    fn test_byte_when_ref() {
        let buffer = ByteStream::from("A4");
        let input = buffer.clone();
        assert_eq!(input.remaining(), 2);
        let ParseResult::Match(output, input) =
            byte::when(u8::is_ascii_alphabetic).extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "A");
        let ParseResult::NoMatch =
            byte::when(u8::is_ascii_alphabetic).extract(input.clone(), None, false)
        else {
            panic!()
        };
        let ParseResult::Match(output, input) =
            byte::when(u8::is_ascii_digit).extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "4");
        let ParseResult::Partial(_) = byte::when(u8::is_ascii_digit).extract(input, None, false)
        else {
            panic!()
        };
    }

    #[test]
    fn test_str_literal() {
        let buffer = ByteStream::from("hello, world!");
        let input = buffer.clone();
        let ParseResult::Match(output, input) = "hello".extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), "hello");
        let ParseResult::Match(output, input) = ", ".extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), ", ");
        let ParseResult::NoMatch = " everyone!".extract(input, None, false) else {
            panic!()
        };
    }

    #[test]
    fn test_1_byte_char() {
        let buffer = ByteStream::from("23");
        let input = buffer.clone();
        assert_eq!(input.remaining(), 2);
        let ParseResult::Match(output, input) = '2'.extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), "2");
        assert_eq!(input.iter().next(), Some(&b'3'));
        let ParseResult::NoMatch = '2'.extract(input, None, false) else {
            panic!()
        };
    }

    #[test]
    fn test_multibyte_char() {
        let buffer = ByteStream::from("€4");
        let input = buffer.clone();
        assert_eq!(input.remaining(), 4);
        let ParseResult::Match(output, input) = '€'.extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), "€");
        assert_eq!(input.iter().next(), Some(&b'4'));
        let ParseResult::NoMatch = '€'.extract(input, None, false) else {
            panic!()
        };

        let input = buffer.clone();
        let ParseResult::Match(output, input) = char::any().extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), "€");
        let ParseResult::Match(output, input) = char::any().extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), "4");
        let ParseResult::Partial(_) = char::any().extract(input, None, false) else {
            panic!()
        };

        let input = buffer.clone();
        let ParseResult::NoMatch =
            char::when(char::is_alphabetic).extract(input.clone(), None, false)
        else {
            panic!()
        };
        let ParseResult::Match(output, input) =
            char::when(|c: char| !c.is_alphabetic()).extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "€");
        let ParseResult::Match(output, input) =
            char::when(|c| char::is_digit(c, 10)).extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "4");
        let ParseResult::Partial(_) = char::when(char::is_alphabetic).extract(input, None, false)
        else {
            panic!()
        };

        let input = buffer.clone();
        let ParseResult::NoMatch =
            char::when(char::is_ascii_alphabetic).extract(input.clone(), None, false)
        else {
            panic!()
        };
        // closure with ref arg does not get inferred for<'a> lifetime if placed in when arg
        let not_alphabetic = |c: &char| -> bool { !c.is_ascii_alphabetic() };
        let ParseResult::Match(output, input) =
            char::when(not_alphabetic).extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "€");
        let ParseResult::Match(output, input) =
            char::when(char::is_ascii_digit).extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "4");
        let ParseResult::Partial(_) = char::when(char::is_ascii_digit).extract(input, None, false)
        else {
            panic!()
        };
    }

    #[test]
    fn test_multibyte_char_byte_by_byte() {
        let mut buffer = ByteStream::from("€4");
        let input = buffer.take_before(1);
        let ParseResult::Partial(state) = '€'.extract(input, None, false) else {
            panic!()
        };
        let input = buffer.take_before(1);
        let ParseResult::Partial(state) = '€'.extract(input, Some(state), false) else {
            panic!()
        };
        let input = buffer;
        let ParseResult::Match(output, input) = '€'.extract(input, Some(state), true) else {
            panic!()
        };
        assert_eq!(output.to_string(), "€");
        assert_eq!(input.iter().next(), Some(&b'4'));
        let ParseResult::NoMatch = '€'.extract(input, None, false) else {
            panic!()
        };
    }

    // A 3-byte symbol that matches until the last byte
    #[test]
    fn test_multibyte_char_nearly() {
        let mut buffer = ByteStream::from("€4");
        let input = buffer.take_before(1);
        let ParseResult::Partial(state) = '₭'.extract(input, None, false) else {
            panic!()
        };
        let input = buffer.take_before(1);
        let ParseResult::Partial(state) = '₭'.extract(input, Some(state), false) else {
            panic!()
        };
        let input = buffer;
        let output = '₭'.extract(input, Some(state), true);
        assert_matches!(output, ParseResult::NoMatch);
    }

    #[test]
    fn test_optional() {
        let input = ByteStream::from("helloworld");
        let ParseResult::Match(output, input) = "hello".optional().extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "hello");
        let ParseResult::Match(output, input) = ", ".optional().extract(input, None, false) else {
            panic!()
        };
        assert!(output.is_empty());
        let ParseResult::Match(output, input) = "world".optional().extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "world");
        let ParseResult::Partial(_) = "world".optional().extract(input.clone(), None, false) else {
            panic!()
        };
        let ParseResult::Match(output, input) = "world".optional().extract(input, None, true)
        else {
            panic!()
        };
        assert!(output.is_empty());
        assert!(input.is_empty());
    }

    #[test]
    fn test_times() {
        let input = ByteStream::from("hellohellohello");
        let ParseResult::Match(output, input) = "hello".times(3).extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.len(), 3);
        for out in output {
            assert_eq!(out.to_string(), "hello");
        }
        assert!(input.is_empty());
    }

    #[test]
    fn test_repeated() {
        let input = ByteStream::from("hellohellohello");
        let ParseResult::Match(output, input) = "hello".repeated(1..3).extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.len(), 2);
        for out in output {
            assert_eq!(out.to_string(), "hello");
        }
        assert_eq!(input.to_string(), "hello");

        let parser = "hello".repeated(1..10);
        let input = ByteStream::from("hellohellohellohe");
        let ParseResult::Partial(state) = parser.extract(input, None, false) else {
            panic!()
        };
        let input = ByteStream::from("llohelp");
        let ParseResult::Match(output, input) = parser.extract(input, Some(state), true) else {
            panic!()
        };
        assert_eq!(output.len(), 4);
        for out in output {
            assert_eq!(out.to_string(), "hello");
        }
        assert_eq!(input.to_string(), "help");

        let input = ByteStream::from("hellohellohellohe");
        let ParseResult::Partial(state) = parser.extract(input, None, false) else {
            panic!()
        };
        let ParseResult::Match(output, input) =
            parser.extract(ByteStream::default(), Some(state), true)
        else {
            panic!()
        };
        assert_eq!(output.len(), 3);
        for out in output {
            assert_eq!(out.to_string(), "hello");
        }
        assert_eq!(input.to_string(), "he");

        let input = ByteStream::from("helo");
        let ParseResult::Match(output, input) = "hello".repeated(0..2).extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.len(), 0);
        for out in output {
            assert_eq!(out.to_string(), "hello");
        }
        assert_eq!(input.to_string(), "helo");
    }

    fn reverse(
        input: ByteStream,
        _state: Option<ByteStream>,
        _last: bool,
    ) -> ParseResult<ByteStream, ByteStream> {
        let mut d = VecDeque::new();
        for b in input.iter().cloned() {
            d.push_front(b);
        }
        let v = d.into_iter().collect::<Vec<_>>();
        let output = ByteStream::from(bytes::Bytes::from(v));
        ParseResult::Match(output, ByteStream::default())
    }

    fn make_reverse(i: usize) -> impl ExtractFunction {
        move |mut bs, state, last| {
            let bs1 = bs.take_before(i);
            reverse(bs1, state, last)
        }
    }

    #[test]
    fn test_function() {
        let input = ByteStream::from("helloworld");
        let ParseResult::Match(output, _) = reverse.extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), "dlrowolleh");

        let input = ByteStream::from("helloworld");
        let ParseResult::Match(output, _) = make_reverse(6).extract(input, None, false) else {
            panic!()
        };
        assert_eq!(output.to_string(), "wolleh");
    }

    #[test]
    fn test_sequence() {
        let input = ByteStream::from("hello3a");
        let ParseResult::Match([out1, out2], input) =
            ("hello", char::when(|c: char| c.is_digit(10)))
                .seq()
                .extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(out1.to_string(), "hello");
        assert_eq!(out2.to_string(), "3");
        assert_eq!(input.to_string(), "a");

        let mut buffer = ByteStream::from("hello3a");
        let input = buffer.take_before(3);
        let ParseResult::Partial(state) = ("hello", char::when(char::is_alphabetic))
            .seq()
            .extract(input, None, false)
        else {
            panic!()
        };
        let ParseResult::NoMatch =
            ("hello", char::when(char::is_alphabetic))
                .seq()
                .extract(buffer, Some(state), true)
        else {
            panic!()
        };
    }

    #[test]
    fn test_optional_sequence() {
        let input = ByteStream::from("hello, world!");
        let ParseResult::Match([out1, out2], input) =
            ("hello", char::when(|c: char| c.is_digit(10)))
                .seq()
                .optional()
                .extract(input, None, false)
        else {
            panic!()
        };
        assert!(out1.is_empty());
        assert!(out2.is_empty());
        assert_eq!(input.to_string(), "hello, world!");

        let ParseResult::Match([out1, out2], input) = ("hello, ", "world!")
            .seq()
            .optional()
            .extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(out1.to_string(), "hello, ");
        assert_eq!(out2.to_string(), "world!");
        assert!(input.is_empty());
    }

    #[test]
    fn test_any() {
        let input = ByteStream::from("hello3a");
        let ParseResult::Match(output, input) = ("hello", char::when(|c: char| c.is_digit(10)))
            .any()
            .extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "hello");
        let ParseResult::Match(output, input) = ("hello", char::when(|c: char| c.is_digit(10)))
            .any()
            .extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "3");
        assert_eq!(input.to_string(), "a");
    }

    #[test]
    fn test_first() {
        let input = ByteStream::from("hello3a");
        let ParseResult::Match(output, input) = ("hello", char::when(|c: char| c.is_digit(10)))
            .first()
            .extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "hello");
        let ParseResult::Match(output, input) = ("hello", char::when(|c: char| c.is_digit(10)))
            .first()
            .extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "3");
        assert_eq!(input.to_string(), "a");
    }

    // any() finds the matching alphabetic character, even though "hello" might
    // eventually match.
    #[test]
    fn test_precedence_any() {
        let mut buffer = ByteStream::from("hello3a");
        let input = buffer.take_before(3);
        let ParseResult::Match(output, input) = ("hello", char::when(char::is_alphabetic))
            .any()
            .extract(input, None, false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "h");
        assert_eq!(input.to_string(), "el");
    }

    // first() finds "hello" because it is first in the tuple even though the alphabetic character
    // pattern matches earlier.
    #[test]
    fn test_precedence_first() {
        let mut buffer = ByteStream::from("hello3a");
        let input = buffer.take_before(3);
        let ParseResult::Partial(state) = ("hello", char::when(char::is_alphabetic))
            .first()
            .extract(input, None, false)
        else {
            panic!()
        };
        let ParseResult::Match(output, input) = ("hello", char::when(char::is_alphabetic))
            .first()
            .extract(buffer, Some(state), true)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "hello");
        assert_eq!(input.to_string(), "3a");
    }

    #[test]
    fn json_number() {
        fn is_digit(c: char) -> bool {
            c.is_ascii_digit()
        }
        let json_number_parser = (
            '-'.optional(),
            (
                '0',
                (
                    ('1', '2', '3', '4', '5', '6', '7', '8', '9').any(),
                    char::when(is_digit).repeated(..).collect(),
                )
                    .seq()
                    .collect(),
            )
                .any(),
            ('.', char::when(is_digit).repeated(..).collect())
                .seq()
                .optional()
                .collect(),
            (
                ('e', 'E').any(),
                ('-', '+').any().optional(),
                char::when(is_digit).repeated(..).collect(),
            )
                .seq()
                .optional()
                .collect(),
        )
            .seq()
            .collect::<ByteStream>();

        let input = ByteStream::from("-123");
        let ParseResult::Partial(state) = json_number_parser.extract(input, None, false) else {
            panic!();
        };
        let ParseResult::Match(output, input) =
            json_number_parser.extract(ByteStream::default(), Some(state), true)
        else {
            panic!();
        };
        assert_eq!(output.to_string(), "-123");
        assert!(input.is_empty());

        let ParseResult::Match(output, input) =
            json_number_parser.extract(ByteStream::from("3.14159,"), None, true)
        else {
            panic!();
        };
        assert_eq!(output.to_string(), "3.14159");
        assert_eq!(input.to_string(), ",");

        let ParseResult::Match(output, input) =
            json_number_parser.extract(ByteStream::from("4.2e+06"), None, true)
        else {
            panic!();
        };
        assert_eq!(output.to_string(), "4.2e+06");
        assert!(input.is_empty());

        let ParseResult::Match(output, input) =
            json_number_parser.extract(ByteStream::from("09.2"), None, true)
        else {
            panic!();
        };
        assert_eq!(output.to_string(), "0");
        assert_eq!(input.to_string(), "9.2");
    }

    #[test]
    fn json_string() {
        let json_string_parser = (
            '"',
            (
                (
                    '\\',
                    (
                        '"',
                        '\\',
                        '/',
                        'b',
                        'f',
                        'n',
                        'r',
                        't',
                        ('u', char::when(char::is_ascii_hexdigit).times(4).collect())
                            .seq()
                            .collect(),
                    )
                        .any(),
                )
                    .seq()
                    .collect(),
                char::when(|c| c != '"' && c != '\\'),
            )
                .any()
                .repeated(..)
                .collect(),
            '"',
        )
            .seq()
            .collect::<ByteStream>();

        let ParseResult::Match(output, input) =
            json_string_parser.extract(ByteStream::from(r#""hello, world!","#), None, true)
        else {
            panic!();
        };
        assert_eq!(output.to_string(), r#""hello, world!""#);
        assert_eq!(input.to_string(), ",");

        let ParseResult::Match(output, input) =
            json_string_parser.extract(ByteStream::from(r#""""#), None, true)
        else {
            panic!();
        };
        assert_eq!(output.to_string(), r#""""#);
        assert!(input.is_empty());

        let ParseResult::Match(output, input) =
            json_string_parser.extract(ByteStream::from(r#""hello, \"!","#), None, true)
        else {
            panic!();
        };
        assert_eq!(output.to_string(), r#""hello, \"!""#);
        assert_eq!(input.to_string(), ",");

        let ParseResult::NoMatch =
            json_string_parser.extract(ByteStream::from(r#""hello, \ufour!","#), None, true)
        else {
            panic!();
        };

        let ParseResult::Match(output, input) =
            json_string_parser.extract(ByteStream::from(r#""hello, \uAbCd\t""#), None, true)
        else {
            panic!();
        };
        assert_eq!(output.to_string(), r#""hello, \uAbCd\t""#);
        assert!(input.is_empty());
    }
}
