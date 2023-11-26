use bytes::Buf as _;

use crate::repeatable::Repeatable;
use crate::{ByteStream, Extract, ParseAny, ParseResult, ParseWhen};

// b'a'
impl Extract for u8 {
    type State = ();
    type Output = ByteStream;

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
    type Output = ByteStream;

    fn extract(
        &self,
        mut input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, ByteStream> {
        let mut bytes = self.as_slice();
        let (seen, mut output) = state.unwrap_or((0, ByteStream::new(input.position())));
        bytes.advance(seen);
        let matched = input.common_prefix_length(bytes);
        if matched == bytes.len() {
            output.merge(input.take_before(matched));
            ParseResult::Match(output, input)
        } else if !last && matched == input.remaining() {
            output.merge(input.take_before(matched));
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
    type Output = ByteStream;

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
    type Output = ByteStream;

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
