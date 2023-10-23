#![feature(assert_matches)]
#![feature(associated_type_defaults)]
#![feature(str_internals)]

use core::str::utf8_char_width;

use byte_stream::ByteStream;
use bytes::Buf;

mod byte_stream;

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

#[allow(dead_code)]
impl<InnerParser> OptionalParser<InnerParser> {
    fn new(inner: InnerParser) -> Self {
        Self { inner }
    }
}

impl<InnerParser> Extract for OptionalParser<InnerParser>
where
    InnerParser: Extract<Output = ByteStream>,
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
                ParseResult::Match(ByteStream::default())
            }
            ParseResult::Partial(inner_state) => ParseResult::Partial((inner_state, saved)),
            ParseResult::Match(output) => ParseResult::Match(output),
        }
    }
}

trait Extract {
    type State;
    type Output = ByteStream;

    fn extract(
        &self,
        input: &mut ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Self::Output>;
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
        let (seen, mut output) = state.unwrap_or((0, ByteStream::default()));
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
        let (mut seen, mut output) = state.unwrap_or((0, ByteStream::default()));
        bytes.advance(seen as usize);
        let matched = input.common_prefix_length(bytes);
        if matched == bytes.len() {
            output.extend(input.take_before(matched));
            ParseResult::Match(output)
        } else if matched == input.remaining() {
            output.extend(input.take_before(matched));
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
        let (mut seen, mut output) = state.unwrap_or((0, ByteStream::default()));
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
            let len = output.fill_slice(&mut bytes);
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
            let len = output.fill_slice(&mut bytes);
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

trait ExtractFunction:
    Fn(&mut ByteStream, Option<ByteStream>) -> ParseResult<ByteStream, ByteStream>
{
}

impl<T> ExtractFunction for T where
    T: Fn(&mut ByteStream, Option<ByteStream>) -> ParseResult<ByteStream, ByteStream>
{
}

impl<F> Extract for F
where
    F: ExtractFunction,
{
    type State = ByteStream;

    fn extract(
        &self,
        input: &mut ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        (self)(input, state)
    }
}

enum TupleState2<E0, E1>
where
    E0: Extract<Output = ByteStream>,
    E1: Extract<Output = ByteStream>,
{
    S0(Option<E0::State>),
    S1(Option<E1::State>, (ByteStream,)),
    Done((ByteStream, ByteStream)),
}

struct TupleSequence2<E0, E1> {
    tuple: (E0, E1),
}

impl<E0, E1> Extract for TupleSequence2<E0, E1>
where
    E0: Extract<Output = ByteStream>,
    E1: Extract<Output = ByteStream>,
{
    type State = TupleState2<E0, E1>;
    type Output = (ByteStream, ByteStream);

    fn extract(
        &self,
        input: &mut ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Self::Output> {
        let mut state = state.unwrap_or(TupleState2::S0(None));
        loop {
            state = match state {
                TupleState2::S0(inner_state) => match self.tuple.0.extract(input, inner_state) {
                    ParseResult::NoMatch => {
                        return ParseResult::NoMatch;
                    }
                    ParseResult::Partial(inner_state) => {
                        return ParseResult::Partial(TupleState2::S0(Some(inner_state)));
                    }
                    ParseResult::Match(output) => TupleState2::S1(None, (output,)),
                },
                TupleState2::S1(inner_state, out) => {
                    match self.tuple.1.extract(input, inner_state) {
                        ParseResult::NoMatch => {
                            return ParseResult::NoMatch;
                        }
                        ParseResult::Partial(inner_state) => {
                            return ParseResult::Partial(TupleState2::S1(Some(inner_state), out));
                        }
                        ParseResult::Match(output) => TupleState2::Done((out.0, output)),
                    }
                }
                TupleState2::Done(output) => {
                    return ParseResult::Match(output);
                }
            }
        }
    }
}

trait ExtractTuple {
    type TupleSequence;

    fn seq(self) -> Self::TupleSequence;
}

impl<E0, E1> ExtractTuple for (E0, E1)
where
    E0: Extract,
    E1: Extract,
{
    type TupleSequence = TupleSequence2<E0, E1>;

    fn seq(self) -> Self::TupleSequence {
        TupleSequence2 { tuple: self }
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;
    use std::collections::VecDeque;

    use bytes::Buf;

    use crate::{ExtractFunction, ExtractTuple};

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

    fn reverse(
        input: &mut ByteStream,
        _state: Option<ByteStream>,
    ) -> ParseResult<ByteStream, ByteStream> {
        let mut d = VecDeque::new();
        for b in input.iter().cloned() {
            d.push_front(b);
        }
        let v = d.into_iter().collect::<Vec<_>>();
        let output = ByteStream::from(bytes::Bytes::from(v));
        ParseResult::Match(output)
    }

    fn make_reverse(i: usize) -> impl ExtractFunction {
        move |bs, state| {
            let mut bs1 = bs.take_before(i);
            reverse(&mut bs1, state)
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

    #[test]
    fn test_sequence() {
        let mut input = ByteStream::from("hello3a");
        let res = ("hello", char::when(|c: char| c.is_digit(10)))
            .seq()
            .extract(&mut input, None);
        assert_matches!(res, ParseResult::Match((out1, out2)) if out1.to_string() == "hello" && out2.to_string() == "3");

        let mut buffer = ByteStream::from("hello3a");
        let mut input = buffer.take_before(3);
        let res = ("hello", char::when(char::is_alphabetic))
            .seq()
            .extract(&mut input, None);
        let ParseResult::Partial(state) = res else {
            panic!("{res:?}");
        };
        let res = ("hello", char::when(char::is_alphabetic))
            .seq()
            .extract(&mut buffer, Some(state));
        assert_matches!(res, ParseResult::NoMatch);
    }
}
