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
    InnerParser: Extract,
    InnerParser::Output: Default,
{
    type State = (Option<InnerParser::State>, ByteStream);
    type Output = InnerParser::Output;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, InnerParser::Output> {
        let (inner_state, mut saved) = state.unwrap_or((None, ByteStream::default()));
        saved.append(&input);
        match self.inner.extract(input, inner_state) {
            ParseResult::NoMatch => ParseResult::Match(Self::Output::default(), saved),
            ParseResult::Partial(inner_state) => ParseResult::Partial((Some(inner_state), saved)),
            ParseResult::Match(output, input) => ParseResult::Match(output, input),
        }
    }
}

trait Extract {
    type State;
    type Output = ByteStream;

    fn extract(
        &self,
        input: ByteStream,
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
        mut input: ByteStream,
        _state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        if input.is_empty() {
            ParseResult::Partial(())
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

impl Repeatable for u8 {}

impl<const N: usize> Extract for [u8; N] {
    type State = (usize, ByteStream);

    fn extract(
        &self,
        mut input: ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        let mut bytes = self.as_slice();
        let (seen, mut output) = state.unwrap_or((0, ByteStream::default()));
        bytes.advance(seen);
        let matched = input.common_prefix_length(bytes);
        if matched == bytes.len() {
            output.extend(input.take_before(matched));
            ParseResult::Match(output, input)
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
        mut input: ByteStream,
        _state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        if input.has_remaining() {
            ParseResult::Match(input.take_before(1), input)
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
        mut input: ByteStream,
        _state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        let first = input.iter().next().cloned();
        match first {
            Some(b) if (self.0)(b) => ParseResult::Match(input.take_before(1), input),
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
        mut input: ByteStream,
        _state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        let first = input.iter().next().cloned();
        match first {
            Some(b) if (self.0)(&b) => ParseResult::Match(input.take_before(1), input),
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
        mut input: ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        let mut char_buf = [0u8; 4];
        let mut bytes = self.encode_utf8(&mut char_buf).as_bytes();
        let (mut seen, mut output) = state.unwrap_or((0, ByteStream::default()));
        bytes.advance(seen as usize);
        let matched = input.common_prefix_length(bytes);
        if matched == bytes.len() {
            output.extend(input.take_before(matched));
            ParseResult::Match(output, input)
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
        mut input: ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, ByteStream> {
        let mut bytes = self.as_bytes();
        let (mut seen, mut output) = state.unwrap_or((0, ByteStream::default()));
        bytes.advance(seen);
        let matched = input.common_prefix_length(bytes);
        if matched == bytes.len() {
            output.extend(input.take_before(matched));
            ParseResult::Match(output, input)
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
        mut input: ByteStream,
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
            output.extend(input.take_before(input_len));
            ParseResult::Partial((required, output))
        } else {
            output.extend(input.take_before(required));
            ParseResult::Match(output, input)
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
        mut input: ByteStream,
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
            output.extend(input.take_before(input_len));
            ParseResult::Partial((required - input_len, output))
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
        mut input: ByteStream,
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
            output.extend(input.take_before(input_len));
            ParseResult::Partial((required - input_len, output))
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
    Fn(ByteStream, Option<ByteStream>) -> ParseResult<ByteStream, ByteStream>
{
}

impl<T> ExtractFunction for T where
    T: Fn(ByteStream, Option<ByteStream>) -> ParseResult<ByteStream, ByteStream>
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
    ) -> ParseResult<Self::State, ByteStream> {
        (self)(input, state)
    }
}

struct TupleAny2<E0, E1>
where
    E0: Extract,
    E1: Extract,
{
    tuple: (E0, E1),
}

impl<E0, E1> Extract for TupleAny2<E0, E1>
where
    E0: Extract<Output = ByteStream>,
    E1: Extract<Output = ByteStream>,
{
    type State = (Option<Option<E0::State>>, Option<Option<E1::State>>);
    type Output = ByteStream;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Self::Output> {
        let mut exhausted = true;
        let mut state = state.unwrap_or((Some(None), Some(None)));
        if let Some(inner_state) = &mut state.0 {
            let input1 = input.clone();
            match self.tuple.0.extract(input1, inner_state.take()) {
                ParseResult::NoMatch => {
                    state.0 = None;
                }
                ParseResult::Partial(new_state) => {
                    *inner_state = Some(new_state);
                    exhausted = false;
                }
                ParseResult::Match(output, input) => {
                    return ParseResult::Match(output, input);
                }
            }
        }
        if let Some(inner_state) = &mut state.1 {
            let input1 = input.clone();
            match self.tuple.1.extract(input1, inner_state.take()) {
                ParseResult::NoMatch => state.1 = None,
                ParseResult::Partial(new_state) => {
                    *inner_state = Some(new_state);
                    exhausted = false;
                }
                ParseResult::Match(output, input) => {
                    return ParseResult::Match(output, input);
                }
            }
        }
        if exhausted {
            return ParseResult::NoMatch;
        } else {
            return ParseResult::Partial(state);
        }
    }
}

struct TupleFirst2<E0, E1>
where
    E0: Extract,
    E1: Extract,
{
    tuple: (E0, E1),
}

impl<E0, E1> Extract for TupleFirst2<E0, E1>
where
    E0: Extract<Output = ByteStream>,
    E1: Extract<Output = ByteStream>,
{
    type State = (
        Option<ParseResult<E0::State, ByteStream>>,
        Option<ParseResult<E1::State, ByteStream>>,
    );
    type Output = ByteStream;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Self::Output> {
        let mut first = true;
        let mut state = state.unwrap_or((None, None));
        state.0 = match state.0.take() {
            Some(ParseResult::NoMatch) => Some(ParseResult::NoMatch),
            Some(ParseResult::Partial(inner_state)) => {
                let res = self.tuple.0.extract(input.clone(), Some(inner_state));
                match res {
                    ParseResult::NoMatch => Some(ParseResult::NoMatch),
                    ParseResult::Partial(state) => {
                        first = false;
                        Some(ParseResult::Partial(state))
                    }
                    ParseResult::Match(output, input) => {
                        if first {
                            return ParseResult::Match(output, input);
                        }
                        Some(ParseResult::Match(output, input))
                    }
                }
            }
            Some(ParseResult::Match(output, input)) => {
                if first {
                    return ParseResult::Match(output, input);
                }
                Some(ParseResult::Match(output, input))
            }
            None => {
                let res = self.tuple.0.extract(input.clone(), None);
                match res {
                    ParseResult::NoMatch => Some(ParseResult::NoMatch),
                    ParseResult::Partial(state) => {
                        first = false;
                        Some(ParseResult::Partial(state))
                    }
                    ParseResult::Match(output, input) => {
                        if first {
                            return ParseResult::Match(output, input);
                        }
                        Some(ParseResult::Match(output, input))
                    }
                }
            }
        };
        state.1 = match state.1.take() {
            Some(ParseResult::NoMatch) => Some(ParseResult::NoMatch),
            Some(ParseResult::Partial(inner_state)) => {
                let res = self.tuple.1.extract(input.clone(), Some(inner_state));
                match res {
                    ParseResult::NoMatch => Some(ParseResult::NoMatch),
                    ParseResult::Partial(state) => {
                        first = false;
                        Some(ParseResult::Partial(state))
                    }
                    ParseResult::Match(output, input) => {
                        if first {
                            return ParseResult::Match(output, input);
                        }
                        Some(ParseResult::Match(output, input))
                    }
                }
            }
            Some(ParseResult::Match(output, input)) => {
                if first {
                    return ParseResult::Match(output, input);
                }
                Some(ParseResult::Match(output, input))
            }
            None => {
                let res = self.tuple.1.extract(input.clone(), None);
                match res {
                    ParseResult::NoMatch => Some(ParseResult::NoMatch),
                    ParseResult::Partial(state) => {
                        first = false;
                        Some(ParseResult::Partial(state))
                    }
                    ParseResult::Match(output, input) => {
                        if first {
                            return ParseResult::Match(output, input);
                        }
                        Some(ParseResult::Match(output, input))
                    }
                }
            }
        };
        if first {
            return ParseResult::NoMatch;
        } else {
            return ParseResult::Partial(state);
        }
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

impl<E0, E1> Repeatable for TupleSequence2<E0, E1>
where
    E0: Extract<Output = ByteStream>,
    E1: Extract<Output = ByteStream>,
{
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
        mut input: ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Self::Output> {
        let mut state = state.unwrap_or(TupleState2::S0(None));
        loop {
            (state, input) = match state {
                TupleState2::S0(inner_state) => match self.tuple.0.extract(input, inner_state) {
                    ParseResult::NoMatch => {
                        return ParseResult::NoMatch;
                    }
                    ParseResult::Partial(inner_state) => {
                        return ParseResult::Partial(TupleState2::S0(Some(inner_state)));
                    }
                    ParseResult::Match(output, input) => (TupleState2::S1(None, (output,)), input),
                },
                TupleState2::S1(inner_state, out) => {
                    match self.tuple.1.extract(input, inner_state) {
                        ParseResult::NoMatch => {
                            return ParseResult::NoMatch;
                        }
                        ParseResult::Partial(inner_state) => {
                            return ParseResult::Partial(TupleState2::S1(Some(inner_state), out));
                        }
                        ParseResult::Match(output, input) => {
                            (TupleState2::Done((out.0, output)), input)
                        }
                    }
                }
                TupleState2::Done(output) => {
                    return ParseResult::Match(output, input);
                }
            }
        }
    }
}

trait ExtractTuple {
    type TupleAny;
    type TupleFirst;
    type TupleSequence;

    fn any(self) -> Self::TupleAny;

    fn first(self) -> Self::TupleFirst;

    fn seq(self) -> Self::TupleSequence;
}

impl<E0, E1> ExtractTuple for (E0, E1)
where
    E0: Extract,
    E1: Extract,
{
    type TupleAny = TupleAny2<E0, E1>;
    type TupleFirst = TupleFirst2<E0, E1>;
    type TupleSequence = TupleSequence2<E0, E1>;

    fn any(self) -> Self::TupleAny {
        TupleAny2 { tuple: self }
    }

    fn first(self) -> Self::TupleFirst {
        TupleFirst2 { tuple: self }
    }

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
        let input = buffer.clone();
        assert_eq!(input.remaining(), 2);
        let ParseResult::Match(output, input) = b'2'.extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "2");
        let ParseResult::Match(output, input) = b'3'.extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "3");
        let ParseResult::Partial(_) = b'4'.extract(input, None) else {
            panic!()
        };

        let input = buffer.clone();
        let ParseResult::NoMatch = b'3'.extract(input, None) else {
            panic!()
        };
    }

    #[test]
    fn test_byte_array_literal() {
        let buffer = ByteStream::from("hello, world!");
        let input = buffer.clone();
        let ParseResult::Match(output, input) = b"hello".extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "hello");
        let ParseResult::Match(output, input) = b", ".extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), ", ");
        let ParseResult::NoMatch = b" everyone!".extract(input, None) else {
            panic!()
        };
    }

    #[test]
    fn test_byte_any() {
        let buffer = ByteStream::from("A4");
        let input = buffer.clone();
        assert_eq!(input.remaining(), 2);
        let ParseResult::Match(output, input) = u8::any().extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "A");
        let ParseResult::Match(output, input) = u8::any().extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "4");
        let ParseResult::Partial(_) = u8::any().extract(input, None) else {
            panic!()
        };
    }

    #[test]
    fn test_byte_when() {
        let buffer = ByteStream::from("A4");
        let input = buffer.clone();
        assert_eq!(input.remaining(), 2);
        let ParseResult::Match(output, input) =
            u8::when(|b: u8| u8::is_ascii_alphabetic(&b)).extract(input, None)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "A");
        let ParseResult::NoMatch =
            u8::when(|b: u8| u8::is_ascii_alphabetic(&b)).extract(input.clone(), None)
        else {
            panic!()
        };
        let ParseResult::Match(output, input) =
            u8::when(|b: u8| u8::is_ascii_digit(&b)).extract(input, None)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "4");
        let ParseResult::Partial(_) = u8::when(|b: u8| u8::is_ascii_digit(&b)).extract(input, None)
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
            u8::when(u8::is_ascii_alphabetic).extract(input, None)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "A");
        let ParseResult::NoMatch = u8::when(u8::is_ascii_alphabetic).extract(input.clone(), None)
        else {
            panic!()
        };
        let ParseResult::Match(output, input) = u8::when(u8::is_ascii_digit).extract(input, None)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "4");
        let ParseResult::Partial(_) = u8::when(u8::is_ascii_digit).extract(input, None) else {
            panic!()
        };
    }

    #[test]
    fn test_str_literal() {
        let buffer = ByteStream::from("hello, world!");
        let input = buffer.clone();
        let ParseResult::Match(output, input) = "hello".extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "hello");
        let ParseResult::Match(output, input) = ", ".extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), ", ");
        let ParseResult::NoMatch = " everyone!".extract(input, None) else {
            panic!()
        };
    }

    #[test]
    fn test_1_byte_char() {
        let buffer = ByteStream::from("23");
        let input = buffer.clone();
        assert_eq!(input.remaining(), 2);
        let ParseResult::Match(output, input) = '2'.extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "2");
        assert_eq!(input.iter().next(), Some(&b'3'));
        let ParseResult::NoMatch = '2'.extract(input, None) else {
            panic!()
        };
    }

    #[test]
    fn test_multibyte_char() {
        let buffer = ByteStream::from("€4");
        let input = buffer.clone();
        assert_eq!(input.remaining(), 4);
        let ParseResult::Match(output, input) = '€'.extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "€");
        assert_eq!(input.iter().next(), Some(&b'4'));
        let ParseResult::NoMatch = '€'.extract(input, None) else {
            panic!()
        };

        let input = buffer.clone();
        let ParseResult::Match(output, input) = char::any().extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "€");
        let ParseResult::Match(output, input) = char::any().extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "4");
        let ParseResult::Partial(_) = char::any().extract(input, None) else {
            panic!()
        };

        let input = buffer.clone();
        let ParseResult::NoMatch = char::when(char::is_alphabetic).extract(input.clone(), None)
        else {
            panic!()
        };
        let ParseResult::Match(output, input) =
            char::when(|c: char| !c.is_alphabetic()).extract(input, None)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "€");
        let ParseResult::Match(output, input) =
            char::when(|c| char::is_digit(c, 10)).extract(input, None)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "4");
        let ParseResult::Partial(_) = char::when(char::is_alphabetic).extract(input, None) else {
            panic!()
        };

        let input = buffer.clone();
        let ParseResult::NoMatch =
            char::when(char::is_ascii_alphabetic).extract(input.clone(), None)
        else {
            panic!()
        };
        // closure with ref arg does not get inferred for<'a> lifetime if placed in when arg
        let not_alphabetic = |c: &char| -> bool { !c.is_ascii_alphabetic() };
        let ParseResult::Match(output, input) = char::when(not_alphabetic).extract(input, None)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "€");
        let ParseResult::Match(output, input) =
            char::when(char::is_ascii_digit).extract(input, None)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "4");
        let ParseResult::Partial(_) = char::when(char::is_ascii_digit).extract(input, None) else {
            panic!()
        };
    }

    #[test]
    fn test_multibyte_char_byte_by_byte() {
        let mut buffer = ByteStream::from("€4");
        let input = buffer.take_before(1);
        let ParseResult::Partial(state) = '€'.extract(input, None) else {
            panic!()
        };
        let input = buffer.take_before(1);
        let ParseResult::Partial(state) = '€'.extract(input, Some(state)) else {
            panic!()
        };
        let input = buffer;
        let ParseResult::Match(output, input) = '€'.extract(input, Some(state)) else {
            panic!()
        };
        assert_eq!(output.to_string(), "€");
        assert_eq!(input.iter().next(), Some(&b'4'));
        let ParseResult::NoMatch = '€'.extract(input, None) else {
            panic!()
        };
    }

    // A 3-byte symbol that matches until the last byte
    #[test]
    fn test_multibyte_char_nearly() {
        let mut buffer = ByteStream::from("€4");
        let input = buffer.take_before(1);
        let ParseResult::Partial(state) = '₭'.extract(input, None) else {
            panic!()
        };
        let input = buffer.take_before(1);
        let ParseResult::Partial(state) = '₭'.extract(input, Some(state)) else {
            panic!()
        };
        let input = buffer;
        let output = '₭'.extract(input, Some(state));
        assert_matches!(output, ParseResult::NoMatch);
    }

    #[test]
    fn test_optional() {
        let input = ByteStream::from("helloworld");
        let ParseResult::Match(output, input) = "hello".optional().extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "hello");
        let ParseResult::Match(output, input) = ", ".optional().extract(input, None) else {
            panic!()
        };
        assert!(output.is_empty());
        let ParseResult::Match(output, input) = "world".optional().extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "world");
        let ParseResult::Partial(_) = "world".optional().extract(input, None) else {
            panic!()
        };
    }

    fn reverse(
        input: ByteStream,
        _state: Option<ByteStream>,
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
        move |mut bs, state| {
            let bs1 = bs.take_before(i);
            reverse(bs1, state)
        }
    }

    #[test]
    fn test_function() {
        let input = ByteStream::from("helloworld");
        let ParseResult::Match(output, _) = reverse.extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "dlrowolleh");

        let input = ByteStream::from("helloworld");
        let ParseResult::Match(output, _) = make_reverse(6).extract(input, None) else {
            panic!()
        };
        assert_eq!(output.to_string(), "wolleh");
    }

    #[test]
    fn test_sequence() {
        let input = ByteStream::from("hello3a");
        let ParseResult::Match((out1, out2), input) =
            ("hello", char::when(|c: char| c.is_digit(10)))
                .seq()
                .extract(input, None)
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
            .extract(input, None)
        else {
            panic!()
        };
        let ParseResult::NoMatch = ("hello", char::when(char::is_alphabetic))
            .seq()
            .extract(buffer, Some(state))
        else {
            panic!()
        };
    }

    #[test]
    fn test_optional_sequence() {
        let input = ByteStream::from("hello, world!");
        let ParseResult::Match((out1, out2), input) =
            ("hello", char::when(|c: char| c.is_digit(10)))
                .seq()
                .optional()
                .extract(input, None)
        else {
            panic!()
        };
        assert!(out1.is_empty());
        assert!(out2.is_empty());
        assert_eq!(input.to_string(), "hello, world!");

        let ParseResult::Match((out1, out2), input) =
            ("hello, ", "world!").seq().optional().extract(input, None)
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
            .extract(input, None)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "hello");
        let ParseResult::Match(output, input) = ("hello", char::when(|c: char| c.is_digit(10)))
            .any()
            .extract(input, None)
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
            .extract(input, None)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "hello");
        let ParseResult::Match(output, input) = ("hello", char::when(|c: char| c.is_digit(10)))
            .first()
            .extract(input, None)
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
            .extract(input, None)
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
            .extract(input, None)
        else {
            panic!()
        };
        let ParseResult::Match(output, input) = ("hello", char::when(char::is_alphabetic))
            .first()
            .extract(buffer, Some(state))
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "hello");
        assert_eq!(input.to_string(), "3a");
    }
}
