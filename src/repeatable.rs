use std::ops::RangeBounds;

use crate::iterable::OutputToByteStream;
use crate::{ByteStream, Extract, ParseResult};

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
            .unwrap_or((None, ByteStream::new(input.position())));
        saved.append(&input);
        match self.inner.extract(input, inner_state, last) {
            ParseResult::NoMatch(_) => ParseResult::Match(Self::Output::default(), saved),
            ParseResult::Partial(_) if last => ParseResult::Match(Self::Output::default(), saved),
            ParseResult::Partial(inner_state) => ParseResult::Partial((inner_state, saved)),
            ParseResult::Match(output, input) => ParseResult::Match(output, input),
        }
    }
}

impl<InnerParser> OutputToByteStream for OptionalParser<InnerParser>
where
    InnerParser: OutputToByteStream,
    InnerParser::Output: Default,
{
    fn output_to_bytestream(output: Self::Output) -> ByteStream {
        InnerParser::output_to_bytestream(output)
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
        let default_position = input.position();
        let (mut inner_state, mut output) = state
            .map(|(s, o)| (Some(s), o))
            .unwrap_or((None, Vec::new()));
        while output.len() < self.times {
            match self.inner.extract(input, inner_state, last) {
                ParseResult::NoMatch(position) => {
                    return ParseResult::NoMatch(position);
                }
                ParseResult::Partial(_) if last => {
                    return ParseResult::NoMatch(default_position);
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

impl<InnerParser> OutputToByteStream for TimesParser<InnerParser>
where
    InnerParser: OutputToByteStream,
{
    fn output_to_bytestream(output: Self::Output) -> ByteStream {
        output
            .into_iter()
            .map(|o| InnerParser::output_to_bytestream(o))
            .collect()
    }
}

pub struct RepeatedParser<InnerParser> {
    inner: InnerParser,
    min: usize,
    max: usize,
}

#[allow(dead_code)]
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
        let default_position = input.position();
        let (mut inner_state, mut saved, mut output) = state
            .map(|(s, saved, output)| (Some(s), saved, output))
            .unwrap_or((None, ByteStream::new(input.position()), Vec::new()));
        while output.len() < self.min {
            match self.inner.extract(input, inner_state, last) {
                ParseResult::NoMatch(position) => {
                    return ParseResult::NoMatch(position);
                }
                ParseResult::Partial(_) if last => {
                    return ParseResult::NoMatch(default_position);
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
                ParseResult::NoMatch(_) => {
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

impl<InnerParser> OutputToByteStream for RepeatedParser<InnerParser>
where
    InnerParser: OutputToByteStream,
{
    fn output_to_bytestream(output: Self::Output) -> ByteStream {
        output
            .into_iter()
            .map(|o| InnerParser::output_to_bytestream(o))
            .collect()
    }
}
