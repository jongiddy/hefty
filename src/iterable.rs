use crate::repeatable::Repeatable;
use crate::{ByteStream, Extract, ParseResult};

pub trait OutputToByteStream: Extract {
    fn output_to_bytestream(output: Self::Output) -> ByteStream;
}

impl<T> OutputToByteStream for &T
where
    T: OutputToByteStream,
{
    fn output_to_bytestream(output: Self::Output) -> ByteStream {
        T::output_to_bytestream(output)
    }
}

// A trait to merge a sequence of ByteStreams into a single ByteStream
pub trait Collectable: Extract + Sized {
    fn collect(self) -> CollectParser<Self> {
        CollectParser::new(self)
    }
}

impl<Parser> Collectable for Parser where Parser: OutputToByteStream {}

pub struct CollectParser<InnerParser> {
    inner: InnerParser,
}

#[allow(dead_code)]
impl<InnerParser> CollectParser<InnerParser> {
    fn new(inner: InnerParser) -> Self {
        Self { inner }
    }
}

impl<InnerParser> Extract for CollectParser<InnerParser>
where
    InnerParser: OutputToByteStream,
{
    type State = InnerParser::State;
    type Output = ByteStream;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, Self::Output> {
        match self.inner.extract(input, state, last) {
            ParseResult::NoMatch(position) => ParseResult::NoMatch(position),
            ParseResult::Partial(state) => ParseResult::Partial(state),
            ParseResult::Match(output, input) => {
                ParseResult::Match(InnerParser::output_to_bytestream(output), input)
            }
        }
    }
}

impl<InnerParser> Repeatable for CollectParser<InnerParser> where InnerParser: OutputToByteStream {}

impl<InnerParser> OutputToByteStream for CollectParser<InnerParser>
where
    InnerParser: OutputToByteStream,
{
    fn output_to_bytestream(output: Self::Output) -> ByteStream {
        output
    }
}
// A trait to map an extractors response to another format
pub trait Mappable: Extract + Sized {
    fn map<Func, Output>(self, func: Func) -> MapParser<Func, Output, Self>
    where
        Self: Extract,
        Func: Fn(Self::Output) -> Output,
    {
        MapParser::new(self, func)
    }
}

impl<Parser> Mappable for Parser where Parser: Extract {}

pub struct MapParser<Func, Output, InnerParser>
where
    InnerParser: Extract,
    Func: Fn(InnerParser::Output) -> Output,
{
    inner: InnerParser,
    func: Func,
}

#[allow(dead_code)]
impl<Func, Output, InnerParser> MapParser<Func, Output, InnerParser>
where
    InnerParser: Extract,
    Func: Fn(InnerParser::Output) -> Output,
{
    fn new(inner: InnerParser, func: Func) -> Self {
        Self { inner, func }
    }
}

impl<Func, Output, InnerParser> Extract for MapParser<Func, Output, InnerParser>
where
    InnerParser: Extract,
    Func: Fn(InnerParser::Output) -> Output,
{
    type State = InnerParser::State;
    type Output = Output;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, Self::Output> {
        match self.inner.extract(input, state, last) {
            ParseResult::NoMatch(position) => ParseResult::NoMatch(position),
            ParseResult::Partial(state) => ParseResult::Partial(state),
            ParseResult::Match(output, input) => ParseResult::Match((self.func)(output), input),
        }
    }
}

impl<Func, Output, InnerParser> Repeatable for MapParser<Func, Output, InnerParser>
where
    InnerParser: Extract,
    Func: Fn(InnerParser::Output) -> Output,
{
}
