use std::marker::PhantomData;

use crate::repeatable::Repeatable;
use crate::{ByteStream, Extract, ParseResult};

// A trait to merge a sequence of ByteStreams into a single ByteStream
pub trait Collectable: Extract + Sized {
    fn collect<Target>(self) -> CollectParser<Target, Self> {
        CollectParser::new(self)
    }
}

impl<Parser> Collectable for Parser
where
    Parser: Extract,
    Parser::Output: IntoIterator<Item = ByteStream>,
{
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

impl<Target, InnerParser> Repeatable for CollectParser<Target, InnerParser> where
    CollectParser<Target, InnerParser>: Extract
{
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

impl<Parser> Mappable for Parser
where
    Parser: Extract,
    Parser::Output: IntoIterator<Item = ByteStream>,
{
}

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

impl<Func, Output, InnerParser> Repeatable for MapParser<Func, Output, InnerParser>
where
    InnerParser: Extract,
    Func: Fn(InnerParser::Output) -> Output,
{
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
            ParseResult::NoMatch => ParseResult::NoMatch,
            ParseResult::Partial(state) => ParseResult::Partial(state),
            ParseResult::Match(output, input) => ParseResult::Match((self.func)(output), input),
        }
    }
}
