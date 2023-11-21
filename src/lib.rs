use repeatable::Repeatable;

mod byte_stream;
mod extract_byte;
mod extract_utf8;
mod iterable;
mod repeatable;
mod tuple;

#[cfg(test)]
mod tests;

pub use self::byte_stream::ByteStream;
pub use self::extract_byte::byte;
pub use self::iterable::{Collectable, Mappable};
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

/// A trait to extract a type from a `ByteStream`.
///
/// The `extract` method is initially called with a state of `None`. If the type is extracted fully,
/// the method returns `ParseResult::Match` with the extracted type and the remaining `ByteStream`.
///
/// If the type cannot be extracted because it does not match, the method returns
/// `ParseResult::NoMatch`.
///
/// If the extraction reads to the end of the `ByteStream` but needs more data to extract the type
/// it returns `ParseResult::Partial` with a state. The next call to `extract` with a new
/// `ByteStream` must pass in the returned state to continue parsing the type.
///
/// If the end of the stream is reached call `extract` with `last=true`, as some extractors can
/// return `ParseResult::Match` in this case. e.g. `'a'.repeated(2..5)` will return `Partial` for
/// "aaa" since it may be part of a longer pattern, but on end of stream, will return the 3
/// characters as a match.
pub trait Extract {
    type State;
    type Output;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, Self::Output>;
}

impl<T> Extract for &T
where
    T: Extract,
{
    type State = T::State;

    type Output = T::Output;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, Self::Output> {
        (*self).extract(input, state, last)
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
