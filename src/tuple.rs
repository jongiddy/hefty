use typle::typle;

use crate::byte_stream::ByteStream;
use crate::iterable::OutputToByteStream;
use crate::{Extract, ParseResult, Repeatable};

pub struct TupleAny<T> {
    tuple: T,
}

#[typle(Tuple for 1..=12)]
impl<T> Extract for TupleAny<T>
where
    T: Tuple,
    T<_>: Extract<Output = ByteStream>,
{
    type State = typle_for!(i in .. => Option<Option<T<{i}>::State>>);
    type Output = ByteStream;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, Self::Output> {
        let mut exhausted = true;
        let mut state = state.unwrap_or(typle_for!(.. => Some(None)));
        for typle_const!(i) in 0..T::LEN {
            if let Some(inner_state) = &mut state[[i]] {
                let input = input.clone();
                match self.tuple[[i]].extract(input, inner_state.take(), last) {
                    ParseResult::NoMatch(_) => {
                        state[[i]] = None;
                    }
                    ParseResult::Partial(new_state) => {
                        *inner_state = Some(new_state);
                        if !last {
                            exhausted = false;
                        }
                    }
                    ParseResult::Match(output, input) => {
                        return ParseResult::Match(output, input);
                    }
                }
            }
        }
        if exhausted {
            return ParseResult::NoMatch(input.position());
        } else {
            return ParseResult::Partial(state);
        }
    }
}

#[typle(Tuple for 1..=12)]
impl<T> Repeatable for TupleAny<T>
where
    T: Tuple,
    T<_>: Extract<Output = ByteStream>,
{
}

#[typle(Tuple for 1..=12)]
impl<T> OutputToByteStream for TupleAny<T>
where
    T: Tuple,
    T<_>: Extract<Output = ByteStream>,
{
    fn output_to_bytestream(output: Self::Output) -> ByteStream {
        output
    }
}

pub struct TupleFirst<T> {
    tuple: T,
}

#[typle(Tuple for 1..=12)]
impl<T> Extract for TupleFirst<T>
where
    T: Tuple,
    T<_>: Extract<Output = ByteStream>,
{
    type State = typle_for!(i in .. => Option<ParseResult<T<{i}>::State, ByteStream>>);
    type Output = ByteStream;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, Self::Output> {
        let mut first = true;
        let mut state = state.unwrap_or(typle_for!(.. => None));
        for typle_const!(i) in 0..T::LEN {
            state[[i]] = match state[[i]].take() {
                Some(ParseResult::NoMatch(position)) => Some(ParseResult::NoMatch(position)),
                Some(ParseResult::Partial(inner_state)) => {
                    let res = self.tuple[[i]].extract(input.clone(), Some(inner_state), last);
                    match res {
                        ParseResult::NoMatch(position) => Some(ParseResult::NoMatch(position)),
                        ParseResult::Partial(state) => {
                            if !last {
                                first = false;
                            }
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
                    let res = self.tuple[[i]].extract(input.clone(), None, last);
                    match res {
                        ParseResult::NoMatch(position) => Some(ParseResult::NoMatch(position)),
                        ParseResult::Partial(state) => {
                            if !last {
                                first = false;
                            }
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
        }
        if first {
            return ParseResult::NoMatch(input.position());
        } else {
            return ParseResult::Partial(state);
        }
    }
}

#[typle(Tuple for 1..=12)]
impl<T> Repeatable for TupleFirst<T>
where
    T: Tuple,
    T<_>: Extract<Output = ByteStream>,
{
}

#[typle(Tuple for 1..=12)]
impl<T> OutputToByteStream for TupleFirst<T>
where
    T: Tuple,
    T<_>: Extract<Output = ByteStream>,
{
    fn output_to_bytestream(output: Self::Output) -> ByteStream {
        output
    }
}

#[typle(Tuple for 1..=12)]
pub enum TupleSequenceState<T>
where
    T: Tuple,
    T<_>: Extract,
{
    S = typle_variant!(i in .. =>
        typle_for!(j in ..i => T::<{j}>::Output), Option<T<{i}>::State>
    ),
}

pub struct TupleSequence<T> {
    tuple: T,
}

#[typle(Tuple for 1..=12)]
impl<T> Extract for TupleSequence<T>
where
    T: Tuple,
    T<_>: Extract,
{
    type State = TupleSequenceState<T<{..}>>;
    type Output = typle_for!(i in .. => <T<{i}> as Extract>::Output);

    fn extract(
        &self,
        mut input: ByteStream,
        state: Option<Self::State>,
        last: bool,
    ) -> ParseResult<Self::State, Self::Output> {
        let default_position = input.position();
        #[allow(unused_mut)]
        let mut state = state.unwrap_or(Self::State::S::<typle_index!(0)>((), None));
        for typle_const!(i) in 0..T::LEN {
            #[allow(irrefutable_let_patterns)]
            if let Self::State::S::<typle_index!(i)>(output, inner_state) = state {
                match self.tuple[[i]].extract(input, inner_state, last) {
                    ParseResult::NoMatch(position) => {
                        return ParseResult::NoMatch(position);
                    }
                    ParseResult::Partial(inner_state) => {
                        if last {
                            return ParseResult::NoMatch(default_position);
                        } else {
                            return ParseResult::Partial(Self::State::S::<typle_index!(i)>(
                                output,
                                Some(inner_state),
                            ));
                        }
                    }
                    ParseResult::Match(matched, remain) => {
                        // Make an output (i+1)-tuple from the existing output i-tuple and `matched`
                        let output = typle_for!(j in ..=i =>
                            if typle_const!(j != i) {
                                output[[j]]
                            } else {
                                matched
                            }
                        );
                        input = remain;
                        if typle_const!(i + 1 == T::LEN) {
                            return ParseResult::Match(output, input);
                        } else {
                            state = Self::State::S::<typle_index!(i + 1)>(output, None);
                        }
                    }
                }
            }
        }
        unreachable!();
    }
}

#[typle(Tuple for 1..=12)]
impl<T> Repeatable for TupleSequence<T>
where
    T: Tuple,
    T<_>: Extract,
{
}

#[typle(Tuple for 1..=12)]
impl<T> OutputToByteStream for TupleSequence<T>
where
    T: Tuple,
    T<_>: OutputToByteStream,
{
    fn output_to_bytestream(output: Self::Output) -> ByteStream {
        #[allow(unused_mut)]
        let mut byte_stream = T::<0>::output_to_bytestream(output.0);
        for typle_const!(i) in 1..T::LEN {
            byte_stream.merge(T::<{ i }>::output_to_bytestream(output[[i]]));
        }
        byte_stream
    }
}

pub trait ExtractTuple {
    type TupleAny;
    type TupleFirst;
    type TupleSequence;

    fn any(self) -> Self::TupleAny;

    fn first(self) -> Self::TupleFirst;

    fn seq(self) -> Self::TupleSequence;
}

#[typle(Tuple for 1..=12)]
impl<T> ExtractTuple for T
where
    T: Tuple,
    T<_>: Extract,
{
    type TupleAny = TupleAny<T>;
    type TupleFirst = TupleFirst<T>;
    type TupleSequence = TupleSequence<T>;

    fn any(self) -> Self::TupleAny {
        TupleAny { tuple: self }
    }

    fn first(self) -> Self::TupleFirst {
        TupleFirst { tuple: self }
    }

    fn seq(self) -> Self::TupleSequence {
        TupleSequence { tuple: self }
    }
}

#[cfg(test)]
mod tests {
    use crate::byte_stream::ByteStream;
    use crate::{Extract, ExtractTuple, ParseResult, ParseWhen, Repeatable};

    #[test]
    fn test_sequence() {
        let input = ByteStream::from("hello3a");
        let ParseResult::Match((out1, out2, out3), input) = (
            "hello",
            char::when(|c: char| c.is_digit(10)),
            char::when(char::is_alphabetic),
        )
            .seq()
            .extract(input, None, true)
        else {
            panic!()
        };
        assert_eq!(out1.to_string(), "hello");
        assert_eq!(out2.to_string(), "3");
        assert_eq!(out3.to_string(), "a");
        assert!(input.is_empty());

        let mut buffer = ByteStream::from("hello3a");
        let input = buffer.take_before(3);
        let ParseResult::Partial(state) = ("hello", char::when(char::is_alphabetic))
            .seq()
            .extract(input, None, false)
        else {
            panic!()
        };
        let ParseResult::NoMatch(5) =
            ("hello", char::when(char::is_alphabetic))
                .seq()
                .extract(buffer, Some(state), false)
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
                .extract(input, None, true)
        else {
            panic!()
        };
        assert!(out1.is_empty());
        assert!(out2.is_empty());
        assert_eq!(input.to_string(), "hello, world!");

        let ParseResult::Match((out1, out2), input) = ("hello, ", "world!")
            .seq()
            .optional()
            .extract(input, None, true)
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
            .extract(input, None, true)
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
            .extract(input, None, true)
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
            .extract(buffer, Some(state), false)
        else {
            panic!()
        };
        assert_eq!(output.to_string(), "hello");
        assert_eq!(input.to_string(), "3a");
    }
}
