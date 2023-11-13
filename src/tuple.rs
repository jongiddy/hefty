use typle::typle;

use crate::byte_stream::ByteStream;
use crate::{Extract, ParseResult, Repeatable};

pub struct TupleAny<T> {
    tuple: T,
}

#[typle(Tuple for 2..=4)]
impl<T> Extract for TupleAny<T>
where
    T: Tuple<impl Extract<Output = ByteStream>>,
{
    type State = typle_expand!(Option<Option<T::State>>);
    type Output = ByteStream;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Self::Output> {
        let mut exhausted = true;
        let mut state = state.unwrap_or(typle_expand!(Some(None)));
        for typle_const!(i) in 0..T::LEN {
            if let Some(inner_state) = &mut state[[i]] {
                let input = input.clone();
                match self.tuple[[i]].extract(input, inner_state.take()) {
                    ParseResult::NoMatch => {
                        state[[i]] = None;
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
        }
        if exhausted {
            return ParseResult::NoMatch;
        } else {
            return ParseResult::Partial(state);
        }
    }
}

pub struct TupleFirst<T> {
    tuple: T,
}

#[typle(Tuple for 2..=4)]
impl<T> Extract for TupleFirst<T>
where
    T: Tuple<impl Extract<Output = ByteStream>>,
{
    type State = typle_expand!(Option<ParseResult<T::State, ByteStream>>);
    type Output = ByteStream;

    fn extract(
        &self,
        input: ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Self::Output> {
        let mut first = true;
        let mut state = state.unwrap_or(typle_expand!(None));
        for typle_const!(i) in 0..T::LEN {
            state[[i]] = match state[[i]].take() {
                Some(ParseResult::NoMatch) => Some(ParseResult::NoMatch),
                Some(ParseResult::Partial(inner_state)) => {
                    let res = self.tuple[[i]].extract(input.clone(), Some(inner_state));
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
                    let res = self.tuple[[i]].extract(input.clone(), None);
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
        }
        if first {
            return ParseResult::NoMatch;
        } else {
            return ParseResult::Partial(state);
        }
    }
}

#[typle(Tuple for 2..=4)]
pub enum TupleState<T>
where
    T: Tuple<impl Extract<Output = ByteStream>>,
{
    S(Option<T::State>, [ByteStream; T::INDEX]) = typle_variants!(),
}

pub struct TupleSequence<T> {
    tuple: T,
}

#[typle(Tuple for 2..=4)]
impl<T> Repeatable for TupleSequence<T> where T: Tuple<impl Extract<Output = ByteStream>> {}

#[typle(Tuple for 2..=4)]
impl<T> Extract for TupleSequence<T>
where
    T: Tuple<impl Extract<Output = ByteStream>>,
{
    type State = TupleState<(T)>;
    type Output = [ByteStream; T::LEN];

    fn extract(
        &self,
        mut input: ByteStream,
        state: Option<Self::State>,
    ) -> ParseResult<Self::State, Self::Output> {
        let mut state = state.unwrap_or(Self::State::S::<typle_index!(0)>(None, []));
        for typle_const!(i) in 0..T::LEN {
            if let Self::State::S::<typle_index!(i)>(inner_state, output) = state {
                match self.tuple[[i]].extract(input, inner_state) {
                    ParseResult::NoMatch => {
                        return ParseResult::NoMatch;
                    }
                    ParseResult::Partial(inner_state) => {
                        return ParseResult::Partial(Self::State::S::<typle_index!(i)>(
                            Some(inner_state),
                            output,
                        ));
                    }
                    ParseResult::Match(matched, remain) => {
                        let mut new_output = <[ByteStream; i + 1]>::default();
                        output
                            .into_iter()
                            .chain(std::iter::once(matched))
                            .enumerate()
                            .for_each(|(j, bs)| new_output[j] = bs);
                        input = remain;
                        if typle_const!(i + 1 == T::LEN) {
                            return ParseResult::Match(new_output, input);
                        } else {
                            state = Self::State::S::<typle_index!(i + 1)>(None, new_output);
                        }
                    }
                }
            }
        }
        unreachable!();
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

#[typle(Tuple for 2..=4)]
impl<T> ExtractTuple for T
where
    T: Tuple<impl Extract>,
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
        let ParseResult::Match([out1, out2, out3], input) = (
            "hello",
            char::when(|c: char| c.is_digit(10)),
            char::when(char::is_alphabetic),
        )
            .seq()
            .extract(input, None)
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
        let ParseResult::Match([out1, out2], input) =
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

        let ParseResult::Match([out1, out2], input) =
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
