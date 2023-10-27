use crate::byte_stream::ByteStream;
use crate::{Extract, ParseResult, Repeatable};

pub struct TupleAny2<E0, E1>
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

pub struct TupleFirst2<E0, E1>
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

pub enum TupleState2<E0, E1>
where
    E0: Extract<Output = ByteStream>,
    E1: Extract<Output = ByteStream>,
{
    S0(Option<E0::State>),
    S1(Option<E1::State>, (ByteStream,)),
    Done((ByteStream, ByteStream)),
}

pub struct TupleSequence2<E0, E1> {
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

pub trait ExtractTuple {
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
    use crate::byte_stream::ByteStream;
    use crate::{Extract, ExtractTuple, ParseResult, ParseWhen, Repeatable};

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
