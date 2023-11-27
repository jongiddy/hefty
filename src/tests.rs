use std::collections::VecDeque;

use bytes::Buf;

use crate::{byte, ExtractTuple};

use super::{
    ByteStream, Collectable, Extract, Mappable, ParseAny, ParseResult, ParseWhen, Repeatable,
};

#[test]
fn test_byte_literal() {
    let buffer = ByteStream::from("23");
    let input = buffer.clone();
    assert_eq!(input.remaining(), 2);
    let ParseResult::Match(output, input) = b'2'.extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), "2");
    let ParseResult::Match(output, input) = b'3'.extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), "3");
    let ParseResult::Partial(_) = b'4'.extract(input, None, false) else {
        panic!()
    };

    let input = buffer.clone();
    let ParseResult::NoMatch(0) = b'3'.extract(input, None, false) else {
        panic!()
    };
}

#[test]
fn test_byte_array_literal() {
    let buffer = ByteStream::from("hello, world!");
    let input = buffer.clone();
    let ParseResult::Match(output, input) = b"hello".extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), "hello");
    let ParseResult::Match(output, input) = b", ".extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), ", ");
    let ParseResult::NoMatch(7) = b" everyone!".extract(input, None, false) else {
        panic!()
    };
}

#[test]
fn test_byte_any() {
    let buffer = ByteStream::from("A4");
    let input = buffer.clone();
    assert_eq!(input.remaining(), 2);
    let ParseResult::Match(output, input) = byte::any().extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), "A");
    let ParseResult::Match(output, input) = byte::any().extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), "4");
    let ParseResult::Partial(_) = byte::any().extract(input, None, false) else {
        panic!()
    };
}

#[test]
fn test_byte_when() {
    let buffer = ByteStream::from("A4");
    let input = buffer.clone();
    assert_eq!(input.remaining(), 2);
    let ParseResult::Match(output, input) =
        byte::when(|b: u8| u8::is_ascii_alphabetic(&b)).extract(input, None, false)
    else {
        panic!()
    };
    assert_eq!(output.to_string(), "A");
    let ParseResult::NoMatch(1) =
        byte::when(|b: u8| u8::is_ascii_alphabetic(&b)).extract(input.clone(), None, false)
    else {
        panic!()
    };
    let ParseResult::Match(output, input) =
        byte::when(|b: u8| u8::is_ascii_digit(&b)).extract(input, None, false)
    else {
        panic!()
    };
    assert_eq!(output.to_string(), "4");
    let ParseResult::Partial(_) =
        byte::when(|b: u8| u8::is_ascii_digit(&b)).extract(input, None, false)
    else {
        panic!()
    };
}

#[test]
fn test_str_literal() {
    let buffer = ByteStream::from("hello, world!");
    let input = buffer.clone();
    let ParseResult::Match(output, input) = "hello".extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), "hello");
    let ParseResult::Match(output, input) = ", ".extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), ", ");
    let ParseResult::NoMatch(7) = " everyone!".extract(input, None, false) else {
        panic!()
    };
}

#[test]
fn test_1_byte_char() {
    let buffer = ByteStream::from("23");
    let input = buffer.clone();
    assert_eq!(input.remaining(), 2);
    let ParseResult::Match(output, input) = '2'.extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), "2");
    assert_eq!(input.iter().next(), Some(&b'3'));
    assert_eq!(input.position(), 1);
    let ParseResult::NoMatch(1) = '2'.extract(input, None, false) else {
        panic!()
    };
}

#[test]
fn test_multibyte_char() {
    let buffer = ByteStream::from("€4");
    let input = buffer.clone();
    assert_eq!(input.remaining(), 4);
    let ParseResult::Match(output, input) = '€'.extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), "€");
    assert_eq!(input.iter().next(), Some(&b'4'));
    let ParseResult::NoMatch(3) = '€'.extract(input, None, false) else {
        panic!()
    };

    let input = buffer.clone();
    let ParseResult::Match(output, input) = char::any().extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), "€");
    let ParseResult::Match(output, input) = char::any().extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), "4");
    let ParseResult::Partial(_) = char::any().extract(input, None, false) else {
        panic!()
    };

    let input = buffer.clone();
    let ParseResult::NoMatch(0) =
        char::when(char::is_alphabetic).extract(input.clone(), None, false)
    else {
        panic!()
    };
    let ParseResult::Match(output, input) =
        char::when(|c: char| !c.is_alphabetic()).extract(input, None, false)
    else {
        panic!()
    };
    assert_eq!(output.to_string(), "€");
    let ParseResult::Match(output, input) =
        char::when(|c| char::is_digit(c, 10)).extract(input, None, false)
    else {
        panic!()
    };
    assert_eq!(output.to_string(), "4");
    let ParseResult::Partial(_) = char::when(char::is_alphabetic).extract(input, None, false)
    else {
        panic!()
    };

    let input = buffer.clone();
    let ParseResult::NoMatch(0) =
        char::when(|c| c.is_ascii_alphabetic()).extract(input.clone(), None, false)
    else {
        panic!()
    };
    let ParseResult::Match(output, input) =
        char::when(|c| !c.is_ascii_alphabetic()).extract(input, None, false)
    else {
        panic!()
    };
    assert_eq!(output.to_string(), "€");
    let ParseResult::Match(output, input) =
        char::when(|c| c.is_ascii_digit()).extract(input, None, false)
    else {
        panic!()
    };
    assert_eq!(output.to_string(), "4");
    let ParseResult::Partial(_) = char::when(|c| c.is_ascii_digit()).extract(input, None, false)
    else {
        panic!()
    };
}

#[test]
fn test_multibyte_char_byte_by_byte() {
    let mut buffer = ByteStream::from("€4");
    let input = buffer.take_before(1);
    let ParseResult::Partial(state) = '€'.extract(input, None, false) else {
        panic!()
    };
    let input = buffer.take_before(1);
    let ParseResult::Partial(state) = '€'.extract(input, Some(state), false) else {
        panic!()
    };
    let input = buffer;
    let ParseResult::Match(output, input) = '€'.extract(input, Some(state), true) else {
        panic!()
    };
    assert_eq!(output.to_string(), "€");
    assert_eq!(input.iter().next(), Some(&b'4'));
    let ParseResult::NoMatch(3) = '€'.extract(input, None, false) else {
        panic!()
    };
}

// A 3-byte symbol that matches until the last byte
#[test]
fn test_multibyte_char_nearly() {
    let mut buffer = ByteStream::from("€4");
    let input = buffer.take_before(1);
    let ParseResult::Partial(state) = '₭'.extract(input, None, false) else {
        panic!()
    };
    let input = buffer.take_before(1);
    let ParseResult::Partial(state) = '₭'.extract(input, Some(state), false) else {
        panic!()
    };
    let input = buffer;
    let output = '₭'.extract(input, Some(state), true);
    assert!(matches!(output, ParseResult::NoMatch(0)));
}

#[test]
fn test_optional() {
    let input = ByteStream::from("helloworld");
    let ParseResult::Match(output, input) = "hello".optional().extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), "hello");
    let ParseResult::Match(output, input) = ", ".optional().extract(input, None, false) else {
        panic!()
    };
    assert!(output.is_empty());
    let ParseResult::Match(output, input) = "world".optional().extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), "world");
    let ParseResult::Partial(_) = "world".optional().extract(input.clone(), None, false) else {
        panic!()
    };
    let ParseResult::Match(output, input) = "world".optional().extract(input, None, true) else {
        panic!()
    };
    assert!(output.is_empty());
    assert!(input.is_empty());
}

#[test]
fn test_times() {
    let input = ByteStream::from("hellohellohello");
    let ParseResult::Match(output, input) = "hello".times(3).extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.len(), 3);
    for out in output {
        assert_eq!(out.to_string(), "hello");
    }
    assert!(input.is_empty());
}

#[test]
fn test_repeated() {
    let input = ByteStream::from("hellohellohello");
    let ParseResult::Match(output, input) = "hello".repeated(1..3).extract(input, None, false)
    else {
        panic!()
    };
    assert_eq!(output.len(), 2);
    for out in output {
        assert_eq!(out.to_string(), "hello");
    }
    assert_eq!(input.to_string(), "hello");

    let parser = "hello".repeated(1..10);
    let input = ByteStream::from("hellohellohellohe");
    let ParseResult::Partial(state) = parser.extract(input, None, false) else {
        panic!()
    };
    let input = ByteStream::from("llohelp");
    let ParseResult::Match(output, input) = parser.extract(input, Some(state), true) else {
        panic!()
    };
    assert_eq!(output.len(), 4);
    for out in output {
        assert_eq!(out.to_string(), "hello");
    }
    assert_eq!(input.to_string(), "help");

    let input = ByteStream::from("hellohellohellohe");
    let ParseResult::Partial(state) = parser.extract(input, None, false) else {
        panic!()
    };
    let ParseResult::Match(output, input) =
        parser.extract(ByteStream::default(), Some(state), true)
    else {
        panic!()
    };
    assert_eq!(output.len(), 3);
    for out in output {
        assert_eq!(out.to_string(), "hello");
    }
    assert_eq!(input.to_string(), "he");

    let input = ByteStream::from("helo");
    let ParseResult::Match(output, input) = "hello".repeated(0..2).extract(input, None, false)
    else {
        panic!()
    };
    assert_eq!(output.len(), 0);
    for out in output {
        assert_eq!(out.to_string(), "hello");
    }
    assert_eq!(input.to_string(), "helo");
}

struct Reverse {
    n: usize,
}

impl Extract for Reverse {
    type State = ();
    type Output = ByteStream;

    fn extract(
        &self,
        mut input: ByteStream,
        _state: Option<Self::State>,
        _last: bool,
    ) -> ParseResult<Self::State, Self::Output> {
        let mut d = VecDeque::new();
        for b in input.take_before(self.n).iter().cloned() {
            d.push_front(b);
        }
        let v = d.into_iter().collect::<Vec<_>>();
        let output = ByteStream::from(bytes::Bytes::from(v));
        ParseResult::Match(output, input)
    }
}

fn reverse(n: usize) -> Reverse {
    Reverse { n }
}

#[test]
fn test_function() {
    let input = ByteStream::from("helloworld");
    let ParseResult::Match(output, input) = reverse(6).extract(input, None, false) else {
        panic!()
    };
    assert_eq!(output.to_string(), "wolleh");
    assert_eq!(input.to_string(), "orld");
}

#[test]
fn test_sequence() {
    let input = ByteStream::from("hello3a");
    let ParseResult::Match((out1, out2), input) = ("hello", char::when(|c: char| c.is_digit(10)))
        .seq()
        .extract(input, None, false)
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
        .extract(input, None, false)
    else {
        panic!()
    };
    let ParseResult::NoMatch(5) =
        ("hello", char::when(char::is_alphabetic))
            .seq()
            .extract(buffer, Some(state), true)
    else {
        panic!()
    };
}

#[test]
fn test_optional_sequence() {
    let input = ByteStream::from("hello, world!");
    let ParseResult::Match((out1, out2), input) = ("hello", char::when(|c: char| c.is_digit(10)))
        .seq()
        .optional()
        .extract(input, None, false)
    else {
        panic!()
    };
    assert!(out1.is_empty());
    assert!(out2.is_empty());
    assert_eq!(input.to_string(), "hello, world!");

    let ParseResult::Match((out1, out2), input) = ("hello, ", "world!")
        .seq()
        .optional()
        .extract(input, None, false)
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
        .extract(input, None, false)
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
        .extract(input, None, false)
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
        .extract(buffer, Some(state), true)
    else {
        panic!()
    };
    assert_eq!(output.to_string(), "hello");
    assert_eq!(input.to_string(), "3a");
}

#[test]
fn json_number() {
    let json_number_parser = (
        '-'.optional(),
        (
            '0',
            (
                ('1', '2', '3', '4', '5', '6', '7', '8', '9').any(),
                char::when(|c| c.is_ascii_digit()).repeated(..),
            )
                .seq()
                .collect(),
        )
            .any(),
        (
            '.',
            char::when(|c| c.is_ascii_digit()).repeated(..).collect(),
        )
            .seq()
            .optional(),
        (
            ('e', 'E').any(),
            ('-', '+').any().optional(),
            char::when(|c| c.is_ascii_digit()).repeated(..),
        )
            .seq()
            .optional(),
    )
        .seq()
        .collect();

    let input = ByteStream::from("-123");
    let ParseResult::Partial(state) = json_number_parser.extract(input, None, false) else {
        panic!();
    };
    let ParseResult::Match(output, input) =
        json_number_parser.extract(ByteStream::default(), Some(state), true)
    else {
        panic!();
    };
    assert_eq!(output.to_string(), "-123");
    assert!(input.is_empty());

    let ParseResult::Match(output, input) =
        json_number_parser.extract(ByteStream::from("3.14159,"), None, true)
    else {
        panic!();
    };
    assert_eq!(output.to_string(), "3.14159");
    assert_eq!(input.to_string(), ",");

    let ParseResult::Match(output, input) =
        json_number_parser.extract(ByteStream::from("4.2e+06"), None, true)
    else {
        panic!();
    };
    assert_eq!(output.to_string(), "4.2e+06");
    assert!(input.is_empty());

    let ParseResult::Match(output, input) =
        json_number_parser.extract(ByteStream::from("09.2"), None, true)
    else {
        panic!();
    };
    assert_eq!(output.to_string(), "0");
    assert_eq!(input.to_string(), "9.2");
}

#[test]
fn json_string() {
    let json_string_parser = (
        '"',
        (
            (
                '\\',
                (
                    '"',
                    '\\',
                    '/',
                    'b',
                    'f',
                    'n',
                    'r',
                    't',
                    (
                        'u',
                        char::when(|c| c.is_ascii_hexdigit()).times(4).collect(),
                    )
                        .seq()
                        .collect(),
                )
                    .any(),
            )
                .seq()
                .collect(),
            char::when(|c| c != '"' && c != '\\'),
        )
            .any()
            .repeated(..)
            .collect(),
        '"',
    )
        .seq()
        .collect();

    let ParseResult::Match(output, input) =
        json_string_parser.extract(ByteStream::from(r#""hello, world!","#), None, true)
    else {
        panic!();
    };
    assert_eq!(output.to_string(), r#""hello, world!""#);
    assert_eq!(input.to_string(), ",");

    let ParseResult::Match(output, input) =
        json_string_parser.extract(ByteStream::from(r#""""#), None, true)
    else {
        panic!();
    };
    assert_eq!(output.to_string(), r#""""#);
    assert!(input.is_empty());

    let ParseResult::Match(output, input) =
        json_string_parser.extract(ByteStream::from(r#""hello, \"!","#), None, true)
    else {
        panic!();
    };
    assert_eq!(output.to_string(), r#""hello, \"!""#);
    assert_eq!(input.to_string(), ",");

    let ParseResult::NoMatch(0) =
        json_string_parser.extract(ByteStream::from(r#""hello, \ufour!","#), None, true)
    else {
        panic!();
    };

    let ParseResult::Match(output, input) =
        json_string_parser.extract(ByteStream::from(r#""hello, \uAbCd\t""#), None, true)
    else {
        panic!();
    };
    assert_eq!(output.to_string(), r#""hello, \uAbCd\t""#);
    assert!(input.is_empty());
}

#[test]
fn url_authority() {
    // Build a parser for `authority` from https://www.rfc-editor.org/rfc/rfc3986.html#appendix-A
    // Share sub-parsers using borrowed references.
    let unreserved = char::when(|c| {
        char::is_ascii_alphanumeric(&c) || c == '-' || c == '.' || c == '_' || c == '~'
    });
    let pct_encoded = (
        '%',
        char::when(|c| c.is_ascii_hexdigit()).times(2).collect(),
    )
        .seq()
        .collect();
    let sub_delims = ('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=').any();

    let userinfo = (
        (&unreserved, &pct_encoded, &sub_delims, ':')
            .any()
            .repeated(..)
            .collect(),
        '@',
    )
        .seq()
        .collect();
    let reg_name = (&unreserved, &pct_encoded, &sub_delims)
        .any()
        .repeated(..)
        .collect();
    // host format is first match of IPv4, IPv6, or reg-name
    let host = (reg_name,).first();
    let port = (
        ':',
        char::when(|c| c.is_ascii_digit()).repeated(..).collect(),
    )
        .seq()
        .collect();
    let authority = (userinfo.optional(), host, port.optional()).seq().collect();

    let ParseResult::Match(output, input) =
        authority.extract(ByteStream::from("example.com:3245/path"), None, true)
    else {
        panic!();
    };
    assert_eq!(output.to_string(), "example.com:3245");
    assert_eq!(input.to_string(), "/path");
}

#[test]
fn url_authority_unpack() {
    // Build a parser for `authority` from https://www.rfc-editor.org/rfc/rfc3986.html#appendix-A
    // Share sub-parsers using borrowed references.
    let unreserved = char::when(|c| {
        char::is_ascii_alphanumeric(&c) || c == '-' || c == '.' || c == '_' || c == '~'
    });
    let pct_encoded = (
        '%',
        char::when(|c| c.is_ascii_hexdigit()).times(2).collect(),
    )
        .seq()
        .collect();
    let sub_delims = ('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=').any();

    // Using `first` ensures that we match the longest sequence of digits
    let dec_octet = (
        ("25", ("0", "1", "2", "3", "4", "5").any()).seq().collect(),
        (
            '2',
            ("0", "1", "2", "3", "4").any(),
            char::when(|c| c.is_ascii_digit()),
        )
            .seq()
            .collect(),
        ('1', char::when(|c| c.is_ascii_digit()).times(2))
            .seq()
            .collect(),
        (
            ("1", "2", "3", "4", "5", "6", "7", "8", "9").any(),
            char::when(|c| c.is_ascii_digit()),
        )
            .seq()
            .collect(),
        char::when(|c| c.is_ascii_digit()),
    )
        .first();
    let ipv4address = (
        &dec_octet, '.', &dec_octet, '.', &dec_octet, '.', &dec_octet,
    )
        .seq();
    let reg_name = (&unreserved, &pct_encoded, &sub_delims).any().repeated(..);

    let userinfo = (
        (&unreserved, &pct_encoded, &sub_delims, ':')
            .any()
            .repeated(..),
        '@',
    )
        .seq()
        .map(|(userinfo, _at)| userinfo.into_iter().collect::<ByteStream>());
    // "In order to disambiguate the syntax, we apply the "first-match-wins" algorithm"
    let host = (ipv4address.collect(), reg_name.collect()).first();
    let port = (
        ':',
        char::when(|c| c.is_ascii_digit()).repeated(..).collect(),
    )
        .seq()
        .map(|(_colon, port)| port);
    let authority = (userinfo.optional(), host, port.optional()).seq();

    let ParseResult::Match((userinfo, host, port), input) =
        authority.extract(ByteStream::from("example.com:3245/path"), None, true)
    else {
        panic!();
    };
    assert!(userinfo.is_empty());
    assert_eq!(host.to_string(), "example.com");
    assert_eq!(port.to_string(), "3245");
    assert_eq!(input.to_string(), "/path");

    let ParseResult::Match((userinfo, host, port), input) =
        authority.extract(ByteStream::from("example@192.168.14.57"), None, true)
    else {
        panic!();
    };
    assert_eq!(userinfo.to_string(), "example");
    assert_eq!(host.to_string(), "192.168.14.57");
    assert!(port.is_empty());
    assert!(input.is_empty());
}

#[test]
fn test_readme() {
    // Build parsers for a URI hostname (https://www.rfc-editor.org/rfc/rfc3986.html#appendix-A)
    let unreserved = char::when(|c| c.is_ascii_alphanumeric() || "-._~".contains(c));
    let pct_encoded = (
        '%',
        char::when(|c| c.is_ascii_hexdigit()).times(2).collect(),
    )
        .seq()
        .collect();
    let sub_delims = ('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=').any();

    let reg_name = (&unreserved, &pct_encoded, &sub_delims)
        .any()
        .repeated(1..255)
        .collect();

    // Parse a hostname arriving as a stream of data.
    let ParseResult::Partial(state) = reg_name.extract(ByteStream::from("www.exa"), None, false)
    else {
        panic!();
    };
    let ParseResult::Partial(state) =
        reg_name.extract(ByteStream::from("mple.co"), Some(state), false)
    else {
        panic!();
    };
    let ParseResult::Match(output, input) =
        reg_name.extract(ByteStream::from("m/path"), Some(state), true)
    else {
        panic!();
    };
    assert_eq!(output.to_string(), "www.example.com");
    assert_eq!(input.to_string(), "/path");
}
