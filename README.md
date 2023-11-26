# hefty

Parser for streaming data.

```rust
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
    .collect::<ByteStream>();

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
```
