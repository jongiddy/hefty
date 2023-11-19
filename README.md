# hefty

Parser for streaming data.

```rust
// Build parsers for a hostname (https://www.rfc-editor.org/rfc/rfc3986.html#appendix-A)
let unreserved = char::when(|c| {
    char::is_ascii_alphanumeric(&c) || c == '-' || c == '.' || c == '_' || c == '~'
});
let pct_encoded = ('%', char::when(char::is_ascii_hexdigit).times(2).collect())
    .seq()
    .collect();
let sub_delims = ('!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=').any();

let host_name = (&unreserved, &pct_encoded, &sub_delims)
    .any()
    .repeated(1..255)
    .collect::<ByteStream>();

// Parse a hostname arriving as a stream of data.
let ParseResult::Partial(state) =
    host_name.extract(ByteStream::from("www.exampl"), None, false)
else {
    panic!();
};
let ParseResult::Match(output, input) =
    host_name.extract(ByteStream::from("e.com/path"), Some(state), true)
else {
    panic!();
};
assert_eq!(output.to_string(), "www.example.com");
assert_eq!(input.to_string(), "/path");
```
