# retrosheet-rs [![Build Status](https://travis-ci.org/eugene-bulkin/retrosheet-rs.svg?branch=master)](https://travis-ci.org/eugene-bulkin/retrosheet-rs) [![codecov](https://codecov.io/gh/eugene-bulkin/retrosheet-rs/branch/master/graph/badge.svg)](https://codecov.io/gh/eugene-bulkin/retrosheet-rs) [![crates.io](https://img.shields.io/crates/v/retrosheet.svg)](https://crates.io/crates/retrosheet)

`retrosheet` is a Rust crate for parsing event data from [Retrosheet], a baseball statistics aggregator. Currently, it 
only supports parsing the `.EVX` files that store a sequence of metadata and play-by-play data.

Using `retrosheet` is fairly straightforward, and most information can be gleaned by looking at the [documentation] or 
at Retrosheet's [event file specification], although the latter is somewhat incomplete.

To use the parser, provide the byte-data of one or more games (e.g. from reading an `EVX` file) and use the `parse`
method on the parser. This will return a vector of `Game` objects which store a variety of data, described in the 
[documentation]. For example:

```rust
let buf: Vec<u8>; // some buffer which may have been read into by a file
let mut parser = Parser::new();
match parser.parse(&buf) {
    Ok(ref games) => {
        for game in games {
            // Do something with the game here
        }
    },
    Err(ref e) => {
        panic!("Oh no! An error occurred: {}", e);
    }
}
```

[Retrosheet]: http://retrosheet.org
[documentation]: http://docs.rs/retrosheet
[event file specification]: http://retrosheet.org/eventfile.htm