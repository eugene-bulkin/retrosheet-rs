extern crate retrosheet;

use retrosheet::{Parser, ParserError};

use std::fs::File;
use std::io::Read;

#[test]
fn test_multiple_games() {
    let mut parser = Parser::new();

    let file_name = format!("{}/test_resources/2016CHN.EVN", env!("CARGO_MANIFEST_DIR"));
    let mut file = File::open(file_name).unwrap();

    let mut buf: Vec<u8> = vec![];
    file.read_to_end(&mut buf).unwrap();

    let result = parser.parse(&buf);

    match result {
        Ok(_) => {},
        Err(e) => {
            match e {
                ParserError::BytesRemaining(bytes) => {
                    let first_chunk: Vec<u8> = bytes.into_iter().take(50).collect();
                    assert!(false, "unfinished: {}...", ::std::str::from_utf8(&first_chunk).unwrap())
                },
                _ => assert!(false, "{}", e),
            }
        }
    }
}