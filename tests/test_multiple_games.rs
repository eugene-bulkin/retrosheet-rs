extern crate retrosheet;

use std::fs::File;
use std::io::Read;

use retrosheet::{Parser, ParserError};

fn test_game_file(file: &str) {
    let mut parser = Parser::new();

    let file_name = format!("{}/test_resources/{}", env!("CARGO_MANIFEST_DIR"), file);
    let mut file = File::open(file_name).unwrap();

    let mut buf: Vec<u8> = vec![];
    file.read_to_end(&mut buf).unwrap();

    let result = parser.parse(&buf);

    match result {
        Ok(_) => {}
        Err(e) => match e {
            ParserError::BytesRemaining(bytes) => {
                let first_chunk: Vec<u8> = bytes.into_iter().take(50).collect();
                assert!(
                    false,
                    "unfinished: {}...",
                    ::std::str::from_utf8(&first_chunk).unwrap()
                )
            }
            _ => assert!(false, "{}", e),
        },
    }
}

#[test]
fn test_2015bal() {
    test_game_file("2015BAL.EVA");
}

#[test]
fn test_2016chn() {
    test_game_file("2016CHN.EVN");
}

#[test]
fn test_2015bos() {
    test_game_file("2015BOS.EVA");
}

#[test]
fn test_2017mil() {
    test_game_file("2017MIL.EVN");
}
