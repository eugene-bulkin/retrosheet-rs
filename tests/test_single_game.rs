extern crate retrosheet;

use std::fs::File;
use std::io::{BufRead, Read};

use retrosheet::{Parser, ParserError};

fn test_single_game(file: &str) {
    let mut parser = Parser::new();

    let file_name = format!("{}/test_resources/{}", env!("CARGO_MANIFEST_DIR"), file);
    let mut file = File::open(file_name).unwrap();

    let mut buf: String = String::new();
    file.read_to_string(&mut buf).expect("could not read");

    if let Err(e) = parser.parse(&buf) {
        match e {
            ParserError::BytesRemaining(s) => {
                let first_bytes: Vec<u8> = s
                    .bytes()
                    .take_while(|&byte| byte != b'\r' && byte != b'\n')
                    .collect();
                let first_line = std::str::from_utf8(&first_bytes).expect("valid utf8");
                assert!(false, "Failed on line: {}", first_line);
            }
            e => assert!(false, "{}", e),
        }
    }
}

#[test]
fn test_chn201604110() {
    test_single_game("CHN201604110.EVN");
}

#[test]
fn test_bos201504270() {
    test_single_game("BOS201504270.EVA");
}
