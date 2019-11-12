extern crate retrosheet;

use std::fs::File;
use std::io::Read;

use retrosheet::Parser;

fn test_single_game(file: &str) {
    let mut parser = Parser::new();

    let file_name = format!("{}/test_resources/{}", env!("CARGO_MANIFEST_DIR"), file);
    let mut file = File::open(file_name).unwrap();

    let mut buf = vec![];
    file.read_exact(&mut buf).expect("could not read");

    assert!(parser.parse(&buf).is_ok());
}

#[test]
fn test_chn201604110() {
    test_single_game("CHN201604110.EVN");
}

#[test]
fn test_bos201504270() {
    test_single_game("BOS201504270.EVA");
}