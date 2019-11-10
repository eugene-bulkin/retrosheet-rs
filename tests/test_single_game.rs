extern crate retrosheet;

use std::fs::File;
use std::io::Read;

use retrosheet::Parser;

#[test]
fn test_single_game() {
    let mut parser = Parser::new();

    let mut file = File::open(format!(
        "{}/test_resources/CHN201604110.EVN",
        env!("CARGO_MANIFEST_DIR")
    ))
    .expect("could not find CHN201604110.EVN");
    let mut buf = vec![];
    file.read_exact(&mut buf).expect("could not read");

    assert!(parser.parse(&mut buf).is_ok());
}
