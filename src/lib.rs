#[macro_use]
extern crate nom;

pub mod game;
pub mod event;
pub mod parser;
mod parsers;

pub use self::parser::{Error as ParserError, Parser};