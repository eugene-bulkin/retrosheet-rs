//! This crate provides a parser for [Retrosheet event files].
//!
//! Retrosheet event files describe play-by-play information and other metadata for a series of MLB
//! games, usually for all games at home by a certain team.
//!
//! The parser provided takes *entire* event files and returns game objects describing the games.
//! The files must be entire in the sense that no game data can be incomplete.
//!
//! [Retrosheet event files]: http://www.retrosheet.org/eventfile.htm

#![deny(missing_docs)]

extern crate nom;

pub use self::game::{Game, GameError};
pub use self::parser::{Parser, ParserError};

pub mod event;
mod game;
mod parser;
mod parsers;
