use nom::IResult;

use ::event::Event;
use ::game::{Game, GameError};
use ::parsers::event;

#[derive(Clone, Debug, PartialEq)]
/// An error that occurs while parsing an event file.
pub enum Error {
    /// The parser received an unexpected `game_id` event.
    UnexpectedGameId {
        /// The id of the game already registered.
        current_game_id: String
    },
    /// The parser received an game event without a game having been started.
    NoGame(Event),
    /// There were bytes remaining after attempting to parse the event file.
    BytesRemaining(Vec<u8>),
    /// An error occurred while processing events for a game.
    GameProcessingError(GameError),
}

#[derive(Clone, Debug, PartialEq)]
enum State {
    Empty,
    Game(Game),
}

#[derive(Clone, Debug, PartialEq)]
/// An event file parser for Retrosheet event files.
pub struct Parser {
    state: State,
}

impl Parser {
    /// Creates a new parser instance.
    pub fn new() -> Parser {
        Parser { state: State::Empty }
    }

    /// Resets the parser to the initial state.
    pub fn reset(&mut self) {
        self.state = State::Empty;
    }

    /// Parses an event file.
    ///
    /// This returns a list of the games in the event file.
    ///
    /// # Arguments
    /// * bytes - A stream of byte-data containing the entirety of the event file.
    ///
    /// # Errors
    /// The parser expects the byte-data coming in to be a *complete* event file, so any bytes
    /// remaining are counted as an error. Any other errors are from invalid events.
    pub fn parse(&mut self, mut bytes: &[u8]) -> Result<Vec<Game>, Error> {
        let result = Vec::new();
        while let IResult::Done(rest, evt) = event(bytes) {
            match self.state {
                State::Empty => {
                    // The only valid event when the state is empty is a game ID (which marks a new
                    // game).
                    if let Event::GameId { id } = evt {
                        let game = Game::new(id);
                        self.state = State::Game(game);
                    } else {
                        return Err(Error::NoGame(evt.clone()));
                    }
                }
                State::Game(ref mut game) => {
                    match evt {
                        Event::GameId { .. } => {
                            // Game IDs can't appear until the previous game is done.
                            // TODO: The way to actually check that the previous game is done is by
                            //       confirming it is in the Data stage.
                            return Err(Error::UnexpectedGameId { current_game_id: game.id.clone() });
                        }
                        Event::Version { .. } => {
                            // We actually don't care about the version as long as it came in a
                            // valid location.
                        }
                        _ => {
                            try!(game.process_event(evt).map_err(Error::GameProcessingError));
                        }
                    }
                }
            }
            bytes = rest;
        }
        if !bytes.is_empty() {
            Err(Error::BytesRemaining(bytes.to_vec()))
        } else {
            Ok(result)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use ::event::Event;

    #[test]
    fn test_unexpected_id() {
        let mut parser = Parser::new();
        let result = parser.parse(b"id,CHN201506130
version,2
id,CHN201604110");
        assert_eq!(Err(Error::UnexpectedGameId { current_game_id: "CHN201506130".into() }), result);
    }

    #[test]
    fn test_unexpected_version() {
        let mut parser = Parser::new();
        let result = parser.parse(b"version,3");
        assert_eq!(Err(Error::NoGame(Event::Version { version: 3 })), result);
    }

    #[test]
    fn test_bytes_remaining() {
        let mut parser = Parser::new();
        let result = parser.parse(b"id,CHN201506130
version,2
foo\n\n");
        assert_eq!(Err(Error::BytesRemaining(b"foo\n\n".to_vec())), result);
    }

    #[test]
    fn test_reset() {
        let mut parser = Parser::new();
        let _ = parser.parse(b"id,CHN201506130");
        parser.reset();
        assert!(parser.parse(b"id,CHN201506130").is_ok(), "resetting parser did not allow new game");
    }
}