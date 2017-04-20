use nom::IResult;

use ::event::Event;
use ::game::{Game, GameError, GameState};
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

impl ::std::fmt::Display for Error {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Error::UnexpectedGameId { ref current_game_id } => {
                write!(f, "game_id event was received before finishing game '{}'", current_game_id)
            }
            Error::NoGame(ref evt) => {
                write!(f, "event {:?} was received before a game was registered", evt)
            }
            Error::BytesRemaining(ref bytes) => {
                write!(f, "bytes remaining after parsing completed: {:?}", bytes)
            }
            Error::GameProcessingError(ref e) => write!(f, "{}", e)
        }
    }
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
        let mut result = Vec::new();
        while let IResult::Done(rest, evt) = event(bytes) {
            let mut new_game: Option<Game> = None;
            match self.state {
                State::Empty => {
                    // The only valid event when the state is empty is a game ID (which marks a new
                    // game).
                    if let Event::GameId { id } = evt {
                        new_game = Some(Game::new(id));
                    } else {
                        return Err(Error::NoGame(evt.clone()));
                    }
                }
                State::Game(ref mut game) => {
                    match evt {
                        Event::GameId { id } => {
                            // Game IDs can't appear until the previous game is done.
                            if let GameState::Data = game.get_state() {
                                // The data stage is the last stage of game processing, so if an id
                                // event occurs during that stage, that means the previous game is
                                // done being parsed.
                                new_game = Some(Game::new(id));
                            } else {
                                return Err(Error::UnexpectedGameId { current_game_id: game.id.clone() });
                            }
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
            if let Some(game) = new_game {
                // We do this because we can't borrow the state mutably twice up there during the
                // match, and it would be too awkward to work around that.
                if self.state != State::Empty {
                    // Pull out the current game in the state and finish it up, then add to the list
                    if let State::Game(mut game) = ::std::mem::replace(&mut self.state, State::Empty) {
                        try!(game.finish().map_err(Error::GameProcessingError));
                        result.push(game);
                    }
                }
                self.state = State::Game(game);
            }
            bytes = rest;
        }
        if !bytes.is_empty() {
            Err(Error::BytesRemaining(bytes.to_vec()))
        } else {
            if self.state != State::Empty {
                // We reached the end of input after a game, so make sure the game can be validly
                // finished, and then add it.
                if let State::Game(mut game) = ::std::mem::replace(&mut self.state, State::Empty) {
                    try!(game.finish().map_err(Error::GameProcessingError));
                    result.push(game);
                }
            }
            Ok(result)
        }
    }
}

#[cfg(test)]
mod tests {
    const SHORT_GAME: &'static [u8] = b"id,CHN201506130
version,2
info,number,0
start,foo,\"Bar\",0,1,6
play,4,1,meh,12,CCX,S8.B-2
play,4,1,buz,00,,NP
sub,buz,\"Bax\",1,3,4
data,er,foo,2";

    use super::*;

    use std::collections::{HashMap, HashSet};
    use std::iter::FromIterator;

    use ::event::{Advance, Base, DataEventType, Event, Info, Pitch, Player, PlayDescription,
                  PlayEvent, Team};
    use ::game::{Game, GameError, GameState, Substitution};

    fn expected_values() -> (HashMap<Info, String>, HashSet<Player>, Vec<Event>, Vec<Event>, Vec<Substitution>) {
        let expected_info = {
            let mut map = HashMap::new();
            map.insert(Info::Number, "0".into());
            map
        };
        let expected_starters = HashSet::from_iter(vec![
            Player {
                id: "foo".into(),
                name: "Bar".into(),
                team: Team::Visiting,
                batting_pos: 1,
                fielding_pos: 6,
            }
        ].into_iter());
        let expected_plays = vec![
            Event::Play {
                inning: 4,
                team: Team::Home,
                player: "meh".into(),
                count: Some((1, 2)),
                pitches: vec![
                    Pitch::CalledStrike,
                    Pitch::CalledStrike,
                    Pitch::BallInPlayBatter,
                ],
                event: PlayEvent {
                    description: PlayDescription::Single(vec![8]),
                    modifiers: vec![],
                    advances: vec![
                        Advance {
                            from: Base::Home,
                            to: Base::Second,
                            success: true,
                            parameters: vec![],
                        }
                    ],
                },
            }
            // The NP should be gone because of the sub!
        ];
        let expected_data = vec![
            Event::Data {
                data_type: DataEventType::EarnedRuns,
                player: "foo".into(),
                value: "2".into(),
            }
        ];
        let expected_substitutions = vec![
            Substitution {
                inning: 4,
                batting_team: Team::Home,
                player: Player {
                    id: "buz".into(),
                    name: "Bax".into(),
                    team: Team::Home,
                    batting_pos: 3,
                    fielding_pos: 4,
                },
            }
        ];

        (expected_info, expected_starters, expected_plays, expected_data, expected_substitutions)
    }

    #[test]
    fn test_game_parse() {
        let (expected_info, expected_starters, expected_plays, expected_data, expected_substitutions) = expected_values();

        let mut parser = Parser::new();
        assert_eq!(State::Empty, parser.state);

        let result = parser.parse(SHORT_GAME).unwrap();
        let ref game_result: Game = result[0];
        assert_eq!(expected_info, game_result.info);
        assert_eq!(expected_starters, game_result.starters);
        assert_eq!(expected_plays, game_result.plays);
        assert_eq!(expected_substitutions, game_result.substitutions);
        assert_eq!(expected_data, game_result.data);

        assert_eq!(State::Empty, parser.state);
    }

    #[test]
    fn test_multiple_games_parse() {
        let (expected_info, expected_starters, expected_plays, expected_data, expected_substitutions) = expected_values();

        let buf = {
            let mut buf: Vec<u8> = SHORT_GAME.to_vec();
            buf.extend_from_slice(b"\n");
            buf.extend_from_slice(SHORT_GAME);
            buf
        };

        let mut parser = Parser::new();

        let result = parser.parse(&buf).unwrap();
        let ref game_result1: Game = result[0];
        let ref game_result2: Game = result[1];

        assert_eq!(expected_info, game_result1.info);
        assert_eq!(expected_starters, game_result1.starters);
        assert_eq!(expected_plays, game_result1.plays);
        assert_eq!(expected_substitutions, game_result1.substitutions);
        assert_eq!(expected_data, game_result1.data);

        assert_eq!(expected_info, game_result2.info);
        assert_eq!(expected_starters, game_result2.starters);
        assert_eq!(expected_plays, game_result2.plays);
        assert_eq!(expected_substitutions, game_result2.substitutions);
        assert_eq!(expected_data, game_result2.data);
    }

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
        let result = parser.parse(SHORT_GAME);
        assert!(result.is_ok(), "resetting parser did not allow new game: {}", result.err().unwrap());
    }

    #[test]
    fn test_error_printing() {
        let event = Event::Data {
            data_type: DataEventType::EarnedRuns,
            player: "fred103".into(),
            value: "2".into()
        };
        let bytes = vec![1, 2, 3];

        assert_eq!("game_id event was received before finishing game 'foo'".to_string(),
        format!("{}", Error::UnexpectedGameId { current_game_id: "foo".into() }));
        assert_eq!(format!("event {:?} was received before a game was registered", event), format!("{}", Error::NoGame(event.clone())));

        assert_eq!(format!("bytes remaining after parsing completed: {:?}", bytes), format!("{}", Error::BytesRemaining(bytes.clone())));

        assert_eq!(format!("the event {:?} is not valid in the starting roster parsing state", event),
        format!("{}", Error::GameProcessingError(GameError::InvalidEvent(GameState::Starters, event.clone()))));
    }
}