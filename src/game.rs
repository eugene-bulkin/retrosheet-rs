pub use self::{Error as GameError, State as GameState};

use std::collections::{HashMap, HashSet};

use ::event::{Event, Info, PlayDescription, Player, Team};

/// An event with any comments that may correspond to it.
pub type EventWithComments = (Event, Vec<Event>);

#[derive(Clone, Debug, PartialEq)]
/// An error that occurs while processing a game.
pub enum Error {
    /// The event provided is not valid in the current state.
    InvalidEvent(State, Event),
    /// The event before a substitution should always be a play, not something else.
    InvalidEventBeforeSub(Event),
    /// The event before a substitution should always be a play.
    NoEventBeforeSub,
    /// Parsing was already completed, so we cannot parse more events.
    ParsingDone,
    /// Parsing was not done when the user attempted to complete game processing.
    ParsingIncomplete,
    /// A portion of processing is not yet implemented.
    Unimplemented,
}

impl ::std::fmt::Display for Error {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Error::InvalidEvent(ref state, ref event) => {
                write!(f, "the event {:?} is not valid in the {} state", event, state)
            }
            Error::InvalidEventBeforeSub(ref event) => {
                write!(f, "substitutions must be preceded by a play event; got {:?}", event)
            }
            Error::NoEventBeforeSub => {
                write!(f, "substitutions must be preceded by a play event")
            }
            Error::ParsingDone => {
                write!(f, "parsing already completed")
            }
            Error::ParsingIncomplete => {
                write!(f, "parsing not fully done before attempted completion")
            }
            Error::Unimplemented => {
                write!(f, "the requested command is not yet implemented")
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
/// A substitution. Denotes what inning and batting team (i.e. bottom or top of inning) and which
/// player was subbed in.
pub struct Substitution {
    /// The inning in which the substitution was made.
    pub inning: u8,
    /// The team that was batting at the time of hte substitution.
    pub batting_team: Team,
    /// The player which was subbed in.
    pub player: Player,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
/// The parsing state of the game.
pub enum State {
    /// Parsing info events.
    Info,
    /// Parsing start events.
    Starters,
    /// Parsing plays.
    Plays,
    /// Parsing end-of-game data.
    Data,
    /// Done parsing.
    Done,
}

impl ::std::fmt::Display for State {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            State::Info => write!(f, "metadata parsing"),
            State::Starters => write!(f, "starting roster parsing"),
            State::Plays => write!(f, "play-by-play parsing"),
            State::Data => write!(f, "data parsing"),
            State::Done => write!(f, "finished"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
/// An MLB game.
pub struct Game {
    /// The game id.
    pub id: String,
    /// Metadata information about the game.
    pub info: HashMap<Info, String>,
    /// The players that started the game.
    pub starters: HashSet<Player>,
    /// The sequence of plays that occurred in the game, along with comments, if they exist.
    pub plays: Vec<EventWithComments>,
    /// The set of substitutions that happened during the game. This can be used to construct a full
    /// list of who was playing during what parts of the game.
    pub substitutions: Vec<Substitution>,
    /// The set of data events that were recorded after play-by-play information.
    pub data: Vec<Event>,
    state: State,
}

impl Game {
    /// Instantiate a new game object in the initial state.
    pub fn new<S: Into<String>>(id: S) -> Game {
        Game {
            id: id.into(),
            info: HashMap::new(),
            state: State::Info,
            starters: HashSet::new(),
            plays: vec![],
            substitutions: vec![],
            data: vec![],
        }
    }

    fn process_info_event(&mut self, event: Event) -> Result<(), Error> {
        match event {
            Event::Info { key, data } => {
                self.info.insert(key, data);
                Ok(())
            }
            Event::Start { .. } => {
                // Done with info, so process the event in the starters state.
                self.state = State::Starters;
                self.process_event(event)
            }
            _ => {
                Err(Error::InvalidEvent(self.state, event.clone()))
            }
        }
    }

    fn process_starter_event(&mut self, event: Event) -> Result<(), Error> {
        match event {
            Event::Start { player } => {
                self.starters.insert(player);
                Ok(())
            }
            Event::Play { .. } => {
                // Done with starters, so start play-by-play.
                self.state = State::Plays;
                self.process_event(event)
            }
            _ => {
                Err(Error::InvalidEvent(self.state, event.clone()))
            }
        }
    }

    fn process_play_event(&mut self, event: Event) -> Result<(), Error> {
        match event {
            Event::Play { .. } => {
                self.plays.push((event, vec![]));
                Ok(())
            },
            Event::BattingAdjustment { .. } | Event::PitchingAdjustment { .. } => {
                self.plays.push((event, vec![]));
                Ok(())
            },
            Event::Sub { player } => {
                // We actually get rid of the last play, because it will always be a NoPlay without
                // any pitch count. If not, it's an error and parsing failed anyway.
                let last_evt = self.plays.pop();
                match last_evt {
                    Some(event) => {
                        match event.0 {
                            Event::Play { ref inning, ref team, event: ref play_event, .. } => {
                                if play_event.description != PlayDescription::NoPlay {
                                    Err(Error::InvalidEventBeforeSub(event.0.clone()))
                                } else {
                                    self.substitutions.push(Substitution {
                                        inning: *inning,
                                        batting_team: *team,
                                        player: player
                                    });
                                    Ok(())
                                }
                            }
                            _ => Err(Error::InvalidEventBeforeSub(event.0.clone()))
                        }
                    }
                    None => Err(Error::NoEventBeforeSub),
                }
            },
            Event::Comment { .. } => {
                let len = self.plays.len();
                self.plays[len - 1].1.push(event);
                Ok(())
            },
            Event::Data { .. } => {
                // Done with plays, so collect data.
                self.state = State::Data;
                self.process_event(event)
            },
            _ => {
                Err(Error::InvalidEvent(self.state, event.clone()))
            }
        }
    }

    fn process_data_event(&mut self, event: Event) -> Result<(), Error> {
        match event {
            Event::Data { .. } => {
                self.data.push(event);
                Ok(())
            }
            _ => {
                Err(Error::InvalidEvent(self.state, event.clone()))
            }
        }
    }

    /// Retrieve the game state.
    pub fn get_state(&self) -> State {
        // TODO: When pub(restricted) gets stabilized, we can just make the field selectively public
        self.state
    }

    /// Attempt to complete parsing. Only allowed in the data state.
    pub fn finish(&mut self) -> Result<(), Error> {
        if let State::Data = self.state {
            self.state = State::Done;
            Ok(())
        } else {
            Err(Error::ParsingIncomplete)
        }
    }

    /// Process an event in the context of the game.
    pub fn process_event(&mut self, event: Event) -> Result<(), Error> {
        match self.state {
            State::Info => self.process_info_event(event),
            State::Starters => self.process_starter_event(event),
            State::Plays => self.process_play_event(event),
            State::Data => self.process_data_event(event),
            State::Done => Err(Error::ParsingDone)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::HashSet;
    use std::iter::FromIterator;

    use ::event::{DataEventType, Event, Info, Player, PlayDescription, PlayEvent, Team};

    #[test]
    fn test_process_info() {
        let mut game = Game::new("foo");

        assert_eq!(Ok(()), game.process_event(Event::Info { key: Info::HomeTeam, data: "bar".into() }));
        assert_eq!(Some(&"bar".into()), game.info.get(&Info::HomeTeam));

        let evt = Event::GameId { id: "blah".into() };
        assert_eq!(Err(Error::InvalidEvent(State::Info, evt.clone())), game.process_event(evt.clone()));

        let player = Player {
            id: "fred103".into(),
            name: "fred".into(),
            team: Team::Home,
            batting_pos: 7,
            fielding_pos: 6,
        };

        assert_eq!(Ok(()), game.process_event(Event::Start { player: player }));
        assert_eq!(State::Starters, game.state);
    }

    #[test]
    fn test_process_starters() {
        let mut game = Game::new("foo");
        let player1 = Player {
            id: "fred103".into(),
            name: "fred".into(),
            team: Team::Home,
            batting_pos: 7,
            fielding_pos: 6,
        };
        let player2 = Player {
            id: "jim104".into(),
            name: "jim".into(),
            team: Team::Visiting,
            batting_pos: 3,
            fielding_pos: 1,
        };

        assert_eq!(Ok(()), game.process_event(Event::Start { player: player1.clone() }));
        assert_eq!(Ok(()), game.process_event(Event::Start { player: player2.clone() }));
        assert_eq!(HashSet::from_iter(vec![player1.clone(), player2.clone()].into_iter()), game.starters);


        let evt = Event::Info { key: Info::HomeTeam, data: "bar".into() };
        assert_eq!(Err(Error::InvalidEvent(State::Starters, evt.clone())), game.process_event(evt.clone()));

        assert_eq!(Ok(()), game.process_event(Event::Play {
            inning: 2,
            team: Team::Visiting,
            player: "foo".into(),
            count: None,
            pitches: vec![],
            event: PlayEvent {
                description: PlayDescription::Balk,
                modifiers: vec![],
                advances: vec![],
            }
        }));
        assert_eq!(State::Plays, game.state);
    }

    #[test]
    fn test_process_plays() {
        let event1 = Event::Play {
            inning: 2,
            team: Team::Visiting,
            player: "foo".into(),
            count: None,
            pitches: vec![],
            event: PlayEvent {
                description: PlayDescription::Balk,
                modifiers: vec![],
                advances: vec![],
            }
        };

        let event_np = Event::Play {
            inning: 2,
            team: Team::Visiting,
            player: "foo".into(),
            count: None,
            pitches: vec![],
            event: PlayEvent {
                description: PlayDescription::NoPlay,
                modifiers: vec![],
                advances: vec![],
            }
        };
        let player = Player {
            id: "fred103".into(),
            name: "fred".into(),
            team: Team::Home,
            batting_pos: 7,
            fielding_pos: 6,
        };
        let event2 = Event::Sub {
            player: player.clone()
        };
        let event3 = Event::Data {
            data_type: DataEventType::EarnedRuns,
            player: "fred103".into(),
            value: "2".into()
        };
        let comment = Event::Comment {
            comment: "foo".into(),
        };

        {
            let mut game = Game::new("foo");
            game.state = State::Plays;
            assert_eq!(Ok(()), game.process_event(event1.clone()));
            assert_eq!(vec![(event1.clone(), vec![])], game.plays);
            assert_eq!(Ok(()), game.process_event(comment.clone()));
            assert_eq!(vec![(event1.clone(), vec![comment.clone()])], game.plays);

            assert_eq!(Ok(()), game.process_event(event_np.clone()));
            assert_eq!(Ok(()), game.process_event(event2.clone()));
            assert_eq!(vec![(event1.clone(), vec![comment.clone()])], game.plays);

            assert_eq!(vec![Substitution {
                inning: 2,
                batting_team: Team::Visiting,
                player: player.clone(),
            }], game.substitutions);

            assert_eq!(Ok(()), game.process_event(event3));
            assert_eq!(State::Data, game.state);
        }

        {
            let mut game = Game::new("foo");
            game.state = State::Plays;
            assert_eq!(Ok(()), game.process_event(event1.clone()));
            assert_eq!(vec![(event1.clone(), vec![])], game.plays);

            assert_eq!(Err(Error::InvalidEventBeforeSub(event1.clone())), game.process_event(event2.clone()));
        }

        {
            let mut game = Game::new("foo");
            game.state = State::Plays;

            assert_eq!(Err(Error::NoEventBeforeSub), game.process_event(event2.clone()));
        }
    }

    #[test]
    fn test_done() {
        let mut game = Game::new("foo");
        assert_eq!(Err(Error::ParsingIncomplete), game.finish());

        game.state = State::Data;
        assert_eq!(Ok(()), game.finish());

        assert_eq!(Err(Error::ParsingDone), game.process_event(Event::Info {
            key: Info::HomeTeam,
            data: "bar".into()
        }));
    }

    #[test]
    fn test_error_printing() {
        let event = Event::Data {
            data_type: DataEventType::EarnedRuns,
            player: "fred103".into(),
            value: "2".into()
        };
        assert_eq!("the requested command is not yet implemented".to_string(), format!("{}", Error::Unimplemented));
        assert_eq!("parsing not fully done before attempted completion".to_string(), format!("{}", Error::ParsingIncomplete));
        assert_eq!("parsing already completed".to_string(), format!("{}", Error::ParsingDone));
        assert_eq!("substitutions must be preceded by a play event".to_string(), format!("{}", Error::NoEventBeforeSub));
        assert_eq!(format!("substitutions must be preceded by a play event; got {:?}", event), format!("{}", Error::InvalidEventBeforeSub(event.clone())));

        assert_eq!(format!("the event {:?} is not valid in the metadata parsing state", event), format!("{}", Error::InvalidEvent(State::Info, event.clone())));
        assert_eq!(format!("the event {:?} is not valid in the starting roster parsing state", event), format!("{}", Error::InvalidEvent(State::Starters, event.clone())));
        assert_eq!(format!("the event {:?} is not valid in the play-by-play parsing state", event), format!("{}", Error::InvalidEvent(State::Plays, event.clone())));
        assert_eq!(format!("the event {:?} is not valid in the data parsing state", event), format!("{}", Error::InvalidEvent(State::Data, event.clone())));
        assert_eq!(format!("the event {:?} is not valid in the finished state", event), format!("{}", Error::InvalidEvent(State::Done, event.clone())));
    }
}