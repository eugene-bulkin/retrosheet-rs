pub use self::{Error as GameError, State as GameState};

use std::collections::{HashMap, HashSet};

use ::event::{Event, Info, Player};

#[derive(Clone, Debug, PartialEq)]
/// An error that occurs while processing a game.
pub enum Error {
    /// The event provided is not valid in the current state.
    InvalidEvent(State, Event),
    /// A portion of processing is not yet implemented.
    Unimplemented,
}

impl ::std::fmt::Display for Error {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Error::InvalidEvent(ref state, ref event) => {
                write!(f, "the event {:?} is not valid in the {:?} state", event, state)
            }
            Error::Unimplemented => {
                write!(f, "the requested command is not yet implemented")
            }
        }
    }
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

    /// Process an event in the context of the game.
    pub fn process_event(&mut self, event: Event) -> Result<(), Error> {
        match self.state {
            State::Info => self.process_info_event(event),
            State::Starters => self.process_starter_event(event),
            _ => Err(Error::Unimplemented),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::HashSet;
    use std::iter::FromIterator;

    use ::event::{Event, Info, Player, Team};

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

        assert_eq!(Err(Error::Unimplemented), game.process_event(Event::Play { inning: 2 }));
        assert_eq!(State::Plays, game.state);
    }
}