pub use self::{Error as GameError, State as GameState};

use std::collections::{HashMap, HashSet};

use ::event::{Event, Info, Player};

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    InvalidEvent(State, Event),
    Unimplemented,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum State {
    Info,
    Starters,
    Plays,
    Data,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Game {
    pub id: String,
    pub info: HashMap<Info, String>,
    pub starters: HashSet<Player>,
    state: State,
}

impl Game {
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