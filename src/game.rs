pub use self::Error as GameError;

use std::collections::HashMap;

use ::event::{Event, Info};

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    Unimplemented,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum State {
    Metadata,
    Plays,
    Data,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Game {
    pub id: String,
    pub info: HashMap<Info, String>,
    state: State,
}

impl Game {
    pub fn new(id: String) -> Game {
        Game {
            id: id,
            info: HashMap::new(),
            state: State::Metadata,
        }
    }

    pub fn process_event(&mut self, event: Event) -> Result<(), Error> {
        Err(Error::Unimplemented)
    }
}