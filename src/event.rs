//! This module contains structs and enums for describing Retrosheet events.

/// A Retrosheet player id.
pub type PlayerId = String;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// A player, referred to in starter events or in substitutions.
pub struct Player {
    /// The player's ID.
    pub id: PlayerId,
    /// The player's name.
    pub name: String,
    /// Which team the player was on for this game.
    pub team: Team,
    /// The batting position of the player (i.e. batting order). If the DH is being used, the
    /// pitcher is given position 0.
    pub batting_pos: u8,
    /// The fielding position of the player in standard notation. DH is 10, pinch hitter is 11, and
    /// pinch runner is 12 (the last two being used for subs).
    pub fielding_pos: u8,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// Which team a player or event corresponds to.
pub enum Team {
    /// The visiting team.
    Visiting = 0,
    /// The home team.
    Home = 1,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// A key describing the type of metadata about the game.
pub enum Info {
    /// The visiting team code.
    VisitingTeam,
    /// The home team code.
    HomeTeam,
    /// Where the game was played, given by park code.
    Site,
    /// The date on which the game was played.
    Date,
    /// Which game this is during a day. If the game is just a single game, this is 0. Otherwise,
    /// this is 1 or 2 depending on which game during a double header this is.
    Number,
    /// What time the game started.
    StartTime,
    /// Whether the game was during the day or at night.
    DayNight,
    /// Whether or not the designated hitter is being used.
    DesignatedHitter,
    /// An unknown metadata type.
    Unknown,
}

impl<'a> From<&'a [u8]> for Info {
    fn from(bytes: &'a [u8]) -> Info {
        match bytes {
            b"visteam" => Info::VisitingTeam,
            b"hometeam" => Info::HomeTeam,
            b"date" => Info::Date,
            _ => Info::Unknown,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
/// A record describing something about an MLB game.
pub enum Event {
    /// A twelve character ID record which identifies the date, home team, and number of the game.
    GameId {
        /// The game ID.
        id: String
    },
    /// A record identifying version information. The version is obsolete and can be ignored.
    Version {
        /// The version of the game data.
        version: u8
    },
    /// A record identifying game information.
    Info {
        /// The type of information in the record.
        key: Info,
        /// The information in the record. Just stored as a string, though it can describe a variety
        /// of data.
        data: String
    },
    /// A record identifying a starter in the game.
    Start {
        /// The player starting.
        player: Player
    },
    /// A record identifying a substitution of a player in the game.
    Sub {
        /// The player being subbed in.
        player: Player
    },
    /// A record identifying a play occurring during a game.
    Play {
        /// The inning, an integer starting at 1
        inning: u8
    },
}