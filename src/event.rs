pub type PlayerId = String;

#[derive(Clone, Debug, PartialEq)]
pub struct Player {
    pub id: PlayerId,
    pub name: String,
    pub team: Team,
    pub batting_pos: u8,
    pub fielding_pos: u8,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Team {
    Visiting = 0,
    Home = 1,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Info {
    VisitingTeam,
    HomeTeam,
    Site,
    Date,
    Number,
    StartTime,
    DayNight,
    DesignatedHitter,
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
pub enum Event {
    GameId { id: String },
    Version { version: u8 },
    Info { key: Info, data: String },
    Start { player: Player },
    Sub { player: Player },
    Play { inning: u8 },
}