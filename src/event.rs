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
    /// The extend of pitch information available. Either `pitches` (full detail), `count` (just the
    /// pitch count) or `none` (nothing) are the values.
    Pitches,
    /// The home umpire's ID.
    UmpireHome,
    /// The first base umpire's ID.
    Umpire1B,
    /// The second base umpire's ID.
    Umpire2B,
    /// The third base umpire's ID.
    Umpire3B,
    /// Field condition: one of `dry`, `soaked`, `wet` or `unknown`.
    FieldCondition,
    /// Precipitation: one of `drizzle`, `none`, `rain`, `showers`, `snow`, or `unknown`.
    Precipitation,
    /// The status of the sky: one of `cloudy`, `dome`, `night`, `overcast`, `sunny`, or `unknown`.
    Sky,
    /// The temperature during the game, in degrees Fahrenheit. 0 is the unknown value because it's
    /// unlikely a game would ever be played in 0F weather.
    Temperature,
    /// The wind direction: one of `fromcf`, `fromlf`, `fromrf`, `ltor`, `rtol`, `tocf`, `tolf`,
    /// `torf`, or `unknown`.
    WindDirection,
    /// The windspeed, with unknown being indicated by -1.
    WindSpeed,
    /// The length of the game in minutes. The key in actual Retrosheet data will be `timeofgame`.
    GameLength,
    /// The attendance of the game.
    Attendance,
    /// The winning pitcher.
    WinningPitcher,
    /// The losing pitcher.
    LosingPitcher,
    /// If applicable, the pitcher who got the save.
    SavePitcher,
    /// When used as an official statistic, the game winning RBI.
    GameWinningRBI,
    /// When the game was processed.
    EditTime,
    /// How the game was scored.
    ScoreMethod,
    /// The version of the program used to score the program.
    InputProgramVersion,
    /// The person who inputted the data.
    Inputter,
    /// The time at which the data was input.
    InputTime,
    /// The scorer of the game.
    Scorer,
    /// The translator of the data.
    Translator,
    /// An unknown metadata type.
    Unknown,
}

impl From<&str> for Info {
    fn from(bytes: &str) -> Info {
        match bytes {
            "visteam" => Info::VisitingTeam,
            "hometeam" => Info::HomeTeam,
            "date" => Info::Date,
            "number" => Info::Number,
            "pitches" => Info::Pitches,
            "umphome" => Info::UmpireHome,
            "ump1b" => Info::Umpire1B,
            "ump2b" => Info::Umpire2B,
            "ump3b" => Info::Umpire3B,
            "fieldcond" => Info::FieldCondition,
            "precip" => Info::Precipitation,
            "sky" => Info::Sky,
            "temp" => Info::Temperature,
            "winddir" => Info::WindDirection,
            "windspeed" => Info::WindSpeed,
            "timeofgame" => Info::GameLength,
            "attendence" => Info::Attendance,
            "wp" => Info::WinningPitcher,
            "lp" => Info::LosingPitcher,
            "save" => Info::SavePitcher,
            "gwrbi" => Info::GameWinningRBI,
            "edittime" => Info::EditTime,
            "howscored" => Info::ScoreMethod,
            "inputprogvers" => Info::InputProgramVersion,
            "inputter" => Info::Inputter,
            "inputtime" => Info::InputTime,
            "scorer" => Info::Scorer,
            "translator" => Info::Translator,
            _ => Info::Unknown,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// A pitch event. This may be a pitch being thrown to a batter, or a fielding event (e.g. a pickoff
/// throw, or a runner stealing).
pub enum Pitch {
    /// The following pickoff throw was by the catcher.
    PickoffThrowCatcher,
    /// The following pitch was blocked by the catcher.
    BlockedByCatcher,
    /// Some play not involving the batter occurred.
    NonBatterPlay,
    /// A pickoff throw to first.
    PickoffFirst,
    /// A pickoff throw to second.
    PickoffSecond,
    /// A pickoff throw to third.
    PickoffThird,
    /// A runner went on the pitch.
    RunnerGoingOnPitch,
    /// A ball.
    Ball,
    /// A called strike.
    CalledStrike,
    /// A foul ball.
    Foul,
    /// A hit-by-pitch.
    HitBatter,
    /// An intentional ball.
    IntentionalBall,
    /// A strike of unknown kind.
    UnknownStrike,
    /// A bunt that went foul.
    FoulBunt,
    /// A bunt attempt that missed.
    MissedBunt,
    /// No pitch (e.g. a balk, interference).
    NoPitch,
    /// A foul tip on a bunt.
    FoulTipBunt,
    /// A pitchout.
    Pitchout,
    /// A swing on a pitchout.
    SwingOnPitchout,
    /// A foul ball on a pitchout.
    FoulOnPitchout,
    /// A swinging strike.
    SwingingStrike,
    /// A foul tip.
    FoulTip,
    /// An unknown or missed pitch, or an unrecognized character.
    Unknown,
    /// A called ball because pitcher went to his mouth.
    BallPitcherWentToMouth,
    /// A ball put into play by the batter.
    BallInPlayBatter,
    /// A ball put into play on a pitchout.
    BallInPlayPitchout,
}

impl From<char> for Pitch {
    fn from(c: char) -> Pitch {
        match c {
            '+' => Pitch::PickoffThrowCatcher,
            '*' => Pitch::BlockedByCatcher,
            '.' => Pitch::NonBatterPlay,
            '1' => Pitch::PickoffFirst,
            '2' => Pitch::PickoffSecond,
            '3' => Pitch::PickoffThird,
            '>' => Pitch::RunnerGoingOnPitch,
            'B' => Pitch::Ball,
            'C' => Pitch::CalledStrike,
            'F' => Pitch::Foul,
            'H' => Pitch::HitBatter,
            'I' => Pitch::IntentionalBall,
            'K' => Pitch::UnknownStrike,
            'L' => Pitch::FoulBunt,
            'M' => Pitch::MissedBunt,
            'N' => Pitch::NoPitch,
            'O' => Pitch::FoulTipBunt,
            'P' => Pitch::Pitchout,
            'Q' => Pitch::SwingOnPitchout,
            'R' => Pitch::FoulOnPitchout,
            'S' => Pitch::SwingingStrike,
            'T' => Pitch::FoulTip,
            'V' => Pitch::BallPitcherWentToMouth,
            'X' => Pitch::BallInPlayBatter,
            'Y' => Pitch::BallInPlayPitchout,
            'U' | _ => Pitch::Unknown,
        }
    }
}

/// A base on the field.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Base {
    /// First base.
    First,
    /// Second base.
    Second,
    /// Third base.
    Third,
    /// Home.
    Home,
}

/// A fielder in standard notation.
pub type Fielder = u8;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// A field parameter; can just be a fielder, or a fielder committing an error.
pub enum FieldParameter {
    /// A fielder successfully made a play.
    Play(Fielder),
    /// A fielder made an error.
    Error(Fielder),
    /// An unknown fielder was involved.
    Unknown,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// A description of the basic play in a play event.
pub enum PlayDescription {
    /// A balk.
    Balk,
    /// A sequence of fielder plays. Just one means a flyout/unassisted groundout, while multiple
    /// means the last fielder made the out and the others got the assist.
    ///
    /// The boolean in the fielder play notes whether the play was an error.
    ///
    /// If the putout is made at a base not normally covered by the fielder the base runner is given
    /// explicitly in the second field. If not, the field is left as `None`.
    FielderSequence(Vec<(Fielder, bool)>, Option<Base>),
    /// The batter grounded into a double play, with the base the first out was recorded on.
    GIDP(Vec<Fielder>, Base),
    /// The batter grounded into a triple play. We provide the assists for the first out (and the
    /// base the first out was recorded on), the second out, and the putout.
    GITP {
        /// The throws for the first out.
        first_assists: Vec<Fielder>,
        /// The base the first out was recorded on.
        first_out: Base,
        /// The throws for the second out.
        second_assists: Vec<Fielder>,
        /// The base the second out was recorded on.
        second_out: Base,
        /// The putout fielder.
        putout: Fielder,
    },
    /// A fielder's choice, where the fielder given is the fielder first fielding the ball. The
    /// batter advance to first is understood if it is not given explicitly.
    FieldersChoice(Fielder),
    /// An error by a fielder.
    Error(Fielder),
    /// An error by a fielder attempting to field a foul fly ball.
    FoulFlyBallError(Fielder),
    /// A strikeout. May or may not be accompanied by an additional event, for example if there was
    /// a dropped third strike. May also be accompanied by a fielder sequence if the dropped third
    /// strike resulted in a putout.
    Strikeout(Option<Box<PlayDescription>>, Vec<(Fielder, bool)>),
    /// A walk. May or may not be accompanied by an additional event, describing potential base
    /// running events.
    Walk(Option<Box<PlayDescription>>),
    /// A passed ball.
    PassedBall,
    /// A wild pitch.
    WildPitch,
    /// A single (optionally) handled by a fielder or fielders.
    Single(Vec<Fielder>),
    /// A double (optionally) handled by a fielder or fielders.
    Double(Vec<Fielder>),
    /// A triple (optionally) handled by a fielder or fielders.
    Triple(Vec<Fielder>),
    /// A home run.
    HomeRun,
    /// An inside-the-park home run, with relevant fielder informatino.
    InsideTheParkHomeRun(Vec<Fielder>),
    /// No play was made. Used when a substitution immediately follows.
    NoPlay,
    /// Stolen base(s), with the base information. If the boolean is set to true, that means it was
    /// a steal of home where the run is unearned.
    StolenBase(Vec<(Base, bool)>),
    /// The player was hit by a pitch.
    HitByPitch,
    /// Catcher interference. Technically, this also covers interference by the pitcher or first
    /// baseman for interfering with the batter, so there is still a fielder option. But usually the
    /// fielder will be the catcher.
    CatcherInterference(Fielder),
    /// A ground rule double. No fielder specified.
    GroundRuleDouble,
    /// An intentional walk.
    IntentionalWalk,
    /// Defensive indifference: no attempt to prevent a stolen base. The advance field specifies
    /// which base the runner went to.
    DefensiveIndifference,
    /// A base runner advance that is not covered by one of the other codes. A comment may be given
    /// explaining the advance.
    OtherAdvance,
    /// A runner was picked off a base, along with a throw and the fielder making the put out. May
    /// instead just be an error (e.g. PO1(E3)), negating the out.
    PickOff(Base, Vec<FieldParameter>),
    /// A pick off that resulted in a caught stealing off of some base, along with a sequence of
    /// throws that resulted in the out. Note that there wouldn't be errors here since the
    /// implication is that they were caught.
    PickOffCaughtStealing(Base, Vec<Fielder>),
    /// A normal caught stealing event.
    CaughtStealing(Base, Vec<Fielder>),
    /// A batter lining into a double play. Should be followed with an `LDP` modifier, but we don't
    /// verify this.
    LinedIntoDoublePlay {
        /// The fielder who made the first out.
        first_out: Fielder,
        /// The throws for the second out.
        second_out: Vec<Fielder>,
        /// The base the second out was recorded on.
        second_out_runner: Base,
    },
    /// A batter lining into a triple play. Should be followed with an `LTP` modifier, but we don't
    /// verify this.
    LinedIntoTriplePlay {
        /// The fielder who made the first out.
        first_out: Fielder,
        /// The throws for the second out.
        second_out: Vec<Fielder>,
        /// The base the second out was recorded on.
        second_out_runner: Base,
        /// The throws for the third out.
        third_out: Vec<Fielder>,
        /// The base the third out was recorded on.
        third_out_runner: Base,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
/// The type of hit which a hit was made.
pub enum HitType {
    /// pop fly
    PopFly,
    /// pop up bunt
    PopUpBunt,
    /// fly
    Fly,
    /// ground ball
    GroundBall,
    /// ground ball bunt
    GroundBallBunt,
    /// line drive
    LineDrive,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
/// The type of hit that was made. For reference, see the [hit location diagram].
///
/// Enum names start with an underscore because we can't name stuff with numbers.
///
/// [hit location diagram]: http://www.retrosheet.org/location.htm
pub enum HitLocation {
    /// 2F
    _2F,
    /// 2
    _2,
    /// 25F
    _25F,
    /// 25
    _25,
    /// 1S
    _1S,
    /// 23
    _23,
    /// 23F
    _23F,
    /// 15
    _15,
    /// 1
    _1,
    /// 13
    _13,
    /// 5S
    _5S,
    /// 56S
    _56S,
    /// 6S
    _6S,
    /// 6MS
    _6MS,
    /// 4MS
    _4MS,
    /// 4S
    _4S,
    /// 34S
    _34S,
    /// 3S
    _3S,
    /// 5F
    _5F,
    /// 5
    _5,
    /// 56
    _56,
    /// 6
    _6,
    /// 6M
    _6M,
    /// 4M
    _4M,
    /// 4
    _4,
    /// 34
    _34,
    /// 3
    _3,
    /// 3F
    _3F,
    /// 5DF
    _5DF,
    /// 5D
    _5D,
    /// 56D
    _56D,
    /// 6D
    _6D,
    /// 6MD
    _6MD,
    /// 4MD
    _4MD,
    /// 4D
    _4D,
    /// 34D
    _34D,
    /// 3D
    _3D,
    /// 3DF
    _3DF,
    /// 7LSF
    _7LSF,
    /// 7LS
    _7LS,
    /// 7S
    _7S,
    /// 78S
    _78S,
    /// 8S
    _8S,
    /// 89S
    _89S,
    /// 9S
    _9S,
    /// 9LS
    _9LS,
    /// 9LSF
    _9LSF,
    /// 7LF
    _7LF,
    /// 7L
    _7L,
    /// 7
    _7,
    /// 78
    _78,
    /// 8
    _8,
    /// 89
    _89,
    /// 9
    _9,
    /// 9L
    _9L,
    /// 9LF
    _9LF,
    /// 7LDF
    _7LDF,
    /// 7LD
    _7LD,
    /// 7D
    _7D,
    /// 78D
    _78D,
    /// 8D
    _8D,
    /// 89D
    _89D,
    /// 9D
    _9D,
    /// 9LD
    _9LD,
    /// 9LDF
    _9LDF,
    /// 78XD
    _78XD,
    /// 8XD
    _8XD,
    /// 89XD
    _89XD,
    /// 5L; this event does not appear in the chart, but does seem to appear in some cases. Perhaps
    /// due to a shift?
    _5L,
}

impl From<&str> for HitLocation {
    fn from(bytes: &str) -> HitLocation {
        match bytes {
            "2F" => HitLocation::_2F,
            "2" => HitLocation::_2,
            "25F" => HitLocation::_25F,
            "25" => HitLocation::_25,
            "1S" => HitLocation::_1S,
            "23" => HitLocation::_23,
            "23F" => HitLocation::_23F,
            "15" => HitLocation::_15,
            "1" => HitLocation::_1,
            "13" => HitLocation::_13,
            "5S" => HitLocation::_5S,
            "56S" => HitLocation::_56S,
            "6S" => HitLocation::_6S,
            "6MS" => HitLocation::_6MS,
            "4MS" => HitLocation::_4MS,
            "4S" => HitLocation::_4S,
            "34S" => HitLocation::_34S,
            "3S" => HitLocation::_3S,
            "5F" => HitLocation::_5F,
            "5" => HitLocation::_5,
            "56" => HitLocation::_56,
            "6" => HitLocation::_6,
            "6M" => HitLocation::_6M,
            "4M" => HitLocation::_4M,
            "4" => HitLocation::_4,
            "34" => HitLocation::_34,
            "3" => HitLocation::_3,
            "3F" => HitLocation::_3F,
            "5DF" => HitLocation::_5DF,
            "5D" => HitLocation::_5D,
            "56D" => HitLocation::_56D,
            "6D" => HitLocation::_6D,
            "6MD" => HitLocation::_6MD,
            "4MD" => HitLocation::_4MD,
            "4D" => HitLocation::_4D,
            "34D" => HitLocation::_34D,
            "3D" => HitLocation::_3D,
            "3DF" => HitLocation::_3DF,
            "7LSF" => HitLocation::_7LSF,
            "7LS" => HitLocation::_7LS,
            "7S" => HitLocation::_7S,
            "78S" => HitLocation::_78S,
            "8S" => HitLocation::_8S,
            "89S" => HitLocation::_89S,
            "9S" => HitLocation::_9S,
            "9LS" => HitLocation::_9LS,
            "9LSF" => HitLocation::_9LSF,
            "7LF" => HitLocation::_7LF,
            "7L" => HitLocation::_7L,
            "7" => HitLocation::_7,
            "78" => HitLocation::_78,
            "8" => HitLocation::_8,
            "89" => HitLocation::_89,
            "9" => HitLocation::_9,
            "9L" => HitLocation::_9L,
            "9LF" => HitLocation::_9LF,
            "7LDF" => HitLocation::_7LDF,
            "7LD" => HitLocation::_7LD,
            "7D" => HitLocation::_7D,
            "78D" => HitLocation::_78D,
            "8D" => HitLocation::_8D,
            "89D" => HitLocation::_89D,
            "9D" => HitLocation::_9D,
            "9LD" => HitLocation::_9LD,
            "9LDF" => HitLocation::_9LDF,
            "78XD" => HitLocation::_78XD,
            "8XD" => HitLocation::_8XD,
            "89XD" => HitLocation::_89XD,
            "5L" => HitLocation::_5L,
            _ => unreachable!(bytes),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// A play event modifier.
pub enum PlayModifier {
    /// A hit of some kind, possibly with a specific location.
    HitWithLocation(HitType, Option<HitLocation>),
    /// Just a hit location (used with home runs).
    HitLocation(HitLocation),
    /// appeal play
    AppealPlay,
    /// bunt hit foul (usually with two strikes)
    BuntFoul,
    /// bunt grounded into double play
    BuntGroundedIntoDoublePlay,
    /// batter interference
    BatterInterference,
    /// line drive bunt
    LineDriveBunt,
    /// batting out of turn
    BattingOutOfTurn,
    /// bunt popped into double play
    BuntPoppedIntoDoublePlay,
    /// runner hit by batted ball
    RunnerHitByBattedBall,
    /// called third strike
    CalledThirdStrike,
    /// courtesy batter
    CourtesyBatter,
    /// courtesy fielder
    CourtesyFielder,
    /// courtesy runner
    CourtesyRunner,
    /// unspecified double play
    UnspecifiedDoublePlay,
    /// error on $
    Error(u8),
    /// fly ball double play
    FlyBallDoublePlay,
    /// fan interference
    FanInterference,
    /// foul
    Foul,
    /// force out
    ForceOut,
    /// ground ball double play
    GroundBallDoublePlay,
    /// ground ball triple play
    GroundBallTriplePlay,
    /// infield fly rule
    InfieldFlyRule,
    /// interference
    Interference,
    /// inside the park home run
    InsideTheParkHR,
    /// lined into double play
    LinedIntoDoublePlay,
    /// lined into triple play
    LinedIntoTriplePlay,
    /// manager challenge of call on the field
    ManagerChallenge,
    /// no double play credited for this play
    NoDoublePlay,
    /// obstruction (fielder obstructing a runner)
    Obstruction,
    /// a runner passed another runner and was called out
    RunnerPassedAnotherRunner,
    /// relay throw from the initial fielder to $ with no out made
    Relay(u8),
    /// runner interference
    RunnerInterference,
    /// sacrifice fly
    SacrificeFly,
    /// sacrifice hit (bunt)
    SacrificeHit,
    /// throw
    Throw,
    /// throw to base %
    ThrowToBase(u8),
    /// unspecified triple play
    UnspecifiedTriplePlay,
    /// umpire interference
    UmpireInterference,
    /// umpire review of call on the field
    UmpireReview,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// Information about a runner's attempt to advance.
pub enum AdvanceParameter {
    /// A throwing error, possibly with the base it was thrown to.
    ThrowingError(Fielder, Option<Base>),
    /// A fielding play.
    FieldingPlay(Vec<FieldParameter>),
    /// A run was unearned (due to player error).
    UnearnedRun,
    /// A team unearned run occurred.
    TeamUnearnedRun,
    /// An RBI was given.
    RBI,
    /// Interference by a fielder.
    Interference(Fielder),
    /// No RBI was given.
    NoRBI,
    /// The runner advanced, and there actually was a throw.
    WithThrow,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// A description of a batter advancing.
pub struct Advance {
    /// The base the runner is coming from.
    pub from: Base,
    /// The base the runner was advancing to.
    pub to: Base,
    /// Whether the runner successfully advanced.
    pub success: bool,
    /// Any other information about the running play.
    pub parameters: Vec<AdvanceParameter>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
/// A description of the full set of events that occurred during a play.
pub struct PlayEvent {
    /// The basic play description.
    pub description: PlayDescription,
    /// Fielding modifiers on the play description.
    pub modifiers: Vec<PlayModifier>,
    /// A description of batters advancing or attempting to advance.
    pub advances: Vec<Advance>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
/// A data record type. Currently only one (earned run) is supported.
pub enum DataEventType {
    /// How many earned runs a pitcher incurred.
    EarnedRuns,
}

impl From<&str> for DataEventType {
    fn from(bytes: &str) -> DataEventType {
        match bytes {
            "er" => DataEventType::EarnedRuns,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
/// A description of what hand someone uses to bat.
pub enum Hand {
    /// Left-handed batting.
    Left,
    /// Right-handed batting.
    Right,
}

#[derive(Clone, Debug, PartialEq)]
/// A record describing something about an MLB game.
pub enum Event {
    /// A twelve character ID record which identifies the date, home team, and number of the game.
    GameId {
        /// The game ID.
        id: String,
    },
    /// A record identifying version information. The version is obsolete and can be ignored.
    Version {
        /// The version of the game data.
        version: u8,
    },
    /// A record identifying game information.
    Info {
        /// The type of information in the record.
        key: Info,
        /// The information in the record. Just stored as a string, though it can describe a variety
        /// of data.
        data: String,
    },
    /// A record identifying a starter in the game.
    Start {
        /// The player starting.
        player: Player,
    },
    /// A record identifying a substitution of a player in the game.
    Sub {
        /// The player being subbed in.
        player: Player,
    },
    /// A record identifying a play occurring during a game.
    Play {
        /// The inning, an integer starting at 1
        inning: u8,
        /// The team which is batting.
        team: Team,
        /// The player at the plate.
        player: PlayerId,
        /// The count on the batter at the time of the play. Some older games may not have this
        /// information, hence why this is optional.
        count: Option<(u8, u8)>,
        /// The pitches the batter saw. Some games don't have this data, hence this may be empty.
        pitches: Vec<Pitch>,
        /// A description of the events of the play.
        event: PlayEvent,
    },
    /// A record describing some data about the game. Currently there's only earned run information
    /// but in the future more records may exist.
    Data {
        /// The data record type.
        data_type: DataEventType,
        /// The player to which the record applies.
        player: PlayerId,
        /// The actual data value.
        value: String,
    },
    /// A record describing a comment of some sort. Can be just an additional note to an unusual
    /// play, or a description of a review.
    Comment {
        /// The comment itself.
        comment: String,
    },
    /// A record describing a batting adjustment; i.e. when a batter bats from a side that is
    /// unexpected.
    BattingAdjustment {
        /// The player to whom the adjustment applies.
        player: PlayerId,
        /// The hand with which they are batting for this instance.
        hand: Hand,
    },
    /// A record describing a pitching adjustment; i.e. when a pitcher pitches from a side that is
    /// unexpected. This has actually only ever happened once, but it is possible.
    PitchingAdjustment {
        /// The player to whom the adjustment applies.
        player: PlayerId,
        /// The hand with which they are pitching for this instance.
        hand: Hand,
    },
    /// A record used when teams bat out of order. The normal assumption is that proper lineup
    /// sequence is followed, therefore, it is necessary to have some special indication when this
    /// is violated.
    LineupAdjustment {
        /// The team which batted out of order.
        team: Team,
        /// The batting order position.
        position: u8,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_info_parse() {
        assert_eq!(Info::VisitingTeam, "visteam".into());
        assert_eq!(Info::HomeTeam, "hometeam".into());
        assert_eq!(Info::Date, "date".into());
        assert_eq!(Info::Number, "number".into());
        assert_eq!(Info::Pitches, "pitches".into());
        assert_eq!(Info::UmpireHome, "umphome".into());
        assert_eq!(Info::Umpire1B, "ump1b".into());
        assert_eq!(Info::Umpire2B, "ump2b".into());
        assert_eq!(Info::Umpire3B, "ump3b".into());
        assert_eq!(Info::FieldCondition, "fieldcond".into());
        assert_eq!(Info::Precipitation, "precip".into());
        assert_eq!(Info::Sky, "sky".into());
        assert_eq!(Info::Temperature, "temp".into());
        assert_eq!(Info::WindDirection, "winddir".into());
        assert_eq!(Info::WindSpeed, "windspeed".into());
        assert_eq!(Info::GameLength, "timeofgame".into());
        assert_eq!(Info::Attendance, "attendence".into());
        assert_eq!(Info::WinningPitcher, "wp".into());
        assert_eq!(Info::LosingPitcher, "lp".into());
        assert_eq!(Info::SavePitcher, "save".into());
        assert_eq!(Info::GameWinningRBI, "gwrbi".into());
        assert_eq!(Info::EditTime, "edittime".into());
        assert_eq!(Info::ScoreMethod, "howscored".into());
        assert_eq!(Info::InputProgramVersion, "inputprogvers".into());
        assert_eq!(Info::Inputter, "inputter".into());
        assert_eq!(Info::InputTime, "inputtime".into());
        assert_eq!(Info::Scorer, "scorer".into());
        assert_eq!(Info::Translator, "translator".into());

        assert_eq!(Info::Unknown, "foobar".into());
        assert_eq!(Info::Unknown, "asdfasdf".into());
    }

    #[test]
    fn test_data_event_parse() {
        assert_eq!(DataEventType::EarnedRuns, "er".into());
    }

    #[test]
    #[should_panic]
    fn test_data_event_panic() {
        let _: DataEventType = ("foo").into();
    }

    #[test]
    #[should_panic]
    fn test_hit_location_panic() {
        let _: HitLocation = "foocmfadksl".into();
    }

    #[test]
    fn test_pitch_parse() {
        assert_eq!(Pitch::PickoffThrowCatcher, '+'.into());
        assert_eq!(Pitch::BlockedByCatcher, '*'.into());
        assert_eq!(Pitch::NonBatterPlay, '.'.into());
        assert_eq!(Pitch::PickoffFirst, '1'.into());
        assert_eq!(Pitch::PickoffSecond, '2'.into());
        assert_eq!(Pitch::PickoffThird, '3'.into());
        assert_eq!(Pitch::RunnerGoingOnPitch, '>'.into());
        assert_eq!(Pitch::Ball, 'B'.into());
        assert_eq!(Pitch::CalledStrike, 'C'.into());
        assert_eq!(Pitch::Foul, 'F'.into());
        assert_eq!(Pitch::HitBatter, 'H'.into());
        assert_eq!(Pitch::IntentionalBall, 'I'.into());
        assert_eq!(Pitch::UnknownStrike, 'K'.into());
        assert_eq!(Pitch::FoulBunt, 'L'.into());
        assert_eq!(Pitch::MissedBunt, 'M'.into());
        assert_eq!(Pitch::NoPitch, 'N'.into());
        assert_eq!(Pitch::FoulTipBunt, 'O'.into());
        assert_eq!(Pitch::Pitchout, 'P'.into());
        assert_eq!(Pitch::SwingOnPitchout, 'Q'.into());
        assert_eq!(Pitch::FoulOnPitchout, 'R'.into());
        assert_eq!(Pitch::SwingingStrike, 'S'.into());
        assert_eq!(Pitch::FoulTip, 'T'.into());
        assert_eq!(Pitch::BallPitcherWentToMouth, 'V'.into());
        assert_eq!(Pitch::BallInPlayBatter, 'X'.into());
        assert_eq!(Pitch::BallInPlayPitchout, 'Y'.into());

        assert_eq!(Pitch::Unknown, 'U'.into());
        assert_eq!(Pitch::Unknown, 'Z'.into());
        assert_eq!(Pitch::Unknown, 'A'.into());
    }
}
