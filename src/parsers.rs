use nom::{alphanumeric, digit, not_line_ending};

use std::str;

use ::event::{Event, Player, Team};

named!(bytes_to_u8 (&[u8]) -> u8, map_res!(map_res!(digit, str::from_utf8), str::parse::<u8>));

named!(event_play (&[u8]) -> Event, do_parse!(
    tag!("play") >>
    tag!(",") >>
    inning: map_res!(map_res!(digit, str::from_utf8), str::parse::<u8>) >>
    //tag!("\n") >>
    (Event::Play {
        inning: inning
    })
));

named!(info (&[u8]) -> Event, do_parse!(
    terminated!(tag!("info"), tag!(",")) >>
    key: map!(alphanumeric, Into::into) >>
    tag!(",") >>
    data: map!(map_res!(not_line_ending, str::from_utf8), String::from) >>
    (Event::Info {
        key: key,
        data: data
    })
));

named!(start (&[u8]) -> Event, do_parse!(
    terminated!(tag!("start"), tag!(",")) >>
    id: map!(map_res!(take_until_and_consume!(","), str::from_utf8), String::from) >>
    tag!("\"") >>
    name: map!(map_res!(take_until_and_consume!("\""), str::from_utf8), String::from) >>
    tag!(",") >>
    team: map_res!(map_res!(alt!(tag!("0") | tag!("1")), str::from_utf8), str::parse::<u8>) >>
    tag!(",") >>
    batting_pos: bytes_to_u8 >>
    tag!(",") >>
    fielding_pos: bytes_to_u8 >>
    (Event::Start { player: Player {
        id: id,
        name: name,
        team: if team == 0 { Team::Visiting } else { Team::Home },
        batting_pos: batting_pos,
        fielding_pos: fielding_pos,
    }})
));

named!(game_id (&[u8]) -> Event, do_parse!(
    tag!("id") >>
    tag!(",") >>
    id: map!(map_res!(take!(12), str::from_utf8), String::from) >>
    (Event::GameId { id: id })
));
named!(version (&[u8]) -> Event, do_parse!(
    tag!("version") >>
    tag!(",") >>
    version: bytes_to_u8 >>
    (Event::Version { version: version })
));

named!(pub event (&[u8]) -> Event, do_parse!(
    event: alt_complete!(game_id | version | event_play | info | start) >>
    alt!(eof!() | tag!("\n")) >>
    (event)
));

#[cfg(test)]
mod tests {
    use super::*;

    use nom::IResult::*;

    use ::event::{Event, Team};

    #[test]
    fn test_version() {
        assert_eq!((&[][..], Event::Version { version: 2 }), version(b"version,2").unwrap());
        assert!(version(b"asdlfk,3,5").is_err());
    }

    #[test]
    fn test_game_id() {
        assert_eq!((&[][..], Event::GameId { id: "CHN201604110".into() }), game_id(b"id,CHN201604110").unwrap());
        assert!(game_id(b"asdlfk,3,5").is_err());
        assert!(game_id(b"id,3455").is_incomplete());
    }

    #[test]
    fn test_start() {
        let player1 = Player {
            id: "fred103".into(),
            name: "fred".into(),
            team: Team::Home,
            batting_pos: 7,
            fielding_pos: 6,
        };
        let player2 = Player {
            id: "bob202".into(),
            name: "bob".into(),
            team: Team::Visiting,
            batting_pos: 3,
            fielding_pos: 9,
        };
        assert_eq!(Done(&[][..], Event::Start { player: player1 }), start(b"start,fred103,\"fred\",1,7,6"));
        assert_eq!(Done(&[][..], Event::Start { player: player2 }), start(b"start,bob202,\"bob\",0,3,9"));
        assert!(start(b"start,bob202,\"bob\",2,3,9").is_err());
        assert!(start(b"start,bob202").is_err());
    }
}