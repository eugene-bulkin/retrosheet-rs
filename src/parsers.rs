use nom::{alpha, alphanumeric, digit, not_line_ending};

use std::str;

use ::event::{Advance, AdvanceParameter, Base, Event, Fielder, FieldParameter, HitType, HitLocation,
              Pitch, Player, PlayDescription, PlayEvent, PlayModifier, Team};

named!(bytes_to_u8 (&[u8]) -> u8, map_res!(map_res!(digit, str::from_utf8), str::parse::<u8>));

named!(base (&[u8]) -> Base, alt_complete!(
    value!(Base::First, tag!("1")) |
    value!(Base::Second, tag!("2")) |
    value!(Base::Third, tag!("3")) |
    value!(Base::Home, tag!("B")) |
    value!(Base::Home, tag!("H"))
));

named!(fielder (&[u8]) -> Fielder, map!(one_of!("123456789"),
                                   |c: char| c.to_digit(10).unwrap() as Fielder));

named!(field_parameters (&[u8]) -> Vec<FieldParameter>, many1!(alt_complete!(
    value!(FieldParameter::Unknown, tag!("U")) |
    map!(preceded!(tag!("E"), fielder), FieldParameter::Error) |
    map!(fielder, FieldParameter::Play)
)));

named!(play_desc_gidp (&[u8]) -> PlayDescription, do_parse!(
    assists: many1!(fielder) >>
    tag!("(") >>
    first_out: base >>
    tag!(")") >>
    putout: fielder >>
    ({
        let mut fielders = assists;
        fielders.push(putout);
        PlayDescription::GIDP(fielders, first_out)
    })
));

named!(play_desc_fielding (&[u8]) -> PlayDescription, do_parse!(
    fielders: many1!(fielder) >>
    abnormal_putout: opt!(complete!(do_parse!(
        tag!("(") >>
        runner: base >>
        tag!(")") >>
        (runner)
    ))) >>
    (PlayDescription::FielderSequence(fielders, abnormal_putout))
));

named!(play_desc_strikeout (&[u8]) -> PlayDescription, do_parse!(
    tag!("K") >>
    additional: opt!(complete!(preceded!(opt!(tag!("+")),
                                         map!(play_description, Box::new)))) >>
    (PlayDescription::Strikeout(additional))
));

named!(play_desc_walk (&[u8]) -> PlayDescription, do_parse!(
    tag!("W") >>
    additional: opt!(complete!(preceded!(opt!(tag!("+")),
                                         map!(play_description, Box::new)))) >>
    (PlayDescription::Walk(additional))
));

named!(play_desc_hr (&[u8]) -> PlayDescription, do_parse!(
    alt_complete!(tag!("HR") | tag!("H")) >>
    fielders: many0!(complete!(fielder)) >>
    ({
        if fielders.is_empty() {
            PlayDescription::HomeRun
        } else {
            PlayDescription::InsideTheParkHomeRun(fielders)
        }
    })
));

named!(play_description (&[u8]) -> PlayDescription, alt_complete!(
    map!(preceded!(tag!("SB"), base), PlayDescription::StolenBase) |
    value!(PlayDescription::HitByPitch, tag!("HP")) |
    value!(PlayDescription::Balk, tag!("BK")) |
    value!(PlayDescription::PassedBall, tag!("PB")) |
    value!(PlayDescription::WildPitch, tag!("WP")) |
    value!(PlayDescription::NoPlay, tag!("NP")) |
    map!(preceded!(tag!("FLE"), fielder), PlayDescription::FoulFlyBallError) |
    map!(preceded!(tag!("E"), fielder), PlayDescription::Error) |
    map!(preceded!(tag!("FC"), fielder), PlayDescription::FieldersChoice) |
    map!(preceded!(tag!("S"), many0!(fielder)), PlayDescription::Single) |
    map!(preceded!(tag!("D"), many0!(fielder)), PlayDescription::Double) |
    map!(preceded!(tag!("T"), many0!(fielder)), PlayDescription::Triple) |
    play_desc_hr |
    play_desc_strikeout |
    play_desc_walk |
    complete!(play_desc_gidp) |
    play_desc_fielding
));

named!(hit_location_mods (&[u8]) -> &[u8], alt_complete!(
    tag!("LDF") | tag!("LSF") | tag!("LF") | tag!("LS") | tag!("LD") | tag!("XD") | tag!("DF") |
    tag!("MS") | tag!("MD") | tag!("S") | tag!("F") | tag!("M") | tag!("D") | tag!("L")
));

named!(hit_location (&[u8]) -> HitLocation, map!(recognize!(pair!(digit, opt!(complete!(hit_location_mods)))), Into::into));

named!(hit_with_location (&[u8]) -> PlayModifier, do_parse!(
    hit_type: alt_complete!(
        value!(HitType::PopFly, tag!("P")) |
        value!(HitType::PopUpBunt, tag!("BP")) |
        value!(HitType::Fly, tag!("F")) |
        value!(HitType::GroundBall, tag!("G")) |
        value!(HitType::GroundBallBunt, tag!("BG")) |
        value!(HitType::LineDrive, tag!("L"))
    ) >>
    hit_location: opt!(complete!(hit_location)) >>
    (PlayModifier::HitWithLocation(hit_type, hit_location))
));

named!(modifier (&[u8]) -> PlayModifier, do_parse!(
    m: alt_complete!(
        value!(PlayModifier::AppealPlay, tag!("AP")) |
        value!(PlayModifier::BuntGroundedIntoDoublePlay, tag!("BGDP")) |
        value!(PlayModifier::BatterInterference, tag!("BINT")) |
        value!(PlayModifier::LineDriveBunt, tag!("BL")) |
        value!(PlayModifier::BattingOutOfTurn, tag!("BOOT")) |
        value!(PlayModifier::BuntPoppedIntoDoublePlay, tag!("BPDP")) |
        value!(PlayModifier::RunnerHitByBattedBall, tag!("BR")) |
        value!(PlayModifier::CourtesyBatter, tag!("COUB")) |
        value!(PlayModifier::CourtesyFielder, tag!("COUF")) |
        value!(PlayModifier::CourtesyRunner, tag!("COUR")) |
        value!(PlayModifier::CalledThirdStrike, tag!("C")) |
        value!(PlayModifier::UnspecifiedDoublePlay, tag!("DP")) |
        map!(preceded!(tag!("E"), bytes_to_u8), PlayModifier::Error) |
        value!(PlayModifier::FlyBallDoublePlay, tag!("FDP")) |
        value!(PlayModifier::FanInterference, tag!("FINT")) |
        value!(PlayModifier::Foul, tag!("FL")) |
        value!(PlayModifier::ForceOut, tag!("FO")) |
        value!(PlayModifier::GroundBallDoublePlay, tag!("GDP")) |
        value!(PlayModifier::GroundBallTriplePlay, tag!("GTP")) |
        value!(PlayModifier::InfieldFlyRule, tag!("IF")) |
        value!(PlayModifier::Interference, tag!("INT")) |
        value!(PlayModifier::InsideTheParkHR, tag!("IPHR")) |
        value!(PlayModifier::LinedIntoDoublePlay, tag!("LDP")) |
        value!(PlayModifier::LinedIntoTriplePlay, tag!("LTP")) |
        value!(PlayModifier::ManagerChallenge, tag!("MREV")) |
        value!(PlayModifier::NoDoublePlay, tag!("NDP")) |
        value!(PlayModifier::Obstruction, tag!("OBS")) |
        value!(PlayModifier::RunnerPassedAnotherRunner, tag!("PASS")) |
        map!(preceded!(tag!("R"), bytes_to_u8), PlayModifier::Relay) |
        value!(PlayModifier::RunnerInterference, tag!("RINT")) |
        value!(PlayModifier::SacrificeFly, tag!("SF")) |
        value!(PlayModifier::SacrificeHit, tag!("SH")) |
        map!(preceded!(tag!("TH"), bytes_to_u8), PlayModifier::ThrowToBase) |
        value!(PlayModifier::Throw, tag!("TH")) |
        value!(PlayModifier::UnspecifiedTriplePlay, tag!("TP")) |
        value!(PlayModifier::UmpireInterference, tag!("UINT")) |
        value!(PlayModifier::UmpireReview, tag!("UREV")) |
        complete!(hit_with_location) |
        map!(complete!(hit_location), PlayModifier::HitLocation)
    ) >>
    // This is undocumented on the guide... no idea what this is.
    opt!(complete!(one_of!("+-"))) >>
    (m)
));

named!(advance_parameter (&[u8]) -> AdvanceParameter, do_parse!(
    tag!("(") >>
    param: alt!(
        value!(AdvanceParameter::UnearnedRun, tag!("UR")) |
        value!(AdvanceParameter::NoRBI, tag!("NR")) |
        value!(AdvanceParameter::NoRBI, tag!("NORBI")) |
        value!(AdvanceParameter::RBI, tag!("RBI")) |
        value!(AdvanceParameter::TeamUnearnedRun, tag!("TUR")) |
        map!(terminated!(fielder, tag!("/INT")), AdvanceParameter::Interference) |
        do_parse!(
            tag!("E") >>
            f: fielder >>
            tag!("/TH") >>
            base: opt!(base) >>
            (AdvanceParameter::ThrowingError(f, base))
        ) |
        value!(AdvanceParameter::WithThrow, tag!("TH")) |
        map!(field_parameters, AdvanceParameter::FieldingPlay)
    ) >>
    tag!(")") >>
    (param)
));

named!(advance (&[u8]) -> Advance, do_parse!(
    from: base >>
    success: alt!(map!(tag!("-"), |_| true) |
                  map!(tag!("X"), |_| false)) >>
    to: base >>
    parameters: many0!(advance_parameter) >>
    (Advance {
        from: from,
        to: to,
        success: success,
        parameters: parameters,
    })
));

named!(play_event (&[u8]) -> PlayEvent, do_parse!(
    play_desc: complete!(play_description) >>
    modifiers: many0!(preceded!(tag!("/"), complete!(modifier))) >>
    advances: opt!(complete!(preceded!(tag!("."),
                             separated_list!(tag!(";"),
                                             advance)))) >>
    (PlayEvent {
        description: play_desc,
        modifiers: modifiers,
        advances: advances.unwrap_or(vec![]),
    })
));

named!(pitch (&[u8]) -> Pitch, map!(none_of!(","), Into::into));

named!(team (&[u8]) -> Team,
    alt!(map!(tag!("0"), |_| Team::Visiting) |
         map!(tag!("1"), |_| Team::Home))
);

named!(pitch_count (&[u8]) -> Option<(u8, u8)>, do_parse!(
    balls: one_of!("?0123") >>
    strikes: one_of!("?012") >>
    ({
        if balls == '?' || strikes == '?' {
            None
        } else {
            Some((balls.to_digit(10).unwrap() as u8,
                  strikes.to_digit(10).unwrap() as u8))
        }
    })
));

named!(play (&[u8]) -> Event, do_parse!(
    terminated!(tag!("play"), tag!(",")) >>
    inning: map_res!(map_res!(digit, str::from_utf8), str::parse::<u8>) >>
    tag!(",") >>
    team: terminated!(team, tag!(",")) >>
    player: map!(map_res!(take_until_and_consume!(","), str::from_utf8), String::from) >>
    count: terminated!(pitch_count, tag!(",")) >>
    pitches: many0!(pitch) >>
    tag!(",") >>
    play_event: play_event >>
    (Event::Play {
        inning: inning,
        team: team,
        player: player,
        count: count,
        pitches: pitches,
        event: play_event,
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

named!(player_entry (&[u8]) -> Player, do_parse!(
    id: map!(map_res!(take_until_and_consume!(","), str::from_utf8), String::from) >>
    tag!("\"") >>
    name: map!(map_res!(take_until_and_consume!("\""), str::from_utf8), String::from) >>
    tag!(",") >>
    team: terminated!(team, tag!(",")) >>
    batting_pos: bytes_to_u8 >>
    tag!(",") >>
    fielding_pos: bytes_to_u8 >>
    (Player {
        id: id,
        name: name,
        team: team,
        batting_pos: batting_pos,
        fielding_pos: fielding_pos,
    })
));

named!(start (&[u8]) -> Event, do_parse!(
    terminated!(tag!("start"), tag!(",")) >>
    player: player_entry >>
    (Event::Start { player: player })
));

named!(sub (&[u8]) -> Event, do_parse!(
    terminated!(tag!("sub"), tag!(",")) >>
    player: player_entry >>
    (Event::Sub { player: player })
));

named!(data (&[u8]) -> Event, do_parse!(
    terminated!(tag!("data"), tag!(",")) >>
    data_type: terminated!(map!(alpha, Into::into), tag!(",")) >>
    player: map!(map_res!(take_until_and_consume!(","), str::from_utf8), String::from) >>
    value: map!(map_res!(not_line_ending, str::from_utf8), String::from) >>
    (Event::Data {
        data_type: data_type,
        player: player,
        value: value,
    })
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
    event: alt_complete!(game_id | version | play | info | start | sub | data) >>
    alt!(eof!() | tag!("\n") | tag!("\r\n")) >>
    (event)
));

#[cfg(test)]
mod tests {
    use super::*;

    use nom::IResult::*;

    use ::event::{Advance, AdvanceParameter, Base, DataEventType, Event, FieldParameter, HitType,
                  HitLocation, Pitch, PlayEvent, PlayDescription, PlayModifier, Team};

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
    fn test_data() {
        assert_eq!(Done(&[][..], Event::Data {
            data_type: DataEventType::EarnedRuns,
            player: "showe001".into(),
            value: "2".into(),
        }), data(b"data,er,showe001,2"));
    }

    #[test]
    fn test_start_sub() {
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
        assert_eq!(Done(&[][..], Event::Start { player: player1.clone() }), start(b"start,fred103,\"fred\",1,7,6"));
        assert_eq!(Done(&[][..], Event::Start { player: player2.clone() }), start(b"start,bob202,\"bob\",0,3,9"));
        assert!(start(b"start,bob202,\"bob\",2,3,9").is_err());
        assert!(start(b"start,bob202").is_err());

        assert_eq!(Done(&[][..], Event::Sub { player: player1.clone() }), sub(b"sub,fred103,\"fred\",1,7,6"));
        assert_eq!(Done(&[][..], Event::Sub { player: player2.clone() }), sub(b"sub,bob202,\"bob\",0,3,9"));
        assert!(sub(b"sub,bob202,\"bob\",2,3,9").is_err());
        assert!(sub(b"sub,bob202").is_err());
    }

    #[test]
    fn test_hit_location() {
        assert_eq!(Done(&[][..], HitLocation::_2F), hit_location(b"2F"));
        assert_eq!(Done(&[][..], HitLocation::_2), hit_location(b"2"));
        assert_eq!(Done(&[][..], HitLocation::_25F), hit_location(b"25F"));
        assert_eq!(Done(&[][..], HitLocation::_25), hit_location(b"25"));
        assert_eq!(Done(&[][..], HitLocation::_1S), hit_location(b"1S"));
        assert_eq!(Done(&[][..], HitLocation::_23), hit_location(b"23"));
        assert_eq!(Done(&[][..], HitLocation::_23F), hit_location(b"23F"));
        assert_eq!(Done(&[][..], HitLocation::_15), hit_location(b"15"));
        assert_eq!(Done(&[][..], HitLocation::_1), hit_location(b"1"));
        assert_eq!(Done(&[][..], HitLocation::_13), hit_location(b"13"));
        assert_eq!(Done(&[][..], HitLocation::_5S), hit_location(b"5S"));
        assert_eq!(Done(&[][..], HitLocation::_56S), hit_location(b"56S"));
        assert_eq!(Done(&[][..], HitLocation::_6S), hit_location(b"6S"));
        assert_eq!(Done(&[][..], HitLocation::_6MS), hit_location(b"6MS"));
        assert_eq!(Done(&[][..], HitLocation::_4MS), hit_location(b"4MS"));
        assert_eq!(Done(&[][..], HitLocation::_4S), hit_location(b"4S"));
        assert_eq!(Done(&[][..], HitLocation::_34S), hit_location(b"34S"));
        assert_eq!(Done(&[][..], HitLocation::_3S), hit_location(b"3S"));
        assert_eq!(Done(&[][..], HitLocation::_5F), hit_location(b"5F"));
        assert_eq!(Done(&[][..], HitLocation::_5), hit_location(b"5"));
        assert_eq!(Done(&[][..], HitLocation::_56), hit_location(b"56"));
        assert_eq!(Done(&[][..], HitLocation::_6), hit_location(b"6"));
        assert_eq!(Done(&[][..], HitLocation::_6M), hit_location(b"6M"));
        assert_eq!(Done(&[][..], HitLocation::_4M), hit_location(b"4M"));
        assert_eq!(Done(&[][..], HitLocation::_4), hit_location(b"4"));
        assert_eq!(Done(&[][..], HitLocation::_34), hit_location(b"34"));
        assert_eq!(Done(&[][..], HitLocation::_3), hit_location(b"3"));
        assert_eq!(Done(&[][..], HitLocation::_3F), hit_location(b"3F"));
        assert_eq!(Done(&[][..], HitLocation::_5DF), hit_location(b"5DF"));
        assert_eq!(Done(&[][..], HitLocation::_5D), hit_location(b"5D"));
        assert_eq!(Done(&[][..], HitLocation::_56D), hit_location(b"56D"));
        assert_eq!(Done(&[][..], HitLocation::_6D), hit_location(b"6D"));
        assert_eq!(Done(&[][..], HitLocation::_6MD), hit_location(b"6MD"));
        assert_eq!(Done(&[][..], HitLocation::_4MD), hit_location(b"4MD"));
        assert_eq!(Done(&[][..], HitLocation::_4D), hit_location(b"4D"));
        assert_eq!(Done(&[][..], HitLocation::_34D), hit_location(b"34D"));
        assert_eq!(Done(&[][..], HitLocation::_3D), hit_location(b"3D"));
        assert_eq!(Done(&[][..], HitLocation::_3DF), hit_location(b"3DF"));
        assert_eq!(Done(&[][..], HitLocation::_7LSF), hit_location(b"7LSF"));
        assert_eq!(Done(&[][..], HitLocation::_7LS), hit_location(b"7LS"));
        assert_eq!(Done(&[][..], HitLocation::_7S), hit_location(b"7S"));
        assert_eq!(Done(&[][..], HitLocation::_78S), hit_location(b"78S"));
        assert_eq!(Done(&[][..], HitLocation::_8S), hit_location(b"8S"));
        assert_eq!(Done(&[][..], HitLocation::_89S), hit_location(b"89S"));
        assert_eq!(Done(&[][..], HitLocation::_9S), hit_location(b"9S"));
        assert_eq!(Done(&[][..], HitLocation::_9LS), hit_location(b"9LS"));
        assert_eq!(Done(&[][..], HitLocation::_9LSF), hit_location(b"9LSF"));
        assert_eq!(Done(&[][..], HitLocation::_7LF), hit_location(b"7LF"));
        assert_eq!(Done(&[][..], HitLocation::_7L), hit_location(b"7L"));
        assert_eq!(Done(&[][..], HitLocation::_7), hit_location(b"7"));
        assert_eq!(Done(&[][..], HitLocation::_78), hit_location(b"78"));
        assert_eq!(Done(&[][..], HitLocation::_8), hit_location(b"8"));
        assert_eq!(Done(&[][..], HitLocation::_89), hit_location(b"89"));
        assert_eq!(Done(&[][..], HitLocation::_9), hit_location(b"9"));
        assert_eq!(Done(&[][..], HitLocation::_9L), hit_location(b"9L"));
        assert_eq!(Done(&[][..], HitLocation::_9LF), hit_location(b"9LF"));
        assert_eq!(Done(&[][..], HitLocation::_7LDF), hit_location(b"7LDF"));
        assert_eq!(Done(&[][..], HitLocation::_7LD), hit_location(b"7LD"));
        assert_eq!(Done(&[][..], HitLocation::_7D), hit_location(b"7D"));
        assert_eq!(Done(&[][..], HitLocation::_78D), hit_location(b"78D"));
        assert_eq!(Done(&[][..], HitLocation::_8D), hit_location(b"8D"));
        assert_eq!(Done(&[][..], HitLocation::_89D), hit_location(b"89D"));
        assert_eq!(Done(&[][..], HitLocation::_9D), hit_location(b"9D"));
        assert_eq!(Done(&[][..], HitLocation::_9LD), hit_location(b"9LD"));
        assert_eq!(Done(&[][..], HitLocation::_9LDF), hit_location(b"9LDF"));
        assert_eq!(Done(&[][..], HitLocation::_78XD), hit_location(b"78XD"));
        assert_eq!(Done(&[][..], HitLocation::_8XD), hit_location(b"8XD"));
        assert_eq!(Done(&[][..], HitLocation::_89XD), hit_location(b"89XD"));
    }

    #[test]
    fn test_modifier() {
        assert_eq!(Done(&[][..], PlayModifier::HitWithLocation(HitType::PopUpBunt, None)), modifier(b"BP"));
        assert_eq!(Done(&[][..], PlayModifier::HitWithLocation(HitType::GroundBallBunt, Some(HitLocation::_9LF))), modifier(b"BG9LF"));
        assert_eq!(Done(&[][..], PlayModifier::HitWithLocation(HitType::Fly, Some(HitLocation::_89S))), modifier(b"F89S"));
        //        assert_eq!(Done(&[][..], PlayModifier::HitWithLocation(HitType::GroundBall, Some(HitLocation::_13))), modifier(b"G13"));
        assert_eq!(Done(&[][..], PlayModifier::HitWithLocation(HitType::LineDrive, None)), modifier(b"L"));
        assert_eq!(Done(&[][..], PlayModifier::HitWithLocation(HitType::PopFly, Some(HitLocation::_4MS))), modifier(b"P4MS"));

        assert_eq!(Done(&[][..], PlayModifier::AppealPlay), modifier(b"AP"));
        assert_eq!(Done(&[][..], PlayModifier::BuntGroundedIntoDoublePlay), modifier(b"BGDP"));
        assert_eq!(Done(&[][..], PlayModifier::BatterInterference), modifier(b"BINT"));
        assert_eq!(Done(&[][..], PlayModifier::LineDriveBunt), modifier(b"BL"));
        assert_eq!(Done(&[][..], PlayModifier::BattingOutOfTurn), modifier(b"BOOT"));
        assert_eq!(Done(&[][..], PlayModifier::BuntPoppedIntoDoublePlay), modifier(b"BPDP"));
        assert_eq!(Done(&[][..], PlayModifier::RunnerHitByBattedBall), modifier(b"BR"));
        assert_eq!(Done(&[][..], PlayModifier::CourtesyBatter), modifier(b"COUB"));
        assert_eq!(Done(&[][..], PlayModifier::CourtesyFielder), modifier(b"COUF"));
        assert_eq!(Done(&[][..], PlayModifier::CourtesyRunner), modifier(b"COUR"));
        assert_eq!(Done(&[][..], PlayModifier::CalledThirdStrike), modifier(b"C"));
        assert_eq!(Done(&[][..], PlayModifier::UnspecifiedDoublePlay), modifier(b"DP"));
        assert_eq!(Done(&[][..], PlayModifier::Error(3)), modifier(b"E3"));
        assert_eq!(Done(&[][..], PlayModifier::Error(7)), modifier(b"E7"));
        assert_eq!(Done(&[][..], PlayModifier::FlyBallDoublePlay), modifier(b"FDP"));
        assert_eq!(Done(&[][..], PlayModifier::FanInterference), modifier(b"FINT"));
        assert_eq!(Done(&[][..], PlayModifier::Foul), modifier(b"FL"));
        assert_eq!(Done(&[][..], PlayModifier::ForceOut), modifier(b"FO"));
        assert_eq!(Done(&[][..], PlayModifier::GroundBallDoublePlay), modifier(b"GDP"));
        assert_eq!(Done(&[][..], PlayModifier::GroundBallTriplePlay), modifier(b"GTP"));
        assert_eq!(Done(&[][..], PlayModifier::InfieldFlyRule), modifier(b"IF"));
        assert_eq!(Done(&[][..], PlayModifier::Interference), modifier(b"INT"));
        assert_eq!(Done(&[][..], PlayModifier::InsideTheParkHR), modifier(b"IPHR"));
        assert_eq!(Done(&[][..], PlayModifier::LinedIntoDoublePlay), modifier(b"LDP"));
        assert_eq!(Done(&[][..], PlayModifier::LinedIntoTriplePlay), modifier(b"LTP"));
        assert_eq!(Done(&[][..], PlayModifier::ManagerChallenge), modifier(b"MREV"));
        assert_eq!(Done(&[][..], PlayModifier::NoDoublePlay), modifier(b"NDP"));
        assert_eq!(Done(&[][..], PlayModifier::Obstruction), modifier(b"OBS"));
        assert_eq!(Done(&[][..], PlayModifier::RunnerPassedAnotherRunner), modifier(b"PASS"));
        assert_eq!(Done(&[][..], PlayModifier::Relay(6)), modifier(b"R6"));
        assert_eq!(Done(&[][..], PlayModifier::Relay(5)), modifier(b"R5"));
        assert_eq!(Done(&[][..], PlayModifier::RunnerInterference), modifier(b"RINT"));
        assert_eq!(Done(&[][..], PlayModifier::SacrificeFly), modifier(b"SF"));
        assert_eq!(Done(&[][..], PlayModifier::SacrificeHit), modifier(b"SH"));
        assert_eq!(Done(&[][..], PlayModifier::Throw), modifier(b"TH"));
        assert_eq!(Done(&[][..], PlayModifier::ThrowToBase(2)), modifier(b"TH2"));
        assert_eq!(Done(&[][..], PlayModifier::ThrowToBase(3)), modifier(b"TH3"));
        assert_eq!(Done(&[][..], PlayModifier::UnspecifiedTriplePlay), modifier(b"TP"));
        assert_eq!(Done(&[][..], PlayModifier::UmpireInterference), modifier(b"UINT"));
        assert_eq!(Done(&[][..], PlayModifier::UmpireReview), modifier(b"UREV"));
    }

    #[test]
    fn test_advance() {
        assert_eq!(Done(&[][..], Advance {
            from: Base::Second,
            to: Base::Third,
            success: true,
            parameters: vec![]
        }), advance(b"2-3"));
        assert_eq!(Done(&[][..], Advance {
            from: Base::First,
            to: Base::Second,
            success: false,
            parameters: vec![AdvanceParameter::FieldingPlay(vec![
                FieldParameter::Play(2),
                FieldParameter::Play(6),
            ])],
        }), advance(b"1X2(26)"));
        assert_eq!(Done(&[][..], Advance {
            from: Base::Home,
            to: Base::Second,
            success: false,
            parameters: vec![AdvanceParameter::FieldingPlay(vec![
                FieldParameter::Play(8),
                FieldParameter::Play(4),
                FieldParameter::Play(3),
                FieldParameter::Play(4),
            ])],
        }), advance(b"BX2(8434)"));
        assert_eq!(Done(&[][..], Advance {
            from: Base::Home,
            to: Base::Second,
            success: false,
            parameters: vec![AdvanceParameter::FieldingPlay(vec![
                FieldParameter::Play(7),
                FieldParameter::Error(4),
            ])],
        }), advance(b"BX2(7E4)"));
        assert_eq!(Done(&[][..], Advance {
            from: Base::First,
            to: Base::Third,
            success: true,
            parameters: vec![AdvanceParameter::ThrowingError(5, None)],
        }), advance(b"1-3(E5/TH)"));
        assert_eq!(Done(&[][..], Advance {
            from: Base::Second,
            to: Base::Home,
            success: true,
            parameters: vec![
                AdvanceParameter::ThrowingError(4, None),
                AdvanceParameter::UnearnedRun,
                AdvanceParameter::NoRBI
            ],
        }), advance(b"2-H(E4/TH)(UR)(NR)"));
        assert_eq!(Done(&[][..], Advance {
            from: Base::Second,
            to: Base::Home,
            success: true,
            parameters: vec![
                AdvanceParameter::ThrowingError(4, None),
                AdvanceParameter::UnearnedRun,
                AdvanceParameter::NoRBI
            ],
        }), advance(b"2-H(E4/TH)(UR)(NORBI)"));
        assert_eq!(Done(&[][..], Advance {
            from: Base::Third,
            to: Base::Home,
            success: true,
            parameters: vec![
                AdvanceParameter::RBI
            ],
        }), advance(b"3-H(RBI)"));
        assert_eq!(Done(&[][..], Advance {
            from: Base::Second,
            to: Base::Third,
            success: false,
            parameters: vec![
                AdvanceParameter::Interference(5)
            ],
        }), advance(b"2X3(5/INT)"));
        assert_eq!(Done(&[][..], Advance {
            from: Base::Second,
            to: Base::Home,
            success: true,
            parameters: vec![
                AdvanceParameter::TeamUnearnedRun
            ],
        }), advance(b"2-H(TUR)"));
        assert_eq!(Done(&[][..], Advance {
            from: Base::Home,
            to: Base::Second,
            success: false,
            parameters: vec![
                AdvanceParameter::FieldingPlay(vec![
                    FieldParameter::Play(8),
                    FieldParameter::Unknown,
                    FieldParameter::Play(3),
                ])
            ],
        }), advance(b"BX2(8U3)"));
        assert_eq!(Done(&[][..], Advance {
            from: Base::Home,
            to: Base::Second,
            success: true,
            parameters: vec![
                AdvanceParameter::WithThrow
            ],
        }), advance(b"B-2(TH)"));
    }

    #[test]
    fn test_play_description() {
        let desc1 = PlayDescription::FielderSequence(vec![2, 3], None);
        let desc2 = PlayDescription::Strikeout(Some(Box::new(PlayDescription::FielderSequence(vec![2, 3], None))));
        let desc3 = PlayDescription::Strikeout(Some(Box::new(PlayDescription::PassedBall)));
        let desc4 = PlayDescription::Strikeout(Some(Box::new(PlayDescription::WildPitch)));
        let desc5 = PlayDescription::Walk(Some(Box::new(PlayDescription::WildPitch)));
        assert_eq!(Done(&[][..], PlayDescription::FielderSequence(vec![5], None)), play_description(b"5"));
        assert_eq!(Done(&[][..], desc1), play_description(b"23"));
        assert_eq!(Done(&[][..], PlayDescription::Balk), play_description(b"BK"));
        assert_eq!(Done(&[][..], PlayDescription::PassedBall), play_description(b"PB"));
        assert_eq!(Done(&[][..], PlayDescription::WildPitch), play_description(b"WP"));
        assert_eq!(Done(&[][..], PlayDescription::Strikeout(None)), play_description(b"K"));
        assert_eq!(Done(&[][..], desc2), play_description(b"K23"));
        assert_eq!(Done(&[][..], desc3), play_description(b"K+PB"));
        assert_eq!(Done(&[][..], desc4), play_description(b"K+WP"));
        assert_eq!(Done(&[][..], PlayDescription::Error(3)), play_description(b"E3"));
        assert_eq!(Done(&[][..], PlayDescription::FoulFlyBallError(3)), play_description(b"FLE3"));
        assert_eq!(Done(&[][..], PlayDescription::Single(vec![])), play_description(b"S"));
        assert_eq!(Done(&[][..], PlayDescription::Double(vec![])), play_description(b"D"));
        assert_eq!(Done(&[][..], PlayDescription::Triple(vec![])), play_description(b"T"));
        assert_eq!(Done(&[][..], PlayDescription::Single(vec![3])), play_description(b"S3"));
        assert_eq!(Done(&[][..], PlayDescription::Double(vec![7])), play_description(b"D7"));
        assert_eq!(Done(&[][..], PlayDescription::Triple(vec![6])), play_description(b"T6"));
        assert_eq!(Done(&[][..], PlayDescription::Single(vec![3, 4])), play_description(b"S34"));
        assert_eq!(Done(&[][..], PlayDescription::Double(vec![9, 7])), play_description(b"D97"));
        assert_eq!(Done(&[][..], PlayDescription::Triple(vec![5, 6])), play_description(b"T56"));
        assert_eq!(Done(&[][..], PlayDescription::HomeRun), play_description(b"H"));
        assert_eq!(Done(&[][..], PlayDescription::HomeRun), play_description(b"HR"));
        assert_eq!(Done(&[][..], PlayDescription::InsideTheParkHomeRun(vec![3, 4])), play_description(b"H34"));
        assert_eq!(Done(&[][..], PlayDescription::InsideTheParkHomeRun(vec![3, 4])), play_description(b"HR34"));
        assert_eq!(Done(&[][..], PlayDescription::Walk(None)), play_description(b"W"));
        assert_eq!(Done(&[][..], desc5), play_description(b"W+WP"));
        assert_eq!(Done(&[][..], PlayDescription::HitByPitch), play_description(b"HP"));
        assert_eq!(Done(&[][..], PlayDescription::NoPlay), play_description(b"NP"));
        assert_eq!(Done(&[][..], PlayDescription::StolenBase(Base::Third)), play_description(b"SB3"));
    }

    #[test]
    fn test_play_event() {
        let event1 = PlayEvent {
            description: PlayDescription::FielderSequence(vec![2, 3], None),
            modifiers: vec![PlayModifier::HitWithLocation(HitType::GroundBall, None)],
            advances: vec![Advance {
                from: Base::First,
                to: Base::Second,
                success: true,
                parameters: vec![],
            }]
        };
        let event2 = PlayEvent {
            description: PlayDescription::FieldersChoice(2),
            modifiers: vec![PlayModifier::HitWithLocation(HitType::GroundBall, None)],
            advances: vec![Advance {
                from: Base::Second,
                to: Base::Third,
                success: false,
                parameters: vec![AdvanceParameter::FieldingPlay(vec![
                    FieldParameter::Play(2),
                    FieldParameter::Play(6),
                    FieldParameter::Play(5),
                ])],
            }, Advance {
                from: Base::Home,
                to: Base::Second,
                success: true,
                parameters: vec![AdvanceParameter::WithThrow],
            }]
        };
        let event3 = PlayEvent {
            description: PlayDescription::Single(vec![8]),
            modifiers: vec![],
            advances: vec![Advance {
                from: Base::Second,
                to: Base::Home,
                success: true,
                parameters: vec![],
            }, Advance {
                from: Base::Home,
                to: Base::Second,
                success: false,
                parameters: vec![AdvanceParameter::FieldingPlay(vec![
                    FieldParameter::Play(8),
                    FieldParameter::Unknown,
                    FieldParameter::Play(3),
                ])],
            }]
        };
        let event4 = PlayEvent {
            description: PlayDescription::Single(vec![]),
            modifiers: vec![PlayModifier::HitWithLocation(HitType::LineDrive, Some(HitLocation::_9S))],
            advances: vec![Advance {
                from: Base::Third,
                to: Base::Home,
                success: true,
                parameters: vec![],
            }, Advance {
                from: Base::Second,
                to: Base::Third,
                success: false,
                parameters: vec![AdvanceParameter::Interference(5)],
            }, Advance {
                from: Base::First,
                to: Base::Second,
                success: true,
                parameters: vec![],
            }]
        };
        let event5 = PlayEvent {
            description: PlayDescription::FielderSequence(vec![5, 4], Some(Base::First)),
            modifiers: vec![
                PlayModifier::ForceOut,
                PlayModifier::HitWithLocation(HitType::GroundBall, Some(HitLocation::_5))
            ],
            advances: vec![Advance {
                from: Base::Third,
                to: Base::Home,
                success: true,
                parameters: vec![],
            }, Advance {
                from: Base::Home,
                to: Base::First,
                success: true,
                parameters: vec![],
            }]
        };

        assert_eq!(Done(&[][..], event1), play_event(b"23/G-.1-2"));
        assert_eq!(Done(&[][..], event2), play_event(b"FC2/G.2X3(265);B-2(TH)"));
        assert_eq!(Done(&[][..], event3), play_event(b"S8.2-H;BX2(8U3)"));
        assert_eq!(Done(&[][..], event4), play_event(b"S/L9S.3-H;2X3(5/INT);1-2"));
        assert_eq!(Done(&[][..], event5), play_event(b"54(1)/FO/G5.3-H;B-1"));
    }

    #[test]
    fn test_play() {
        let play1 = b"play,8,0,philb001,12,FBS1FX,23/G-.1-2";
        let play2 = b"play,7,0,finnb001,01,LX,FC2/G.2X3(265);B-2(TH)";
        let play3 = b"play,6,1,heywj001,??,CBFBBS,K";
        let play4 = b"play,3,0,hamib001,12,FCBX,HR/7/F";

        let parsed1 = Event::Play {
            inning: 8,
            team: Team::Visiting,
            player: "philb001".into(),
            count: Some((1, 2)),
            pitches: vec![
                Pitch::Foul,
                Pitch::Ball,
                Pitch::SwingingStrike,
                Pitch::PickoffFirst,
                Pitch::Foul,
                Pitch::BallInPlayBatter,
            ],
            event: PlayEvent {
                description: PlayDescription::FielderSequence(vec![2, 3], None),
                modifiers: vec![PlayModifier::HitWithLocation(HitType::GroundBall, None)],
                advances: vec![Advance {
                    from: Base::First,
                    to: Base::Second,
                    success: true,
                    parameters: vec![],
                }]
            }
        };
        let parsed2 = Event::Play {
            inning: 7,
            team: Team::Visiting,
            player: "finnb001".into(),
            count: Some((0, 1)),
            pitches: vec![
                Pitch::FoulBunt,
                Pitch::BallInPlayBatter,
            ],
            event: PlayEvent {
                description: PlayDescription::FieldersChoice(2),
                modifiers: vec![PlayModifier::HitWithLocation(HitType::GroundBall, None)],
                advances: vec![Advance {
                    from: Base::Second,
                    to: Base::Third,
                    success: false,
                    parameters: vec![AdvanceParameter::FieldingPlay(vec![
                        FieldParameter::Play(2),
                        FieldParameter::Play(6),
                        FieldParameter::Play(5),
                    ])],
                }, Advance {
                    from: Base::Home,
                    to: Base::Second,
                    success: true,
                    parameters: vec![AdvanceParameter::WithThrow],
                }]
            }
        };
        let parsed3 = Event::Play {
            inning: 6,
            team: Team::Home,
            player: "heywj001".into(),
            count: None,
            pitches: vec![Pitch::CalledStrike, Pitch::Ball, Pitch::Foul, Pitch::Ball, Pitch::Ball, Pitch::SwingingStrike],
            event: PlayEvent {
                description: PlayDescription::Strikeout(None),
                modifiers: vec![],
                advances: vec![],
            }
        };
        let parsed4 = Event::Play {
            inning: 3,
            team: Team::Visiting,
            player: "hamib001".into(),
            count: Some((1, 2)),
            pitches: vec![Pitch::Foul, Pitch::CalledStrike, Pitch::Ball, Pitch::BallInPlayBatter],
            event: PlayEvent {
                description: PlayDescription::HomeRun,
                modifiers: vec![
                    PlayModifier::HitLocation(HitLocation::_7),
                    PlayModifier::HitWithLocation(HitType::Fly, None)
                ],
                advances: vec![],
            }
        };

        assert_eq!(Done(&[][..], parsed1), play(play1));
        assert_eq!(Done(&[][..], parsed2), play(play2));
        assert_eq!(Done(&[][..], parsed3), play(play3));
        assert_eq!(Done(&[][..], parsed4), play(play4));
    }
}