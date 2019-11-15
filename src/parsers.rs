use std::str;

use nom::{
    branch::alt,
    bytes::{
        complete::{take, take_until},
        complete::tag,
    },
    character::complete::{alpha1, digit1, none_of, not_line_ending},
    character::complete::one_of,
    combinator::{complete, opt, recognize},
    combinator::{map, map_res},
    combinator::value,
    IResult,
    multi::many0,
    multi::many1,
    sequence::{pair, tuple},
    sequence::{preceded, terminated},
};
use nom::character::complete::alphanumeric1;
use nom::multi::{separated_list, separated_nonempty_list};

use event::{
    Advance, AdvanceParameter, Base, Event, Fielder, FieldParameter, Hand, HitLocation, HitType,
    Pitch, PlayDescription, Player, PlayEvent, PlayModifier, Team,
};

fn to_u8(input: &str) -> IResult<&str, u8> {
    map_res(digit1, str::parse::<u8>)(input)
}

fn quoted(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("\"")(input)?;
    let (input, result) = take_until("\"")(input)?;
    let (input, _) = tag("\"")(input)?;
    Ok((input, result))
}

fn team(input: &str) -> IResult<&str, Team> {
    alt((value(Team::Visiting, tag("0")), value(Team::Home, tag("1"))))(input)
}

fn base(input: &str) -> IResult<&str, Base> {
    alt((
        value(Base::First, tag("1")),
        value(Base::Second, tag("2")),
        value(Base::Third, tag("3")),
        value(Base::Home, tag("B")),
        value(Base::Home, tag("H")),
    ))(input)
}

fn version(input: &str) -> IResult<&str, Event> {
    let (input, _) = terminated(tag("version"), tag(","))(input)?;
    let (input, version) = to_u8(input)?;

    Ok((input, Event::Version { version }))
}

fn game_id(input: &str) -> IResult<&str, Event> {
    let (input, _) = terminated(tag("id"), tag(","))(input)?;
    let (input, id) = map(take(12usize), String::from)(input)?;

    Ok((input, Event::GameId { id }))
}

fn data(input: &str) -> IResult<&str, Event> {
    let (input, _) = terminated(tag("data"), tag(","))(input)?;
    let (input, data_type) = map(terminated(alpha1, tag(",")), Into::into)(input)?;
    let (input, player) = map(terminated(take_until(","), tag(",")), String::from)(input)?;
    let (input, value) = map(not_line_ending, String::from)(input)?;

    Ok((
        input,
        Event::Data {
            data_type,
            player,
            value,
        },
    ))
}

fn player_entry(input: &str) -> IResult<&str, Player> {
    let (input, id) = map(terminated(take_until(","), tag(",")), String::from)(input)?;
    let (input, name) = map(terminated(quoted, tag(",")), String::from)(input)?;
    let (input, team) = terminated(team, tag(","))(input)?;
    let (input, batting_pos) = terminated(to_u8, tag(","))(input)?;
    let (input, fielding_pos) = to_u8(input)?;

    Ok((
        input,
        Player {
            id,
            name,
            team,
            batting_pos,
            fielding_pos,
        },
    ))
}

fn start(input: &str) -> IResult<&str, Event> {
    let (input, _) = terminated(tag("start"), tag(","))(input)?;
    let (input, player) = player_entry(input)?;

    Ok((input, Event::Start { player }))
}

fn sub(input: &str) -> IResult<&str, Event> {
    let (input, _) = terminated(tag("sub"), tag(","))(input)?;
    let (input, player) = player_entry(input)?;

    Ok((input, Event::Sub { player }))
}

fn hit_location_mods(input: &str) -> IResult<&str, &str> {
    alt((
        tag("LDF"),
        tag("LSF"),
        tag("LF"),
        tag("LS"),
        tag("LD"),
        tag("XD"),
        tag("DF"),
        tag("MS"),
        tag("MD"),
        tag("S"),
        tag("F"),
        tag("M"),
        tag("D"),
        tag("L"),
    ))(input)
}

fn hit_location(input: &str) -> IResult<&str, HitLocation> {
    map(recognize(pair(digit1, opt(hit_location_mods))), Into::into)(input)
}

fn hit_with_location(input: &str) -> IResult<&str, PlayModifier> {
    let (input, hit_type) = alt((
        value(HitType::PopFly, tag("P")),
        value(HitType::PopUpBunt, tag("BP")),
        value(HitType::Fly, tag("F")),
        value(HitType::GroundBall, tag("G")),
        value(HitType::GroundBallBunt, tag("BG")),
        value(HitType::LineDrive, tag("L")),
    ))(input)?;
    let (input, hit_location) = opt(hit_location)(input)?;
    Ok((input, PlayModifier::HitWithLocation(hit_type, hit_location)))
}

fn modifier(input: &str) -> IResult<&str, PlayModifier> {
    let (input, modifier) = alt((
        alt((
            value(PlayModifier::AppealPlay, tag("AP")),
            value(PlayModifier::BuntFoul, tag("BF")),
            value(PlayModifier::BuntGroundedIntoDoublePlay, tag("BGDP")),
            value(PlayModifier::BatterInterference, tag("BINT")),
            value(PlayModifier::LineDriveBunt, tag("BL")),
            value(PlayModifier::BattingOutOfTurn, tag("BOOT")),
            value(PlayModifier::BuntPoppedIntoDoublePlay, tag("BPDP")),
            value(PlayModifier::RunnerHitByBattedBall, tag("BR")),
            value(PlayModifier::CourtesyBatter, tag("COUB")),
            value(PlayModifier::CourtesyFielder, tag("COUF")),
            value(PlayModifier::CourtesyRunner, tag("COUR")),
            value(PlayModifier::CalledThirdStrike, tag("C")),
            value(PlayModifier::UnspecifiedDoublePlay, tag("DP")),
            map(preceded(tag("E"), to_u8), PlayModifier::Error),
            value(PlayModifier::FlyBallDoublePlay, tag("FDP")),
            value(PlayModifier::FanInterference, tag("FINT")),
            value(PlayModifier::Foul, tag("FL")),
            value(PlayModifier::ForceOut, tag("FO")),
            value(PlayModifier::GroundBallDoublePlay, tag("GDP")),
            value(PlayModifier::GroundBallTriplePlay, tag("GTP")),
            value(PlayModifier::InfieldFlyRule, tag("IF")),
        )),
        alt((
            value(PlayModifier::Interference, tag("INT")),
            value(PlayModifier::InsideTheParkHR, tag("IPHR")),
            value(PlayModifier::LinedIntoDoublePlay, tag("LDP")),
            value(PlayModifier::LinedIntoTriplePlay, tag("LTP")),
            value(PlayModifier::ManagerChallenge, tag("MREV")),
            value(PlayModifier::NoDoublePlay, tag("NDP")),
            value(PlayModifier::Obstruction, tag("OBS")),
            value(PlayModifier::RunnerPassedAnotherRunner, tag("PASS")),
            map(preceded(tag("R"), to_u8), PlayModifier::Relay),
            value(PlayModifier::RunnerInterference, tag("RINT")),
            value(PlayModifier::SacrificeFly, tag("SF")),
            value(PlayModifier::SacrificeHit, tag("SH")),
            map(preceded(tag("TH"), to_u8), PlayModifier::ThrowToBase),
            value(PlayModifier::ThrowToBase(4), tag("THH")),
            value(PlayModifier::Throw, tag("TH")),
            value(PlayModifier::UnspecifiedTriplePlay, tag("TP")),
            value(PlayModifier::UmpireInterference, tag("UINT")),
            value(PlayModifier::UmpireReview, tag("UREV")),
            complete(hit_with_location),
            map(hit_location, PlayModifier::HitLocation),
        )),
    ))(input)?;
    // This is undocumented on the guide... no idea what this is.
    let (input, _) = opt(one_of("+-"))(input)?;

    Ok((input, modifier))
}

fn fielder(input: &str) -> IResult<&str, Fielder> {
    map(one_of("123456789"), |c: char| {
        c.to_digit(10).unwrap() as Fielder
    })(input)
}

fn field_parameters(input: &str) -> IResult<&str, Vec<FieldParameter>> {
    many1(alt((
        value(FieldParameter::Unknown, tag("U")),
        map(preceded(tag("E"), fielder), FieldParameter::Error),
        map(fielder, FieldParameter::Play),
    )))(input)
}

fn advance_parameter(input: &str) -> IResult<&str, AdvanceParameter> {
    let (input, _) = tag("(")(input)?;
    let (input, param) = alt((
        value(AdvanceParameter::UnearnedRun, tag("UR")),
        value(AdvanceParameter::NoRBI, tag("NR")),
        value(AdvanceParameter::NoRBI, tag("NORBI")),
        value(AdvanceParameter::RBI, tag("RBI")),
        value(AdvanceParameter::TeamUnearnedRun, tag("TUR")),
        map(
            terminated(fielder, tag("/INT")),
            AdvanceParameter::Interference,
        ),
        map(
            tuple((tag("E"), fielder, tag("/TH"), opt(base))),
            |(_, f, _, base)| AdvanceParameter::ThrowingError(f, base),
        ),
        value(
            AdvanceParameter::WithThrow,
            pair(tag("TH"), opt(one_of("23H"))),
        ),
        map(tuple((field_parameters, opt(tag("/TH")))), |(params, _)| {
            AdvanceParameter::FieldingPlay(params)
        }),
    ))(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, param))
}

fn advance(input: &str) -> IResult<&str, Advance> {
    let (input, from) = base(input)?;
    let (input, success) = alt((value(true, tag("-")), value(false, tag("X"))))(input)?;
    let (input, to) = base(input)?;
    let (input, parameters) = many0(advance_parameter)(input)?;

    Ok((
        input,
        Advance {
            from,
            to,
            success,
            parameters,
        },
    ))
}

///////////////////////
// Play Descriptions //
///////////////////////

fn play_desc_gidp(input: &str) -> IResult<&str, PlayDescription> {
    let (input, assists) = many1(fielder)(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, first_out) = base(input)?;
    let (input, _) = tag(")")(input)?;
    let (input, putout) = fielder(input)?;

    let mut fielders = assists;
    fielders.push(putout);

    Ok((input, PlayDescription::GIDP(fielders, first_out)))
}

fn play_desc_gitp(input: &str) -> IResult<&str, PlayDescription> {
    let (input, first_assists) = many1(fielder)(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, first_out) = base(input)?;
    let (input, _) = tag(")")(input)?;
    let (input, second_assists) = many1(fielder)(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, second_out) = base(input)?;
    let (input, _) = tag(")")(input)?;
    let (input, putout) = fielder(input)?;

    Ok((
        input,
        PlayDescription::GITP {
            first_assists,
            first_out,
            second_assists,
            second_out,
            putout,
        },
    ))
}

fn abnormal_putout_parser(input: &str) -> IResult<&str, Base> {
    let (input, _) = tag("(")(input)?;
    let (input, runner) = base(input)?;
    let (input, _) = tag(")")(input)?;
    Ok((input, runner))
}

fn error_fielder(input: &str) -> IResult<&str, (Fielder, bool)> {
    map(pair(opt(tag("E")), fielder), |(error, fielder)| {
        (fielder, error.is_some())
    })(input)
}

fn play_desc_fielding(input: &str) -> IResult<&str, PlayDescription> {
    let (input, fielders) = many1(error_fielder)(input)?;
    let (input, abnormal_putout) = opt(complete(abnormal_putout_parser))(input)?;

    Ok((
        input,
        PlayDescription::FielderSequence(fielders, abnormal_putout),
    ))
}

fn play_desc_strikeout(input: &str) -> IResult<&str, PlayDescription> {
    let (input, _) = tag("K")(input)?;
    let (input, fielders) = many0(error_fielder)(input)?;
    let (input, additional) = opt(complete(preceded(
        opt(tag("+")),
        map(play_description, Box::new),
    )))(input)?;

    Ok((input, PlayDescription::Strikeout(additional, fielders)))
}

fn play_desc_walk(input: &str) -> IResult<&str, PlayDescription> {
    let (input, _) = tag("W")(input)?;
    let (input, additional) = opt(complete(preceded(
        opt(tag("+")),
        map(play_description, Box::new),
    )))(input)?;

    Ok((input, PlayDescription::Walk(additional)))
}

fn play_desc_hr(input: &str) -> IResult<&str, PlayDescription> {
    let (input, _) = alt((tag("HR"), tag("H")))(input)?;
    let (input, fielders) = many0(complete(fielder))(input)?;

    Ok((
        input,
        if fielders.is_empty() {
            PlayDescription::HomeRun
        } else {
            PlayDescription::InsideTheParkHomeRun(fielders)
        },
    ))
}

fn play_desc_pickoff_cs(input: &str) -> IResult<&str, PlayDescription> {
    let (input, _) = tag("POCS")(input)?;
    let (input, base) = base(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, throws) = many1(complete(fielder))(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, PlayDescription::PickOffCaughtStealing(base, throws)))
}

fn play_desc_cs(input: &str) -> IResult<&str, PlayDescription> {
    let (input, _) = tag("CS")(input)?;
    let (input, base) = base(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, throws) = many1(complete(fielder))(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, PlayDescription::CaughtStealing(base, throws)))
}

fn play_desc_pickoff(input: &str) -> IResult<&str, PlayDescription> {
    let (input, _) = tag("PO")(input)?;
    let (input, base) = base(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, throws) = field_parameters(input)?;
    let (input, _) = opt(tag("/TH"))(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((input, PlayDescription::PickOff(base, throws)))
}

fn play_desc_ldp(input: &str) -> IResult<&str, PlayDescription> {
    let (input, first_out) = fielder(input)?;
    let (input, _) = tag("(B)")(input)?;
    let (input, second_out) = many1(fielder)(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, second_out_runner) = base(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((
        input,
        PlayDescription::LinedIntoDoublePlay {
            first_out,
            second_out,
            second_out_runner,
        },
    ))
}

fn play_desc_ltp(input: &str) -> IResult<&str, PlayDescription> {
    let (input, first_out) = fielder(input)?;
    let (input, _) = tag("(B)")(input)?;
    let (input, second_out) = many1(fielder)(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, second_out_runner) = base(input)?;
    let (input, _) = tag(")")(input)?;
    let (input, third_out) = many1(fielder)(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, third_out_runner) = base(input)?;
    let (input, _) = tag(")")(input)?;

    Ok((
        input,
        PlayDescription::LinedIntoTriplePlay {
            first_out,
            second_out,
            second_out_runner,
            third_out,
            third_out_runner,
        },
    ))
}

fn stolen_base(input: &str) -> IResult<&str, (Base, bool)> {
    let (input, _) = tag("SB")(input)?;
    let (input, base) = base(input)?;
    let (input, is_unearned) = map(opt(complete(tag("(UR)"))), |ue| ue.is_some())(input)?;
    Ok((input, (base, is_unearned)))
}

fn play_description(input: &str) -> IResult<&str, PlayDescription> {
    alt((
        alt((
            map(
                complete(separated_nonempty_list(tag(";"), stolen_base)),
                PlayDescription::StolenBase,
            ),
            value(PlayDescription::OtherAdvance, tag("OA")),
            value(PlayDescription::IntentionalWalk, tag("IW")),
            value(PlayDescription::IntentionalWalk, tag("I")),
            value(PlayDescription::HitByPitch, tag("HP")),
            value(PlayDescription::Balk, tag("BK")),
            value(PlayDescription::PassedBall, tag("PB")),
            value(PlayDescription::WildPitch, tag("WP")),
            value(PlayDescription::GroundRuleDouble, tag("DGR")),
            value(PlayDescription::DefensiveIndifference, tag("DI")),
            value(PlayDescription::NoPlay, tag("NP")),
            map(
                preceded(tag("FLE"), fielder),
                PlayDescription::FoulFlyBallError,
            ),
            map(preceded(tag("E"), fielder), PlayDescription::Error),
            map(
                preceded(tag("FC"), fielder),
                PlayDescription::FieldersChoice,
            ),
            map(
                preceded(tag("C/E"), fielder),
                PlayDescription::CatcherInterference,
            ),
            map(preceded(tag("S"), many0(fielder)), PlayDescription::Single),
            map(preceded(tag("D"), many0(fielder)), PlayDescription::Double),
            map(preceded(tag("T"), many0(fielder)), PlayDescription::Triple),
        )),
        alt((
            play_desc_cs,
            complete(play_desc_ltp),
            complete(play_desc_ldp),
            complete(play_desc_pickoff_cs),
            complete(play_desc_pickoff),
            play_desc_hr,
            play_desc_strikeout,
            play_desc_walk,
            complete(play_desc_gitp),
            complete(play_desc_gidp),
            play_desc_fielding,
        )),
    ))(input)
}

fn play_event(input: &str) -> IResult<&str, PlayEvent> {
    let (input, play_desc) = complete(play_description)(input)?;
    let (input, _) = opt(complete(alt((tag("!"), tag("?")))))(input)?;
    let (input, modifiers) = many0(preceded(tag("/"), complete(modifier)))(input)?;
    let (input, advances) = opt(complete(preceded(
        tag("."),
        separated_list(tag(";"), advance),
    )))(input)?;
    let (input, _) = opt(complete(tag("#")))(input)?;

    Ok((
        input,
        PlayEvent {
            description: play_desc,
            modifiers,
            advances: advances.unwrap_or_else(|| vec![]),
        },
    ))
}

fn pitch(input: &str) -> IResult<&str, Pitch> {
    map(none_of(","), Into::into)(input)
}

fn pitch_count(input: &str) -> IResult<&str, Option<(u8, u8)>> {
    let (input, balls) = one_of("?0123")(input)?;
    let (input, strikes) = one_of("?012")(input)?;

    Ok((input, {
        if balls == '?' || strikes == '?' {
            None
        } else {
            Some((
                balls.to_digit(10).unwrap() as u8,
                strikes.to_digit(10).unwrap() as u8,
            ))
        }
    }))
}

fn play(input: &str) -> IResult<&str, Event> {
    let (input, _) = terminated(tag("play"), tag(","))(input)?;
    let (input, inning) = map_res(digit1, str::parse::<u8>)(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, team) = terminated(team, tag(","))(input)?;
    let (input, player) = map(take_until(","), String::from)(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, count) = terminated(pitch_count, tag(","))(input)?;
    let (input, pitches) = many0(pitch)(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, event) = play_event(input)?;

    Ok((
        input,
        Event::Play {
            inning,
            team,
            player,
            count,
            pitches,
            event,
        },
    ))
}

fn comment(input: &str) -> IResult<&str, Event> {
    let (input, _) = terminated(tag("com"), tag(","))(input)?;
    let (input, _) = tag("\"")(input)?;
    let (input, comment) = map(take_until("\""), String::from)(input)?;
    let (input, _) = tag("\"")(input)?;

    Ok((input, Event::Comment { comment }))
}

fn hand(input: &str) -> IResult<&str, Hand> {
    alt((value(Hand::Left, tag("L")), value(Hand::Right, tag("R"))))(input)
}

fn badj(input: &str) -> IResult<&str, Event> {
    let (input, _) = terminated(tag("badj"), tag(","))(input)?;
    let (input, player) = map(take_until(","), String::from)(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, hand) = hand(input)?;
    Ok((input, Event::BattingAdjustment { player, hand }))
}

fn padj(input: &str) -> IResult<&str, Event> {
    let (input, _) = terminated(tag("padj"), tag(","))(input)?;
    let (input, player) = map(take_until(","), String::from)(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, hand) = hand(input)?;
    Ok((input, Event::PitchingAdjustment { player, hand }))
}

fn ladj(input: &str) -> IResult<&str, Event> {
    let (input, _) = terminated(tag("ladj"), tag(","))(input)?;
    let (input, team) = team(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, position) = to_u8(input)?;
    Ok((input, Event::LineupAdjustment { team, position }))
}

fn info(input: &str) -> IResult<&str, Event> {
    let (input, _) = terminated(tag("info"), tag(","))(input)?;
    let (input, key) = map(alphanumeric1, Into::into)(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, data) = map(not_line_ending, String::from)(input)?;
    Ok((input, Event::Info { key, data }))
}

pub fn event(input: &str) -> IResult<&str, Event> {
    let (input, event) = alt((
        game_id, version, play, info, start, sub, data, comment, badj, padj, ladj,
    ))(input)?;
    let (input, _) = if !input.is_empty() {
        alt((tag("\n"), tag("\r\n")))(input)?
    } else {
        (input, "")
    };

    Ok((input, event))
}

#[cfg(test)]
mod tests {
    use event::{
        Advance, AdvanceParameter, Base, DataEventType, Event, FieldParameter, Hand, HitLocation,
        HitType, Pitch, PlayDescription, PlayEvent, PlayModifier, Team,
    };

    use super::*;

    macro_rules! assert_parsed {
        ($expected: expr, $result: expr) => {
            assert_eq!(Ok(("", $expected)), $result);
        };
    }

    #[test]
    fn test_version() {
        assert_parsed!(Event::Version { version: 2 }, version("version,2"));
        assert!(version("asdlfk,3,5").is_err());
    }

    #[test]
    fn test_game_id() {
        assert_parsed!(
            Event::GameId {
                id: "CHN201604110".into()
            },
            game_id("id,CHN201604110")
        );
        assert!(game_id("asdlfk,3,5").is_err());
        assert!(game_id("id,3455").is_err());
    }

    #[test]
    fn test_data() {
        assert_parsed!(
            Event::Data {
                data_type: DataEventType::EarnedRuns,
                player: "showe001".into(),
                value: "2".into(),
            },
            data("data,er,showe001,2")
        );
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
        assert_parsed!(
            Event::Start {
                player: player1.clone()
            },
            start("start,fred103,\"fred\",1,7,6")
        );
        assert_parsed!(
            Event::Start {
                player: player2.clone()
            },
            start("start,bob202,\"bob\",0,3,9")
        );
        assert!(start("start,bob202,\"bob\",2,3,9").is_err());
        assert!(start("start,bob202").is_err());

        assert_parsed!(
            Event::Sub {
                player: player1.clone()
            },
            sub("sub,fred103,\"fred\",1,7,6")
        );
        assert_parsed!(
            Event::Sub {
                player: player2.clone()
            },
            sub("sub,bob202,\"bob\",0,3,9")
        );
        assert!(sub("sub,bob202,\"bob\",2,3,9").is_err());
        assert!(sub("sub,bob202").is_err());
    }

    #[test]
    fn test_hit_location() {
        let test_cases = vec![
            (HitLocation::_2F, "2F"),
            (HitLocation::_2, "2"),
            (HitLocation::_25F, "25F"),
            (HitLocation::_25, "25"),
            (HitLocation::_1S, "1S"),
            (HitLocation::_23, "23"),
            (HitLocation::_23F, "23F"),
            (HitLocation::_15, "15"),
            (HitLocation::_1, "1"),
            (HitLocation::_13, "13"),
            (HitLocation::_5S, "5S"),
            (HitLocation::_56S, "56S"),
            (HitLocation::_6S, "6S"),
            (HitLocation::_6MS, "6MS"),
            (HitLocation::_4MS, "4MS"),
            (HitLocation::_4S, "4S"),
            (HitLocation::_34S, "34S"),
            (HitLocation::_3S, "3S"),
            (HitLocation::_5F, "5F"),
            (HitLocation::_5, "5"),
            (HitLocation::_56, "56"),
            (HitLocation::_6, "6"),
            (HitLocation::_6M, "6M"),
            (HitLocation::_4M, "4M"),
            (HitLocation::_4, "4"),
            (HitLocation::_34, "34"),
            (HitLocation::_3, "3"),
            (HitLocation::_3F, "3F"),
            (HitLocation::_5DF, "5DF"),
            (HitLocation::_5D, "5D"),
            (HitLocation::_56D, "56D"),
            (HitLocation::_6D, "6D"),
            (HitLocation::_6MD, "6MD"),
            (HitLocation::_4MD, "4MD"),
            (HitLocation::_4D, "4D"),
            (HitLocation::_34D, "34D"),
            (HitLocation::_3D, "3D"),
            (HitLocation::_3DF, "3DF"),
            (HitLocation::_7LSF, "7LSF"),
            (HitLocation::_7LS, "7LS"),
            (HitLocation::_7S, "7S"),
            (HitLocation::_78S, "78S"),
            (HitLocation::_8S, "8S"),
            (HitLocation::_89S, "89S"),
            (HitLocation::_9S, "9S"),
            (HitLocation::_9LS, "9LS"),
            (HitLocation::_9LSF, "9LSF"),
            (HitLocation::_7LF, "7LF"),
            (HitLocation::_7L, "7L"),
            (HitLocation::_7, "7"),
            (HitLocation::_78, "78"),
            (HitLocation::_8, "8"),
            (HitLocation::_89, "89"),
            (HitLocation::_9, "9"),
            (HitLocation::_9L, "9L"),
            (HitLocation::_9LF, "9LF"),
            (HitLocation::_7LDF, "7LDF"),
            (HitLocation::_7LD, "7LD"),
            (HitLocation::_7D, "7D"),
            (HitLocation::_78D, "78D"),
            (HitLocation::_8D, "8D"),
            (HitLocation::_89D, "89D"),
            (HitLocation::_9D, "9D"),
            (HitLocation::_9LD, "9LD"),
            (HitLocation::_9LDF, "9LDF"),
            (HitLocation::_78XD, "78XD"),
            (HitLocation::_8XD, "8XD"),
            (HitLocation::_89XD, "89XD"),
            (HitLocation::_5L, "5L"),
        ];
        for (expected, input) in &test_cases {
            assert_parsed!(*expected, hit_location(input));
        }
    }

    #[test]
    fn test_modifier() {
        let test_cases = vec![
            (
                PlayModifier::HitWithLocation(HitType::PopUpBunt, None),
                "BP",
            ),
            (
                PlayModifier::HitWithLocation(HitType::GroundBallBunt, Some(HitLocation::_9LF)),
                "BG9LF",
            ),
            (
                PlayModifier::HitWithLocation(HitType::Fly, Some(HitLocation::_89S)),
                "F89S",
            ),
            (
                PlayModifier::HitWithLocation(HitType::GroundBall, Some(HitLocation::_13)),
                "G13",
            ),
            (PlayModifier::HitWithLocation(HitType::LineDrive, None), "L"),
            (
                PlayModifier::HitWithLocation(HitType::PopFly, Some(HitLocation::_4MS)),
                "P4MS",
            ),
            (PlayModifier::AppealPlay, "AP"),
            (PlayModifier::BuntFoul, "BF"),
            (PlayModifier::BuntGroundedIntoDoublePlay, "BGDP"),
            (PlayModifier::BatterInterference, "BINT"),
            (PlayModifier::LineDriveBunt, "BL"),
            (PlayModifier::BattingOutOfTurn, "BOOT"),
            (PlayModifier::BuntPoppedIntoDoublePlay, "BPDP"),
            (PlayModifier::RunnerHitByBattedBall, "BR"),
            (PlayModifier::CourtesyBatter, "COUB"),
            (PlayModifier::CourtesyFielder, "COUF"),
            (PlayModifier::CourtesyRunner, "COUR"),
            (PlayModifier::CalledThirdStrike, "C"),
            (PlayModifier::UnspecifiedDoublePlay, "DP"),
            (PlayModifier::Error(3), "E3"),
            (PlayModifier::Error(7), "E7"),
            (PlayModifier::FlyBallDoublePlay, "FDP"),
            (PlayModifier::FanInterference, "FINT"),
            (PlayModifier::Foul, "FL"),
            (PlayModifier::ForceOut, "FO"),
            (PlayModifier::GroundBallDoublePlay, "GDP"),
            (PlayModifier::GroundBallTriplePlay, "GTP"),
            (PlayModifier::InfieldFlyRule, "IF"),
            (PlayModifier::Interference, "INT"),
            (PlayModifier::InsideTheParkHR, "IPHR"),
            (PlayModifier::LinedIntoDoublePlay, "LDP"),
            (PlayModifier::LinedIntoTriplePlay, "LTP"),
            (PlayModifier::ManagerChallenge, "MREV"),
            (PlayModifier::NoDoublePlay, "NDP"),
            (PlayModifier::Obstruction, "OBS"),
            (PlayModifier::RunnerPassedAnotherRunner, "PASS"),
            (PlayModifier::Relay(6), "R6"),
            (PlayModifier::Relay(5), "R5"),
            (PlayModifier::RunnerInterference, "RINT"),
            (PlayModifier::SacrificeFly, "SF"),
            (PlayModifier::SacrificeHit, "SH"),
            (PlayModifier::Throw, "TH"),
            (PlayModifier::ThrowToBase(2), "TH2"),
            (PlayModifier::ThrowToBase(3), "TH3"),
            (PlayModifier::UnspecifiedTriplePlay, "TP"),
            (PlayModifier::UmpireInterference, "UINT"),
            (PlayModifier::UmpireReview, "UREV"),
        ];

        for (expected, input) in &test_cases {
            assert_parsed!(*expected, modifier(input));
        }
    }

    #[test]
    fn test_advance() {
        assert_parsed!(
            Advance {
                from: Base::Second,
                to: Base::Third,
                success: true,
                parameters: vec![]
            },
            advance("2-3")
        );
        assert_parsed!(
            Advance {
                from: Base::First,
                to: Base::Second,
                success: false,
                parameters: vec![AdvanceParameter::FieldingPlay(vec![
                    FieldParameter::Play(2),
                    FieldParameter::Play(6),
                ])],
            },
            advance("1X2(26)")
        );
        assert_parsed!(
            Advance {
                from: Base::Home,
                to: Base::Second,
                success: false,
                parameters: vec![AdvanceParameter::FieldingPlay(vec![
                    FieldParameter::Play(8),
                    FieldParameter::Play(4),
                    FieldParameter::Play(3),
                    FieldParameter::Play(4),
                ])],
            },
            advance("BX2(8434)")
        );
        assert_parsed!(
            Advance {
                from: Base::Home,
                to: Base::Second,
                success: false,
                parameters: vec![AdvanceParameter::FieldingPlay(vec![
                    FieldParameter::Play(8),
                    FieldParameter::Play(4),
                    FieldParameter::Play(3),
                    FieldParameter::Play(4),
                ])],
            },
            advance("BX2(8434/TH)")
        );
        assert_parsed!(
            Advance {
                from: Base::Home,
                to: Base::Second,
                success: false,
                parameters: vec![AdvanceParameter::FieldingPlay(vec![
                    FieldParameter::Play(7),
                    FieldParameter::Error(4),
                ])],
            },
            advance("BX2(7E4)")
        );
        assert_parsed!(
            Advance {
                from: Base::First,
                to: Base::Third,
                success: true,
                parameters: vec![AdvanceParameter::ThrowingError(5, None)],
            },
            advance("1-3(E5/TH)")
        );
        assert_parsed!(
            Advance {
                from: Base::Second,
                to: Base::Home,
                success: true,
                parameters: vec![
                    AdvanceParameter::ThrowingError(4, None),
                    AdvanceParameter::UnearnedRun,
                    AdvanceParameter::NoRBI
                ],
            },
            advance("2-H(E4/TH)(UR)(NR)")
        );
        assert_parsed!(
            Advance {
                from: Base::Second,
                to: Base::Home,
                success: true,
                parameters: vec![
                    AdvanceParameter::ThrowingError(4, None),
                    AdvanceParameter::UnearnedRun,
                    AdvanceParameter::NoRBI
                ],
            },
            advance("2-H(E4/TH)(UR)(NORBI)")
        );
        assert_parsed!(
            Advance {
                from: Base::Third,
                to: Base::Home,
                success: true,
                parameters: vec![AdvanceParameter::RBI],
            },
            advance("3-H(RBI)")
        );
        assert_parsed!(
            Advance {
                from: Base::Second,
                to: Base::Third,
                success: false,
                parameters: vec![AdvanceParameter::Interference(5)],
            },
            advance("2X3(5/INT)")
        );
        assert_parsed!(
            Advance {
                from: Base::Second,
                to: Base::Home,
                success: true,
                parameters: vec![AdvanceParameter::TeamUnearnedRun],
            },
            advance("2-H(TUR)")
        );
        assert_parsed!(
            Advance {
                from: Base::Home,
                to: Base::Second,
                success: false,
                parameters: vec![AdvanceParameter::FieldingPlay(vec![
                    FieldParameter::Play(8),
                    FieldParameter::Unknown,
                    FieldParameter::Play(3),
                ])],
            },
            advance("BX2(8U3)")
        );
        assert_parsed!(
            Advance {
                from: Base::Home,
                to: Base::Second,
                success: true,
                parameters: vec![AdvanceParameter::WithThrow],
            },
            advance("B-2(TH)")
        );
    }

    #[test]
    fn test_play_description() {
        let desc1 = PlayDescription::FielderSequence(vec![(2, false), (3, false)], None);
        let desc2 = PlayDescription::Strikeout(None, vec![(2, false), (3, false)]);
        let desc3 = PlayDescription::Strikeout(Some(Box::new(PlayDescription::PassedBall)), vec![]);
        let desc4 = PlayDescription::Strikeout(Some(Box::new(PlayDescription::WildPitch)), vec![]);
        let desc5 = PlayDescription::Walk(Some(Box::new(PlayDescription::WildPitch)));
        let desc6 = PlayDescription::LinedIntoDoublePlay {
            first_out: 8,
            second_out: vec![8, 4],
            second_out_runner: Base::Second,
        };
        let desc7 = PlayDescription::LinedIntoDoublePlay {
            first_out: 3,
            second_out: vec![3],
            second_out_runner: Base::First,
        };
        let desc8 = PlayDescription::LinedIntoTriplePlay {
            first_out: 1,
            second_out: vec![1, 6],
            second_out_runner: Base::Second,
            third_out: vec![6, 3],
            third_out_runner: Base::First,
        };
        let desc9 = PlayDescription::GITP {
            first_assists: vec![5],
            first_out: Base::Second,
            second_assists: vec![4],
            second_out: Base::First,
            putout: 3,
        };
        assert_parsed!(
            PlayDescription::GIDP(vec![6, 4, 3], Base::Second),
            play_description("64(2)3")
        );
        assert_parsed!(
            PlayDescription::FielderSequence(vec![(5, false)], None),
            play_description("5")
        );
        assert_parsed!(desc1, play_description("23"));
        assert_parsed!(PlayDescription::Balk, play_description("BK"));
        assert_parsed!(PlayDescription::PassedBall, play_description("PB"));
        assert_parsed!(PlayDescription::GroundRuleDouble, play_description("DGR"));
        assert_parsed!(PlayDescription::WildPitch, play_description("WP"));
        assert_parsed!(
            PlayDescription::Strikeout(None, vec![]),
            play_description("K")
        );
        assert_parsed!(PlayDescription::IntentionalWalk, play_description("I"));
        assert_parsed!(PlayDescription::IntentionalWalk, play_description("IW"));
        assert_parsed!(desc2, play_description("K23"));
        assert_parsed!(desc3, play_description("K+PB"));
        assert_parsed!(desc4, play_description("K+WP"));
        assert_parsed!(PlayDescription::Error(3), play_description("E3"));
        assert_parsed!(
            PlayDescription::FoulFlyBallError(3),
            play_description("FLE3")
        );
        assert_parsed!(PlayDescription::Single(vec![]), play_description("S"));
        assert_parsed!(PlayDescription::Double(vec![]), play_description("D"));
        assert_parsed!(PlayDescription::Triple(vec![]), play_description("T"));
        assert_parsed!(PlayDescription::Single(vec![3]), play_description("S3"));
        assert_parsed!(PlayDescription::Double(vec![7]), play_description("D7"));
        assert_parsed!(PlayDescription::Triple(vec![6]), play_description("T6"));
        assert_parsed!(PlayDescription::Single(vec![3, 4]), play_description("S34"));
        assert_parsed!(PlayDescription::Double(vec![9, 7]), play_description("D97"));
        assert_parsed!(PlayDescription::Triple(vec![5, 6]), play_description("T56"));
        assert_parsed!(PlayDescription::HomeRun, play_description("H"));
        assert_parsed!(PlayDescription::HomeRun, play_description("HR"));
        assert_parsed!(
            PlayDescription::InsideTheParkHomeRun(vec![3, 4]),
            play_description("H34")
        );
        assert_parsed!(
            PlayDescription::InsideTheParkHomeRun(vec![3, 4]),
            play_description("HR34")
        );
        assert_parsed!(PlayDescription::Walk(None), play_description("W"));
        assert_parsed!(desc5, play_description("W+WP"));
        assert_parsed!(PlayDescription::HitByPitch, play_description("HP"));
        assert_parsed!(PlayDescription::NoPlay, play_description("NP"));
        assert_parsed!(
            PlayDescription::StolenBase(vec![(Base::Third, false)]),
            play_description("SB3")
        );
        assert_parsed!(
            PlayDescription::StolenBase(vec![(Base::Third, false), (Base::Second, false)]),
            play_description("SB3;SB2")
        );
        assert_parsed!(
            PlayDescription::StolenBase(vec![(Base::Home, true), (Base::Second, false)]),
            play_description("SBH(UR);SB2")
        );
        assert_parsed!(
            PlayDescription::CatcherInterference(1),
            play_description("C/E1")
        );
        assert_parsed!(
            PlayDescription::CatcherInterference(2),
            play_description("C/E2")
        );
        assert_parsed!(
            PlayDescription::CatcherInterference(3),
            play_description("C/E3")
        );
        assert_parsed!(
            PlayDescription::DefensiveIndifference,
            play_description("DI")
        );
        assert_parsed!(PlayDescription::OtherAdvance, play_description("OA"));
        assert_parsed!(
            PlayDescription::PickOff(
                Base::Second,
                vec![FieldParameter::Play(1), FieldParameter::Play(4),]
            ),
            play_description("PO2(14)")
        );
        assert_parsed!(
            PlayDescription::PickOff(Base::First, vec![FieldParameter::Error(3),]),
            play_description("PO1(E3)")
        );
        assert_parsed!(
            PlayDescription::PickOffCaughtStealing(Base::Second, vec![1, 3, 6, 1]),
            play_description("POCS2(1361)")
        );
        assert_parsed!(
            PlayDescription::CaughtStealing(Base::Second, vec![1, 3, 6, 1]),
            play_description("CS2(1361)")
        );
        assert_parsed!(desc6, play_description("8(B)84(2)"));
        assert_parsed!(desc7, play_description("3(B)3(1)"));
        assert_parsed!(desc8, play_description("1(B)16(2)63(1)"));
        assert_parsed!(desc9, play_description("5(2)4(1)3"));
    }

    #[test]
    fn test_play_event() {
        let event1 = PlayEvent {
            description: PlayDescription::FielderSequence(vec![(2, false), (3, false)], None),
            modifiers: vec![PlayModifier::HitWithLocation(HitType::GroundBall, None)],
            advances: vec![Advance {
                from: Base::First,
                to: Base::Second,
                success: true,
                parameters: vec![],
            }],
        };
        let event2 = PlayEvent {
            description: PlayDescription::FieldersChoice(2),
            modifiers: vec![PlayModifier::HitWithLocation(HitType::GroundBall, None)],
            advances: vec![
                Advance {
                    from: Base::Second,
                    to: Base::Third,
                    success: false,
                    parameters: vec![AdvanceParameter::FieldingPlay(vec![
                        FieldParameter::Play(2),
                        FieldParameter::Play(6),
                        FieldParameter::Play(5),
                    ])],
                },
                Advance {
                    from: Base::Home,
                    to: Base::Second,
                    success: true,
                    parameters: vec![AdvanceParameter::WithThrow],
                },
            ],
        };
        let event3 = PlayEvent {
            description: PlayDescription::Single(vec![8]),
            modifiers: vec![],
            advances: vec![
                Advance {
                    from: Base::Second,
                    to: Base::Home,
                    success: true,
                    parameters: vec![],
                },
                Advance {
                    from: Base::Home,
                    to: Base::Second,
                    success: false,
                    parameters: vec![AdvanceParameter::FieldingPlay(vec![
                        FieldParameter::Play(8),
                        FieldParameter::Unknown,
                        FieldParameter::Play(3),
                    ])],
                },
            ],
        };
        let event4 = PlayEvent {
            description: PlayDescription::Single(vec![]),
            modifiers: vec![PlayModifier::HitWithLocation(
                HitType::LineDrive,
                Some(HitLocation::_9S),
            )],
            advances: vec![
                Advance {
                    from: Base::Third,
                    to: Base::Home,
                    success: true,
                    parameters: vec![],
                },
                Advance {
                    from: Base::Second,
                    to: Base::Third,
                    success: false,
                    parameters: vec![AdvanceParameter::Interference(5)],
                },
                Advance {
                    from: Base::First,
                    to: Base::Second,
                    success: true,
                    parameters: vec![],
                },
            ],
        };
        let event5 = PlayEvent {
            description: PlayDescription::FielderSequence(
                vec![(5, false), (4, false)],
                Some(Base::First),
            ),
            modifiers: vec![
                PlayModifier::ForceOut,
                PlayModifier::HitWithLocation(HitType::GroundBall, Some(HitLocation::_5)),
            ],
            advances: vec![
                Advance {
                    from: Base::Third,
                    to: Base::Home,
                    success: true,
                    parameters: vec![],
                },
                Advance {
                    from: Base::Home,
                    to: Base::First,
                    success: true,
                    parameters: vec![],
                },
            ],
        };
        let event6 = PlayEvent {
            description: PlayDescription::FielderSequence(vec![(7, false)], None),
            modifiers: vec![
                PlayModifier::HitWithLocation(HitType::Fly, None),
                PlayModifier::SacrificeFly,
            ],
            advances: vec![
                Advance {
                    from: Base::Third,
                    to: Base::Home,
                    success: true,
                    parameters: vec![],
                },
                Advance {
                    from: Base::Second,
                    to: Base::Third,
                    success: true,
                    parameters: vec![],
                },
                Advance {
                    from: Base::First,
                    to: Base::Second,
                    success: true,
                    parameters: vec![AdvanceParameter::WithThrow],
                },
            ],
        };
        let event7 = PlayEvent {
            description: PlayDescription::FielderSequence(vec![(4, false), (3, true)], None),
            modifiers: vec![PlayModifier::HitWithLocation(HitType::GroundBall, None)],
            advances: vec![
                Advance {
                    from: Base::Second,
                    to: Base::Third,
                    success: true,
                    parameters: vec![],
                },
                Advance {
                    from: Base::First,
                    to: Base::Second,
                    success: true,
                    parameters: vec![],
                },
            ],
        };

        let event8 = PlayEvent {
            description: PlayDescription::StolenBase(vec![
                (Base::Home, true),
                (Base::Second, false),
            ]),
            modifiers: vec![],
            advances: vec![],
        };

        let event9 = PlayEvent {
            description: PlayDescription::FielderSequence(vec![(5, false), (3, false)], None),
            modifiers: vec![
                PlayModifier::HitWithLocation(HitType::GroundBall, Some(HitLocation::_5L)),
                PlayModifier::UmpireReview,
            ],
            advances: vec![],
        };

        assert_parsed!(event1, play_event("23/G-.1-2"));
        assert_parsed!(event2, play_event("FC2/G.2X3(265);B-2(TH)"));
        assert_parsed!(event3, play_event("S8.2-H;BX2(8U3)"));
        assert_parsed!(event4, play_event("S/L9S.3-H;2X3(5/INT);1-2"));
        assert_parsed!(event5, play_event("54(1)/FO/G5.3-H;B-1"));
        assert_parsed!(event6, play_event("7/F/SF.3-H;2-3;1-2(THH)"));
        assert_parsed!(event7, play_event("4E3/G.2-3;1-2"));
        assert_parsed!(event8, play_event("SBH(UR);SB2"));
        assert_parsed!(event9, play_event("53/G5L/UREV"));
    }

    #[test]
    fn test_play() {
        let play1 = "play,8,0,philb001,12,FBS1FX,23/G-.1-2";
        let play2 = "play,7,0,finnb001,01,LX,FC2/G.2X3(265);B-2(TH)";
        let play3 = "play,6,1,heywj001,??,CBFBBS,K";
        let play4 = "play,3,0,hamib001,12,FCBX,HR/7/F";

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
                description: PlayDescription::FielderSequence(vec![(2, false), (3, false)], None),
                modifiers: vec![PlayModifier::HitWithLocation(HitType::GroundBall, None)],
                advances: vec![Advance {
                    from: Base::First,
                    to: Base::Second,
                    success: true,
                    parameters: vec![],
                }],
            },
        };
        let parsed2 = Event::Play {
            inning: 7,
            team: Team::Visiting,
            player: "finnb001".into(),
            count: Some((0, 1)),
            pitches: vec![Pitch::FoulBunt, Pitch::BallInPlayBatter],
            event: PlayEvent {
                description: PlayDescription::FieldersChoice(2),
                modifiers: vec![PlayModifier::HitWithLocation(HitType::GroundBall, None)],
                advances: vec![
                    Advance {
                        from: Base::Second,
                        to: Base::Third,
                        success: false,
                        parameters: vec![AdvanceParameter::FieldingPlay(vec![
                            FieldParameter::Play(2),
                            FieldParameter::Play(6),
                            FieldParameter::Play(5),
                        ])],
                    },
                    Advance {
                        from: Base::Home,
                        to: Base::Second,
                        success: true,
                        parameters: vec![AdvanceParameter::WithThrow],
                    },
                ],
            },
        };
        let parsed3 = Event::Play {
            inning: 6,
            team: Team::Home,
            player: "heywj001".into(),
            count: None,
            pitches: vec![
                Pitch::CalledStrike,
                Pitch::Ball,
                Pitch::Foul,
                Pitch::Ball,
                Pitch::Ball,
                Pitch::SwingingStrike,
            ],
            event: PlayEvent {
                description: PlayDescription::Strikeout(None, vec![]),
                modifiers: vec![],
                advances: vec![],
            },
        };
        let parsed4 = Event::Play {
            inning: 3,
            team: Team::Visiting,
            player: "hamib001".into(),
            count: Some((1, 2)),
            pitches: vec![
                Pitch::Foul,
                Pitch::CalledStrike,
                Pitch::Ball,
                Pitch::BallInPlayBatter,
            ],
            event: PlayEvent {
                description: PlayDescription::HomeRun,
                modifiers: vec![
                    PlayModifier::HitLocation(HitLocation::_7),
                    PlayModifier::HitWithLocation(HitType::Fly, None),
                ],
                advances: vec![],
            },
        };

        assert_parsed!(parsed1, play(play1));
        assert_parsed!(parsed2, play(play2));
        assert_parsed!(parsed3, play(play3));
        assert_parsed!(parsed4, play(play4));
    }

    #[test]
    fn test_comment() {
        assert_parsed!(
            Event::Comment {
                comment: "foo".into()
            },
            comment("com,\"foo\"")
        );
        assert!(comment("com,foo").is_err());
    }

    #[test]
    fn test_adjs() {
        assert_parsed!(
            Event::BattingAdjustment {
                player: "bonib001".into(),
                hand: Hand::Right,
            },
            badj("badj,bonib001,R")
        );
        assert_parsed!(
            Event::BattingAdjustment {
                player: "dempr101".into(),
                hand: Hand::Left,
            },
            badj("badj,dempr101,L")
        );

        assert_parsed!(
            Event::PitchingAdjustment {
                player: "harrg001".into(),
                hand: Hand::Left,
            },
            padj("padj,harrg001,L")
        );

        assert_parsed!(
            Event::LineupAdjustment {
                team: Team::Home,
                position: 7,
            },
            ladj("ladj,1,7")
        );
    }
}
