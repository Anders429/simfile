use crate::song;
use either::Either;
use serde::{
    de::{EnumAccess, Error, MapAccess, Unexpected, VariantAccess, Visitor},
    ser::SerializeStruct,
    Deserialize, Deserializer, Serialize, Serializer,
};
use std::{fmt, iter, mem::MaybeUninit, str};

/// All valid step combinations, according to the MSD specification.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(in crate::song) enum Step {
    None,
    Up,
    Right,
    Down,
    Left,
    UpRight,
    DownRight,
    DownLeft,
    UpLeft,
    UpDown,
    LeftRight,
}

impl Step {
    fn as_serialized_byte(&self) -> u8 {
        match self {
            Self::None => b'0',
            Self::DownLeft => b'1',
            Self::Down => b'2',
            Self::DownRight => b'3',
            Self::Left => b'4',
            Self::Right => b'6',
            Self::UpLeft => b'7',
            Self::Up => b'8',
            Self::UpRight => b'9',
            Self::UpDown => b'A',
            Self::LeftRight => b'B',
        }
    }

    fn from_serialized_byte<E>(byte: u8) -> Result<Self, E>
    where
        E: Error,
    {
        match byte {
            b'0' => Ok(Self::None),
            b'1' => Ok(Self::DownLeft),
            b'2' => Ok(Self::Down),
            b'3' => Ok(Self::DownRight),
            b'4' => Ok(Self::Left),
            b'6' => Ok(Self::Right),
            b'7' => Ok(Self::UpLeft),
            b'8' => Ok(Self::Up),
            b'9' => Ok(Self::UpRight),
            b'A' => Ok(Self::UpDown),
            b'B' => Ok(Self::LeftRight),
            _ => Err(Error::invalid_value(
                Unexpected::Char(byte.into()),
                &"'0', '1', '2', '3', '4', '6', '7', '8', '9', 'A', or 'B'",
            )),
        }
    }
}



impl From<Step> for [song::Panel; 4] {
    fn from(step: Step) -> Self {
        match step {
            Step::None => [song::Panel::None; 4],
            Step::Left => [song::Panel::Step, song::Panel::None, song::Panel::None, song::Panel::None],
            Step::Down => [song::Panel::None, song::Panel::Step, song::Panel::None, song::Panel::None],
            Step::Up => [song::Panel::None, song::Panel::None, song::Panel::Step, song::Panel::None],
            Step::Right => [song::Panel::None, song::Panel::None, song::Panel::None, song::Panel::Step],
            Step::DownLeft => [song::Panel::Step, song::Panel::Step, song::Panel::None, song::Panel::None],
            Step::UpLeft => [song::Panel::Step, song::Panel::None, song::Panel::Step, song::Panel::None],
            Step::LeftRight => [song::Panel::Step, song::Panel::None, song::Panel::None, song::Panel::Step],
            Step::UpDown => [song::Panel::None, song::Panel::Step, song::Panel::Step, song::Panel::None],
            Step::DownRight => [song::Panel::None, song::Panel::Step, song::Panel::None, song::Panel::Step],
            Step::UpRight => [song::Panel::None, song::Panel::None, song::Panel::Step, song::Panel::Step],
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(in crate::song) enum Notes {
    Eighth(Step),
}

impl Notes {
    fn serialization_capacity_requirement(&self) -> usize {
        match self {
            Notes::Eighth(_) => 1,
        }
    }

    fn serialize_to_bytes(&self, bytes: &mut Vec<u8>) {
        match self {
            Notes::Eighth(step) => {
                bytes.push(step.as_serialized_byte());
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub(in crate::song) struct Steps {
    pub(in crate::song) notes: Vec<Notes>,
}

impl Serialize for Steps {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut result = Vec::with_capacity(
            self.notes
                .iter()
                .map(|notes| notes.serialization_capacity_requirement())
                .sum(),
        );
        for notes in &self.notes {
            notes.serialize_to_bytes(&mut result);
        }
        serializer.serialize_bytes(&result)
    }
}

impl<'de> Deserialize<'de> for Steps {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct StepsVisitor;

        impl<'de> Visitor<'de> for StepsVisitor {
            type Value = Steps;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("steps")
            }

            fn visit_bytes<E>(self, bytes: &[u8]) -> Result<Self::Value, E>
            where
                E: Error,
            {
                // TODO: Make this capacity approximation more intelligent.
                let mut notes = Vec::with_capacity(bytes.len());
                for &byte in bytes.iter().filter(|b| !b.is_ascii_whitespace()) {
                    notes.push(Notes::Eighth(Step::from_serialized_byte(byte)?));
                }
                Ok(Steps { notes })
            }
        }

        deserializer.deserialize_bytes(StepsVisitor)
    }
}

impl From<Steps> for song::Steps<4> {
    fn from(msd_steps: Steps) -> Self {
        let mut steps = Vec::new();

        for notes in msd_steps.notes {
            match notes {
                Notes::Eighth(step) => {
                    steps.push(song::Step {
                        panels: step.into(),
                        duration: song::Duration::Eighth,
                    });
                }
            }
        }

        song::Steps { steps }
    }
}

impl From<(Steps, Steps)> for song::Steps<8> {
    fn from(msd_steps: (Steps, Steps)) -> Self {
        let mut steps = Vec::new();

        // Combines the notes from both step charts into a single iterator, accounting for differing length as well.
        let notes_0_len = msd_steps.0.notes.len();
        let notes_1_len = msd_steps.1.notes.len();
        let notes_iter = if notes_0_len > notes_1_len {
            Either::Left(Either::Left(
                msd_steps.0.notes.into_iter().zip(
                    msd_steps.1.notes.into_iter().chain(
                        iter::repeat(Notes::Eighth(Step::None))
                            .take(notes_0_len - notes_1_len),
                    ),
                ),
            ))
        } else if notes_0_len < notes_1_len {
            Either::Left(Either::Right(
                msd_steps
                    .0
                    .notes
                    .into_iter()
                    .chain(
                        iter::repeat(Notes::Eighth(Step::None))
                            .take(notes_0_len - notes_1_len),
                    )
                    .zip(msd_steps.1.notes.into_iter()),
            ))
        } else {
            Either::Right(
                msd_steps
                    .0
                    .notes
                    .into_iter()
                    .zip(msd_steps.1.notes.into_iter()),
            )
        };

        for (left_notes, right_notes) in notes_iter {
            match (left_notes, right_notes) {
                (Notes::Eighth(left_step), Notes::Eighth(right_step)) => {
                    steps.push(song::Step {
                        panels: {
                            let mut whole = MaybeUninit::uninit();
                            let ptr = whole.as_mut_ptr() as *mut [song::Panel; 4];
                            unsafe {
                                ptr.write(left_step.into());
                                ptr.add(1).write(right_step.into());
                                whole.assume_init()
                            }
                        },
                        duration: song::Duration::Eighth,
                    })
                }
            }
        }

        song::Steps { steps }
    }
}

/// All valid MSD difficulties.
#[derive(Debug, Eq, PartialEq)]
pub(in crate::song) enum Difficulty {
    /// Basic mode.
    Basic,
    /// Another mode.
    Another,
    /// Maniac mode.
    Maniac,
}

impl Serialize for Difficulty {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Difficulty::Basic => serializer.serialize_unit_variant("Difficulty", 0, "BASIC"),
            Difficulty::Another => serializer.serialize_unit_variant("Difficulty", 1, "ANOTHER"),
            Difficulty::Maniac => serializer.serialize_unit_variant("Difficulty", 2, "MANIAC"),
        }
    }
}

impl<'de> Deserialize<'de> for Difficulty {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        enum Variant {
            Basic,
            Another,
            Maniac,
        }

        impl<'de> Deserialize<'de> for Variant {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct VariantVisitor;

                impl<'de> Visitor<'de> for VariantVisitor {
                    type Value = Variant;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("`BASIC`, `ANOTHER`, or `MANIAC`")
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
                    where
                        E: Error,
                    {
                        match value {
                            "BASIC" => Ok(Variant::Basic),
                            "ANOTHER" => Ok(Variant::Another),
                            "MANIAC" => Ok(Variant::Maniac),
                            _ => Err(Error::unknown_variant(value, VARIANTS)),
                        }
                    }
                }

                deserializer.deserialize_identifier(VariantVisitor)
            }
        }

        struct DifficultyVisitor;

        impl<'de> Visitor<'de> for DifficultyVisitor {
            type Value = Difficulty;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("enum Difficulty")
            }

            fn visit_enum<A>(self, enum_access: A) -> Result<Self::Value, A::Error>
            where
                A: EnumAccess<'de>,
            {
                match enum_access.variant()? {
                    (Variant::Basic, variant_access) => {
                        variant_access.unit_variant().map(|()| Difficulty::Basic)
                    }
                    (Variant::Another, variant_access) => {
                        variant_access.unit_variant().map(|()| Difficulty::Another)
                    }
                    (Variant::Maniac, variant_access) => {
                        variant_access.unit_variant().map(|()| Difficulty::Maniac)
                    }
                }
            }
        }

        const VARIANTS: &'static [&'static str] = &["BASIC", "ANOTHER", "MANIAC"];

        deserializer.deserialize_enum("Difficulty", VARIANTS, DifficultyVisitor)
    }
}

#[derive(Debug, PartialEq)]
pub(in crate::song) struct Song {
    pub(in crate::song) file: Option<String>,
    pub(in crate::song) title: Option<String>,
    pub(in crate::song) artist: Option<String>,
    pub(in crate::song) msd: Option<String>,
    pub(in crate::song) bpm: Option<f64>,
    pub(in crate::song) gap: Option<i64>,
    pub(in crate::song) back: Option<String>,
    pub(in crate::song) bgm: Option<String>,
    pub(in crate::song) select: Option<String>,
    pub(in crate::song) single: Vec<(Difficulty, u8, Steps)>,
    pub(in crate::song) double: Vec<(Difficulty, u8, Steps, Steps)>,
    pub(in crate::song) couple: Vec<(Difficulty, u8, Steps, Steps)>,
}

impl Serialize for Song {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut serialize_struct = serializer.serialize_struct("Song", 11)?;
        serialize_struct.serialize_field("FILE", &self.file)?;
        serialize_struct.serialize_field("TITLE", &self.title)?;
        serialize_struct.serialize_field("ARTIST", &self.artist)?;
        serialize_struct.serialize_field("MSD", &self.msd)?;
        serialize_struct.serialize_field("BPM", &self.bpm)?;
        serialize_struct.serialize_field("GAP", &self.gap)?;
        serialize_struct.serialize_field("BACK", &self.back)?;
        serialize_struct.serialize_field("BGM", &self.bgm)?;
        serialize_struct.serialize_field("SELECT", &self.select)?;
        serialize_struct.serialize_field("SINGLE", &self.single)?;
        serialize_struct.serialize_field("DOUBLE", &self.double)?;
        serialize_struct.serialize_field("COUPLE", &self.couple)?;
        serialize_struct.end()
    }
}

impl<'de> Deserialize<'de> for Song {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        enum Field {
            File,
            Title,
            Artist,
            Msd,
            Bpm,
            Gap,
            Back,
            Bgm,
            Select,
            Single,
            Double,
            Couple,
        }

        impl<'de> Deserialize<'de> for Field {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct FieldVisitor;

                impl<'de> Visitor<'de> for FieldVisitor {
                    type Value = Field;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("`FILE`, `TITLE`, `ARTIST`, `BPM`, `GAP`, `BACK`, `BGM`, `SELECT`, `SINGLE`, `DOUBLE`, or `COUPLE`")
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
                    where
                        E: Error,
                    {
                        match value {
                            "FILE" => Ok(Field::File),
                            "TITLE" => Ok(Field::Title),
                            "ARTIST" => Ok(Field::Artist),
                            "MSD" => Ok(Field::Msd),
                            "BPM" => Ok(Field::Bpm),
                            "GAP" => Ok(Field::Gap),
                            "BACK" => Ok(Field::Back),
                            "BGM" => Ok(Field::Bgm),
                            "SELECT" => Ok(Field::Select),
                            "SINGLE" => Ok(Field::Single),
                            "DOUBLE" => Ok(Field::Double),
                            "COUPLE" => Ok(Field::Couple),
                            _ => Err(Error::unknown_field(value, FIELDS)),
                        }
                    }
                }

                deserializer.deserialize_identifier(FieldVisitor)
            }
        }

        struct SongVisitor;

        impl<'de> Visitor<'de> for SongVisitor {
            type Value = Song;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Song")
            }

            fn visit_map<A>(self, mut map_access: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut file = None;
                let mut title = None;
                let mut artist = None;
                let mut msd = None;
                let mut bpm = None;
                let mut gap = None;
                let mut back = None;
                let mut bgm = None;
                let mut select = None;
                let mut single = Vec::new();
                let mut double = Vec::new();
                let mut couple = Vec::new();

                while let Some(key) = map_access.next_key()? {
                    match key {
                        Field::File => {
                            if file.is_some() {
                                return Err(Error::duplicate_field("FILE"));
                            }
                            file = map_access.next_value()?;
                        }
                        Field::Title => {
                            if title.is_some() {
                                return Err(Error::duplicate_field("TITLE"));
                            }
                            title = map_access.next_value()?;
                        }
                        Field::Artist => {
                            if artist.is_some() {
                                return Err(Error::duplicate_field("ARTIST"));
                            }
                            artist = map_access.next_value()?;
                        }
                        Field::Msd => {
                            if msd.is_some() {
                                return Err(Error::duplicate_field("MSD"));
                            }
                            msd = map_access.next_value()?;
                        }
                        Field::Bpm => {
                            if bpm.is_some() {
                                return Err(Error::duplicate_field("BPM"));
                            }
                            bpm = map_access.next_value()?;
                        }
                        Field::Gap => {
                            if gap.is_some() {
                                return Err(Error::duplicate_field("GAP"));
                            }
                            gap = map_access.next_value()?;
                        }
                        Field::Back => {
                            if back.is_some() {
                                return Err(Error::duplicate_field("BACK"));
                            }
                            back = map_access.next_value()?;
                        }
                        Field::Bgm => {
                            if bgm.is_some() {
                                return Err(Error::duplicate_field("BGM"));
                            }
                            bgm = map_access.next_value()?;
                        }
                        Field::Select => {
                            if select.is_some() {
                                return Err(Error::duplicate_field("SELECT"));
                            }
                            select = map_access.next_value()?;
                        }
                        Field::Single => {
                            single.extend(map_access.next_value::<Vec<(Difficulty, u8, Steps)>>()?);
                        }
                        Field::Double => {
                            double.extend(
                                map_access.next_value::<Vec<(Difficulty, u8, Steps, Steps)>>()?,
                            );
                        }
                        Field::Couple => {
                            couple.extend(
                                map_access.next_value::<Vec<(Difficulty, u8, Steps, Steps)>>()?,
                            );
                        }
                    }
                }

                Ok(Song {
                    file,
                    title,
                    artist,
                    msd,
                    bpm,
                    gap,
                    back,
                    bgm,
                    select,
                    single,
                    double,
                    couple,
                })
            }
        }

        const FIELDS: &'static [&'static str] = &[
            "FILE", "TITLE", "ARTIST", "MSD", "BPM", "GAP", "BACK", "BGM", "SELECT", "SINGLE",
            "DOUBLE", "COUPLE",
        ];
        deserializer.deserialize_struct("Song", FIELDS, SongVisitor)
    }
}

impl From<Song> for song::Song {
    fn from(song: Song) -> Self {
        let mut charts = Vec::new();

        for single in song.single {
            charts.push(song::Chart {
                difficulty: single.0.into(),
                meter: single.1,
                style: song::Style::Single(single.2.into()),
            })
        }

        for double in song.double {
            charts.push(song::Chart {
                difficulty: double.0.into(),
                meter: double.1,
                style: song::Style::Double((double.2, double.3).into()),
            })
        }

        for couple in song.couple {
            charts.push(song::Chart {
                difficulty: couple.0.into(),
                meter: couple.1,
                style: song::Style::Couple((couple.2, couple.3).into()),
            })
        }

        song::Song {
            title: song.title,
            subtitle: None,
            artist: song.artist,
            credit: song.msd,

            bpm: song.bpm,
            offset: song.gap,

            background_file: song.back,
            music_preview_file: song.select,
            music_file: song.bgm,

            charts: charts,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_test::{assert_tokens, Token};

    #[test]
    fn steps_ser_de_empty() {
        assert_tokens(&Steps { notes: Vec::new() }, &[Token::Bytes(b"")]);
    }

    #[test]
    fn steps_ser_de_eighths() {
        assert_tokens(
            &Steps {
                notes: vec![
                    Notes::Eighth(Step::Up),
                    Notes::Eighth(Step::LeftRight),
                    Notes::Eighth(Step::None),
                    Notes::Eighth(Step::DownRight),
                ],
            },
            &[Token::Bytes(b"8B03")],
        );
    }

    #[test]
    fn difficulty_ser_de_basic() {
        assert_tokens(
            &Difficulty::Basic,
            &[Token::UnitVariant {
                name: "Difficulty",
                variant: "BASIC",
            }],
        );
    }

    #[test]
    fn difficulty_ser_de_another() {
        assert_tokens(
            &Difficulty::Another,
            &[Token::UnitVariant {
                name: "Difficulty",
                variant: "ANOTHER",
            }],
        );
    }

    #[test]
    fn difficulty_ser_de_maniac() {
        assert_tokens(
            &Difficulty::Maniac,
            &[Token::UnitVariant {
                name: "Difficulty",
                variant: "MANIAC",
            }],
        );
    }

    #[test]
    fn msd_ser_de_full() {
        assert_tokens(
            &Song {
                file: Some("file".to_string()),
                title: Some("title".to_string()),
                artist: Some("artist".to_string()),
                msd: Some("msd".to_string()),
                bpm: Some(42.9),
                gap: Some(-100),
                back: Some("back".to_string()),
                bgm: Some("bgm".to_string()),
                select: Some("select".to_string()),
                single: vec![
                    (
                        Difficulty::Basic,
                        4,
                        Steps {
                            notes: vec![
                                Notes::Eighth(Step::Up),
                                Notes::Eighth(Step::None),
                                Notes::Eighth(Step::Down),
                                Notes::Eighth(Step::Right),
                            ],
                        },
                    ),
                    (
                        Difficulty::Another,
                        7,
                        Steps {
                            notes: vec![
                                Notes::Eighth(Step::UpDown),
                                Notes::Eighth(Step::UpLeft),
                                Notes::Eighth(Step::None),
                                Notes::Eighth(Step::Up),
                            ],
                        },
                    ),
                ],
                double: vec![(
                    Difficulty::Maniac,
                    9,
                    Steps {
                        notes: vec![
                            Notes::Eighth(Step::Down),
                            Notes::Eighth(Step::Down),
                            Notes::Eighth(Step::Down),
                            Notes::Eighth(Step::Down),
                        ],
                    },
                    Steps {
                        notes: vec![
                            Notes::Eighth(Step::Up),
                            Notes::Eighth(Step::Down),
                            Notes::Eighth(Step::LeftRight),
                            Notes::Eighth(Step::DownLeft),
                        ],
                    },
                )],
                couple: vec![
                    (
                        Difficulty::Basic,
                        2,
                        Steps {
                            notes: vec![
                                Notes::Eighth(Step::UpRight),
                                Notes::Eighth(Step::Down),
                                Notes::Eighth(Step::DownRight),
                                Notes::Eighth(Step::None),
                            ],
                        },
                        Steps {
                            notes: vec![
                                Notes::Eighth(Step::Up),
                                Notes::Eighth(Step::Up),
                                Notes::Eighth(Step::UpDown),
                                Notes::Eighth(Step::Up),
                            ],
                        },
                    ),
                    (
                        Difficulty::Another,
                        6,
                        Steps {
                            notes: vec![
                                Notes::Eighth(Step::None),
                                Notes::Eighth(Step::None),
                                Notes::Eighth(Step::None),
                                Notes::Eighth(Step::None),
                            ],
                        },
                        Steps {
                            notes: vec![
                                Notes::Eighth(Step::Left),
                                Notes::Eighth(Step::Up),
                                Notes::Eighth(Step::Right),
                                Notes::Eighth(Step::Down),
                            ],
                        },
                    ),
                ],
            },
            &[
                Token::Struct {
                    name: "Song",
                    len: 11,
                },
                Token::Str("FILE"),
                Token::Some,
                Token::Str("file"),
                Token::Str("TITLE"),
                Token::Some,
                Token::Str("title"),
                Token::Str("ARTIST"),
                Token::Some,
                Token::Str("artist"),
                Token::Str("MSD"),
                Token::Some,
                Token::Str("msd"),
                Token::Str("BPM"),
                Token::Some,
                Token::F64(42.9),
                Token::Str("GAP"),
                Token::Some,
                Token::I64(-100),
                Token::Str("BACK"),
                Token::Some,
                Token::Str("back"),
                Token::Str("BGM"),
                Token::Some,
                Token::Str("bgm"),
                Token::Str("SELECT"),
                Token::Some,
                Token::Str("select"),
                Token::Str("SINGLE"),
                Token::Seq { len: Some(2) },
                Token::Tuple { len: 3 },
                Token::UnitVariant {
                    name: "Difficulty",
                    variant: "BASIC",
                },
                Token::U8(4),
                Token::Bytes(b"8026"),
                Token::TupleEnd,
                Token::Tuple { len: 3 },
                Token::UnitVariant {
                    name: "Difficulty",
                    variant: "ANOTHER",
                },
                Token::U8(7),
                Token::Bytes(b"A708"),
                Token::TupleEnd,
                Token::SeqEnd,
                Token::Str("DOUBLE"),
                Token::Seq { len: Some(1) },
                Token::Tuple { len: 4 },
                Token::UnitVariant {
                    name: "Difficulty",
                    variant: "MANIAC",
                },
                Token::U8(9),
                Token::Bytes(b"2222"),
                Token::Bytes(b"82B1"),
                Token::TupleEnd,
                Token::SeqEnd,
                Token::Str("COUPLE"),
                Token::Seq { len: Some(2) },
                Token::Tuple { len: 4 },
                Token::UnitVariant {
                    name: "Difficulty",
                    variant: "BASIC",
                },
                Token::U8(2),
                Token::Bytes(b"9230"),
                Token::Bytes(b"88A8"),
                Token::TupleEnd,
                Token::Tuple { len: 4 },
                Token::UnitVariant {
                    name: "Difficulty",
                    variant: "ANOTHER",
                },
                Token::U8(6),
                Token::Bytes(b"0000"),
                Token::Bytes(b"4862"),
                Token::TupleEnd,
                Token::SeqEnd,
                Token::StructEnd,
            ],
        )
    }
}
