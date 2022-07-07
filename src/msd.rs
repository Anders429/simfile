use serde::{
    ser::{SerializeStruct},
    de::{EnumAccess, Error, MapAccess, Unexpected, VariantAccess, Visitor},
    Deserialize, Deserializer, Serialize, Serializer,
};
use std::{fmt, str};

/// All valid step combinations, according to the MSD specification.
#[derive(Debug, Eq, PartialEq)]
pub enum Step {
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

#[derive(Debug, Eq, PartialEq)]
pub enum Notes {
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
pub struct Steps {
    notes: Vec<Notes>,
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
                for &byte in bytes {
                    notes.push(Notes::Eighth(Step::from_serialized_byte(byte)?));
                }
                Ok(Steps { notes })
            }
        }

        deserializer.deserialize_bytes(StepsVisitor)
    }
}

/// All valid MSD difficulties.
#[derive(Debug, Eq, PartialEq)]
pub enum Difficulty {
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
pub struct Msd {
    file: String,
    title: String,
    artist: String,
    bpm: f64,
    gap: i64,
    back: String,
    bgm: String,
    select: String,
    single: Vec<(Difficulty, u8, Steps)>,
    double: Vec<(Difficulty, u8, Steps, Steps)>,
    couple: Vec<(Difficulty, u8, Steps, Steps)>,
}

impl Serialize for Msd {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
        let mut serialize_struct = serializer.serialize_struct("Msd", 11)?;
        serialize_struct.serialize_field("FILE", &self.file)?;
        serialize_struct.serialize_field("TITLE", &self.title)?;
        serialize_struct.serialize_field("ARTIST", &self.artist)?;
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

impl<'de> Deserialize<'de> for Msd {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'de> {
        enum Field {
            File,
            Title,
            Artist,
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
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> where D: Deserializer<'de> {
                struct FieldVisitor;

                impl<'de> Visitor<'de> for FieldVisitor {
                    type Value = Field;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("`FILE`, `TITLE`, `ARTIST`, `BPM`, `GAP`, `BACK`, `BGM`, `SELECT`, `SINGLE`, `DOUBLE`, or `COUPLE`")
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E> where E: Error {
                        match value {
                            "FILE" => Ok(Field::File),
                            "TITLE" => Ok(Field::Title),
                            "ARTIST" => Ok(Field::Artist),
                            "BPM" => Ok(Field::Bpm),
                            "GAP" => Ok(Field::Gap),
                            "BACK" => Ok(Field::Back),
                            "BGM" => Ok(Field::Bgm),
                            "SELECT" => Ok(Field::Select),
                            "SINGLE" => Ok(Field::Single),
                            "DOUBLE" => Ok(Field::Double),
                            "COUPLE" => Ok(Field::Couple),
                            _ => Err(Error::unknown_field(value, FIELDS))
                        }
                    }
                }

                deserializer.deserialize_identifier(FieldVisitor)
            }
        }

        struct MsdVisitor;

        impl<'de> Visitor<'de> for MsdVisitor {
            type Value = Msd;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Msd")
            }

            fn visit_map<A>(self, mut map_access: A) -> Result<Self::Value, A::Error> where A: MapAccess<'de> {
                let mut file: Option<String> = None;
                let mut title: Option<String> = None;
                let mut artist: Option<String> = None;
                let mut bpm: Option<f64> = None;
                let mut gap: Option<i64> = None;
                let mut back: Option<String> = None;
                let mut bgm: Option<String> = None;
                let mut select: Option<String> = None;
                let mut single: Option<Vec<(Difficulty, u8, Steps)>> = None;
                let mut double: Option<Vec<(Difficulty, u8, Steps, Steps)>> = None;
                let mut couple: Option<Vec<(Difficulty, u8, Steps, Steps)>> = None;

                while let Some(key) = map_access.next_key()? {
                    match key {
                        Field::File => {
                            if file.is_some() {
                                return Err(Error::duplicate_field("FILE"));
                            }
                            file = Some(map_access.next_value()?);
                        }
                        Field::Title => {
                            if title.is_some() {
                                return Err(Error::duplicate_field("TITLE"));
                            }
                            title = Some(map_access.next_value()?);
                        }
                        Field::Artist => {
                            if artist.is_some() {
                                return Err(Error::duplicate_field("ARTIST"));
                            }
                            artist = Some(map_access.next_value()?);
                        }
                        Field::Bpm => {
                            if bpm.is_some() {
                                return Err(Error::duplicate_field("BPM"));
                            }
                            bpm = Some(map_access.next_value()?);
                        }
                        Field::Gap => {
                            if gap.is_some() {
                                return Err(Error::duplicate_field("GAP"));
                            }
                            gap = Some(map_access.next_value()?);
                        }
                        Field::Back => {
                            if back.is_some() {
                                return Err(Error::duplicate_field("BACK"));
                            }
                            back = Some(map_access.next_value()?);
                        }
                        Field::Bgm => {
                            if bgm.is_some() {
                                return Err(Error::duplicate_field("BGM"));
                            }
                            bgm = Some(map_access.next_value()?);
                        }
                        Field::Select => {
                            if select.is_some() {
                                return Err(Error::duplicate_field("SELECT"));
                            }
                            select = Some(map_access.next_value()?);
                        }
                        Field::Single => {
                            if single.is_some() {
                                return Err(Error::duplicate_field("SINGLE"));
                            }
                            single = Some(map_access.next_value()?);
                        }
                        Field::Double => {
                            if double.is_some() {
                                return Err(Error::duplicate_field("DOUBLE"));
                            }
                            double = Some(map_access.next_value()?);
                        }
                        Field::Couple => {
                            if couple.is_some() {
                                return Err(Error::duplicate_field("COUPLE"));
                            }
                            couple = Some(map_access.next_value()?);
                        }
                    }
                }

                let file = file.unwrap_or_default();
                let title = title.unwrap_or_default();
                let artist = artist.unwrap_or_default();
                let bpm = bpm.unwrap_or_default();
                let gap = gap.unwrap_or_default();
                let back = back.unwrap_or_default();
                let bgm = bgm.unwrap_or_default();
                let select = select.unwrap_or_default();
                let single = single.unwrap_or_default();
                let double = double.unwrap_or_default();
                let couple = couple.unwrap_or_default();

                Ok(Msd {
                    file,
                    title,
                    artist,
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

        const FIELDS: &'static [&'static str] = &["FILE", "TITLE", "ARTIST", "BPM", "GAP", "BACK", "BGM", "SELECT", "SINGLE", "DOUBLE", "COUPLE"];
        deserializer.deserialize_struct("Msd", FIELDS, MsdVisitor)
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
            &Msd {
                file: "file".to_string(),
                title: "title".to_string(),
                artist: "artist".to_string(),
                bpm: 42.9,
                gap: -100,
                back: "back".to_string(),
                bgm: "bgm".to_string(),
                select: "select".to_string(),
                single: vec![
                    (Difficulty::Basic, 4, Steps {
                        notes: vec![
                            Notes::Eighth(Step::Up),
                            Notes::Eighth(Step::None),
                            Notes::Eighth(Step::Down),
                            Notes::Eighth(Step::Right),
                        ],
                    }),
                    (Difficulty::Another, 7, Steps {
                        notes: vec![
                            Notes::Eighth(Step::UpDown),
                            Notes::Eighth(Step::UpLeft),
                            Notes::Eighth(Step::None),
                            Notes::Eighth(Step::Up),
                        ],
                    }),
                ],
                double: vec![
                    (Difficulty::Maniac, 9, Steps {
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
                    }),
                ],
                couple: vec![
                    (Difficulty::Basic, 2, Steps {
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
                    }),
                    (Difficulty::Another, 6, Steps {
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
                    }),
                ]
            },
            &[
                Token::Struct {
                    name: "Msd", len: 11
                },
                Token::Str("FILE"),
                Token::Str("file"),
                Token::Str("TITLE"),
                Token::Str("title"),
                Token::Str("ARTIST"),
                Token::Str("artist"),
                Token::Str("BPM"),
                Token::F64(42.9),
                Token::Str("GAP"),
                Token::I64(-100),
                Token::Str("BACK"),
                Token::Str("back"),
                Token::Str("BGM"),
                Token::Str("bgm"),
                Token::Str("SELECT"),
                Token::Str("select"),
                Token::Str("SINGLE"),
                Token::Seq {len: Some(2)},
                Token::Tuple {len: 3},
                Token::UnitVariant {
                    name: "Difficulty",
                    variant: "BASIC",
                },
                Token::U8(4),
                Token::Bytes(b"8026"),
                Token::TupleEnd,
                Token::Tuple {len: 3},
                Token::UnitVariant {
                    name: "Difficulty",
                    variant: "ANOTHER",
                },
                Token::U8(7),
                Token::Bytes(b"A708"),
                Token::TupleEnd,
                Token::SeqEnd,
                Token::Str("DOUBLE"),
                Token::Seq {len: Some(1)},
                Token::Tuple {len: 4},
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
                Token::Seq {len: Some(2)},
                Token::Tuple {len: 4},
                Token::UnitVariant {
                    name: "Difficulty",
                    variant: "BASIC",
                },
                Token::U8(2),
                Token::Bytes(b"9230"),
                Token::Bytes(b"88A8"),
                Token::TupleEnd,
                Token::Tuple {len: 4},
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
            ]
        )
    }
}
