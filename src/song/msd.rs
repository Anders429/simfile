use crate::song;
use serde::{
    de::{EnumAccess, Error, MapAccess, Unexpected, VariantAccess, Visitor},
    ser::SerializeStruct,
    Deserialize, Deserializer, Serialize, Serializer,
};
use std::{cmp::Ordering, fmt, iter, mem::MaybeUninit, str};

/// All valid panel combinations, according to the MSD specification.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(in crate::song) enum Panels {
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

impl Panels {
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

impl From<Panels> for [song::Panel; 4] {
    fn from(step: Panels) -> Self {
        match step {
            Panels::None => [song::Panel::None; 4],
            Panels::Left => [
                song::Panel::Step,
                song::Panel::None,
                song::Panel::None,
                song::Panel::None,
            ],
            Panels::Down => [
                song::Panel::None,
                song::Panel::Step,
                song::Panel::None,
                song::Panel::None,
            ],
            Panels::Up => [
                song::Panel::None,
                song::Panel::None,
                song::Panel::Step,
                song::Panel::None,
            ],
            Panels::Right => [
                song::Panel::None,
                song::Panel::None,
                song::Panel::None,
                song::Panel::Step,
            ],
            Panels::DownLeft => [
                song::Panel::Step,
                song::Panel::Step,
                song::Panel::None,
                song::Panel::None,
            ],
            Panels::UpLeft => [
                song::Panel::Step,
                song::Panel::None,
                song::Panel::Step,
                song::Panel::None,
            ],
            Panels::LeftRight => [
                song::Panel::Step,
                song::Panel::None,
                song::Panel::None,
                song::Panel::Step,
            ],
            Panels::UpDown => [
                song::Panel::None,
                song::Panel::Step,
                song::Panel::Step,
                song::Panel::None,
            ],
            Panels::DownRight => [
                song::Panel::None,
                song::Panel::Step,
                song::Panel::None,
                song::Panel::Step,
            ],
            Panels::UpRight => [
                song::Panel::None,
                song::Panel::None,
                song::Panel::Step,
                song::Panel::Step,
            ],
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(in crate::song) enum Notes {
    Eighth(Panels),
    Sixteenth(Panels),
}

impl Notes {
    fn serialization_capacity_requirement(&self, previous: Option<Self>) -> usize {
        match self {
            Notes::Eighth(_) => 1 + matches!(previous, Some(Notes::Sixteenth(_))) as usize,
            Notes::Sixteenth(_) => 1 + !matches!(previous, Some(Notes::Sixteenth(_))) as usize,
        }
    }

    fn serialize_to_bytes(&self, bytes: &mut Vec<u8>, previous: Option<Self>) {
        match self {
            Notes::Eighth(step) => {
                if matches!(previous, Some(Notes::Sixteenth(_))) {
                    bytes.push(b')');
                }
                bytes.push(step.as_serialized_byte());
            }
            Notes::Sixteenth(step) => {
                if !matches!(previous, Some(Notes::Sixteenth(_))) {
                    bytes.push(b'(');
                }
                bytes.push(step.as_serialized_byte());
            }
        }
    }

    /// How many 192nd notes this value takes up.
    fn as_isize(&self) -> isize {
        match self {
            Notes::Eighth(_) => 24,
            Notes::Sixteenth(_) => 12,
        }
    }

    fn panels(&self) -> Panels {
        match self {
            Notes::Eighth(panels) | Notes::Sixteenth(panels) => *panels,
        }
    }

    fn into_steps_for_length(&self, mut length: isize) -> Vec<song::Step<4>> {
        let mut steps = Vec::new();

        // Convert step.
        if 24 <= length {
            steps.push(song::Step {
                panels: self.panels().into(),
                duration: song::Duration::Eighth,
            });
            length -= 24;
        } else if 12 <= length {
            steps.push(song::Step {
                panels: self.panels().into(),
                duration: song::Duration::Sixteenth,
            });
            length -= 12;
        } else {
            // Shouldn't ever get here.
            unreachable!()
        }

        // Fill rest of space.
        while length > 0 {
            if 24 <= length {
                steps.push(song::Step {
                    panels: Panels::None.into(),
                    duration: song::Duration::Eighth,
                });
                length -= 24;
            } else if 12 <= length {
                steps.push(song::Step {
                    panels: Panels::None.into(),
                    duration: song::Duration::Sixteenth,
                });
                length -= 12;
            } else {
                // Shouldn't ever get here.
                unreachable!()
            }
        }
        steps
    }
}

impl PartialOrd for Notes {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Notes {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_isize().cmp(&other.as_isize())
    }
}

impl From<Notes> for song::Duration {
    fn from(notes: Notes) -> Self {
        match notes {
            Notes::Eighth(_) => song::Duration::Eighth,
            Notes::Sixteenth(_) => song::Duration::Sixteenth,
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
        let mut result = Vec::with_capacity({
            let mut previous = None;
            let mut sum = 0;
            for &notes in &self.notes {
                sum += notes.serialization_capacity_requirement(previous);
                previous = Some(notes);
            }
            sum
        });
        let mut previous = None;
        for &notes in &self.notes {
            notes.serialize_to_bytes(&mut result, previous);
            previous = Some(notes);
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
                // TODO: Separate out the duration into a separate enum, distinct from the step.
                // Then this part will scale better with other granularities.
                let mut sixteenth = false;
                for &byte in bytes.iter().filter(|b| !b.is_ascii_whitespace()) {
                    match byte {
                        b'(' => {
                            sixteenth = true;
                            continue;
                        }
                        b')' => {
                            sixteenth = false;
                            continue;
                        }
                        _ => {}
                    }
                    if sixteenth {
                        notes.push(Notes::Sixteenth(Panels::from_serialized_byte(byte)?));
                    } else {
                        notes.push(Notes::Eighth(Panels::from_serialized_byte(byte)?));
                    }
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
                Notes::Sixteenth(step) => {
                    steps.push(song::Step {
                        panels: step.into(),
                        duration: song::Duration::Sixteenth,
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

        // If both are at same point, pop them both and add.
        // If one is ahead of the other, pop that one only and add.

        // If this is positive, then right side is ahead. If negative, then left side is ahead.
        let mut alignment = 0;
        let mut notes_0 = msd_steps.0.notes.iter();
        let mut notes_1 = msd_steps.1.notes.iter();
        loop {
            match alignment.cmp(&0) {
                Ordering::Equal => {
                    match (notes_0.next(), notes_1.next()) {
                        (Some(&left_notes), Some(&right_notes)) => {
                            // Find with duration to use.
                            match left_notes.cmp(&right_notes) {
                                Ordering::Equal | Ordering::Less => {
                                    steps.push(song::Step {
                                        panels: {
                                            let mut whole = MaybeUninit::uninit();
                                            let ptr = whole.as_mut_ptr() as *mut [song::Panel; 4];
                                            unsafe {
                                                ptr.write(left_notes.panels().into());
                                                ptr.add(1).write(right_notes.panels().into());
                                                whole.assume_init()
                                            }
                                        },
                                        duration: left_notes.into(),
                                    });
                                }
                                Ordering::Greater => {
                                    steps.push(song::Step {
                                        panels: {
                                            let mut whole = MaybeUninit::uninit();
                                            let ptr = whole.as_mut_ptr() as *mut [song::Panel; 4];
                                            unsafe {
                                                ptr.write(left_notes.panels().into());
                                                ptr.add(1).write(right_notes.panels().into());
                                                whole.assume_init()
                                            }
                                        },
                                        duration: right_notes.into(),
                                    });
                                }
                            }
                            // Remember where the notes are currently aligned to.
                            // Note that these values may be the opposite of what you intuitively
                            // think, as they represent the denominators of their note durations.
                            alignment = left_notes.as_isize() - right_notes.as_isize();
                        }
                        (Some(left_notes), None) => {
                            for &notes in iter::once(left_notes).chain(notes_0) {
                                steps.push(song::Step {
                                    panels: {
                                        let mut whole = MaybeUninit::uninit();
                                        let ptr = whole.as_mut_ptr() as *mut [song::Panel; 4];
                                        unsafe {
                                            ptr.write(notes.panels().into());
                                            ptr.add(1).write(Panels::None.into());
                                            whole.assume_init()
                                        }
                                    },
                                    duration: notes.into(),
                                });
                            }
                            break;
                        }
                        (None, Some(right_notes)) => {
                            for &notes in iter::once(right_notes).chain(notes_1) {
                                steps.push(song::Step {
                                    panels: {
                                        let mut whole = MaybeUninit::uninit();
                                        let ptr = whole.as_mut_ptr() as *mut [song::Panel; 4];
                                        unsafe {
                                            ptr.write(Panels::None.into());
                                            ptr.add(1).write(notes.panels().into());
                                            whole.assume_init()
                                        }
                                    },
                                    duration: notes.into(),
                                });
                            }
                            break;
                        }
                        (None, None) => {
                            break;
                        }
                    }
                }
                Ordering::Greater => {
                    // Just do the right side.
                    match notes_1.next() {
                        Some(&notes) => {
                            // Find duration.
                            let duration_value = notes.as_isize();
                            if duration_value > alignment {
                                // Take care not to skip past the other side's note.
                                for step in notes.into_steps_for_length(duration_value - alignment)
                                {
                                    steps.push(song::Step {
                                        panels: {
                                            let mut whole = MaybeUninit::uninit();
                                            let ptr = whole.as_mut_ptr() as *mut [song::Panel; 4];
                                            unsafe {
                                                ptr.write(Panels::None.into());
                                                ptr.add(1).write(step.panels);
                                                whole.assume_init()
                                            }
                                        },
                                        duration: step.duration,
                                    });
                                }
                            } else {
                                steps.push(song::Step {
                                    panels: {
                                        let mut whole = MaybeUninit::uninit();
                                        let ptr = whole.as_mut_ptr() as *mut [song::Panel; 4];
                                        unsafe {
                                            ptr.write(Panels::None.into());
                                            ptr.add(1).write(notes.panels().into());
                                            whole.assume_init()
                                        }
                                    },
                                    duration: notes.into(),
                                });
                            }

                            alignment -= duration_value;
                        }
                        None => {
                            for &notes in notes_0 {
                                steps.push(song::Step {
                                    panels: {
                                        let mut whole = MaybeUninit::uninit();
                                        let ptr = whole.as_mut_ptr() as *mut [song::Panel; 4];
                                        unsafe {
                                            ptr.write(notes.panels().into());
                                            ptr.add(1).write(Panels::None.into());
                                            whole.assume_init()
                                        }
                                    },
                                    duration: notes.into(),
                                });
                            }
                            break;
                        }
                    }
                }
                Ordering::Less => {
                    // Just do the left side.
                    match notes_0.next() {
                        Some(&notes) => {
                            // Find duration.
                            let duration_value = notes.as_isize();
                            if duration_value > -alignment {
                                // Take care not to skip past the other side's note.
                                for step in notes.into_steps_for_length(duration_value + alignment)
                                {
                                    steps.push(song::Step {
                                        panels: {
                                            let mut whole = MaybeUninit::uninit();
                                            let ptr = whole.as_mut_ptr() as *mut [song::Panel; 4];
                                            unsafe {
                                                ptr.write(step.panels);
                                                ptr.add(1).write(Panels::None.into());
                                                whole.assume_init()
                                            }
                                        },
                                        duration: step.duration,
                                    });
                                }
                            } else {
                                steps.push(song::Step {
                                    panels: {
                                        let mut whole = MaybeUninit::uninit();
                                        let ptr = whole.as_mut_ptr() as *mut [song::Panel; 4];
                                        unsafe {
                                            ptr.write(notes.panels().into());
                                            ptr.add(1).write(Panels::None.into());
                                            whole.assume_init()
                                        }
                                    },
                                    duration: notes.into(),
                                });
                            }

                            alignment += duration_value;
                        }
                        None => {
                            for &notes in notes_1 {
                                steps.push(song::Step {
                                    panels: {
                                        let mut whole = MaybeUninit::uninit();
                                        let ptr = whole.as_mut_ptr() as *mut [song::Panel; 4];
                                        unsafe {
                                            ptr.write(Panels::None.into());
                                            ptr.add(1).write(notes.panels().into());
                                            whole.assume_init()
                                        }
                                    },
                                    duration: notes.into(),
                                });
                            }
                            break;
                        }
                    }
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

impl From<Difficulty> for song::Difficulty {
    fn from(difficulty: Difficulty) -> Self {
        match difficulty {
            Difficulty::Basic => song::Difficulty::Easy,
            Difficulty::Another => song::Difficulty::Medium,
            Difficulty::Maniac => song::Difficulty::Hard,
        }
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

        // Possibly extract a subtitle from the title.
        let title;
        let subtitle;
        if let Some(song_title) = song.title {
            let (split_title, split_subtitle) = song::util::split_title_and_subtitle(song_title);
            title = Some(split_title);
            subtitle = split_subtitle;
        } else {
            title = song.title;
            subtitle = None;
        }

        song::Song {
            title: title,
            subtitle: subtitle,
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
    use crate::song;
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
                    Notes::Eighth(Panels::Up),
                    Notes::Eighth(Panels::LeftRight),
                    Notes::Eighth(Panels::None),
                    Notes::Eighth(Panels::DownRight),
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
    fn song_ser_de_full() {
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
                                Notes::Eighth(Panels::Up),
                                Notes::Eighth(Panels::None),
                                Notes::Eighth(Panels::Down),
                                Notes::Eighth(Panels::Right),
                            ],
                        },
                    ),
                    (
                        Difficulty::Another,
                        7,
                        Steps {
                            notes: vec![
                                Notes::Eighth(Panels::UpDown),
                                Notes::Eighth(Panels::UpLeft),
                                Notes::Eighth(Panels::None),
                                Notes::Eighth(Panels::Up),
                            ],
                        },
                    ),
                ],
                double: vec![(
                    Difficulty::Maniac,
                    9,
                    Steps {
                        notes: vec![
                            Notes::Eighth(Panels::Down),
                            Notes::Eighth(Panels::Down),
                            Notes::Eighth(Panels::Down),
                            Notes::Eighth(Panels::Down),
                        ],
                    },
                    Steps {
                        notes: vec![
                            Notes::Eighth(Panels::Up),
                            Notes::Eighth(Panels::Down),
                            Notes::Eighth(Panels::LeftRight),
                            Notes::Eighth(Panels::DownLeft),
                        ],
                    },
                )],
                couple: vec![
                    (
                        Difficulty::Basic,
                        2,
                        Steps {
                            notes: vec![
                                Notes::Eighth(Panels::UpRight),
                                Notes::Eighth(Panels::Down),
                                Notes::Eighth(Panels::DownRight),
                                Notes::Eighth(Panels::None),
                            ],
                        },
                        Steps {
                            notes: vec![
                                Notes::Eighth(Panels::Up),
                                Notes::Eighth(Panels::Up),
                                Notes::Eighth(Panels::UpDown),
                                Notes::Eighth(Panels::Up),
                            ],
                        },
                    ),
                    (
                        Difficulty::Another,
                        6,
                        Steps {
                            notes: vec![
                                Notes::Eighth(Panels::None),
                                Notes::Eighth(Panels::None),
                                Notes::Eighth(Panels::None),
                                Notes::Eighth(Panels::None),
                            ],
                        },
                        Steps {
                            notes: vec![
                                Notes::Eighth(Panels::Left),
                                Notes::Eighth(Panels::Up),
                                Notes::Eighth(Panels::Right),
                                Notes::Eighth(Panels::Down),
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

    #[test]
    fn double_steps_into_generic_steps() {
        let steps_0 = Steps {
            notes: vec![
                Notes::Eighth(Panels::Up),
                Notes::Sixteenth(Panels::Down),
                Notes::Eighth(Panels::Right),
                Notes::Sixteenth(Panels::Left),
            ],
        };
        let steps_1 = Steps {
            notes: vec![
                Notes::Sixteenth(Panels::Up),
                Notes::Sixteenth(Panels::Down),
                Notes::Eighth(Panels::Right),
                Notes::Sixteenth(Panels::UpDown),
                Notes::Sixteenth(Panels::Left),
            ],
        };

        assert_eq!(song::Steps::from((steps_0, steps_1)), song::Steps {
            steps: vec![
                song::Step {
                    panels: [
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::Step,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::Step,
                        song::Panel::None,
                    ],
                    duration: song::Duration::Sixteenth,
                },
                song::Step {
                    panels: [
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::Step,
                        song::Panel::None,
                        song::Panel::None,
                    ],
                    duration: song::Duration::Sixteenth,
                },
                song::Step {
                    panels: [
                        song::Panel::None,
                        song::Panel::Step,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::Step,
                    ],
                    duration: song::Duration::Sixteenth,
                },
                song::Step {
                    panels: [
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::Step,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                    ],
                    duration: song::Duration::Sixteenth,
                },
                song::Step {
                    panels: [
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::Step,
                        song::Panel::Step,
                        song::Panel::None,
                    ],
                    duration: song::Duration::Sixteenth,
                },
                song::Step {
                    panels: [
                        song::Panel::Step,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::Step,
                        song::Panel::None,
                        song::Panel::None,
                        song::Panel::None,
                    ],
                    duration: song::Duration::Sixteenth,
                },
            ],
        });
    }
}
