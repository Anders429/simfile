use super::util;
use crate::song;
use serde::{
    de,
    de::{EnumAccess, MapAccess, Unexpected, VariantAccess, Visitor},
    ser::SerializeStruct,
    Deserialize, Deserializer, Serialize, Serializer,
};
use std::{cmp::Ordering, fmt, iter, mem::MaybeUninit, str};

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    UnsupportedPanelCombination,
    UnsupportedDifficulty,
}

/// All valid panel combinations, according to the MSD specification.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Panels {
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
        E: de::Error,
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
            _ => Err(de::Error::invalid_value(
                Unexpected::Char(byte.into()),
                &"`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, or `B`",
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

impl TryFrom<[song::Panel; 4]> for Panels {
    type Error = Error;

    fn try_from(panels: [song::Panel; 4]) -> Result<Self, Self::Error> {
        match panels {
            [song::Panel::None, song::Panel::None, song::Panel::None, song::Panel::None] => {
                Ok(Panels::None)
            }
            [song::Panel::Step, song::Panel::None, song::Panel::None, song::Panel::None] => {
                Ok(Panels::Left)
            }
            [song::Panel::None, song::Panel::Step, song::Panel::None, song::Panel::None] => {
                Ok(Panels::Down)
            }
            [song::Panel::None, song::Panel::None, song::Panel::Step, song::Panel::None] => {
                Ok(Panels::Up)
            }
            [song::Panel::None, song::Panel::None, song::Panel::None, song::Panel::Step] => {
                Ok(Panels::Right)
            }
            [song::Panel::Step, song::Panel::Step, song::Panel::None, song::Panel::None] => {
                Ok(Panels::DownLeft)
            }
            [song::Panel::Step, song::Panel::None, song::Panel::Step, song::Panel::None] => {
                Ok(Panels::UpLeft)
            }
            [song::Panel::Step, song::Panel::None, song::Panel::None, song::Panel::Step] => {
                Ok(Panels::LeftRight)
            }
            [song::Panel::None, song::Panel::Step, song::Panel::Step, song::Panel::None] => {
                Ok(Panels::UpDown)
            }
            [song::Panel::None, song::Panel::Step, song::Panel::None, song::Panel::Step] => {
                Ok(Panels::DownRight)
            }
            [song::Panel::None, song::Panel::None, song::Panel::Step, song::Panel::Step] => {
                Ok(Panels::UpRight)
            }
            _ => Err(Error::UnsupportedPanelCombination),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Duration {
    Eighth,
    Sixteenth,
    TwentyFourth,
    SixtyFourth,
}

impl Duration {
    fn serialization_capacity_requirement(&self, previous: Option<Self>) -> usize {
        match self {
            Duration::Eighth => {
                1 + matches!(
                    previous,
                    Some(Duration::Sixteenth)
                        | Some(Duration::TwentyFourth)
                        | Some(Duration::SixtyFourth)
                ) as usize
            }
            Duration::Sixteenth => {
                1 + !matches!(previous, Some(Duration::Sixteenth)) as usize
                    + matches!(
                        previous,
                        Some(Duration::TwentyFourth) | Some(Duration::SixtyFourth)
                    ) as usize
            }
            Duration::TwentyFourth => {
                1 + !matches!(previous, Some(Duration::TwentyFourth)) as usize
                    + matches!(
                        previous,
                        Some(Duration::Sixteenth) | Some(Duration::SixtyFourth)
                    ) as usize
            }
            Duration::SixtyFourth => {
                1 + !matches!(previous, Some(Duration::SixtyFourth)) as usize
                    + matches!(
                        previous,
                        Some(Duration::Sixteenth) | Some(Duration::TwentyFourth)
                    ) as usize
            }
        }
    }

    /// How many 192nd notes this value takes up.
    fn as_isize(&self) -> isize {
        match self {
            Self::Eighth => 24,
            Self::Sixteenth => 12,
            Self::TwentyFourth => 8,
            Self::SixtyFourth => 3,
        }
    }
}

impl PartialOrd for Duration {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Duration {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_isize().cmp(&other.as_isize())
    }
}

impl From<Duration> for song::Duration {
    fn from(duration: Duration) -> Self {
        match duration {
            Duration::Eighth => song::Duration::Eighth,
            Duration::Sixteenth => song::Duration::Sixteenth,
            Duration::TwentyFourth => song::Duration::TwentyFourth,
            Duration::SixtyFourth => song::Duration::SixtyFourth,
        }
    }
}

impl TryFrom<song::Duration> for Duration {
    type Error = Error;

    fn try_from(duration: song::Duration) -> Result<Self, Self::Error> {
        match duration {
            song::Duration::Eighth => Ok(Duration::Eighth),
            song::Duration::Sixteenth => Ok(Duration::Sixteenth),
            song::Duration::TwentyFourth => Ok(Duration::TwentyFourth),
            song::Duration::SixtyFourth => Ok(Duration::SixtyFourth),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Step {
    panels: Panels,
    duration: Duration,
}

impl Step {
    fn serialize_to_bytes(&self, bytes: &mut Vec<u8>, previous: Option<Duration>) {
        match self.duration {
            Duration::Eighth => {
                if matches!(previous, Some(Duration::Sixteenth)) {
                    bytes.push(b')');
                } else if matches!(previous, Some(Duration::TwentyFourth)) {
                    bytes.push(b']');
                } else if matches!(previous, Some(Duration::SixtyFourth)) {
                    bytes.push(b'}');
                }
                bytes.push(self.panels.as_serialized_byte());
            }
            Duration::Sixteenth => {
                if matches!(previous, Some(Duration::TwentyFourth)) {
                    bytes.push(b']');
                } else if matches!(previous, Some(Duration::SixtyFourth)) {
                    bytes.push(b'}');
                }
                if !matches!(previous, Some(Duration::Sixteenth)) {
                    bytes.push(b'(');
                }
                bytes.push(self.panels.as_serialized_byte());
            }
            Duration::TwentyFourth => {
                if matches!(previous, Some(Duration::Sixteenth)) {
                    bytes.push(b')');
                } else if matches!(previous, Some(Duration::SixtyFourth)) {
                    bytes.push(b'}');
                }
                if !matches!(previous, Some(Duration::TwentyFourth)) {
                    bytes.push(b'[');
                }
                bytes.push(self.panels.as_serialized_byte());
            }
            Duration::SixtyFourth => {
                if matches!(previous, Some(Duration::Sixteenth)) {
                    bytes.push(b')');
                } else if matches!(previous, Some(Duration::TwentyFourth)) {
                    bytes.push(b']');
                }
                if !matches!(previous, Some(Duration::SixtyFourth)) {
                    bytes.push(b'{');
                }
                bytes.push(self.panels.as_serialized_byte());
            }
        }
    }

    fn into_steps_for_length(self, mut length: isize) -> Vec<song::Step<4>> {
        let mut steps = Vec::new();

        // Convert step.
        if 24 <= length {
            steps.push(song::Step {
                panels: self.panels.into(),
                duration: song::Duration::Eighth,
            });
            length -= 24;
        } else if 12 <= length {
            steps.push(song::Step {
                panels: self.panels.into(),
                duration: song::Duration::Sixteenth,
            });
            length -= 12;
        } else if 8 <= length {
            steps.push(song::Step {
                panels: self.panels.into(),
                duration: song::Duration::TwentyFourth,
            })
        } else if 3 <= length {
            steps.push(song::Step {
                panels: self.panels.into(),
                duration: song::Duration::SixtyFourth,
            })
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
            } else if 8 <= length {
                steps.push(song::Step {
                    panels: Panels::None.into(),
                    duration: song::Duration::TwentyFourth,
                })
            } else if 3 <= length {
                steps.push(song::Step {
                    panels: Panels::None.into(),
                    duration: song::Duration::SixtyFourth,
                })
            } else {
                // Shouldn't ever get here.
                unreachable!()
            }
        }
        steps
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Steps {
    steps: Vec<Step>,
}

impl Serialize for Steps {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut result = Vec::with_capacity({
            let mut previous = None;
            let mut sum = 0;
            for &step in &self.steps {
                sum += step.duration.serialization_capacity_requirement(previous);
                previous = Some(step.duration);
            }
            sum
        });
        let mut previous = None;
        for &step in &self.steps {
            step.serialize_to_bytes(&mut result, previous);
            previous = Some(step.duration);
        }
        match previous {
            Some(Duration::Sixteenth) => result.push(b')'),
            Some(Duration::TwentyFourth) => result.push(b']'),
            Some(Duration::SixtyFourth) => result.push(b'}'),
            _ => {}
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
                E: de::Error,
            {
                // TODO: Make this capacity approximation more intelligent.
                let mut steps = Vec::with_capacity(bytes.len());
                let mut duration = Duration::Eighth;
                for &byte in bytes.iter().filter(|b| !b.is_ascii_whitespace()) {
                    match byte {
                        b'(' => {
                            if !matches!(duration, Duration::Eighth) {
                                return Err(de::Error::invalid_value(
                                    Unexpected::Other("incorrectly nested `(`"),
                                    &"`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, or `B`",
                                ));
                            }
                            duration = Duration::Sixteenth;
                            continue;
                        }
                        b'[' => {
                            if !matches!(duration, Duration::Eighth) {
                                return Err(de::Error::invalid_value(
                                    Unexpected::Other("incorrectly nested `[`"),
                                    &"`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, or `B`",
                                ));
                            }
                            duration = Duration::TwentyFourth;
                            continue;
                        }
                        b'{' => {
                            if !matches!(duration, Duration::Eighth) {
                                return Err(de::Error::invalid_value(
                                    Unexpected::Other("incorrectly nested `{`"),
                                    &"`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, or `B`",
                                ));
                            }
                            duration = Duration::SixtyFourth;
                            continue;
                        }
                        b')' => {
                            if !matches!(duration, Duration::Sixteenth) {
                                return Err(de::Error::invalid_value(
                                    Unexpected::Other("mismatched `)`"),
                                    &match duration {
                                        Duration::Eighth => "`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, or `B`",
                                        Duration::TwentyFourth => "`]`",
                                        Duration::SixtyFourth => "`}`",
                                        _ => unreachable!(),
                                    },
                                ));
                            }
                            duration = Duration::Eighth;
                            continue;
                        }
                        b']' => {
                            if !matches!(duration, Duration::TwentyFourth) {
                                return Err(de::Error::invalid_value(
                                    Unexpected::Other("mismatched `)`"),
                                    &match duration {
                                        Duration::Eighth => "`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, or `B`",
                                        Duration::Sixteenth => "`)`",
                                        Duration::SixtyFourth => "`}`",
                                        _ => unreachable!(),
                                    },
                                ));
                            }
                            duration = Duration::Eighth;
                            continue;
                        }
                        b'}' => {
                            if !matches!(duration, Duration::SixtyFourth) {
                                return Err(de::Error::invalid_value(
                                    Unexpected::Other("mismatched `)`"),
                                    &match duration {
                                        Duration::Eighth => "`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, or `B`",
                                        Duration::Sixteenth => "`)`",
                                        Duration::TwentyFourth => "`]`",
                                        _ => unreachable!(),
                                    },
                                ));
                            }
                            duration = Duration::Eighth;
                            continue;
                        }
                        _ => {}
                    }
                    steps.push(Step {
                        panels: Panels::from_serialized_byte(byte)?,
                        duration,
                    });
                }
                Ok(Steps { steps })
            }
        }

        deserializer.deserialize_bytes(StepsVisitor)
    }
}

impl From<Steps> for song::Steps<4> {
    fn from(msd_steps: Steps) -> Self {
        let mut steps = Vec::new();

        for step in msd_steps.steps {
            steps.push(song::Step {
                panels: step.panels.into(),
                duration: step.duration.into(),
            });
        }

        song::Steps { steps }
    }
}

impl TryFrom<song::Steps<4>> for Steps {
    type Error = Error;

    fn try_from(song_steps: song::Steps<4>) -> Result<Self, Self::Error> {
        let mut steps = Vec::new();

        for step in song_steps.steps {
            steps.push(Step {
                panels: step.panels.try_into()?,
                duration: step.duration.try_into()?,
            });
        }

        Ok(Steps { steps })
    }
}

impl From<(Steps, Steps)> for song::Steps<8> {
    fn from(msd_steps: (Steps, Steps)) -> Self {
        fn combine_steps(
            left: [song::Panel; 4],
            right: [song::Panel; 4],
            duration: song::Duration,
        ) -> song::Step<8> {
            song::Step {
                panels: {
                    let mut whole = MaybeUninit::uninit();
                    let ptr = whole.as_mut_ptr() as *mut [song::Panel; 4];
                    // SAFETY: The entirety of `whole` is initialized properly by these writes.
                    unsafe {
                        ptr.write(left);
                        ptr.add(1).write(right);
                        whole.assume_init()
                    }
                },
                duration,
            }
        }

        let mut steps = Vec::new();

        // If both are at same point, pop them both and add.
        // If one is ahead of the other, pop that one only and add.

        // If this is positive, then right side is ahead. If negative, then left side is ahead.
        let mut alignment = 0;
        let mut steps_0 = msd_steps.0.steps.iter();
        let mut steps_1 = msd_steps.1.steps.iter();
        loop {
            match alignment.cmp(&0) {
                Ordering::Equal => {
                    match (steps_0.next(), steps_1.next()) {
                        (Some(&left_step), Some(&right_step)) => {
                            // Find with duration to use.
                            match left_step.duration.cmp(&right_step.duration) {
                                Ordering::Equal | Ordering::Less => {
                                    steps.push(combine_steps(
                                        left_step.panels.into(),
                                        right_step.panels.into(),
                                        left_step.duration.into(),
                                    ));
                                }
                                Ordering::Greater => {
                                    steps.push(combine_steps(
                                        left_step.panels.into(),
                                        right_step.panels.into(),
                                        right_step.duration.into(),
                                    ));
                                }
                            }
                            alignment =
                                left_step.duration.as_isize() - right_step.duration.as_isize();
                        }
                        (Some(left_step), None) => {
                            for &step in iter::once(left_step).chain(steps_0) {
                                steps.push(combine_steps(
                                    step.panels.into(),
                                    Panels::None.into(),
                                    step.duration.into(),
                                ));
                            }
                            break;
                        }
                        (None, Some(right_step)) => {
                            for &step in iter::once(right_step).chain(steps_1) {
                                steps.push(combine_steps(
                                    step.panels.into(),
                                    Panels::None.into(),
                                    step.duration.into(),
                                ));
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
                    match steps_1.next() {
                        Some(&step) => {
                            // Find duration.
                            let duration_value = step.duration.as_isize();
                            if duration_value > alignment {
                                // Take care not to skip past the other side's note.
                                for step in step.into_steps_for_length(duration_value - alignment) {
                                    steps.push(combine_steps(
                                        Panels::None.into(),
                                        step.panels,
                                        step.duration,
                                    ));
                                }
                            } else {
                                steps.push(combine_steps(
                                    Panels::None.into(),
                                    step.panels.into(),
                                    step.duration.into(),
                                ));
                            }

                            alignment -= duration_value;
                        }
                        None => {
                            for &step in steps_0 {
                                steps.push(combine_steps(
                                    step.panels.into(),
                                    Panels::None.into(),
                                    step.duration.into(),
                                ));
                            }
                            break;
                        }
                    }
                }
                Ordering::Less => {
                    // Just do the left side.
                    match steps_0.next() {
                        Some(&step) => {
                            // Find duration.
                            let duration_value = step.duration.as_isize();
                            if duration_value > -alignment {
                                // Take care not to skip past the other side's note.
                                for step in step.into_steps_for_length(duration_value + alignment) {
                                    steps.push(combine_steps(
                                        step.panels,
                                        Panels::None.into(),
                                        step.duration,
                                    ));
                                }
                            } else {
                                steps.push(combine_steps(
                                    step.panels.into(),
                                    Panels::None.into(),
                                    step.duration.into(),
                                ));
                            }

                            alignment += duration_value;
                        }
                        None => {
                            for &step in steps_1 {
                                steps.push(combine_steps(
                                    Panels::None.into(),
                                    step.panels.into(),
                                    step.duration.into(),
                                ));
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

impl TryFrom<song::Steps<8>> for (Steps, Steps) {
    type Error = Error;

    fn try_from(steps: song::Steps<8>) -> Result<Self, Self::Error> {
        let mut left_steps = Vec::new();
        let mut right_steps = Vec::new();

        for step in steps.steps {
            let left_panels;
            let right_panels;
            let panels_ptr = step.panels.as_ptr() as *const [song::Panel; 4];
            // SAFETY: The reads done here are safe. The source array is 8 elements long, so it is
            // safe to read as 2 arrays of 4 elements each.
            unsafe {
                left_panels = MaybeUninit::new(panels_ptr.read());
                right_panels = MaybeUninit::new(panels_ptr.add(1).read());
            }
            left_steps.push(Step {
                // SAFETY: `left_panels` is guaranteed to be completely filled from the above reads.
                panels: unsafe { left_panels.assume_init() }.try_into()?,
                duration: step.duration.try_into()?,
            });
            right_steps.push(Step {
                // SAFETY: `right_panels` is guaranteed to be completely filled from the above reads.
                panels: unsafe { right_panels.assume_init() }.try_into()?,
                duration: step.duration.try_into()?,
            });
        }

        Ok((Steps { steps: left_steps }, Steps { steps: right_steps }))
    }
}

/// All valid MSD difficulties.
#[derive(Debug, Eq, PartialEq)]
enum Difficulty {
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
                        E: de::Error,
                    {
                        match value {
                            "BASIC" => Ok(Variant::Basic),
                            "ANOTHER" => Ok(Variant::Another),
                            "MANIAC" => Ok(Variant::Maniac),
                            _ => Err(de::Error::unknown_variant(value, VARIANTS)),
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

        const VARIANTS: &[&str] = &["BASIC", "ANOTHER", "MANIAC"];

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

impl TryFrom<song::Difficulty> for Difficulty {
    type Error = Error;

    fn try_from(difficulty: song::Difficulty) -> Result<Self, Self::Error> {
        match difficulty {
            song::Difficulty::Easy => Ok(Self::Basic),
            song::Difficulty::Medium => Ok(Self::Another),
            song::Difficulty::Hard => Ok(Self::Maniac),
            _ => Err(Error::UnsupportedDifficulty),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(in crate::song) struct Song {
    file: Option<String>,
    title: Option<String>,
    artist: Option<String>,
    msd: Option<String>,
    bpm: Option<f64>,
    gap: Option<i64>,
    back: Option<String>,
    bgm: Option<String>,
    select: Option<String>,
    single: Vec<(Difficulty, u8, Steps)>,
    double: Vec<(Difficulty, u8, Steps, Steps)>,
    couple: Vec<(Difficulty, u8, Steps, Steps)>,
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
            Remixer,
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
                        formatter.write_str("`FILE`, `TITLE`, `ARTIST`, `MSD`, `REMIXER`, `BPM`, `GAP`, `BACK`, `BGM`, `SELECT`, `SINGLE`, `DOUBLE`, or `COUPLE`")
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        match value {
                            "FILE" => Ok(Field::File),
                            "TITLE" => Ok(Field::Title),
                            "ARTIST" => Ok(Field::Artist),
                            "MSD" => Ok(Field::Msd),
                            "REMIXER" => Ok(Field::Remixer),
                            "BPM" => Ok(Field::Bpm),
                            "GAP" => Ok(Field::Gap),
                            "BACK" => Ok(Field::Back),
                            "BGM" => Ok(Field::Bgm),
                            "SELECT" => Ok(Field::Select),
                            "SINGLE" => Ok(Field::Single),
                            "DOUBLE" => Ok(Field::Double),
                            "COUPLE" => Ok(Field::Couple),
                            _ => Err(de::Error::unknown_field(value, FIELDS)),
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
                let mut remixer = None;
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
                                return Err(de::Error::duplicate_field("FILE"));
                            }
                            file = map_access.next_value()?;
                        }
                        Field::Title => {
                            if title.is_some() {
                                return Err(de::Error::duplicate_field("TITLE"));
                            }
                            title = map_access.next_value()?;
                        }
                        Field::Artist => {
                            if artist.is_some() {
                                return Err(de::Error::duplicate_field("ARTIST"));
                            }
                            artist = map_access.next_value()?;
                        }
                        Field::Msd => {
                            if msd.is_some() {
                                return Err(de::Error::duplicate_field("MSD"));
                            }
                            msd = map_access.next_value()?;
                        }
                        Field::Remixer => {
                            if remixer.is_some() {
                                return Err(de::Error::duplicate_field("REMIXER"));
                            }
                            remixer = map_access.next_value()?;
                        }
                        Field::Bpm => {
                            if bpm.is_some() {
                                return Err(de::Error::duplicate_field("BPM"));
                            }
                            bpm = map_access.next_value()?;
                        }
                        Field::Gap => {
                            if gap.is_some() {
                                return Err(de::Error::duplicate_field("GAP"));
                            }
                            gap = map_access.next_value()?;
                        }
                        Field::Back => {
                            if back.is_some() {
                                return Err(de::Error::duplicate_field("BACK"));
                            }
                            back = map_access.next_value()?;
                        }
                        Field::Bgm => {
                            if bgm.is_some() {
                                return Err(de::Error::duplicate_field("BGM"));
                            }
                            bgm = map_access.next_value()?;
                        }
                        Field::Select => {
                            if select.is_some() {
                                return Err(de::Error::duplicate_field("SELECT"));
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
                    // Note that both `#MSD` and `#REMIXER` appear to have been used interchangably
                    // to store the name of the step artist. We prefer `#MSD` over `#REMIXER`,
                    // since it seems to be more common.
                    msd: msd.or(remixer),
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

        const FIELDS: &[&str] = &[
            "FILE", "TITLE", "ARTIST", "MSD", "REMIXER", "BPM", "GAP", "BACK", "BGM", "SELECT",
            "SINGLE", "DOUBLE", "COUPLE",
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
            title,
            subtitle,
            artist: song.artist,
            credit: song.msd,

            bpm: song.bpm,
            offset: song.gap,

            background_file: song.back,
            music_preview_file: song.select,
            music_file: song.bgm,

            charts,
        }
    }
}

impl TryFrom<song::Song> for Song {
    type Error = Error;

    fn try_from(song: song::Song) -> Result<Self, Self::Error> {
        let mut single = Vec::new();
        let mut double = Vec::new();
        let mut couple = Vec::new();

        for chart in song.charts {
            match chart.style {
                song::Style::Single(steps) => {
                    single.push((chart.difficulty.try_into()?, chart.meter, steps.try_into()?))
                }
                song::Style::Double(steps) => {
                    let (left_steps, right_steps) = steps.try_into()?;
                    double.push((
                        chart.difficulty.try_into()?,
                        chart.meter,
                        left_steps,
                        right_steps,
                    ));
                }
                song::Style::Couple(steps) => {
                    let (left_steps, right_steps) = steps.try_into()?;
                    couple.push((
                        chart.difficulty.try_into()?,
                        chart.meter,
                        left_steps,
                        right_steps,
                    ));
                }
            }
        }

        Ok(Song {
            file: None,
            title: {
                if let Some(subtitle) = song.subtitle {
                    Some(util::combine_title_and_subtitle(
                        song.title.unwrap_or_default(),
                        subtitle,
                    ))
                } else {
                    song.title
                }
            },
            artist: song.artist,
            msd: song.credit,
            bpm: song.bpm,
            gap: song.offset,
            back: song.background_file,
            select: song.music_preview_file,
            bgm: song.music_file,
            single,
            double,
            couple,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::song;
    use claim::{assert_err_eq, assert_ok_eq};
    use more_asserts::assert_gt;
    use serde::{
        de,
        de::Unexpected,
    };
    use serde::de::Error as DeError;
    use serde_test::{assert_tokens, Token};

    #[test]
    fn panels_none_as_serialized_byte() {
        assert_eq!(Panels::None.as_serialized_byte(), b'0');
    }

    #[test]
    fn panels_down_left_as_serialized_byte() {
        assert_eq!(Panels::DownLeft.as_serialized_byte(), b'1');
    }

    #[test]
    fn panels_down_as_serialized_byte() {
        assert_eq!(Panels::Down.as_serialized_byte(), b'2');
    }

    #[test]
    fn panels_down_right_as_serialized_byte() {
        assert_eq!(Panels::DownRight.as_serialized_byte(), b'3');
    }

    #[test]
    fn panels_left_as_serialized_byte() {
        assert_eq!(Panels::Left.as_serialized_byte(), b'4');
    }

    #[test]
    fn panels_right_as_serialized_byte() {
        assert_eq!(Panels::Right.as_serialized_byte(), b'6');
    }

    #[test]
    fn panels_up_left_as_serialized_byte() {
        assert_eq!(Panels::UpLeft.as_serialized_byte(), b'7');
    }

    #[test]
    fn panels_up_as_serialized_byte() {
        assert_eq!(Panels::Up.as_serialized_byte(), b'8');
    }

    #[test]
    fn panels_up_right_as_serialized_byte() {
        assert_eq!(Panels::UpRight.as_serialized_byte(), b'9');
    }

    #[test]
    fn panels_up_down_as_serialized_byte() {
        assert_eq!(Panels::UpDown.as_serialized_byte(), b'A');
    }

    #[test]
    fn panels_left_right_as_serialized_byte() {
        assert_eq!(Panels::LeftRight.as_serialized_byte(), b'B');
    }

    #[test]
    fn panels_none_from_serialized_byte() {
        assert_ok_eq!(
            Panels::from_serialized_byte::<de::value::Error>(b'0'),
            Panels::None
        );
    }

    #[test]
    fn panels_down_left_from_serialized_byte() {
        assert_ok_eq!(
            Panels::from_serialized_byte::<de::value::Error>(b'1'),
            Panels::DownLeft
        );
    }

    #[test]
    fn panels_down_from_serialized_byte() {
        assert_ok_eq!(
            Panels::from_serialized_byte::<de::value::Error>(b'2'),
            Panels::Down
        );
    }

    #[test]
    fn panels_down_right_from_serialized_byte() {
        assert_ok_eq!(
            Panels::from_serialized_byte::<de::value::Error>(b'3'),
            Panels::DownRight
        );
    }

    #[test]
    fn panels_left_from_serialized_byte() {
        assert_ok_eq!(
            Panels::from_serialized_byte::<de::value::Error>(b'4'),
            Panels::Left
        );
    }

    #[test]
    fn panels_right_from_serialized_byte() {
        assert_ok_eq!(
            Panels::from_serialized_byte::<de::value::Error>(b'6'),
            Panels::Right
        );
    }

    #[test]
    fn panels_up_left_from_serialized_byte() {
        assert_ok_eq!(
            Panels::from_serialized_byte::<de::value::Error>(b'7'),
            Panels::UpLeft
        );
    }

    #[test]
    fn panels_up_from_serialized_byte() {
        assert_ok_eq!(
            Panels::from_serialized_byte::<de::value::Error>(b'8'),
            Panels::Up
        );
    }

    #[test]
    fn panels_up_right_from_serialized_byte() {
        assert_ok_eq!(
            Panels::from_serialized_byte::<de::value::Error>(b'9'),
            Panels::UpRight
        );
    }

    #[test]
    fn panels_up_down_from_serialized_byte() {
        assert_ok_eq!(
            Panels::from_serialized_byte::<de::value::Error>(b'A'),
            Panels::UpDown
        );
    }

    #[test]
    fn panels_left_right_from_serialized_byte() {
        assert_ok_eq!(
            Panels::from_serialized_byte::<de::value::Error>(b'B'),
            Panels::LeftRight
        );
    }

    #[test]
    fn panels_invalid_from_serialized_byte() {
        assert_err_eq!(
            Panels::from_serialized_byte::<de::value::Error>(b'C'),
            de::value::Error::invalid_value(
                Unexpected::Char('C'),
                &"`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, or `B`",
            )
        );
    }

    #[test]
    fn panels_none_into_generic() {
        assert_eq!(
            <[song::Panel; 4]>::from(Panels::None),
            [
                song::Panel::None,
                song::Panel::None,
                song::Panel::None,
                song::Panel::None
            ]
        );
    }

    #[test]
    fn panels_down_left_into_generic() {
        assert_eq!(
            <[song::Panel; 4]>::from(Panels::DownLeft),
            [
                song::Panel::Step,
                song::Panel::Step,
                song::Panel::None,
                song::Panel::None
            ]
        );
    }

    #[test]
    fn panels_down_into_generic() {
        assert_eq!(
            <[song::Panel; 4]>::from(Panels::Down),
            [
                song::Panel::None,
                song::Panel::Step,
                song::Panel::None,
                song::Panel::None
            ]
        );
    }

    #[test]
    fn panels_down_right_into_generic() {
        assert_eq!(
            <[song::Panel; 4]>::from(Panels::DownRight),
            [
                song::Panel::None,
                song::Panel::Step,
                song::Panel::None,
                song::Panel::Step
            ]
        );
    }

    #[test]
    fn panels_left_into_generic() {
        assert_eq!(
            <[song::Panel; 4]>::from(Panels::Left),
            [
                song::Panel::Step,
                song::Panel::None,
                song::Panel::None,
                song::Panel::None,
            ]
        );
    }

    #[test]
    fn panels_right_into_generic() {
        assert_eq!(
            <[song::Panel; 4]>::from(Panels::Right),
            [
                song::Panel::None,
                song::Panel::None,
                song::Panel::None,
                song::Panel::Step
            ]
        );
    }

    #[test]
    fn panels_up_left_into_generic() {
        assert_eq!(
            <[song::Panel; 4]>::from(Panels::UpLeft),
            [
                song::Panel::Step,
                song::Panel::None,
                song::Panel::Step,
                song::Panel::None
            ]
        );
    }

    #[test]
    fn panels_up_into_generic() {
        assert_eq!(
            <[song::Panel; 4]>::from(Panels::Up),
            [
                song::Panel::None,
                song::Panel::None,
                song::Panel::Step,
                song::Panel::None
            ]
        );
    }

    #[test]
    fn panels_up_right_into_generic() {
        assert_eq!(
            <[song::Panel; 4]>::from(Panels::UpRight),
            [
                song::Panel::None,
                song::Panel::None,
                song::Panel::Step,
                song::Panel::Step
            ]
        );
    }

    #[test]
    fn panels_up_down_into_generic() {
        assert_eq!(
            <[song::Panel; 4]>::from(Panels::UpDown),
            [
                song::Panel::None,
                song::Panel::Step,
                song::Panel::Step,
                song::Panel::None
            ]
        );
    }

    #[test]
    fn panels_left_right_into_generic() {
        assert_eq!(
            <[song::Panel; 4]>::from(Panels::LeftRight),
            [
                song::Panel::Step,
                song::Panel::None,
                song::Panel::None,
                song::Panel::Step
            ]
        );
    }

    #[test]
    fn panels_none_try_from_generic() {
        assert_ok_eq!(
            Panels::try_from([
                song::Panel::None,
                song::Panel::None,
                song::Panel::None,
                song::Panel::None
            ]),
            Panels::None,
        );
    }

    #[test]
    fn panels_down_left_try_from_generic() {
        assert_ok_eq!(
            Panels::try_from([
                song::Panel::Step,
                song::Panel::Step,
                song::Panel::None,
                song::Panel::None
            ]),
            Panels::DownLeft,
        );
    }

    #[test]
    fn panels_down_try_from_generic() {
        assert_ok_eq!(
            Panels::try_from([
                song::Panel::None,
                song::Panel::Step,
                song::Panel::None,
                song::Panel::None
            ]),
            Panels::Down,
        );
    }

    #[test]
    fn panels_down_right_try_from_generic() {
        assert_ok_eq!(
            Panels::try_from([
                song::Panel::None,
                song::Panel::Step,
                song::Panel::None,
                song::Panel::Step
            ]),
            Panels::DownRight,
        );
    }

    #[test]
    fn panels_left_try_from_generic() {
        assert_ok_eq!(
            Panels::try_from([
                song::Panel::Step,
                song::Panel::None,
                song::Panel::None,
                song::Panel::None
            ]),
            Panels::Left,
        );
    }

    #[test]
    fn panels_right_try_from_generic() {
        assert_ok_eq!(
            Panels::try_from([
                song::Panel::None,
                song::Panel::None,
                song::Panel::None,
                song::Panel::Step
            ]),
            Panels::Right,
        );
    }

    #[test]
    fn panels_up_left_try_from_generic() {
        assert_ok_eq!(
            Panels::try_from([
                song::Panel::Step,
                song::Panel::None,
                song::Panel::Step,
                song::Panel::None
            ]),
            Panels::UpLeft,
        );
    }

    #[test]
    fn panels_up_try_from_generic() {
        assert_ok_eq!(
            Panels::try_from([
                song::Panel::None,
                song::Panel::None,
                song::Panel::Step,
                song::Panel::None
            ]),
            Panels::Up,
        );
    }

    #[test]
    fn panels_up_right_try_from_generic() {
        assert_ok_eq!(
            Panels::try_from([
                song::Panel::None,
                song::Panel::None,
                song::Panel::Step,
                song::Panel::Step
            ]),
            Panels::UpRight,
        );
    }

    #[test]
    fn panels_up_down_try_from_generic() {
        assert_ok_eq!(
            Panels::try_from([
                song::Panel::None,
                song::Panel::Step,
                song::Panel::Step,
                song::Panel::None
            ]),
            Panels::UpDown,
        );
    }

    #[test]
    fn panels_left_right_try_from_generic() {
        assert_ok_eq!(
            Panels::try_from([
                song::Panel::Step,
                song::Panel::None,
                song::Panel::None,
                song::Panel::Step
            ]),
            Panels::LeftRight,
        );
    }

    #[test]
    fn panels_unsupported_try_from_generic() {
        assert_err_eq!(
            Panels::try_from([
                song::Panel::Step,
                song::Panel::Step,
                song::Panel::Step,
                song::Panel::Step
            ]),
            Error::UnsupportedPanelCombination,
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_eighth() {
        assert_eq!(Duration::Eighth.serialization_capacity_requirement(None), 1);
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixteenth() {
        assert_eq!(Duration::Sixteenth.serialization_capacity_requirement(None), 2);
    }

    #[test]
    fn duration_serialization_capacity_requirement_twentyfourth() {
        assert_eq!(Duration::TwentyFourth.serialization_capacity_requirement(None), 2);
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixtyfourth() {
        assert_eq!(Duration::SixtyFourth.serialization_capacity_requirement(None), 2);
    }

    #[test]
    fn duration_serialization_capacity_requirement_eighth_after_eighth() {
        assert_eq!(Duration::Eighth.serialization_capacity_requirement(Some(Duration::Eighth)), 1);
    }

    #[test]
    fn duration_serialization_capacity_requirement_eighth_after_sixteenth() {
        assert_eq!(Duration::Eighth.serialization_capacity_requirement(Some(Duration::Sixteenth)), 2);
    }

    #[test]
    fn duration_serialization_capacity_requirement_eighth_after_twentyfourth() {
        assert_eq!(Duration::Eighth.serialization_capacity_requirement(Some(Duration::TwentyFourth)), 2);
    }

    #[test]
    fn duration_serialization_capacity_requirement_eighth_after_sixtyfourth() {
        assert_eq!(Duration::Eighth.serialization_capacity_requirement(Some(Duration::SixtyFourth)), 2);
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixteenth_after_eighth() {
        assert_eq!(Duration::Sixteenth.serialization_capacity_requirement(Some(Duration::Eighth)), 2);
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixteenth_after_sixteenth() {
        assert_eq!(Duration::Sixteenth.serialization_capacity_requirement(Some(Duration::Sixteenth)), 1);
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixteenth_after_twentyfourth() {
        assert_eq!(Duration::Sixteenth.serialization_capacity_requirement(Some(Duration::TwentyFourth)), 3);
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixteenth_after_sixtyfourth() {
        assert_eq!(Duration::Sixteenth.serialization_capacity_requirement(Some(Duration::SixtyFourth)), 3);
    }

    #[test]
    fn duration_serialization_capacity_requirement_twentyfourth_after_eighth() {
        assert_eq!(Duration::TwentyFourth.serialization_capacity_requirement(Some(Duration::Eighth)), 2);
    }

    #[test]
    fn duration_serialization_capacity_requirement_twentyfourth_after_sixteenth() {
        assert_eq!(Duration::TwentyFourth.serialization_capacity_requirement(Some(Duration::Sixteenth)), 3);
    }

    #[test]
    fn duration_serialization_capacity_requirement_twentyfourth_after_twentyfourth() {
        assert_eq!(Duration::TwentyFourth.serialization_capacity_requirement(Some(Duration::TwentyFourth)), 1);
    }

    #[test]
    fn duration_serialization_capacity_requirement_twentyfourth_after_sixtyfourth() {
        assert_eq!(Duration::TwentyFourth.serialization_capacity_requirement(Some(Duration::SixtyFourth)), 3);
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixtyfourth_after_eighth() {
        assert_eq!(Duration::SixtyFourth.serialization_capacity_requirement(Some(Duration::Eighth)), 2);
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixtyfourth_after_sixteenth() {
        assert_eq!(Duration::SixtyFourth.serialization_capacity_requirement(Some(Duration::Sixteenth)), 3);
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixtyfourth_after_twentyfourth() {
        assert_eq!(Duration::SixtyFourth.serialization_capacity_requirement(Some(Duration::TwentyFourth)), 3);
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixtyfourth_after_sixtyfourth() {
        assert_eq!(Duration::SixtyFourth.serialization_capacity_requirement(Some(Duration::SixtyFourth)), 1);
    }

    #[test]
    fn duration_as_isize_eighth() {
        assert_eq!(Duration::Eighth.as_isize(), 24);
    }

    #[test]
    fn duration_as_isize_sixteenth() {
        assert_eq!(Duration::Sixteenth.as_isize(), 12);
    }

    #[test]
    fn duration_as_isize_twentyfourth() {
        assert_eq!(Duration::TwentyFourth.as_isize(), 8);
    }

    #[test]
    fn duration_as_isize_sixtyfourth() {
        assert_eq!(Duration::SixtyFourth.as_isize(), 3);
    }

    #[test]
    fn duration_compare_eighth_eighth() {
        assert_eq!(Duration::Eighth, Duration::Eighth);
    }

    #[test]
    fn duration_compare_eighth_sixteenth() {
        assert_gt!(Duration::Eighth, Duration::Sixteenth);
    }

    #[test]
    fn duration_compare_eighth_twentyfourth() {
        assert_gt!(Duration::Eighth, Duration::TwentyFourth);
    }

    #[test]
    fn duration_compare_eighth_sixtyfourth() {
        assert_gt!(Duration::Eighth, Duration::SixtyFourth);
    }

    #[test]
    fn duration_compare_sixteenth_sixteenth() {
        assert_eq!(Duration::Sixteenth, Duration::Sixteenth);
    }

    #[test]
    fn duration_compare_sixteenth_twentyfourth() {
        assert_gt!(Duration::Sixteenth, Duration::TwentyFourth);
    }

    #[test]
    fn duration_compare_sixteenth_sixtyfourth() {
        assert_gt!(Duration::Sixteenth, Duration::SixtyFourth);
    }

    #[test]
    fn duration_compare_twentyfourth_twentyfourth() {
        assert_eq!(Duration::TwentyFourth, Duration::TwentyFourth);
    }

    #[test]
    fn duration_compare_twentyfourth_sixtyfourth() {
        assert_gt!(Duration::TwentyFourth, Duration::SixtyFourth);
    }    

    #[test]
    fn duration_compare_sixtyfourth_sixtyfourth() {
        assert_eq!(Duration::SixtyFourth, Duration::SixtyFourth);
    }

    #[test]
    fn duration_eighth_into_generic() {
        assert_eq!(song::Duration::from(Duration::Eighth), song::Duration::Eighth);
    }

    #[test]
    fn duration_sixteenth_into_generic() {
        assert_eq!(song::Duration::from(Duration::Sixteenth), song::Duration::Sixteenth);
    }

    #[test]
    fn duration_twentyfourth_into_generic() {
        assert_eq!(song::Duration::from(Duration::TwentyFourth), song::Duration::TwentyFourth);
    }

    #[test]
    fn duration_sixtyfourth_into_generic() {
        assert_eq!(song::Duration::from(Duration::SixtyFourth), song::Duration::SixtyFourth);
    }

    #[test]
    fn duration_eighth_try_from_generic() {
        assert_ok_eq!(Duration::try_from(song::Duration::Eighth), Duration::Eighth);
    }

    #[test]
    fn duration_sixteenth_try_from_generic() {
        assert_ok_eq!(Duration::try_from(song::Duration::Sixteenth), Duration::Sixteenth);
    }

    #[test]
    fn duration_twentyfourth_try_from_generic() {
        assert_ok_eq!(Duration::try_from(song::Duration::TwentyFourth), Duration::TwentyFourth);
    }

    #[test]
    fn duration_sixtyfourth_try_from_generic() {
        assert_ok_eq!(Duration::try_from(song::Duration::SixtyFourth), Duration::SixtyFourth);
    }

    #[test]
    fn steps_ser_de_empty() {
        assert_tokens(&Steps { steps: Vec::new() }, &[Token::Bytes(b"")]);
    }

    #[test]
    fn steps_ser_de_eighths() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels::Up,
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels::LeftRight,
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels::None,
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels::DownRight,
                        duration: Duration::Eighth,
                    },
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
                            steps: vec![
                                Step {
                                    panels: Panels::Up,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::None,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::Down,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::Right,
                                    duration: Duration::Eighth,
                                },
                            ],
                        },
                    ),
                    (
                        Difficulty::Another,
                        7,
                        Steps {
                            steps: vec![
                                Step {
                                    panels: Panels::UpDown,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::UpLeft,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::None,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::Up,
                                    duration: Duration::Eighth,
                                },
                            ],
                        },
                    ),
                ],
                double: vec![(
                    Difficulty::Maniac,
                    9,
                    Steps {
                        steps: vec![
                            Step {
                                panels: Panels::Down,
                                duration: Duration::Eighth,
                            },
                            Step {
                                panels: Panels::Down,
                                duration: Duration::Eighth,
                            },
                            Step {
                                panels: Panels::Down,
                                duration: Duration::Eighth,
                            },
                            Step {
                                panels: Panels::Down,
                                duration: Duration::Eighth,
                            },
                        ],
                    },
                    Steps {
                        steps: vec![
                            Step {
                                panels: Panels::Up,
                                duration: Duration::Eighth,
                            },
                            Step {
                                panels: Panels::Down,
                                duration: Duration::Eighth,
                            },
                            Step {
                                panels: Panels::LeftRight,
                                duration: Duration::Eighth,
                            },
                            Step {
                                panels: Panels::DownLeft,
                                duration: Duration::Eighth,
                            },
                        ],
                    },
                )],
                couple: vec![
                    (
                        Difficulty::Basic,
                        2,
                        Steps {
                            steps: vec![
                                Step {
                                    panels: Panels::UpRight,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::Down,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::DownRight,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::None,
                                    duration: Duration::Eighth,
                                },
                            ],
                        },
                        Steps {
                            steps: vec![
                                Step {
                                    panels: Panels::Up,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::Up,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::UpDown,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::Up,
                                    duration: Duration::Eighth,
                                },
                            ],
                        },
                    ),
                    (
                        Difficulty::Another,
                        6,
                        Steps {
                            steps: vec![
                                Step {
                                    panels: Panels::None,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::None,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::None,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::None,
                                    duration: Duration::Eighth,
                                },
                            ],
                        },
                        Steps {
                            steps: vec![
                                Step {
                                    panels: Panels::Left,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::Up,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::Right,
                                    duration: Duration::Eighth,
                                },
                                Step {
                                    panels: Panels::Down,
                                    duration: Duration::Eighth,
                                },
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
            steps: vec![
                Step {
                    panels: Panels::Up,
                    duration: Duration::Eighth,
                },
                Step {
                    panels: Panels::Down,
                    duration: Duration::Sixteenth,
                },
                Step {
                    panels: Panels::Right,
                    duration: Duration::Eighth,
                },
                Step {
                    panels: Panels::Left,
                    duration: Duration::Sixteenth,
                },
            ],
        };
        let steps_1 = Steps {
            steps: vec![
                Step {
                    panels: Panels::Up,
                    duration: Duration::Sixteenth,
                },
                Step {
                    panels: Panels::Down,
                    duration: Duration::Sixteenth,
                },
                Step {
                    panels: Panels::Right,
                    duration: Duration::Eighth,
                },
                Step {
                    panels: Panels::UpDown,
                    duration: Duration::Sixteenth,
                },
                Step {
                    panels: Panels::Left,
                    duration: Duration::Sixteenth,
                },
            ],
        };

        assert_eq!(
            song::Steps::from((steps_0, steps_1)),
            song::Steps {
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
            }
        );
    }
}
