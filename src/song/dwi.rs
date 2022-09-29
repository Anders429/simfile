//! Types for representing valid `.dwi` files.
//!
//! Generic `Song` types can be converted to `.dwi` equivalents, and vice-versa. These `.dwi` types
//! can be serialized to and deserialized from `.dwi` files directly.

use crate::song;
use arrayvec::ArrayVec;
use serde::{
    de,
    de::{Unexpected, Visitor},
    Deserialize, Deserializer, Serialize, Serializer,
};
use std::fmt;

#[derive(Debug)]
enum ConversionError {}

/// Valid states for an individual panel to be in.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Panel {
    /// No state.
    None,
    /// A step.
    Step,
    /// The beginning of a hold.
    ///
    /// This is considered both a step and the start of a hold.
    HoldStart,
    /// The end of a hold.
    ///
    /// Contrary to `Step`, `HoldEnd` cannot occur simultaneously with a `HoldStart`.
    HoldEnd,
}

impl From<Panel> for song::Panel {
    fn from(panel: Panel) -> Self {
        match panel {
            Panel::None => Self::None,
            Panel::Step => Self::Step,
            Panel::HoldStart => Self::HoldStart,
            Panel::HoldEnd => Self::HoldEnd,
        }
    }
}

impl TryFrom<song::Panel> for Panel {
    type Error = ConversionError;

    fn try_from(panel: song::Panel) -> Result<Self, Self::Error> {
        match panel {
            song::Panel::None => Ok(Self::None),
            song::Panel::Step => Ok(Self::Step),
            song::Panel::HoldStart => Ok(Self::HoldStart),
            song::Panel::HoldEnd => Ok(Self::HoldEnd),
        }
    }
}

/// A combination of panel states.
///
/// The generic const `PANELS` defines the game mode, and the correct number of panels will be
/// stored for each respective game mode.
#[derive(Debug, PartialEq)]
struct Panels<const PANELS: usize> {
    panels: [Panel; PANELS],
}

impl Panels<4> {
    /// Encodes this set of panels as an array of serialized bytes.
    ///
    /// This value will be a maximum of 9 bytes (for the case of a group of steps with a group of
    /// holds). However, it may be much less for common cases, with 1 byte being the smallest.
    ///
    /// Note that the grouping here is done by trivially deciding the byte combinations to
    /// represent step groups. This means that groups will likely not be preserved when doing a
    /// round-trip deserializing and re-serializing.
    fn as_serialized_bytes(&self) -> ArrayVec<u8, 9> {
        macro_rules! new_single_byte {
            ($byte:literal) => {{
                let mut result = ArrayVec::<u8, 9>::new();
                // SAFETY: The array has capacity for 9 bytes, so it can definitely hold this
                // one.
                unsafe { result.push_unchecked($byte) };
                result
            }};
        }

        /// SAFETY: Must be used to add exactly 4 bytes.
        macro_rules! new_four_byte {
            ($bytes:literal) => {{
                let mut result = ArrayVec::<u8, 9>::new();
                // SAFETY: The array has capacity for 9 bytes, so it can definitely hold these
                // 4.
                unsafe { result.try_extend_from_slice($bytes).unwrap_unchecked() };
                result
            }};
        }

        /// SAFETY: Only use this to add up to 5 bytes.
        macro_rules! extend_hold {
            ($result:ident, $bytes:literal) => {{
                // SAFETY: The array has capacity for 9 bytes, with up to 4 already
                // initialized, so it can definitely hold these 2 to 5.
                unsafe { $result.try_extend_from_slice($bytes).unwrap_unchecked() };
            }};
        }

        // Pattern alias.
        macro_rules! step {
            () => {
                Panel::Step | Panel::HoldStart | Panel::HoldEnd
            };
        }

        macro_rules! non_hold_step {
            () => {
                Panel::None | Panel::Step | Panel::HoldEnd
            };
        }

        let mut result = match self.panels {
            [Panel::None, Panel::None, Panel::None, Panel::None] => new_single_byte!(b'0'),
            [step!(), step!(), Panel::None, Panel::None] => {
                new_single_byte!(b'1')
            }
            [Panel::None, step!(), Panel::None, Panel::None] => {
                new_single_byte!(b'2')
            }
            [Panel::None, step!(), Panel::None, step!()] => {
                new_single_byte!(b'3')
            }
            [step!(), Panel::None, Panel::None, Panel::None] => {
                new_single_byte!(b'4')
            }
            [Panel::None, Panel::None, Panel::None, step!()] => {
                new_single_byte!(b'6')
            }
            [step!(), Panel::None, step!(), Panel::None] => {
                new_single_byte!(b'7')
            }
            [Panel::None, Panel::None, step!(), Panel::None] => {
                new_single_byte!(b'8')
            }
            [Panel::None, Panel::None, step!(), step!()] => {
                new_single_byte!(b'9')
            }
            [Panel::None, step!(), step!(), Panel::None] => {
                new_single_byte!(b'A')
            }
            [step!(), Panel::None, Panel::None, step!()] => {
                new_single_byte!(b'B')
            }
            [step!(), step!(), step!(), Panel::None] => {
                new_four_byte!(b"<18>")
            }
            [step!(), step!(), Panel::None, step!()] => {
                new_four_byte!(b"<16>")
            }
            [step!(), Panel::None, step!(), step!()] => {
                new_four_byte!(b"<76>")
            }
            [Panel::None, step!(), step!(), step!()] => {
                new_four_byte!(b"<A6>")
            }
            [step!(), step!(), step!(), step!()] => {
                new_four_byte!(b"<19>")
            }
        };

        match self.panels {
            [Panel::HoldStart, Panel::HoldStart, non_hold_step!(), non_hold_step!()] => {
                extend_hold!(result, b"!1")
            }
            [non_hold_step!(), Panel::HoldStart, non_hold_step!(), non_hold_step!()] => {
                extend_hold!(result, b"!2")
            }
            [non_hold_step!(), Panel::HoldStart, non_hold_step!(), Panel::HoldStart] => {
                extend_hold!(result, b"!3")
            }
            [Panel::HoldStart, non_hold_step!(), non_hold_step!(), non_hold_step!()] => {
                extend_hold!(result, b"!4")
            }
            [non_hold_step!(), non_hold_step!(), non_hold_step!(), Panel::HoldStart] => {
                extend_hold!(result, b"!6")
            }
            [Panel::HoldStart, non_hold_step!(), Panel::HoldStart, non_hold_step!()] => {
                extend_hold!(result, b"!7")
            }
            [non_hold_step!(), non_hold_step!(), Panel::HoldStart, non_hold_step!()] => {
                extend_hold!(result, b"!8")
            }
            [non_hold_step!(), non_hold_step!(), Panel::HoldStart, Panel::HoldStart] => {
                extend_hold!(result, b"!9")
            }
            [non_hold_step!(), Panel::HoldStart, Panel::HoldStart, non_hold_step!()] => {
                extend_hold!(result, b"!A")
            }
            [Panel::HoldStart, non_hold_step!(), non_hold_step!(), Panel::HoldStart] => {
                extend_hold!(result, b"!B")
            }
            [Panel::HoldStart, Panel::HoldStart, Panel::HoldStart, non_hold_step!()] => {
                extend_hold!(result, b"!<18>")
            }
            [Panel::HoldStart, Panel::HoldStart, non_hold_step!(), Panel::HoldStart] => {
                extend_hold!(result, b"!<16>")
            }
            [Panel::HoldStart, non_hold_step!(), Panel::HoldStart, Panel::HoldStart] => {
                extend_hold!(result, b"!<76>")
            }
            [non_hold_step!(), Panel::HoldStart, Panel::HoldStart, Panel::HoldStart] => {
                extend_hold!(result, b"!<A6>")
            }
            [Panel::HoldStart, Panel::HoldStart, Panel::HoldStart, Panel::HoldStart] => {
                extend_hold!(result, b"!<19>")
            }
            _ => {}
        }

        result
    }

    /// Deserializes the given byte as a step, respecting and updating the current holds.
    ///
    /// If a hold exists on a panel, then the hold is removed and a `HoldEnd` is placed in that
    /// panel. Otherwise, a `Step` is used.
    fn step_from_serialized_byte<E>(byte: u8, holds: &mut [bool; 4]) -> Result<Self, E>
    where
        E: de::Error,
    {
        /// Define a new step at the given index.
        ///
        /// # Safety
        /// `index` must be a valid index into `holds`.
        unsafe fn create_step(index: usize, holds: &mut [bool; 4]) -> Panel {
            let hold = unsafe { holds.get_unchecked_mut(index) };
            // SAFETY: This index is guaranteed to be valid.
            if *hold {
                *hold = false;
                Panel::HoldEnd
            } else {
                Panel::Step
            }
        }

        match byte {
            b'0' => Ok([Panel::None; 4]),
            b'1' => Ok([
                unsafe { create_step(0, holds) },
                unsafe { create_step(1, holds) },
                Panel::None,
                Panel::None,
            ]),
            b'2' => Ok([
                Panel::None,
                unsafe { create_step(1, holds) },
                Panel::None,
                Panel::None,
            ]),
            b'3' => Ok([
                Panel::None,
                unsafe { create_step(1, holds) },
                Panel::None,
                unsafe { create_step(3, holds) },
            ]),
            b'4' => Ok([
                unsafe { create_step(0, holds) },
                Panel::None,
                Panel::None,
                Panel::None,
            ]),
            b'6' => Ok([Panel::None, Panel::None, Panel::None, unsafe {
                create_step(3, holds)
            }]),
            b'7' => Ok([
                unsafe { create_step(0, holds) },
                Panel::None,
                unsafe { create_step(2, holds) },
                Panel::None,
            ]),
            b'8' => Ok([
                Panel::None,
                Panel::None,
                unsafe { create_step(2, holds) },
                Panel::None,
            ]),
            b'9' => Ok([
                Panel::None,
                Panel::None,
                unsafe { create_step(2, holds) },
                unsafe { create_step(3, holds) },
            ]),
            b'A' => Ok([
                Panel::None,
                unsafe { create_step(1, holds) },
                unsafe { create_step(2, holds) },
                Panel::None,
            ]),
            b'B' => Ok([
                unsafe { create_step(0, holds) },
                Panel::None,
                Panel::None,
                unsafe { create_step(3, holds) },
            ]),
            _ => Err(de::Error::invalid_value(
                Unexpected::Char(byte.into()),
                &"`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, or `B`",
            )),
        }
        .map(|panels| Panels { panels })
    }

    // Deserializes the given byte as a hold, respecting and updating the current holds.
    ///
    /// A `Hold` is placed on each panel specified by `byte`, and `holds` is also updated to
    /// reflect the new hold.
    fn hold_from_serialized_byte<E>(byte: u8, holds: &mut [bool; 4]) -> Result<Self, E>
    where
        E: de::Error,
    {
        /// Define a new hold at the given index.
        ///
        /// # Safety
        /// `index` must be a valid index into `holds`.
        unsafe fn create_hold(index: usize, holds: &mut [bool; 4]) -> Panel {
            *unsafe { holds.get_unchecked_mut(index) } = true;
            Panel::HoldStart
        }

        match byte {
            b'0' => Ok([Panel::None; 4]),
            b'1' => Ok([
                unsafe { create_hold(0, holds) },
                unsafe { create_hold(1, holds) },
                Panel::None,
                Panel::None,
            ]),
            b'2' => Ok([
                Panel::None,
                unsafe { create_hold(1, holds) },
                Panel::None,
                Panel::None,
            ]),
            b'3' => Ok([
                Panel::None,
                unsafe { create_hold(1, holds) },
                Panel::None,
                unsafe { create_hold(3, holds) },
            ]),
            b'4' => Ok([
                unsafe { create_hold(0, holds) },
                Panel::None,
                Panel::None,
                Panel::None,
            ]),
            b'6' => Ok([Panel::None, Panel::None, Panel::None, unsafe {
                create_hold(3, holds)
            }]),
            b'7' => Ok([
                unsafe { create_hold(0, holds) },
                Panel::None,
                unsafe { create_hold(2, holds) },
                Panel::None,
            ]),
            b'8' => Ok([
                Panel::None,
                Panel::None,
                unsafe { create_hold(2, holds) },
                Panel::None,
            ]),
            b'9' => Ok([
                Panel::None,
                Panel::None,
                unsafe { create_hold(2, holds) },
                unsafe { create_hold(3, holds) },
            ]),
            b'A' => Ok([
                Panel::None,
                unsafe { create_hold(1, holds) },
                unsafe { create_hold(2, holds) },
                Panel::None,
            ]),
            b'B' => Ok([
                unsafe { create_hold(0, holds) },
                Panel::None,
                Panel::None,
                unsafe { create_hold(3, holds) },
            ]),
            _ => Err(de::Error::invalid_value(
                Unexpected::Char(byte.into()),
                &"`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, or `B`",
            )),
        }
        .map(|panels| Panels { panels })
    }

    /// Merges another `Panels` into this one.
    ///
    /// The new `Panel` values will override the current ones. Note that it is incorrect to
    /// override a `Panel::HoldEnd` with a `Panel::HoldStart`; this will result in an error.
    fn merge_in_place<E>(&mut self, other: Self) -> Result<(), E>
    where
        E: de::Error,
    {
        for index in 0..4 {
            // SAFETY: `index` will always be less than 4, and therefore within the bounds of
            // `other.panels`.
            let panel = unsafe { self.panels.get_unchecked_mut(index) };
            match unsafe { other.panels.get_unchecked(index) } {
                &other_panel @ (Panel::Step | Panel::HoldEnd) => *panel = other_panel,
                &other_panel @ Panel::HoldStart => {
                    if matches!(panel, Panel::HoldEnd) {
                        return Err(de::Error::invalid_value(
                            Unexpected::Other("`HoldStart` on same beat as `HoldEnd`"),
                            &"`HoldStart` to happen on a different beat than `HoldEnd`",
                        ));
                    } else {
                        *panel = other_panel;
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }
}

/// All valid DWI durations between steps.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Duration {
    Eighth,
    Sixteenth,
    TwentyFourth,
    SixtyFourth,
    OneHundredNinetySecond,
}

impl Duration {
    /// Returns the capacity requirement for a byte serialized with this duration.
    fn serialization_capacity_requirement(&self, previous: Option<Self>) -> usize {
        match self {
            Duration::Eighth => {
                1 + matches!(
                    previous,
                    Some(Duration::Sixteenth)
                        | Some(Duration::TwentyFourth)
                        | Some(Duration::SixtyFourth)
                        | Some(Duration::OneHundredNinetySecond)
                ) as usize
            }
            Duration::Sixteenth => {
                1 + !matches!(previous, Some(Duration::Sixteenth)) as usize
                    + matches!(
                        previous,
                        Some(Duration::TwentyFourth)
                            | Some(Duration::SixtyFourth)
                            | Some(Duration::OneHundredNinetySecond)
                    ) as usize
            }
            Duration::TwentyFourth => {
                1 + !matches!(previous, Some(Duration::TwentyFourth)) as usize
                    + matches!(
                        previous,
                        Some(Duration::Sixteenth)
                            | Some(Duration::SixtyFourth)
                            | Some(Duration::OneHundredNinetySecond)
                    ) as usize
            }
            Duration::SixtyFourth => {
                1 + !matches!(previous, Some(Duration::SixtyFourth)) as usize
                    + matches!(
                        previous,
                        Some(Duration::Sixteenth)
                            | Some(Duration::TwentyFourth)
                            | Some(Duration::OneHundredNinetySecond)
                    ) as usize
            }
            Duration::OneHundredNinetySecond => {
                1 + !matches!(previous, Some(Duration::OneHundredNinetySecond)) as usize
                    + matches!(
                        previous,
                        Some(Duration::Sixteenth)
                            | Some(Duration::TwentyFourth)
                            | Some(Duration::SixtyFourth)
                    ) as usize
            }
        }
    }
}

impl From<Duration> for song::Duration {
    fn from(duration: Duration) -> Self {
        match duration {
            Duration::Eighth => Self::Eighth,
            Duration::Sixteenth => Self::Sixteenth,
            Duration::TwentyFourth => Self::TwentyFourth,
            Duration::SixtyFourth => Self::SixtyFourth,
            Duration::OneHundredNinetySecond => Self::OneHundredNinetySecond,
        }
    }
}

impl TryFrom<song::Duration> for Duration {
    type Error = ConversionError;

    fn try_from(duration: song::Duration) -> Result<Self, Self::Error> {
        match duration {
            song::Duration::Eighth => Ok(Self::Eighth),
            song::Duration::Sixteenth => Ok(Self::Sixteenth),
            song::Duration::TwentyFourth => Ok(Self::TwentyFourth),
            song::Duration::SixtyFourth => Ok(Self::SixtyFourth),
            song::Duration::OneHundredNinetySecond => Ok(Self::OneHundredNinetySecond),
        }
    }
}

/// A single step.
///
/// A step contains both the panel information as well as the duration until the next step.
#[derive(Debug, PartialEq)]
struct Step<const PANELS: usize> {
    panels: Panels<PANELS>,
    duration: Duration,
}

impl Step<4> {
    /// Serializes the current step, respecting the previously-serialized duration.
    fn serialize_to_bytes(&self, bytes: &mut Vec<u8>, previous: Option<Duration>) {
        match self.duration {
            Duration::Eighth => {
                if matches!(previous, Some(Duration::Sixteenth)) {
                    bytes.push(b')');
                } else if matches!(previous, Some(Duration::TwentyFourth)) {
                    bytes.push(b']');
                } else if matches!(previous, Some(Duration::SixtyFourth)) {
                    bytes.push(b'}');
                } else if matches!(previous, Some(Duration::OneHundredNinetySecond)) {
                    bytes.push(b'\'');
                }
                bytes.extend(self.panels.as_serialized_bytes());
            }
            Duration::Sixteenth => {
                if matches!(previous, Some(Duration::TwentyFourth)) {
                    bytes.push(b']');
                } else if matches!(previous, Some(Duration::SixtyFourth)) {
                    bytes.push(b'}');
                } else if matches!(previous, Some(Duration::OneHundredNinetySecond)) {
                    bytes.push(b'\'');
                }
                if !matches!(previous, Some(Duration::Sixteenth)) {
                    bytes.push(b'(');
                }
                bytes.extend(self.panels.as_serialized_bytes());
            }
            Duration::TwentyFourth => {
                if matches!(previous, Some(Duration::Sixteenth)) {
                    bytes.push(b')');
                } else if matches!(previous, Some(Duration::SixtyFourth)) {
                    bytes.push(b'}');
                } else if matches!(previous, Some(Duration::OneHundredNinetySecond)) {
                    bytes.push(b'\'');
                }
                if !matches!(previous, Some(Duration::TwentyFourth)) {
                    bytes.push(b'[');
                }
                bytes.extend(self.panels.as_serialized_bytes());
            }
            Duration::SixtyFourth => {
                if matches!(previous, Some(Duration::Sixteenth)) {
                    bytes.push(b')');
                } else if matches!(previous, Some(Duration::TwentyFourth)) {
                    bytes.push(b']');
                } else if matches!(previous, Some(Duration::OneHundredNinetySecond)) {
                    bytes.push(b'\'');
                }
                if !matches!(previous, Some(Duration::SixtyFourth)) {
                    bytes.push(b'{');
                }
                bytes.extend(self.panels.as_serialized_bytes());
            }
            Duration::OneHundredNinetySecond => {
                if matches!(previous, Some(Duration::Sixteenth)) {
                    bytes.push(b')');
                } else if matches!(previous, Some(Duration::TwentyFourth)) {
                    bytes.push(b']');
                } else if matches!(previous, Some(Duration::SixtyFourth)) {
                    bytes.push(b'}');
                }
                if !matches!(previous, Some(Duration::OneHundredNinetySecond)) {
                    bytes.push(b'`');
                }
                bytes.extend(self.panels.as_serialized_bytes());
            }
        }
    }
}

/// Multiple sequential steps for the entire song.
///
/// These are steps for a single style and difficulty. Additional timing information is not
/// included here, but is included in the header instead.
#[derive(Debug, PartialEq)]
struct Steps<const PANELS: usize> {
    steps: Vec<Step<PANELS>>,
}

impl Serialize for Steps<4> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut steps = Vec::with_capacity({
            let mut previous = None;
            let mut sum = 0;
            for step in &self.steps {
                sum += step.duration.serialization_capacity_requirement(previous);
                previous = Some(step.duration);
            }
            sum
        });
        let mut previous = None;
        for step in &self.steps {
            step.serialize_to_bytes(&mut steps, previous);
            previous = Some(step.duration);
        }
        match previous {
            Some(Duration::Sixteenth) => steps.push(b')'),
            Some(Duration::TwentyFourth) => steps.push(b']'),
            Some(Duration::SixtyFourth) => steps.push(b'}'),
            Some(Duration::OneHundredNinetySecond) => steps.push(b'\''),
            _ => {}
        }
        serializer.serialize_bytes(&steps)
    }
}

impl<'de> Deserialize<'de> for Steps<4> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct StepsVisitor;

        impl<'de> Visitor<'de> for StepsVisitor {
            type Value = Steps<4>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("dance steps")
            }

            fn visit_bytes<E>(self, bytes: &[u8]) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                enum State {
                    Steps,
                    StepsGroup,
                    Holds,
                    HoldsGroup,
                }

                // TODO: Make this capacity approximation more intelligent.
                let mut steps = Vec::with_capacity(bytes.len());
                let mut panels = None;
                let mut duration = Duration::Eighth;
                let mut state = State::Steps;
                let mut holds = [false; 4];
                for &byte in bytes.iter().filter(|b| !b.is_ascii_whitespace()) {
                    match state {
                        State::Steps => match byte {
                            b'(' => {
                                if !matches!(duration, Duration::Eighth) {
                                    return Err(de::Error::invalid_value(
                                            Unexpected::Other("incorrectly nested `(`"),
                                            &"`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`",
                                        ));
                                }
                                if let Some(panels) = panels {
                                    steps.push(Step { panels, duration });
                                }
                                panels = None;
                                duration = Duration::Sixteenth;
                            }
                            b'[' => {
                                if !matches!(duration, Duration::Eighth) {
                                    return Err(de::Error::invalid_value(
                                            Unexpected::Other("incorrectly nested `[`"),
                                            &"`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`",
                                        ));
                                }
                                if let Some(panels) = panels {
                                    steps.push(Step { panels, duration });
                                }
                                panels = None;
                                duration = Duration::TwentyFourth;
                            }
                            b'{' => {
                                if !matches!(duration, Duration::Eighth) {
                                    return Err(de::Error::invalid_value(
                                            Unexpected::Other("incorrectly nested `{`"),
                                            &"`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`",
                                        ));
                                }
                                if let Some(panels) = panels {
                                    steps.push(Step { panels, duration });
                                }
                                panels = None;
                                duration = Duration::SixtyFourth;
                            }
                            b'`' => {
                                if !matches!(duration, Duration::Eighth) {
                                    return Err(de::Error::invalid_value(
                                            Unexpected::Other("incorrectly nested ```"),
                                            &"`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`",
                                        ));
                                }
                                if let Some(panels) = panels {
                                    steps.push(Step { panels, duration });
                                }
                                panels = None;
                                duration = Duration::OneHundredNinetySecond;
                            }
                            b')' => {
                                if !matches!(duration, Duration::Sixteenth) {
                                    return Err(de::Error::invalid_value(
                                            Unexpected::Other("mismatched `)`"),
                                            &match duration {
                                                Duration::Eighth => "`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`",
                                                Duration::TwentyFourth => "`]`",
                                                Duration::SixtyFourth => "`}`",
                                                Duration::OneHundredNinetySecond => "`'`",
                                                _ => unreachable!(),
                                            },
                                        ));
                                }
                                if let Some(panels) = panels {
                                    steps.push(Step { panels, duration });
                                }
                                panels = None;
                                duration = Duration::Eighth;
                            }
                            b']' => {
                                if !matches!(duration, Duration::TwentyFourth) {
                                    return Err(de::Error::invalid_value(
                                            Unexpected::Other("mismatched `]`"),
                                            &match duration {
                                                Duration::Eighth => "`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`",
                                                Duration::Sixteenth => "`)`",
                                                Duration::SixtyFourth => "`}`",
                                                Duration::OneHundredNinetySecond => "`'`",
                                                _ => unreachable!(),
                                            },
                                        ));
                                }
                                if let Some(panels) = panels {
                                    steps.push(Step { panels, duration });
                                }
                                panels = None;
                                duration = Duration::Eighth;
                            }
                            b'}' => {
                                if !matches!(duration, Duration::SixtyFourth) {
                                    return Err(de::Error::invalid_value(
                                            Unexpected::Other("mismatched `}`"),
                                            &match duration {
                                                Duration::Eighth => "`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`",
                                                Duration::Sixteenth => "`)`",
                                                Duration::TwentyFourth => "`]`",
                                                Duration::OneHundredNinetySecond => "`'`",
                                                _ => unreachable!(),
                                            },
                                        ));
                                }
                                if let Some(panels) = panels {
                                    steps.push(Step { panels, duration });
                                }
                                panels = None;
                                duration = Duration::Eighth;
                            }
                            b'\'' => {
                                if !matches!(duration, Duration::OneHundredNinetySecond) {
                                    return Err(de::Error::invalid_value(
                                            Unexpected::Other("mismatched `'`"),
                                            &match duration {
                                                Duration::Eighth => "`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`",
                                                Duration::Sixteenth => "`)`",
                                                Duration::TwentyFourth => "`]`",
                                                Duration::SixtyFourth => "`}`",
                                                _ => unreachable!(),
                                            },
                                        ));
                                }
                                if let Some(panels) = panels {
                                    steps.push(Step { panels, duration });
                                }
                                panels = None;
                                duration = Duration::Eighth;
                            }
                            b'<' => {
                                state = State::StepsGroup;
                                if let Some(panels) = panels {
                                    steps.push(Step { panels, duration });
                                }
                                panels = None;
                            }
                            b'!' => {
                                state = State::Holds;
                            }
                            _ => {
                                if let Some(panels) = panels {
                                    steps.push(Step { panels, duration });
                                }
                                panels = Some(Panels::step_from_serialized_byte(byte, &mut holds)?);
                            }
                        },
                        State::StepsGroup => {
                            if matches!(byte, b'>') {
                                state = State::Steps;
                                continue;
                            }
                            let new_panels = Panels::step_from_serialized_byte(byte, &mut holds)?;
                            if let Some(panels) = panels.as_mut() {
                                panels.merge_in_place(new_panels)?;
                            } else {
                                panels = Some(new_panels);
                            }
                        }
                        State::Holds => {
                            if matches!(byte, b'<') {
                                state = State::HoldsGroup;
                                continue;
                            }
                            if let Some(panels) = panels.as_mut() {
                                panels.merge_in_place(Panels::hold_from_serialized_byte(
                                    byte, &mut holds,
                                )?)?;
                            } else {
                                return Err(de::Error::invalid_value(Unexpected::Other("`!` with no preceding step"), &"`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>` preceding the `!`"));
                            }
                            state = State::Steps;
                        }
                        State::HoldsGroup => {
                            if matches!(byte, b'>') {
                                state = State::Steps;
                                continue;
                            }
                            let new_panels = Panels::hold_from_serialized_byte(byte, &mut holds)?;
                            if let Some(panels) = panels.as_mut() {
                                panels.merge_in_place(new_panels)?;
                            } else {
                                return Err(de::Error::invalid_value(Unexpected::Other("`!` with no preceding step"), &"`0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>` preceding the `!`"));
                            }
                        }
                    }
                }
                if let Some(panels) = panels {
                    steps.push(Step { panels, duration });
                }
                Ok(Steps { steps })
            }
        }

        deserializer.deserialize_bytes(StepsVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use claim::assert_ok_eq;
    use serde_test::{
        assert_de_tokens, assert_de_tokens_error, assert_ser_tokens, assert_tokens, Token,
    };

    #[test]
    fn panel_into_generic_panel_none() {
        assert_eq!(song::Panel::from(Panel::None), song::Panel::None);
    }

    #[test]
    fn panel_into_generic_panel_step() {
        assert_eq!(song::Panel::from(Panel::Step), song::Panel::Step);
    }

    #[test]
    fn panel_into_generic_panel_hold_start() {
        assert_eq!(song::Panel::from(Panel::HoldStart), song::Panel::HoldStart);
    }

    #[test]
    fn panel_into_generic_panel_hold_end() {
        assert_eq!(song::Panel::from(Panel::HoldEnd), song::Panel::HoldEnd);
    }

    #[test]
    fn panel_try_from_generic_panel_none() {
        assert_ok_eq!(Panel::try_from(song::Panel::None), Panel::None);
    }

    #[test]
    fn panel_try_from_generic_panel_step() {
        assert_ok_eq!(Panel::try_from(song::Panel::Step), Panel::Step);
    }

    #[test]
    fn panel_try_from_generic_panel_hold_start() {
        assert_ok_eq!(Panel::try_from(song::Panel::HoldStart), Panel::HoldStart);
    }

    #[test]
    fn panel_try_from_generic_panel_hold_end() {
        assert_ok_eq!(Panel::try_from(song::Panel::HoldEnd), Panel::HoldEnd);
    }

    #[test]
    fn four_panels_as_serialized_bytes_single() {
        assert_eq!(
            Panels {
                panels: [Panel::Step, Panel::None, Panel::None, Panel::None]
            }
            .as_serialized_bytes(),
            ArrayVec::try_from(b"4".as_slice()).unwrap()
        );
    }

    #[test]
    fn four_panels_as_serialized_bytes_single_hold_start() {
        assert_eq!(
            Panels {
                panels: [Panel::None, Panel::HoldStart, Panel::None, Panel::None]
            }
            .as_serialized_bytes(),
            ArrayVec::try_from(b"2!2".as_slice()).unwrap()
        );
    }

    #[test]
    fn four_panels_as_serialized_bytes_single_hold_end() {
        assert_eq!(
            Panels {
                panels: [Panel::None, Panel::None, Panel::HoldEnd, Panel::None]
            }
            .as_serialized_bytes(),
            ArrayVec::try_from(b"8".as_slice()).unwrap()
        );
    }

    #[test]
    fn four_panels_as_serialized_bytes_multi_panel() {
        assert_eq!(
            Panels {
                panels: [Panel::Step, Panel::Step, Panel::Step, Panel::Step]
            }
            .as_serialized_bytes(),
            ArrayVec::try_from(b"<19>".as_slice()).unwrap()
        );
    }

    #[test]
    fn four_panels_as_serialized_bytes_step_and_hold_start() {
        assert_eq!(
            Panels {
                panels: [Panel::HoldStart, Panel::None, Panel::None, Panel::Step]
            }
            .as_serialized_bytes(),
            ArrayVec::try_from(b"B!4".as_slice()).unwrap()
        );
    }

    #[test]
    fn four_panels_as_serialized_bytes_step_and_hold_end() {
        assert_eq!(
            Panels {
                panels: [Panel::None, Panel::HoldEnd, Panel::Step, Panel::None]
            }
            .as_serialized_bytes(),
            ArrayVec::try_from(b"A".as_slice()).unwrap()
        );
    }

    #[test]
    fn four_panels_as_serialized_bytes_all_different_types() {
        assert_eq!(
            Panels {
                panels: [Panel::Step, Panel::HoldEnd, Panel::None, Panel::HoldStart]
            }
            .as_serialized_bytes(),
            ArrayVec::try_from(b"<16>!6".as_slice()).unwrap()
        );
    }

    #[test]
    fn four_panels_as_serialized_bytes_all_hold() {
        assert_eq!(
            Panels {
                panels: [
                    Panel::HoldStart,
                    Panel::HoldStart,
                    Panel::HoldStart,
                    Panel::HoldStart
                ]
            }
            .as_serialized_bytes(),
            ArrayVec::try_from(b"<19>!<19>".as_slice()).unwrap()
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_eighth() {
        assert_eq!(Duration::Eighth.serialization_capacity_requirement(None), 1);
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixteenth() {
        assert_eq!(
            Duration::Sixteenth.serialization_capacity_requirement(None),
            2
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_twentyfourth() {
        assert_eq!(
            Duration::TwentyFourth.serialization_capacity_requirement(None),
            2
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixtyfourth() {
        assert_eq!(
            Duration::SixtyFourth.serialization_capacity_requirement(None),
            2
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_one_hundred_ninety_second() {
        assert_eq!(
            Duration::OneHundredNinetySecond.serialization_capacity_requirement(None),
            2
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_eighth_after_eighth() {
        assert_eq!(
            Duration::Eighth.serialization_capacity_requirement(Some(Duration::Eighth)),
            1
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_eighth_after_sixteenth() {
        assert_eq!(
            Duration::Eighth.serialization_capacity_requirement(Some(Duration::Sixteenth)),
            2
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_eighth_after_twentyfourth() {
        assert_eq!(
            Duration::Eighth.serialization_capacity_requirement(Some(Duration::TwentyFourth)),
            2
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_eighth_after_sixtyfourth() {
        assert_eq!(
            Duration::Eighth.serialization_capacity_requirement(Some(Duration::SixtyFourth)),
            2
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_eighth_after_one_hundred_ninety_second() {
        assert_eq!(
            Duration::Eighth
                .serialization_capacity_requirement(Some(Duration::OneHundredNinetySecond)),
            2
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixteenth_after_eighth() {
        assert_eq!(
            Duration::Sixteenth.serialization_capacity_requirement(Some(Duration::Eighth)),
            2
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixteenth_after_sixteenth() {
        assert_eq!(
            Duration::Sixteenth.serialization_capacity_requirement(Some(Duration::Sixteenth)),
            1
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixteenth_after_twentyfourth() {
        assert_eq!(
            Duration::Sixteenth.serialization_capacity_requirement(Some(Duration::TwentyFourth)),
            3
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixteenth_after_sixtyfourth() {
        assert_eq!(
            Duration::Sixteenth.serialization_capacity_requirement(Some(Duration::SixtyFourth)),
            3
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixteenth_after_one_hundred_ninety_second() {
        assert_eq!(
            Duration::Sixteenth
                .serialization_capacity_requirement(Some(Duration::OneHundredNinetySecond)),
            3
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_twentyfourth_after_eighth() {
        assert_eq!(
            Duration::TwentyFourth.serialization_capacity_requirement(Some(Duration::Eighth)),
            2
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_twentyfourth_after_sixteenth() {
        assert_eq!(
            Duration::TwentyFourth.serialization_capacity_requirement(Some(Duration::Sixteenth)),
            3
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_twentyfourth_after_twentyfourth() {
        assert_eq!(
            Duration::TwentyFourth.serialization_capacity_requirement(Some(Duration::TwentyFourth)),
            1
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_twentyfourth_after_sixtyfourth() {
        assert_eq!(
            Duration::TwentyFourth.serialization_capacity_requirement(Some(Duration::SixtyFourth)),
            3
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_twentyfourth_after_one_hundred_ninety_second() {
        assert_eq!(
            Duration::TwentyFourth
                .serialization_capacity_requirement(Some(Duration::OneHundredNinetySecond)),
            3
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixtyfourth_after_eighth() {
        assert_eq!(
            Duration::SixtyFourth.serialization_capacity_requirement(Some(Duration::Eighth)),
            2
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixtyfourth_after_sixteenth() {
        assert_eq!(
            Duration::SixtyFourth.serialization_capacity_requirement(Some(Duration::Sixteenth)),
            3
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixtyfourth_after_twentyfourth() {
        assert_eq!(
            Duration::SixtyFourth.serialization_capacity_requirement(Some(Duration::TwentyFourth)),
            3
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixtyfourth_after_sixtyfourth() {
        assert_eq!(
            Duration::SixtyFourth.serialization_capacity_requirement(Some(Duration::SixtyFourth)),
            1
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_sixtyfourth_after_one_hundred_ninety_second() {
        assert_eq!(
            Duration::SixtyFourth
                .serialization_capacity_requirement(Some(Duration::OneHundredNinetySecond)),
            3
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_one_hundred_ninety_second_after_eighth() {
        assert_eq!(
            Duration::OneHundredNinetySecond
                .serialization_capacity_requirement(Some(Duration::Eighth)),
            2
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_one_hundred_ninety_second_after_sixteenth() {
        assert_eq!(
            Duration::OneHundredNinetySecond
                .serialization_capacity_requirement(Some(Duration::Sixteenth)),
            3
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_one_hundred_ninety_second_after_twentyfourth() {
        assert_eq!(
            Duration::OneHundredNinetySecond
                .serialization_capacity_requirement(Some(Duration::TwentyFourth)),
            3
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_one_hundred_ninety_second_after_sixtyfourth() {
        assert_eq!(
            Duration::OneHundredNinetySecond
                .serialization_capacity_requirement(Some(Duration::SixtyFourth)),
            3
        );
    }

    #[test]
    fn duration_serialization_capacity_requirement_one_hundred_ninety_second_after_one_hundred_ninety_second(
    ) {
        assert_eq!(
            Duration::OneHundredNinetySecond
                .serialization_capacity_requirement(Some(Duration::OneHundredNinetySecond)),
            1
        );
    }

    #[test]
    fn duration_into_generic_duration_eighth() {
        assert_eq!(
            song::Duration::from(Duration::Eighth),
            song::Duration::Eighth
        );
    }

    #[test]
    fn duration_into_generic_duration_sixteenth() {
        assert_eq!(
            song::Duration::from(Duration::Sixteenth),
            song::Duration::Sixteenth
        );
    }

    #[test]
    fn duration_into_generic_duration_twentyfourth() {
        assert_eq!(
            song::Duration::from(Duration::TwentyFourth),
            song::Duration::TwentyFourth
        );
    }

    #[test]
    fn duration_into_generic_duration_sixtyfourth() {
        assert_eq!(
            song::Duration::from(Duration::SixtyFourth),
            song::Duration::SixtyFourth
        );
    }

    #[test]
    fn duration_into_generic_duration_one_hundred_ninety_second() {
        assert_eq!(
            song::Duration::from(Duration::OneHundredNinetySecond),
            song::Duration::OneHundredNinetySecond
        );
    }

    #[test]
    fn duration_try_from_generic_duration_eighth() {
        assert_ok_eq!(Duration::try_from(song::Duration::Eighth), Duration::Eighth);
    }

    #[test]
    fn duration_try_from_generic_duration_sixteenth() {
        assert_ok_eq!(
            Duration::try_from(song::Duration::Sixteenth),
            Duration::Sixteenth
        );
    }

    #[test]
    fn duration_try_from_generic_duration_twentyfourth() {
        assert_ok_eq!(
            Duration::try_from(song::Duration::TwentyFourth),
            Duration::TwentyFourth
        );
    }

    #[test]
    fn duration_try_from_generic_duration_sixtyfourth() {
        assert_ok_eq!(
            Duration::try_from(song::Duration::SixtyFourth),
            Duration::SixtyFourth
        );
    }

    #[test]
    fn duration_try_from_generic_duration_one_hundred_ninety_second() {
        assert_ok_eq!(
            Duration::try_from(song::Duration::OneHundredNinetySecond),
            Duration::OneHundredNinetySecond
        );
    }

    #[test]
    fn dance_step_serialize_to_bytes_eighth() {
        let mut result = Vec::new();

        Step {
            panels: Panels {
                panels: [Panel::Step, Panel::Step, Panel::None, Panel::None],
            },
            duration: Duration::Eighth,
        }
        .serialize_to_bytes(&mut result, None);

        assert_eq!(result, b"1");
    }

    #[test]
    fn dance_step_serialize_to_bytes_sixteenth() {
        let mut result = Vec::new();

        Step {
            panels: Panels {
                panels: [Panel::None, Panel::Step, Panel::None, Panel::None],
            },
            duration: Duration::Sixteenth,
        }
        .serialize_to_bytes(&mut result, None);

        assert_eq!(result, b"(2");
    }

    #[test]
    fn dance_step_serialize_to_bytes_twentyfourth() {
        let mut result = Vec::new();

        Step {
            panels: Panels {
                panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
            },
            duration: Duration::TwentyFourth,
        }
        .serialize_to_bytes(&mut result, None);

        assert_eq!(result, b"[3");
    }

    #[test]
    fn dance_step_serialize_to_bytes_sixtyfourth() {
        let mut result = Vec::new();

        Step {
            panels: Panels {
                panels: [Panel::Step, Panel::None, Panel::None, Panel::None],
            },
            duration: Duration::SixtyFourth,
        }
        .serialize_to_bytes(&mut result, None);

        assert_eq!(result, b"{4");
    }

    #[test]
    fn dance_step_serialize_to_bytes_one_hundred_ninety_second() {
        let mut result = Vec::new();

        Step {
            panels: Panels {
                panels: [Panel::None, Panel::None, Panel::None, Panel::Step],
            },
            duration: Duration::OneHundredNinetySecond,
        }
        .serialize_to_bytes(&mut result, None);

        assert_eq!(result, b"`6");
    }

    #[test]
    fn dance_step_serialize_to_bytes_after_eighth() {
        let mut result = Vec::new();

        Step {
            panels: Panels {
                panels: [Panel::Step, Panel::Step, Panel::None, Panel::None],
            },
            duration: Duration::Eighth,
        }
        .serialize_to_bytes(&mut result, Some(Duration::Eighth));

        assert_eq!(result, b"1");
    }

    #[test]
    fn dance_step_serialize_to_bytes_after_sixteenth() {
        let mut result = Vec::new();

        Step {
            panels: Panels {
                panels: [Panel::None, Panel::Step, Panel::None, Panel::None],
            },
            duration: Duration::Eighth,
        }
        .serialize_to_bytes(&mut result, Some(Duration::Sixteenth));

        assert_eq!(result, b")2");
    }

    #[test]
    fn dance_step_serialize_to_bytes_after_twentyfourth() {
        let mut result = Vec::new();

        Step {
            panels: Panels {
                panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
            },
            duration: Duration::Eighth,
        }
        .serialize_to_bytes(&mut result, Some(Duration::TwentyFourth));

        assert_eq!(result, b"]3");
    }

    #[test]
    fn dance_step_serialize_to_bytes_after_sixtyfourth() {
        let mut result = Vec::new();

        Step {
            panels: Panels {
                panels: [Panel::Step, Panel::None, Panel::None, Panel::None],
            },
            duration: Duration::Eighth,
        }
        .serialize_to_bytes(&mut result, Some(Duration::SixtyFourth));

        assert_eq!(result, b"}4");
    }

    #[test]
    fn dance_step_serialize_to_bytes_after_one_hundred_ninety_second() {
        let mut result = Vec::new();

        Step {
            panels: Panels {
                panels: [Panel::None, Panel::None, Panel::None, Panel::Step],
            },
            duration: Duration::Eighth,
        }
        .serialize_to_bytes(&mut result, Some(Duration::OneHundredNinetySecond));

        assert_eq!(result, b"'6");
    }

    #[test]
    fn dance_steps_ser_de_empty() {
        assert_tokens(&Steps { steps: Vec::new() }, &[Token::Bytes(b"")]);
    }

    #[test]
    fn dance_steps_ser_de_eighths() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::Step, Panel::None],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::None, Panel::Step],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::Eighth,
                    },
                ],
            },
            &[Token::Bytes(b"8B03")],
        );
    }

    #[test]
    fn dance_steps_ser_de_sixteenths() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::Step, Panel::None],
                        },
                        duration: Duration::Sixteenth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::None, Panel::Step],
                        },
                        duration: Duration::Sixteenth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::Sixteenth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::Sixteenth,
                    },
                ],
            },
            &[Token::Bytes(b"(8B03)")],
        );
    }

    #[test]
    fn dance_steps_ser_de_twentyfourths() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::Step, Panel::None],
                        },
                        duration: Duration::TwentyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::None, Panel::Step],
                        },
                        duration: Duration::TwentyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::TwentyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::TwentyFourth,
                    },
                ],
            },
            &[Token::Bytes(b"[8B03]")],
        );
    }

    #[test]
    fn dance_steps_ser_de_sixtyfourths() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::Step, Panel::None],
                        },
                        duration: Duration::SixtyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::None, Panel::Step],
                        },
                        duration: Duration::SixtyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::SixtyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::SixtyFourth,
                    },
                ],
            },
            &[Token::Bytes(b"{8B03}")],
        );
    }

    #[test]
    fn dance_steps_ser_de_one_hundred_ninety_seconds() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::Step, Panel::None],
                        },
                        duration: Duration::OneHundredNinetySecond,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::None, Panel::Step],
                        },
                        duration: Duration::OneHundredNinetySecond,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::OneHundredNinetySecond,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::OneHundredNinetySecond,
                    },
                ],
            },
            &[Token::Bytes(b"`8B03'")],
        );
    }

    #[test]
    fn dance_steps_ser_de_eighths_and_sixteenths() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::Step, Panel::None],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::None, Panel::Step],
                        },
                        duration: Duration::Sixteenth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::Sixteenth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::Eighth,
                    },
                ],
            },
            &[Token::Bytes(b"8(B0)3")],
        );
    }

    #[test]
    fn dance_steps_ser_de_eighths_and_twentyfourths() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::Step, Panel::None],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::None, Panel::Step],
                        },
                        duration: Duration::TwentyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::TwentyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::Eighth,
                    },
                ],
            },
            &[Token::Bytes(b"8[B0]3")],
        );
    }

    #[test]
    fn dance_steps_ser_de_eighths_and_sixtyfourths() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::Step, Panel::None],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::None, Panel::Step],
                        },
                        duration: Duration::SixtyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::SixtyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::Eighth,
                    },
                ],
            },
            &[Token::Bytes(b"8{B0}3")],
        );
    }

    #[test]
    fn dance_steps_ser_de_eighths_and_one_hundred_ninety_seconds() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::Step, Panel::None],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::None, Panel::Step],
                        },
                        duration: Duration::OneHundredNinetySecond,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::OneHundredNinetySecond,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::Eighth,
                    },
                ],
            },
            &[Token::Bytes(b"8`B0'3")],
        );
    }

    #[test]
    fn dance_steps_ser_de_sixteenths_and_twentyfourths() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::Step, Panel::None],
                        },
                        duration: Duration::Sixteenth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::None, Panel::Step],
                        },
                        duration: Duration::Sixteenth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::TwentyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::TwentyFourth,
                    },
                ],
            },
            &[Token::Bytes(b"(8B)[03]")],
        );
    }

    #[test]
    fn dance_steps_ser_de_twentyfourths_and_sixtyfourths() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::Step, Panel::None],
                        },
                        duration: Duration::TwentyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::None, Panel::Step],
                        },
                        duration: Duration::TwentyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::SixtyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::SixtyFourth,
                    },
                ],
            },
            &[Token::Bytes(b"[8B]{03}")],
        );
    }

    #[test]
    fn dance_steps_ser_de_sixtyfourths_and_one_hundred_ninety_seconds() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::Step, Panel::None],
                        },
                        duration: Duration::SixtyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::None, Panel::Step],
                        },
                        duration: Duration::SixtyFourth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::OneHundredNinetySecond,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::OneHundredNinetySecond,
                    },
                ],
            },
            &[Token::Bytes(b"{8B}`03'")],
        );
    }

    #[test]
    fn dance_steps_ser_groups() {
        assert_ser_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::Step, Panel::Step],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::Eighth,
                    },
                ],
            },
            &[Token::Bytes(b"<76>3")],
        );
    }

    #[test]
    fn dance_steps_de_groups() {
        assert_de_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::Step, Panel::Step],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::Step, Panel::None, Panel::Step],
                        },
                        duration: Duration::Eighth,
                    },
                ],
            },
            &[Token::Bytes(b"<8B><3>")],
        );
    }

    #[test]
    fn dance_steps_ser_de_holds() {
        assert_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::HoldStart, Panel::None],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::HoldStart, Panel::None, Panel::None, Panel::Step],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::HoldEnd, Panel::None, Panel::HoldEnd, Panel::Step],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [
                                Panel::HoldStart,
                                Panel::Step,
                                Panel::HoldStart,
                                Panel::HoldStart,
                            ],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::HoldEnd, Panel::Step, Panel::HoldEnd, Panel::HoldEnd],
                        },
                        duration: Duration::Eighth,
                    },
                ],
            },
            &[Token::Bytes(b"8!8B!40<76><19>!<76><19>")],
        );
    }

    #[test]
    fn dance_steps_de_holds_no_notes() {
        assert_de_tokens(
            &Steps {
                steps: vec![
                    Step {
                        panels: Panels {
                            panels: [Panel::None, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::Eighth,
                    },
                    Step {
                        panels: Panels {
                            panels: [Panel::Step, Panel::None, Panel::None, Panel::None],
                        },
                        duration: Duration::Eighth,
                    },
                ],
            },
            &[Token::Bytes(b"0!04!0")],
        );
    }

    #[test]
    fn dance_steps_de_invalid_hold_no_preceding_step() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"!2")],
            "invalid value: `!` with no preceding step, expected `0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>` preceding the `!`"
        );
    }

    #[test]
    fn dance_steps_de_invalid_hold_group_no_preceding_step() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"!<2A>")],
            "invalid value: `!` with no preceding step, expected `0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>` preceding the `!`"
        );
    }

    #[test]
    fn dance_steps_de_invalid_step_value() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"C")],
            "invalid value: character `C`, expected `0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, or `B`"
        );
    }

    #[test]
    fn dance_steps_de_invalid_hold_value() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"B!C")],
            "invalid value: character `C`, expected `0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, or `B`"
        );
    }

    #[test]
    fn dance_steps_de_nested_sixteenths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"((")],
            "invalid value: incorrectly nested `(`, expected `0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`"
        );
    }

    #[test]
    fn dance_steps_de_nested_twentyfourths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"[[")],
            "invalid value: incorrectly nested `[`, expected `0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`"
        );
    }

    #[test]
    fn dance_steps_de_nested_sixtyfourths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"{{")],
            "invalid value: incorrectly nested `{`, expected `0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`"
        );
    }

    #[test]
    fn dance_steps_de_nested_one_hundred_ninety_seconds() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"``")],
            "invalid value: incorrectly nested ```, expected `0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`"
        );
    }

    #[test]
    fn dance_steps_de_unopened_sixteenths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b")")],
            "invalid value: mismatched `)`, expected `0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`"
        );
    }

    #[test]
    fn dance_steps_de_mismatched_sixteenths_with_twentyfourths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"[)")],
            "invalid value: mismatched `)`, expected `]`",
        );
    }

    #[test]
    fn dance_steps_de_mismatched_sixteenths_with_sixtyfourths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"{)")],
            "invalid value: mismatched `)`, expected `}`",
        );
    }

    #[test]
    fn dance_steps_de_mismatched_sixteenths_with_one_hundred_ninety_seconds() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"`)")],
            "invalid value: mismatched `)`, expected `'`",
        );
    }

    #[test]
    fn dance_steps_de_unopened_twentyfourths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"]")],
            "invalid value: mismatched `]`, expected `0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`"
        );
    }

    #[test]
    fn dance_steps_de_mismatched_twentyfourths_with_sixteenths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"(]")],
            "invalid value: mismatched `]`, expected `)`",
        );
    }

    #[test]
    fn dance_steps_de_mismatched_twentyfourths_with_sixtyfourths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"{]")],
            "invalid value: mismatched `]`, expected `}`",
        );
    }

    #[test]
    fn dance_steps_de_mismatched_twentyfourths_with_one_hundred_ninety_seconds() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"`]")],
            "invalid value: mismatched `]`, expected `'`",
        );
    }

    #[test]
    fn dance_steps_de_unopened_sixtyfourths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"}")],
            "invalid value: mismatched `}`, expected `0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`"
        );
    }

    #[test]
    fn dance_steps_de_mismatched_sixtyfourths_with_sixteenths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"(}")],
            "invalid value: mismatched `}`, expected `)`",
        );
    }

    #[test]
    fn dance_steps_de_mismatched_sixtyfourths_with_twentyfourths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"[}")],
            "invalid value: mismatched `}`, expected `]`",
        );
    }

    #[test]
    fn dance_steps_de_mismatched_sixtyfourths_with_one_hundred_ninety_seconds() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"`}")],
            "invalid value: mismatched `}`, expected `'`",
        );
    }

    #[test]
    fn dance_steps_de_unopened_one_hundred_ninety_seconds() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"'")],
            "invalid value: mismatched `'`, expected `0`, `1`, `2`, `3`, `4`, `6`, `7`, `8`, `9`, `A`, `B`, or a group `<...>`"
        );
    }

    #[test]
    fn dance_steps_de_mismatched_one_hundred_ninety_seconds_with_sixteenths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"('")],
            "invalid value: mismatched `'`, expected `)`",
        );
    }

    #[test]
    fn dance_steps_de_mismatched_one_hundred_ninety_seconds_with_twentyfourths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"['")],
            "invalid value: mismatched `'`, expected `]`",
        );
    }

    #[test]
    fn dance_steps_de_mismatched_one_hundred_ninety_seconds_with_sixtyfourths() {
        assert_de_tokens_error::<Steps<4>>(
            &[Token::Bytes(b"{'")],
            "invalid value: mismatched `'`, expected `}`",
        );
    }
}
