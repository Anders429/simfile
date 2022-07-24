//! Types for representing valid `.dwi` files.
//!
//! Generic `Song` types can be converted to `.dwi` equivalents, and vice-versa. These `.dwi` types
//! can be serialized to and deserialized from `.dwi` files directly.

use crate::song;

#[derive(Debug)]
enum ConversionError {}

/// Valid states for an individual panel to be in.
#[derive(Debug, PartialEq)]
enum Panel {
    None,
    Step,
    HoldStart,
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

/// All valid DWI durations between steps.
enum Duration {
    Eighth,
    Sixteenth,
    TwentyFourth,
    SixtyFourth,
    OneHundredNinetySecond,
}

#[cfg(test)]
mod tests {
    use super::*;
    use claim::assert_ok_eq;

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
}
