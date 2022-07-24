//! Types for representing valid `.dwi` files.
//! 
//! Generic `Song` types can be converted to `.dwi` equivalents, and vice-versa. These `.dwi` types
//! can be serialized to and deserialized from `.dwi` files directly.

use crate::song;

/// Valid states for an individual panel to be in.
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

#[cfg(test)]
mod tests {
    use super::*;

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
}
