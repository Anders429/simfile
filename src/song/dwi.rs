//! Types for representing valid `.dwi` files.
//! 
//! Generic `Song` types can be converted to `.dwi` equivalents, and vice-versa. These `.dwi` types
//! can be serialized to and deserialized from `.dwi` files directly.

/// Valid states for an individual panel to be in.
enum Panel {
    None,
    Step,
    HoldStart,
    HoldEnd,
}
