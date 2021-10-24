//! Difficulties of songs and courses.

/// Difficulty levels.
pub enum Difficulty {
    /// Beginner difficulty.
    ///
    /// Also known as Novice.
    Beginner,
    /// Easy difficulty.
    ///
    /// Also known as Basic or Light.
    Easy,
    /// Medium difficulty.
    ///
    /// Also known as Another, Trick, Standard, or Difficult.
    Medium,
    /// Hard difficulty.
    ///
    /// Also known as SSR, Maniac, or Heavy.
    Hard,
    /// Expert difficulty.
    ///
    /// Also known as SManiac, Challenge, or Oni.
    Expert,
    /// Custom difficulty.
    ///
    /// Often user-generated, although sometimes used by the original simfile artist to provide
    /// more difficulty levels after `Expert`.
    Edit,
}
