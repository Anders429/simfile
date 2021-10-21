//! Uniform representation of song simfiles.
//!
//! This module provides a [`Song`] type for uniformly representing a song simfile. This allows for
//! reading, manipulating, and writing simfiles between formats interchangeably.

mod interpret;

use crate::parse;
use std::{fs, path::Path};

/// Situations in which a song is selectable.
///
/// Not all of these options are supported in every simfile format. The `Roulette`, `ExtraStage`,
/// `OneMoreExtraStage`, and `OnRemainingStages` variants seem to originate from Stepmania 3.9+, an
/// old unofficial fork of Stepmania 3.9. However, they are *technically* supported in Stepmania 5,
/// within both the `sm` and `ssc` file formats, to the extent that the values are simply
/// interpreted as `Always`. For data preservation reasons, they are included here.
#[derive(Debug, PartialEq)]
pub enum Selectable {
    /// Selectable.
    Always,
    /// Not selectable.
    Never,
    /// Can only be selected by roulette.
    Roulette,
    /// Can only be selected on an extra stage.
    ExtraStage,
    /// Can only be selected on a second extra stage.
    OneMoreExtraStage,
    /// Can only be selected when there are the provided number of stages remaining.
    ///
    /// # Example
    /// To make a song selectable only on the final stage, use a value of `1`:
    ///
    /// ```
    /// use simfile::song::Selectable;
    ///
    /// let final_stage = Selectable::OnRemainingStages(1);
    /// ```
    OnRemainingStages(u8),
}

/// A song simfile.
///
/// This struct strives to be a universal representation of a song simfile.
#[derive(Debug, Default, PartialEq)]
pub struct Song {
    /// The song's primary title.
    pub title: Option<String>,
    /// The song's subtitle.
    pub subtitle: Option<String>,
    /// The song's artist.
    pub artist: Option<String>,

    /// Selectability during song selection.
    pub selectable: Option<Selectable>,
}

impl Song {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_msd<P>(path: P) -> Self
    where
        P: AsRef<Path>,
    {
        interpret::msd::interpret(parse::msd::parse(&fs::read_to_string(path).unwrap())).unwrap()
    }
}
