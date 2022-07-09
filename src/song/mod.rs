//! Generic representation of a song.
//!
//! This module contains all generic types necessary to represent a song simfile regardless of that
//! simfile's original format. This allows for simfiles to be universally read and edited, as well
//! as enabling easy transcoding between simfile formats.

mod msd;
mod util;

use std::{fs::File, io, io::BufReader, path::Path};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Deserialization(::msd::de::Error),
}

#[derive(Clone, Copy, Debug)]
pub enum Panel {
    None,
    Step,
}

#[derive(Debug)]
pub enum Duration {
    Eighth,
}

#[derive(Debug)]
pub struct Step<const PANELS: usize> {
    panels: [Panel; PANELS],
    /// Duration after this step until the next step.
    duration: Duration,
}

#[derive(Debug)]
pub struct Steps<const PANELS: usize> {
    steps: Vec<Step<PANELS>>,
}

#[derive(Debug)]
pub enum Style {
    Single(Steps<4>),
    Double(Steps<8>),
    Couple(Steps<8>),
}

#[derive(Debug)]
pub enum Difficulty {
    Beginner,
    Easy,
    Medium,
    Hard,
    Challenge,
    Edit,
}

#[derive(Debug)]
pub struct Chart {
    difficulty: Difficulty,
    meter: u8,
    /// Contains style and steps for that style.
    style: Style,
}

#[derive(Debug)]
pub struct Song {
    title: Option<String>,
    subtitle: Option<String>,
    artist: Option<String>,
    credit: Option<String>,

    bpm: Option<f64>,
    offset: Option<i64>,

    background_file: Option<String>,
    music_file: Option<String>,
    music_preview_file: Option<String>,

    charts: Vec<Chart>,
}

impl Song {
    pub fn from_msd<P>(path: P) -> Result<Self, Error>
    where
        P: AsRef<Path>,
    {
        ::msd::from_reader::<_, msd::Song>(BufReader::new(
            File::open(path).map_err(|error| Error::Io(error))?,
        ))
        .map_err(|error| Error::Deserialization(error))
        .map(|song| song.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        dbg!(Song::from_msd("test/data/msd/BAD.msd"));
    }
}
