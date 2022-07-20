//! Generic representation of a song.
//!
//! This module contains all generic types necessary to represent a song simfile regardless of that
//! simfile's original format. This allows for simfiles to be universally read and edited, as well
//! as enabling easy transcoding between simfile formats.

mod msd;
mod util;

use ctrl_z::ReadToCtrlZ;
use std::{
    fs::File,
    io,
    io::{BufReader, BufWriter},
    path::Path,
};

#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    Io(io::Error),
    Serialization(::msd::ser::Error),
    Deserialization(::msd::de::Error),

    ToMsd(msd::Error),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Panel {
    None,
    Step,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Duration {
    Eighth,
    Sixteenth,
    TwentyFourth,
    SixtyFourth,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Step<const PANELS: usize> {
    panels: [Panel; PANELS],
    /// Duration after this step until the next step.
    duration: Duration,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Steps<const PANELS: usize> {
    steps: Vec<Step<PANELS>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Style {
    Single(Steps<4>),
    Double(Steps<8>),
    Couple(Steps<8>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Difficulty {
    Beginner,
    Easy,
    Medium,
    Hard,
    Challenge,
    Edit,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Chart {
    difficulty: Difficulty,
    meter: u8,
    /// Contains style and steps for that style.
    style: Style,
}

#[derive(Clone, Debug, PartialEq)]
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
    /// Read a `.msd` formatted song simfile.
    ///
    /// Specifically, this function will read the simfile using the `MSD 2.0` specification.
    pub fn read_msd<P>(path: P) -> Result<Self, Error>
    where
        P: AsRef<Path>,
    {
        ::msd::from_reader::<_, msd::Song>(BufReader::new(ReadToCtrlZ::new(File::open(path).map_err(Error::Io)?)))
            .map_err(Error::Deserialization)
            .map(|song| song.into())
    }

    /// Write this song simfile to a `.msd` formatted file.
    ///
    /// This method will fail if the song simfile contains elements that cannot be properly encoded
    /// in a `.msd` file. Specifically, this method will attempt to encode the simfile using the
    /// `MSD 2.0` specification.
    pub fn write_msd<P>(self, path: P) -> Result<(), Error>
    where
        P: AsRef<Path>,
    {
        ::msd::to_writer::<_, msd::Song>(
            BufWriter::new(File::create(path).map_err(Error::Io)?),
            &self.try_into().map_err(Error::ToMsd)?,
        )
        .map_err(Error::Serialization)
    }
}
