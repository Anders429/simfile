//! Generic representation of a song.
//! 
//! This module contains all generic types necessary to represent a song simfile regardless of that
//! simfile's original format. This allows for simfiles to be universally read and edited, as well
//! as enabling easy transcoding between simfile formats. 

mod msd;

use either::Either;
use std::{fs::File, io, io::BufReader, iter, mem::MaybeUninit, path::Path};

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

impl From<msd::Step> for [Panel; 4] {
    fn from(msd_step: msd::Step) -> Self {
        match msd_step {
            msd::Step::None => [Panel::None; 4],
            msd::Step::Left => [Panel::Step, Panel::None, Panel::None, Panel::None],
            msd::Step::Down => [Panel::None, Panel::Step, Panel::None, Panel::None],
            msd::Step::Up => [Panel::None, Panel::None, Panel::Step, Panel::None],
            msd::Step::Right => [Panel::None, Panel::None, Panel::None, Panel::Step],
            msd::Step::DownLeft => [Panel::Step, Panel::Step, Panel::None, Panel::None],
            msd::Step::UpLeft => [Panel::Step, Panel::None, Panel::Step, Panel::None],
            msd::Step::LeftRight => [Panel::Step, Panel::None, Panel::None, Panel::Step],
            msd::Step::UpDown => [Panel::None, Panel::Step, Panel::Step, Panel::None],
            msd::Step::DownRight => [Panel::None, Panel::Step, Panel::None, Panel::Step],
            msd::Step::UpRight => [Panel::None, Panel::None, Panel::Step, Panel::Step],
        }
    }
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

impl From<msd::Steps> for Steps<4> {
    fn from(msd_steps: msd::Steps) -> Self {
        let mut steps = Vec::new();

        for notes in msd_steps.notes {
            match notes {
                msd::Notes::Eighth(step) => {
                    steps.push(Step {
                        panels: step.into(),
                        duration: Duration::Eighth,
                    });
                }
            }
        }

        Steps { steps }
    }
}

impl From<(msd::Steps, msd::Steps)> for Steps<8> {
    fn from(msd_steps: (msd::Steps, msd::Steps)) -> Self {
        let mut steps = Vec::new();

        // Combines the notes from both step charts into a single iterator, accounting for differing length as well.
        let notes_0_len = msd_steps.0.notes.len();
        let notes_1_len = msd_steps.1.notes.len();
        let notes_iter = if notes_0_len > notes_1_len {
            Either::Left(Either::Left(
                msd_steps.0.notes.into_iter().zip(
                    msd_steps.1.notes.into_iter().chain(
                        iter::repeat(msd::Notes::Eighth(msd::Step::None))
                            .take(notes_0_len - notes_1_len),
                    ),
                ),
            ))
        } else if notes_0_len < notes_1_len {
            Either::Left(Either::Right(
                msd_steps
                    .0
                    .notes
                    .into_iter()
                    .chain(
                        iter::repeat(msd::Notes::Eighth(msd::Step::None))
                            .take(notes_0_len - notes_1_len),
                    )
                    .zip(msd_steps.1.notes.into_iter()),
            ))
        } else {
            Either::Right(
                msd_steps
                    .0
                    .notes
                    .into_iter()
                    .zip(msd_steps.1.notes.into_iter()),
            )
        };

        for (left_notes, right_notes) in notes_iter {
            match (left_notes, right_notes) {
                (msd::Notes::Eighth(left_step), msd::Notes::Eighth(right_step)) => {
                    steps.push(Step {
                        panels: {
                            let mut whole = MaybeUninit::uninit();
                            let ptr = whole.as_mut_ptr() as *mut [Panel; 4];
                            unsafe {
                                ptr.write(left_step.into());
                                ptr.add(1).write(right_step.into());
                                whole.assume_init()
                            }
                        },
                        duration: Duration::Eighth,
                    })
                }
            }
        }

        Steps { steps }
    }
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

impl From<msd::Difficulty> for Difficulty {
    fn from(difficulty: msd::Difficulty) -> Self {
        match difficulty {
            msd::Difficulty::Basic => Difficulty::Easy,
            msd::Difficulty::Another => Difficulty::Medium,
            msd::Difficulty::Maniac => Difficulty::Hard,
        }
    }
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

impl From<msd::Song> for Song {
    fn from(song: msd::Song) -> Self {
        let mut charts = Vec::new();

        for single in song.single {
            charts.push(Chart {
                difficulty: single.0.into(),
                meter: single.1,
                style: Style::Single(single.2.into()),
            })
        }

        for double in song.double {
            charts.push(Chart {
                difficulty: double.0.into(),
                meter: double.1,
                style: Style::Double((double.2, double.3).into()),
            })
        }

        for couple in song.couple {
            charts.push(Chart {
                difficulty: couple.0.into(),
                meter: couple.1,
                style: Style::Couple((couple.2, couple.3).into()),
            })
        }

        Song {
            title: song.title,
            subtitle: None,
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

    #[test]
    fn test() {
        dbg!(Song::from_msd("test/data/msd/BAD.msd"));
    }
}
