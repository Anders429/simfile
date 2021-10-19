//! A library for reading, writing, and manipulating simfiles for rhythm game simulators.

pub mod song;

mod internal_log;
mod parse;

#[doc(inline)]
pub use song::Song;
