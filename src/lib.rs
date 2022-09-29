//! A library for reading, writing, and manipulating simfiles for rhythm game simulators.

#![warn(unsafe_op_in_unsafe_fn)]

pub mod song;

#[doc(inline)]
pub use song::Song;
