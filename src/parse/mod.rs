//! Generic internal parsers for various configuration formats used as the basis of different file
//! formats.
//!
//! This is parsing logic shared across multiple simfile types. Several different simfiles have a
//! shared history, and therefore have shared parsing logic. For example, `msd`, `dwi`, `sm`, and
//! `ssc` all share the same tagged parameter format, derived from the original `msd` format
//! specification. Therefore, these formats (and many others) all share the same parser, which
//! extracts the data and prepares it for interpretation.

pub(crate) mod msd;
