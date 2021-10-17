//! Parsing logic for `msd`-style simfile formats.
//!
//! The `msd` format style originates from `msd` files originally designed for use with the
//! *DDR'99* simulator. The format is built on the idea of providing sets of parameters to be
//! interpreted, in a format like the following:
//!
//! ```
//! #param0:param1:param2;  // Any nonzero number of parameters is acceptable.
//! ```
//!
//! This basic idea is used all throughout `msd`-style simfile formats to serialize simfile data.
//! The derived formats have maintained this same structure with a few exceptions. Some formats
//! have allowed for elision of the terminating `;` character. `dwi` specifically has specified
//! that some parameter lists should not begin with a `#` character. These exceptions are accounted
//! for within this parser.
