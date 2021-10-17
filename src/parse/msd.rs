//! Parsing logic for `msd`-style simfile formats.
//!
//! The `msd` format style originates from `msd` files originally designed for use with the
//! *DDR'99* simulator. The format is built on the idea of providing sets of parameters to be
//! interpreted, in a format like the following:
//!
//! ``` msd
//! #param0:param1:param2;  // Any nonzero number of parameters is acceptable.
//! ```
//!
//! This basic idea is used all throughout `msd`-style simfile formats to serialize simfile data.
//! The derived formats have maintained this same structure with a few exceptions. Some formats
//! have allowed for elision of the terminating `;` character. `dwi` specifically has specified
//! that some parameter lists should not begin with a `#` character. These exceptions are accounted
//! for within this parser.
//!
//! The MSD format is here specified as follows:
//!
//! A **parameter** is any string of characters. Note that `#`, `;`, `:`, `\`, and `/` when after
//! another unescaped `/` must all be escaped with a preceeding `\` character.
//!
//! A **parameter list** is one or more parameters separated by the `:` character and terminated by
//! the `;` character.
//!
//! A **tagged parameter list** is a parameter list preceeded by a `#` character.
//!
//! A **comment** is any string of characters preceeded by `//` and terminated by the `\n` newline
//! character.
//!
//! An `msd` file is a sequence of parameter lists, tagged parameter lists, comments, and
//! whitespace in any order.

/// A list of parameters provided together in the simfile.
///
/// Parameter lists can either be `Tagged` or `Untagged`, denoting whether they begin with a `#`
/// character or not. Most parameter lists will be tagged, but there are some exceptions, such as,
/// for example, the separate parameters of the `#BACKGROUND` `dwi` tag. Consumers should only use
/// the `Untagged` variant if that is what they are explicitly expecting.
pub(crate) enum ParameterList<'a> {
    /// A tagged parameter list.
    ///
    /// This is a list of parameters beginning with a `#` character.
    Tagged(Vec<&'a str>),
    /// An untagged parameter list.
    ///
    /// This is a list of parameters not beginning with a `#` character.
    Untagged(Vec<&'a str>),
}

pub(crate) fn parse<'a>(input: &'a str) -> impl Iterator<Item = ParameterList<'a>> {
    vec![].into_iter()
}
