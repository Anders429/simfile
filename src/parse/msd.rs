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
//! another unescaped `/` must all be escaped with a preceeding `\` character. Leading and trailing
//! whitespace characters are ommitted.
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
#[derive(Debug, PartialEq)]
pub(crate) enum ParameterList {
    /// A tagged parameter list.
    ///
    /// This is a list of parameters beginning with a `#` character.
    Tagged(Vec<String>),
    /// An untagged parameter list.
    ///
    /// This is a list of parameters not beginning with a `#` character.
    Untagged(Vec<String>),
}

/// Parse an `msd`-style input string.
///
/// This function returns an iterator over the represented `ParameterList`s in the order in which
/// they appear. All comments are skipped and not included in the result.
///
/// Note that this parsing is infallible. Any string can be parsed as an `msd`-style format. It is
/// up to the consumer of the output to determine whether the resulting tokens are valid for the
/// expected format.
pub(crate) fn parse(input: &str) -> impl Iterator<Item = ParameterList> {
    /// What type of block we are currently within.
    enum BlockState {
        /// Currently within an untagged parameter list.
        UntaggedParameterList,
        /// Currently within a tagged parameter list.
        TaggedParameterList,
    }

    /// The current parameter parsing state.
    enum ParsingState {
        /// Attempting to find the beginning of a parameter.
        FindingParameter,
        /// Currently parsing a parameter.
        ///
        /// The stored `String` is a buffer containing parsed whitespace. It is flushed into the
        /// parameter when a non-whitespace parameter character is encountered.
        InParameter(String),
    }

    /// The current state regarding comments.
    enum CommentState {
        /// Not in a comment.
        None,
        /// Inside a comment. Skip all tokens until we reach a newline character.
        InComment,
    }

    /// State regarding the previous character parsed.
    enum PrevCharState {
        /// The previous characer is inconsequential.
        None,
        /// The previous character was a `/`, meaning we may be entering a comment.
        EnteringComment,
        /// The previous character was a `\`, so the next character is being escaped.
        Escaping,
    }

    /// The state regarding the parsing position on a line.
    enum LineState {
        /// Parsing is at the beginning of a line.
        ///
        /// This is not counting any whitespace encountered. This only indicates that a
        /// non-whitespace character has not been encountered on this line.
        Beginning,
        /// Parsing is past the beginning of a line.
        Middle,
    }

    let mut result = Vec::new();
    let mut parameter = String::new();
    let mut list = Vec::new();

    let mut block_state = BlockState::UntaggedParameterList;
    let mut parsing_state = ParsingState::FindingParameter;
    let mut comment_state = CommentState::None;
    let mut prev_char_state = PrevCharState::None;
    let mut line_state = LineState::Beginning;
    for c in input.chars() {
        match comment_state {
            CommentState::None => {
                match c {
                    '#' => {
                        match prev_char_state {
                            PrevCharState::Escaping => match parsing_state {
                                ParsingState::FindingParameter => {
                                    parsing_state = ParsingState::InParameter(String::new());
                                    parameter.push(c)
                                }
                                ParsingState::InParameter(ref mut buffer) => {
                                    parameter.push_str(buffer);
                                    parameter.push(c);
                                    *buffer = String::new();
                                }
                            },
                            PrevCharState::EnteringComment => {
                                parameter.push('/');
                                parameter.push(c);
                            }
                            PrevCharState::None => {
                                if list.is_empty()
                                    && parameter.is_empty()
                                    && !matches!(block_state, BlockState::TaggedParameterList)
                                {
                                    block_state = BlockState::TaggedParameterList;
                                } else {
                                    match line_state {
                                        LineState::Beginning => {
                                            // Finish the previous parameter list.
                                            if !parameter.is_empty()
                                                || !list.is_empty()
                                                || matches!(
                                                    block_state,
                                                    BlockState::TaggedParameterList
                                                )
                                            {
                                                list.push(parameter);
                                                parameter = String::new();
                                                parsing_state = ParsingState::FindingParameter;
                                            }
                                            if !list.is_empty() {
                                                match block_state {
                                                    BlockState::TaggedParameterList => {
                                                        result.push(ParameterList::Tagged(list));
                                                    }
                                                    BlockState::UntaggedParameterList => {
                                                        result.push(ParameterList::Untagged(list));
                                                    }
                                                }
                                                list = Vec::new();
                                            }
                                            block_state = BlockState::TaggedParameterList;
                                        }
                                        LineState::Middle => match parsing_state {
                                            ParsingState::FindingParameter => {
                                                parsing_state =
                                                    ParsingState::InParameter(String::new());
                                                parameter.push(c)
                                            }
                                            ParsingState::InParameter(ref mut buffer) => {
                                                parameter.push_str(buffer);
                                                parameter.push(c);
                                                *buffer = String::new();
                                            }
                                        },
                                    }
                                }
                            }
                        }
                        prev_char_state = PrevCharState::None;
                    }
                    ':' => {
                        match prev_char_state {
                            PrevCharState::Escaping => match parsing_state {
                                ParsingState::FindingParameter => {
                                    parsing_state = ParsingState::InParameter(String::new());
                                    parameter.push(c)
                                }
                                ParsingState::InParameter(ref mut buffer) => {
                                    parameter.push_str(buffer);
                                    parameter.push(c);
                                    *buffer = String::new();
                                }
                            },
                            PrevCharState::EnteringComment => {
                                parameter.push('/');
                                list.push(parameter);
                                parameter = String::new();
                                parsing_state = ParsingState::FindingParameter;
                            }
                            PrevCharState::None => {
                                list.push(parameter);
                                parameter = String::new();
                                parsing_state = ParsingState::FindingParameter;
                            }
                        }
                        prev_char_state = PrevCharState::None;
                    }
                    ';' => {
                        match prev_char_state {
                            PrevCharState::Escaping => match parsing_state {
                                ParsingState::FindingParameter => {
                                    parsing_state = ParsingState::InParameter(String::new());
                                    parameter.push(c)
                                }
                                ParsingState::InParameter(ref mut buffer) => {
                                    parameter.push_str(buffer);
                                    parameter.push(c);
                                    *buffer = String::new();
                                }
                            },
                            PrevCharState::EnteringComment => {
                                parameter.push('/');
                                list.push(parameter);
                                parameter = String::new();
                                parsing_state = ParsingState::FindingParameter;
                                match block_state {
                                    BlockState::TaggedParameterList => {
                                        result.push(ParameterList::Tagged(list));
                                    }
                                    BlockState::UntaggedParameterList => {
                                        result.push(ParameterList::Untagged(list));
                                    }
                                }
                                list = Vec::new();
                                block_state = BlockState::UntaggedParameterList;
                            }
                            PrevCharState::None => {
                                list.push(parameter);
                                parameter = String::new();
                                parsing_state = ParsingState::FindingParameter;
                                match block_state {
                                    BlockState::TaggedParameterList => {
                                        result.push(ParameterList::Tagged(list));
                                    }
                                    BlockState::UntaggedParameterList => {
                                        result.push(ParameterList::Untagged(list));
                                    }
                                }
                                list = Vec::new();
                                block_state = BlockState::UntaggedParameterList;
                            }
                        }
                        prev_char_state = PrevCharState::None;
                    }
                    '/' => {
                        if matches!(prev_char_state, PrevCharState::Escaping) {
                            match parsing_state {
                                ParsingState::FindingParameter => {
                                    parsing_state = ParsingState::InParameter(String::new());
                                    parameter.push(c)
                                }
                                ParsingState::InParameter(ref mut buffer) => {
                                    parameter.push_str(buffer);
                                    parameter.push(c);
                                    *buffer = String::new();
                                }
                            }
                            prev_char_state = PrevCharState::None;
                        } else {
                            match prev_char_state {
                                PrevCharState::EnteringComment => {
                                    comment_state = CommentState::InComment;
                                    prev_char_state = PrevCharState::None;
                                }
                                PrevCharState::Escaping | PrevCharState::None => {
                                    prev_char_state = PrevCharState::EnteringComment;
                                }
                            }
                        }
                    }
                    '\\' => match prev_char_state {
                        PrevCharState::Escaping => {
                            parameter.push(c);
                            prev_char_state = PrevCharState::None;
                        }
                        PrevCharState::EnteringComment => {
                            parameter.push('/');
                            prev_char_state = PrevCharState::Escaping;
                        }
                        PrevCharState::None => {
                            prev_char_state = PrevCharState::Escaping;
                        }
                    },
                    _ => {
                        match parsing_state {
                            ParsingState::FindingParameter => match prev_char_state {
                                PrevCharState::None | PrevCharState::Escaping => {
                                    if !c.is_whitespace() {
                                        parsing_state = ParsingState::InParameter(String::new());
                                        parameter.push(c);
                                    }
                                }
                                PrevCharState::EnteringComment => {
                                    parameter.push('/');
                                    if c.is_whitespace() {
                                        parsing_state = ParsingState::InParameter(c.to_string());
                                    } else {
                                        parsing_state = ParsingState::InParameter(String::new());
                                        parameter.push(c);
                                    }
                                }
                            },
                            ParsingState::InParameter(ref mut buffer) => {
                                if c.is_whitespace() {
                                    buffer.push(c);
                                } else {
                                    parameter.push_str(buffer);
                                    parameter.push(c);
                                    *buffer = String::new();
                                }
                            }
                        }
                        prev_char_state = PrevCharState::None;
                    }
                }

                // Evaluate where parsing is within the line.
                match c {
                    '\n' => {
                        line_state = LineState::Beginning;
                    }
                    _ => {
                        if !c.is_whitespace() {
                            line_state = LineState::Middle;
                        }
                    }
                }
            }
            CommentState::InComment => {
                if c == '\n' {
                    comment_state = CommentState::None;
                    line_state = LineState::Beginning;
                }
            }
        }
    }

    if matches!(comment_state, CommentState::None) {
        if !parameter.is_empty()
            || !list.is_empty()
            || matches!(block_state, BlockState::TaggedParameterList)
        {
            list.push(parameter);
        }
        if !list.is_empty() {
            match block_state {
                BlockState::TaggedParameterList => {
                    result.push(ParameterList::Tagged(list));
                }
                BlockState::UntaggedParameterList => {
                    result.push(ParameterList::Untagged(list));
                }
            }
        }
    }

    result.into_iter()
}

#[cfg(test)]
mod tests {
    use super::{parse, ParameterList};

    #[test]
    fn empty() {
        itertools::assert_equal(parse(""), []);
    }

    #[test]
    fn comment() {
        itertools::assert_equal(parse("//foo"), []);
    }

    #[test]
    fn tagged_parameter_list_single() {
        itertools::assert_equal(
            parse("#foo;"),
            [ParameterList::Tagged(vec!["foo".to_owned()])],
        );
    }

    #[test]
    fn tagged_parameter_list_many() {
        itertools::assert_equal(
            parse("#foo:bar:baz:qux;"),
            [ParameterList::Tagged(vec![
                "foo".to_owned(),
                "bar".to_owned(),
                "baz".to_owned(),
                "qux".to_owned(),
            ])],
        );
    }

    #[test]
    fn tagged_parameter_list_two_lines() {
        itertools::assert_equal(
            parse("#foo:\nbar;"),
            [ParameterList::Tagged(vec![
                "foo".to_owned(),
                "bar".to_owned(),
            ])],
        );
    }

    #[test]
    fn tagged_parameter_list_with_comment() {
        itertools::assert_equal(
            parse("#foo://comment\nbar;"),
            [ParameterList::Tagged(vec![
                "foo".to_owned(),
                "bar".to_owned(),
            ])],
        );
    }

    #[test]
    fn tagged_parameter_list_escaped_characters() {
        itertools::assert_equal(
            parse(r"#\#\;\:/\/\\;"),
            [ParameterList::Tagged(vec![r"#;://\".to_owned()])],
        );
    }

    #[test]
    fn tagged_parameter_list_elided_semicolon_at_end_of_input() {
        itertools::assert_equal(
            parse("#foo"),
            [ParameterList::Tagged(vec!["foo".to_owned()])],
        );
    }

    #[test]
    fn tagged_parameter_list_elided_semicolon_with_following_tagged_parameter_list() {
        itertools::assert_equal(
            parse("#foo\n#bar;"),
            [
                ParameterList::Tagged(vec!["foo".to_owned()]),
                ParameterList::Tagged(vec!["bar".to_owned()]),
            ],
        );
    }

    #[test]
    fn tagged_parameter_list_unelided_semicolon_without_newline() {
        itertools::assert_equal(
            parse("#foo#bar;"),
            [ParameterList::Tagged(vec!["foo#bar".to_owned()])],
        );
    }

    #[test]
    fn tagged_parameter_list_elided_semicolon_with_newline_and_space() {
        itertools::assert_equal(
            parse("#foo\n #bar;"),
            [
                ParameterList::Tagged(vec!["foo".to_owned()]),
                ParameterList::Tagged(vec!["bar".to_owned()]),
            ],
        );
    }

    #[test]
    fn untagged_parameter_list_single() {
        itertools::assert_equal(
            parse("foo;"),
            [ParameterList::Untagged(vec!["foo".to_owned()])],
        );
    }

    #[test]
    fn untagged_parameter_list_many() {
        itertools::assert_equal(
            parse("foo:bar:baz:qux;"),
            [ParameterList::Untagged(vec![
                "foo".to_owned(),
                "bar".to_owned(),
                "baz".to_owned(),
                "qux".to_owned(),
            ])],
        );
    }

    #[test]
    fn untagged_parameter_list_two_lines() {
        itertools::assert_equal(
            parse("foo:\nbar;"),
            [ParameterList::Untagged(vec![
                "foo".to_owned(),
                "bar".to_owned(),
            ])],
        );
    }

    #[test]
    fn untagged_parameter_list_with_comment() {
        itertools::assert_equal(
            parse("foo://comment\nbar;"),
            [ParameterList::Untagged(vec![
                "foo".to_owned(),
                "bar".to_owned(),
            ])],
        );
    }

    #[test]
    fn untagged_parameter_list_escaped_characters() {
        itertools::assert_equal(
            parse(r"\#\;\:/\/\\;"),
            [ParameterList::Untagged(vec![r"#;://\".to_owned()])],
        );
    }

    #[test]
    fn untagged_parameter_list_elided_semicolon_at_end_of_input() {
        itertools::assert_equal(
            parse("foo"),
            [ParameterList::Untagged(vec!["foo".to_owned()])],
        );
    }

    #[test]
    fn untagged_parameter_list_elided_semicolon_with_following_tagged_parameter_list() {
        itertools::assert_equal(
            parse("foo\n#bar;"),
            [
                ParameterList::Untagged(vec!["foo".to_owned()]),
                ParameterList::Tagged(vec!["bar".to_owned()]),
            ],
        );
    }

    #[test]
    fn untagged_parameter_list_unelided_semicolon_without_newline() {
        itertools::assert_equal(
            parse("foo#bar;"),
            [ParameterList::Untagged(vec!["foo#bar".to_owned()])],
        );
    }

    #[test]
    fn untagged_parameter_list_unelided_semicolon_with_newline_and_space() {
        itertools::assert_equal(
            parse("foo\n #bar;"),
            [
                ParameterList::Untagged(vec!["foo".to_owned()]),
                ParameterList::Tagged(vec!["bar".to_owned()]),
            ],
        );
    }

    #[test]
    fn multiple_tagged_parameter_lists() {
        itertools::assert_equal(
            parse("#foo:bar;\n#baz:qux;"),
            [
                ParameterList::Tagged(vec!["foo".to_owned(), "bar".to_owned()]),
                ParameterList::Tagged(vec!["baz".to_owned(), "qux".to_owned()]),
            ],
        );
    }

    #[test]
    fn multiple_untagged_parameter_lists() {
        itertools::assert_equal(
            parse("foo:bar;\nbaz:qux;"),
            [
                ParameterList::Untagged(vec!["foo".to_owned(), "bar".to_owned()]),
                ParameterList::Untagged(vec!["baz".to_owned(), "qux".to_owned()]),
            ],
        );
    }

    #[test]
    fn mixed_tagged_and_untagged_parameter_lists() {
        itertools::assert_equal(
            parse("#foo:bar;\nbaz:qux;\n#quux:quuz"),
            [
                ParameterList::Tagged(vec!["foo".to_owned(), "bar".to_owned()]),
                ParameterList::Untagged(vec!["baz".to_owned(), "qux".to_owned()]),
                ParameterList::Tagged(vec!["quux".to_owned(), "quuz".to_owned()]),
            ],
        );
    }

    #[test]
    fn mixed_tagged_and_untagged_parameter_lists_no_newlines() {
        itertools::assert_equal(
            parse("#foo:bar;baz:qux;#quux:quuz"),
            [
                ParameterList::Tagged(vec!["foo".to_owned(), "bar".to_owned()]),
                ParameterList::Untagged(vec!["baz".to_owned(), "qux".to_owned()]),
                ParameterList::Tagged(vec!["quux".to_owned(), "quuz".to_owned()]),
            ],
        );
    }

    #[test]
    fn comment_between_parameter_lists() {
        itertools::assert_equal(
            parse("#foo:bar;\n//comment\nbaz:qux;"),
            [
                ParameterList::Tagged(vec!["foo".to_owned(), "bar".to_owned()]),
                ParameterList::Untagged(vec!["baz".to_owned(), "qux".to_owned()]),
            ],
        );
    }

    #[test]
    fn tagged_and_untagged_parameter_lists_comments_and_whitespace() {
        itertools::assert_equal(
            parse("#foo:bar;\n//comment\n  \n\nbaz:qux;"),
            [
                ParameterList::Tagged(vec!["foo".to_owned(), "bar".to_owned()]),
                ParameterList::Untagged(vec!["baz".to_owned(), "qux".to_owned()]),
            ],
        );
    }

    #[test]
    fn multiple_empty_parameters() {
        itertools::assert_equal(
            parse(":"),
            [ParameterList::Untagged(vec!["".to_owned(), "".to_owned()])],
        );
    }

    #[test]
    fn only_one_empty_parameter_untagged_elided_semicolon() {
        itertools::assert_equal(parse(" "), []);
    }

    #[test]
    fn only_one_empty_parameter_tagged_elided_semicolon() {
        itertools::assert_equal(parse("# "), [ParameterList::Tagged(vec!["".to_owned()])]);
    }

    #[test]
    fn parameters_strip_leading_whitespace() {
        itertools::assert_equal(
            parse("# foo;"),
            [ParameterList::Tagged(vec!["foo".to_owned()])],
        );
    }

    #[test]
    fn parameters_strip_trailing_whitespace() {
        itertools::assert_equal(
            parse("#foo ;"),
            [ParameterList::Tagged(vec!["foo".to_owned()])],
        );
    }

    #[test]
    fn parameters_strip_leading_and_trailing_whitespace() {
        itertools::assert_equal(
            parse("# foo ;"),
            [ParameterList::Tagged(vec!["foo".to_owned()])],
        );
    }

    #[test]
    fn sequential_empty_tagged_parameter_lists_elided_semicolons() {
        itertools::assert_equal(
            parse("#\n#"),
            [
                ParameterList::Tagged(vec!["".to_owned()]),
                ParameterList::Tagged(vec!["".to_owned()]),
            ],
        );
    }

    #[test]
    fn sequential_empty_tagged_parameter_lists() {
        itertools::assert_equal(
            parse("#;#;"),
            [
                ParameterList::Tagged(vec!["".to_owned()]),
                ParameterList::Tagged(vec!["".to_owned()]),
            ],
        );
    }

    #[test]
    fn sequential_empty_untagged_parameter_lists() {
        itertools::assert_equal(
            parse(";;"),
            [
                ParameterList::Untagged(vec!["".to_owned()]),
                ParameterList::Untagged(vec!["".to_owned()]),
            ],
        );
    }

    #[test]
    fn single_slash() {
        itertools::assert_equal(
            parse("/foo;"),
            [ParameterList::Untagged(vec!["/foo".to_owned()])],
        );
    }

    #[test]
    fn single_slash_followed_by_whitespace() {
        itertools::assert_equal(
            parse("/ foo;"),
            [ParameterList::Untagged(vec!["/ foo".to_owned()])],
        );
    }

    #[test]
    fn escaped_slash_at_start_of_parameter() {
        itertools::assert_equal(
            parse(r"\/;"),
            [ParameterList::Untagged(vec!["/".to_owned()])],
        );
    }

    #[test]
    fn escaped_colon_at_start_of_parameter() {
        itertools::assert_equal(
            parse(r"\:;"),
            [ParameterList::Untagged(vec![":".to_owned()])],
        );
    }

    #[test]
    fn escaped_semicolon_at_start_of_parameter() {
        itertools::assert_equal(
            parse(r"\;;"),
            [ParameterList::Untagged(vec![";".to_owned()])],
        );
    }

    #[test]
    fn escaped_tag_in_middle_of_parameter() {
        itertools::assert_equal(
            parse(r"foo\#;"),
            [ParameterList::Untagged(vec!["foo#".to_owned()])],
        );
    }

    #[test]
    fn unescaped_tag_in_middle_of_line() {
        itertools::assert_equal(
            parse("foo#;"),
            [ParameterList::Untagged(vec!["foo#".to_owned()])],
        );
    }

    #[test]
    fn unescaped_tag_in_middle_of_line_and_beginning_of_parameter() {
        itertools::assert_equal(parse("##;"), [ParameterList::Tagged(vec!["#".to_owned()])]);
    }

    #[test]
    fn unescaped_tag_in_middle_of_line_and_beginning_of_parameter_after_slash() {
        itertools::assert_equal(
            parse("#/#;"),
            [ParameterList::Tagged(vec!["/#".to_owned()])],
        );
    }

    #[test]
    fn unescaped_tag_after_slash() {
        itertools::assert_equal(
            parse("/#;"),
            [ParameterList::Untagged(vec!["/#".to_owned()])],
        );
    }

    #[test]
    fn slash_at_end_of_parameter() {
        itertools::assert_equal(
            parse("/:;"),
            [ParameterList::Untagged(vec!["/".to_owned(), "".to_owned()])],
        );
    }

    #[test]
    fn slash_at_end_of_untagged_parameter_list() {
        itertools::assert_equal(parse("/;"), [ParameterList::Untagged(vec!["/".to_owned()])]);
    }

    #[test]
    fn slash_at_end_of_tagged_parameter_list() {
        itertools::assert_equal(parse("#/;"), [ParameterList::Tagged(vec!["/".to_owned()])]);
    }
}
