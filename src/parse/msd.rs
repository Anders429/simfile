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

pub(crate) fn parse(input: &str) -> impl Iterator<Item = ParameterList> {
    enum BlockState {
        UntaggedParameterList,
        TaggedParameterList,
    }

    enum ParsingState {
        FindingParameter,
        InParameter(String),
    }

    enum CommentState {
        None,
        InComment,
    }

    enum PrevCharState {
        None,
        EnteringComment,
        Escaping,
    }

    enum LineState {
        Beginning,
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
                        todo!("Account for escapes here and in other blocks.");
                        if list.is_empty() {
                            todo!();
                        } else {
                            match line_state {
                                LineState::Beginning => {
                                    // Finish the previous parameter list.
                                    if !parameter.is_empty() {
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
                                LineState::Middle => {
                                    todo!();
                                }
                            }
                        }
                        prev_char_state = PrevCharState::None;
                    }
                    ':' => {
                        list.push(parameter);
                        parameter = String::new();
                        parsing_state = ParsingState::FindingParameter;
                        prev_char_state = PrevCharState::None;
                    }
                    ';' => {
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
                        prev_char_state = PrevCharState::None;
                    }
                    '/' => {
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
                    '\\' => {
                        match prev_char_state {
                            PrevCharState::Escaping => {
                                parameter.push(c);
                                prev_char_state = PrevCharState::None;
                            }
                            PrevCharState::EnteringComment | PrevCharState::None => {
                                prev_char_state = PrevCharState::Escaping;
                            }
                        }
                    }
                    _ => {
                        match parsing_state {
                            ParsingState::FindingParameter => {
                                match prev_char_state {
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
                                }
                            }
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
        itertools::assert_equal(parse("#foo;"), [ParameterList::Tagged(vec!["foo".to_owned()])]);
    }

    #[test]
    fn tagged_parameter_list_many() {
        itertools::assert_equal(
            parse("#foo:bar:baz:qux;"),
            [ParameterList::Tagged(vec!["foo".to_owned(), "bar".to_owned(), "baz".to_owned(), "qux".to_owned()])],
        );
    }

    #[test]
    fn tagged_parameter_list_two_lines() {
        itertools::assert_equal(
            parse("#foo:\nbar;"),
            [ParameterList::Tagged(vec!["foo".to_owned(), "bar".to_owned()])],
        );
    }

    #[test]
    fn tagged_parameter_list_with_comment() {
        itertools::assert_equal(
            parse("#foo://comment\nbar;"),
            [ParameterList::Tagged(vec!["foo".to_owned(), "bar".to_owned()])],
        );
    }

    #[test]
    fn tagged_parameter_list_escaped_characters() {
        itertools::assert_equal(
            parse(r"#\#\;\:/\/;"),
            [ParameterList::Tagged(vec!["#;://".to_owned()])],
        );
    }

    #[test]
    fn tagged_parameter_list_elided_semicolon_at_end_of_input() {
        itertools::assert_equal(parse("#foo"), [ParameterList::Tagged(vec!["foo".to_owned()])]);
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
        itertools::assert_equal(parse("#foo#bar;"), [ParameterList::Tagged(vec!["foo#bar".to_owned()])]);
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
        itertools::assert_equal(parse("foo;"), [ParameterList::Untagged(vec!["foo".to_owned()])]);
    }

    #[test]
    fn untagged_parameter_list_many() {
        itertools::assert_equal(
            parse("foo:bar:baz:qux;"),
            [ParameterList::Untagged(vec!["foo".to_owned(), "bar".to_owned(), "baz".to_owned(), "qux".to_owned()])],
        );
    }

    #[test]
    fn untagged_parameter_list_two_lines() {
        itertools::assert_equal(
            parse("foo:\nbar;"),
            [ParameterList::Untagged(vec!["foo".to_owned(), "bar".to_owned()])],
        );
    }

    #[test]
    fn untagged_parameter_list_with_comment() {
        itertools::assert_equal(
            parse("foo://comment\nbar;"),
            [ParameterList::Untagged(vec!["foo".to_owned(), "bar".to_owned()])],
        );
    }

    #[test]
    fn untagged_parameter_list_escaped_characters() {
        itertools::assert_equal(
            parse(r"\#\;\:/\/;"),
            [ParameterList::Untagged(vec!["#;://".to_owned()])],
        );
    }

    #[test]
    fn untagged_parameter_list_elided_semicolon_at_end_of_input() {
        itertools::assert_equal(parse("foo"), [ParameterList::Untagged(vec!["foo".to_owned()])]);
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
        itertools::assert_equal(parse(":"), [ParameterList::Untagged(vec!["".to_owned(), "".to_owned()])]);
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
        itertools::assert_equal(parse("# foo;"), [ParameterList::Tagged(vec!["foo".to_owned()])]);
    }

    #[test]
    fn parameters_strip_trailing_whitespace() {
        itertools::assert_equal(parse("#foo ;"), [ParameterList::Tagged(vec!["foo".to_owned()])]);
    }

    #[test]
    fn parameters_strip_leading_and_trailing_whitespace() {
        itertools::assert_equal(parse("# foo ;"), [ParameterList::Tagged(vec!["foo".to_owned()])]);
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
}
