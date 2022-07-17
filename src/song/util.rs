/// Derives a title and subtitle from a single title string.
///
/// This is required by some older formats that did not have a dedicated subtitle string. The
/// subtitle is included directly in the title for those formats, and is extracted using this
/// function.
///
/// Specifically, this function finds the first occurrance of one of the following delimiter
/// substrings: `"\t", " -", " ~", " (", or " ["`, giving precedence to the substrings in the order
/// provided here (meaning `"\t"` has highest precedence, and `" ["` has lowest precedence). The
/// title is then split at that occurrance and returned. This strategy is adopted directly from
/// Stepmania.
pub(in crate::song) fn split_title_and_subtitle(mut title: String) -> (String, Option<String>) {
    // Valid separators, in order of precedence.
    enum Separator {
        Dash,
        Tilde,
        Parenthesis,
        Bracket,
        None,
    }
    let mut space = false;
    let mut separator = Separator::None;
    let mut current_index = None;
    let mut char_indices = title.char_indices();
    if let Some(index) = loop {
        if let Some((index, c)) = char_indices.next() {
            if c == '\t' {
                break Some(index + 1);
            }

            if space {
                match c {
                    '-' => {
                        if matches!(
                            separator,
                            Separator::None
                                | Separator::Bracket
                                | Separator::Parenthesis
                                | Separator::Tilde
                        ) {
                            separator = Separator::Dash;
                            current_index = Some(index);
                        }
                    }
                    '~' => {
                        if matches!(
                            separator,
                            Separator::None | Separator::Bracket | Separator::Parenthesis
                        ) {
                            separator = Separator::Tilde;
                            current_index = Some(index);
                        }
                    }
                    '(' => {
                        if matches!(separator, Separator::None | Separator::Bracket) {
                            separator = Separator::Parenthesis;
                            current_index = Some(index);
                        }
                    }
                    '[' => {
                        if matches!(separator, Separator::None) {
                            separator = Separator::Bracket;
                            current_index = Some(index);
                        }
                    }
                    _ => {}
                }
            }

            if c == ' ' {
                space = true;
            } else {
                space = false;
            }
        } else {
            break current_index;
        }
    } {
        // SAFETY: This split is done on a guaranteed char boundary, because the index was obtained
        // from `title.char_indices()`. This optimization allows skipping the unnecessary char
        // boundary checks.
        unsafe {
            let title_vec = title.as_mut_vec();
            let subtitle_vec = title_vec.split_off(index);
            // Remove the whitespace character (which is a single byte).
            title.pop();
            (title, Some(String::from_utf8_unchecked(subtitle_vec)))
        }
    } else {
        // No subtitle to split off.
        (title, None)
    }
}

pub(in crate::song) fn combine_title_and_subtitle(title: String, subtitle: String) -> String {
    macro_rules! find_prefix {
        ($chars:ident, $pattern:pat $(, $tab:literal)?) => {{
            let mut space = false;
            loop {
                if let Some(c) = $chars.next() {
                    match c {
                        ' ' => {
                            space = true;
                        }
                        $(
                            $tab => {
                                break true;
                            }
                        )?
                        $pattern => {
                            if space {
                                break true;
                            }
                            space = false;
                        }
                        _ => {
                            space = false;
                        }
                    }
                } else {
                    break false;
                }
            }
        }};
    }

    // This complicated tree is meant to perform the inverse of the `split_title_and_subtitle()`
    // function above. It ensures, to the best it can, that the output here will again be split
    // into the same title and subtitle pair.
    //
    // Note that this logic doesn't both about other tab characters being before the split. It only
    // checks for tabs after the split (in the subtitle). If there are tabs in the title, then it
    // just isn't possible to combine in a reversible way.
    let mut title_chars = title.chars();
    let mut subtitle_chars = subtitle.chars();
    if match subtitle_chars.next() {
        Some('-') => find_prefix!(title_chars, '-') || subtitle.contains('\t'),
        Some('~') => {
            find_prefix!(title_chars, '-' | '~') || find_prefix!(subtitle_chars, '-', '\t')
        }
        Some('(') => {
            find_prefix!(title_chars, '-' | '~' | '(')
                || find_prefix!(subtitle_chars, '-' | '~', '\t')
        }
        Some('[') => {
            find_prefix!(title_chars, '-' | '~' | '(' | '[')
                || find_prefix!(subtitle_chars, '-' | '~' | '(', '\t')
        }
        _ => true,
    } {
        format!("{}\t{}", title, subtitle)
    } else {
        format!("{} {}", title, subtitle)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_title_and_subtitle_no_subtitle() {
        assert_eq!(
            split_title_and_subtitle("foo".to_owned()),
            ("foo".to_owned(), None)
        );
    }

    #[test]
    fn split_title_and_subtitle_at_tab() {
        assert_eq!(
            split_title_and_subtitle("foo\tbar".to_owned()),
            ("foo".to_owned(), Some("bar".to_owned()))
        );
    }

    #[test]
    fn split_title_and_subtitle_at_dash() {
        assert_eq!(
            split_title_and_subtitle("foo -bar-".to_owned()),
            ("foo".to_owned(), Some("-bar-".to_owned()))
        );
    }

    #[test]
    fn split_title_and_subtitle_at_tilde() {
        assert_eq!(
            split_title_and_subtitle("foo ~bar~".to_owned()),
            ("foo".to_owned(), Some("~bar~".to_owned()))
        );
    }

    #[test]
    fn split_title_and_subtitle_at_parenthesis() {
        assert_eq!(
            split_title_and_subtitle("foo (bar)".to_owned()),
            ("foo".to_owned(), Some("(bar)".to_owned()))
        );
    }

    #[test]
    fn split_title_and_subtitle_at_bracket() {
        assert_eq!(
            split_title_and_subtitle("foo [bar]".to_owned()),
            ("foo".to_owned(), Some("[bar]".to_owned()))
        );
    }

    #[test]
    fn split_title_and_subtitle_prefer_tab() {
        assert_eq!(
            split_title_and_subtitle("foo [bar (baz ~qux -quux\tquuz".to_owned()),
            (
                "foo [bar (baz ~qux -quux".to_owned(),
                Some("quuz".to_owned())
            )
        );
    }

    #[test]
    fn split_title_and_subtitle_prefer_dash() {
        assert_eq!(
            split_title_and_subtitle("foo [bar (baz ~qux -quux".to_owned()),
            ("foo [bar (baz ~qux".to_owned(), Some("-quux".to_owned()))
        );
    }

    #[test]
    fn split_title_and_subtitle_prefer_tilde() {
        assert_eq!(
            split_title_and_subtitle("foo [bar (baz ~qux".to_owned()),
            ("foo [bar (baz".to_owned(), Some("~qux".to_owned()))
        );
    }

    #[test]
    fn split_title_and_subtitle_prefer_parenthesis() {
        assert_eq!(
            split_title_and_subtitle("foo [bar (baz".to_owned()),
            ("foo [bar".to_owned(), Some("(baz".to_owned()))
        );
    }

    #[test]
    fn combine_title_and_subtitle_tab() {
        assert_eq!(
            combine_title_and_subtitle("foo".to_owned(), "bar".to_owned()),
            "foo\tbar".to_owned()
        );
    }

    #[test]
    fn combine_title_and_subtitle_dash() {
        assert_eq!(
            combine_title_and_subtitle("foo".to_owned(), "-bar".to_owned()),
            "foo -bar".to_owned()
        );
    }

    #[test]
    fn combine_title_and_subtitle_tilde() {
        assert_eq!(
            combine_title_and_subtitle("foo".to_owned(), "~bar".to_owned()),
            "foo ~bar".to_owned()
        );
    }

    #[test]
    fn combine_title_and_subtitle_parenthesis() {
        assert_eq!(
            combine_title_and_subtitle("foo".to_owned(), "(bar".to_owned()),
            "foo (bar".to_owned()
        );
    }

    #[test]
    fn combine_title_and_subtitle_bracket() {
        assert_eq!(
            combine_title_and_subtitle("foo".to_owned(), "[bar".to_owned()),
            "foo [bar".to_owned()
        );
    }
}
