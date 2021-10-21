use crate::{internal_log, parse::msd::ParameterList, Song};

/// Constants for all valid `msd` tags.
mod tags {
    pub(super) const TITLE: &str = "title";
    pub(super) const ARTIST: &str = "artist";
}

pub(crate) fn interpret<I>(parameter_lists: I) -> Result<Song, ()>
where
    I: Iterator<Item = ParameterList>,
{
    let mut song = Song::default();

    for parameter_list in parameter_lists {
        match parameter_list {
            ParameterList::Tagged(mut list) => {
                if list.is_empty() {
                    internal_log::error!("empty tagged parameter list");
                    return Err(());
                }
                let tag = unsafe {
                    // SAFETY: Just checked that `list` is nonempty.
                    list.get_unchecked_mut(0)
                };
                tag.make_ascii_lowercase();
                match tag.as_str() {
                    tags::TITLE => {
                        if list.len() != 2 {
                            internal_log::error!(
                                "expected 1 parameter for title, found {}",
                                list.len() - 1
                            );
                            return Err(());
                        }
                        let title = unsafe {
                            // SAFETY: Just checked that `list` has exactly 2 elements.
                            list.get_unchecked(1)
                        };
                        song.title = Some(title.clone());
                    }
                    tags::ARTIST => {
                        if list.len() != 2 {
                            internal_log::error!(
                                "expected 1 parameter for artist, found {}",
                                list.len() - 1
                            );
                            return Err(());
                        }
                        let artist = unsafe {
                            // SAFETY: Just checked that `list` has exactly 2 elements.
                            list.get_unchecked(1)
                        };
                        song.artist = Some(artist.clone());
                    }
                    _ => {
                        internal_log::warn!("skipping unrecognized tag {}", tag);
                    }
                }
            }
            ParameterList::Untagged(list) => {
                internal_log::error!(
                    "encountered untagged parameter list {:?}; untagged parameter lists are not used in `.msd` files",
                    list
                );
                return Err(());
            }
        }
    }

    Ok(song)
}

#[cfg(test)]
mod tests {
    use super::interpret;
    use crate::{parse::msd::ParameterList, Song};
    use claim::{assert_err, assert_ok_eq};
    use std::iter;

    #[test]
    fn empty() {
        assert_ok_eq!(interpret(iter::empty()), Song::default());
    }

    #[test]
    fn untagged_parameter_list_errors() {
        assert_err!(interpret(iter::once(ParameterList::Untagged(Vec::new()))));
    }

    #[test]
    fn empty_tag_errors() {
        assert_err!(interpret(iter::once(ParameterList::Tagged(Vec::new()))));
    }

    #[test]
    fn unrecognized_tag() {
        assert_ok_eq!(
            interpret(iter::once(ParameterList::Tagged(vec![
                "UNRECOGNIZED".to_owned()
            ]))),
            Song::default()
        );
    }

    #[test]
    fn title() {
        let mut expected = Song::default();
        expected.title = Some("foo".to_owned());

        assert_ok_eq!(
            interpret(iter::once(ParameterList::Tagged(vec![
                "TITLE".to_owned(),
                "foo".to_owned()
            ]))),
            expected
        );
    }

    #[test]
    fn title_too_many_parameters() {
        assert_err!(interpret(iter::once(ParameterList::Tagged(vec![
            "TITLE".to_owned(),
            "foo".to_owned(),
            "bar".to_owned(),
        ]))));
    }

    #[test]
    fn title_too_few_parameters() {
        assert_err!(interpret(iter::once(ParameterList::Tagged(vec![
            "TITLE".to_owned(),
        ]))));
    }

    #[test]
    fn artist() {
        let mut expected = Song::default();
        expected.artist = Some("foo".to_owned());

        assert_ok_eq!(
            interpret(iter::once(ParameterList::Tagged(vec![
                "ARTIST".to_owned(),
                "foo".to_owned()
            ]))),
            expected
        );
    }

    #[test]
    fn artist_too_many_parameters() {
        assert_err!(interpret(iter::once(ParameterList::Tagged(vec![
            "ARTIST".to_owned(),
            "foo".to_owned(),
            "bar".to_owned(),
        ]))));
    }

    #[test]
    fn artist_too_few_parameters() {
        assert_err!(interpret(iter::once(ParameterList::Tagged(vec![
            "ARTIST".to_owned(),
        ]))));
    }
}
