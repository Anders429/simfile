use crate::{parse::msd::ParameterList, Song};

pub(crate) fn interpret<I>(parameter_lists: I) -> Result<Song, ()>
where
    I: Iterator<Item = ParameterList>,
{
    Ok(Song::default())
}

#[cfg(test)]
mod tests {
    use super::interpret;
    use crate::{parse::msd::ParameterList, Song};
    use claim::assert_ok_eq;
    use std::iter;

    #[test]
    fn empty() {
        assert_ok_eq!(interpret(iter::empty()), Song::default());
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
}
