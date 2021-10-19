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
}
