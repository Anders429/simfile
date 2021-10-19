use crate::{parse::msd::ParameterList, Song};

pub(crate) fn interpret<I>(parameter_lists: I) -> Result<Song, ()>
where
    I: Iterator<Item = ParameterList>,
{
    todo!()
}
