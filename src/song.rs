/// Situations in which a song is selectable.
///
/// Not all of these options are supported in every simfile format. The `Roulette`, `ExtraStage`,
/// `OneMoreExtraStage`, and `OnRemainingStages` variants seem to originate from Stepmania 3.9+, an
/// old unofficial fork of Stepmania 3.9. However, they are *technically* supported in Stepmania 5,
/// within both the `sm` and `ssc` file formats, to the extent that the values are simply
/// interpreted as `Always`. For data preservation reasons, they are included here.
pub enum Selectable {
    /// Selectable.
    Always,
    /// Not selectable.
    Never,
    /// Can only be selected by roulette.
    Roulette,
    /// Can only be selected on an extra stage.
    ExtraStage,
    /// Can only be selected on a second extra stage.
    OneMoreExtraStage,
    /// Can only be selected when there are the provided number of stages remaining.
    ///
    /// # Example
    /// To make a song selectable only on the final stage, use a value of `1`:
    ///
    /// ```
    /// use simfile::song::Selectable;
    ///
    /// let final_stage = Selectable::OnRemainingStages(1);
    /// ```
    OnRemainingStages(u8),
}

pub struct Song {
    pub title: Option<String>,
    pub subtitle: Option<String>,
    pub artist: Option<String>,

    /// Selectability during song selection.
    pub selectable: Option<Selectable>,
}
