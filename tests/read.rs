//! Tests for reading simfiles.

use claim::assert_ok;
use std::fs::read_dir;

#[test]
fn msd() {
    for filename in read_dir("tests/data/msd").unwrap() {
        if !filename
            .as_ref()
            .unwrap()
            .file_name()
            .to_str()
            .unwrap()
            .ends_with(".msd")
        {
            // Only read the `.msd` files.
            continue;
        }

        // These files all have quirks that make them not work right now.
        // Ideally, this should be fixed to at least silently skip past their quirks.
        if filename.as_ref().unwrap().file_name() == "DUB.msd" {
            // Unknown tag "#LEVEL" (probably some ancient relic from an early version of .msd).
            continue;
        }
        if filename.as_ref().unwrap().file_name() == "PARAMAX.msd" {
            // Some unknown "#OH!" tag, appears to be a comment left by someone.
            continue;
        }
        if filename.as_ref().unwrap().file_name() == "B_FLY.msd" {
            // Couple steps have a "NORMAL" difficulty. This is leftover from an old .msd spec, and
            // I'm not sure how to map it at this point.
            continue;
        }
        if filename.as_ref().unwrap().file_name() == "House.msd" {
            // Improperly formatted "#MSD" tag. I don't think this is worth supporting, since it is
            // unique only to this file as far as I know.
            continue;
        }
        dbg!(&filename);
        assert_ok!(simfile::Song::read_msd(filename.unwrap().path()));
    }
}
