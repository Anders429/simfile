# simfile

[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/Anders429/simfile/tests)](https://github.com/Anders429/simfile/actions)
[![codecov.io](https://img.shields.io/codecov/c/gh/Anders429/simfile)](https://codecov.io/gh/Anders429/simfile)
[![crates.io](https://img.shields.io/crates/v/simfile)](https://crates.io/crates/simfile)
[![docs.rs](https://docs.rs/simfile/badge.svg)](https://docs.rs/simfile)
[![License](https://img.shields.io/crates/l/simfile)](#license)

Reading, writing, manipulating, and converting simfiles.

A simfile is a file used in a rhythm game simulator. This library provides tools for reading them,
creating them, modifying them, and transcoding between them. The intention is to have a one-stop
shop for working with all formats. A number of common simfile types are supported, including:

- `.msd`
- (More planned for future versions)

## Usage
This library provides a `Song` type, which stores the data for a single simfile song. A `Song` can
be read directly from an existing simfile of any supported type.

### Example
Reading a `Song` from a `.msd` simfile is done as follows:

``` rust
use simfile::Song;

let song = Song::read_msd("simfile.msd").expect("error while reading `.msd` file");
```

Similarly, writing to a `.msd` simfile is done from an existing `Song` as follows:

``` rust
song.write_msd("simfile.msd").expect("error while writing `.msd` file");
```

Note that both `Song::read_msd()` and `Song::write_msd()` return `Result` types that must be
handled appropriately.

## License
This project is licensed under either of

* Apache License, Version 2.0
([LICENSE-APACHE](https://github.com/Anders429/simfile/blob/HEAD/LICENSE-APACHE) or
http://www.apache.org/licenses/LICENSE-2.0)
* MIT license
([LICENSE-MIT](https://github.com/Anders429/simfile/blob/HEAD/LICENSE-MIT) or
http://opensource.org/licenses/MIT)

at your option.

### Contribution
Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
