use include_dir::{include_dir, Dir};

pub const STD: Dir = include_dir!("$CARGO_MANIFEST_DIR/std");
