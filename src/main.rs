#![feature(external_doc)]
#![doc(include = "../README.md")]

#[cfg(feature = "solc-backend")]
mod _utils;
#[cfg(feature = "solc-backend")]
mod main_full;

#[cfg(not(feature = "solc-backend"))]
fn main() {
    println!("Using the CLI requires passing `--features solc-backend`");
}

// This is moved to its own file so that we don't have to sprinkle the code with
// linting exceptions
#[cfg(feature = "solc-backend")]
fn main() {
    main_full::main();
}
