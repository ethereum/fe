#[macro_use]
#[allow(unused_macros)]
#[cfg(target_arch = "wasm32")]
macro_rules! assert_snapshot_wasm {
    ($module:ident, $name:ident, $actual:expr) => {
        let snap = include_str!(concat!(
            "snapshots/fe_compiler_tests__",
            stringify!($module),
            "__",
            stringify!($name),
            ".snap"
        ));
        let (_, expected) = snap.rsplit_once("---\n").unwrap();
        pretty_assertions::assert_eq!($actual.trim(), expected.trim());
    };
}

#[macro_use]
#[allow(unused_macros)]
#[cfg(not(target_arch = "wasm32"))]
macro_rules! assert_snapshot_wasm {
    ($module:ident, $name:ident, $actual:expr) => {};
}

#[cfg(test)]
mod analysis;
#[cfg(test)]
mod demo_erc20;
#[cfg(test)]
mod demo_guestbook;
#[cfg(test)]
mod demo_uniswap;
#[cfg(test)]
mod features;
#[cfg(test)]
mod runtime;
#[cfg(test)]
mod solidity;
#[cfg(test)]
mod stress;
#[cfg(test)]
mod yulgen;
