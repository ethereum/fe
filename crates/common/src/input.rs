use rust_embed::Embed;

#[derive(Embed)]
#[folder = "../../library/core"]
struct Core;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IngotKind {
    /// A standalone ingot is a dummy ingot when the compiler is invoked
    /// directly on a file.
    StandAlone,

    /// A local ingot which is the current ingot being compiled.
    Local,

    /// An external ingot which is depended on by the current ingot.
    External,

    /// Core library ingot.
    Core,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IngotFileKind {
    /// A source file containing Fe code.
    Source,

    /// A configuration file for the ingot.
    Config,
}

// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// pub struct IngotDependency {
//     /// The ingot may have a alias name from the original ingot name.
//     pub name: SmolStr,
//     /// An ingot which the current ingot depends on.
//     pub ingot: InputIngot,
// }
// impl IngotDependency {
//     pub fn new(name: &str, ingot: InputIngot) -> Self {
//         Self {
//             name: SmolStr::new(name),
//             ingot,
//         }
//     }
// }

// impl PartialOrd for IngotDependency {
//     fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
//         Some(self.ingot.cmp(&other.ingot))
//     }
// }
// impl Ord for IngotDependency {
//     fn cmp(&self, other: &Self) -> std::cmp::Ordering {
//         self.ingot.cmp(&other.ingot)
//     }
// }

pub type Version = semver::Version;
