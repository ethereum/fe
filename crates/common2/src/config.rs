// use camino::Utf8PathBuf;
// use indexmap::IndexSet;
// use serde::{Deserialize, Serialize};
// use toml::Table;

// use crate::resolver::ingot::IngotDesc;

// #[derive(Deserialize, Serialize)]
// pub struct Config {
//     pub modes: Table,
//     pub base_registry: Table,
// }

// impl Config {
//     pub fn new() -> Self {
//         Self {
//             base_registry: Table::new(),
//         }
//     }

//     pub fn base_registry(&self) -> IndexSet<IngotDesc> {
//         self.base_registry
//             .iter()
//             .map(|(name, value)| IngotDesc::from(name, value))
//             .collect()
//     }
// }

// pub fn load_config(path: Utf8PathBuf) -> Config {
//     let text = std::fs::read_to_string(path).unwrap();
//     toml::from_str(&text).unwrap()
// }

// pub fn write_config(path: Utf8PathBuf, config: Config) {
//     let text = toml::to_string(&config).unwrap();
//     std::fs::write(path, text).unwrap();
// }
