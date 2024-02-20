use std::env::VarError;

use camino::Utf8PathBuf;

pub struct HomeDir(Utf8PathBuf);

impl HomeDir {
    pub fn load() -> Self {
        match std::env::var("HOME") {
            Ok(home) => HomeDir(Utf8PathBuf::from(home)),
            Err(VarError::NotPresent) => HomeDir(Utf8PathBuf::from("~/.fe")),
            Err(_) => panic!("no unicode"),
        }
    }

    pub fn path(&self) -> Utf8PathBuf {
        self.0.clone()
    }
}
