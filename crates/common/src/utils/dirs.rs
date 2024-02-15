use std::path::PathBuf;

pub fn get_fe_home() -> PathBuf {
    let fe_home = std::env::var("FE_HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            dirs::home_dir()
                .expect("Failed to get home dir")
                .join(".fe")
        });

    if !fe_home.exists() {
        std::fs::create_dir_all(&fe_home).expect("Failed to create FE_HOME");
    }

    fe_home
}

pub fn get_fe_deps() -> PathBuf {
    let fe_deps = get_fe_home().join("deps");

    if !fe_deps.exists() {
        std::fs::create_dir_all(&fe_deps).expect("Failed to create FE_DEPS");
    }
    fe_deps
}
