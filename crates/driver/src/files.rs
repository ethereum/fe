use camino::Utf8PathBuf;

pub const FE_TOML: &str = "fe.toml";

pub fn find_project_root() -> Option<Utf8PathBuf> {
    let mut path = Utf8PathBuf::from_path_buf(
        std::env::current_dir().expect("Unable to get current directory"),
    )
    .expect("Expected utf8 path");

    loop {
        let fe_toml = path.join(FE_TOML);
        if fe_toml.is_file() {
            return Some(path);
        }

        if !path.pop() {
            break;
        }
    }

    None
}
