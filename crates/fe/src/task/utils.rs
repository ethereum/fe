use walkdir::WalkDir;

pub fn load_files_from_dir(dir_path: &str) -> Result<Vec<(String, String)>, std::io::Error> {
    let entries = WalkDir::new(dir_path);
    let mut files = vec![];
    for entry in entries.into_iter() {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() && path.extension().and_then(std::ffi::OsStr::to_str) == Some("fe") {
            let content = std::fs::read_to_string(path)?;
            files.push((path.to_string_lossy().to_string(), content));
        }
    }
    Ok(files)
}
