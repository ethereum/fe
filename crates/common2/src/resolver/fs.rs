use camino::Utf8PathBuf;
use serde::Deserialize;

use super::Resolver;

#[derive(Deserialize)]
pub struct DirPath {
    pub path: String,
}

pub struct DirReader {
    path: Utf8PathBuf,
}

pub struct LazyDirResolver;

impl Resolver for LazyDirResolver {
    type ResourceDesc = DirPath;
    type Resource = DirReader;
    type ResolutionError = String;

    fn resolve(&self, desc: &DirPath) -> Result<DirReader, String> {
        if let Ok(path) = Utf8PathBuf::try_from(desc.path.clone()) {
            if !path.exists() {
                Err("Path does not exist".to_string())
            } else {
                Ok(DirReader { path })
            }
        } else {
            Err("Invalid path".to_string())
        }
    }
}

// fn collect_file_paths(
//     path: &Utf8PathBuf,
//     files: &mut IndexSet<Utf8PathBuf>,
// ) -> std::io::Result<()> {
//     if path.is_dir() {
//         for entry in path.read_dir()? {
//             let entry = entry?;
//             let path = Utf8PathBuf::from_path_buf(entry.path())
//                 .expect("could not covert PathBuf to Utf8PathBuf");

//             if path.is_dir() {
//                 collect_file_paths(&path, files)?;
//             } else {
//                 files.insert(path);
//             }
//         }
//     }
//     Ok(())
// }
