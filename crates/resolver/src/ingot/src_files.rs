use super::IngotDesc;
use crate::Resolver;
use camino::Utf8PathBuf;
use indexmap::IndexMap;
use std::fs;
use std::hash::Hash;
use std::io;

#[derive(Debug)]
pub enum SrcFilesResolutionError {
    FileNotFound,
    DirectoryNotFound,
    FileReadError(io::Error),
}

pub struct SrcFilesResolver<FR> {
    files_resolver: FR,
}

impl<FR> Resolver for SrcFilesResolver<FR>
where
    FR: Resolver,
    FR::ResourceDesc: Hash,
    FR::Resource: AsRef<Utf8PathBuf>,
{
    type Config = ();
    type ResourceDesc = IngotDesc<FR::ResourceDesc>;
    type Resource = IndexMap<Utf8PathBuf, Vec<u8>>;
    type ResolutionError = SrcFilesResolutionError;

    fn from_config(_config: &Self::Config) -> Self {
        todo!()
    }

    fn resolve(
        &self,
        desc: &IngotDesc<FR::ResourceDesc>,
    ) -> Result<IndexMap<Utf8PathBuf, Vec<u8>>, SrcFilesResolutionError> {
        let src_dir = self
            .files_resolver
            .resolve(&desc.files_desc)
            .map_err(|_| SrcFilesResolutionError::FileNotFound)?
            .as_ref()
            .join("src");

        if !src_dir.exists() || !src_dir.is_dir() {
            return Err(SrcFilesResolutionError::DirectoryNotFound);
        }

        let mut file_map = IndexMap::new();

        collect_files_and_contents(&src_dir, &mut file_map)?;

        Ok(file_map)
    }
}

fn collect_files_and_contents(
    dir: &Utf8PathBuf,
    file_map: &mut IndexMap<Utf8PathBuf, Vec<u8>>,
) -> Result<(), SrcFilesResolutionError> {
    for entry in fs::read_dir(dir).map_err(SrcFilesResolutionError::FileReadError)? {
        let entry = entry.map_err(SrcFilesResolutionError::FileReadError)?;
        let path = Utf8PathBuf::from_path_buf(entry.path()).map_err(|_| {
            SrcFilesResolutionError::FileReadError(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid UTF-8 path",
            ))
        })?;

        if path.is_dir() {
            collect_files_and_contents(&path, file_map)?;
        } else if path.is_file() {
            let content = fs::read(&path).map_err(SrcFilesResolutionError::FileReadError)?;
            file_map.insert(path, content);
        }
    }

    Ok(())
}
