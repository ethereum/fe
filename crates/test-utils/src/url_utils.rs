#[cfg(target_arch = "wasm32")]
use std::path::Path;
#[cfg(target_arch = "wasm32")]
use url::Url;

#[cfg(target_arch = "wasm32")]
pub trait UrlExt {
    fn from_file_path<P: AsRef<Path>>(path: P) -> Result<Url, ()>;
    fn from_directory_path<P: AsRef<Path>>(path: P) -> Result<Url, ()>;
}

#[cfg(target_arch = "wasm32")]
impl UrlExt for Url {
    fn from_file_path<P: AsRef<Path>>(path: P) -> Result<Url, ()> {
        let path_str = path.as_ref().to_string_lossy();
        let url_str = if path_str.starts_with('/') {
            format!("file://{}", path_str)
        } else {
            format!("file:///{}", path_str)
        };
        Url::parse(&url_str).map_err(|_| ())
    }

    fn from_directory_path<P: AsRef<Path>>(path: P) -> Result<Url, ()> {
        let path_str = path.as_ref().to_string_lossy();
        let url_str = if path_str.starts_with('/') {
            if path_str.ends_with('/') {
                format!("file://{}", path_str)
            } else {
                format!("file://{}/", path_str)
            }
        } else {
            if path_str.ends_with('/') {
                format!("file:///{}", path_str)
            } else {
                format!("file:///{}/", path_str)
            }
        };
        Url::parse(&url_str).map_err(|_| ())
    }
}
