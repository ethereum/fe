use url::Url;

#[derive(Debug)]
pub enum UrlExtError {
    DirectoryRangeError,
}

trait UrlExt {
    fn parent(&self) -> Option<Url>;
    fn directory(&self) -> Option<Url>;
    fn directory_range(&self) -> Result<(Url, Url), UrlExtError>;
}

impl UrlExt for Url {
    fn directory(&self) -> Option<Url> {
        if self.cannot_be_a_base() {
            return None;
        };
        let mut url = self.clone();

        if url.path().ends_with('/') {
            return Some(url);
        }

        if let Ok(mut segments) = url.path_segments_mut() {
            segments.pop();
            segments.push("");
        }

        Some(url)
    }

    fn parent(&self) -> Option<Url> {
        let directory = self.directory()?;

        if *self != directory {
            // If we're not already at a directory, return the directory
            Some(directory)
        } else if self.path() == "/" {
            return None;
        } else {
            // We're already at a directory, go up one level
            let mut parent = self.clone();
            if let Ok(mut segments) = parent.path_segments_mut() {
                segments.pop();
                segments.pop();
            }
            return Some(parent);
        }
    }

    fn directory_range(&self) -> Result<(Url, Url), UrlExtError> {
        let left = self.directory().ok_or(UrlExtError::DirectoryRangeError)?;
        let mut right = left.clone();

        // If at root level
        if left.path() == "/" {
            // Increment the host
            if let Some(mut host) = right.host().map(|h| h.to_string()) {
                if let Some(last) = host.pop() {
                    host.push((last as u8 + 1) as char);
                }
                right
                    .set_host(Some(&host))
                    .map_err(|_| UrlExtError::DirectoryRangeError)?;
            } else {
                return Err(UrlExtError::DirectoryRangeError);
            }
        } else {
            // For a directory URL like "https://example.com/foo/bar/"
            // We want to increment "bar" to "bas"
            let path = left.path();

            // Find the last segment by looking for the last '/' that isn't at the end
            if let Some(last_slash_pos) = path[..path.len() - 1].rfind('/') {
                let segment_to_increment = &path[last_slash_pos + 1..path.len() - 1];
                let incremented = increment_string(segment_to_increment);

                // Build new path
                let new_path = format!("{}{}/", &path[..last_slash_pos + 1], incremented);
                right.set_path(&new_path);
            } else {
                return Err(UrlExtError::DirectoryRangeError);
            }
        }

        Ok((left, right))
    }
}

/// lexicographically increment a string
fn increment_string(s: &str) -> String {
    let mut result = s.to_string();
    if let Some(last) = result.pop() {
        result.push((last as u8 + 1) as char);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_directory_basic() {
        let url = Url::parse("https://example.com/foo/bar/baz").unwrap();
        let directory = url.directory();
        assert!(directory.is_some());
        assert_eq!(directory.unwrap().as_str(), "https://example.com/foo/bar/");

        let url = Url::parse("https://example.com/foo/bar/baz/").unwrap();
        let directory = url.directory();
        assert!(directory.is_some());
        assert_eq!(
            directory.unwrap().as_str(),
            "https://example.com/foo/bar/baz/"
        );
    }

    #[test]
    fn test_parent_basic() {
        let url = Url::parse("https://example.com/foo/bar").unwrap();
        let parent = url.parent();
        assert!(parent.is_some());
        assert_eq!(parent.unwrap().as_str(), "https://example.com/foo/");

        let url = Url::parse("https://example.com/foo/").unwrap();
        let parent = url.parent();
        assert!(parent.is_some());
        assert_eq!(parent.unwrap().as_str(), "https://example.com/");

        let url = Url::parse("https://example.com/").unwrap();
        let parent = url.parent();
        assert!(
            parent.is_none(),
            "Parent should be `None` but instead we got {:?}",
            parent
        );
    }

    #[test]
    fn test_increment_string() {
        assert_eq!(increment_string("a"), "b");
        assert_eq!(increment_string("z"), "{");
        assert_eq!(increment_string("9"), ":");
        assert_eq!(increment_string("foo"), "fop");
        assert_eq!(increment_string("bar"), "bas");
        assert_eq!(increment_string(""), "");
    }

    #[test]
    fn test_directory_range_basic() {
        let url = Url::parse("https://example.com/foo/bar/").unwrap();
        let (left, right) = url.directory_range().unwrap();
        assert_eq!(left.as_str(), "https://example.com/foo/bar/");
        assert_eq!(right.as_str(), "https://example.com/foo/bas/");

        // Test with nested directory
        let url = Url::parse("https://example.com/foo/bar/baz/").unwrap();
        let (left, right) = url.directory_range().unwrap();
        assert_eq!(left.as_str(), "https://example.com/foo/bar/baz/");
        assert_eq!(right.as_str(), "https://example.com/foo/bar/bba/");
    }

    #[test]
    fn test_directory_range_root() {
        let url = Url::parse("https://example.com/").unwrap();
        let (left, right) = url.directory_range().unwrap();
        assert_eq!(left.as_str(), "https://example.com/");
        assert_eq!(right.as_str(), "https://example.con/");
    }

    #[test]
    fn test_directory_range_non_directory() {
        let url = Url::parse("https://example.com/foo/bar").unwrap();
        let (left, right) = url.directory_range().unwrap();
        assert_eq!(left.as_str(), "https://example.com/foo/");
        assert_eq!(right.as_str(), "https://example.com/fop/");
    }

    #[test]
    fn test_directory_range_error() {
        let url = Url::parse("data:text/plain,Hello").unwrap();
        let result = url.directory_range();
        assert!(result.is_err());
    }
}
