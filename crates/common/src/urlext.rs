use url::Url;

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
        let mut segments = left
            .path_segments()
            .ok_or(UrlExtError::DirectoryRangeError)?;

        if let Some(final_segment) = segments.next_back() {
            // if there's a final segment we aren't at the root level yet
            let mut right_segments = right
                .path_segments_mut()
                .map_err(|_| UrlExtError::DirectoryRangeError)?;
            let incremented_final_segment = increment_string(final_segment);
            right_segments
                .pop()
                .push(incremented_final_segment.as_str());
            drop(right_segments);
            return Ok((left, right));
        } else if let Some(mut host) = right.host().map(|h| h.to_string()) {
            // we must have been at the root level...
            // so we'll need to increment the last character of the host
            if let Some(last) = host.to_string().pop() {
                host.push((last as u8 + 1) as char);
            }
            match right.set_host(Some(host.as_str())) {
                Ok(_) => return Ok((left, right)),
                Err(_) => return Err(UrlExtError::DirectoryRangeError),
            }
        } else {
            Err(UrlExtError::DirectoryRangeError)
        }
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
}
