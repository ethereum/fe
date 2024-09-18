use std::ops::Range;

use camino::Utf8Path;
use codespan_reporting as cs;
use common::InputFile;
use cs::files as cs_files;

use crate::db::{LanguageServerDatabase, LanguageServerDb};

#[salsa::tracked(return_ref)]
pub fn file_line_starts(db: &dyn LanguageServerDb, file: InputFile) -> Vec<usize> {
    cs::files::line_starts(file.text(db.as_input_db())).collect()
}

impl<'a> cs_files::Files<'a> for LanguageServerDatabase {
    type FileId = InputFile;
    type Name = &'a Utf8Path;
    type Source = &'a str;

    fn name(&'a self, file_id: Self::FileId) -> Result<Self::Name, cs_files::Error> {
        Ok(file_id.path(self).as_path())
    }

    fn source(&'a self, file_id: Self::FileId) -> Result<Self::Source, cs_files::Error> {
        Ok(file_id.text(self))
    }

    fn line_index(
        &'a self,
        file_id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, cs_files::Error> {
        let starts = file_line_starts(self, file_id);
        Ok(starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(
        &'a self,
        file_id: Self::FileId,
        line_index: usize,
    ) -> Result<Range<usize>, cs_files::Error> {
        let line_starts = file_line_starts(self, file_id);

        let start = *line_starts
            .get(line_index)
            .ok_or(cs_files::Error::LineTooLarge {
                given: line_index,
                max: line_starts.len() - 1,
            })?;

        let end = if line_index == line_starts.len() - 1 {
            file_id.text(self).len()
        } else {
            *line_starts
                .get(line_index + 1)
                .ok_or(cs_files::Error::LineTooLarge {
                    given: line_index,
                    max: line_starts.len() - 1,
                })?
        };

        Ok(Range { start, end })
    }
}
