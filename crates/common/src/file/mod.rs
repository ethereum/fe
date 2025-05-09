mod index;
mod workspace;

use index::FileIndex;

#[salsa::input(constructor = __new_impl)]
#[derive(Debug)]
pub struct File {
    #[return_ref]
    pub text: String,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy, PartialOrd, Ord)]
pub struct IndexedFile {
    pub(super) index: FileIndex,
    pub(super) file: File,
}

#[salsa::tracked]
impl IndexedFile {
    // pub fn touch_with_initial_content(
    //     db: &mut dyn InputDb,
    //     index: FileIndex,
    //     url: Url,
    //     initial_content: Option<String>,
    // ) -> IndexedFile {
    //     // Check if the file already exists
    //     // Return file if it already exists, wrapped in Some
    //     if let Some(file) = index.get(db, &url) {
    //         return IndexedFile { index, file };
    //     }
    //     let initial = initial_content.unwrap_or_default();

    //     // Create the InputFile first with an immutable borrow
    //     let input_file = File::__new_impl(db, initial);
    //     // Then set it with the mutable borrow
    //     index
    //         .set(db, url, input_file)
    //         .expect("Failed to create file");
    //     IndexedFile {
    //         index,
    //         file: input_file,
    //     }
    // }
}
