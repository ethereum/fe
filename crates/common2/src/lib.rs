pub mod diagnostics;
pub mod input;
pub mod recursive_def;

pub use input::{InputFile, InputIngot};

#[salsa::jar(db = InputDb)]
pub struct Jar(InputIngot, InputFile);

pub trait InputDb: salsa::DbWithJar<Jar> {
    fn as_input_db(&self) -> &dyn InputDb {
        <Self as salsa::DbWithJar<Jar>>::as_jar_db::<'_>(self)
    }
}
impl<DB> InputDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}
