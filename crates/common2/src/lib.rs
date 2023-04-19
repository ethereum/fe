pub mod diagnostics;
pub mod input;

pub use input::{InputFile, InputIngot};

#[salsa::jar(db = InputDb)]
pub struct Jar(InputIngot, InputFile);

pub trait InputDb: salsa::DbWithJar<Jar> {}
impl<DB> InputDb for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}
