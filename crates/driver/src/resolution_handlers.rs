use camino::Utf8PathBuf;
use common::InputFile;
use resolver::{Graph, ResolutionHandler};

use crate::DriverDataBase;

impl ResolutionHandler<Utf8PathBuf, String, InputFile> for DriverDataBase {
    fn already_handled(&self, description: Utf8PathBuf) -> bool {
        todo!()
    }

    fn handle_resolution(&mut self, description: Utf8PathBuf, resource: String) -> InputFile {
        todo!()
    }
}

impl ResolutionHandler<(), Graph<InputFile>, ()> for DriverDataBase {
    fn already_handled(&self, description: ()) -> bool {
        todo!()
    }

    fn handle_resolution(&mut self, description: (), resource: Graph<InputFile>) -> () {
        todo!()
    }
}
