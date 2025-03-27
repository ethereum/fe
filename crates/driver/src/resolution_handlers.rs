use camino::Utf8PathBuf;
use common::InputFile;
use resolver::{Graph, ResolutionHandler};

use crate::DriverDataBase;

impl ResolutionHandler<Utf8PathBuf, Vec<(Utf8PathBuf, String)>, InputFile> for DriverDataBase {
    fn handle_resolution(
        &mut self,
        description: Utf8PathBuf,
        resource: Vec<(Utf8PathBuf, String)>,
    ) -> InputFile {
        todo!()
    }
}

impl ResolutionHandler<InputFile, Graph<InputFile>, ()> for DriverDataBase {
    fn handle_resolution(&mut self, description: InputFile, resource: Graph<InputFile>) -> () {
        todo!()
    }
}
