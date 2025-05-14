// use std::default;
//
// use camino::Utf8PathBuf;
// use common::config::IngotArguments;
// use resolver::{
//     files::FilesResolver,
//     graph::{DiGraph, GraphResolver},
//     ingot::IngotGraphResolver,
//     ResolutionHandler, Resolver,
// };
//
// #[derive(Default)]
// struct IngotFilesHandler {
//     config_files: Vec<()>,
//     source_files: Vec<()>,
// }
//
// impl ResolutionHandler<FilesResolver> for IngotFilesHandler {
//     type Item = Vec<(Utf8PathBuf, ())>;
//
//     fn handle_resolution(
//         &mut self,
//         ingot_path: &Utf8PathBuf,
//         ingot_files: Vec<(Utf8PathBuf, String)>,
//     ) -> Self::Item {
//         for (file_path, file_content) in ingot_files {
//             if file_path.ends_with("fe.toml") {
//                 self.config_files.push(());
//             }
//         }
//         return vec![];
//     }
// }
//
// #[derive(Default)]
// struct IngotGraphHandler;
//
// impl<GR> ResolutionHandler<GR> for IngotGraphHandler
// where
//     GR: GraphResolver<FilesResolver, IngotFilesHandler, Option<IngotArguments>>,
// {
//     type Item = ();
//
//     fn handle_resolution(
//         &mut self,
//         root_ingot_path: &<GR as Resolver>::Description,
//         ingot_graph: <GR as Resolver>::Resource,
//     ) -> Self::Item {
//         todo!()
//     }
// }
//
// pub fn check(path: &Utf8PathBuf) {
//     // let mut files_handler = IngotFilesHandler::default();
//     let mut graph_resolver = IngotGraphResolver::<IngotFilesHandler>::default();
//     let mut graph_handler = IngotGraphHandler::default();
//
//     let graph = graph_resolver.resolve();
// }
