pub mod db;
pub mod diagnostics;
pub mod files;

use camino::Utf8PathBuf;
use common::{
    config::{Config, DependencyDescription, IngotArguments},
    file::workspace::Workspace,
    InputDb,
};
pub use db::DriverDataBase;
use resolver::{files::FilesResolver, ingot::ingot_graph_resolver, ResolutionHandler, Resolver};
use smol_str::SmolStr;
use url::Url;

// use camino::Utf8PathBuf;
// use common::core::HasBuiltinCore;
// use common::ingot::{IngotBaseUrl, IngotIndex};
// use common::InputDb;
// use test_utils::url_utils::UrlExt;

// use clap::{Parser, Subcommand};
// use hir::hir_def::TopLevelMod;
// use resolver::{
//     ingot::{source_files::SourceFiles, Ingot, IngotResolver},
//     Resolver,
// };
// use url::Url;

// pub fn run(opts: &Options) {
//     match &opts.command {
//         Command::Build => eprintln!("`fe build` doesn't work at the moment"),
//         Command::Check { path, core } => {
//             let mut db = DriverDataBase::default();
//             let mut ingot_resolver = IngotResolver::default();

//             let core_ingot = if let Some(core_path) = core {
//                 match ingot_resolver.resolve(core_path) {
//                     Ok(Ingot::Folder {
//                         config,
//                         source_files:
//                             Some(SourceFiles {
//                                 root: Some(_root),
//                                 files,
//                             }),
//                     }) => {
//                         let core_base_url = Url::parse("core-ingot:///").unwrap();
//                         let diagnostics = ingot_resolver.take_diagnostics();
//                         if !diagnostics.is_empty() {
//                             eprintln!("an error was encountered while resolving `{core_path}`");
//                             for diagnostic in diagnostics {
//                                 eprintln!("{diagnostic}")
//                             }
//                             std::process::exit(2)
//                         }
//                         let index = db.workspace();
//                         index.touch_ingot(
//                             &mut db,
//                             &core_base_url,
//                         );
//                         for (path, content) in files {
//                             core_base_url.touch(&mut db, path, Some(content));
//                         }
//                         core_base_url
//                     }
//                     Ok(Ingot::SingleFile { .. }) => {
//                         eprintln!("standalone core ingot not supported");
//                         std::process::exit(2)
//                     }
//                     Ok(_) => {
//                         eprintln!("an error was encountered while resolving `{core_path}`");
//                         for diagnostic in ingot_resolver.take_diagnostics() {
//                             eprintln!("{diagnostic}")
//                         }
//                         std::process::exit(2)
//                     }
//                     Err(error) => {
//                         eprintln!("an error was encountered while resolving `{core_path}`");
//                         eprintln!("{error}");
//                         std::process::exit(2)
//                     }
//                 }
//             } else {
//                 db.builtin_core().base(&db)
//             };

//             let local_ingot = match ingot_resolver.resolve(path) {
//                 Ok(Ingot::Folder {
//                     config,
//                     source_files:
//                         Some(SourceFiles {
//                             root: Some(_root),
//                             files,
//                         }),
//                 }) => {
//                     let diagnostics = ingot_resolver.take_diagnostics();
//                     if !diagnostics.is_empty() {
//                         eprintln!("an error was encountered while resolving `{path}`");
//                         for diagnostic in diagnostics {
//                             eprintln!("{diagnostic}")
//                         }
//                         std::process::exit(2)
//                     }
//                     let local_base_url = Url::from_file_path_lossy(&_root);
//                     let index = db.workspace();
//                     index.touch_ingot(&mut db, &local_base_url);
//                     for (path, content) in files {
//                         local_base_url.touch(&mut db, path, Some(content));
//                     }
//                     local_base_url
//                 }
//                 Ok(Ingot::SingleFile { path, content }) => {
//                     let url = Url::from_file_path_lossy(&path);
//                     db.workspace().touch(&mut db, url.clone(), Some(content));
//                     db.workspace()
//                         .containing_ingot_base(&db, &url)
//                         .expect("Failed to find ingot base")
//                 }
//                 Ok(_) => {
//                     eprintln!("an error was encountered while resolving `{path}`");
//                     for diagnostic in ingot_resolver.take_diagnostics() {
//                         eprintln!("{diagnostic}")
//                     }
//                     std::process::exit(2)
//                 }
//                 Err(error) => {
//                     eprintln!("an error was encountered while resolving `{path}`");
//                     eprintln!("{error}");
//                     std::process::exit(2)
//                 }
//             };

//             let core_diags =
//                 db.run_on_ingot(core_ingot.ingot(&db).expect("core ingot should exist"));
//             let local_diags =
//                 db.run_on_ingot(local_ingot.ingot(&db).expect("local ingot should exist"));

//             if !core_diags.is_empty() || !local_diags.is_empty() {
//                 core_diags.emit(&db);
//                 local_diags.emit(&db);
//                 std::process::exit(1);
//             }
//         }
//         Command::New => eprintln!("`fe new` doesn't work at the moment"),
//     }
// }

// #[derive(Debug, Clone, Parser)]
// #[command(version, about, long_about = None)]
// pub struct Options {
//     #[command(subcommand)]
//     pub command: Command,
// }

// #[derive(Debug, Clone, Subcommand)]
// pub enum Command {
//     Build,
//     Check {
//         // #[clap(default_value_t = find_project_root().unwrap_or(Utf8PathBuf::from(".")))]
//         path: Utf8PathBuf,
//         #[arg(short, long)]
//         core: Option<Utf8PathBuf>,
//     },
//     New,
// }

// fn _dump_scope_graph(db: &DriverDataBase, top_mod: TopLevelMod) -> String {
//     let mut s = vec![];
//     top_mod.scope_graph(db).write_as_dot(db, &mut s).unwrap();
//     String::from_utf8(s).unwrap()
// }

pub struct InputNodeHandler<'a> {
    pub db: &'a mut dyn InputDb,
    pub workspace: Workspace,
}

impl<'a> ResolutionHandler<FilesResolver> for InputNodeHandler<'a> {
    type Item = Vec<(Utf8PathBuf, (SmolStr, IngotArguments))>;

    fn handle_resolution(
        &mut self,
        ingot_path: &Utf8PathBuf,
        files: Vec<(Utf8PathBuf, String)>,
    ) -> Self::Item {
        let mut config = None;

        // println!("{ingot_path}: {:#?}", files);

        for (path, content) in files {
            if path.ends_with("fe.toml") {
                self.workspace.touch(
                    self.db,
                    Url::from_file_path(path).unwrap(),
                    Some(content.clone()),
                );
                config = Some(content);
            } else {
                self.workspace
                    .touch(self.db, Url::from_file_path(path).unwrap(), Some(content));
            }
        }

        if let Some(content) = config {
            let config = Config::from_string(content);
            config
                .dependencies
                .into_iter()
                .map(|dependency| match dependency.description {
                    DependencyDescription::Path(path) => (
                        ingot_path.join(path).canonicalize_utf8().unwrap(),
                        (dependency.alias, IngotArguments::default()),
                    ),
                    DependencyDescription::PathWithArguments { path, arguments } => (
                        ingot_path.join(path).canonicalize_utf8().unwrap(),
                        (dependency.alias, arguments),
                    ),
                })
                .collect()
        } else {
            vec![]
        }
    }
}

pub fn setup_workspace(db: &mut dyn InputDb, path: &Utf8PathBuf) -> Workspace {
    let workspace = db.workspace();
    let node_handler = InputNodeHandler { db, workspace };
    let mut graph_resolver = ingot_graph_resolver(node_handler);
    let graph = graph_resolver
        .transient_resolve(&path.canonicalize_utf8().unwrap())
        .unwrap();

    workspace
}

// resolve all ingots in a path or resolve all other fields if it is src dir or src file
// resolve graphs for each individual ingot or one big graph
//  - we need a way to kick off the resolution process when there is a discrepency
// resolver finds fe.toml files
// --> puts them in workspace index
// --> salsa tracked ingots get rebuilt because of new ingot config files
// --> dependencies list gets updated
// --> some dependencies are missing
// --> stick missing dependencies in a list of "dependencies pending resolution"
// --> we want to get this list down to 0
// --> kick off resolver for dependecies
// --> resolver resolves missing dependencies
// --> creates new fe.toml files
