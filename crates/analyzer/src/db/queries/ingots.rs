use crate::namespace::items::{IngotId, IngotMode, ModuleId, ModuleSource};
use crate::AnalyzerDb;
use fe_common::files::{SourceFileId, Utf8Path, Utf8PathBuf};
use indexmap::IndexSet;
use std::rc::Rc;

pub fn ingot_modules(db: &dyn AnalyzerDb, ingot: IngotId) -> Rc<[ModuleId]> {
    let files: Vec<(SourceFileId, Rc<Utf8PathBuf>)> = db
        .ingot_files(ingot)
        .iter()
        .map(|f| (*f, f.path(db.upcast())))
        .collect();

    // Create a module for every .fe source file.
    let file_mods = files
        .iter()
        .map(|(file, path)| {
            ModuleId::new(
                db,
                path.file_stem().unwrap(),
                ModuleSource::File(*file),
                ingot,
            )
        })
        .collect();

    // We automatically build a module hierarchy that matches the directory
    // structure. We don't (yet?) require a .fe file for each directory like
    // rust does. (eg `a/b.fe` alongside `a/b/`), but we do allow it (the
    // module's items will be everything inside the .fe file, and the
    // submodules inside the dir).
    //
    // Collect the set of all directories in the file hierarchy
    // (after stripping the common prefix from all paths).
    // eg given ["src/lib.fe", "src/a/b/x.fe", "src/a/c/d/y.fe"],
    // the dir set is {"a", "a/b", "a/c", "a/c/d"}.
    let file_path_prefix = &ingot.data(db).src_dir;
    let dirs = files
        .iter()
        .flat_map(|(_file, path)| {
            path.strip_prefix(file_path_prefix.as_str())
                .unwrap_or(path)
                .ancestors()
                .skip(1) // first elem of .ancestors() is the path itself
        })
        .collect::<IndexSet<&Utf8Path>>();

    let dir_mods = dirs
        // Skip the dirs that have an associated fe file; eg skip "a/b" if "a/b.fe" exists.
        .difference(
            &files
                .iter()
                .map(|(_file, path)| {
                    path.strip_prefix(file_path_prefix.as_str())
                        .unwrap_or(path)
                        .as_str()
                        .trim_end_matches(".fe")
                        .into()
                })
                .collect::<IndexSet<&Utf8Path>>(),
        )
        .filter_map(|dir| {
            dir.file_name()
                .map(|name| ModuleId::new(db, name, ModuleSource::Dir(dir.as_str().into()), ingot))
        })
        .collect::<Vec<_>>();

    [file_mods, dir_mods].concat().into()
}

pub fn ingot_root_module(db: &dyn AnalyzerDb, ingot: IngotId) -> Option<ModuleId> {
    let filename = match ingot.data(db).mode {
        IngotMode::Lib => "lib.fe",
        IngotMode::Main => "main.fe",
        IngotMode::StandaloneModule => return Some(ingot.all_modules(db)[0]),
    };

    ingot
        .all_modules(db)
        .iter()
        .find(|modid| modid.file_path_relative_to_src_dir(db) == filename)
        .copied()
}
