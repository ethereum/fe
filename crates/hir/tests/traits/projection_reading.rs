//! While an impl's where clauses are proved, the projection it defines is
//! read as its own definition only where proving them may meet the projection
//! again, as found by a structural search (`may_need_projection`). Reading it
//! for every candidate is always correct, only slower. So every program must
//! give the same diagnostics both ways: a difference is a path the search
//! misses.

use std::path::{Path, PathBuf};

use fe_hir::test_db::{
    HirAnalysisTestDb, initialize_test_analysis_pass, set_read_definition_always,
};

#[allow(dead_code)]
mod families {
    include!("limit_families.rs");

    /// Every family's programs at depth 3 and 70, in both settings.
    pub(super) fn programs() -> Vec<(String, String)> {
        let mut programs = Vec::new();
        for family in FAMILIES {
            for depth in [3, 70] {
                for show in [false, true] {
                    programs.push((
                        format!("{} depth={depth} show={show}", family.name),
                        source(family, false, depth, show),
                    ));
                }
            }
        }
        programs
    }
}

/// The rendered diagnostics of `src`, in order.
fn diagnostics(src: &str) -> Vec<String> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("projection_reading.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    let mut manager = initialize_test_analysis_pass();
    manager
        .run_on_module(&db, top_mod)
        .into_iter()
        .map(|diag| format!("{:?}", diag.to_complete(&db)))
        .collect()
}

fn fe_files(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            fe_files(&path, out);
        } else if path.extension().is_some_and(|ext| ext == "fe") {
            out.push(path);
        }
    }
}

/// The programs: the probes of the projection reading (each path that leads
/// back to a projection, valid and failing), and a few type-checking
/// fixtures. With `FE_PROJECTION_READING_CORPUS` set to a directory, also
/// every program in it, every type-checking fixture, and the limit matrix's
/// families: the full check, run outside the default suite.
fn programs() -> Vec<(String, String)> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let fixtures = root.join("../uitest/fixtures");
    let mut files = Vec::new();
    fe_files(&root.join("test_files/projection_reading"), &mut files);
    for fixture in [
        "ty_check/normalization_unproved_impl.fe",
        "ty_check/normalization_limit_proofs.fe",
        "ty_check/normalization_limit_cycles.fe",
        "ty_check/trait_bound_undecided.fe",
    ] {
        files.push(fixtures.join(fixture));
    }
    let corpus = std::env::var_os("FE_PROJECTION_READING_CORPUS");
    if let Some(corpus) = &corpus {
        fe_files(Path::new(corpus), &mut files);
        fe_files(&fixtures.join("ty_check"), &mut files);
        fe_files(&fixtures.join("ty"), &mut files);
    }
    files.sort();
    files.dedup();
    let mut programs: Vec<(String, String)> = files
        .into_iter()
        .map(|path| {
            let text = std::fs::read_to_string(&path)
                .unwrap()
                .replace("\r\n", "\n");
            (path.display().to_string(), text)
        })
        .collect();
    if corpus.is_some() {
        programs.extend(families::programs());
    }
    programs
}

#[test]
fn reading_every_definition_changes_no_diagnostic() {
    let programs = programs();
    let next = std::sync::atomic::AtomicUsize::new(0);
    let differences = std::sync::Mutex::new(Vec::new());
    std::thread::scope(|scope| {
        for _ in 0..8 {
            scope.spawn(|| {
                loop {
                    let idx = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    let Some((name, src)) = programs.get(idx) else {
                        break;
                    };
                    set_read_definition_always(false);
                    let searched = diagnostics(src);
                    set_read_definition_always(true);
                    let always = diagnostics(src);
                    set_read_definition_always(false);
                    if searched != always {
                        differences.lock().unwrap().push(format!(
                            "{name}:\nwith the search: {searched:#?}\nreading always: {always:#?}"
                        ));
                    }
                }
            });
        }
    });
    let differences = differences.into_inner().unwrap();
    assert!(differences.is_empty(), "{}", differences.join("\n\n"));
}
