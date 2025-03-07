use camino::{Utf8Path, Utf8PathBuf};
use common::InputIngot;
use criterion::{criterion_group, criterion_main, Criterion, SamplingMode};
use driver::DriverDataBase;
use walkdir::WalkDir;

use parser::parse_source_file;

fn diagnostics(c: &mut Criterion) {
    let mut g = c.benchmark_group("analysis");
    g.sampling_mode(SamplingMode::Flat);

    g.bench_function("analyze corelib", |b| {
        b.iter_with_large_drop(|| {
            let db = DriverDataBase::default();
            let core = InputIngot::core(&db);
            db.run_on_ingot(core);
            db
        });
    });

    let files = test_files("../uitest/fixtures/".into());

    g.bench_function("uitest parsing", |b| {
        b.iter(|| {
            for (_, content) in &files {
                parse_source_file(content);
            }
        })
    });

    g.bench_function("uitest diagnostics", |b| {
        b.iter_with_large_drop(|| {
            let mut db = DriverDataBase::default();
            let core = InputIngot::core(&db);
            for (path, content) in &files {
                let (ingot, file) = db.standalone(path, content, core);
                let top_mod = db.top_mod(ingot, file);

                db.run_on_top_mod(top_mod);
            }
            db
        });
    });

    g.finish();
}

fn test_files(path: &Utf8Path) -> Vec<(Utf8PathBuf, String)> {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");

    let test_dir = Utf8Path::new(manifest_dir).join(path);

    WalkDir::new(test_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "fe"))
        .map(|e| {
            let path = Utf8PathBuf::from_path_buf(e.into_path()).unwrap();
            let content = std::fs::read_to_string(&path).unwrap();
            (path, content)
        })
        .collect()
}

criterion_group!(benches, diagnostics);
criterion_main!(benches);
