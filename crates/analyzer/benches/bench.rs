use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use fe_analyzer::namespace::items::{Global, Module, ModuleContext, ModuleFileContent};
use fe_analyzer::{AnalyzerDb, TestDb};
use fe_common::diagnostics::print_diagnostics;
use fe_common::files::FileStore;
use std::rc::Rc;
// use fe_parser::

fn criterion_benchmark(c: &mut Criterion) {
    let mut files = FileStore::new();
    let path = "demos/uniswap.fe";
    let src = test_files::fixture(path);
    let id = files.add_file(path, src);
    let ast = match fe_parser::parse_file(id, &src) {
        Ok((module, _)) => module,
        Err(diags) => {
            print_diagnostics(&diags, &files);
            panic!("parsing failed");
        }
    };

    c.bench_function("analyze uniswap", |b| {
        b.iter_batched(
            || {
                let db = TestDb::default();
                let global = Global::default();
                let global_id = db.intern_global(Rc::new(global));
                let module = Module {
                    name: "test_module".into(),
                    context: ModuleContext::Global(global_id),
                    file_content: ModuleFileContent::File { file: id },
                    ast: ast.clone(),
                };
                let module_id = db.intern_module(Rc::new(module));
                (db, module_id)
            },
            |(db, module_id)| fe_analyzer::analyze_module(&db, module_id),
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
