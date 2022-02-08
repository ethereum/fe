use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use fe_analyzer::namespace::items::ModuleId;
use fe_analyzer::TestDb;

fn criterion_benchmark(c: &mut Criterion) {
    let path = "demos/uniswap.fe";
    let src = test_files::fixture(path);

    c.bench_function("analyze uniswap", |b| {
        b.iter_batched(
            || {
                let mut db = TestDb::default();
                let module = ModuleId::new_standalone(&mut db, path, src);
                let _ = module.ast(&db); // parse before the timer starts
                (db, module)
            },
            |(db, module_id)| fe_analyzer::analyze_module(&db, module_id),
            BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
