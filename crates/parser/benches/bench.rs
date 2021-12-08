use criterion::{criterion_group, criterion_main, Criterion};
use fe_common::files::FileStore;

fn uniswap(c: &mut Criterion) {
    let mut files = FileStore::new();
    let path = "demos/uniswap.fe";
    let src = fe_test_files::fixture(path);
    let id = files.add_file(path, src);

    c.bench_function("parse_uniswap", |b| {
        b.iter(|| fe_parser::parse_file(id, &src))
    });
    c.bench_function("clone_ast", |b| {
        let ast = fe_parser::parse_file(id, &src).unwrap();
        b.iter(|| ast.clone())
    });
}

criterion_group!(benches, uniswap);
criterion_main!(benches);
