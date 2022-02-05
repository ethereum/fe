use criterion::{criterion_group, criterion_main, Criterion};
use fe_common::files::SourceFileId;

fn uniswap(c: &mut Criterion) {
    let src = fe_test_files::fixture("demos/uniswap.fe");
    let id = SourceFileId::dummy_file();

    c.bench_function("parse_uniswap", |b| {
        b.iter(|| fe_parser::parse_file(id, src))
    });
    c.bench_function("clone_ast", |b| {
        let ast = fe_parser::parse_file(id, src);
        b.iter(|| ast.clone())
    });
}

criterion_group!(benches, uniswap);
criterion_main!(benches);
