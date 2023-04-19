use criterion::{criterion_group, criterion_main, Criterion};
use fe_parser2::parse_source_file;

fn uniswap(c: &mut Criterion) {
    let src = fe_test_files::fixture("demos/uniswap_parser2.fe");
    c.bench_function("parse_uniswap", |b| {
        b.iter(|| {
            parse_source_file(src);
        })
    });
}

criterion_group!(benches, uniswap);
criterion_main!(benches);
