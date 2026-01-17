use criterion::{criterion_group, criterion_main, Criterion};
use serde::de::DeserializeSeed;
use serde_describe::SchemaBuilder;
use serde_describe_benchmark_suite::{run_matrix, Dataset, Format, GithubEvents};
use std::hint::black_box;

fn benchmark_format_dataset<FormatT: Format, DatasetT: Dataset>(
    c: &mut Criterion,
    format: &FormatT,
    dataset: &DatasetT,
) {
    let serialized = format.serialize_to_bytes(dataset);
    c.bench_function(
        &format!("{},{},serialize", dataset.name(), format.name()),
        |b| b.iter(|| format.serialize_to_bytes(black_box(dataset))),
    );
    c.bench_function(
        &format!("{},{},deserialize", dataset.name(), format.name()),
        |b| b.iter(|| format.deserialize_from_bytes::<DatasetT>(black_box(&serialized))),
    );
}

fn serialize_deserialize_benchmark(criterion: &mut Criterion) {
    run_matrix!(benchmark_format_dataset(criterion));
}

fn external_schema_benchmark(c: &mut Criterion) {
    let events = GithubEvents::load();
    let mut builder = SchemaBuilder::new();
    let trace = builder.trace(&events).unwrap();
    let schema = builder.build().unwrap();
    let described = schema.describe_trace(trace);
    let postcard_serialized = postcard::to_stdvec(&described).unwrap();
    let seed = schema.describe_type::<GithubEvents>();
    c.bench_function("external-schema postcard serialize github_events", |b| {
        b.iter(|| postcard::to_stdvec(black_box(&described)))
    });
    c.bench_function("external-schema postcard deserialize github_events", |b| {
        b.iter(|| {
            black_box(seed)
                .deserialize(&mut postcard::Deserializer::from_bytes(black_box(
                    &postcard_serialized,
                )))
                .unwrap();
        })
    });
}

criterion_group!(
    benches,
    serialize_deserialize_benchmark,
    external_schema_benchmark
);
criterion_main!(benches);
