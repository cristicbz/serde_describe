use serde_describe_benchmark_suite::{run_matrix, Dataset, Format};
use zstd::encode_all;

fn format_dataset_sizes<FormatT, DatasetT>(format: &FormatT, dataset: &DatasetT)
where
    FormatT: Format,
    DatasetT: Dataset,
{
    let bytes = format.serialize_to_bytes(dataset);
    let raw_size = bytes.len();
    let zstd3_size = encode_all(&bytes[..], 3).unwrap().len();
    let zstd9_size = encode_all(&bytes[..], 9).unwrap().len();
    let zstd22_size = encode_all(&bytes[..], 22).unwrap().len();
    println!(
        "{},{},{},{},{},{}",
        dataset.name(),
        format.name(),
        raw_size,
        zstd3_size,
        zstd9_size,
        zstd22_size
    );
}

fn main() {
    println!("dataset,format,raw_size,zstd3,zstd9,zstd22");
    run_matrix!(format_dataset_sizes());
}
