# Benchmark results

[Commit](af313aad98ede635309fd4f287650828eac07e3e)

## Relative Measurements

All numbers divided by best in dataset, geometrically averaged across datasets.
Only self-describing formats can serialize some of the datasets (due to
`skip_serializing_if`-s or other serde features), those are excluded from
the averages for non-self-describing formats.

| Format                  | Size | zstd3 | zstd22 | Serialization | Deserialization |
|-------------------------|------|-------|--------|---------------|-----------------|
| bitcode                 | 1.00 | 1.00  | 1.00   | 2.48          | 1.15            |
| postcard                | 1.04 | 1.22  | 1.18   | 1.00          | 1.01            |
| SelfDescribed(bitcode)  | 1.01 | 1.05  | 1.07   | 28.39         | 1.94            |
| SelfDescribed(postcard) | 1.09 | 1.31  | 1.23   | 28.67         | 1.88            |
| pot                     | 1.36 | 1.42  | 1.27   | 2.33          | 3.74            |
| serde_cbor              | 2.28 | 1.65  | 1.35   | 2.88          | 2.22            |
| serde_json              | 3.04 | 1.60  | 1.33   | 3.96          | 2.93            |

## Raw Sizes & Timings

Median numbers from criterion on an GCE N4D instance with 4vCPU (2 cores), 16
GiB RAM.

| Dataset                   | Format                  | Size (KiB) | zstd3 (KiB) | zstd22 (KiB) | Serialization (us) | Deserialization (us) |
|---------------------------|-------------------------|------------|-------------|--------------|--------------------|----------------------|
| Github Events             | bitcode                 | 31.47      | 10.31       | 9.92         | 33.39              | 56.56                |
|                           | postcard                | 32.34      | 10.84       | 10.47        | 20.56              | 58.12                |
|                           | SelfDescribed(bitcode)  | 32.04      | 10.79       | 10.35        | 294.08             | 90.52                |
|                           | SelfDescribed(postcard) | 32.93      | 11.36       | 10.95        | 269.66             | 93.24                |
|                           | pot                     | 37.12      | 12.14       | 11.26        | 35.41              | 119.31               |
|                           | serde_cbor              | 48.53      | 11.98       | 11.14        | 41.29              | 111.79               |
|                           | serde_json              | 57.24      | 11.03       | 10.41        | 53.57              | 128.81               |
| -                         | -                       | -          | -           | -            | -                  | -                    |
| Fixed Schema Game Level   | bitcode                 | 32.97      | 1.66        | 1.30         | 45.39              | 56.61                |
|                           | postcard                | 34.37      | 2.36        | 1.71         | 11.97              | 42.83                |
|                           | SelfDescribed(bitcode)  | 33.21      | 1.87        | 1.51         | 1178.80            | 174.83               |
|                           | SelfDescribed(postcard) | 34.63      | 2.56        | 1.83         | 1279.00            | 172.09               |
|                           | pot                     | 42.64      | 2.75        | 1.91         | 87.977             | 490.43               |
|                           | serde_cbor              | 77.86      | 3.38        | 2.08         | 118.85             | 211.48               |
|                           | serde_json              | 108.30     | 3.47        | 2.15         | 172.94             | 306.00               |
| -                         | -                       | -          | -           | -            | -                  | -                    |
| Dynamic Schema Game Level | SelfDescribed(bitcode)  | 14.73      | 1.67        | 1.43         | 950.31             | 151.73               |
|                           | SelfDescribed(postcard) | 17.38      | 2.23        | 1.73         | 984.23             | 136.00               |
|                           | pot                     | 24.53      | 2.44        | 1.76         | 58.523             | 293.58               |
|                           | serde_cbor              | 48.23      | 3.19        | 1.98         | 70.396             | 152.46               |
|                           | serde_json              | 69.22      | 3.08        | 1.96         | 96.327             | 209.88               |

