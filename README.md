serde_describe
---

Make a non-self-describing [`serde`](https://docs.rs/serde) format (like [`bincode`](https://docs.rs/bincode2),
[`bitcode`](https://docs.rs/bitcode) or [`postcard`](https://docs.rs/postcard))
behave as if it were self-describing by transparently serializing a schema
alongside (or separately from) the data.

# Example

```rust
use serde::{Deserialize, Serialize};
use serde_describe::{Schema, SelfDescribed};

// Non-self-describing formats don't generally support skipped
// fields and untagged unions, so let's use both.
#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct TopLevel {
    int32: u32,

    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    untaggeds: Vec<Untagged>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
enum Untagged {
    Int(u32),
    Str(String),
}

// Roundtrip a `Vec` of `TopLevel` objects.
let original = vec![
    TopLevel {
        int32: 10,
        untaggeds: vec![Untagged::Str("hello".to_owned()), Untagged::Int(10)],
    },
    TopLevel {
        int32: 20,
        untaggeds: Vec::new(),
    },
];

// Using the `SelfDescribed` wrapper, serialize as a combined
// "schema + object" pair. Use `bitcode` which is a particularly restrictive
// format.
let bytes = bitcode::serialize(&SelfDescribed(&original))?;

// Deserialize using the same wrapper to use the embedded schema.
let roundtripped =
    bitcode::deserialize::<SelfDescribed<Vec<TopLevel>>>(&bytes)?;
assert_eq!(roundtripped, original);


// Get a human-readable dump of the inferred schema for debugging.
assert_eq!(
    format!("{:#}", Schema::display_for_value(&original)?),
    "seq_6(TopLevel_5 { int32: u32_1, untaggeds?: seq_4(union_3(u32_1, str_2)) })"
);

Ok::<_, Box<dyn std::error::Error>>(())
```
