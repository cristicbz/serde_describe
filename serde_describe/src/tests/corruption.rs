use crate::SchemaBuilder;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct WithSkippable {
    always_present: u32,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    sometimes_skipped: Option<u32>,
}

// An out-of-range field presence discriminant must fail to deserialize. It used to cause an
// integer underflow in `DiscriminantSkipper::num_skipped`, panicking in debug builds and
// producing a bogus field count in release builds.
#[test]
fn test_out_of_range_presence_discriminant_errors() {
    // Trace both presence states so `sometimes_skipped` ends up in the skip list.
    let mut builder = SchemaBuilder::new();
    let trace = builder
        .trace(&WithSkippable {
            always_present: 1,
            sometimes_skipped: Some(2),
        })
        .unwrap();
    let _ = builder
        .trace(&WithSkippable {
            always_present: 3,
            sometimes_skipped: None,
        })
        .unwrap();
    let schema = builder.build().unwrap();
    let mut bytes = postcard::to_stdvec(&schema.describe_trace(trace)).unwrap();

    let roundtripped: WithSkippable = schema
        .deserialize_described(&mut postcard::Deserializer::from_bytes(&bytes))
        .unwrap();
    assert_eq!(roundtripped.always_present, 1);

    // The struct is the root value, so the first byte is its presence discriminant. With a
    // single skippable field, only `0` and `1` are valid.
    assert_eq!(bytes[0], 1);
    bytes[0] = 0x7f;
    let result: Result<WithSkippable, _> =
        schema.deserialize_described(&mut postcard::Deserializer::from_bytes(&bytes));
    assert!(result.is_err());
}
