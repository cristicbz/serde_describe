use crate::{SchemaBuilder, TraceError};
use serde::{
    Deserialize, Serialize,
    ser::{Error as _, SerializeStruct, SerializeTuple, Serializer},
};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct WithSkippable {
    always_present: u32,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    sometimes_skipped: Option<u32>,
}

#[test]
fn test_out_of_range_presence_discriminant_errors() {
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

    assert_eq!(bytes[0], 1);
    bytes[0] = 0x7f;
    let result: Result<WithSkippable, _> =
        schema.deserialize_described(&mut postcard::Deserializer::from_bytes(&bytes));
    assert!(result.is_err());
}

struct LyingStruct {
    declared: usize,
    actual: usize,
}

impl Serialize for LyingStruct {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        const NAMES: [&str; 3] = ["f0", "f1", "f2"];
        let mut serializer = serializer.serialize_struct("LyingStruct", self.declared)?;
        for name in &NAMES[..self.actual] {
            serializer.serialize_field(name, &0u32)?;
        }
        serializer.end()
    }
}

struct LyingTuple {
    declared: usize,
    actual: usize,
}

impl Serialize for LyingTuple {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut serializer = serializer.serialize_tuple(self.declared)?;
        for _ in 0..self.actual {
            serializer.serialize_element(&0u32)?;
        }
        serializer.end()
    }
}

#[test]
fn test_struct_length_mismatch_errors() {
    for (declared, actual) in [(0, 1), (1, 2), (2, 1)] {
        let result = SchemaBuilder::new().trace(&LyingStruct { declared, actual });
        assert!(matches!(result, Err(TraceError::LengthMismatch)));
    }
}

#[test]
fn test_tuple_length_mismatch_errors() {
    for (declared, actual) in [(0, 1), (1, 2), (2, 1)] {
        let result = SchemaBuilder::new().trace(&LyingTuple { declared, actual });
        assert!(matches!(result, Err(TraceError::LengthMismatch)));
    }
}

struct FailingValue;

impl Serialize for FailingValue {
    fn serialize<S: Serializer>(&self, _serializer: S) -> Result<S::Ok, S::Error> {
        Err(S::Error::custom("failing value"))
    }
}

struct IgnoresFieldError;

impl Serialize for IgnoresFieldError {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut serializer = serializer.serialize_struct("IgnoresFieldError", 1)?;
        let _ = serializer.serialize_field("a", &FailingValue);
        serializer.skip_field("b")?;
        serializer.end()
    }
}

// A `Serialize` implementation that ignores a failed `serialize_field` and carries on leaves the
// field names and types out of sync; this must fail tracing rather than panic when the schema is
// built.
#[test]
fn test_ignored_field_error_mismatch_errors() {
    let mut builder = SchemaBuilder::new();
    let result = builder.trace(&IgnoresFieldError);
    // Building the schema used to panic on the skippable field index left out of bounds by the
    // ignored error.
    let _ = builder.build();
    assert!(matches!(result, Err(TraceError::LengthMismatch)));
}
