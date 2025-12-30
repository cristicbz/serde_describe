use crate::Schema;

/// Wraps a serializable and/or deserializable type such that it gets serialized together with its
/// schema, making non-self-describing formats effectively self-describing.
///
/// Example
/// -------
/// ```rust
/// use serde_describe::SelfDescribed;
/// use serde::{Serialize, Deserialize};
///
/// // An initial version of the top-level serialized object.
/// #[derive(Debug, PartialEq, Serialize, Deserialize)]
/// struct TopLevelV1 {
///     int32: u32,
///
///     // Skippable fields, usually not supported by non-self-describing formats.
///     #[serde(default, skip_serializing_if="Vec::is_empty")]
///     children: Vec<UntaggedV1>,
/// }
///
/// // An untagged enum, usually not serializable with non-self-describing formats.
/// #[derive(Debug, PartialEq, Serialize, Deserialize)]
/// #[serde(untagged)]
/// enum UntaggedV1 {
///     Integer(u32),
///     Float(f32),
///     Bool(bool),
///     String(String)
/// }
///
/// let original = TopLevelV1 {
///     int32: 10,
///     children: vec![
///         UntaggedV1::String("hello".to_owned()),
///         UntaggedV1::Integer(10),
///         UntaggedV1::Float(2.5),
///     ],
/// };
///
/// // Any non-self-describing format will work, but let's use `bitcode` which is
/// // particularly fussy about what it supports and what it doesn't.
/// let serialized = bitcode::serialize(&SelfDescribed(&original))?;
/// let SelfDescribed(roundtripped) =
///     bitcode::deserialize::<SelfDescribed<TopLevelV1>>(&serialized)?;
/// assert_eq!(roundtripped, original);
///
/// // An evolution of the previous type.
/// #[derive(Debug, PartialEq, Serialize, Deserialize)]
/// struct TopLevelV2 {
///     // We increased the integer size to 64 bits, but still want to deserialize
///     // from the old name (and type).
///     #[serde(alias="int32")]
///     int64: u64,
///
///     // We added a new field that we want defaulted to `0.0` if reading old data.
///     #[serde(default)]
///     new: f32,
///
///     #[serde(default, skip_serializing_if="Vec::is_empty")]
///     children: Vec<UntaggedV2>,
/// }
///
/// // We removed the `Bool` option that was unused, and added a `Vec` option instead,
/// // shifting the variant order around in the process.
/// #[derive(Debug, PartialEq, Serialize, Deserialize)]
/// #[serde(untagged)]
/// enum UntaggedV2 {
///     Integer(u32),
///     Float(f32),
///     String(String),
///     Vec(Vec<u64>),
/// }
///
/// let SelfDescribed(roundtripped) =
///     bitcode::deserialize::<SelfDescribed<TopLevelV2>>(&serialized)?;
/// assert_eq!(
///     roundtripped,
///     TopLevelV2 {
///         int64: 10,
///         new: 0.0,
///         children: vec![
///             UntaggedV2::String("hello".to_owned()),
///             UntaggedV2::Integer(10),
///             UntaggedV2::Float(2.5),
///         ],
///     }
/// );
///
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
#[derive(Copy, Clone)]
pub struct SelfDescribed<T>(pub T);

/// Wraps a serializable and/or deserializable type together with a reference to a [`Schema`].
///
/// This object is most conveniently constructed by [`Schema::describe_type`],
/// [`Schema::describe_seed`] or [`Schema::describe_value`].
///
/// The resulting object is serialized WITHOUT the schema. The schema will need to be saved
/// separately and provided at deserialization time as a seed. See [`crate::SchemaBuilder`] for a
/// complete example.
#[derive(Copy, Clone)]
pub struct DescribedBy<'schema, T>(pub T, pub &'schema Schema);
