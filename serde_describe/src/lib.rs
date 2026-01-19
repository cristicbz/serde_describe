//! Make a non-self-describing [`serde`](https://docs.rs/serde) format (like
//! [`bincode`](https://docs.rs/bincode2), [`bitcode`](https://docs.rs/bitcode) or
//! [`postcard`](https://docs.rs/postcard)) behave as like a self-describing one by transparently
//! serializing a schema alongside (or [separately from](#advanced-usage-external-schema)) the
//! data.
//!
//! The main entry point to the crate is [`SelfDescribed`]. For advanced uses [`SchemaBuilder`]
//! and [`Schema`] may also be of interest.
//!
//! ```
//! use serde::{Deserialize, Serialize};
//! use serde_describe::{Schema, SelfDescribed};
//!
//! // Define a type that non-self-describing formats would generally struggle
//! // with, using skipped fields and untagged unions.
//! #[derive(Debug, PartialEq, Serialize, Deserialize)]
//! struct TopLevel {
//!     int32: u32,
//!
//!     #[serde(default, skip_serializing_if = "Vec::is_empty")]
//!     items: Vec<Item>,
//! }
//!
//! #[derive(Debug, PartialEq, Serialize, Deserialize)]
//! #[serde(untagged)]
//! enum Item {
//!     Int(u32),
//!     Str(String),
//! }
//!
//! // Serialize a `Vec<TopLevel>` using the `SelfDescribed` wrapper and bitcode, a
//! // particularly restrictive binary format.
//! let original = vec![
//!     TopLevel {
//!         int32: 10,
//!         items: vec![Item::Str("hello".to_owned()), Item::Int(10)],
//!     },
//!     TopLevel {
//!         int32: 20,
//!         items: Vec::new(),
//!     },
//! ];
//! let bytes = bitcode::serialize(&SelfDescribed(&original))?;
//!
//! // Then deserialize using the same wrapper to use the embedded schema.
//! let roundtripped =
//!     bitcode::deserialize::<SelfDescribed<Vec<TopLevel>>>(&bytes)?;
//! assert_eq!(roundtripped, original);
//!
//! # Ok::<_, Box<dyn std::error::Error>>(())
//! ```
#![forbid(unsafe_code)]
#![deny(missing_docs)]

pub(crate) mod anonymous_union;
pub(crate) mod builder;
pub(crate) mod de;
pub(crate) mod deferred;
pub(crate) mod described;
pub(crate) mod dump;
pub(crate) mod indices;
pub(crate) mod lossless_cast;
pub(crate) mod pool;
pub(crate) mod schema;
pub(crate) mod ser;
pub(crate) mod trace;

pub use builder::{SchemaBuilder, TraceError};
pub use described::{DescribedBy, SelfDescribed};
pub use schema::Schema;
pub use trace::Trace;

#[cfg(test)]
mod tests;

#[cfg(doctest)]
mod doctests;
