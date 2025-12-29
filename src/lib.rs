// #![deny(missing_docs)]

pub(crate) mod anonymous_union;
pub(crate) mod builder;
pub(crate) mod de;
pub(crate) mod deferred;
pub(crate) mod described;
pub(crate) mod errors;
pub(crate) mod indices;
pub(crate) mod pool;
pub(crate) mod schema;
pub(crate) mod ser;
pub(crate) mod trace;

pub use builder::{SchemaBuilder, Value};
pub use described::{DescribedBy, SeedDescribedBy, SeedSelfDescribed, SelfDescribed};
pub use schema::Schema;
