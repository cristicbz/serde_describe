[![License](https://img.shields.io/badge/license-MIT%2FApache-blue.svg)](https://github.com/cristicbz/serde_describe#license)
[![Crates.io](https://img.shields.io/crates/v/serde_describe.svg)](https://crates.io/crates/serde_describe)
[![Docs](https://docs.rs/serde_describe/badge.svg)](https://docs.rs/serde_describe/latest/serde_describe/)
[![CI](https://github.com/cristicbz/serde_describe/actions/workflows/ci.yml/badge.svg)](https://github.com/cristicbz/serde_describe/actions/workflows/ci.yml)

# serde_describe

Make a non-self-describing [`serde`](https://docs.rs/serde) format (like
[`bincode`](https://docs.rs/bincode2), [`bitcode`](https://docs.rs/bitcode) or
[`postcard`](https://docs.rs/postcard)) behave as a self-describing one by
transparently serializing a schema alongside (or [separately
from](#advanced-usage-external-schema)) the data.

In the simplest case it's simply adding a newtype-wrapper around your data

```norust
let bytes = bitcode::serialize(&original)?;
let roundtripped: Original = bitcode::deserialize(&bytes)?;
```

becomes

```norust
let bytes = bitcode::serialize(&SelfDescribed(&original))?;
let SelfDescribed(roundtripped): SelfDescribed<Original> =
  bitcode::deserialize(&bytes)?;
```

Or, for a working example

```rust
use serde::{Deserialize, Serialize};
use serde_describe::{Schema, SelfDescribed};

// Define a type that non-self-describing formats would generally struggle
// with, using skipped fields and untagged unions.
#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct TopLevel {
    int32: u32,

    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    items: Vec<Item>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
enum Item {
    Int(u32),
    Str(String),
}

// Serialize a `Vec<TopLevel>` using the `SelfDescribed` wrapper and bitcode, a
// particularly restrictive binary format.
let original = vec![
    TopLevel {
        int32: 10,
        items: vec![Item::Str("hello".to_owned()), Item::Int(10)],
    },
    TopLevel {
        int32: 20,
        items: Vec::new(),
    },
];
let bytes = bitcode::serialize(&SelfDescribed(&original))?;

// Then deserialize using the same wrapper to use the embedded schema.
let roundtripped: SelfDescribed<Vec<TopLevel>> =
    bitcode::deserialize(&bytes)?;
assert_eq!(roundtripped, original);

# Ok::<_, Box<dyn std::error::Error>>(())
```

## Trade-offs
At a high level, the library prioritizes deserialization speed and size on the
wire at the expense of serialization speed.

* **Deserialization Speed**. Should only introduce a small amount of overhead
  compared to the underlying format. The only additional operations performed
  are validation and dispatching based on a relatively small, cache-friendly
  schema object. In addition to the schema object itself, the only other data
  the wrapper adds is discriminants to tag dynamically shaped data (enums,
  tagged or otherwise, and structs with skipped fields).

* **Size**. Given a compact underlying format, the `SelfDescribed`
  representation should end up more compact than typical self-describing
  formats for large enough objects, as type information (shapes, field names
  and variant names) is deduplicated across the entire object. See the
  [Advanced Usage](#advanced-usage-external-schema) section on how to go
  further and amortize the schema overhead across multiple objects as well.

* **Serialization Speed**. The library is optimized for data that is written
  once and read many times. As such, various, potentially expensive, schema
  optimization passes are performed to keep the schema size small (and not grow
  unbounded with the input data).

Note that these goals are, at this point, mostly a statement of intent and
reflect only high-level architectural choices. On the nuts and bolts of the
implementation, the crate hasn't gone through extensive profiling and
optimization yet.

## Current limitations
The crate's current implementation has a few limitations. None of these are
fundamental architectural constraints and they could all be removed
backwards-compatibly if there is sufficient demand / motivation to do so.

* **Objects must be < ~4GiB in size.** More precisely, the various indices used
  internally by the library need to fit in 32-bit unsigned integers*
* **Structs can have at most 64 skippable fields.** These are fields that, in
  any given trace, appear as both present and absent. Fields that are always
  present or always absent (irrespective of their
  `#[serde(skip_serializing_if)]` attributes) do not count towards this limit.

## Advanced usage: external schema

As shown above, the simplest way to use this library is to simply wrap your
data in `SelfDescribed`. However, if you're storing many objects of the same
type and you need to deserialize them independently (so you can't simply
serialize a `Vec<Item>`), then saving each one as a separate `SelfDescribed`
object adds unnecessary overhead due to repeatedly serializing the schema with
each object

```rust
use serde::{Deserialize, Serialize};
use serde_describe::SelfDescribed;

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Item {
    id: u64,
    #[serde(default, skip_serializing_if="Option::is_none")]
    tag: Option<String>,
}

let bytes1 = postcard::to_stdvec(
    &SelfDescribed(Item { id: 1, tag: Some("foo".to_owned()) }),
)?;
let bytes2 = postcard::to_stdvec(
    &SelfDescribed(Item { id: 2, tag: None }),
)?;

// Quite a few bytes per item, despite the objects' small size:
assert_eq!(bytes1.len(), 41);
assert_eq!(bytes2.len(), 34);
assert_eq!(bytes1.len() + bytes2.len(), 75);

let SelfDescribed(item1) =
    postcard::from_bytes::<SelfDescribed<Item>>(&bytes1)?;
let SelfDescribed(item2) =
    postcard::from_bytes::<SelfDescribed<Item>>(&bytes2)?;
assert_eq!([item1.id, item2.id], [1, 2]);

# Ok::<(), Box<dyn std::error::Error>>(())
```

To avoid this overhead, `serde_describe` supports the `SchemaBuilder` API
which, while more complicated to use, makes schema serialization and
deserialization explicit and separate from the data

```rust
use serde::{Deserialize, Serialize};
use serde_describe::{SchemaBuilder, DescribedBy, Schema};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Item {
    id: u64,
    #[serde(default, skip_serializing_if="Option::is_none")]
    tag: Option<String>,
}

// A `SchemaBuilder` is used to trace multiple values, before serializing them
// and the inferred schema that describes all of them.
let mut builder = SchemaBuilder::new();
let trace1 = builder.trace(&Item { id: 1, tag: Some("foo".to_owned()) })?;
let trace2 = builder.trace(&Item { id: 2, tag: None })?;
let schema = builder.build()?;

// Save the three objects without the schema, and the schema separately.
let bytes1 = postcard::to_stdvec(&schema.describe_trace(trace1))?;
let bytes2 = postcard::to_stdvec(&schema.describe_trace(trace2))?;
let bytes_schema = postcard::to_stdvec(&schema)?;

// Each individual serialized object is now very small and the combined size of
// the two objects + schema is still smaller, as the schema is only saved once.
assert_eq!(bytes1.len(), 6);
assert_eq!(bytes2.len(), 2);
assert_eq!(bytes_schema.len(), 38);
assert_eq!(bytes1.len() + bytes2.len() + bytes_schema.len(), 46);

// To deserialize the data, first load the schema, then use it to deserialize
// `DescribedBy` objects for each of the items. Behind the scenes this uses
// `DeserializeSeed` so the underlying format needs to have a public API that
// supports it.
let schema = postcard::from_bytes::<Schema>(&bytes_schema)?;
let item1: Item = schema.deserialize_described(
    &mut postcard::Deserializer::from_bytes(&bytes1),
)?;
let item2: Item = schema.deserialize_described(
    &mut postcard::Deserializer::from_bytes(&bytes2),
)?;
assert_eq!([item1.id, item2.id], [1, 2]);

// You can also get a human-readable view of the inferred schema for debugging
// purposes.
assert_eq!(
    format!("{:#}", schema),
    "Item_4 { id: u64_1, tag?: some_3(str_2) }"
);

# Ok::<(), Box<dyn std::error::Error>>(())
```

## How it works

You can completely skip this section unless you're curious about the internal
implementation of the library.

Serializing anything with `serde_describe` is a two-pass process
(`SelfDescribed` simply does both of these two phases for you)
 1. An serializer runs on the given value producing a `Trace` and a `Schema`.
 2. The underlying serializer is used to save the pair of `(schema,
    transform(schema, trace))`.

At deserialization time, a `Deserializer` is built which wraps the underlying
deserializer and walks the schema graph performing coercions and calling the
appropriate `serde::de::Visitor` methods.

We'll tackle the trace, schema and transform in turn.

### Trace

The `Trace` is a replayable representation of all the `serde::Serializer` calls
made on the internal serializer (`serialize_u32`, `serialize_struct`,
`serialize_struct(field_name, ...)` etc).

The upside of using this intermediate representation is that it side-steps any
risks of non-deterministic serialization and its structure is chosen such that
matching back to the `Schema` later is simpler. The downside is that it
potentially doubles the amount of memory required during serialization, and it
is almost certainly slower than serializing the same type again would be.

### Schema
The `schema` contains interned type, field and variant names, as well as a
graph for the finite state machine describing how to parse the stored object.

Importantly, it is a representation of the **serialized** data, not of the
actual abstract type definition from Rust. So if certain kinds of data are
never observed, information about them isn't serialized, saving space. The
upshot is that this makes a particular schema only useful with the data that
was traced to generate it, not generally useful for any value of given type.

To peek behind the curtain and bring the point home let's look first at a human
human-readable description of the inferred schema for a non-trivial object.

```rust
use serde::{Deserialize, Serialize};
use serde_describe::{SchemaBuilder, Schema, SelfDescribed};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Parent {
    #[serde(default, skip)]
    always_skipped_by_serde: Child,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    always_skipped_but_data_dependent: Option<Child>,

    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    never_skipped: Vec<Child>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    sometimes_skipped: Option<u32>,

    tag: Option<String>,
}

#[derive(Serialize, Deserialize, PartialEq, Default, Debug)]
enum Child {
    #[default]
    NoData,
    Unused(u32),
    WithData {
        mixed: Option<String>,
        always_some: Option<String>,
        always_none: Option<String>,
    }
}

let data = vec![
    Parent {
        always_skipped_by_serde: Child::NoData,
        always_skipped_but_data_dependent: None,
        never_skipped: vec![
            Child::WithData {
                mixed: None,
                always_some: Some("value1".to_owned()),
                always_none: None,
            },
            Child::WithData {
                mixed: None,
                always_some: Some("value2".to_owned()),
                always_none: None,
            },
            Child::NoData,
        ],
        sometimes_skipped: Some(1234),
        tag: None,
    },
    Parent {
        always_skipped_by_serde: Child::NoData,
        always_skipped_but_data_dependent: None,
        never_skipped: vec![
            Child::WithData {
                mixed: Some("value3".to_owned()),
                always_some: Some("value4".to_owned()),
                always_none: None,
            },
        ],
        sometimes_skipped: None,
        tag: Some("tag_value".to_owned()),
    },
];

// Print the serialized `SelfDescribed` object as RON making it easier to
// inspect. We'll look at this later.
eprintln!(
    "{}",
    ron::ser::to_string_pretty(
        &SelfDescribed(&data),
        ron::ser::PrettyConfig::default(),
    ).unwrap(),
);

// The `display_for_value` method on `Schema` does the work of tracing the
// value and converts it to a human-readable format. The output isn't
// stable, but is useful for debugging.
assert_eq!(
    format!("{:#}", Schema::display_for_value(&data).unwrap()),
    "\
seq_12(
  Parent_11 {
    always_skipped_but_data_dependent?: !,
    never_skipped: seq_8(
      union_7(
        Child::WithData_5 {
          mixed: union_4(none_1, some_3(str_2)),
          always_some: some_3(..),
          always_none: none_1,
        },
        Child::NoData_6,
      ),
    ),
    sometimes_skipped?: some_10(u32_9),
    tag: union_4(..),
  },
)",
);
```

Each type gets displayed with a `_<id>` suffix which maps to a unique node id
in the FSM graph. This way, the mostly tree-like schema can be displayed as
such, even though in reality it has cycles and repeated nodes. Children of
previously expanded nodes nodes are simply shown as `..`; see for instance
`union_4(..)`, not repeating the previous `union_4(none_1, some_3(str_2))`.

Getting back to the schema itself:
 1. Top-level, we see it's a `seq` (sequence) of `Parent` structs (braces mean
    struct).
 2. The `always_skipped_by_serde` field doesn't show up at all. Its presence
    will not take up any space in serialized object
 3. The `always_skipped_but_data_dependent` field shows up as optional (suffixed
    with `?`), but its type is `!` (the never type, like in rust). Its presence
    will not take up any space in serialized objects because it's inferred to
    never be present.
 4. The `never_skipped` field, even though it /could/ be skipped by `serde`
    simply shows up as "required" (the same as `tag`), because it is always
    present in the traced data.
 5. The `sometimes_skipped` field is sometimes persent and sometimes absent. As
    such, its presence will require 1 bit per serialized object. More detail on
    how this is actually represented in the transform section below.
 6. The type of `sometimes_skipped` is `some_10(u32_9)`; because the value is
    never `None`, we don't actually need a discriminant for this type; when
    present, it's always `Some`.
 7. Unlike `sometimes_skipped`, `tag` is sometimes `None` sometimes `Some`, so
    we'll store it as a discriminated union between `None` and `Some(String)`.
    `Option`-s are not treated in any special way, instead they act like any
    other unit struct and newtype struct respectively.

Looking at the children in `never_skipped`, note that the schema if unified
across different values of `Parent` and `Child`. Also note that `Child::Unused`
doesn't appear in `union_7`, because it's never serialized.


### Transform
The `transform` function converts the object into a subset of `serde` features
that are supported by non-self-describing formats. In particular:
 1. Unions (as seen above) are used to turn heterogeneous sequences and maps
    into homogeneous collections of discriminated enums.
 2. Union discriminants rely on variant index, with original enum variant names
    deduplicated and stored in the schema.
 3. Structs are turned into tuples with field names, types and order
    deduplicated and stored in the schema.
 4. Structs (and struct variants) with skippable fields are turned into enums
    with the discriminant encoding a bitset of present fields. As mentioned
    above, bits are only used for fields that are sometimes present; fields
    that always absent or always present do not use up a bit.
 5. Discriminants with more than 256 values are chunked into nested enums of at
    most 256 variants. This is because `serde` insists on `'static` variant
    names. So the crate ships with a static array of variants named `_00`
    through to `_ff` which can be used any times they're required.

This is what the RON dump of the object from the `Schema` example, annotated
with comments and cleaned up a bit.

```ron
// seq_12
[
    // Parent_11::_01 (discriminant means the `sometimes_skipped` field is present)
    _01(
        // .never_skipped: seq_8
        [
            // union_7::_00 => Child::WithData_5
            _00(
                // mixed: union_4::_00 => None
                _00,

                // always_some: some_3(str_2)
                "value1",  // (no Some("...") or discriminant, as it's never None)
            ),

            // union_7::_00 => Child::WithData_5
            _00(_00, "value2"),

            // union_7::_01 => Child::NoData
            _01,
        ],

        // .sometimes_skipped: some_10
        1234, // (no discriminant as it's never None when present)

        // .tag: union_4
        _00,  // _00 => none_1
    ),

    // Parent_11::_00 (`sometimes_skipped` absent)
    _00(
        // .never_skipped:
        [
            // union_7::_00 => Child::WithData_5
            _00(
                // .mixed: union_4::_01 => Some(String)
                _01("value3"),

                // .always_some: some_3(str_2)
                "value4",
            ),
        ],

        // tag: union_4
        _01("tag_value"),  // ::_01 => some_3(str_2)
    ),
]
```
