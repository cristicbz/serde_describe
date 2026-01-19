use super::helpers::check_roundtrip;
use maplit::{btreemap, btreeset};
use serde::{Deserialize, Serialize};
use serde_bytes::ByteBuf;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
struct UnitStruct;
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
struct NewtypeStruct(u32);
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
struct NewtypeUnitStruct(());
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
struct NewtypeTupleStruct((u32, u64));
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
struct TupleStruct(u32, u64);
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
struct EmptyTupleStruct();
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
struct EmptyFieldStruct {}
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
struct FieldStructOne {
    x: u32,
}
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
struct FieldStructTwo {
    x: u32,
    y: u32,
}

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
enum AllVariantKinds {
    Unit,
    Newtype(u32),
    NewtypeUnit(()),
    NewtypeTuple((u32, u64)),
    NewtypeStructUnit(UnitStruct),
    NewTypeStructNewtype(NewtypeStruct),
    NewTypeStructNewtypeUnit(NewtypeUnitStruct),
    NewTypeStructNewtypeTuple(NewtypeTupleStruct),
    NewTypeStructTuple(TupleStruct),
    NewTypeStructEmptyTuple(EmptyTupleStruct),
    NewTypeStructEmptyField(EmptyFieldStruct),
    NewTypeStructFieldOne(FieldStructOne),
    NewTypeStructFieldTwo(FieldStructTwo),
    NewTypeOption(Option<Box<AllVariantKinds>>),
    Tuple(u32, u64),
    EmptyTuple(),
    EmptyStruct {},
    StructOne { x: u32 },
    StructTwo { x: u32, y: u32 },
}

macro_rules! simple {
    ($(fn $name:ident($value:expr);)+) => {
        $(
            #[test]
            fn $name() {
                check_roundtrip(&$value);
            }
        )+
    };
}

simple! {
    fn test_u8(1u8);
    fn test_u16(1u16);
    fn test_u32(1u32);
    fn test_u64(1u64);
    fn test_u128(1u128);
    fn test_zero_u8(0u8);
    fn test_zero_u16(0u16);
    fn test_zero_u32(0u32);
    fn test_zero_u64(0u64);
    fn test_zero_u128(0u128);
    fn test_max_u8(u8::MAX);
    fn test_max_u16(u16::MAX);
    fn test_max_u32(u32::MAX);
    fn test_max_u64(u64::MAX);
    fn test_max_u128(u128::MAX);
    fn test_negative_i8(-1i8);
    fn test_negative_i16(-1i16);
    fn test_negative_i32(-1i32);
    fn test_negative_i64(-1i64);
    fn test_negative_i128(-1i128);
    fn test_positive_i8(1i8);
    fn test_positive_i16(1i16);
    fn test_positive_i32(1i32);
    fn test_positive_i64(1i64);
    fn test_positive_i128(1i128);
    fn test_max_i8(i8::MAX);
    fn test_max_i16(i16::MAX);
    fn test_max_i32(i32::MAX);
    fn test_max_i64(i64::MAX);
    fn test_max_i128(i128::MAX);
    fn test_min_i8(i8::MIN);
    fn test_min_i16(i16::MIN);
    fn test_min_i32(i32::MIN);
    fn test_min_i64(i64::MIN);
    fn test_min_i128(i128::MIN);
    fn test_empty_bytes(ByteBuf::new());
    fn test_nonempty_bytes(ByteBuf::from(vec![0u8, 1u8]));
    fn test_empty_string(String::new());
    fn test_nonempty_string("hello".to_owned());
    fn test_char('c');
    fn test_unit(());
    fn test_unit_struct(UnitStruct);
    fn test_unit_variant(AllVariantKinds::Unit);
    fn test_none(None::<u32>);

    fn test_some(Some(10u32));
    fn test_newtype_struct(NewtypeStruct(1));
    fn test_newtype_variant(AllVariantKinds::Newtype(1));

    fn test_empty_map(BTreeMap::<String, u32>::new());
    fn test_nonempty_map(btreemap! { 1u32 => -1i64 });
    fn test_empty_sequence(Vec::<u32>::new());
    fn test_nonempty_sequence(vec![0u32, 1u32]);

    fn test_tuple_one((1,));
    fn test_tuple_two((1, 2));
    fn test_empty_tuple_struct(EmptyTupleStruct());
    fn test_tuple_struct(TupleStruct(1, 2));
    fn test_empty_tuple_variant(AllVariantKinds::EmptyTuple());
    fn test_tuple_variant(AllVariantKinds::Tuple(1, 2));

    fn test_empty_field_struct(EmptyFieldStruct {});
    fn test_field_struct_one(FieldStructOne { x: 1 });
    fn test_field_struct_two(FieldStructTwo { x: 1, y: 2 });
    fn test_empty_struct_variant(AllVariantKinds::EmptyStruct {});
    fn test_struct_one_variant(AllVariantKinds::StructOne { x: 1 });
    fn test_struct_two_variant(AllVariantKinds::StructTwo { x: 1, y: 2 });
    fn test_sequence_none_some(vec![None, Some(0u32)]);
    fn test_sequence_none_some_some(vec![None, Some(None), Some(Some(None)), Some(Some(Some(0u32)))]);

    fn test_enum_newtype_unit(AllVariantKinds::NewtypeUnit(()));
    fn test_enum_newtype_tuple(AllVariantKinds::NewtypeTuple((2, 3)));
    fn test_enum_newtype_struct_unit(AllVariantKinds::NewtypeStructUnit(UnitStruct));
    fn test_enum_newtype_struct_newtype(AllVariantKinds::NewTypeStructNewtype(NewtypeStruct(4)));
    fn test_enum_newtype_struct_newtype_unit(AllVariantKinds::NewTypeStructNewtypeUnit(NewtypeUnitStruct(())));
    fn test_enum_newtype_struct_newtype_tuple(AllVariantKinds::NewTypeStructNewtypeTuple(NewtypeTupleStruct((5, 6))));
    fn test_enum_newtype_struct_tuple(AllVariantKinds::NewTypeStructTuple(TupleStruct(7, 8)));
    fn test_enum_newtype_struct_empty_tuple(AllVariantKinds::NewTypeStructEmptyTuple(EmptyTupleStruct()));
    fn test_enum_newtype_struct_empty_field(AllVariantKinds::NewTypeStructEmptyField(EmptyFieldStruct {}));
    fn test_enum_newtype_struct_field_one(AllVariantKinds::NewTypeStructFieldOne(FieldStructOne { x: 9 }));
    fn test_enum_newtype_struct_field_two(AllVariantKinds::NewTypeStructFieldTwo(FieldStructTwo { x: 10, y : 11 }));
    fn test_enum_newtype_option_none(AllVariantKinds::NewTypeOption(None));
    fn test_enum_newtype_option_unit(AllVariantKinds::NewTypeOption(Some(Box::new(AllVariantKinds::Unit))));
    fn test_enum_newtype_option_newtype(AllVariantKinds::NewTypeOption(Some(Box::new(AllVariantKinds::Newtype(12)))));

    fn test_enum_some_newtype_option_none(Some(AllVariantKinds::NewTypeOption(None)));
    fn test_enum_some_newtype_option_unit(Some(AllVariantKinds::NewTypeOption(Some(Box::new(AllVariantKinds::Unit)))));
    fn test_enum_some_newtype_option_newtype(Some(AllVariantKinds::NewTypeOption(Some(Box::new(AllVariantKinds::Newtype(12))))));
}

#[derive(Default, PartialEq, Eq, Debug, Serialize, Deserialize)]
struct Complex {
    map: BTreeMap<Option<Key>, Config>,
    sources: BTreeSet<Source>,
}

#[derive(Default, PartialOrd, Ord, PartialEq, Eq, Debug, Serialize, Deserialize)]
enum Key {
    #[default]
    Default,

    // Variant is never used.
    JustTag(String),

    IdTag {
        id: u32,
        tag: String,
    },
}

#[derive(Default, PartialOrd, Ord, PartialEq, Eq, Debug, Serialize, Deserialize)]
enum Source {
    #[default]
    User,

    // Skipping tuple fields doesn't emit `skip_field`, so these should be treated as different
    // variants.
    Host(
        String,
        #[serde(default = "http_default", skip_serializing_if = "is_http")] u16,
    ),
}

#[derive(Default, PartialEq, Eq, Debug, Serialize, Deserialize)]
struct Settings {
    source: Option<Source>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    flags: Option<u64>,

    #[serde(default, skip_serializing_if = "String::is_empty")]
    name: String,

    #[serde(default, skip_serializing_if = "String::is_empty")]
    description: String,
}

#[derive(Default, PartialEq, Eq, Debug, Serialize, Deserialize)]
struct Config {
    encoded: bool,

    // This field will always be skipped.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    legacy_flags: Vec<u64>,

    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    settings: Vec<Settings>,
}

fn is_http(port: &u16) -> bool {
    *port == 80
}

fn http_default() -> u16 {
    80
}

#[test]
fn test_complex_default() {
    check_roundtrip(&Complex::default());
}

#[test]
fn test_complex_full() {
    check_roundtrip(&Complex {
        map: btreemap! {
            None => Config::default(),
            Some(Key::IdTag { tag:"global".to_owned(), id: 1 }) => Config {
                encoded: true,
                settings: vec![
                    Settings {
                        source: Some(Source::User),
                        flags: None,
                        name: "cristi".to_owned(),
                        description: String::new(),
                    },
                    Settings {
                        source: None,
                        flags: Some(1234),
                        name: "bob".to_owned(),
                        description: "this is a description".to_owned()
                    },
                ],
                legacy_flags: Vec::new(),
            },
            Some(Key::IdTag { tag: "local".to_owned(), id: 2 }) => Config {
                encoded: true,
                settings: Vec::new(),
                legacy_flags: Vec::new(),
            },
            Some(Key::Default) => Config {
                encoded: false,
                settings: vec![
                    Settings {
                        source: Some(Source::User),
                        flags: Some(111),
                        name: "marty".to_owned(),
                        description: "another description".to_owned()
                    },
                ],
                legacy_flags: Vec::new(),
            },
        },
        sources: btreeset![
            Source::User,
            Source::Host("example.com".to_owned(), 443),
            Source::Host("google.com".to_owned(), 80),
        ],
    });
}
