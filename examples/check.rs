use bincol::Described;
use itertools::sorted;
use ron::ser::PrettyConfig;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
struct A {
    #[serde(skip_serializing_if = "Vec::is_empty")]
    b: Vec<Option<B>>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    c: Vec<Option<C>>,
}

#[derive(Debug, Serialize, Deserialize)]
enum B {
    X(u32),
    Y(u64),
    Z,
}

#[derive(Debug, Serialize, Deserialize)]
struct C {
    #[serde(skip_serializing_if = "Option::is_none")]
    x: Option<(u32, u32)>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    y: Vec<D>,
}

#[derive(Debug, Serialize, Deserialize)]
struct D {
    #[serde(skip_serializing_if = "Option::is_none")]
    z: Option<u32>,

    #[serde(skip_serializing_if = "Option::is_none")]
    w: Option<f32>,
}

#[derive(Debug, Serialize, Deserialize)]
struct E(u32);

#[derive(Debug, Serialize, Deserialize)]
struct F(Option<u32>);

#[derive(Debug, Serialize, Deserialize)]
enum Untagged {
    U32(u32),
    F32(f32),
    OptionU32(Option<u32>),
    OptionE(Option<E>),
    F(F),
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(transparent)]
struct Value<T>(T);

#[derive(Debug, Serialize, Deserialize)]
struct Simple {
    a: Option<u32>,
}

fn main() {
    let mut value = Value(vec![Simple { a: None }, Simple { a: Some(10) }]);
    //let original = Value(vec![
    //    Untagged::U32(10),
    //    Untagged::F32(0.5),
    //    Untagged::OptionU32(Some(10)),
    //    Untagged::OptionE(Some(E(20))),
    //    Untagged::OptionE(None),
    //    Untagged::F(F(None)),
    //    Untagged::F(F(Some(30))),
    //]);
    eprintln!("ORIGINAL:\n{:#?}\n\n", value);
    eprintln!(
        "DESCRIBED:\n{}\n\n",
        ron::ser::to_string_pretty(
            &Described(&value),
            PrettyConfig::default()
                .struct_names(true)
                .number_suffixes(true)
        )
        .unwrap()
    );
    let serialized = bitcode::serialize(&Described(value)).unwrap();
    value = bitcode::deserialize::<Described<Value<_>>>(&serialized)
        .unwrap()
        .0;
    eprintln!("DESERIALIZED:\n{:#?}\n\n", value,);
}
