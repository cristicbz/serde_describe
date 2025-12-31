use ron::ser::PrettyConfig;
use serde::{Deserialize, Serialize};
use serde_describe::SelfDescribed;

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
    #[serde(skip_serializing_if = "Option::is_none")]
    a: Option<u32>,

    b: Option<u32>,

    #[serde(skip_serializing_if = "Option::is_none")]
    c: Option<u32>,
}

#[derive(Serialize, Deserialize, Debug)]
struct WithTransform {
    #[serde(default, skip_serializing_if = "is_default")]
    transform: Transform,
}

#[derive(Serialize, Deserialize, PartialEq, Debug)]
struct Transform {
    #[serde(default, skip_serializing_if = "is_default")]
    translation: Vec3,

    #[serde(default, skip_serializing_if = "is_default")]
    rotation: Quat,

    #[serde(default = "ones", skip_serializing_if = "is_ones")]
    scale: Vec3,
}

impl Default for Transform {
    #[inline]
    fn default() -> Self {
        Transform {
            translation: Vec3::default(),
            rotation: Quat::default(),
            scale: ones(),
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize, PartialEq)]
struct Vec3(f32, f32, f32);

#[derive(Debug, Serialize, Deserialize, PartialEq, Default)]
struct Quat(f32, f32, f32, f32);

fn main() {
    let mut value = Value(vec![
        WithTransform {
            transform: Transform::default(),
        },
        WithTransform {
            transform: Transform {
                translation: Vec3(1.0, 0.0, 0.0),
                ..Default::default()
            },
        },
        WithTransform {
            transform: Transform {
                rotation: Quat(2.0, 2.0, 2.0, 2.0),
                ..Default::default()
            },
        },
        WithTransform {
            transform: Transform {
                translation: Vec3(1.0, 0.0, 0.0),
                rotation: Quat(2.0, 2.0, 2.0, 2.0),
                ..Default::default()
            },
        },
    ]);
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
            &SelfDescribed(&value),
            PrettyConfig::default()
                .struct_names(true)
                .number_suffixes(true)
        )
        .unwrap()
    );
    let serialized = bitcode::serialize(&SelfDescribed(value)).unwrap();
    value = bitcode::deserialize::<SelfDescribed<Value<_>>>(&serialized)
        .unwrap()
        .0;
    eprintln!("DESERIALIZED:\n{:#?}\n\n", value,);
}

#[inline]
fn is_default<T: Default + PartialEq>(value: &T) -> bool {
    *value == T::default()
}

#[inline]
fn ones() -> Vec3 {
    Vec3(1., 1., 1.)
}

#[inline]
fn is_ones(value: &Vec3) -> bool {
    value == &Vec3(1., 1., 1.)
}
