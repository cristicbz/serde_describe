use ron::ser::PrettyConfig;
use serde::Serialize;

#[derive(Serialize)]
struct A {
    #[serde(skip_serializing_if = "Vec::is_empty")]
    b: Vec<Option<B>>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    c: Vec<Option<C>>,
}

#[derive(Serialize)]
enum B {
    X(u32),
    Y(u64),
    Z,
}

#[derive(Serialize)]
struct C {
    #[serde(skip_serializing_if = "Option::is_none")]
    x: Option<(u32, u32)>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    y: Vec<D>,
}

#[derive(Serialize)]
struct D {
    #[serde(skip_serializing_if = "Option::is_none")]
    z: Option<u32>,

    #[serde(skip_serializing_if = "Option::is_none")]
    w: Option<f32>,
}

fn main() {
    let value = vec![
        C {
            x: None,
            y: Vec::new(),
        },
        C {
            x: Some((1, 2)),
            y: vec![D { z: None, w: None }],
        },
        //C {
        //    x: Some((1, 2)),
        //    y: vec![D { z: None, w: None }],
        //},
    ];
    println!(
        "{}",
        ron::ser::to_string_pretty(
            &bincol::to_value(&value).unwrap(),
            PrettyConfig::default()
                .struct_names(true)
                .number_suffixes(true)
        )
        .unwrap()
    );
}
