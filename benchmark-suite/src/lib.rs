use serde::{Deserialize, Serialize};
use serde_describe::SelfDescribed;

pub mod github_events;
pub mod level;

pub use github_events::GithubEvents;
pub use level::{DynamicLevel, FixedLevel};

#[doc(hidden)]
pub use serde_describe as __private_serde_describe;

pub trait Dataset: serde::Serialize + serde::de::DeserializeOwned + Sized {
    fn name(&self) -> String;
    fn is_dynamic(&self) -> bool;
    fn load() -> Self;
}

pub trait Format {
    fn name(&self) -> String;
    fn is_self_describing(&self) -> bool;
    fn serialize_to_bytes<DatasetT: Serialize>(&self, dataset: &DatasetT) -> Vec<u8>;
    fn deserialize_from_bytes<'de, DatasetT: Deserialize<'de>>(&self, data: &'de [u8]) -> DatasetT;
}

pub struct Bitcode;
impl Format for Bitcode {
    fn name(&self) -> String {
        "bitcode".to_owned()
    }

    fn is_self_describing(&self) -> bool {
        false
    }

    fn serialize_to_bytes<DatasetT: Serialize>(&self, dataset: &DatasetT) -> Vec<u8> {
        bitcode::serialize(dataset).unwrap()
    }

    fn deserialize_from_bytes<'de, DatasetT: Deserialize<'de>>(&self, data: &'de [u8]) -> DatasetT {
        bitcode::deserialize(data).unwrap()
    }
}

pub struct Postcard;
impl Format for Postcard {
    fn name(&self) -> String {
        "postcard".to_owned()
    }

    fn is_self_describing(&self) -> bool {
        false
    }

    fn serialize_to_bytes<DatasetT: Serialize>(&self, dataset: &DatasetT) -> Vec<u8> {
        postcard::to_stdvec(dataset).unwrap()
    }

    fn deserialize_from_bytes<'de, DatasetT: Deserialize<'de>>(&self, data: &'de [u8]) -> DatasetT {
        postcard::from_bytes(data).unwrap()
    }
}

pub struct SerdeCbor;
impl Format for SerdeCbor {
    fn name(&self) -> String {
        "serde_cbor".to_owned()
    }

    fn is_self_describing(&self) -> bool {
        true
    }

    fn serialize_to_bytes<DatasetT: Serialize>(&self, dataset: &DatasetT) -> Vec<u8> {
        serde_cbor::to_vec(dataset).unwrap()
    }

    fn deserialize_from_bytes<'de, DatasetT: Deserialize<'de>>(&self, data: &'de [u8]) -> DatasetT {
        serde_cbor::from_slice(data).unwrap()
    }
}

pub struct SerdeJson;
impl Format for SerdeJson {
    fn name(&self) -> String {
        "serde_json".to_owned()
    }

    fn is_self_describing(&self) -> bool {
        true
    }

    fn serialize_to_bytes<DatasetT: Serialize>(&self, dataset: &DatasetT) -> Vec<u8> {
        serde_json::to_vec(dataset).unwrap()
    }

    fn deserialize_from_bytes<'de, DatasetT: Deserialize<'de>>(&self, data: &'de [u8]) -> DatasetT {
        serde_json::from_slice(data).unwrap()
    }
}

pub struct Pot;
impl Format for Pot {
    fn name(&self) -> String {
        "pot".to_owned()
    }

    fn is_self_describing(&self) -> bool {
        true
    }

    fn serialize_to_bytes<DatasetT: Serialize>(&self, dataset: &DatasetT) -> Vec<u8> {
        pot::to_vec(dataset).unwrap()
    }

    fn deserialize_from_bytes<'de, DatasetT: Deserialize<'de>>(&self, data: &'de [u8]) -> DatasetT {
        pot::from_slice(data).unwrap()
    }
}

impl<FormatT> Format for SelfDescribed<&FormatT>
where
    FormatT: Format,
{
    fn name(&self) -> String {
        format!("self-described-{}", self.0.name())
    }

    fn is_self_describing(&self) -> bool {
        true
    }

    fn serialize_to_bytes<DatasetT: Serialize>(&self, dataset: &DatasetT) -> Vec<u8> {
        self.0.serialize_to_bytes(&SelfDescribed(dataset))
    }

    fn deserialize_from_bytes<'de, DatasetT: Deserialize<'de>>(&self, data: &'de [u8]) -> DatasetT {
        self.0
            .deserialize_from_bytes::<SelfDescribed<DatasetT>>(data)
            .0
    }
}

#[macro_export]
macro_rules! run_matrix {
    ($function:ident($($arg:expr),*)) => {
        run_matrix!(
            @start $function,
            ($($arg,)*),
            [$crate::GithubEvents, $crate::FixedLevel, $crate::DynamicLevel],
            [$crate::Bitcode, $crate::Postcard, $crate::SerdeCbor, $crate::Pot, $crate::SerdeJson],
        );
    };
    (@start $function:ident, $args:tt, [$($dataset:path),+], $formats:tt,) => {
        $(
            run_matrix!(@load $function, $args, $dataset, $formats);
        )+
    };
    (@load $function:ident, $args:tt, $dataset:path, $formats:tt) => {
        {
            let dataset = <$dataset as $crate::Dataset>::load();
            run_matrix!(@dataset $function, $args, dataset, $formats);
        }
    };
    (@dataset $function:ident, $args:tt, $dataset:path, [$($format:path),+]) => {
        $(
            run_matrix!(@format $function, $args, $dataset, $format);
        )+
    };
    (@format $function:ident, ($($arg:expr,)*), $dataset:path, $format:path) => {
        if $format.is_self_describing() || !$dataset.is_dynamic() {
            $function($($arg,)* &$format, &$dataset);
        }
        if !$format.is_self_describing() {
            $function($($arg,)* &$crate::__private_serde_describe::SelfDescribed(&$format), &$dataset);
        }
    };
}
