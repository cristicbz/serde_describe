use serde::{Deserialize, Serialize};

impl crate::Dataset for DynamicLevel {
    fn name(&self) -> String {
        "dynamic_level".to_owned()
    }

    fn is_dynamic(&self) -> bool {
        true
    }

    fn load() -> Self {
        serde_json::from_str(JSON).unwrap()
    }
}

impl crate::Dataset for FixedLevel {
    fn name(&self) -> String {
        "fixed_level".to_owned()
    }

    fn is_dynamic(&self) -> bool {
        false
    }

    fn load() -> Self {
        serde_json::from_str(JSON).unwrap()
    }
}

#[derive(Serialize, Deserialize, PartialEq, Debug, Default)]
pub struct UVec3(pub [u32; 3]);

#[derive(Serialize, Deserialize, PartialEq, Debug, Default)]
pub struct Quat(pub [f32; 4]);

#[derive(Serialize, Deserialize, PartialEq, Debug, Default)]
pub struct Vec3(pub [f32; 3]);

#[derive(Serialize, Deserialize)]
pub enum DynamicLevel {
    V1(DynamicLevelV1),
}

#[derive(Serialize, Deserialize, Default)]
pub struct DynamicLevelV1 {
    pub size: UVec3,

    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub cells: Vec<DynamicCell>,

    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub scene_assets: Vec<String>,
}

#[derive(Serialize, Deserialize)]
pub struct DynamicCell {
    pub position: UVec3,

    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub floors: Vec<DynamicCellFloor>,
}

#[derive(Serialize, Deserialize)]
pub struct DynamicCellFloor {
    pub i_scene: usize,

    #[serde(default, skip_serializing_if = "is_default")]
    pub transform: DynamicTransform,
}

#[derive(Serialize, Deserialize, PartialEq)]
pub struct DynamicTransform {
    #[serde(default, skip_serializing_if = "is_default")]
    pub translation: Vec3,

    #[serde(default, skip_serializing_if = "is_default")]
    pub rotation: Quat,

    #[serde(default = "ones", skip_serializing_if = "is_ones")]
    pub scale: Vec3,
}

impl Default for DynamicTransform {
    #[inline]
    fn default() -> Self {
        Self {
            translation: Vec3([0.0; 3]),
            rotation: Quat([0.0; 4]),
            scale: Vec3([1.0; 3]),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub enum FixedLevel {
    V1(FixedLevelV1),
}

#[derive(Serialize, Deserialize, Default)]
pub struct FixedLevelV1 {
    pub size: UVec3,

    #[serde(default)]
    pub cells: Vec<FixedCell>,

    #[serde(default)]
    pub scene_assets: Vec<String>,
}

#[derive(Serialize, Deserialize)]
pub struct FixedCell {
    pub position: UVec3,

    #[serde(default)]
    pub floors: Vec<FixedCellFloor>,
}

#[derive(Serialize, Deserialize)]
pub struct FixedCellFloor {
    pub i_scene: usize,

    #[serde(default)]
    pub transform: FixedTransform,
}

#[derive(Serialize, Deserialize, PartialEq)]
pub struct FixedTransform {
    #[serde(default)]
    pub translation: Vec3,

    #[serde(default)]
    pub rotation: Quat,

    #[serde(default = "ones")]
    pub scale: Vec3,
}

impl Default for FixedTransform {
    #[inline]
    fn default() -> Self {
        Self {
            translation: Vec3([0.0; 3]),
            rotation: Quat([0.0; 4]),
            scale: Vec3([1.0; 3]),
        }
    }
}

#[inline]
fn is_default<T: Default + PartialEq>(value: &T) -> bool {
    *value == T::default()
}

#[inline]
fn ones() -> Vec3 {
    Vec3([1.0; 3])
}

#[inline]
fn is_ones(value: &Vec3) -> bool {
    value == &Vec3([1.0; 3])
}

const JSON: &str = include_str!("../data/level.json");
