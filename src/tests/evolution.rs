use super::helpers::{StrictDeserialize, check_evolution_fails, check_evolution_ok};
use serde::{
    Deserialize, Serialize,
    de::{DeserializeOwned, IgnoredAny},
};
use serde_bytes::ByteBuf;
use std::{fmt::Debug, marker::PhantomData};

macro_rules! tests {
    ($(fn $name:ident<$t:ty$(, $u:ty)?>($check:ident $(, $values:expr)?);)+) => {
        $(
            tests!{@one; $name; $t; $($u)?; $check; $($values)?;}
        )+
    };
    (@one; $name:ident; $t:ty;; $check:ident; $($values:expr)?;) => {
        tests!{@one; $name; $t; $t; $check; $($values)?;}
    };
    (@one; $name:ident; $t:ty; $u:ty; $check:ident; $values:expr;) => {
        #[test]
        fn $name() {
            $check::<_, $t, $u>($values);
        }
    };
    (@one; $name:ident; $t:ty; $u:ty; $check:ident;;) => {
        #[test]
        fn $name() {
            $check::<_, $t, $u>(&[String::new(), "hello".to_owned()]);
        }
    };
    (@one; $name:ident; $t:ty; $u:ty; $check:ident; $values:expr;) => {
        #[test]
        fn $name() {
            $check::<_, $t, $u>($values);
        }
    };
}

tests! {
    // Wrapper evolution
    fn test_string_to_ignored_any_ok<Strict, IgnoredAny>(check_ok_one_way);
    fn test_string_and_newtype_equal<Strict, NewType>(check_equals_two_way);
    fn test_string_and_tuple_struct_fail<Strict, TupleStruct>(check_fails_two_way);
    fn test_string_and_field_struct_fail<Strict, FieldStruct>(check_fails_two_way);
    fn test_string_and_option_some_equal<Strict, Optional>(check_equals_two_way);
    fn test_swap_newtype_and_option_equal<NewType<Optional>, Optional<NewType>>(check_equals_two_way);
    fn test_transparent_newtypes_optionals<NewType<Optional<NewType<Optional<NewType>>>>, NewType<NewType<NewType>>>(check_equals_two_way);

    fn test_newtype_variant_to_string_fail<NewTypeVariant, Strict>(check_fails_one_way);
    fn test_tuple_variant_to_string_fail<TupleVariant, Strict>(check_fails_one_way);
    fn test_struct_variant_to_string_fail<StructVariant, Strict>(check_fails_one_way);
    fn test_option_none_to_string_fails<OptionNone, Strict>(check_fails_one_way);
    fn test_unit_to_string_fails<Unit, Strict>(check_fails_one_way);
    fn test_unit_struct_to_string_fails<UnitStruct, Strict>(check_fails_one_way);
    fn test_unit_variant_to_string_fails<UnitVariant, Strict>(check_fails_one_way);

    // Signed integers compatible with each other
    fn test_i8_i16_equal<Strict<i8>, Strict<i16>>(check_equals_two_way, &[0i8, i8::MIN, i8::MAX]);
    fn test_i8_i32_equal<Strict<i8>, Strict<i32>>(check_equals_two_way, &[0i8, i8::MIN, i8::MAX]);
    fn test_i8_i64_equal<Strict<i8>, Strict<i64>>(check_equals_two_way, &[0i8, i8::MIN, i8::MAX]);
    fn test_i8_i128_equal<Strict<i8>, Strict<i128>>(check_equals_two_way, &[0i8, i8::MIN, i8::MAX]);

    fn test_i16_i32_equal<Strict<i16>, Strict<i32>>(check_equals_two_way, &[0i16, i16::MIN, i16::MAX]);
    fn test_i16_i64_equal<Strict<i16>, Strict<i64>>(check_equals_two_way, &[0i16, i16::MIN, i16::MAX]);
    fn test_i16_i128_equal<Strict<i16>, Strict<i128>>(check_equals_two_way, &[0i16, i16::MIN, i16::MAX]);

    fn test_i32_i64_equal<Strict<i32>, Strict<i64>>(check_equals_two_way, &[0i32, i32::MIN, i32::MAX]);
    fn test_i32_i128_equal<Strict<i32>, Strict<i128>>(check_equals_two_way, &[0i32, i32::MIN, i32::MAX]);

    fn test_i64_i128_equal<Strict<i64>, Strict<i128>>(check_equals_two_way, &[0i64, i64::MIN, i64::MAX]);
    //
    //// Unsigned integers compatible with each other
    fn test_u8_u16_equal<Strict<u8>, Strict<u16>>(check_equals_two_way, &[0u8, u8::MAX]);
    fn test_u8_u32_equal<Strict<u8>, Strict<u32>>(check_equals_two_way, &[0u8, u8::MAX]);
    fn test_u8_u64_equal<Strict<u8>, Strict<u64>>(check_equals_two_way, &[0u8, u8::MAX]);
    fn test_u8_u128_equal<Strict<u8>, Strict<u128>>(check_equals_two_way, &[0u8, u8::MAX]);

    fn test_u16_u32_equal<Strict<u16>, Strict<u32>>(check_equals_two_way, &[0u16, u16::MAX]);
    fn test_u16_u64_equal<Strict<u16>, Strict<u64>>(check_equals_two_way, &[0u16, u16::MAX]);
    fn test_u16_u128_equal<Strict<u16>, Strict<u128>>(check_equals_two_way, &[0u16, u16::MAX]);

    fn test_u32_u64_equal<Strict<u32>, Strict<u64>>(check_equals_two_way, &[0u32, u32::MAX]);
    fn test_u32_u128_equal<Strict<u32>, Strict<u128>>(check_equals_two_way, &[0u32, u32::MAX]);

    fn test_u64_u128_equal<Strict<u64>, Strict<u128>>(check_equals_two_way, &[0u64, u64::MAX]);

    // Unsigned and signed integers compatible
    fn test_u8_i8_equal<Strict<u8>, Strict<i8>>(check_equals_two_way, &[0u8, i8::MAX as u8]);
    fn test_u8_i16_equal<Strict<u8>, Strict<i16>>(check_equals_two_way, &[0u8, u8::MAX]);
    fn test_u8_i32_equal<Strict<u8>, Strict<i32>>(check_equals_two_way, &[0u8, u8::MAX]);
    fn test_u8_i64_equal<Strict<u8>, Strict<i64>>(check_equals_two_way, &[0u8, u8::MAX]);
    fn test_u8_i128_equal<Strict<u8>, Strict<i128>>(check_equals_two_way, &[0u8, u8::MAX]);

    fn test_u16_i8_equal<Strict<u16>, Strict<i8>>(check_equals_two_way, &[0u16, i8::MAX as u16]);
    fn test_u16_i16_equal<Strict<u16>, Strict<i16>>(check_equals_two_way, &[0u16, i16::MAX as u16]);
    fn test_u16_i32_equal<Strict<u16>, Strict<i32>>(check_equals_two_way, &[0u16, u16::MAX]);
    fn test_u16_i64_equal<Strict<u16>, Strict<i64>>(check_equals_two_way, &[0u16, u16::MAX]);
    fn test_u16_i128_equal<Strict<u16>, Strict<i128>>(check_equals_two_way, &[0u16, u16::MAX]);

    fn test_u32_i8_equal<Strict<u32>, Strict<i8>>(check_equals_two_way, &[0u32, i8::MAX as u32]);
    fn test_u32_i16_equal<Strict<u32>, Strict<i16>>(check_equals_two_way, &[0u32, i16::MAX as u32]);
    fn test_u32_i32_equal<Strict<u32>, Strict<i32>>(check_equals_two_way, &[0u32, i32::MAX as u32]);
    fn test_u32_i64_equal<Strict<u32>, Strict<i64>>(check_equals_two_way, &[0u32, u32::MAX]);
    fn test_u32_i128_equal<Strict<u32>, Strict<i128>>(check_equals_two_way, &[0u32, u32::MAX]);

    fn test_u64_i8_equal<Strict<u64>, Strict<i8>>(check_equals_two_way, &[0u64, i8::MAX as u64]);
    fn test_u64_i16_equal<Strict<u64>, Strict<i16>>(check_equals_two_way, &[0u64, i16::MAX as u64]);
    fn test_u64_i32_equal<Strict<u64>, Strict<i32>>(check_equals_two_way, &[0u64, i32::MAX as u64]);
    fn test_u64_i64_equal<Strict<u64>, Strict<i64>>(check_equals_two_way, &[0u64, i64::MAX as u64]);
    fn test_u64_i128_equal<Strict<u64>, Strict<i128>>(check_equals_two_way, &[0u64, u64::MAX]);

    fn test_u128_i8_equal<Strict<u128>, Strict<i8>>(check_equals_two_way, &[0u128, i8::MAX as u128]);
    fn test_u128_i16_equal<Strict<u128>, Strict<i16>>(check_equals_two_way, &[0u128, i16::MAX as u128]);
    fn test_u128_i32_equal<Strict<u128>, Strict<i32>>(check_equals_two_way, &[0u128, i32::MAX as u128]);
    fn test_u128_i64_equal<Strict<u128>, Strict<i64>>(check_equals_two_way, &[0u128, i64::MAX as u128]);
    fn test_u128_i128_equal<Strict<u128>, Strict<i128>>(check_equals_two_way, &[0u128, i128::MAX as u128]);

    // Out of range unsigned to unsigned integers not compatible visits the larger integer.
    fn test_u16_to_u8_fallback<Strict<u16>, Fallback<u8, u16>>(check_equals_one_way, &[u8::MAX as u16 + 1, u16::MAX]);
    fn test_u32_to_u8_fallback<Strict<u32>, Fallback<u8, u32>>(check_equals_one_way, &[u8::MAX as u32 + 1, u32::MAX]);
    fn test_u64_to_u8_fallback<Strict<u64>, Fallback<u8, u64>>(check_equals_one_way, &[u8::MAX as u64 + 1, u64::MAX]);
    fn test_u128_to_u8_fallback<Strict<u128>, Fallback<u8, u128>>(check_equals_one_way, &[u8::MAX as u128 + 1, u128::MAX]);

    fn test_u32_to_u16_fallback<Strict<u32>, Fallback<u16, u32>>(check_equals_one_way, &[u16::MAX as u32 + 1, u32::MAX]);
    fn test_u64_to_u16_fallback<Strict<u64>, Fallback<u16, u64>>(check_equals_one_way, &[u16::MAX as u64 + 1, u64::MAX]);
    fn test_u128_to_u16_fallback<Strict<u128>, Fallback<u16, u128>>(check_equals_one_way, &[u16::MAX as u128 + 1, u128::MAX]);

    fn test_u64_to_u32_fallback<Strict<u64>, Fallback<u32, u64>>(check_equals_one_way, &[u32::MAX as u64 + 1, u64::MAX]);
    fn test_u128_to_u32_fallback<Strict<u128>, Fallback<u32, u128>>(check_equals_one_way, &[u32::MAX as u128 + 1, u128::MAX]);

    fn test_u128_to_u64_fallback<Strict<u128>, Fallback<u64, u128>>(check_equals_one_way, &[u64::MAX as u128 + 1, u128::MAX]);

    // Out of range signed to signed integers not compatible
    fn test_i16_to_i8_fallback<Strict<i16>, Fallback<i8, i16>>(check_equals_one_way, &[i8::MAX as i16 + 1, i16::MAX]);
    fn test_i32_to_i8_fallback<Strict<i32>, Fallback<i8, i32>>(check_equals_one_way, &[i8::MAX as i32 + 1, i32::MAX]);
    fn test_i64_to_i8_fallback<Strict<i64>, Fallback<i8, i64>>(check_equals_one_way, &[i8::MAX as i64 + 1, i64::MAX]);
    fn test_i128_to_i8_fallback<Strict<i128>, Fallback<i8, i128>>(check_equals_one_way, &[i8::MAX as i128 + 1, i128::MAX]);

    fn test_i32_to_i16_fallback<Strict<i32>, Fallback<i16, i32>>(check_equals_one_way, &[i16::MAX as i32 + 1, i32::MAX]);
    fn test_i64_to_i16_fallback<Strict<i64>, Fallback<i16, i64>>(check_equals_one_way, &[i16::MAX as i64 + 1, i64::MAX]);
    fn test_i128_to_i16_fallback<Strict<i128>, Fallback<i16, i128>>(check_equals_one_way, &[i16::MAX as i128 + 1, i128::MAX]);

    fn test_i64_to_i32_fallback<Strict<i64>, Fallback<i32, i64>>(check_equals_one_way, &[i32::MAX as i64 + 1, i64::MAX]);
    fn test_i128_to_i32_fallback<Strict<i128>, Fallback<i32, i128>>(check_equals_one_way, &[i32::MAX as i128 + 1, i128::MAX]);

    fn test_i128_to_i64_fallback<Strict<i128>, Fallback<i64, i128>>(check_equals_one_way, &[i64::MAX as i128 + 1, i128::MAX]);

    // Out of range signed to unsigned integers not compatible
    fn test_i8_to_u8_fallback<Strict<i8>, Fallback<u8, i8>>(check_equals_one_way, &[-1i8]);
    fn test_i16_to_u8_fallback<Strict<i16>, Fallback<u8, i16>>(check_equals_one_way, &[-1i16, u8::MAX as i16 + 1]);
    fn test_i32_to_u8_fallback<Strict<i32>, Fallback<u8, i32>>(check_equals_one_way, &[-1i32, u8::MAX as i32 + 1]);
    fn test_i64_to_u8_fallback<Strict<i64>, Fallback<u8, i64>>(check_equals_one_way, &[-1i64, u8::MAX as i64 + 1]);
    fn test_i128_to_u8_fallback<Strict<i128>, Fallback<u8, i128>>(check_equals_one_way, &[-1i128, u8::MAX as i128 + 1]);

    fn test_i8_to_u16_fallback<Strict<i8>, Fallback<u16, i8>>(check_equals_one_way, &[-1i8]);
    fn test_i16_to_u16_fallback<Strict<i16>, Fallback<u16, i16>>(check_equals_one_way, &[-1i16]);
    fn test_i32_to_u16_fallback<Strict<i32>, Fallback<u16, i32>>(check_equals_one_way, &[-1i32, u16::MAX as i32 + 1]);
    fn test_i64_to_u16_fallback<Strict<i64>, Fallback<u16, i64>>(check_equals_one_way, &[-1i64, u16::MAX as i64 + 1]);
    fn test_i128_to_u16_fallback<Strict<i128>, Fallback<u16, i128>>(check_equals_one_way, &[-1i128, u16::MAX as i128 + 1]);

    fn test_i8_to_u32_fallback<Strict<i8>, Fallback<u32, i8>>(check_equals_one_way, &[-1i8]);
    fn test_i16_to_u32_fallback<Strict<i16>, Fallback<u32, i16>>(check_equals_one_way, &[-1i16]);
    fn test_i32_to_u32_fallback<Strict<i32>, Fallback<u32, i32>>(check_equals_one_way, &[-1i32]);
    fn test_i64_to_u32_fallback<Strict<i64>, Fallback<u32, i64>>(check_equals_one_way, &[-1i64, u32::MAX as i64 + 1]);
    fn test_i128_to_u32_fallback<Strict<i128>, Fallback<u32, i128>>(check_equals_one_way, &[-1i128, u32::MAX as i128 + 1]);

    fn test_i8_to_u64_fallback<Strict<i8>, Fallback<u64, i8>>(check_equals_one_way, &[-1i8]);
    fn test_i16_to_u64_fallback<Strict<i16>, Fallback<u64, i16>>(check_equals_one_way, &[-1i16]);
    fn test_i32_to_u64_fallback<Strict<i32>, Fallback<u64, i32>>(check_equals_one_way, &[-1i32]);
    fn test_i64_to_u64_fallback<Strict<i64>, Fallback<u64, i64>>(check_equals_one_way, &[-1i64]);
    fn test_i128_to_u64_fallback<Strict<i128>, Fallback<u64, i128>>(check_equals_one_way, &[-1i128, u64::MAX as i128 + 1]);

    fn test_i8_to_u128_fallback<Strict<i8>, Fallback<u128, i8>>(check_equals_one_way, &[-1i8]);
    fn test_i16_to_u128_fallback<Strict<i16>, Fallback<u128, i16>>(check_equals_one_way, &[-1i16]);
    fn test_i32_to_u128_fallback<Strict<i32>, Fallback<u128, i32>>(check_equals_one_way, &[-1i32]);
    fn test_i64_to_u128_fallback<Strict<i64>, Fallback<u128, i64>>(check_equals_one_way, &[-1i64]);
    fn test_i128_to_u128_fallback<Strict<i128>, Fallback<u128, i128>>(check_equals_one_way, &[-1i128]);

    // Out of range unsigned to signed integers not compatible
    fn test_u8_to_i8_fallback<Strict<u8>, Fallback<i8, u8>>(check_equals_one_way, &[i8::MAX as u8 + 1, u8::MAX]);

    fn test_u16_to_i8_fallback<Strict<u16>, Fallback<i8, u16>>(check_equals_one_way, &[i8::MAX as u16 +1, u16::MAX]);
    fn test_u16_to_i16_fallback<Strict<u16>, Fallback<i16, u16>>(check_equals_one_way, &[i16::MAX as u16 + 1, u16::MAX]);

    fn test_u32_to_i8_fallback<Strict<u32>, Fallback<i8, u32>>(check_equals_one_way, &[i8::MAX as u32 +1, u32::MAX]);
    fn test_u32_to_i16_fallback<Strict<u32>, Fallback<i16, u32>>(check_equals_one_way, &[i16::MAX as u32 + 1, u32::MAX]);
    fn test_u32_to_i32_fallback<Strict<u32>, Fallback<i32, u32>>(check_equals_one_way, &[i32::MAX as u32 + 1, u32::MAX]);

    fn test_u64_to_i8_fallback<Strict<u64>, Fallback<i8, u64>>(check_equals_one_way, &[i8::MAX as u64 +1, u64::MAX]);
    fn test_u64_to_i16_fallback<Strict<u64>, Fallback<i16, u64>>(check_equals_one_way, &[i16::MAX as u64 +1, u64::MAX]);
    fn test_u64_to_i32_fallback<Strict<u64>, Fallback<i32, u64>>(check_equals_one_way, &[i32::MAX as u64 +1, u64::MAX]);

    fn test_u128_to_i8_fallback<Strict<u128>, Fallback<i8, u128>>(check_equals_one_way, &[i8::MAX as u128 +1, u128::MAX]);
    fn test_u128_to_i16_fallback<Strict<u128>, Fallback<i16, u128>>(check_equals_one_way, &[i16::MAX as u128 + 1, u128::MAX]);
    fn test_u128_to_i32_fallback<Strict<u128>, Fallback<i32, u128>>(check_equals_one_way, &[i32::MAX as u128 + 1, u128::MAX]);
    fn test_u128_to_i64_fallback<Strict<u128>, Fallback<i64, u128>>(check_equals_one_way, &[i64::MAX as u128 + 1, u128::MAX]);
    fn test_u128_to_i128_fallback<Strict<u128>, Fallback<i128, u128>>(check_equals_one_way, &[i128::MAX as u128 + 1, u128::MAX]);

    // f32 -> f64 should work just fine.
    fn test_f32_to_f64_equal<Strict<f32>, Strict<f64>>(check_equals_one_way, &[
        0.0f32, f32::MIN, f32::MAX, f32::INFINITY, f32::NEG_INFINITY,
    ]);

    // f64 -> f32 always falls back to f64, to avoid lossy conversions.
    fn test_f64_to_f32_fallback<Strict<f64>, Fallback<f32, f64>>(check_equals_one_way, &[
        0.0f64, 1.0f64, -1.0f64, f64::MIN, f64::MAX, f64::INFINITY, f64::NEG_INFINITY,
    ]);

    // Integers and floats compatible.
    fn test_u8_f32_equal<Strict<u8>, Strict<f32>>(check_equals_two_way, &[0u8, u8::MIN, u8::MAX]);
    fn test_u16_f32_equal<Strict<u16>, Strict<f32>>(check_equals_two_way, &[0u16, u16::MIN, u16::MAX]);
    fn test_u32_f32_equal<Strict<u32>, Strict<f32>>(check_equals_two_way, &[0u32, u32::MIN, u32::MAX]);
    fn test_u64_f32_equal<Strict<u64>, Strict<f32>>(check_equals_two_way, &[0u64, u64::MIN, u64::MAX]);
    fn test_u128_f32_equal<Strict<u128>, Strict<f32>>(check_equals_two_way, &[0u128, f32::MAX as u128]);

    fn test_i8_f32_equal<Strict<i8>, Strict<f32>>(check_equals_two_way, &[0i8, i8::MIN, i8::MAX]);
    fn test_i16_f32_equal<Strict<i16>, Strict<f32>>(check_equals_two_way, &[0i16, i16::MIN, i16::MAX]);
    fn test_i32_f32_equal<Strict<i32>, Strict<f32>>(check_equals_two_way, &[0i32, i32::MIN, i32::MAX]);
    fn test_i64_f32_equal<Strict<i64>, Strict<f32>>(check_equals_two_way, &[0i64, i64::MIN, i64::MAX]);
    fn test_i128_f32_equal<Strict<i128>, Strict<f32>>(check_equals_two_way, &[0i128, i128::MIN, i128::MAX]);

    fn test_u8_f64_equal<Strict<u8>, Strict<f64>>(check_equals_two_way, &[0u8, u8::MIN, u8::MAX]);
    fn test_u16_f64_equal<Strict<u16>, Strict<f64>>(check_equals_two_way, &[0u16, u16::MIN, u16::MAX]);
    fn test_u32_f64_equal<Strict<u32>, Strict<f64>>(check_equals_two_way, &[0u32, u32::MIN, u32::MAX]);
    fn test_u64_f64_equal<Strict<u64>, Strict<f64>>(check_equals_two_way, &[0u64, u64::MIN, u64::MAX]);
    fn test_u128_f64_equal<Strict<u128>, Strict<f64>>(check_equals_two_way, &[0u128, u128::MIN, u128::MAX]);

    fn test_i8_f64_equal<Strict<i8>, Strict<f64>>(check_equals_two_way, &[0i8, i8::MIN, i8::MAX]);
    fn test_i16_f64_equal<Strict<i16>, Strict<f64>>(check_equals_two_way, &[0i16, i16::MIN, i16::MAX]);
    fn test_i32_f64_equal<Strict<i32>, Strict<f64>>(check_equals_two_way, &[0i32, i32::MIN, i32::MAX]);
    fn test_i64_f64_equal<Strict<i64>, Strict<f64>>(check_equals_two_way, &[0i64, i64::MIN, i64::MAX]);
    fn test_i128_f64_equal<Strict<i128>, Strict<f64>>(check_equals_two_way, &[0i128, i128::MIN, i128::MAX]);

    // Float -> unsigned falls back on non-integer / out of bounds values.
    fn test_f32_to_u8_fallback<Strict<f32>, Fallback<u8, f32>>(check_equals_one_way, &[-1.0f32, 0.5f32, f32::INFINITY, f32::NEG_INFINITY, (u8::MAX as f32) * 2.0]);
    fn test_f32_to_u16_fallback<Strict<f32>, Fallback<u16, f32>>(check_equals_one_way, &[-1.0f32, 0.5f32, f32::INFINITY, f32::NEG_INFINITY, (u16::MAX as f32) * 2.0]);
    fn test_f32_to_u32_fallback<Strict<f32>, Fallback<u32, f32>>(check_equals_one_way, &[-1.0f32, 0.5f32, f32::INFINITY, f32::NEG_INFINITY, (u32::MAX as f32) * 2.0]);
    fn test_f32_to_u64_fallback<Strict<f32>, Fallback<u64, f32>>(check_equals_one_way, &[-1.0f32, 0.5f32, f32::INFINITY, f32::NEG_INFINITY, (u64::MAX as f32) * 2.0]);
    fn test_f32_to_u128_fallback<Strict<f32>, Fallback<u128, f32>>(check_equals_one_way, &[-1.0f32, 0.5f32, f32::INFINITY, f32::NEG_INFINITY, (u128::MAX as f32) * 2.0]);

    fn test_f64_to_u8_fallback<Strict<f64>, Fallback<u8, f64>>(check_equals_one_way, &[-1.0f64, 0.5f64, f64::INFINITY, f64::NEG_INFINITY, (u8::MAX as f64) * 2.0]);
    fn test_f64_to_u16_fallback<Strict<f64>, Fallback<u16, f64>>(check_equals_one_way, &[-1.0f64, 0.5f64, f64::INFINITY, f64::NEG_INFINITY, (u16::MAX as f64) * 2.0]);
    fn test_f64_to_u32_fallback<Strict<f64>, Fallback<u32, f64>>(check_equals_one_way, &[-1.0f64, 0.5f64, f64::INFINITY, f64::NEG_INFINITY, (u32::MAX as f64) * 2.0]);
    fn test_f64_to_u64_fallback<Strict<f64>, Fallback<u64, f64>>(check_equals_one_way, &[-1.0f64, 0.5f64, f64::INFINITY, f64::NEG_INFINITY, (u64::MAX as f64) * 2.0]);
    fn test_f64_to_u128_fallback<Strict<f64>, Fallback<u128, f64>>(check_equals_one_way, &[-1.0f64, 0.5f64, f64::INFINITY, f64::NEG_INFINITY, (u128::MAX as f64) * 2.0]);

    fn test_f32_to_i8_fallback<Strict<f32>, Fallback<i8, f32>>(check_equals_one_way, &[(i8::MIN as f32) * 2.0, 0.5f32, f32::INFINITY, f32::NEG_INFINITY, (i8::MAX as f32) * 2.0]);
    fn test_f32_to_i16_fallback<Strict<f32>, Fallback<i16, f32>>(check_equals_one_way, &[(i16::MIN as f32) * 2.0, 0.5f32, f32::INFINITY, f32::NEG_INFINITY, (i16::MAX as f32) * 2.0]);
    fn test_f32_to_i32_fallback<Strict<f32>, Fallback<i32, f32>>(check_equals_one_way, &[(i32::MIN as f32) * 2.0, 0.5f32, f32::INFINITY, f32::NEG_INFINITY, (i32::MAX as f32) * 2.0]);
    fn test_f32_to_i64_fallback<Strict<f32>, Fallback<i64, f32>>(check_equals_one_way, &[(i64::MIN as f32) * 2.0, 0.5f32, f32::INFINITY, f32::NEG_INFINITY, (i64::MAX as f32) * 2.0]);
    fn test_f32_to_i128_fallback<Strict<f32>, Fallback<i128, f32>>(check_equals_one_way, &[(i128::MIN as f32) * 2.0, 0.5f32, f32::INFINITY, f32::NEG_INFINITY, (i128::MAX as f32) * 2.0]);

    fn test_f64_to_i8_fallback<Strict<f64>, Fallback<i8, f64>>(check_equals_one_way, &[(i8::MIN as f64) * 2.0, 0.5f64, f64::INFINITY, f64::NEG_INFINITY, (i8::MAX as f64) * 2.0]);
    fn test_f64_to_i16_fallback<Strict<f64>, Fallback<i16, f64>>(check_equals_one_way, &[(i16::MIN as f64) * 2.0, 0.5f64, f64::INFINITY, f64::NEG_INFINITY, (i16::MAX as f64) * 2.0]);
    fn test_f64_to_i32_fallback<Strict<f64>, Fallback<i32, f64>>(check_equals_one_way, &[(i32::MIN as f64) * 2.0, 0.5f64, f64::INFINITY, f64::NEG_INFINITY, (i32::MAX as f64) * 2.0]);
    fn test_f64_to_i64_fallback<Strict<f64>, Fallback<i64, f64>>(check_equals_one_way, &[(i64::MIN as f64) * 2.0, 0.5f64, f64::INFINITY, f64::NEG_INFINITY, (i64::MAX as f64) * 2.0]);
    fn test_f64_to_i128_fallback<Strict<f64>, Fallback<i128, f64>>(check_equals_one_way, &[(i128::MIN as f64) * 2.0, 0.5f64, f64::INFINITY, f64::NEG_INFINITY, (i128::MAX as f64) * 2.0]);
}

trait AsCast<T> {
    fn as_cast(&self) -> T;
}

macro_rules! impl_as_cast {
    ($($type:ident,)+) => {
        impl_as_cast! { @many_to_many; ($($type,)+); ($($type,)+) }
    };
    (@many_to_many; ($($from:ident,)+); $to:tt) => {
        $(
            impl_as_cast! { @one_to_many; $from; $to }
        )+
    };
    (@one_to_many; $from:ident; ($($to:ident,)+)) => {
        $(
            impl_as_cast! { @one; $from; $to }
        )+
    };
    (@one; $from:ident; $to:ident) => {
        impl AsCast<$to> for $from {
            fn as_cast(&self) -> $to {
                *self as $to
            }
        }
    };
}

impl_as_cast! {
    u8, u16, u32, u64, u128,
    i8, i16, i32, i64, i128,
    f32, f64,
}

impl AsCast<String> for String {
    fn as_cast(&self) -> String {
        self.clone()
    }
}

trait WithRef<RefT: ?Sized> {
    fn with_ref<WithRefT, ResultT>(&self, with: WithRefT) -> ResultT
    where
        WithRefT: FnOnce(&RefT) -> ResultT;
}

#[derive(Clone, Serialize, PartialEq, Debug)]
#[serde(transparent)]
struct Strict<InnerT = String>(InnerT);

trait FromValue<T> {
    fn from_value(value: T) -> Self;
}

impl<OtherT, InnerT> FromValue<OtherT> for Strict<InnerT>
where
    OtherT: AsCast<InnerT>,
{
    fn from_value(value: OtherT) -> Self {
        Self(value.as_cast())
    }
}

impl<OtherT, InnerT> WithRef<OtherT> for Strict<InnerT>
where
    InnerT: AsCast<OtherT>,
{
    fn with_ref<WithRefT, ResultT>(&self, with: WithRefT) -> ResultT
    where
        WithRefT: FnOnce(&OtherT) -> ResultT,
    {
        with(&self.0.as_cast())
    }
}

impl<'de, InnerT> Deserialize<'de> for Strict<InnerT>
where
    InnerT: StrictDeserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        InnerT::strict_deserialize(deserializer, InnerT::Visitor::default()).map(Self)
    }
}

#[derive(Clone, PartialEq, Debug)]
struct Fallback<DeserializeT, FallbackT>(FallbackT, PhantomData<(FallbackT, DeserializeT)>);

impl<OtherT, DeserializeT, FallbackT> FromValue<OtherT> for Fallback<DeserializeT, FallbackT>
where
    OtherT: AsCast<FallbackT>,
{
    fn from_value(value: OtherT) -> Self {
        Self(value.as_cast(), PhantomData)
    }
}

impl<OtherT, DeserializeT, FallbackT> WithRef<OtherT> for Fallback<DeserializeT, FallbackT>
where
    FallbackT: AsCast<OtherT>,
{
    fn with_ref<WithRefT, ResultT>(&self, with: WithRefT) -> ResultT
    where
        WithRefT: FnOnce(&OtherT) -> ResultT,
    {
        with(&self.0.as_cast())
    }
}

impl<'de, DeserializeT, FallbackT> Deserialize<'de> for Fallback<DeserializeT, FallbackT>
where
    FallbackT: StrictDeserialize<'de>,
    DeserializeT: StrictDeserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        DeserializeT::strict_deserialize(deserializer, FallbackT::Visitor::default())
            .map(|value| Self(value, PhantomData))
    }
}

#[derive(Debug)]
struct UnitStruct<T = Strict> {
    phantom: PhantomData<T>,
}

impl<T> UnitStruct<T> {
    fn new(_field: T) -> Self {
        Self {
            phantom: PhantomData,
        }
    }
}

impl<T> Serialize for UnitStruct<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_unit_struct("UnitStruct")
    }
}

impl<'de, T> Deserialize<'de> for UnitStruct<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor<T>(PhantomData<T>);
        impl<'de, T> serde::de::Visitor<'de> for Visitor<T> {
            type Value = UnitStruct<T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "UnitStruct")
            }

            fn visit_unit<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(UnitStruct {
                    phantom: PhantomData,
                })
            }
        }
        deserializer.deserialize_unit_struct("UnitStruct", Visitor(PhantomData))
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct NewType<T = Strict>(T);

impl<T> NewType<T> {
    fn new(field: T) -> Self {
        Self(field)
    }

    fn as_inner(&self) -> &T {
        &self.0
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct TupleStruct<T = Strict, U = ()>(T, U);

impl<T, U> TupleStruct<T, U> {
    fn new(field: T) -> Self
    where
        U: Default,
    {
        Self(field, U::default())
    }

    fn as_inner(&self) -> &T {
        &self.0
    }
}

#[derive(Serialize, Deserialize, Debug)]
struct FieldStruct<T = Strict> {
    field: T,
}

impl<T> FieldStruct<T> {
    fn new(field: T) -> Self {
        Self { field }
    }

    fn as_inner(&self) -> &T {
        &self.field
    }
}

#[derive(Serialize, Deserialize, Debug)]
enum Enum<T = Strict, U = ()> {
    Unit,
    NewType(T),
    Tuple(T, U),
    Struct { field: T },
}

impl<T, U> Enum<T, U> {
    fn as_inner(&self) -> &T {
        match self {
            Self::NewType(field) | Self::Tuple(field, _) | Self::Struct { field } => field,
            Self::Unit => panic!("cannot reference unit enum"),
        }
    }
}

#[derive(Serialize, Debug)]
#[serde(transparent)]
struct UnitVariant<T = Strict>(Enum<T>);

impl<T> UnitVariant<T> {
    fn new(_field: T) -> Self {
        Self(Enum::Unit)
    }
}

#[derive(Serialize, Debug)]
#[serde(transparent)]
struct NewTypeVariant<T = Strict>(Enum<T>);

impl<T> NewTypeVariant<T> {
    fn new(field: T) -> Self {
        Self(Enum::NewType(field))
    }

    fn as_inner(&self) -> &T {
        self.0.as_inner()
    }
}

#[derive(Serialize, Debug)]
#[serde(transparent)]
struct TupleVariant<T = Strict, U = ()>(Enum<T, U>);

impl<T, U> TupleVariant<T, U> {
    fn new(field: T) -> Self
    where
        U: Default,
    {
        Self(Enum::Tuple(field, U::default()))
    }

    fn as_inner(&self) -> &T {
        self.0.as_inner()
    }
}

#[derive(Serialize, Debug)]
#[serde(transparent)]
struct StructVariant<T = Strict>(Enum<T>);

impl<T> StructVariant<T> {
    fn new(field: T) -> Self {
        Self(Enum::Struct { field })
    }

    fn as_inner(&self) -> &T {
        self.0.as_inner()
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(transparent)]
struct Optional<T = Strict>(Option<T>);

impl<T> Optional<T> {
    fn new(field: T) -> Self {
        Self(Some(field))
    }

    fn as_inner(&self) -> &T {
        self.0.as_ref().unwrap()
    }
}

#[derive(Serialize, Debug)]
#[serde(transparent)]
struct OptionNone<T = Strict>(Option<T>);

impl<T> OptionNone<T> {
    fn new(_field: T) -> Self {
        Self(None)
    }
}

#[derive(Serialize, Debug)]
#[serde(transparent)]
struct Unit<T = Strict> {
    inner: (),
    phantom: PhantomData<T>,
}

impl<T> Unit<T> {
    fn new(_field: T) -> Self {
        Self {
            inner: (),
            phantom: PhantomData,
        }
    }
}

macro_rules! impl_primitive_conversions {
    ($(impl < T $(, $typevar:ident)? > $($trait:ident),+ for $($type:ident<$($dummy:ident),+>),+;)+) => {
        $(
            impl_primitive_conversions! { @one_invocation; ($($type,)+); ($($trait,)+); ($($typevar)?); }
        )+
    };
    (@one_invocation; ($($type:ident,)+); $traits:tt; $typevars:tt; ) => {
        $(
            impl_primitive_conversions! { @one_type; $type; $traits; $typevars; }
        )+
    };
    (@one_type; $type:ident; ($($trait:ident,)+); $typevars:tt;) => {
        $(
            impl_primitive_conversions! {
                @one_trait;
                $type;
                $trait;
                (
                    i8, i16, i32, i64, i128,
                    u8, u16, u32, u64, u128,
                    f32, f64, char, bool,
                    String, ByteBuf,
                );
                $typevars;
            }
        )+
    };
    (@one_trait; $type:ident; $trait:ident; ($($primitive:ty,)+); $typevars:tt;) => {
        $(
            impl_primitive_conversions! {
                @one_impl;
                $type;
                $trait;
                $primitive;
                $typevars;
            }
        )+
    };
    (@one_impl; $type:ident; WithRef; $primitive:ty; ($($typevar:ident)?);) => {
        impl<T$(, $typevar)?> WithRef<$primitive> for $type<T$(, $typevar)?>
        where
            T: WithRef<$primitive>,
        {
            fn with_ref<F: FnOnce(&$primitive) -> R, R>(&self, with: F) -> R {
                self.as_inner().with_ref(with)
            }
        }
    };
    (@one_impl; $type:ident; From; str; ($($typevar:ident)?);) => {};
    (@one_impl; $type:ident; From; $primitive:ty; ($($typevar:ident)?);) => {
        impl<T$(, $typevar)?> FromValue<$primitive> for $type<T$(, $typevar)?>
        where
            T: FromValue<$primitive>,
            $($typevar: Default,)?
        {
            fn from_value(field: $primitive) -> Self {
                Self::new(T::from_value(field))
            }
        }
    };
}

impl_primitive_conversions! {
    impl<T> From for Unit<T>, UnitStruct<T>, UnitVariant<T>, OptionNone<T>;
    impl<T> From, WithRef for NewTypeVariant<T>, StructVariant<T>;
    impl<T> From, WithRef for NewType<T>, FieldStruct<T>, Optional<T>;
    impl<T, U> From, WithRef for TupleVariant<T, U>, TupleStruct<T, U>;
    impl<T> WithRef for Enum<T>;
}

fn check_equals_two_way<V, T, U>(values: &[V])
where
    V: Clone + PartialEq,
    T: DeserializeOwned + Serialize + Debug + WithRef<V> + FromValue<V>,
    U: DeserializeOwned + Serialize + Debug + WithRef<V> + FromValue<V>,
    NewType<T>: FromValue<V> + WithRef<V>,
    TupleStruct<T>: FromValue<V> + WithRef<V>,
    FieldStruct<T>: FromValue<V> + WithRef<V>,
    NewTypeVariant<T>: FromValue<V> + WithRef<V>,
    TupleVariant<T>: FromValue<V> + WithRef<V>,
    StructVariant<T>: FromValue<V> + WithRef<V>,
    Optional<T>: FromValue<V> + WithRef<V>,
    Enum<T>: WithRef<V>,
    NewType<U>: FromValue<V> + WithRef<V>,
    TupleStruct<U>: FromValue<V> + WithRef<V>,
    FieldStruct<U>: FromValue<V> + WithRef<V>,
    NewTypeVariant<U>: FromValue<V> + WithRef<V>,
    TupleVariant<U>: FromValue<V> + WithRef<V>,
    StructVariant<U>: FromValue<V> + WithRef<V>,
    Optional<U>: FromValue<V> + WithRef<V>,
    Enum<U>: WithRef<V>,
{
    check_equals_one_way::<_, T, U>(values);
    check_equals_one_way::<_, U, T>(values);
}

fn check_equals_one_way<V, T, U>(values: &[V])
where
    V: Clone + PartialEq,
    T: Serialize + Debug + WithRef<V> + FromValue<V>,
    U: DeserializeOwned + Debug + WithRef<V>,
    NewType<T>: FromValue<V> + WithRef<V>,
    TupleStruct<T>: FromValue<V> + WithRef<V>,
    FieldStruct<T>: FromValue<V> + WithRef<V>,
    NewTypeVariant<T>: FromValue<V> + WithRef<V>,
    TupleVariant<T>: FromValue<V> + WithRef<V>,
    StructVariant<T>: FromValue<V> + WithRef<V>,
    Optional<T>: FromValue<V> + WithRef<V>,
    NewType<U>: WithRef<V>,
    TupleStruct<U>: WithRef<V>,
    FieldStruct<U>: WithRef<V>,
    NewTypeVariant<U>: WithRef<V>,
    TupleVariant<U>: WithRef<V>,
    StructVariant<U>: WithRef<V>,
    Optional<U>: WithRef<V>,
    Enum<U>: WithRef<V>,
{
    check_equals_one_way_leaf::<_, T, U>(values);
    check_equals_one_way_leaf::<_, NewType<T>, NewType<U>>(values);
    check_equals_one_way_leaf::<_, TupleStruct<T>, TupleStruct<U>>(values);
    check_equals_one_way_leaf::<_, FieldStruct<T>, FieldStruct<U>>(values);
    check_equals_one_way_leaf::<_, NewTypeVariant<T>, Enum<U>>(values);
    check_equals_one_way_leaf::<_, TupleVariant<T>, Enum<U>>(values);
    check_equals_one_way_leaf::<_, StructVariant<T>, Enum<U>>(values);
    check_equals_one_way_leaf::<_, Optional<T>, Optional<U>>(values);
}

fn check_equals_one_way_leaf<V, T, U>(values: &[V])
where
    V: Clone + PartialEq,
    T: Serialize + Debug + WithRef<V> + FromValue<V>,
    U: DeserializeOwned + Debug + WithRef<V>,
{
    for value in values {
        check_evolution_ok::<_, U>(&T::from_value(value.clone()), |old, new| {
            old.with_ref(|old| new.with_ref(|new| old == new))
        });
    }
}

//fn check_ok_two_way<V, T, U>(values: &[V])
//where
//    V: Clone,
//    T: DeserializeOwned + Serialize + Debug + FromValue<V>,
//    U: DeserializeOwned + Serialize + Debug + FromValue<V>,
//    NewType<T>: FromValue<V>,
//    TupleStruct<T>: FromValue<V>,
//    FieldStruct<T>: FromValue<V>,
//    NewTypeVariant<T>: FromValue<V>,
//    TupleVariant<T>: FromValue<V>,
//    StructVariant<T>: FromValue<V>,
//    Optional<T>: FromValue<V>,
//    NewType<U>: FromValue<V>,
//    TupleStruct<U>: FromValue<V>,
//    FieldStruct<U>: FromValue<V>,
//    NewTypeVariant<U>: FromValue<V>,
//    TupleVariant<U>: FromValue<V>,
//    StructVariant<U>: FromValue<V>,
//    Optional<U>: FromValue<V>,
//{
//    check_ok_one_way::<_, T, U>(values);
//    check_ok_one_way::<_, U, T>(values);
//}

fn check_ok_one_way<V, T, U>(values: &[V])
where
    V: Clone,
    T: Serialize + Debug + FromValue<V>,
    U: DeserializeOwned + Debug,
    NewType<T>: FromValue<V>,
    TupleStruct<T>: FromValue<V>,
    FieldStruct<T>: FromValue<V>,
    NewTypeVariant<T>: FromValue<V>,
    TupleVariant<T>: FromValue<V>,
    StructVariant<T>: FromValue<V>,
    Optional<T>: FromValue<V>,
{
    check_ok_one_way_leaf::<_, T, U>(values);
    check_ok_one_way_leaf::<_, NewType<T>, NewType<U>>(values);
    check_ok_one_way_leaf::<_, TupleStruct<T>, TupleStruct<U>>(values);
    check_ok_one_way_leaf::<_, FieldStruct<T>, FieldStruct<U>>(values);
    check_ok_one_way_leaf::<_, NewTypeVariant<T>, Enum<U>>(values);
    check_ok_one_way_leaf::<_, TupleVariant<T>, Enum<U>>(values);
    check_ok_one_way_leaf::<_, StructVariant<T>, Enum<U>>(values);
    check_ok_one_way_leaf::<_, Optional<T>, Optional<U>>(values);
}

fn check_ok_one_way_leaf<
    V: Clone,
    T: Serialize + Debug + FromValue<V>,
    U: DeserializeOwned + Debug,
>(
    values: &[V],
) {
    for value in values {
        check_evolution_ok::<_, U>(&T::from_value(value.clone()), |_, _| true);
    }
}

fn check_fails_two_way<V, T, U>(values: &[V])
where
    V: Clone,
    T: DeserializeOwned + Serialize + Debug + FromValue<V>,
    U: DeserializeOwned + Serialize + Debug + FromValue<V>,
    NewType<T>: FromValue<V>,
    TupleStruct<T>: FromValue<V>,
    FieldStruct<T>: FromValue<V>,
    NewTypeVariant<T>: FromValue<V>,
    TupleVariant<T>: FromValue<V>,
    StructVariant<T>: FromValue<V>,
    Optional<T>: FromValue<V>,
    NewType<U>: FromValue<V>,
    TupleStruct<U>: FromValue<V>,
    FieldStruct<U>: FromValue<V>,
    NewTypeVariant<U>: FromValue<V>,
    TupleVariant<U>: FromValue<V>,
    StructVariant<U>: FromValue<V>,
    Optional<U>: FromValue<V>,
{
    check_fails_one_way::<_, T, U>(values);
    check_fails_one_way::<_, U, T>(values);
}

fn check_fails_one_way<V, T, U>(values: &[V])
where
    V: Clone,
    T: Serialize + Debug + FromValue<V>,
    U: DeserializeOwned + Debug,
    NewType<T>: FromValue<V>,
    TupleStruct<T>: FromValue<V>,
    FieldStruct<T>: FromValue<V>,
    NewTypeVariant<T>: FromValue<V>,
    TupleVariant<T>: FromValue<V>,
    StructVariant<T>: FromValue<V>,
    Optional<T>: FromValue<V>,
{
    check_fails_one_way_leaf::<V, T, U>(values);
    check_fails_one_way_leaf::<V, NewType<T>, NewType<U>>(values);
    check_fails_one_way_leaf::<V, TupleStruct<T>, TupleStruct<U>>(values);
    check_fails_one_way_leaf::<V, FieldStruct<T>, FieldStruct<U>>(values);
    check_fails_one_way_leaf::<V, NewTypeVariant<T>, Enum<U>>(values);
    check_fails_one_way_leaf::<V, TupleVariant<T>, Enum<U>>(values);
    check_fails_one_way_leaf::<V, StructVariant<T>, Enum<U>>(values);
    check_fails_one_way_leaf::<V, Optional<T>, Optional<U>>(values);
}

fn check_fails_one_way_leaf<
    V: Clone,
    T: Serialize + Debug + FromValue<V>,
    U: DeserializeOwned + Debug,
>(
    values: &[V],
) {
    for value in values {
        check_evolution_fails::<_, U>(&T::from_value(value.clone()));
    }
}
