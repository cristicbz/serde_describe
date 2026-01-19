pub(crate) trait LosslessCast<IntoT> {
    fn lossless_cast(self) -> Option<IntoT>;
}

macro_rules! impl_lossless_cast {
    ($($int:ident),+; $($float:ident),+; ) => {
        impl_lossless_cast! { @try_from_many_to_many; ($($int,)+); ($($int,)+) }
        impl_lossless_cast! { @int_to_float_many_to_many; ($($int,)+); ($($float,)+) }
    };
    (@try_from_many_to_many; ($($from:ident,)+); $to:tt) => {
        $(
            impl_lossless_cast! { @try_from_one_to_many; $from; $to }
        )+
    };
    (@try_from_one_to_many; $from:ident; ($($to:ident,)+)) => {
        $(
            impl_lossless_cast! { @try_from_one; $from; $to }
        )+
    };
    (@try_from_one; $from:ident; $to:ident) => {
        impl LosslessCast<$to> for $from {
            #[inline]
            fn lossless_cast(self) -> Option<$to> {
                $to::try_from(self).ok()
            }
        }
    };
    (@int_to_float_many_to_many; $ints:tt; ($($float:ident,)+)) => {
        $(
            impl LosslessCast<$float> for $float {
                #[inline]
                fn lossless_cast(self) -> Option<$float> {
                    Some(self)
                }
            }

            impl_lossless_cast! { @int_to_float_one_to_many; $ints; $float }
        )+
    };
    (@int_to_float_one_to_many; ($($int:ident,)+); $float:ident) => {
        $(
            impl_lossless_cast! { @int_to_float_one; $int; $float }
        )+
    };
    (@int_to_float_one; $int:ident; $float:ident) => {
        impl LosslessCast<$int> for $float {
            #[inline]
            fn lossless_cast(self) -> Option<$int> {
                if self.is_finite() && (self as $int as $float) == self {
                    Some(self as $int)
                } else {
                    None
                }
            }
        }

        impl LosslessCast<$float> for $int {
            #[inline]
            fn lossless_cast(self) -> Option<$float> {
                if (self as $float).is_finite() && (self as $float as $int) == self {
                    Some(self as $float)
                } else {
                    None
                }
            }
        }
    };
}

impl_lossless_cast! {
    u8, u16, u32, u64, u128,
    i8, i16, i32, i64, i128;
    f32, f64;
}

impl LosslessCast<f64> for f32 {
    #[inline]
    fn lossless_cast(self) -> Option<f64> {
        Some(f64::from(self))
    }
}

impl LosslessCast<f32> for f64 {
    #[inline]
    fn lossless_cast(self) -> Option<f32> {
        None
    }
}
