use crate::Schema;

#[derive(Copy, Clone)]
pub struct SelfDescribed<T>(pub T);

#[derive(Copy, Clone)]
pub struct DescribedBy<'schema, T>(pub T, pub &'schema Schema);

#[derive(Copy, Clone)]
pub struct SeedSelfDescribed<SeedT>(pub SeedT);

#[derive(Copy, Clone)]
pub struct SeedDescribedBy<'schema, SeedT>(pub SeedT, pub &'schema Schema);
