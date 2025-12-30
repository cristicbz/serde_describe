pub(crate) fn serialized_anonymous_variant<ErrorT>(i_variant: u32) -> Result<&'static str, ErrorT>
where
    ErrorT: serde::ser::Error,
{
    UNION_VARIANT_NAMES
        .get(usize::try_from(i_variant).expect("usize should be at least 32 bits"))
        .copied()
        .ok_or_else(|| {
            ErrorT::custom(format!(
                "too many union variants {i_variant} >= {NUM_ANONYMOUS_NAMES}"
            ))
        })
}

pub(crate) fn deserialized_anonymous_variants<ErrorT>(
    num_variants: usize,
) -> Result<&'static [&'static str], ErrorT>
where
    ErrorT: serde::de::Error,
{
    UNION_VARIANT_NAMES
        .split_at_checked(num_variants)
        .map(|(variants, _)| variants)
        .ok_or_else(|| {
            ErrorT::custom(format!(
                "too many union variants {num_variants} > {NUM_ANONYMOUS_NAMES}"
            ))
        })
}

pub(crate) const NUM_ANONYMOUS_NAMES: usize = UNION_VARIANT_NAMES.len();
pub(crate) static UNION_ENUM_NAME: &str = "Union";
static UNION_VARIANT_NAMES: &[&str] = &[
    "_00", "_01", "_02", "_03", "_04", "_05", "_06", "_07", "_08", "_09", "_0a", "_0b", "_0c",
    "_0d", "_0e", "_0f", "_10", "_11", "_12", "_13", "_14", "_15", "_16", "_17", "_18", "_19",
    "_1a", "_1b", "_1c", "_1d", "_1e", "_1f", "_20", "_21", "_22", "_23", "_24", "_25", "_26",
    "_27", "_28", "_29", "_2a", "_2b", "_2c", "_2d", "_2e", "_2f", "_30", "_31", "_32", "_33",
    "_34", "_35", "_36", "_37", "_38", "_39", "_3a", "_3b", "_3c", "_3d", "_3e", "_3f", "_40",
    "_41", "_42", "_43", "_44", "_45", "_46", "_47", "_48", "_49", "_4a", "_4b", "_4c", "_4d",
    "_4e", "_4f", "_50", "_51", "_52", "_53", "_54", "_55", "_56", "_57", "_58", "_59", "_5a",
    "_5b", "_5c", "_5d", "_5e", "_5f", "_60", "_61", "_62", "_63", "_64", "_65", "_66", "_67",
    "_68", "_69", "_6a", "_6b", "_6c", "_6d", "_6e", "_6f", "_70", "_71", "_72", "_73", "_74",
    "_75", "_76", "_77", "_78", "_79", "_7a", "_7b", "_7c", "_7d", "_7e", "_7f", "_80", "_81",
    "_82", "_83", "_84", "_85", "_86", "_87", "_88", "_89", "_8a", "_8b", "_8c", "_8d", "_8e",
    "_8f", "_90", "_91", "_92", "_93", "_94", "_95", "_96", "_97", "_98", "_99", "_9a", "_9b",
    "_9c", "_9d", "_9e", "_9f", "_a0", "_a1", "_a2", "_a3", "_a4", "_a5", "_a6", "_a7", "_a8",
    "_a9", "_aa", "_ab", "_ac", "_ad", "_ae", "_af", "_b0", "_b1", "_b2", "_b3", "_b4", "_b5",
    "_b6", "_b7", "_b8", "_b9", "_ba", "_bb", "_bc", "_bd", "_be", "_bf", "_c0", "_c1", "_c2",
    "_c3", "_c4", "_c5", "_c6", "_c7", "_c8", "_c9", "_ca", "_cb", "_cc", "_cd", "_ce", "_cf",
    "_d0", "_d1", "_d2", "_d3", "_d4", "_d5", "_d6", "_d7", "_d8", "_d9", "_da", "_db", "_dc",
    "_dd", "_de", "_df", "_e0", "_e1", "_e2", "_e3", "_e4", "_e5", "_e6", "_e7", "_e8", "_e9",
    "_ea", "_eb", "_ec", "_ed", "_ee", "_ef", "_f0", "_f1", "_f2", "_f3", "_f4", "_f5", "_f6",
    "_f7", "_f8", "_f9", "_fa", "_fb", "_fc", "_fd", "_fe", "_ff",
];
