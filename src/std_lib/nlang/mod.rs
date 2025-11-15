pub fn std_module() -> &'static str {
    concat!(
        include_str!("abs.nlang"), "\n",
        include_str!("abs_float.nlang"), "\n",
        include_str!("sum.nlang"), "\n",
        include_str!("min.nlang"), "\n",
        include_str!("max.nlang"), "\n",
        include_str!("pow.nlang"), "\n",
        include_str!("reverse.nlang"), "\n",
        include_str!("sort.nlang"), "\n",
    )
}