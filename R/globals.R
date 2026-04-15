# Suppress "no visible binding for global variable" notes from R CMD CHECK.
# These column names are created and referenced within dplyr::mutate() calls
# where intra-mutate sequential evaluation prevents use of the .data pronoun.
utils::globalVariables(c(
  "estimate",
  "estimate_low",
  "estimate_upp",
  "estimate_pct",
  "estimate_val",
  "lower_ci",
  "upper_ci",
  "cumsum_num",
  "cumsum_den"
))
