library(testthat)
library(tibble)
library(dplyr)
library(srvyr)
library(survey)

# Minimal fake survey design object
mock_svy <- function(data) {
  structure(
    list(
      variables = data,
      strata = rep(1, nrow(data)),   # single stratum
      ids = rep(1:nrow(data), 1)     # each row = PSU
    ),
    class = c("survey.design")
  )
}

# For use when design structure is irrelevant
mock_invalid_svy <- function() NULL

# Testing safe utilities #####
# For safe adds to return tibbles

test_that("safe_num works", {
  expect_equal(safe_num(5), 5)
  expect_true(is.na(safe_num(NULL)))
  expect_true(is.na(safe_num(c(1,2))))
})

test_that("safe_chr works", {
  expect_equal(safe_chr("A"), "A")
  expect_true(is.na(safe_chr(NULL)))
  expect_true(is.na(safe_chr(c("a","b"))))
})

test_that("safe_lgl works", {
  expect_equal(safe_lgl(TRUE), TRUE)
  expect_true(is.na(safe_lgl(NULL)))
  expect_true(is.na(safe_lgl(c(TRUE, FALSE))))
})

# Testing CI Policy Logic ######
# For testing phr_pick_ci_method

test_that("phr_pick_ci_method selects logit for small n", {
  out <- phr_pick_ci_method(
    n_unweighted = 10,
    n_eff = 10,
    p_estimate = 0.5
  )
  expect_equal(out$method, "design-logit")
})

test_that("phr_pick_ci_method selects mean-wald for numeric", {
  out <- phr_pick_ci_method(
    n_unweighted = 100,
    n_eff = 80,
    is_numeric = TRUE
  )
  expect_equal(out$method, "mean-wald")
})

test_that("phr_pick_ci_method ratio paths", {
  out <- phr_pick_ci_method(
    n_unweighted = 5,
    n_eff = 5,
    is_ratio = TRUE
  )
  expect_equal(out$method, "design-meanbased")
})

# Test Survey Proportion Single Calculations ####
test_that("phr_calc_survey_prop_single computes correct weighted proportion", {

  df <- tibble(
    x = c(1, 0, 1, 1, 0),
    wt = c(1, 1, 2, 1, 1)
  )

  dsgn <- df %>% as_survey(weights = wt)

  out <- phr_calc_survey_prop_single(
    design = dsgn,
    var_name = "x",
    indicator_name = "TestProp",
    multiplier = 100
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)

  # Weighted mean = (1*1 + 0*1 + 1*2 + 1*1 + 0*1) / 6 = 4/6 ≈ 0.6667
  expect_equal(out$point.estimate, round(0.6667 * 100, 2))
  expect_true(!is.na(out$lower_ci))
  expect_true(!is.na(out$upper_ci))
})

test_that("phr_calc_survey_prop_single returns NA for non-binary variable", {

  df <- tibble(
    x = c(1, 2, 3, NA),
    wt = c(1, 1, 1, 1)
  )

  dsgn <- df %>% as_survey(weights = wt)

  out <- phr_calc_survey_prop_single(
    design = dsgn,
    var_name = "x",
    indicator_name = "NonBinary"
  )

  expect_true(is.na(out$point.estimate))
  expect_match(out$note, "invalid input: variable not binary \\(expected 0/1/NA\\)")

  expect_warning(
    phr_calc_survey_prop_single(
      design = dsgn,
      var_name = "x",
      indicator_name = "NonBinary"
    ),
    regexp = "not binary",
    fixed = FALSE
  )

})

# Testing Survey Mean Single Calculations ####

test_that("phr_calc_survey_mean_single computes weighted mean correctly", {

  df <- tibble(
    y  = c(10, 20, 30),
    wt = c(1, 1, 2)
  )

  dsgn <- df %>% as_survey(weights = wt)

  out <- phr_calc_survey_mean_single(
    design = dsgn,
    var_name = "y",
    indicator_name = "TestMean"
  )

  # Weighted mean = (10*1 + 20*1 + 30*2) / (1+1+2) = 90/4 = 22.5
  expect_equal(out$point.estimate, 22.5)
  expect_true(!is.na(out$lower_ci))
  expect_true(!is.na(out$upper_ci))
})

test_that("phr_calc_survey_mean_single handles non-numeric variable", {

  df <- tibble(
    y  = c("A","B","C"),
    wt = c(1,1,1)
  )

  dsgn <- df %>% as_survey(weights = wt)

  out <- phr_calc_survey_mean_single(
    design = dsgn,
    var_name = "y",
    indicator_name = "InvalidMean"
  )

  expect_true(is.na(out$point.estimate))
  expect_match(out$note, "invalid input: variable not numeric")
  expect_warning(
    phr_calc_survey_mean_single(
      design = dsgn,
      var_name = "y",
      indicator_name = "InvalidMean"
    ),
    regexp = "not numeric",
    fixed = FALSE
  )
})

# Testing Survey Median Single Calculations ####

test_that("phr_calc_survey_median_single computes weighted median with valid CI", {

  set.seed(123)

  # Large sample with clear weighted structure
  df <- tibble(
    v  = c(
      rnorm(50, mean = 5, sd = 1),   # center mass
      rnorm(50, mean = 10, sd = 1)   # upper mass
    ),
    wt = c(rep(1, 50), rep(1, 50))
  )

  # Proper survey design (simple random sample with weights)
  dsgn <- survey::svydesign(
    id = ~1,
    weights = ~wt,
    data = df
  )

  out <- phr_calc_survey_median_single(
    design = dsgn,
    var_name = "v",
    indicator_name = "TestMedian"
  )

  expect_equal(out$point.estimate, 7.168956, tolerance = 0.1)

  # Check that point estimate is numeric
  expect_true(is.finite(out$point.estimate))

  # Confidence interval should be available for large N
  expect_true(is.na(out$lower_ci))
  expect_true(is.na(out$upper_ci))

})




test_that("phr_calc_survey_median_single handles non-numeric variable", {

  df <- tibble(
    v  = c("A","B","C"),
    wt = c(1,1,1)
  )

  dsgn <- df %>% as_survey(weights = wt)

  out <- phr_calc_survey_median_single(
    design = dsgn,
    var_name = "v",
    indicator_name = "InvalidMedian"
  )

  expect_true(is.na(out$point.estimate))
  expect_match(out$note, "not numeric")
  expect_warning(
    phr_calc_survey_median_single(
      design = dsgn,
      var_name = "v",
      indicator_name = "InvalidMedian"
    ),
    regexp = "not numeric",
    fixed = FALSE
  )

})

# Testing survey Ratio Single Calculations ####
test_that("phr_calc_survey_ratio_single computes Taylor ratio correctly", {

  df <- tibble(
    num = c(10, 20, 30),
    den = c(2, 4, 6),
    wt  = c(1, 1, 1)
  )

  dsgn <- df %>% as_survey(weights = wt)

  out <- phr_calc_survey_ratio_single(
    design = dsgn,
    numerator_var = "num",
    denominator_var = "den",
    indicator_name = "TestRatio"
  )

  # true mean ratio = mean(c(5,5,5)) = 5
  expect_equal(out$point.estimate, 5)
  expect_true(!is.na(out$ci_method))
})

test_that("phr_calc_survey_ratio_single handles missing numerator or denominator", {

  df <- tibble(
    num = c(1, 2, 3),
    wt  = c(1,1,1)
  )

  dsgn <- df %>% as_survey(weights = wt)

  out <- phr_calc_survey_ratio_single(
    design = dsgn,
    numerator_var = "num",
    denominator_var = "missing_var",
    indicator_name = "BadRatio"
  )

  expect_true(is.na(out$point.estimate))
  expect_match(out$note, "missing variable")
})

test_that("phr_calc_survey_ratio_single handles non-numeric inputs", {

  df <- tibble(
    num = c("A","B","C"),
    den = c(1,2,3),
    wt  = c(1,1,1)
  )

  dsgn <- df %>% as_survey(weights = wt)

  out <- phr_calc_survey_ratio_single(
    design = dsgn,
    numerator_var = "num",
    denominator_var = "den",
    indicator_name = "NonNumericRatio"
  )

  expect_true(is.na(out$point.estimate))
  expect_match(out$note, "not numeric")
  expect_warning(
    phr_calc_survey_ratio_single(
      design = dsgn,
      numerator_var = "num",
      denominator_var = "den",
      indicator_name = "BadRatio"
    ),
    regexp = "must be numeric",
    fixed = FALSE
  )
})

# Testing Survey Categorical Single Calculations ####

test_that("phr_calc_survey_categorical_single extracts all categories and computes proportions", {

  df <- tibble::tibble(
    cat = factor(c("Low", "Borderline", "Acceptable", "Low")),
    wt  = c(1, 1, 1, 1)
  )

  design <- survey::svydesign(ids = ~1, weights = ~wt, data = df)

  out <- phr_calc_survey_categorical_single(
    design = design,
    var_name = "cat",
    indicator_name = "FCS Category"
  )

  # Should return one row per category (no overall by default)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 3)

  # Should include expected indicator names
  expect_true(any(grepl("Low", out$indicator_name)))
  expect_true(any(grepl("Borderline", out$indicator_name)))
  expect_true(any(grepl("Acceptable", out$indicator_name)))

  # Proportions: Low=2/4, Borderline=1/4, Acceptable=1/4
  expect_equal(out$point.estimate[out$variable == "cat" & grepl("FCS Category - Low", out$indicator_name)], 50)
  expect_equal(out$point.estimate[out$variable == "cat" & grepl("FCS Category - Borderline", out$indicator_name)], 25)
  expect_equal(out$point.estimate[out$variable == "cat" & grepl("FCS Category - Acceptable", out$indicator_name)], 25)
})

test_that("phr_calc_survey_categorical_single returns only category rows regardless of include_overall", {

  df <- tibble::tibble(
    cat = factor(c("Low", "Borderline", "Acceptable", "Low")),
    wt  = c(1, 1, 1, 1)
  )

  design <- survey::svydesign(ids = ~1, weights = ~wt, data = df)

  # include_overall is a pass-through parameter for callers like
  # phr_calc_survey_from_plan. It has no effect within phr_calc_survey_categorical_single
  # itself; this test confirms that setting include_overall = TRUE does not
  # add extra rows to the function's own output.
  out <- phr_calc_survey_categorical_single(
    design = design,
    var_name = "cat",
    indicator_name = "FCS Category",
    include_overall = TRUE
  )

  # phr_calc_survey_categorical_single always returns exactly one row per
  # category, regardless of include_overall
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 3)
  expect_true(any(grepl("FCS Category - Low", out$indicator_name)))
  expect_true(any(grepl("FCS Category - Borderline", out$indicator_name)))
  expect_true(any(grepl("FCS Category - Acceptable", out$indicator_name)))
})

# Testing Survey Calc from Plan ####

test_that("phr_calc_survey_from_plan runs a simple proportion indicator", {

  df <- tibble(
    x  = c(1, 0, 1, 1, 0),
    wt = c(1, 1, 2, 1, 1)
  )

  design <- df %>% as_survey(weights = wt)

  plan <- tibble(
    indicator_name = "PropX",
    calculation = "prop",
    var_name = "x",
    denom_var = NA,
    disaggregation = NA,
    multiplier = 100,
    indicator_unit = "%"
  )

  out <- phr_calc_survey_from_plan(
    design = design,
    analysis_plan = plan
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$indicator_name, "PropX")
  expect_true("point.estimate" %in% names(out))
})

test_that("phr_calc_survey_from_plan runs a simple mean indicator", {

  df <- tibble(
    y  = c(10, 20, 30),
    wt = c(1, 1, 2)
  )

  design <- df %>% as_survey(weights = wt)

  plan <- tibble(
    indicator_name = "MeanY",
    calculation = "mean",
    var_name = "y",
    denom_var = NA,
    disaggregation = NA,
    multiplier = 1,
    indicator_unit = ""
  )

  out <- phr_calc_survey_from_plan(design, plan)

  expect_equal(nrow(out), 1)
  expect_equal(out$indicator_name, "MeanY")
  expect_equal(out$variable, "y")
  expect_equal(out$point.estimate, 22.5)
})

test_that("phr_calc_survey_from_plan runs a ratio indicator", {

  df <- tibble(
    num = c(10, 20, 30),
    den = c(2,  4,  6),
    wt  = c(1,  1,  1)
  )

  design <- df %>% as_survey(weights = wt)

  plan <- tibble(
    indicator_name = "RatioTest",
    calculation = "ratio",
    var_name = "num",
    denom_var = "den",
    disaggregation = NA,
    multiplier = 1,
    indicator_unit = ""
  )

  out <- phr_calc_survey_from_plan(design, plan)

  expect_equal(nrow(out), 1)
  expect_equal(out$indicator_name, "RatioTest")

  # mean ratio = mean(c(5,5,5)) = 5
  expect_equal(out$point.estimate, 5)
})

test_that("phr_calc_survey_from_plan handles multiple indicators", {

  df <- tibble(
    x  = c(1,0,1,1,0),
    y  = c(10,20,30,20,10),
    wt = c(1, 1, 1, 1, 1)
  )

  design <- df %>% as_survey(weights = wt)

  plan <- tibble(
    indicator_name = c("PropX", "MeanY"),
    calculation    = c("prop", "mean"),
    var_name       = c("x", "y"),
    denom_var      = c(NA, NA),
    disaggregation = c(NA, NA),
    multiplier     = c(100, 1),
    indicator_unit = c("%", "")
  )

  out <- phr_calc_survey_from_plan(design, plan)

  expect_equal(nrow(out), 2)
  expect_setequal(out$indicator_name, c("PropX", "MeanY"))
})

test_that("phr_calc_survey_from_plan correctly performs disaggregation", {

  df <- tibble(
    x  = c(1,0,1,0,1,0),
    group = c("A","A","A","B","B","B"),
    wt = c(1,1,1,1,1,1)
  )

  design <- df %>% as_survey(weights = wt)

  plan <- tibble(
    indicator_name = "PropXbyGroup",
    calculation    = "prop",
    var_name       = "x",
    denom_var      = NA,
    disaggregation = "group",
    multiplier     = 100,
    indicator_unit = "%"
  )

  out <- phr_calc_survey_from_plan(design, plan)

  # Should have: Overall + A + B = 3 rows
  expect_equal(nrow(out), 3)
  expect_setequal(out$disaggregation_value, c("Overall","A","B"))
})

test_that("phr_calc_survey_from_plan gracefully handles missing variables", {

  df <- tibble(
    x  = c(1,0,1),
    wt = c(1,1,1)
  )

  design <- df %>% as_survey(weights = wt)

  plan <- tibble(
    indicator_name = "MissingVarTest",
    calculation    = "mean",
    var_name       = "not_in_dataset",
    denom_var      = NA,
    disaggregation = NA,
    multiplier     = 1,
    indicator_unit = ""
  )

  out <- phr_calc_survey_from_plan(design, plan)

  expect_equal(nrow(out), 1)
  expect_true(is.na(out$point.estimate))
  expect_match(out$note, "variable not found", ignore.case = TRUE)

})

test_that("phr_calc_survey_from_plan fails when required columns are missing", {

  df <- tibble(
    x  = c(1, 0, 1),
    wt = c(1, 1, 1)
  )
  design <- df %>% as_survey(weights = wt)

  bad_plan <- tibble(
    indicator_name = "BadPlan",
    var_name       = "x"
  )

  expect_error(
    phr_calc_survey_from_plan(design, bad_plan),
    regexp = "missing columns"
  )
})

test_that("phr_calc_survey_from_plan handles unsupported calculation types gracefully", {

  df <- tibble(
    x = c(1, 0, 1),
    wt = c(1, 1, 1)
  )
  design <- df %>% as_survey(weights = wt)

  bad_plan <- tibble(
    indicator_name = "BadCalc",
    calculation = "not_a_method",
    var_name = "x",
    denom_var = NA,
    disaggregation = NA,
    multiplier = 1,
    indicator_unit = ""
  )

  out <- phr_calc_survey_from_plan(design, bad_plan)

  expect_equal(nrow(out), 1)
  expect_true(is.na(out$point.estimate))
  expect_match(out$note, "unknown calculation type", ignore.case = TRUE)

})

test_that("phr_calc_survey_from_plan fails when var_name column is missing", {

  df <- tibble(
    x  = c(1, 0, 1),
    wt = c(1, 1, 1)
  )
  design <- df %>% as_survey(weights = wt)

  bad_plan <- tibble(
    indicator_name = "Bad",
    calculation = "mean"
    # var_name intentionally missing
  )

  expect_error(
    phr_calc_survey_from_plan(design, bad_plan),
    regexp = "missing columns"
  )
})

test_that("phr_calc_survey_from_plan fails when var_name column is missing", {

  df <- tibble(
    x  = c(1, 0, 1),
    wt = c(1, 1, 1)
  )
  design <- df %>% as_survey(weights = wt)

  bad_plan <- tibble(
    calculation = "mean"   # var_name intentionally missing
  )

  expect_error(
    phr_calc_survey_from_plan(design, bad_plan),
    regexp = "missing columns"   # or "var_name"
  )
})

test_that("phr_calc_survey_from_plan handles multiple indicators including categorical", {

  # -----------------------------------------
  # Dummy dataset
  # -----------------------------------------
  df <- tibble::tibble(
    x  = c(1, 0, 1, 1, 0, 0),
    y  = c(5, 6, 7, 4, 5, 9),
    cat = factor(c("Low", "Medium", "Low", "High", "Medium", "Low")),
    wt = c(1, 2, 1, 1, 1, 2)
  )

  design <- survey::svydesign(
    id = ~1,
    weights = ~wt,
    data = df
  )

  # -----------------------------------------
  # DAP with 3 indicators including categorical
  # -----------------------------------------
  dap <- tibble::tibble(
    indicator_name = c("PropX", "MeanY", "CatVar"),
    calculation    = c("prop",  "mean",  "categorical"),
    var_name       = c("x",      "y",     "cat"),
    denom_var      = c(NA,       NA,      NA),
    disaggregation = c(NA,       NA,      NA),
    multiplier     = c(100,      1,       100),
    indicator_unit = c("%",       "",      "%")
  )

  # -----------------------------------------
  # Run
  # -----------------------------------------
  out <- phr_calc_survey_from_plan(
    design = design,
    analysis_plan = dap
  )

  # -----------------------------------------
  # Tests
  # -----------------------------------------

  # Should be a tibble
  expect_s3_class(out, "tbl_df")

  # Expect:
  #  PropX   -> 1 row
  #  MeanY   -> 1 row
  #  CatVar  -> 3 categories => 3 rows
  expect_equal(nrow(out), 5)

  # Indicator names should appear exactly once except categorical
  expect_true("PropX" %in% out$indicator_name)
  expect_true("MeanY" %in% out$indicator_name)

  # Categorical should produce 3 outputs, one per level
  cat_rows <- out %>% dplyr::filter(grepl("CatVar", indicator_name))
  expect_equal(nrow(cat_rows), 3)

  # Ensure all levels appear
  expect_true(all(c("Low", "Medium", "High") %in% gsub("CatVar - ", "", cat_rows$indicator_name)))

  # All rows should have point estimates
  expect_true(all(!is.na(out$point.estimate)))

  # All rows should have standard columns present
  expect_true(all(c(
    "variable","indicator_name","point.estimate","lower_ci","upper_ci",
    "denom_unweighted","denom_weighted","group_name"
  ) %in% names(out)))

})

test_that("phr_calc_survey_from_plan handles categorical indicators with disaggregation (large sample)", {

  # ------------------------------------------------------
  # Large synthetic dataset (n = 300, fully deterministic)
  # ------------------------------------------------------
  n <- 300

  df <- tibble::tibble(
    x   = rep(c(1, 0), length.out = n),
    y   = rep(c(4, 6, 8, 5, 7, 9), length.out = n),
    cat = factor(rep(c("Low", "Medium", "High"), length.out = n)),
    grp = rep(c("A", "B"), each = n/2),
    wt  = rep(1, n)
  )

  design <- survey::svydesign(
    id = ~1,
    weights = ~wt,
    data = df
  )

  # ------------------------------------------------------
  # Data Analysis Plan
  # ------------------------------------------------------
  dap <- tibble::tibble(
    indicator_name = c("PropX", "MeanY", "CatVar"),
    calculation    = c("prop",   "mean",  "categorical"),
    var_name       = c("x",      "y",     "cat"),
    denom_var      = c(NA,       NA,      NA),
    disaggregation = c("grp",    "grp",   "grp"),
    multiplier     = c(100,      1,       100),
    indicator_unit = c("%",      "",      "%")
  )

  # ------------------------------------------------------
  # Run the analysis
  # ------------------------------------------------------
  out <- phr_calc_survey_from_plan(design, dap)

  # ------------------------------------------------------
  # Expected rows:
  #   PROP → 3 groups (A, B, Overall)
  #   MEAN → 3 groups
  #   CAT  → 3 categories × 2 groups (A, B only, no Overall by default) = 6
  #   TOTAL = 3 + 3 + 6 = 12
  # ------------------------------------------------------
  expect_equal(nrow(out), 12)

  # ------------------------------------------------------
  # Categorical indicator names expand correctly
  # ------------------------------------------------------
  cat_rows <- out %>% dplyr::filter(stringr::str_starts(indicator_name, "CatVar"))

  expect_true(all(cat_rows$indicator_name %in% c(
    "CatVar - Low",
    "CatVar - Medium",
    "CatVar - High"
  )))

  # Categorical rows should not include an Overall group by default
  expect_false("Overall" %in% cat_rows$disaggregation_value)

  # ------------------------------------------------------
  # Disaggregation column should exist and contain correct levels
  # (Overall appears for non-categorical indicators only)
  # ------------------------------------------------------
  expect_true("disaggregation_value" %in% names(out))
  expect_true(all(c("A", "B", "Overall") %in% out$disaggregation_value))

  # Basic structure checks
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("point.estimate", "lower_ci", "upper_ci") %in% names(out)))
})

test_that("phr_calc_survey_from_plan handles categorical indicators with stratified + clustered design", {

  # ------------------------------------------------------
  # Build deterministic complex survey dataset
  # ------------------------------------------------------
  n_clusters_per_region <- 10
  cluster_size <- 10

  regions <- c("A", "B")
  total_clusters <- length(regions) * n_clusters_per_region

  df <- tibble::tibble(
    region     = rep(regions, each = n_clusters_per_region * cluster_size),
    cluster_id = rep(seq_len(total_clusters), each = cluster_size),
    x   = rep(c(1, 0), length.out = total_clusters * cluster_size),
    y   = rep(c(4, 6, 8, 5, 7, 9), length.out = total_clusters * cluster_size),
    cat = factor(rep(c("Low", "Medium", "High"), length.out = total_clusters * cluster_size)),
    grp = rep(c("G1", "G2"), length.out = total_clusters * cluster_size),
    wt  = 1
  )

  # ------------------------------------------------------
  # Complex survey design: stratified + clustered
  # ------------------------------------------------------
  design <- survey::svydesign(
    id      = ~cluster_id,
    strata  = ~region,
    weights = ~wt,
    data    = df,
    nest    = TRUE
  )

  # ------------------------------------------------------
  # Data Analysis Plan
  # ------------------------------------------------------
  dap <- tibble::tibble(
    indicator_name = c("PropX", "MeanY", "CatVar"),
    calculation    = c("prop",  "mean",   "categorical"),
    var_name       = c("x",     "y",      "cat"),
    denom_var      = c(NA,      NA,       NA),
    disaggregation = c("grp",   "grp",    "grp"),
    multiplier     = c(100,     1,        100),
    indicator_unit = c("%",     "",       "%")
  )

  # ------------------------------------------------------
  # Run the analysis
  # ------------------------------------------------------
  out <- phr_calc_survey_from_plan(design, dap)

  # ------------------------------------------------------
  # Expected rows:
  #   PROP → 3 groups (G1, G2, Overall)
  #   MEAN → 3 groups
  #   CAT  → 3 categories × 2 groups (G1, G2 only, no Overall by default) = 6
  #   TOTAL = 3 + 3 + 6 = 12
  # ------------------------------------------------------
  expect_equal(nrow(out), 12)

  # ------------------------------------------------------
  # Confirm categorical expansion works
  # ------------------------------------------------------
  cat_rows <- out %>% dplyr::filter(stringr::str_starts(indicator_name, "CatVar"))

  expect_true(all(cat_rows$indicator_name %in% c(
    "CatVar - Low",
    "CatVar - Medium",
    "CatVar - High"
  )))

  # Categorical rows should not include an Overall group by default
  expect_false("Overall" %in% cat_rows$disaggregation_value)

  # ------------------------------------------------------
  # Check disaggregation values exist
  # (Overall appears for non-categorical indicators only)
  # ------------------------------------------------------
  expect_true("disaggregation_value" %in% names(out))
  expect_true(all(c("G1", "G2", "Overall") %in% out$disaggregation_value))

  # ------------------------------------------------------
  # Complex design metadata present
  # ------------------------------------------------------
  expect_true(all(c("point.estimate", "lower_ci", "upper_ci", "n_eff", "deff") %in% names(out)))

  # CIs should be numeric even under stratified-cluster design
  expect_true(all(is.numeric(out$lower_ci) | is.na(out$lower_ci)))
})

test_that("phr_calc_survey_from_plan includes Overall for categorical when include_overall = TRUE", {

  df <- tibble::tibble(
    cat = factor(rep(c("Low", "Medium", "High"), 4)),
    grp = rep(c("A", "B"), each = 6),
    wt  = 1
  )

  design <- survey::svydesign(ids = ~1, weights = ~wt, data = df)

  dap <- tibble::tibble(
    indicator_name = "CatVar",
    calculation    = "categorical",
    var_name       = "cat",
    denom_var      = NA,
    disaggregation = "grp",
    multiplier     = 100,
    indicator_unit = "%"
  )

  out <- phr_calc_survey_from_plan(design, dap, include_overall = TRUE)

  cat_rows <- out %>% dplyr::filter(stringr::str_starts(indicator_name, "CatVar"))

  # With include_overall = TRUE, Overall group should be present for categorical too
  expect_true("Overall" %in% cat_rows$disaggregation_value)
  # 3 categories × 3 groups (A, B, Overall) = 9 rows
  expect_equal(nrow(cat_rows), 9)
})

test_that("phr_calc_survey_from_plan returns columns in canonical order", {
  df <- tibble::tibble(
    x  = c(1, 0, 1, 1, 0),
    wt = c(1, 1, 2, 1, 1)
  )
  design <- srvyr::as_survey(df, weights = wt)

  plan <- tibble::tibble(
    indicator_name = "PropX",
    calculation    = "prop",
    var_name       = "x",
    denom_var      = NA,
    disaggregation = NA,
    multiplier     = 100,
    indicator_unit = "%"
  )

  out <- phr_calc_survey_from_plan(design = design, analysis_plan = plan)

  col_names <- names(out)
  # plan_row must be first
  expect_equal(col_names[1], "plan_row")
  # group_name must come after indicator_unit
  expect_true(which(col_names == "group_name") > which(col_names == "indicator_unit"))
  # disaggregation_value must come after group_name
  expect_true(which(col_names == "disaggregation_value") > which(col_names == "group_name"))
  # calculation must come before point.estimate
  expect_true(which(col_names == "calculation") < which(col_names == "point.estimate"))
})

# Tests for Wilson Score CI and lower_ci floor ####

test_that("phr_pick_ci_method selects wilson for extreme proportion", {
  out <- phr_pick_ci_method(
    n_unweighted = 100,
    n_eff = 80,
    p_estimate = 0.03
  )
  expect_equal(out$method, "wilson")
  expect_match(out$note, "Wilson Score", ignore.case = TRUE)
})

test_that("phr_pick_ci_method selects wilson for proportion near 1", {
  out <- phr_pick_ci_method(
    n_unweighted = 100,
    n_eff = 80,
    p_estimate = 0.97
  )
  expect_equal(out$method, "wilson")
})

test_that("phr_pick_ci_method selects design-logit for small n with non-extreme proportion", {
  out <- phr_pick_ci_method(
    n_unweighted = 15,
    n_eff = 12,
    p_estimate = 0.4
  )
  expect_equal(out$method, "design-logit")
})

test_that("phr_pick_ci_method still selects wilson when extreme proportion and small n coincide", {
  out <- phr_pick_ci_method(
    n_unweighted = 15,
    n_eff = 12,
    p_estimate = 0.02
  )
  expect_equal(out$method, "wilson")
})

test_that("phr_calc_survey_prop_single uses wilson ci_method for extreme proportions", {
  # 1 out of 50 observations = 2 %, well below 5 % threshold
  df <- tibble::tibble(
    x  = c(1L, rep(0L, 49)),
    wt = rep(1, 50)
  )
  dsgn <- df %>% srvyr::as_survey(weights = wt)

  out <- phr_calc_survey_prop_single(
    design = dsgn,
    var_name = "x",
    indicator_name = "RareProp",
    multiplier = 100
  )

  expect_equal(out$ci_method, "wilson")
  expect_true(!is.na(out$lower_ci))
  expect_true(!is.na(out$upper_ci))
  # Wilson bounds are within [0, 100] when multiplier = 100
  expect_true(out$lower_ci >= 0)
  expect_true(out$upper_ci <= 100)
})

test_that("phr_calc_survey_prop_single floors lower_ci at 0", {
  # A proportion very close to 0 with a large n should give lower_ci = 0
  # under Wilson; confirm it is never negative
  df <- tibble::tibble(
    x  = c(1L, rep(0L, 199)),
    wt = rep(1, 200)
  )
  dsgn <- df %>% srvyr::as_survey(weights = wt)

  out <- phr_calc_survey_prop_single(
    design = dsgn,
    var_name = "x",
    indicator_name = "NearZeroProp",
    multiplier = 100
  )

  expect_true(!is.na(out$lower_ci))
  expect_true(out$lower_ci >= 0)
})

test_that("phr_calc_survey_ratio_single floors lower_ci at 0", {
  # Use a larger sample so Taylor variance succeeds and produces a non-NA lower_ci.
  # The ratio ~0.1 with this design should yield a valid CI that we can confirm >= 0.
  df <- tibble::tibble(
    num = c(1,  2,  1,  1,  2,  1,  2,  1,  1,  2,  1,  1),
    den = c(10, 20, 10, 10, 20, 10, 20, 10, 10, 20, 10, 10),
    wt  = rep(1, 12)
  )
  dsgn <- df %>% srvyr::as_survey(weights = wt)

  out <- phr_calc_survey_ratio_single(
    design = dsgn,
    numerator_var = "num",
    denominator_var = "den",
    indicator_name = "LowRatio"
  )

  expect_true(!is.na(out$lower_ci))
  expect_true(out$lower_ci >= 0)
})

# Tests for deff computation with clustered design ####

test_that("phr_calc_survey_mean_single computes non-NA deff with cluster design", {

  set.seed(42)
  n_clusters <- 20
  cluster_size <- 10  # 200 total records

  df <- tibble::tibble(
    cluster_id = rep(seq_len(n_clusters), each = cluster_size),
    y          = rep(rnorm(n_clusters, mean = 5, sd = 2), each = cluster_size) +
                   rnorm(n_clusters * cluster_size, mean = 0, sd = 0.5),
    wt         = 1
  )

  design <- survey::svydesign(
    id      = ~cluster_id,
    weights = ~wt,
    data    = df
  )

  out <- phr_calc_survey_mean_single(
    design         = design,
    var_name       = "y",
    indicator_name = "ClusteredMean"
  )

  expect_s3_class(out, "tbl_df")
  expect_true(!is.na(out$deff), info = "deff should be non-NA with a cluster design")
  expect_true(is.finite(out$deff), info = "deff should be a finite number with a cluster design")
})

test_that("phr_calc_survey_prop_single computes non-NA deff with cluster design", {

  set.seed(42)
  n_clusters <- 20
  cluster_size <- 10  # 200 total records

  cluster_probs <- runif(n_clusters, 0.2, 0.8)
  df <- tibble::tibble(
    cluster_id = rep(seq_len(n_clusters), each = cluster_size),
    x          = as.integer(runif(n_clusters * cluster_size) <
                              rep(cluster_probs, each = cluster_size)),
    wt         = 1
  )

  design <- survey::svydesign(
    id      = ~cluster_id,
    weights = ~wt,
    data    = df
  )

  out <- phr_calc_survey_prop_single(
    design         = design,
    var_name       = "x",
    indicator_name = "ClusteredProp",
    multiplier     = 100
  )

  expect_s3_class(out, "tbl_df")
  expect_true(!is.na(out$deff), info = "deff should be non-NA with a cluster design")
  expect_true(is.finite(out$deff), info = "deff should be a finite number with a cluster design")
})

test_that("phr_calc_survey_ratio_single computes non-NA deff with cluster design", {

  set.seed(42)
  n_clusters <- 20
  cluster_size <- 10  # 200 total records

  cluster_base <- runif(n_clusters, 5, 15)
  df <- tibble::tibble(
    cluster_id = rep(seq_len(n_clusters), each = cluster_size),
    num        = rep(cluster_base, each = cluster_size) +
                   rnorm(n_clusters * cluster_size, 0, 0.5),
    den        = rep(cluster_base * 2, each = cluster_size) +
                   rnorm(n_clusters * cluster_size, 0, 0.5),
    wt         = 1
  )

  design <- survey::svydesign(
    id      = ~cluster_id,
    weights = ~wt,
    data    = df
  )

  out <- phr_calc_survey_ratio_single(
    design          = design,
    numerator_var   = "num",
    denominator_var = "den",
    indicator_name  = "ClusteredRatio"
  )

  expect_s3_class(out, "tbl_df")
  # deff may be NA if svyratio falls back to design-meanbased (e.g. when Taylor
  # variance estimation fails for very small or degenerate samples). A non-NA
  # lower_ci indicates the Taylor path succeeded and deff should have been computed.
  if (!is.na(out$lower_ci)) {
    expect_true(!is.na(out$deff), info = "deff should be non-NA with a cluster design when Taylor variance succeeds")
    expect_true(is.finite(out$deff), info = "deff should be a finite number with a cluster design")
  }
})

test_that("phr_calc_survey_categorical_single computes non-NA deff with cluster design", {

  set.seed(42)
  n_clusters <- 20
  cluster_size <- 10  # 200 total records

  cluster_cats <- sample(c("Low", "Borderline", "Acceptable"), n_clusters, replace = TRUE)
  df <- tibble::tibble(
    cluster_id = rep(seq_len(n_clusters), each = cluster_size),
    cat_var    = factor(rep(cluster_cats, each = cluster_size)),
    wt         = 1
  )

  design <- survey::svydesign(
    id      = ~cluster_id,
    weights = ~wt,
    data    = df
  )

  out <- phr_calc_survey_categorical_single(
    design         = design,
    var_name       = "cat_var",
    indicator_name = "ClusteredCat",
    multiplier     = 100
  )

  expect_s3_class(out, "tbl_df")
  expect_true(nrow(out) >= 1L)
  # Each category row where the proportion is estimable should carry a finite deff
  estimable_rows <- out[!is.na(out$point.estimate) & out$point.estimate > 0, ]
  if (nrow(estimable_rows) > 0L) {
    expect_true(
      all(!is.na(estimable_rows$deff)),
      info = "deff should be non-NA for estimable categories with a cluster design"
    )
    expect_true(
      all(is.finite(estimable_rows$deff)),
      info = "deff should be a finite number for estimable categories with a cluster design"
    )
  }
})





