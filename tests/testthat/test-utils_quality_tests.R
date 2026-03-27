# ---------------------------
# UTILS QUALITY TESTS TEST SUITE
# ---------------------------

library(testthat)
library(tibble)


# 1. CORRELATION TEST ####


test_that("quality_test_correlation calculates correlation correctly", {
  df <- tibble::tibble(
    var1 = c(1, 2, 3, 4, 5),
    var2 = c(2, 4, 6, 8, 10)
  )

  result <- quality_test_correlation(df, c("var1", "var2"))

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_equal(result$statistic, 1.0, tolerance = 0.01)  # Perfect correlation
})

test_that("quality_test_correlation handles missing values", {
  df <- tibble::tibble(
    var1 = c(1, 2, NA, 4, 5),
    var2 = c(2, 4, 6, NA, 10)
  )

  result <- quality_test_correlation(df, c("var1", "var2"))

  expect_true(is.list(result))
  # Should still calculate correlation for complete cases
})

test_that("quality_test_correlation errors with wrong number of variables", {
  df <- tibble::tibble(var1 = 1:5)

  expect_warning(
    quality_test_correlation(df, c("var1")),
    regexp = "exactly 2 variables"
  )
})

test_that("quality_test_correlation handles missing variables", {
  df <- tibble::tibble(var1 = 1:5, var2 = 2:6)

  expect_warning(
    result <- quality_test_correlation(df, c("var1", "nonexistent")),
    regexp = "variables not found"  # May or may not warn
  )
})

test_that("quality_test_correlation supports different methods", {
  df <- tibble::tibble(
    var1 = c(1, 2, 3, 4, 5),
    var2 = c(2, 4, 6, 8, 10)
  )

  pearson <- quality_test_correlation(df, c("var1", "var2"), method = "pearson")
  spearman <- quality_test_correlation(df, c("var1", "var2"), method = "spearman")

  expect_true(!is.na(pearson$statistic))
  expect_true(!is.na(spearman$statistic))
})

test_that("quality_test_correlation handles insufficient data", {
  df <- tibble::tibble(
    var1 = c(1, NA, NA),
    var2 = c(NA, 2, NA)
  )

  expect_warning(
    result <- quality_test_correlation(df, c("var1", "var2")),
    regexp = "Insufficient complete cases"  # May warn about insufficient cases
  )
})

test_that("quality_test functions handle edge cases gracefully", {
  # Empty data frame
  df_empty <- tibble::tibble(var1 = numeric(0), var2 = numeric(0))

  expect_warning(
    result <- quality_test_correlation(df_empty, c("var1", "var2")),
    regexp = "Insufficient complete cases"
  )

  # All NA data
  df_na <- tibble::tibble(var1 = rep(NA_real_, 5), var2 = rep(NA_real_, 5))

  expect_warning(
    result <- quality_test_correlation(df_na, c("var1", "var2")),
    regexp = "Insufficient complete cases"
  )
})

# 2. T-TEST####


test_that("quality_test_ttest performs one-sample t-test", {
  df <- tibble::tibble(
    var1 = c(10, 12, 14, 16, 18)
  )

  result <- quality_test_ttest(df, "var1", mu = 10)

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
})

test_that("quality_test_ttest performs two-sample t-test", {
  df <- tibble::tibble(
    var1 = c(10, 12, 14, 16, 18),
    var2 = c(11, 13, 15, 17, 19)
  )

  result <- quality_test_ttest(df, c("var1", "var2"))

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))
  expect_true(!is.na(result$p_value))
})

test_that("quality_test_ttest handles paired t-test", {
  df <- tibble::tibble(
    before = c(10, 10, 8, 25, 18),
    after = c(11, 13, 15, 17, 19)
  )

  result <- quality_test_ttest(df, c("before", "after"), paired = TRUE)

  expect_true(is.list(result))
})

test_that("quality_test_ttest errors with invalid number of variables", {
  df <- tibble::tibble(var1 = 1:5, var2 = 2:6, var3 = 3:7)

  expect_warning(
    quality_test_ttest(df, c("var1", "var2", "var3")),
    regexp = "1 or 2 variables"
  )
})

test_that("quality_test_ttest handles missing variable", {
  df <- tibble::tibble(var1 = 1:5)

  expect_warning(
    result <- quality_test_ttest(df, "nonexistent"),
    regexp = "Variable not found"  # May or may not warn
  )
})

test_that("quality_test_ttest handles insufficient data", {
  df <- tibble::tibble(var1 = c(1, NA))

  expect_warning(
    result <- quality_test_ttest(df, "var1"),
    regexp = "Insufficient data"  # May warn about insufficient data
  )
})

test_that("quality_test_ttest removes NA values", {
  df <- tibble::tibble(
    var1 = c(10, NA, 14, 16, 18)
  )

  result <- quality_test_ttest(df, "var1", mu = 10)

  expect_true(is.list(result))
  # Should calculate with non-NA values
})


# 3. CHI-SQUARED TEST ####


test_that("quality_test_chisq performs chi-squared test", {
  df <- tibble::tibble(
    gender = rep(c("M", "F"), each = 25),
    outcome = sample(c("yes", "no"), 50, replace = TRUE)
  )

  result <- quality_test_chisq(df, c("gender", "outcome"))

  expect_true(is.list(result))
  expect_true("statistic" %in% names(result))
  expect_true("p_value" %in% names(result))
})

test_that("quality_test_chisq errors with wrong number of variables", {
  df <- tibble::tibble(var1 = c("a", "b", "c"))

  expect_warning(
    quality_test_chisq(df, "var1"),
    regexp = "exactly 2 variables"
  )
})

test_that("quality_test_chisq handles missing variables", {
  df <- tibble::tibble(var1 = c("a", "b"), var2 = c("x", "y"))

  expect_warning(
    result <- quality_test_chisq(df, c("var1", "nonexistent")),
    regexp = "both variables not found"  # May or may not warn
  )
})

test_that("quality_test_chisq handles insufficient data", {
  df <- tibble::tibble(
    var1 = c("a", "b"),
    var2 = c("x", "y")
  )

  expect_warning(
    result <- quality_test_chisq(df, c("var1", "var2")),
    regexp = "Insufficient data"  # May warn about insufficient data
  )
})

test_that("quality_test_chisq removes missing values", {
  df <- tibble::tibble(
    gender = c("M", "F", NA, "M", "F", "M", "F", "M", "F", "M"),
    outcome = c("yes", "no", "yes", NA, "no", "yes", "no", "yes", "no", "yes")
  )

  result <- quality_test_chisq(df, c("gender", "outcome"))

  expect_true(is.list(result))
  # Should calculate with complete cases only
})

test_that("quality_test_chisq handles small contingency tables", {
  df <- tibble::tibble(
    var1 = rep("a", 10),  # Only one level
    var2 = sample(c("x", "y"), 10, replace = TRUE)
  )

  expect_warning(
    result <- quality_test_chisq(df, c("var1", "var2")),
    regexp = "Contingency table too small"  # May warn about table size
  )
})

# 4. FLAG PERCENTAGE TEST ####

# Test Suite for quality_test_flag_percentage

test_that("quality_test_flag_percentage calculates percentage correctly with TRUE flags", {
  df <- tibble::tibble(
    flag = c(TRUE, TRUE, FALSE, TRUE, FALSE)
  )

  result <- quality_test_flag_percentage(df, "flag", flag_value = TRUE)

  expect_true(is.list(result))
  expect_equal(result$statistic, 60)  # 3 out of 5 = 60%
  expect_true(is.na(result$p_value))
})

test_that("quality_test_flag_percentage works with numeric flags", {
  df <- tibble::tibble(
    status = c(1, 0, 1, 1, 0, 0, 1)
  )

  result <- quality_test_flag_percentage(df, "status", flag_value = 1)

  expect_true(is.list(result))
  expect_equal(result$statistic, 4/7 * 100)  # 4 out of 7 ≈ 57.14%
  expect_true(is.na(result$p_value))
})

test_that("quality_test_flag_percentage handles all flags TRUE/FALSE", {
  # All TRUE
  df_all_true <- tibble::tibble(
    flag = c(TRUE, TRUE, TRUE, TRUE)
  )

  result_all_true <- quality_test_flag_percentage(df_all_true, "flag", flag_value = TRUE)
  expect_equal(result_all_true$statistic, 100)

  # All FALSE
  df_all_false <- tibble::tibble(
    flag = c(FALSE, FALSE, FALSE, FALSE)
  )

  result_all_false <- quality_test_flag_percentage(df_all_false, "flag", flag_value = TRUE)
  expect_equal(result_all_false$statistic, 0)
})

test_that("quality_test_flag_percentage handles NA values correctly", {
  df <- tibble::tibble(
    flag = c(TRUE, NA, FALSE, TRUE, NA, FALSE)
  )

  result <- quality_test_flag_percentage(df, "flag", flag_value = TRUE)

  expect_true(is.list(result))
  # Should ignore NAs: 2 TRUE out of 4 non-NA = 50%
  expect_equal(result$statistic, 50)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_flag_percentage handles edge cases and errors", {
  # Edge case: All NA values
  df_all_na <- tibble::tibble(
    flag = c(NA, NA, NA)
  )

  expect_warning(
    result_all_na <- quality_test_flag_percentage(df_all_na, "flag", flag_value = TRUE),
    "No non-missing values found"
  )
  expect_true(is.na(result_all_na$statistic))
  expect_true(is.na(result_all_na$p_value))

  # Edge case: Single value
  df_single <- tibble::tibble(
    flag = c(TRUE)
  )

  result_single <- quality_test_flag_percentage(df_single, "flag", flag_value = TRUE)
  expect_equal(result_single$statistic, 100)

  # Error case: Variable not found
  df <- tibble::tibble(
    flag = c(TRUE, FALSE)
  )

  expect_warning(
    result_missing <- quality_test_flag_percentage(df, "nonexistent", flag_value = TRUE),
    "Variable not found in data"
  )
  expect_true(is.na(result_missing$statistic))

  # Error case: Wrong number of variables
  expect_warning(
    quality_test_flag_percentage(df, c("flag", "other"), flag_value = TRUE),
    "Flag percentage test requires exactly 1 variable"
  )

  expect_warning(
    quality_test_flag_percentage(df, character(0), flag_value = TRUE),
    "Flag percentage test requires exactly 1 variable"
  )
})

test_that("quality_test_flag_percentage works with custom flag values", {
  df <- tibble::tibble(
    status = c("yes", "no", "yes", "yes", "no")
  )

  result <- quality_test_flag_percentage(df, "status", flag_value = "yes")

  expect_true(is.list(result))
  expect_equal(result$statistic, 60)  # 3 out of 5 = 60%
  expect_true(is.na(result$p_value))
})

# 5. FLAG PERCENT MISSING ####

# Test Suite for quality_test_missing_percentage

test_that("quality_test_missing_percentage calculates percentage correctly for single variable", {
  df <- tibble::tibble(
    var1 = c(1, 2, NA, 4, NA)
  )

  result <- quality_test_missing_percentage(df, "var1")

  expect_true(is.list(result))
  expect_equal(result$statistic, 40)  # 2 out of 5 = 40%
  expect_true(is.na(result$p_value))
})

test_that("quality_test_missing_percentage works with multiple variables", {
  df <- tibble::tibble(
    var1 = c(1, NA, 3, 4),
    var2 = c(NA, 2, 3, NA),
    var3 = c(1, 2, 3, 4)
  )

  result <- quality_test_missing_percentage(df, c("var1", "var2", "var3"))

  expect_true(is.list(result))
  # var1: 1 NA, var2: 2 NAs, var3: 0 NAs = 3 NAs out of 12 cells = 25%
  expect_equal(result$statistic, 25)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_missing_percentage handles all missing or no missing cases", {
  # All missing
  df_all_missing <- tibble::tibble(
    var1 = c(NA, NA, NA),
    var2 = c(NA, NA, NA)
  )

  result_all_missing <- quality_test_missing_percentage(df_all_missing, c("var1", "var2"))
  expect_equal(result_all_missing$statistic, 100)

  # No missing
  df_no_missing <- tibble::tibble(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6)
  )

  result_no_missing <- quality_test_missing_percentage(df_no_missing, c("var1", "var2"))
  expect_equal(result_no_missing$statistic, 0)
})

test_that("quality_test_missing_percentage handles partial variable matches", {
  df <- tibble::tibble(
    var1 = c(1, NA, 3),
    var2 = c(4, 5, 6)
  )

  # Some variables exist, some don't
  result <- quality_test_missing_percentage(df, c("var1", "var2", "nonexistent"))

  expect_true(is.list(result))
  # Should only count var1 and var2: 1 NA out of 6 cells ≈ 16.67%
  expect_equal(result$statistic, 1/6 * 100, tolerance = 0.01)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_missing_percentage handles different data types", {
  df <- tibble::tibble(
    numeric_var = c(1, NA, 3),
    character_var = c("a", NA, "c"),
    logical_var = c(TRUE, FALSE, NA),
    date_var = as.Date(c("2023-01-01", NA, "2023-01-03"))
  )

  result <- quality_test_missing_percentage(
    df,
    c("numeric_var", "character_var", "logical_var", "date_var")
  )

  expect_true(is.list(result))
  # 4 NAs out of 12 cells = 33.3%
  expect_equal(result$statistic, 100/3)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_missing_percentage handles edge cases and errors", {
  df <- tibble::tibble(
    var1 = c(1, 2, 3)
  )

  # Edge case: No variables specified
  expect_warning(
    quality_test_missing_percentage(df, character(0)),
    "Missing percentage test requires at least 1 variable"
  )

  # Edge case: None of the specified variables exist
  expect_warning(
    result_no_vars <- quality_test_missing_percentage(df, c("nonexistent1", "nonexistent2")),
    "No specified variables found in data"
  )
  expect_true(is.na(result_no_vars$statistic))
  expect_true(is.na(result_no_vars$p_value))

  # Edge case: Single value, no missing
  df_single <- tibble::tibble(
    var1 = c(1)
  )

  result_single <- quality_test_missing_percentage(df_single, "var1")
  expect_equal(result_single$statistic, 0)

  # Edge case: Single value, is missing
  df_single_na <- tibble::tibble(
    var1 = c(NA)
  )

  result_single_na <- quality_test_missing_percentage(df_single_na, "var1")
  expect_equal(result_single_na$statistic, 100)
})

test_that("quality_test_missing_percentage handles empty data frame edge case", {
  # Empty data frame (0 rows)
  df_empty <- tibble::tibble(
    var1 = numeric(0),
    var2 = character(0)
  )

  result <- quality_test_missing_percentage(df_empty, c("var1", "var2"))

  # 0 total cells should return NA
  expect_true(is.na(result$statistic))
  expect_true(is.na(result$p_value))
})

test_that("quality_test_missing_percentage calculates correctly with varying missingness patterns", {
  df <- tibble::tibble(
    complete = c(1, 2, 3, 4, 5),           # 0 NAs
    half_missing = c(1, NA, 3, NA, 5),     # 2 NAs
    mostly_missing = c(NA, NA, NA, NA, 5)  # 4 NAs
  )

  result <- quality_test_missing_percentage(df, c("complete", "half_missing", "mostly_missing"))

  expect_true(is.list(result))
  # 0 + 2 + 4 = 6 NAs out of 15 cells = 40%
  expect_equal(result$statistic, 40)
  expect_true(is.na(result$p_value))
})

# 6. OUTLIER DETECTION TESTS with Z-SCORES ####

# Test Suite for quality_test_outlier_percentage

test_that("quality_test_outlier_percentage calculates percentage correctly with outliers", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5, 100)  # 100 is an outlier
  )

  result <- quality_test_outlier_percentage(df, "values", z_threshold = 2)

  expect_true(is.list(result))
  expect_true(result$statistic > 0)  # Should detect at least one outlier
  expect_true(is.na(result$p_value))
})

test_that("quality_test_outlier_percentage works with no outliers", {
  df <- tibble::tibble(
    values = c(10, 11, 12, 13, 14, 15)  # No extreme outliers
  )

  result <- quality_test_outlier_percentage(df, "values", z_threshold = 3)

  expect_true(is.list(result))
  expect_equal(result$statistic, 0)  # No outliers expected
  expect_true(is.na(result$p_value))
})

test_that("quality_test_outlier_percentage works with different z_threshold values", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 50)
  )

  # Stricter threshold (z = 2)
  result_strict <- quality_test_outlier_percentage(df, "values", z_threshold = 2)

  # Looser threshold (z = 4)
  result_loose <- quality_test_outlier_percentage(df, "values", z_threshold = 4)

  # Stricter threshold should find more outliers
  expect_true(result_strict$statistic >= result_loose$statistic)
})

test_that("quality_test_outlier_percentage handles NA values correctly", {
  df <- tibble::tibble(
    values = c(1, 2, NA, 3, 4, NA, 5, 100)
  )

  result <- quality_test_outlier_percentage(df, "values", z_threshold = 3)

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))  # Should calculate despite NAs
  expect_true(is.na(result$p_value))
})

test_that("quality_test_outlier_percentage handles multiple outliers", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5, 100, 200)  # Two outliers
  )

  result <- quality_test_outlier_percentage(df, "values", z_threshold = .7)

  expect_true(is.list(result))
  expect_true(result$statistic > 0)
  # With 2 outliers out of 7 values, should be ~28.57%
  expect_true(result$statistic > 28)
})

test_that("quality_test_outlier_percentage handles zero variance data", {
  # All identical values
  df <- tibble::tibble(
    values = c(5, 5, 5, 5, 5)
  )


    result <- quality_test_outlier_percentage(df, "values", z_threshold = 3)

  expect_true(is.list(result))
  expect_equal(result$statistic, 0)  # No outliers when all values identical
  expect_true(is.na(result$p_value))
})

test_that("quality_test_outlier_percentage validates input types correctly", {
  df <- tibble::tibble(
    numeric_var = c(1, 2, 3, 4, 5),
    character_var = c("a", "b", "c", "d", "e"),
    logical_var = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )

  # Should work with numeric
  result_numeric <- quality_test_outlier_percentage(df, "numeric_var", z_threshold = 3)
  expect_true(is.list(result_numeric))

  # Should warn with character
  expect_warning(
    result_char <- quality_test_outlier_percentage(df, "character_var", z_threshold = 3),
    "numeric data"
  )

  # Should warn with logical
  expect_warning(
    result_logical <- quality_test_outlier_percentage(df, "logical_var", z_threshold = 3),
    "numeric data"
  )
})

test_that("quality_test_outlier_percentage handles edge cases and errors", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5)
  )

  # Error case: Not a data frame
  expect_warning(
    quality_test_outlier_percentage(list(values = c(1, 2, 3)), "values"),
    "data frame"
  )

  # Error case: Wrong number of variables
  expect_warning(
    quality_test_outlier_percentage(df, c("values", "other")),
    "exactly 1 variable"
  )

  expect_warning(
    quality_test_outlier_percentage(df, character(0)),
    "exactly 1 variable"
  )

  # Error case: NULL variables
  expect_warning(
    quality_test_outlier_percentage(df, NULL),
    "exactly 1 variable"
  )

  # Error case: Variable not character
  expect_warning(
    quality_test_outlier_percentage(df, 1),
    "character string"
  )

  # Error case: Variable not found
  expect_warning(
    result_missing <- quality_test_outlier_percentage(df, "nonexistent"),
    "not found"
  )
  expect_true(is.na(result_missing$statistic))

  # Edge case: Insufficient data (< 3 values)
  df_small <- tibble::tibble(
    values = c(1, 2)
  )

  expect_warning(
    result_small <- quality_test_outlier_percentage(df_small, "values"),
    "Insufficient data"
  )
  expect_true(is.na(result_small$statistic))

  # Edge case: All NA values
  df_all_na <- tibble::tibble(
    values = c(NA, NA, NA, NA)
  )

  expect_warning(
    result_all_na <- quality_test_outlier_percentage(df_all_na, "values"),
    "requires numeric data"
  )
  expect_true(is.na(result_all_na$statistic))
})

test_that("quality_test_outlier_percentage validates z_threshold parameter", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5, 100)
  )

  # Error case: Non-numeric z_threshold
  expect_warning(
    quality_test_outlier_percentage(df, "values", z_threshold = "3"),
    "single numeric value"
  )

  # Error case: Negative z_threshold
  expect_warning(
    quality_test_outlier_percentage(df, "values", z_threshold = -2),
    "positive number"
  )

  # Error case: Zero z_threshold
  expect_warning(
    quality_test_outlier_percentage(df, "values", z_threshold = 0),
    "positive number"
  )

  # Error case: NA z_threshold
  expect_warning(
    quality_test_outlier_percentage(df, "values", z_threshold = NA),
    "single numeric value"
  )

  # Error case: Multiple z_threshold values
  expect_warning(
    quality_test_outlier_percentage(df, "values", z_threshold = c(2, 3)),
    "single numeric value"
  )

})

test_that("quality_test_outlier_percentage calculates exact percentages correctly", {
  # Create data with known outliers
  df <- tibble::tibble(
    values = c(rep(10, 9), 1000)  # 1 outlier out of 10 = 10%
  )

  result <- quality_test_outlier_percentage(df, "values", z_threshold = 2)

  expect_true(is.list(result))
  expect_equal(result$statistic, 10, tolerance = 0.1)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_outlier_percentage handles negative outliers", {
  df <- tibble::tibble(
    values = c(100, 101, 102, 103, 104, 1)  # 1 is a negative outlier
  )

  result <- quality_test_outlier_percentage(df, "values", z_threshold = 2)

  expect_true(is.list(result))
  expect_true(result$statistic > 0)  # Should detect the negative outlier
  expect_true(is.na(result$p_value))
})

test_that("quality_test_outlier_percentage handles extreme outliers on both ends", {
  df <- tibble::tibble(
    values = c(-100, 10, 11, 12, 13, 14, 15, 200)  # Outliers on both ends
  )

  result <- quality_test_outlier_percentage(df, "values", z_threshold = 1.45)

  expect_true(is.list(result))
  # Should detect both outliers: 2 out of 8 = 25%
  expect_true(result$statistic >= 20)
  expect_true(is.na(result$p_value))
})

# 7. COEFFICIENT VARIATION TEST ####

# Test Suite for quality_test_coefficient_variation

test_that("quality_test_coefficient_variation calculates CV correctly with normal data", {
  df <- tibble::tibble(
    values = c(10, 12, 14, 16, 18, 20)
  )

  result <- quality_test_coefficient_variation(df, "values")

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))
  expect_true(result$statistic > 0)  # Should have some variation
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation calculates exact CV correctly", {
  # Create data with known mean and sd
  # values: 10, 20, 30 -> mean = 20, sd = 10, CV = (10/20)*100 = 50%
  df <- tibble::tibble(
    values = c(10, 20, 30)
  )

  result <- quality_test_coefficient_variation(df, "values")

  expect_true(is.list(result))
  expect_equal(result$statistic, 50, tolerance = 0.1)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles low variation data", {
  # Values very close together should have low CV
  df <- tibble::tibble(
    values = c(100, 100.1, 100.2, 99.9, 99.8)
  )

  result <- quality_test_coefficient_variation(df, "values")

  expect_true(is.list(result))
  expect_true(result$statistic < 1)  # Very low CV
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles high variation data", {
  # Values spread far apart should have high CV
  df <- tibble::tibble(
    values = c(1, 50, 100, 150, 200)
  )

  result <- quality_test_coefficient_variation(df, "values")

  expect_true(is.list(result))
  expect_true(result$statistic > 50)  # High CV
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles NA values correctly", {
  df <- tibble::tibble(
    values = c(10, NA, 20, NA, 30, 40)
  )

  result <- quality_test_coefficient_variation(df, "values")

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))  # Should calculate despite NAs
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles zero variance data", {
  # All identical values
  df <- tibble::tibble(
    values = c(5, 5, 5, 5, 5)
  )

  expect_warning(
    result <- quality_test_coefficient_variation(df, "values"),
    "zero variance"
  )

  expect_true(is.list(result))
  expect_equal(result$statistic, 0)  # CV = 0 when no variation
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles zero mean data", {
  # Values that average to zero
  df <- tibble::tibble(
    values = c(-10, -5, 0, 5, 10)
  )

  expect_warning(
    result <- quality_test_coefficient_variation(df, "values"),
    "Mean is zero"
  )

  expect_true(is.list(result))
  expect_true(is.na(result$statistic))  # CV undefined when mean = 0
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles negative values correctly", {
  # CV should use absolute value of mean
  df <- tibble::tibble(
    values = c(-20, -18, -16, -14, -12, -10)
  )

  result <- quality_test_coefficient_variation(df, "values")

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))
  expect_true(result$statistic > 0)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation validates input types correctly", {
  df <- tibble::tibble(
    numeric_var = c(1, 2, 3, 4, 5),
    character_var = c("a", "b", "c", "d", "e"),
    logical_var = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )

  # Should work with numeric
  result_numeric <- quality_test_coefficient_variation(df, "numeric_var")
  expect_true(is.list(result_numeric))
  expect_true(!is.na(result_numeric$statistic))

  # Should warn with character
  expect_warning(
    result_char <- quality_test_coefficient_variation(df, "character_var"),
    "numeric data"
  )
  expect_true(is.na(result_char$statistic))

  # Should warn with logical
  expect_warning(
    result_logical <- quality_test_coefficient_variation(df, "logical_var"),
    "numeric data"
  )
  expect_true(is.na(result_logical$statistic))
})

test_that("quality_test_coefficient_variation handles edge cases and errors", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5)
  )

  # Error case: Not a data frame
  expect_warning(
    quality_test_coefficient_variation(list(values = c(1, 2, 3)), "values"),
    "data frame"
  )

  # Error case: Wrong number of variables
  expect_warning(
    quality_test_coefficient_variation(df, c("values", "other")),
    "exactly 1 variable"
  )

  expect_warning(
    quality_test_coefficient_variation(df, character(0)),
    "exactly 1 variable"
  )

  # Error case: NULL variables
  expect_warning(
    quality_test_coefficient_variation(df, NULL),
    "exactly 1 variable"
  )

  # Error case: Variable not character
  expect_warning(
    quality_test_coefficient_variation(df, 1),
    "character string"
  )

  # Error case: Variable not found
  expect_warning(
    result_missing <- quality_test_coefficient_variation(df, "nonexistent"),
    "not found"
  )
  expect_true(is.na(result_missing$statistic))

  # Edge case: Insufficient data (< 2 values)
  df_small <- tibble::tibble(
    values = c(1)
  )

  expect_warning(
    result_small <- quality_test_coefficient_variation(df_small, "values"),
    "Insufficient data"
  )
  expect_true(is.na(result_small$statistic))

  # Edge case: All NA values
  df_all_na <- tibble::tibble(
    values = c(NA, NA, NA, NA)
  )

  expect_warning(
    result_all_na <- quality_test_coefficient_variation(df_all_na, "values"),
    "strictly numeric value"
  )
  expect_true(is.na(result_all_na$statistic))
})

test_that("quality_test_coefficient_variation handles minimum data case", {
  # Exactly 2 values (minimum for CV calculation)
  df <- tibble::tibble(
    values = c(10, 20)
  )

  result <- quality_test_coefficient_variation(df, "values")

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))
  # For values 10, 20: mean = 15, sd = 7.07, CV ≈ 47.14%
  expect_equal(result$statistic, (sd(c(10, 20)) / mean(c(10, 20))) * 100, tolerance = 0.01)
})

test_that("quality_test_coefficient_variation handles empty data frame edge case", {
  # Empty data frame (0 rows)
  df_empty <- tibble::tibble(
    values = numeric(0)
  )

  expect_warning(
    result <- quality_test_coefficient_variation(df_empty, "values"),
    "Insufficient data"
  )

  expect_true(is.na(result$statistic))
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation compares relative variation correctly", {
  # Two datasets with same SD but different means
  # Higher mean should have lower CV
  df1 <- tibble::tibble(
    low_mean = c(8, 10, 12)  # mean = 10, sd ≈ 2
  )

  df2 <- tibble::tibble(
    high_mean = c(98, 100, 102)  # mean = 100, sd ≈ 2
  )

  result1 <- quality_test_coefficient_variation(df1, "low_mean")
  result2 <- quality_test_coefficient_variation(df2, "high_mean")

  # CV should be higher for lower mean (same SD, smaller mean = higher CV)
  expect_true(result1$statistic > result2$statistic)
})

test_that("quality_test_coefficient_variation handles mixed positive and negative values", {
  # Mixed values with non-zero mean
  df <- tibble::tibble(
    values = c(-5, -2, 1, 4, 7, 10)  # mean = 2.5
  )

  result <- quality_test_coefficient_variation(df, "values")

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))
  expect_true(result$statistic > 0)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_coefficient_variation handles very small positive values", {
  # Very small positive values near zero
  df <- tibble::tibble(
    values = c(0.001, 0.002, 0.003, 0.004, 0.005)
  )

  result <- quality_test_coefficient_variation(df, "values")

  expect_true(is.list(result))
  expect_true(!is.na(result$statistic))
  expect_true(is.finite(result$statistic))
})

# 8. RANGE VIOLATION TESTS ####

# Test Suite for quality_test_range_violation

test_that("quality_test_range_violation calculates percentage correctly with violations", {
  df <- tibble::tibble(
    values = c(1, 5, 10, 15, 20, 100)  # 100 is outside range [0, 50]
  )

  result <- quality_test_range_violation(df, "values", min_value = 0, max_value = 50)

  expect_true(is.list(result))
  expect_equal(result$statistic, 1/6 * 100, tolerance = 0.01)  # 1 out of 6 ≈ 16.67%
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation works with no violations", {
  df <- tibble::tibble(
    values = c(10, 20, 30, 40, 50)
  )

  result <- quality_test_range_violation(df, "values", min_value = 0, max_value = 100)

  expect_true(is.list(result))
  expect_equal(result$statistic, 0)  # All values within range
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation works with all violations", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5)
  )

  result <- quality_test_range_violation(df, "values", min_value = 10, max_value = 20)

  expect_true(is.list(result))
  expect_equal(result$statistic, 100)  # All values outside range
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation handles lower bound violations", {
  df <- tibble::tibble(
    values = c(-5, -2, 0, 5, 10, 15)
  )

  result <- quality_test_range_violation(df, "values", min_value = 0, max_value = 100)

  expect_true(is.list(result))
  expect_equal(result$statistic, 2/6 * 100, tolerance = 0.01)  # 2 values below 0
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation handles upper bound violations", {
  df <- tibble::tibble(
    values = c(5, 10, 15, 20, 105, 110)
  )

  result <- quality_test_range_violation(df, "values", min_value = 0, max_value = 100)

  expect_true(is.list(result))
  expect_equal(result$statistic, 2/6 * 100, tolerance = 0.01)  # 2 values above 100
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation handles both bound violations", {
  df <- tibble::tibble(
    values = c(-10, 5, 10, 15, 20, 110)
  )

  result <- quality_test_range_violation(df, "values", min_value = 0, max_value = 100)

  expect_true(is.list(result))
  expect_equal(result$statistic, 2/6 * 100, tolerance = 0.01)  # 1 below, 1 above
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation handles NA values correctly", {
  df <- tibble::tibble(
    values = c(5, NA, 10, NA, 15, 105)
  )

  result <- quality_test_range_violation(df, "values", min_value = 0, max_value = 100)

  expect_true(is.list(result))
  # Should ignore NAs: 1 violation out of 4 non-NA = 25%
  expect_equal(result$statistic, 25)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation works with default infinite bounds", {
  df <- tibble::tibble(
    values = c(-1000, -100, 0, 100, 1000)
  )

  # Default min_value = -Inf, max_value = Inf
  result <- quality_test_range_violation(df, "values")

  expect_true(is.list(result))
  expect_equal(result$statistic, 0)  # All finite values are within infinite range
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation works with only min_value specified", {
  df <- tibble::tibble(
    values = c(-10, 0, 5, 10, 15)
  )

  result <- quality_test_range_violation(df, "values", min_value = 0)

  expect_true(is.list(result))
  expect_equal(result$statistic, 1/5 * 100)  # Only -10 violates
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation works with only max_value specified", {
  df <- tibble::tibble(
    values = c(5, 10, 15, 20, 25)
  )

  result <- quality_test_range_violation(df, "values", max_value = 15)

  expect_true(is.list(result))
  expect_equal(result$statistic, 2/5 * 100)  # 20 and 25 violate
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation handles infinite values in data", {
  df <- tibble::tibble(
    values = c(5, 10, Inf, 15, 20)
  )

  expect_warning(
    result <- quality_test_range_violation(df, "values", min_value = 0, max_value = 100),
    "infinite values"
  )

  expect_true(is.list(result))
  # Should exclude Inf: 0 violations out of 4 finite values
  expect_equal(result$statistic, 0)
})

test_that("quality_test_range_violation handles negative infinite values in data", {
  df <- tibble::tibble(
    values = c(-Inf, 5, 10, 15, 20)
  )

  expect_warning(
    result <- quality_test_range_violation(df, "values", min_value = 0, max_value = 100),
    "infinite values"
  )

  expect_true(is.list(result))
  # Should exclude -Inf: 0 violations out of 4 finite values
  expect_equal(result$statistic, 0)
})

test_that("quality_test_range_violation validates input types correctly", {
  df <- tibble::tibble(
    numeric_var = c(1, 2, 3, 4, 5),
    character_var = c("a", "b", "c", "d", "e"),
    logical_var = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )

  # Should work with numeric
  result_numeric <- quality_test_range_violation(df, "numeric_var", min_value = 0, max_value = 10)
  expect_true(is.list(result_numeric))

  # Should warn with character
  expect_warning(
    result_char <- quality_test_range_violation(df, "character_var", min_value = 0, max_value = 10),
    "numeric data"
  )
  expect_true(is.na(result_char$statistic))

  # Should warn with logical
  expect_warning(
    result_logical <- quality_test_range_violation(df, "logical_var", min_value = 0, max_value = 10),
    "numeric data"
  )
  expect_true(is.na(result_logical$statistic))
})

test_that("quality_test_range_violation handles edge cases and errors", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5)
  )

  # Error case: Not a data frame
  expect_warning(
    quality_test_range_violation(list(values = c(1, 2, 3)), "values"),
    "data frame"
  )

  # Error case: Wrong number of variables
  expect_warning(
    quality_test_range_violation(df, c("values", "other")),
    "exactly 1 variable"
  )

  expect_warning(
    quality_test_range_violation(df, character(0)),
    "exactly 1 variable"
  )

  # Error case: NULL variables
  expect_warning(
    quality_test_range_violation(df, NULL),
    "exactly 1 variable"
  )

  # Error case: Variable not character
  expect_warning(
    quality_test_range_violation(df, 1),
    "character string"
  )

  # Error case: Variable not found
  expect_warning(
    result_missing <- quality_test_range_violation(df, "nonexistent"),
    "not found"
  )
  expect_true(is.na(result_missing$statistic))

  # Edge case: All NA values
  df_all_na <- tibble::tibble(
    values = c(NA, NA, NA, NA)
  )

  expect_warning(
    result_all_na <- quality_test_range_violation(df_all_na, "values"),
    "requires numeric data"
  )
  expect_true(is.na(result_all_na$statistic))

  # Edge case: Single value within range
  df_single <- tibble::tibble(
    values = c(5)
  )

  result_single <- quality_test_range_violation(df_single, "values", min_value = 0, max_value = 10)
  expect_equal(result_single$statistic, 0)

  # Edge case: Single value outside range
  result_single_out <- quality_test_range_violation(df_single, "values", min_value = 10, max_value = 20)
  expect_equal(result_single_out$statistic, 100)
})

test_that("quality_test_range_violation validates range parameters", {
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5)
  )

  # Error case: Non-numeric min_value
  expect_warning(
    quality_test_range_violation(df, "values", min_value = "0"),
    "single numeric value"
  )

  # Error case: Non-numeric max_value
  expect_warning(
    quality_test_range_violation(df, "values", max_value = "10"),
    "single numeric value"
  )

  # Error case: NA min_value
  expect_warning(
    quality_test_range_violation(df, "values", min_value = NA),
    "single numeric value"
  )

  # Error case: NA max_value
  expect_warning(
    quality_test_range_violation(df, "values", max_value = NA),
    "single numeric value"
  )

  # Error case: min_value > max_value
  expect_warning(
    quality_test_range_violation(df, "values", min_value = 10, max_value = 5),
    "less than or equal to"
  )

  # Error case: Multiple min_value values
  expect_warning(
    quality_test_range_violation(df, "values", min_value = c(0, 5)),
    "single numeric value"
  )

  # Error case: Multiple max_value values
  expect_warning(
    quality_test_range_violation(df, "values", max_value = c(10, 20)),
    "single numeric value"
  )
})

test_that("quality_test_range_violation handles boundary values correctly", {
  df <- tibble::tibble(
    values = c(0, 5, 10, 15, 20)
  )

  # Values exactly at boundaries should NOT violate
  result <- quality_test_range_violation(df, "values", min_value = 0, max_value = 20)

  expect_true(is.list(result))
  expect_equal(result$statistic, 0)  # Boundary values are included in valid range
})

test_that("quality_test_range_violation handles negative ranges", {
  df <- tibble::tibble(
    values = c(-20, -15, -10, -5, 0)
  )

  result <- quality_test_range_violation(df, "values", min_value = -15, max_value = -5)

  expect_true(is.list(result))
  expect_equal(result$statistic, 2/5 * 100)  # -20 and 0 violate
})

test_that("quality_test_range_violation handles empty data frame edge case", {
  df_empty <- tibble::tibble(
    values = numeric(0)
  )

  expect_warning(
    result <- quality_test_range_violation(df_empty, "values", min_value = 0, max_value = 10),
    "No non-missing values"
  )

  expect_true(is.na(result$statistic))
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation handles data with only infinite values", {
  df <- tibble::tibble(
    values = c(Inf, -Inf, Inf)
  )

  expect_warning(
    result <- quality_test_range_violation(df, "values", min_value = 0, max_value = 100),
    "No finite values found"
  )

  expect_true(is.na(result$statistic))
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation calculates exact percentages", {
  # Create data with known violations
  df <- tibble::tibble(
    values = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 100)  # 1 violation out of 10 = 10%
  )

  result <- quality_test_range_violation(df, "values", min_value = 0, max_value = 50)

  expect_true(is.list(result))
  expect_equal(result$statistic, 10)
  expect_true(is.na(result$p_value))
})

test_that("quality_test_range_violation works with very tight ranges", {
  df <- tibble::tibble(
    values = c(9.9, 10.0, 10.1, 10.2, 10.3)
  )

  result <- quality_test_range_violation(df, "values", min_value = 10.0, max_value = 10.2)

  expect_true(is.list(result))
  expect_equal(result$statistic, 2/5 * 100)  # 9.9 and 10.3 violate
})

# 7. INTEGRATION TESTS ####


test_that("quality tests work together in a pipeline", {
  df <- tibble::tibble(
    age = c(25, 30, 35, 40, 45, 50, 55, 60),
    income = c(30000, 35000, 40000, 45000, 50000, 55000, 60000, 65000),
    gender = rep(c("M", "F"), 4),
    employed = sample(c("yes", "no"), 8, replace = TRUE)
  )

  # Correlation test
  cor_result <- quality_test_correlation(df, c("age", "income"))
  expect_true(!is.na(cor_result$statistic))

  # T-test
  t_result <- quality_test_ttest(df, "age", mu = 40)
  expect_true(!is.na(t_result$statistic))

  # Chi-squared test
  chisq_result <- quality_test_chisq(df, c("gender", "employed"))
  expect_true(is.list(chisq_result))
})

test_that("quality tests return consistent structure", {
  df <- tibble::tibble(
    var1 = 1:10,
    var2 = 2:11,
    cat1 = rep(c("a", "b"), 5),
    cat2 = rep(c("x", "y"), 5)
  )

  cor_result <- quality_test_correlation(df, c("var1", "var2"))
  t_result <- quality_test_ttest(df, "var1")
  chisq_result <- quality_test_chisq(df, c("cat1", "cat2"))

  # All should return lists with statistic and p_value
  expect_true(all(c("statistic", "p_value") %in% names(cor_result)))
  expect_true(all(c("statistic", "p_value") %in% names(t_result)))
  expect_true(all(c("statistic", "p_value") %in% names(chisq_result)))
})


# 6. NUMERIC CONVERSION TESTS ####


test_that("quality tests handle character numeric input", {
  df <- tibble::tibble(
    var1 = c("1", "2", "3", "4", "5"),
    var2 = c("2", "4", "6", "8", "10")
  )

  result <- quality_test_correlation(df, c("var1", "var2"))

  expect_true(is.list(result))
  # Should convert to numeric and calculate correlation
})

test_that("quality tests handle factor input", {
  df <- tibble::tibble(
    var1 = factor(c("a", "b", "a", "b", "a")),
    var2 = factor(c("x", "y", "x", "y", "x"))
  )

  result <- quality_test_chisq(df, c("var1", "var2"))

  expect_true(is.list(result))
})

