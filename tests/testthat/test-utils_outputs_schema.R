library(testthat)

# ============================================================================
# Helper: a minimal valid outputs schema data frame
# ============================================================================

valid_outputs_df <- function(...) {
  base <- data.frame(
    output_title     = "Age Pyramid",
    output_name      = "age_pyramid",
    output_subtitle  = "By sex",
    variables        = "age_years, sex",
    disaggregation   = "enum_name",
    output_func_name = "plot_age_pyramid",
    test_params      = "weights_col=weight, flip=TRUE",
    output_type      = "visualization",
    dataset_type     = "base",
    stringsAsFactors = FALSE
  )

  overrides <- list(...)
  for (nm in names(overrides)) base[[nm]] <- overrides[[nm]]
  base
}


# ============================================================================
# OUTPUTS_PARSE_LIST_FIELD ####
# ============================================================================

test_that("outputs_parse_list_field splits a comma-separated cell", {
  expect_equal(
    outputs_parse_list_field("age_years, sex"),
    c("age_years", "sex")
  )
})

test_that("outputs_parse_list_field returns NULL for empty input", {
  expect_null(outputs_parse_list_field(NULL))
  expect_null(outputs_parse_list_field(NA))
  expect_null(outputs_parse_list_field(NA_character_))
  expect_null(outputs_parse_list_field(""))
  expect_null(outputs_parse_list_field("   "))
})

test_that("outputs_parse_list_field handles a single value", {
  expect_equal(outputs_parse_list_field("age_years"), "age_years")
})

test_that("outputs_parse_list_field keeps commas inside parentheses", {
  expect_equal(
    outputs_parse_list_field("c(0.4, 0.6), sex"),
    c("c(0.4, 0.6)", "sex")
  )
})

test_that("outputs_parse_list_field drops empty entries", {
  expect_equal(outputs_parse_list_field("age_years, , sex"), c("age_years", "sex"))
})


# ============================================================================
# OUTPUTS_PARSE_TEST_PARAMS ####
# ============================================================================

test_that("outputs_parse_test_params splits key=value pairs", {
  out <- outputs_parse_test_params("weights_col=weight, title=Age")

  expect_true(is.list(out))
  expect_equal(out$weights_col, "weight")
  expect_equal(out$title, "Age")
})

test_that("outputs_parse_test_params coerces numerics and logicals", {
  out <- outputs_parse_test_params("threshold=0.25, flip=TRUE, keep=false")

  expect_equal(out$threshold, 0.25)
  expect_true(is.numeric(out$threshold))
  expect_identical(out$flip, TRUE)
  expect_identical(out$keep, FALSE)
})

test_that("outputs_parse_test_params preserves c(...) vectors as strings", {
  out <- outputs_parse_test_params("expected_ratio=c(0.4118, 0.5882)")

  expect_equal(out$expected_ratio, "c(0.4118, 0.5882)")
})

test_that("outputs_parse_test_params preserves @variable_map references", {
  out <- outputs_parse_test_params("weights_col=@variable_map$weight")

  expect_equal(out$weights_col, "@variable_map$weight")
})

test_that("outputs_parse_test_params returns NULL for empty input", {
  expect_null(outputs_parse_test_params(NULL))
  expect_null(outputs_parse_test_params(NA_character_))
  expect_null(outputs_parse_test_params(""))
})

test_that("outputs_parse_test_params ignores entries without an '=' sign", {
  out <- outputs_parse_test_params("weights_col=weight, junk")

  expect_equal(names(out), "weights_col")
})


# ============================================================================
# OUTPUTS_VALIDATE_SCHEMA ####
# ============================================================================

test_that("outputs_validate_schema accepts a valid outputs schema", {
  expect_true(outputs_validate_schema(valid_outputs_df()))
})

test_that("outputs_validate_schema errors when required columns are missing", {
  df <- valid_outputs_df()
  df$output_func_name <- NULL

  expect_error(outputs_validate_schema(df))
})

test_that("outputs_validate_schema errors when input is not a data frame", {
  expect_error(outputs_validate_schema("not a df"))
  expect_error(outputs_validate_schema(list()))
})

test_that("outputs_validate_schema errors on invalid output_type", {
  df <- valid_outputs_df(output_type = "chart")

  expect_error(
    outputs_validate_schema(df),
    regexp = "output_type|visualization"
  )
})

test_that("outputs_validate_schema errors on invalid dataset_type", {
  df <- valid_outputs_df(dataset_type = "data")

  expect_error(
    outputs_validate_schema(df),
    regexp = "dataset_type|survey_design"
  )
})

test_that("outputs_validate_schema accepts both supported dataset_type values", {
  expect_true(outputs_validate_schema(valid_outputs_df(dataset_type = "base")))
  expect_true(
    outputs_validate_schema(valid_outputs_df(dataset_type = "survey_design"))
  )
})

test_that("outputs_validate_schema errors on a missing output_title", {
  df <- valid_outputs_df(output_title = NA_character_)

  expect_error(outputs_validate_schema(df), regexp = "output_title")
})

test_that("outputs_validate_schema does not warn on duplicate output_title alone", {
  df <- rbind(
    valid_outputs_df(output_name = "age_pyramid_a"),
    valid_outputs_df(output_name = "age_pyramid_b")
  )

  expect_true(outputs_validate_schema(df))
})

test_that("outputs_validate_schema warns on a duplicate storage key", {
  df <- rbind(valid_outputs_df(), valid_outputs_df())

  expect_warning(outputs_validate_schema(df), regexp = "Duplicate output_name")
})

test_that("outputs_validate_schema does not warn when outputs_group differs", {
  df <- rbind(
    valid_outputs_df(outputs_group = "overall"),
    valid_outputs_df(outputs_group = "strata")
  )

  expect_true(outputs_validate_schema(df))
})


# ============================================================================
# OUTPUTS_NORMALIZE_SCHEMA ####
# ============================================================================

test_that("outputs_normalize_schema returns a tibble", {
  out <- outputs_normalize_schema(valid_outputs_df())

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
})

test_that("outputs_normalize_schema adds missing optional columns as NA", {
  out <- outputs_normalize_schema(valid_outputs_df())

  for (col in c(
    "outputs_group", "outputs_per_group",
    "output_title_english", "output_title_french", "output_title_arabic"
  )) {
    expect_true(col %in% names(out))
    expect_true(is.na(out[[col]][[1]]))
  }
})

test_that("outputs_normalize_schema coerces known columns to character", {
  df <- valid_outputs_df()
  df$outputs_per_group <- 3

  out <- outputs_normalize_schema(df)

  expect_type(out$outputs_per_group, "character")
  expect_equal(out$outputs_per_group[[1]], "3")
})

test_that("outputs_normalize_schema preserves row values", {
  out <- outputs_normalize_schema(valid_outputs_df())

  expect_equal(out$output_title[[1]], "Age Pyramid")
  expect_equal(out$output_name[[1]], "age_pyramid")
  expect_equal(out$output_func_name[[1]], "plot_age_pyramid")
  expect_equal(out$dataset_type[[1]], "base")
})

test_that("outputs_normalize_schema keeps multiple rows with the same output_name", {
  df <- rbind(
    valid_outputs_df(),
    valid_outputs_df(output_title = "Age Pyramid 2")
  )

  out <- outputs_normalize_schema(df)

  expect_equal(nrow(out), 2)
})

test_that("outputs_normalize_schema rejects an invalid schema", {
  expect_error(outputs_normalize_schema(valid_outputs_df(output_type = "chart")))
})

test_that("outputs_normalize_schema round-trips through parse helpers", {
  out <- outputs_normalize_schema(valid_outputs_df())

  expect_equal(
    outputs_parse_list_field(out$variables[[1]]),
    c("age_years", "sex")
  )
  expect_equal(
    outputs_parse_test_params(out$test_params[[1]])$flip,
    TRUE
  )
})
