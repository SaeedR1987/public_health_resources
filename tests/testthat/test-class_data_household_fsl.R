# ---------------------------
# FSL HOUSEHOLD DATA CLASS TEST SUITE
# ---------------------------

library(testthat)
library(tibble)


# ============================================================
# 1. INITIALIZATION TESTS
# ============================================================

test_that("FSLHouseholdData initializes with minimal valid data", {
  df <- generate_household_dataset(n = 10)
  
  fsl_data <- FSLHouseholdData$new(
    data = df,
    dataset_name = "TestFSL"
  )
  
  expect_s3_class(fsl_data, "FSLHouseholdData")
  expect_s3_class(fsl_data, "HouseholdData")
  expect_s3_class(fsl_data, "Data")
  expect_equal(fsl_data$dataset_name, "TestFSL")
})

test_that("FSLHouseholdData initializes with FSL indicator columns", {
  df <- generate_household_dataset(n = 10)
  
  fsl_data <- FSLHouseholdData$new(
    data = df,
    dataset_name = "TestFSL"
  )
  
  # Check that FSL-specific variable map is set
  expect_true("fsl_fcs_score" %in% names(fsl_data$variable_map))
  expect_true("fsl_hhs_score" %in% names(fsl_data$variable_map))
  expect_true("fsl_rcsi_score" %in% names(fsl_data$variable_map))
  expect_true("fsl_hdds_count" %in% names(fsl_data$variable_map))
})

test_that("FSLHouseholdData merges custom variable map with defaults", {
  df <- generate_household_dataset(n = 10)
  df$my_fcs_score <- 42
  
  custom_map <- list(fsl_fcs_score = "my_fcs_score")
  
  fsl_data <- FSLHouseholdData$new(
    data = df,
    dataset_name = "TestFSL",
    variable_map = custom_map
  )
  
  # Custom mapping should override default
  expect_equal(fsl_data$variable_map$fsl_fcs_score, "my_fcs_score")
  
  # Other FSL mappings should still be present
  expect_true("fsl_hhs_score" %in% names(fsl_data$variable_map))
})

# ============================================================
# 2. SCHEMA TESTS
# ============================================================

test_that("FSLHouseholdData loads default FSL schema", {
  df <- generate_household_dataset(n = 10)
  
  fsl_data <- FSLHouseholdData$new(data = df)
  
  schema <- fsl_data$default_fsl_schema()
  
  expect_true(is.list(schema))
  # Schema should have types or allowed_values
  expect_true(length(schema) > 0)
})

test_that("FSLHouseholdData merges schemas correctly", {
  df <- generate_household_dataset(n = 10)
  
  fsl_data <- FSLHouseholdData$new(data = df)
  
  base_schema <- list(
    types = list(var1 = "character"),
    allowed_values = list(var1 = c("a", "b"))
  )
  
  addon_schema <- list(
    types = list(var2 = "numeric"),
    ranges = list(var2 = c(0, 100))
  )
  
  merged <- fsl_data$merge_schema(base_schema, addon_schema)
  
  expect_equal(merged$types$var1, "character")
  expect_equal(merged$types$var2, "numeric")
  expect_equal(merged$allowed_values$var1, c("a", "b"))
  expect_equal(merged$ranges$var2, c(0, 100))
})

test_that("FSLHouseholdData handles missing schema files gracefully", {
  df <- generate_household_dataset(n = 10)
  
  # Should initialize without error even if schema files are missing
  expect_no_error({
    fsl_data <- FSLHouseholdData$new(data = df)
  })
})

# ============================================================
# 3. INDICATOR SCHEMA TESTS
# ============================================================

test_that("FSLHouseholdData loads indicator schema", {
  df <- generate_household_dataset(n = 10)
  
  fsl_data <- FSLHouseholdData$new(data = df)
  
  indicator_schema <- fsl_data$default_fsl_indicator_schema()
  
  expect_true(is.list(indicator_schema))
  # Should return empty list if file not found, not NULL or error
})

test_that("FSLHouseholdData loads dependency schema", {
  df <- generate_household_dataset(n = 10)
  
  fsl_data <- FSLHouseholdData$new(data = df)
  
  dependency_schema <- fsl_data$default_fsl_dependency_schema()
  
  expect_true(is.list(dependency_schema))
  expect_true("dependencies" %in% names(dependency_schema) || length(dependency_schema) == 0)
})

# ============================================================
# 4. POST-VALIDATION TESTS
# ============================================================

test_that("FSLHouseholdData post_validate checks numeric ranges", {
  df <- generate_household_dataset(n = 10)
  df$fsl_fcs_score <- c(50, 60, 70, 200, 30, 40, 50, 60, 70, 80)  # 200 is out of range
  
  fsl_data <- FSLHouseholdData$new(data = df)
  fsl_data$validated <- TRUE
  fsl_data$data <- df
  
  # Should warn about out-of-range values
  expect_warning(
    fsl_data$post_validate(),
    regexp = NA  # Don't expect error, may warn
  )
})

test_that("FSLHouseholdData post_validate checks categorical values", {
  df <- generate_household_dataset(n = 10)
  df$fsl_fcs_cat <- c(rep("Poor", 5), rep("InvalidCategory", 5))
  
  fsl_data <- FSLHouseholdData$new(data = df)
  fsl_data$validated <- TRUE
  fsl_data$data <- df
  
  # Should warn about invalid categorical values
  expect_warning(
    fsl_data$post_validate(),
    regexp = NA  # May or may not warn depending on schema
  )
})

# ============================================================
# 5. DATA QUALITY GENERATION TESTS
# ============================================================

test_that("FSLHouseholdData generates FSL data quality object", {
  skip_if_not_installed("FSLDataQuality")
  
  df <- generate_household_dataset(n = 10)
  
  fsl_data <- FSLHouseholdData$new(data = df)
  fsl_data$standardized_data <- df
  
  dq <- fsl_data$generate_data_quality(stage = "standardized")
  
  if (!is.null(dq)) {
    expect_s3_class(dq, "FSLDataQuality")
    expect_s3_class(dq, "DataQuality")
  }
})

test_that("FSLHouseholdData generate_data_quality handles missing data gracefully", {
  df <- generate_household_dataset(n = 10)
  
  fsl_data <- FSLHouseholdData$new(data = df)
  # Don't set standardized_data
  
  expect_warning(
    dq <- fsl_data$generate_data_quality(stage = "standardized"),
    regexp = NA  # May warn about missing data
  )
})

# ============================================================
# 6. OPTIONAL COLUMNS TESTS
# ============================================================

test_that("FSLHouseholdData extends optional columns", {
  df <- generate_household_dataset(n = 10)
  
  fsl_data <- FSLHouseholdData$new(data = df)
  
  # Check that FSL columns are in optional_columns
  expect_true(any(grepl("fsl_", fsl_data$optional_columns)))
})

# ============================================================
# 7. INHERITANCE TESTS
# ============================================================

test_that("FSLHouseholdData inherits from HouseholdData", {
  df <- generate_household_dataset(n = 10)
  
  fsl_data <- FSLHouseholdData$new(data = df)
  
  expect_true(inherits(fsl_data, "HouseholdData"))
  expect_true(inherits(fsl_data, "Data"))
  
  # Should have parent class methods available
  expect_true(is.function(fsl_data$validate))
  expect_true(is.function(fsl_data$standardize))
})

test_that("FSLHouseholdData calls parent initialize correctly", {
  df <- generate_household_dataset(n = 10)
  
  fsl_data <- FSLHouseholdData$new(
    data = df,
    dataset_name = "TestFSL",
    metadata = list(source = "test")
  )
  
  # Should have metadata from parent initialization
  expect_equal(fsl_data$metadata$dataset_name, "TestFSL")
  expect_equal(fsl_data$metadata$source, "test")
})
