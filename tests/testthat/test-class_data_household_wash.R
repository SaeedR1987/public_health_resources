library(testthat)
library(tibble)

# Source generate_household_samples.R from dev directory
source(file.path(here::here(), "dev", "generate_household_samples.R"))

# ==============================================================================
# WASHHouseholdData Tests
# ==============================================================================

# Initialization Tests ####

test_that("WASHHouseholdData initializes with minimal valid data", {

  df <- tibble::tibble(
    uuid = "1",
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1"
  )

  wash <- WASHHouseholdData$new(data = df)

  expect_s3_class(wash, "WASHHouseholdData")
  expect_s3_class(wash, "HouseholdData")
  expect_s3_class(wash, "Data")
  expect_equal(wash$dataset_name, "WASHHouseholdData")
})

test_that("WASHHouseholdData initializes with WASH-specific columns", {

  df <- generate_household_dataset(n = 10)

  wash <- WASHHouseholdData$new(data = df)

  expect_true("wash_water_source_primary" %in% names(wash$variable_map))
  expect_true("wash_sanitation_type" %in% names(wash$variable_map))
  expect_true("wash_handwashing_facility" %in% names(wash$variable_map))
})

test_that("WASHHouseholdData accepts custom variable_map", {

  df <- tibble::tibble(
    uuid = "1",
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1",
    water_src = "borehole",
    sanit_type = "pit_slab"
  )

  wash <- WASHHouseholdData$new(
    data = df,
    variable_map = list(
      wash_water_source_primary = "water_src",
      wash_sanitation_type = "sanit_type"
    )
  )

  expect_equal(wash$variable_map$wash_water_source_primary, "water_src")
  expect_equal(wash$variable_map$wash_sanitation_type, "sanit_type")
})

test_that("WASHHouseholdData has WASH-specific schema attached", {

  df <- generate_household_dataset(n = 5)
  wash <- WASHHouseholdData$new(data = df)

  schema <- wash$schema

  expect_true(is.list(schema))
  expect_true("wash_water_source_primary" %in% names(schema$types))
  expect_true("wash_sanitation_type" %in% names(schema$types))
  expect_true("wash_water_source_primary" %in% names(schema$allowed_values))
})


# Tests for add_liters_per_person_per_day ####

# Define the test dataset for usual cases
example_data <- data.frame(
  total_liters = c(100, 200, 300),
  household_size = c(4, 5, 6),
  num_days = c(1, 2, 3)
)

test_that("Usual cases: Calculations are correct", {
  result <- add_liters_per_person_per_day(
    .dataset = example_data,
    total_liters_col = "total_liters",
    household_size_col = "household_size",
    num_days_col = "num_days"
  )

  # Check the calculated columns
  expect_true("liters_pppd" %in% colnames(result))
  expect_true("liters_z_score" %in% colnames(result))
  expect_true("liters_pppd_z_score" %in% colnames(result))
  expect_true("liters_log" %in% colnames(result))
  expect_true("wash_lppd_cat" %in% colnames(result))

  # Check correctness of liters_pppd
  expect_equal(result$liters_pppd[1], 100 / (4 * 1))
  expect_equal(result$liters_pppd[2], 200 / (5 * 2))
  expect_equal(result$liters_pppd[3], 300 / (6 * 3))
})

test_that("Edge case: when num_days is NULL, uses default value of 1", {
  example_data_no_days <- example_data %>%
    select(-num_days)  # Remove the num_days column

  result <- add_liters_per_person_per_day(
    .dataset = example_data_no_days,
    total_liters_col = "total_liters",
    household_size_col = "household_size"
  )

  # Check if the temporary 'num_days' default has been handled correctly
  expect_true(all(result$liters_pppd == example_data$total_liters / example_data$household_size))
})

test_that("Edge case: empty dataset throws an error", {
  empty_data <- data.frame()

  expect_error(
    add_liters_per_person_per_day(
      .dataset = empty_data,
      total_liters_col = "total_liters",
      household_size_col = "household_size",
      num_days_col = "num_days"
    ),
    "Dataset is empty."
  )
})

test_that("Error handling: Missing columns produce an error", {
  missing_data <- example_data %>% select(-total_liters)  # Remove total_liters column

  expect_error(
    add_liters_per_person_per_day(
      .dataset = missing_data,
      total_liters_col = "total_liters",
      household_size_col = "household_size",
      num_days_col = "num_days"
    ),
    "Ensure all required columns are present in the dataset."
  )
})

test_that("Edge case: Negative values in columns throw an error", {
  invalid_data <- example_data
  invalid_data$total_liters[1] <- -100  # Set a negative value

  expect_error(
    add_liters_per_person_per_day(
      .dataset = invalid_data,
      total_liters_col = "total_liters",
      household_size_col = "household_size",
      num_days_col = "num_days"
    ),
    "Column `total_liters` must be numeric and contain non-negative values."
  )
})

test_that("Edge case: Non-numeric columns throw an error", {
  invalid_data <- example_data
  invalid_data$total_liters <- as.character(invalid_data$total_liters)  # Convert to character

  expect_error(
    add_liters_per_person_per_day(
      .dataset = invalid_data,
      total_liters_col = "total_liters",
      household_size_col = "household_size",
      num_days_col = "num_days"
    ),
    "Column `total_liters` must be numeric and contain non-negative values."
  )
})

test_that("Edge case: Large numbers and zeros are handled correctly", {
  edge_data <- data.frame(
    total_liters = c(0, 1e10, 1e5),
    household_size = c(1, 100, 0),
    num_days = c(1, 1, 1)
  )

  result <- suppressWarnings(add_liters_per_person_per_day(
    .dataset = edge_data,
    total_liters_col = "total_liters",
    household_size_col = "household_size",
    num_days_col = "num_days"
  ))

  # Check handling of zeros in household_size (expect Inf or warning)
  expect_type(result$liters_pppd[3], "double") # Not crash but handle per-person
})

# JMP Water Ladder Tests ####

test_that("calculate_jmp_water_ladder calculates basic service level correctly", {

  df <- tibble::tibble(
    uuid = c("1", "2", "3"),
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1",
    wash_water_source_primary = c("borehole", "unprotected_well", "surface_water"),
    wash_water_collection_time_minutes = c(15, 45, 30)
  )

  wash <- HouseholdData$new(data = df)
  wash$validate("raw")
  wash$standardize()

  result <- wash$standardized_data$wash_jmp_water_ladder

  expect_equal(result[1], "basic")      # Improved source, < 30 min
  expect_equal(result[2], "unimproved") # Unprotected well
  expect_equal(result[3], "surface_water") # Surface water
})

test_that("calculate_jmp_water_ladder handles limited service (> 30 min)", {

  df <- tibble::tibble(
    uuid = "1",
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1",
    wash_water_source_primary = "borehole",
    wash_water_collection_time_minutes = 60  # Over 30 minutes
  )

  wash <- WASHHouseholdData$new(data = df)
  wash$validate("raw")
  wash$standardize()

  wash$calculate_jmp_water_ladder("standardized")

  expect_equal(wash$standardized_data$wash_jmp_water_ladder, "limited")
})

# JMP Sanitation Ladder Tests ####

test_that("calculate_jmp_sanitation_ladder classifies correctly", {

  df <- tibble::tibble(
    uuid = c("1", "2", "3", "4"),
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1",
    wash_sanitation_type = c("flush_sewer", "pit_slab", "pit_no_slab", "open_defecation"),
    wash_sanitation_shared = c("no", "yes", "no", "no")
  )

  wash <- WASHHouseholdData$new(data = df)
  wash$validate("raw")
  wash$standardize()

  wash$calculate_jmp_sanitation_ladder("standardized")

  result <- wash$standardized_data$wash_jmp_sanitation_ladder

  expect_equal(result[1], "basic")           # Improved, not shared
  expect_equal(result[2], "limited")         # Improved but shared
  expect_equal(result[3], "unimproved")      # Unimproved facility
  expect_equal(result[4], "open_defecation") # OD
})

# JMP Hygiene Ladder Tests ####

test_that("calculate_jmp_hygiene_ladder classifies correctly", {

  df <- tibble::tibble(
    uuid = c("1", "2", "3"),
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1",
    wash_handwashing_facility = c("fixed_observed", "fixed_observed", "none"),
    wash_handwashing_water = c("yes", "yes", NA),
    wash_handwashing_soap = c("yes", "no", NA)
  )

  wash <- WASHHouseholdData$new(data = df)
  wash$validate("raw")
  wash$standardize()

  wash$calculate_jmp_hygiene_ladder("standardized")

  result <- wash$standardized_data$wash_jmp_hygiene_ladder

  expect_equal(result[1], "basic")       # Facility with water AND soap
  expect_equal(result[2], "limited")     # Facility but missing soap
  expect_equal(result[3], "no_facility") # No facility
})

# Calculate All JMP Ladders ####

test_that("calculate_all_jmp_ladders calculates all three ladders", {

  df <- generate_household_dataset(n = 20)

  wash <- WASHHouseholdData$new(data = df)
  wash$validate("raw")
  wash$standardize()

  wash$calculate_all_jmp_ladders("standardized")

  std_data <- wash$standardized_data

  expect_true("wash_jmp_water_ladder" %in% names(std_data))
  expect_true("wash_jmp_sanitation_ladder" %in% names(std_data))
  expect_true("wash_jmp_hygiene_ladder" %in% names(std_data))
})

# Summarize WASH Indicators ####

test_that("summarize_wash_indicators returns expected structure", {

  df <- generate_household_dataset(n = 30)

  wash <- WASHHouseholdData$new(data = df)
  wash$validate("raw")
  wash$standardize()
  wash$calculate_all_jmp_ladders("standardized")

  summary <- wash$summarize_wash_indicators("standardized")

  expect_true(is.list(summary))
  expect_true("water_source" %in% names(summary))
  expect_true("sanitation_type" %in% names(summary))
})

# Post-validation Tests ####

test_that("post_validate warns on out-of-range water collection time", {

  df <- tibble::tibble(
    uuid = "1",
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1",
    wash_water_collection_time_minutes = 2000  # Out of range (max 1440)
  )

  wash <- WASHHouseholdData$new(data = df)

  expect_warning(
    wash$validate("raw"),
    regexp = "outside range"
  )
})

test_that("post_validate warns on invalid water source values", {

  df <- tibble::tibble(
    uuid = "1",
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1",
    wash_water_source_primary = "invalid_source"
  )

  wash <- WASHHouseholdData$new(data = df)

  expect_warning(
    wash$validate("raw"),
    regexp = "invalid values"
  )
})

# Integration Tests ####

test_that("WASHHouseholdData completes full pipeline", {

  df <- generate_household_dataset(n = 50)

  wash <- WASHHouseholdData$new(data = df)

  # Validation
  expect_no_error(wash$validate("raw"))

  # Standardization
  expect_no_error(wash$standardize())
  expect_true(wash$standardized)

  # JMP calculations
  expect_no_error(wash$calculate_all_jmp_ladders("standardized"))

  # Cleaning
  expect_no_error(wash$clean())
  expect_true(wash$cleaned)

  # Summary
  summary <- wash$summarize_wash_indicators("clean")
  expect_true(is.list(summary))
})

test_that("WASHHouseholdData can link to parent HouseholdData", {

  hh_df <- generate_household_dataset(n = 10)
  wash_df <- generate_household_dataset(n = 10)

  # Ensure matching UUIDs
  wash_df$uuid <- hh_df$uuid

  hh <- HouseholdData$new(data = hh_df)
  wash <- WASHHouseholdData$new(data = wash_df)

  # Link WASH to HH
  wash$add_linked_dataset("household", hh, by_self_role = "uuid", by_other_role = "uuid")

  expect_true("household" %in% names(wash$linked_objects))
})
