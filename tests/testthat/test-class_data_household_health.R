library(testthat)
library(tibble)

# Source generate_household_samples.R from dev directory
source(file.path(here::here(), "dev", "generate_household_samples.R"))

# ==============================================================================
# HealthHouseholdData Tests
# ==============================================================================

# Initialization Tests ####

test_that("HealthHouseholdData initializes with minimal valid data", {

  df <- tibble::tibble(
    uuid = "1",
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1"
  )

  health <- HealthHouseholdData$new(data = df)

  expect_s3_class(health, "HealthHouseholdData")
  expect_s3_class(health, "HouseholdData")
  expect_s3_class(health, "Data")
  expect_equal(health$dataset_name, "HealthHouseholdData")
})

test_that("HealthHouseholdData initializes with health-specific columns", {

  df <- generate_household_dataset(n = 10)

  health <- HealthHouseholdData$new(data = df)

  expect_true("health_facility_distance_km" %in% names(health$variable_map))
  expect_true("health_facility_access_barrier" %in% names(health$variable_map))
  expect_true("health_insurance_coverage" %in% names(health$variable_map))
})

test_that("HealthHouseholdData accepts custom variable_map", {

  df <- tibble::tibble(
    uuid = "1",
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1",
    distance = 5.2,
    barrier = "cost"
  )

  health <- HealthHouseholdData$new(
    data = df,
    variable_map = list(
      health_facility_distance_km = "distance",
      health_facility_access_barrier = "barrier"
    )
  )

  expect_equal(health$variable_map$health_facility_distance_km, "distance")
  expect_equal(health$variable_map$health_facility_access_barrier, "barrier")
})

test_that("HealthHouseholdData has health-specific schema attached", {

  df <- generate_household_dataset(n = 5)
  health <- HealthHouseholdData$new(data = df)

  schema <- health$schema

  expect_true(is.list(schema))
  expect_true("health_facility_distance_km" %in% names(schema$types))
  expect_true("health_insurance_type" %in% names(schema$allowed_values))
})

# Health Access Score Tests ####

test_that("calculate_health_access_score computes scores correctly", {

  df <- tibble::tibble(
    uuid = c("1", "2", "3"),
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1",
    health_facility_distance_km = c(1, 15, 60),
    health_facility_time_minutes = c(10, 60, 300),
    health_facility_access_barrier = c("none", "cost", "security")
  )

  health <- HealthHouseholdData$new(data = df)
  health$validate("raw")
  health$standardize()

  health$calculate_health_access_score("standardized")

  scores <- health$standardized_data$health_access_score

  # First household should have highest score (close, quick, no barrier)
  expect_true(scores[1] > scores[2])
  expect_true(scores[2] > scores[3])
})

test_that("calculate_health_access_score handles missing components", {

  df <- tibble::tibble(
    uuid = "1",
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1",
    health_facility_distance_km = 5  # Only distance, no time or barrier
  )

  health <- HealthHouseholdData$new(data = df)
  health$validate("raw")
  health$standardize()

  expect_no_error(health$calculate_health_access_score("standardized"))
  expect_true(!is.na(health$standardized_data$health_access_score))
})

# Barrier Summary Tests ####

test_that("summarize_barriers returns correct distribution", {

  df <- tibble::tibble(
    uuid = c("1", "2", "3", "4", "5"),
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1",
    health_facility_access_barrier = c("none", "cost", "cost", "distance", "cost")
  )

  health <- HealthHouseholdData$new(data = df)
  health$validate("raw")
  health$standardize()

  summary <- health$summarize_barriers("standardized")

  expect_s3_class(summary, "data.frame")
  expect_true("barrier" %in% names(summary))
  expect_true("count" %in% names(summary))
  expect_true("proportion" %in% names(summary))

  # Cost should be most common
  expect_equal(summary$barrier[1], "cost")
  expect_equal(summary$count[1], 3)
})

test_that("summarize_barriers handles missing barrier column", {

  df <- tibble::tibble(
    uuid = "1",
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1"
  )

  health <- HealthHouseholdData$new(data = df)
  health$validate("raw")
  health$standardize()

  expect_warning(
    result <- health$summarize_barriers("standardized"),
    regexp = "not found"
  )
  expect_null(result)
})

# Maternal Health Summary Tests ####

test_that("summarize_maternal_health calculates facility delivery rate", {

  df <- tibble::tibble(
    uuid = c("1", "2", "3", "4"),
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1",
    health_maternal_anc_access = c("yes", "yes", "no", "not_applicable"),
    health_anc_visits = c(4, 6, 2, NA),
    health_delivery_location_last = c("hospital", "home", "health_center", "home"),
    health_delivery_attended = c("doctor", "traditional_birth_attendant", "nurse_midwife", "relative")
  )

  health <- HealthHouseholdData$new(data = df)
  health$validate("raw")
  health$standardize()

  summary <- health$summarize_maternal_health("standardized")

  expect_true(is.list(summary))
  expect_true("facility_delivery_rate" %in% names(summary))
  expect_true("skilled_birth_attendant_rate" %in% names(summary))

  # 2 facility deliveries out of 4
  expect_equal(summary$facility_delivery_rate, 50)
})

test_that("summarize_maternal_health calculates ANC 4+ percentage", {

  df <- tibble::tibble(
    uuid = c("1", "2", "3", "4"),
    consent = "yes",
    interview_date = Sys.Date(),
    enumerator_id = "E1",
    health_anc_visits = c(1, 4, 6, 3)
  )

  health <- HealthHouseholdData$new(data = df)
  health$validate("raw")
  health$standardize()

  summary <- health$summarize_maternal_health("standardized")

  # 2 out of 4 have 4+ visits = 50%
  expect_equal(summary$anc_visits$pct_4_plus, 50)
})

# Summarize All Health Indicators ####

test_that("summarize_health_indicators returns comprehensive structure", {

  df <- generate_household_dataset(n = 30)

  health <- HealthHouseholdData$new(data = df)
  health$validate("raw")
  health$standardize()
  health$calculate_health_access_score("standardized")

  summary <- health$summarize_health_indicators("standardized")

  expect_true(is.list(summary))
  expect_true("health_care_use" %in% names(summary))
  expect_true("insurance_coverage" %in% names(summary))
  expect_true("barriers" %in% names(summary))
  expect_true("maternal_health" %in% names(summary))
})

# Post-validation Tests ####



# Integration Tests ####

test_that("HealthHouseholdData completes full pipeline", {

  df <- generate_household_dataset(n = 50)

  health <- HealthHouseholdData$new(data = df)

  # Validation
  expect_no_error(health$validate("raw"))

  # Standardization
  expect_no_error(health$standardize())
  expect_true(health$standardized)

  # Access score calculation
  expect_no_error(health$calculate_health_access_score("standardized"))

  # Cleaning
  expect_no_error(health$clean())
  expect_true(health$cleaned)

  # Summary
  summary <- health$summarize_health_indicators("clean")
  expect_true(is.list(summary))
})

test_that("HealthHouseholdData can link to IndividualData", {

  hh_df <- generate_household_dataset(n = 10)
  ind_df <- generate_hh_roster_dataset(n = 30)

  health_hh <- HealthHouseholdData$new(data = hh_df)
  ind <- IndividualData$new(data = ind_df, uuid = "uuid")

  # Set up the link
  ind$set_variable("hh_uuid", "hh_uuid")
  health_hh$set_variable("uuid", "uuid")

  # Link individuals to households
  health_hh$add_linked_dataset("individuals", ind, by_self_role = "uuid", by_other_role = "hh_uuid")

  expect_true("individuals" %in% names(health_hh$linked_objects))
})
