# Tests for SurveyProtocol$calculate_sample_sizes() field plan integration.
# Covers the estimate_field_plan() call added to the calculate_sample_sizes
# workflow.

# ---- helpers -----------------------------------------------------------------

make_protocol <- function() {
  create_survey_protocol(
    assessment_title = "Test Assessment",
    country_name     = "Testland",
    month_year       = "January 2024"
  )
}

# ---- add_stratum initialises field-plan columns as NA -------------------------

test_that("add_stratum initialises field-plan columns as NA", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Stratum 1",
    population_size         = 10000,
    pop_expected_prevalence = 50,
    pop_precision           = 5
  )
  st <- p$get_sample_table()
  expect_true("num_interview_per_enum_per_day" %in% names(st))
  expect_true("num_days"                       %in% names(st))
  expect_true("num_psu_needed"                 %in% names(st))
  expect_true("psu_size"                       %in% names(st))
  expect_true(is.na(st$num_interview_per_enum_per_day))
  expect_true(is.na(st$num_days))
  expect_true(is.na(st$num_psu_needed))
  expect_true(is.na(st$psu_size))
})

# ---- field-plan columns remain NA when logistics params absent ----------------

test_that("calculate_sample_sizes leaves field-plan columns NA when logistics params are absent", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Stratum 1",
    population_size         = 10000,
    pop_expected_prevalence = 50,
    pop_precision           = 5
  )
  p$calculate_sample_sizes()
  st <- p$get_sample_table()
  expect_true(is.na(st$num_interview_per_enum_per_day))
  expect_true(is.na(st$num_days))
  expect_true(is.na(st$num_psu_needed))
  expect_true(is.na(st$psu_size))
})

# ---- simple_random field plan populates columns ------------------------------

test_that("calculate_sample_sizes populates field-plan columns for simple_random stratum", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Urban",
    population_size         = 10000,
    sampling_method         = "srs",
    pop_expected_prevalence = 50,
    pop_precision           = 5,
    teams                   = 2,
    enumerators_per_team    = 3,
    avg_interview_time      = 45,
    avg_travel_time         = 30,
    avg_rest_time           = 60,
    start_time              = "2024-01-01",
    end_time                = "2024-01-31"
  )
  p$calculate_sample_sizes()
  st <- p$get_sample_table()

  expect_false(is.na(st$num_interview_per_enum_per_day))
  expect_false(is.na(st$num_days))
  expect_true(is.na(st$num_psu_needed))
  expect_true(is.na(st$psu_size))
  expect_true(st$num_interview_per_enum_per_day > 0)
  expect_true(st$num_days > 0)
})

# ---- cluster field plan populates all four columns ---------------------------

test_that("calculate_sample_sizes populates all field-plan columns for cluster stratum", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Rural",
    population_size         = 20000,
    sampling_method         = "pps_cluster",
    pop_expected_prevalence = 50,
    pop_precision           = 5,
    pop_design_effect       = 1.5,
    teams                   = 3,
    enumerators_per_team    = 4,
    clusters_per_day        = 2,
    avg_interview_time      = 40,
    avg_travel_time         = 45,
    avg_rest_time           = 60,
    start_time              = "2024-01-01",
    end_time                = "2024-01-31"
  )
  p$calculate_sample_sizes()
  st <- p$get_sample_table()

  expect_false(is.na(st$num_interview_per_enum_per_day))
  expect_false(is.na(st$num_days))
  expect_false(is.na(st$num_psu_needed))
  expect_false(is.na(st$psu_size))
  expect_true(st$num_interview_per_enum_per_day > 0)
  expect_true(st$num_days > 0)
  expect_true(st$num_psu_needed > 0)
  expect_true(st$psu_size > 0)
})

# ---- cluster stratum missing clusters_per_day leaves field-plan NA -----------

test_that("cluster stratum without clusters_per_day leaves field-plan columns NA", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Rural",
    population_size         = 20000,
    sampling_method         = "pps_cluster",
    pop_expected_prevalence = 50,
    pop_precision           = 5,
    teams                   = 3,
    enumerators_per_team    = 4,
    # clusters_per_day intentionally omitted
    avg_interview_time      = 40,
    avg_travel_time         = 45,
    avg_rest_time           = 60,
    start_time              = "2024-01-01",
    end_time                = "2024-01-31"
  )
  p$calculate_sample_sizes()
  st <- p$get_sample_table()

  expect_true(is.na(st$num_interview_per_enum_per_day))
  expect_true(is.na(st$num_days))
  expect_true(is.na(st$num_psu_needed))
  expect_true(is.na(st$psu_size))
})

# ---- multi-stratum: each row gets its own field plan -------------------------

test_that("calculate_sample_sizes fills field-plan per stratum independently", {
  p <- make_protocol()

  # stratum with full logistics params
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Urban",
    population_size         = 10000,
    sampling_method         = "srs",
    pop_expected_prevalence = 50,
    pop_precision           = 5,
    teams                   = 2,
    enumerators_per_team    = 3,
    avg_interview_time      = 45,
    avg_travel_time         = 30,
    avg_rest_time           = 60,
    start_time              = "2024-01-01",
    end_time                = "2024-01-31"
  )

  # stratum without logistics params
  p$add_stratum(
    stratum_id              = "s2",
    stratum_name            = "Rural",
    population_size         = 5000,
    sampling_method         = "srs",
    pop_expected_prevalence = 50,
    pop_precision           = 7
  )

  p$calculate_sample_sizes()
  st <- p$get_sample_table()

  s1 <- st[st$stratum_id == "s1", ]
  s2 <- st[st$stratum_id == "s2", ]

  expect_false(is.na(s1$num_days))
  expect_true(is.na(s2$num_days))
})
