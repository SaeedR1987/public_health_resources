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
    sampling_method         = "simple_random",
    n_sites                 = 5,
    pop_expected_prevalence = 50,
    pop_precision           = 5
  )
  st <- p$get_sample_table()
  expect_true("num_interview_per_enum_per_day" %in% names(st))
  expect_true("num_days"                       %in% names(st))
  expect_true(is.na(st$num_interview_per_enum_per_day))
  expect_true(is.na(st$num_days))
})

# ---- field-plan columns remain NA when logistics params absent ----------------

test_that("calculate_sample_sizes leaves field-plan columns NA when logistics params are absent", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Stratum 1",
    population_size         = 10000,
    sampling_method         = "simple_random",
    n_sites                 = 5,
    pop_expected_prevalence = 50,
    pop_precision           = 5
  )
  p$calculate_sample_sizes()
  st <- p$get_sample_table()
  expect_true(is.na(st$num_interview_per_enum_per_day))
  expect_true(is.na(st$num_days))
})

# ---- simple_random field plan populates columns ------------------------------

test_that("calculate_sample_sizes populates field-plan columns for simple_random stratum", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Urban",
    population_size         = 10000,
    sampling_method         = "simple_random",
    n_sites                 = 5,
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
  # simple_random: n_psu and cluster_size stay NA (no cluster field plan)
  expect_true(is.na(st$n_psu))
  expect_true(is.na(st$cluster_size))
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
  # cluster: n_psu and cluster_size are populated from field plan
  expect_false(is.na(st$n_psu))
  expect_false(is.na(st$cluster_size))
  expect_true(st$num_interview_per_enum_per_day > 0)
  expect_true(st$num_days > 0)
  expect_true(st$n_psu > 0)
  expect_true(st$cluster_size > 0)
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
  expect_true(is.na(st$n_psu))
  expect_true(is.na(st$cluster_size))
})

# ---- multi-stratum: each row gets its own field plan -------------------------

test_that("calculate_sample_sizes fills field-plan per stratum independently", {
  p <- make_protocol()

  # stratum with full logistics params
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Urban",
    population_size         = 10000,
    sampling_method         = "simple_random",
    n_sites                 = 5,
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
    sampling_method         = "simple_random",
    n_sites                 = 5,
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

# ---- calc_method is correctly derived from sampling_method -------------------

test_that("add_stratum derives calc_method correctly for all sampling_method values", {
  p <- make_protocol()

  simple_random_methods <- c("simple_random", "proportional", "systematic", "purposive")
  cluster_methods       <- c("pps_cluster", "pps_rlc")

  for (m in simple_random_methods) {
    p$add_stratum(stratum_id = m, stratum_name = m, sampling_method = m, n_sites = 1)
  }
  # pps_cluster: n_psu optional at add_stratum time
  p$add_stratum(stratum_id = "pps_cluster", stratum_name = "pps_cluster",
                sampling_method = "pps_cluster")
  # pps_rlc requires n_sites
  p$add_stratum(stratum_id = "pps_rlc", stratum_name = "pps_rlc",
                sampling_method = "pps_rlc", n_sites = 10)

  st <- p$get_sample_table()
  expect_true("calc_method" %in% names(st))

  for (m in simple_random_methods) {
    expect_equal(st$calc_method[st$stratum_id == m], "simple_random",
                 info = paste("calc_method for sampling_method =", m))
  }
  for (m in cluster_methods) {
    expect_equal(st$calc_method[st$stratum_id == m], "cluster",
                 info = paste("calc_method for sampling_method =", m))
  }
})

test_that("add_stratum rejects invalid sampling_method values", {
  p <- make_protocol()
  expect_error(
    p$add_stratum(stratum_id = "s1", stratum_name = "s1", sampling_method = "srs",
                  n_sites = 5),
    regexp = "sampling_method must be one of"
  )
})

test_that("add_stratum errors when sampling_method is not provided", {
  p <- make_protocol()
  expect_error(
    p$add_stratum(stratum_id = "s1", stratum_name = "s1"),
    regexp = "sampling_method is required"
  )
})

test_that("add_stratum errors when n_sites missing for non-pps_cluster method", {
  p <- make_protocol()
  expect_error(
    p$add_stratum(stratum_id = "s1", stratum_name = "s1",
                  sampling_method = "simple_random"),
    regexp = "n_sites is required"
  )
})

test_that("add_stratum for pps_rlc defaults cluster_size to 3", {
  p <- make_protocol()
  p$add_stratum(stratum_id = "s1", stratum_name = "s1",
                sampling_method = "pps_rlc", n_sites = 10)
  st <- p$get_sample_table()
  expect_equal(st$cluster_size, 3)
})

# ---- SamplingFrame initialises as a blank SamplingFrame object ---------------

test_that("SurveyProtocol initialises sampling_frame as a SamplingFrame object", {
  p <- make_protocol()
  expect_true(inherits(p$sampling_frame, "SamplingFrame"))
  expect_equal(nrow(p$sampling_frame$log_df), 0L)
  expected_cols <- c("stratum", "psu", "population_size", "inclusion",
                     "sampled_psu", "allocated_sample")
  expect_true(all(expected_cols %in% names(p$sampling_frame$log_df)))
})

test_that("SurveyProtocol accepts a data frame on init and stores it in SamplingFrame", {
  frame <- data.frame(
    stratum          = "urban",
    psu              = "psu_1",
    population_size  = 500,
    inclusion        = TRUE,
    sampled_psu      = NA_real_,
    allocated_sample = NA_real_,
    stringsAsFactors = FALSE
  )
  p <- create_survey_protocol(sampling_frame = frame)
  expect_true(inherits(p$sampling_frame, "SamplingFrame"))
  expect_equal(nrow(p$sampling_frame$log_df), 1L)
  expect_equal(p$sampling_frame$log_df$psu, "psu_1")
})

test_that("set_sampling_frame stores data in the SamplingFrame log_df", {
  p <- make_protocol()
  frame <- data.frame(
    stratum          = c("urban", "rural"),
    psu              = c("psu_1", "psu_2"),
    population_size  = c(500, 800),
    inclusion        = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  expect_equal(nrow(p$sampling_frame$log_df), 2L)
  expect_true("inclusion" %in% names(p$sampling_frame$log_df))
})

# ---- SamplingFrame class initialises correctly -------------------------------

test_that("SamplingFrame initialises empty with required columns", {
  sf <- SamplingFrame$new()
  expect_true(inherits(sf, "SamplingFrame"))
  expect_equal(nrow(sf$log_df), 0L)
  expected_cols <- c("stratum", "psu", "population_size", "inclusion",
                     "sampled_psu", "allocated_sample")
  expect_true(all(expected_cols %in% names(sf$log_df)))
})

test_that("SamplingFrame initialises with a provided data frame", {
  frame <- data.frame(
    stratum          = "A",
    psu              = "psu_1",
    population_size  = 1000,
    inclusion        = TRUE,
    sampled_psu      = NA_real_,
    allocated_sample = NA_real_,
    stringsAsFactors = FALSE
  )
  sf <- SamplingFrame$new(log_df = frame)
  expect_equal(nrow(sf$log_df), 1L)
  expect_equal(sf$log_df$stratum, "A")
})

# ---- clear_sample clears selection columns but retains frame -----------------

test_that("clear_sample resets sampled_psu and allocated_sample but retains other columns", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Urban",
    population_size         = 10000,
    sampling_method         = "simple_random",
    n_sites                 = 5,
    pop_expected_prevalence = 50,
    pop_precision           = 5
  )
  p$calculate_sample_sizes()

  frame <- data.frame(
    stratum          = rep("s1", 10),
    psu              = paste0("psu_", seq_len(10)),
    population_size  = rep(500, 10),
    inclusion        = rep(TRUE, 10),
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  p$draw_sample()

  # After drawing, at least some PSUs should be selected
  expect_false(all(is.na(p$sampling_frame$log_df$sampled_psu)))
  expect_false(is.null(p$drawn_sample))

  # Clear and verify
  p$clear_sample()

  expect_true(all(is.na(p$sampling_frame$log_df$sampled_psu)))
  expect_true(all(is.na(p$sampling_frame$log_df$allocated_sample)))
  expect_null(p$drawn_sample)
  expect_null(p$drawn_sample_full)

  # Frame columns other than the cleared ones are retained
  expect_equal(nrow(p$sampling_frame$log_df), 10L)
  expect_true(all(p$sampling_frame$log_df$stratum == "s1"))
  expect_equal(p$sampling_frame$log_df$psu, paste0("psu_", seq_len(10)))
})

test_that("clear_sample is a no-op on an empty sampling frame", {
  p <- make_protocol()
  expect_no_error(p$clear_sample())
  expect_equal(nrow(p$sampling_frame$log_df), 0L)
  expect_null(p$drawn_sample)
})

# ---- draw_sample warns and skips stratum when required param missing ----------

test_that("draw_sample issues warning and skips pps_cluster stratum when n_psu missing", {
  p <- make_protocol()
  p$add_stratum(
    stratum_id      = "s1",
    stratum_name    = "Rural",
    sampling_method = "pps_cluster",
    cluster_size    = 5,
    General_HH_Sample_Size = 50
  )
  frame <- data.frame(
    stratum         = rep("s1", 20),
    psu             = paste0("psu_", seq_len(20)),
    population_size = round(runif(20, 100, 500)),
    inclusion       = TRUE,
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  expect_warning(p$draw_sample(), regexp = "skipped")
  # All PSUs remain unselected
  expect_true(all(is.na(p$sampling_frame$log_df$sampled_psu)))
})

test_that("draw_sample warns and skips on failure but continues with other strata", {
  p <- make_protocol()
  # s1: pps_cluster without n_psu — will warn and skip
  p$add_stratum(
    stratum_id      = "s1",
    stratum_name    = "Rural",
    sampling_method = "pps_cluster",
    cluster_size    = 5,
    General_HH_Sample_Size = 50
  )
  # s2: simple_random with n_sites — will succeed
  p$add_stratum(
    stratum_id      = "s2",
    stratum_name    = "Urban",
    sampling_method = "simple_random",
    n_sites         = 3,
    General_HH_Sample_Size = 30
  )
  frame <- data.frame(
    stratum         = c(rep("s1", 10), rep("s2", 10)),
    psu             = paste0("psu_", seq_len(20)),
    population_size = round(runif(20, 100, 500)),
    inclusion       = TRUE,
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  expect_warning(p$draw_sample(), regexp = "skipped")
  # s2 should have some PSUs selected
  s2_rows <- p$sampling_frame$log_df[p$sampling_frame$log_df$stratum == "s2", ]
  expect_false(all(is.na(s2_rows$sampled_psu)))
})
