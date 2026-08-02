# Tests for SurveyProtocol and related classes (Sample, SamplingFrame).
#
# Principle: inherited functionality (Orchestrator, Document, Protocol) is
# tested in their own test files.  This file covers only what SurveyProtocol
# adds: sampling-frame management, sample-size calculations, draw_sample /
# clear_sample integration, active bindings, and SurveyProtocol-specific
# coherence checks.

# ---- helpers -----------------------------------------------------------------

make_protocol <- function() {
  create_survey_protocol(
    assessment_title = "Test Assessment",
    country_name     = "Testland",
    month_year       = "January 2024"
  )
}

# Shortcut: add a single simple_random stratum to a SurveyProtocol sample_object
add_simple_stratum <- function(p, stratum_id = "s1", stratum_name = "S1",
                                n_sites = 5, population_size = 10000,
                                sample_size = 100) {
  p$sample_object$add_stratum(
    stratum_id       = stratum_id,
    stratum_name     = stratum_name,
    sampling_method_site = "simple_random",
    n_sites          = n_sites,
    population_size  = population_size,
    General_HH_Sample_Size = sample_size
  )
  invisible(p)
}

make_frame <- function(strata = "s1", n_psu = 20, pop = 500) {
  data.frame(
    stratum         = rep(strata, each = n_psu),
    psu             = paste0("psu_", seq_len(n_psu * length(strata))),
    population_size = rep(pop, n_psu * length(strata)),
    inclusion       = TRUE,
    stringsAsFactors = FALSE
  )
}


# ── SurveyProtocol initialization ─────────────────────────────────────────────

test_that("SurveyProtocol initializes and inherits from Protocol", {
  p <- make_protocol()
  expect_true(inherits(p, "SurveyProtocol"))
  expect_true(inherits(p, "Protocol"))
  expect_true(inherits(p, "Document"))
  expect_true(inherits(p, "Orchestrator"))
})

test_that("SurveyProtocol initialises sample_object as a Sample", {
  p <- make_protocol()
  expect_true(inherits(p$sample_object, "Sample"))
  expect_null(p$get_sample_table())
})

test_that("SurveyProtocol initialises sampling_frame as a SamplingFrame object", {
  p <- make_protocol()
  expect_true(inherits(p$sampling_frame, "SamplingFrame"))
  expect_equal(nrow(p$sampling_frame$log_df), 0L)
  expected_cols <- c("stratum", "psu", "population_size", "inclusion",
                     "sampled_psu", "allocated_sample")
  expect_true(all(expected_cols %in% names(p$sampling_frame$log_df)))
})

test_that("SurveyProtocol stores metadata passed to constructor", {
  p <- create_survey_protocol(
    assessment_title = "My Survey",
    country_name     = "Somalia",
    month_year       = "March 2025"
  )
  expect_equal(p$metadata$assessment_title, "My Survey")
  expect_equal(p$metadata$country_name, "Somalia")
  expect_equal(p$metadata$month_year, "March 2025")
})

test_that("SurveyProtocol accepts a sampling_frame on initialization", {
  frame <- data.frame(
    stratum          = "urban",
    psu              = "psu_1",
    population_size  = 500,
    inclusion        = TRUE,
    sampled_psu      = NA_character_,
    allocated_sample = NA_real_,
    stringsAsFactors = FALSE
  )
  p <- create_survey_protocol(sampling_frame = frame)
  expect_true(inherits(p$sampling_frame, "SamplingFrame"))
  expect_equal(nrow(p$sampling_frame$log_df), 1L)
  expect_equal(p$sampling_frame$log_df$psu, "psu_1")
})

# ── Sample$add_stratum via sample_object ───────────────────────────────────────

test_that("sample_object$add_stratum adds a row to the sample table", {
  p <- make_protocol()
  p$sample_object$add_stratum(
    stratum_id       = "s1",
    stratum_name     = "Stratum 1",
    sampling_method_site = "simple_random",
    n_sites          = 5,
    population_size  = 10000
  )
  st <- p$get_sample_table()
  expect_equal(nrow(st), 1L)
  expect_equal(st$stratum_id, "s1")
  expect_equal(st$stratum_name, "Stratum 1")
  expect_equal(st$sampling_method_site, "simple_random")
  expect_equal(st$n_sites, 5)
})

test_that("sample_object$add_stratum initializes num_interview_per_enum_per_day and num_days as NA", {
  p <- make_protocol()
  add_simple_stratum(p)
  st <- p$get_sample_table()
  expect_true("num_interview_per_enum_per_day" %in% names(st))
  expect_true("num_days"                       %in% names(st))
  expect_true(is.na(st$num_interview_per_enum_per_day))
  expect_true(is.na(st$num_days))
})

test_that("sample_object$add_stratum requires sampling_method_site", {
  p <- make_protocol()
  expect_error(
    p$sample_object$add_stratum(stratum_id = "s1", stratum_name = "S1"),
    regexp = "sampling_method_site is required"
  )
})

test_that("sample_object$add_stratum rejects invalid sampling_method_site values", {
  p <- make_protocol()
  expect_error(
    p$sample_object$add_stratum(
      stratum_id = "s1", stratum_name = "S1",
      sampling_method_site = "pps_cluster", n_sites = 5
    ),
    regexp = "sampling_method_site must be one of"
  )
})

test_that("sample_object$add_stratum requires n_sites", {
  p <- make_protocol()
  expect_error(
    p$sample_object$add_stratum(
      stratum_id = "s1", stratum_name = "S1",
      sampling_method_site = "simple_random"
    ),
    regexp = "n_sites is required"
  )
})

test_that("sample_object$add_stratum defaults sampling_method_hh to simple_random", {
  p <- make_protocol()
  add_simple_stratum(p)
  st <- p$get_sample_table()
  expect_equal(st$sampling_method_hh, "simple_random")
})

test_that("sample_object$add_stratum sets rlc household method when specified", {
  p <- make_protocol()
  p$sample_object$add_stratum(
    stratum_id       = "s1",
    stratum_name     = "S1",
    sampling_method_site = "simple_random",
    sampling_method_hh   = "rlc",
    n_sites          = 5,
    cluster_size     = 4
  )
  st <- p$get_sample_table()
  expect_equal(st$sampling_method_hh, "rlc")
  expect_equal(st$cluster_size, 4)
})

test_that("sample_object$add_stratum defaults cluster_size to 3 when rlc and no cluster_size given", {
  p <- make_protocol()
  p$sample_object$add_stratum(
    stratum_id       = "s1",
    stratum_name     = "S1",
    sampling_method_site = "simple_random",
    sampling_method_hh   = "rlc",
    n_sites          = 5
  )
  st <- p$get_sample_table()
  expect_equal(st$cluster_size, 3L)
})

test_that("sample_object$add_stratum overwrites existing stratum_id with warning", {
  p <- make_protocol()
  add_simple_stratum(p, sample_size = 100)
  expect_warning(
    add_simple_stratum(p, sample_size = 200),
    regexp = "already exists"
  )
  st <- p$get_sample_table()
  expect_equal(nrow(st), 1L)
  expect_equal(st$General_HH_Sample_Size, 200)
})

test_that("sample_object$add_stratum multiple strata accumulates rows", {
  p <- make_protocol()
  add_simple_stratum(p, stratum_id = "s1", stratum_name = "S1")
  add_simple_stratum(p, stratum_id = "s2", stratum_name = "S2")
  st <- p$get_sample_table()
  expect_equal(nrow(st), 2L)
})

test_that("sample_object$add_stratum all valid site selection methods work", {
  p <- make_protocol()
  for (m in c("simple_random", "proportional", "systematic", "purposive")) {
    p$sample_object$add_stratum(
      stratum_id = m, stratum_name = m,
      sampling_method_site = m, n_sites = 5
    )
  }
  # cluster method also valid
  p$sample_object$add_stratum(
    stratum_id = "cluster", stratum_name = "cluster",
    sampling_method_site = "cluster", n_sites = 10
  )
  st <- p$get_sample_table()
  expect_equal(nrow(st), 5L)
})

# ── sample_object delegation via SurveyProtocol methods ───────────────────────

test_that("get_sample_table returns NULL when no strata added", {
  p <- make_protocol()
  expect_null(p$get_sample_table())
})

test_that("get_sample_table returns the sample table after add_stratum", {
  p <- make_protocol()
  add_simple_stratum(p)
  st <- p$get_sample_table()
  expect_true(is.data.frame(st))
  expect_equal(nrow(st), 1L)
})

test_that("get_sampling_methods returns character(0) when no strata", {
  p <- make_protocol()
  expect_equal(p$get_sampling_methods(), character(0))
})

test_that("get_sampling_methods returns methods after add_stratum", {
  p <- make_protocol()
  add_simple_stratum(p)
  methods <- p$get_sampling_methods()
  expect_equal(methods, "simple_random")
})

test_that("get_strata_names returns character(0) when no strata", {
  p <- make_protocol()
  expect_equal(p$get_strata_names(), character(0))
})

test_that("get_strata_names returns names after add_stratum", {
  p <- make_protocol()
  add_simple_stratum(p, stratum_id = "s1", stratum_name = "S1")
  add_simple_stratum(p, stratum_id = "s2", stratum_name = "S2")
  names_out <- p$get_strata_names()
  expect_true("S1" %in% names_out)
  expect_true("S2" %in% names_out)
})

test_that("validate_strata_table returns FALSE when no sample table", {
  p <- make_protocol()
  expect_false(p$validate_strata_table())
})

test_that("validate_strata_table returns TRUE for a valid sample table", {
  p <- make_protocol()
  add_simple_stratum(p)
  result <- p$validate_strata_table()
  expect_true(isTRUE(result))
})

# ── Sample$calculate_sample_sizes via sample_object ────────────────────────────

test_that("sample_object$calculate_sample_sizes leaves field-plan columns NA when logistics params absent", {
  p <- make_protocol()
  add_simple_stratum(p,
    population_size = 10000,
    n_sites = 5,
    sample_size = 100
  )
  p$sample_object$calculate_sample_sizes()
  st <- p$get_sample_table()
  expect_true(is.na(st$num_interview_per_enum_per_day))
  expect_true(is.na(st$num_days))
})

test_that("sample_object$calculate_sample_sizes populates field-plan columns when logistics given", {
  p <- make_protocol()
  p$sample_object$add_stratum(
    stratum_id              = "s1",
    stratum_name            = "Urban",
    sampling_method_site    = "simple_random",
    n_sites                 = 5,
    population_size         = 10000,
    General_HH_Sample_Size  = 100,
    teams                   = 2,
    enumerators_per_team    = 3,
    avg_interview_time      = 45,
    avg_travel_time         = 30,
    avg_rest_time           = 60,
    start_time              = "2024-01-01",
    end_time                = "2024-01-31"
  )
  p$sample_object$calculate_sample_sizes()
  st <- p$get_sample_table()
  expect_false(is.na(st$num_interview_per_enum_per_day))
  expect_false(is.na(st$num_days))
  expect_true(st$num_interview_per_enum_per_day > 0)
  expect_true(st$num_days > 0)
})

test_that("sample_object$calculate_sample_sizes errors when sample table empty", {
  s <- Sample$new()
  expect_error(s$calculate_sample_sizes(), regexp = "sample_table is empty")
})

test_that("sample_object$calculate_sample_sizes fills field-plan per stratum independently", {
  p <- make_protocol()
  # stratum with full logistics
  p$sample_object$add_stratum(
    stratum_id           = "s1",
    stratum_name         = "Urban",
    sampling_method_site = "simple_random",
    n_sites              = 5,
    population_size      = 10000,
    General_HH_Sample_Size = 100,
    teams                = 2,
    enumerators_per_team = 3,
    avg_interview_time   = 45,
    avg_travel_time      = 30,
    avg_rest_time        = 60,
    start_time           = "2024-01-01",
    end_time             = "2024-01-31"
  )
  # stratum without logistics
  add_simple_stratum(p, stratum_id = "s2", stratum_name = "Rural")
  p$sample_object$calculate_sample_sizes()
  st <- p$get_sample_table()
  s1 <- st[st$stratum_id == "s1", ]
  s2 <- st[st$stratum_id == "s2", ]
  expect_false(is.na(s1$num_days))
  expect_true(is.na(s2$num_days))
})

# ── set_sampling_frame ─────────────────────────────────────────────────────────

test_that("set_sampling_frame stores data in the SamplingFrame log_df", {
  p <- make_protocol()
  frame <- data.frame(
    stratum         = c("urban", "rural"),
    psu             = c("psu_1", "psu_2"),
    population_size = c(500, 800),
    inclusion       = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  expect_equal(nrow(p$sampling_frame$log_df), 2L)
  expect_true("inclusion" %in% names(p$sampling_frame$log_df))
})

test_that("set_sampling_frame adds inclusion=TRUE column when absent", {
  p <- make_protocol()
  frame <- data.frame(
    stratum         = "urban",
    psu             = "psu_1",
    population_size = 500,
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  expect_true("inclusion" %in% names(p$sampling_frame$log_df))
  expect_true(all(p$sampling_frame$log_df$inclusion))
})

test_that("set_sampling_frame errors on empty data frame", {
  p <- make_protocol()
  expect_error(
    p$set_sampling_frame(data.frame()),
    regexp = "empty"
  )
})

test_that("set_sampling_frame errors on NULL input", {
  p <- make_protocol()
  expect_error(p$set_sampling_frame(NULL))
})

test_that("set_sampling_frame triggers coherence check and updates sampling_frame_strata_names", {
  p <- make_protocol()
  frame <- data.frame(
    stratum         = c("urban", "rural"),
    psu             = paste0("psu_", 1:4),
    population_size = rep(500, 4),
    inclusion       = TRUE,
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  expect_true(length(p$sampling_frame_strata_names) > 0)
  expect_true("urban" %in% p$sampling_frame_strata_names)
  expect_true("rural" %in% p$sampling_frame_strata_names)
})

# ── get_frame_column ───────────────────────────────────────────────────────────

test_that("get_frame_column returns NULL when frame not set", {
  p <- make_protocol()
  expect_null(p$get_frame_column("psu"))
})

test_that("get_frame_column returns NULL for missing column", {
  p <- make_protocol()
  p$set_sampling_frame(make_frame())
  expect_null(p$get_frame_column("nonexistent_col"))
})

test_that("get_frame_column returns all included values by default", {
  p <- make_protocol()
  p$set_sampling_frame(make_frame())
  psus <- p$get_frame_column("psu")
  expect_equal(length(psus), 20L)
})

test_that("get_frame_column filters by stratum", {
  p <- make_protocol()
  frame <- data.frame(
    stratum         = c(rep("urban", 5), rep("rural", 10)),
    psu             = paste0("psu_", seq_len(15)),
    population_size = rep(500, 15),
    inclusion       = TRUE,
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  urban_psus <- p$get_frame_column("psu", strata = "urban")
  expect_equal(length(urban_psus), 5L)
})

test_that("get_frame_column respects included_only flag", {
  p <- make_protocol()
  frame <- data.frame(
    stratum         = rep("s1", 5),
    psu             = paste0("psu_", seq_len(5)),
    population_size = rep(500, 5),
    inclusion       = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  included <- p$get_frame_column("psu", included_only = TRUE)
  all_psus <- p$get_frame_column("psu", included_only = FALSE)
  expect_equal(length(included), 3L)
  expect_equal(length(all_psus), 5L)
})

# ── diagnose_coherence strata checks ──────────────────────────────────────────

test_that("diagnose_coherence adds strata_missing_in_frame issue when mismatch", {
  p <- make_protocol()
  add_simple_stratum(p, stratum_id = "s1", stratum_name = "S1")
  frame <- data.frame(
    stratum         = "other_stratum",
    psu             = paste0("psu_", seq_len(5)),
    population_size = 500,
    inclusion       = TRUE,
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  p$diagnose_coherence()
  expect_true(
    !is.null(p$issues_coherence$strata_missing_in_frame) ||
    !is.null(p$issues_coherence$strata_missing_in_table)
  )
})

test_that("diagnose_coherence finds no strata mismatch when strata match", {
  p <- make_protocol()
  add_simple_stratum(p, stratum_id = "s1", stratum_name = "S1")
  frame <- data.frame(
    stratum         = rep("s1", 5),
    psu             = paste0("psu_", seq_len(5)),
    population_size = 500,
    inclusion       = TRUE,
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  p$diagnose_coherence()
  expect_null(p$issues_coherence$strata_missing_in_frame)
  expect_null(p$issues_coherence$strata_missing_in_table)
})

# ── SamplingFrame class ────────────────────────────────────────────────────────

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
    sampled_psu      = NA_character_,
    allocated_sample = NA_real_,
    stringsAsFactors = FALSE
  )
  sf <- SamplingFrame$new(log_df = frame)
  expect_equal(nrow(sf$log_df), 1L)
  expect_equal(sf$log_df$stratum, "A")
})

# ── draw_sample via sampling_frame ─────────────────────────────────────────────

test_that("sampling_frame$draw_sample selects PSUs for simple_random stratum", {
  p <- make_protocol()
  add_simple_stratum(p, stratum_id = "s1", stratum_name = "Urban",
                     n_sites = 5, population_size = 10000, sample_size = 50)
  p$set_sampling_frame(make_frame())
  p$sampling_frame$draw_sample(strata_table = p$get_sample_table())
  expect_false(all(is.na(p$sampling_frame$log_df$sampled_psu)))
  expect_false(is.null(p$sampling_frame$drawn_sample))
})

test_that("sampling_frame$draw_sample (simple_random) includes RC-labelled reserve PSUs", {
  p <- make_protocol()
  add_simple_stratum(p, stratum_id = "s1", n_sites = 5, sample_size = 50)
  p$set_sampling_frame(make_frame(n_psu = 20))
  p$sampling_frame$draw_sample(strata_table = p$get_sample_table())
  psu_vals <- p$sampling_frame$drawn_sample$sampled_psu
  # n_main = 5 (<=10) -> 3 RC; total drawn = 8
  expect_equal(sum(psu_vals == "RC"), 3L)
})

test_that("sampling_frame$draw_sample (purposive) selects all PSUs", {
  p <- make_protocol()
  p$sample_object$add_stratum(
    stratum_id       = "s1",
    stratum_name     = "All",
    sampling_method_site = "purposive",
    n_sites          = 10,
    General_HH_Sample_Size = 50
  )
  frame <- make_frame(n_psu = 10)
  p$set_sampling_frame(frame)
  p$sampling_frame$draw_sample(strata_table = p$get_sample_table())
  # Purposive selects all PSUs
  expect_equal(nrow(p$sampling_frame$drawn_sample), 10L)
})

test_that("sampling_frame$draw_sample (cluster/pps) requires n_psu and cluster_size at draw time", {
  p <- make_protocol()
  p$sample_object$add_stratum(
    stratum_id       = "s1",
    stratum_name     = "Rural",
    sampling_method_site = "cluster",
    n_sites          = 10,
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
  # cluster without n_psu in sample table -> warning and skip
  expect_warning(
    p$sampling_frame$draw_sample(strata_table = p$get_sample_table()),
    regexp = "skipped"
  )
  expect_true(all(is.na(p$sampling_frame$log_df$sampled_psu)))
})

test_that("sampling_frame$draw_sample with cluster/rlc selects only n_sites PSUs", {
  p <- make_protocol()
  p$sample_object$add_stratum(
    stratum_id       = "s1",
    stratum_name     = "Rural",
    sampling_method_site = "cluster",
    sampling_method_hh   = "rlc",
    n_sites          = 5,
    cluster_size     = 3,
    General_HH_Sample_Size = 30
  )
  frame <- make_frame(n_psu = 20)
  p$set_sampling_frame(frame)
  p$sampling_frame$draw_sample(strata_table = p$get_sample_table())
  selected <- p$sampling_frame$log_df[!is.na(p$sampling_frame$log_df$sampled_psu), ]
  expect_lte(nrow(selected), 5L)
})

test_that("sampling_frame$draw_sample warns and skips stratum not in frame", {
  p <- make_protocol()
  add_simple_stratum(p, stratum_id = "s1")
  frame <- data.frame(
    stratum         = rep("s2", 5),
    psu             = paste0("psu_", seq_len(5)),
    population_size = 500,
    inclusion       = TRUE,
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  expect_warning(
    p$sampling_frame$draw_sample(strata_table = p$get_sample_table()),
    regexp = "skipped"
  )
  expect_true(all(is.na(p$sampling_frame$log_df$sampled_psu)))
})

test_that("sampling_frame$draw_sample continues with other strata when one fails", {
  p <- make_protocol()
  # s1: cluster without n_psu -- will warn and skip
  p$sample_object$add_stratum(
    stratum_id       = "s1",
    stratum_name     = "Rural",
    sampling_method_site = "cluster",
    n_sites          = 5,
    cluster_size     = 5,
    General_HH_Sample_Size = 50
  )
  # s2: simple_random -- will succeed
  add_simple_stratum(p, stratum_id = "s2", stratum_name = "Urban",
                     n_sites = 3, sample_size = 30)
  frame <- data.frame(
    stratum         = c(rep("s1", 10), rep("s2", 10)),
    psu             = paste0("psu_", seq_len(20)),
    population_size = round(runif(20, 100, 500)),
    inclusion       = TRUE,
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  expect_warning(
    p$sampling_frame$draw_sample(strata_table = p$get_sample_table()),
    regexp = "skipped"
  )
  s2_rows <- p$sampling_frame$log_df[p$sampling_frame$log_df$stratum == "s2", ]
  expect_false(all(is.na(s2_rows$sampled_psu)))
})

test_that("sampling_frame$draw_sample multi-stratum uses sequential PSU offset", {
  p <- make_protocol()
  add_simple_stratum(p, stratum_id = "s1", stratum_name = "Urban",
                     n_sites = 5, sample_size = 50)
  add_simple_stratum(p, stratum_id = "s2", stratum_name = "Rural",
                     n_sites = 3, sample_size = 30)
  frame <- data.frame(
    stratum         = c(rep("s1", 20), rep("s2", 15)),
    psu             = paste0("psu_", seq_len(35)),
    population_size = rep(500, 35),
    inclusion       = rep(TRUE, 35),
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  p$sampling_frame$draw_sample(strata_table = p$get_sample_table())
  s1_vals <- p$sampling_frame$log_df$sampled_psu[p$sampling_frame$log_df$stratum == "s1"]
  s2_vals <- p$sampling_frame$log_df$sampled_psu[p$sampling_frame$log_df$stratum == "s2"]
  s1_nums <- suppressWarnings(as.integer(s1_vals[!is.na(s1_vals) & s1_vals != "RC"]))
  s2_nums <- suppressWarnings(as.integer(s2_vals[!is.na(s2_vals) & s2_vals != "RC"]))
  # Sequential offset: s2 numbers > max(s1 numbers)
  expect_true(min(s2_nums) > max(s1_nums))
})

# ── clear_sample via sampling_frame ───────────────────────────────────────────

test_that("sampling_frame$clear_sample resets sampled_psu and allocated_sample", {
  p <- make_protocol()
  add_simple_stratum(p, stratum_id = "s1", n_sites = 5, sample_size = 50)
  p$set_sampling_frame(make_frame(n_psu = 20))
  p$sampling_frame$draw_sample(strata_table = p$get_sample_table())
  expect_false(all(is.na(p$sampling_frame$log_df$sampled_psu)))

  # Clear and verify
  p$sampling_frame$log_df <- p$sampling_frame$clear_sample(p$sampling_frame$log_df)
  expect_true(all(is.na(p$sampling_frame$log_df$sampled_psu)))
  expect_true(all(is.na(p$sampling_frame$log_df$allocated_sample)))
  expect_null(p$sampling_frame$drawn_sample)
  expect_null(p$sampling_frame$drawn_sample_full)
})

test_that("sampling_frame$clear_sample is a no-op on an empty frame", {
  p <- make_protocol()
  sf <- SamplingFrame$new()
  result <- sf$clear_sample(sf$log_df)
  expect_equal(nrow(result), 0L)
  expect_null(sf$drawn_sample)
})

# ── Nested sample_object accessibility (light) ────────────────────────────────

test_that("sample_object is a Sample and accessible via $sample_object", {
  p <- make_protocol()
  expect_true(inherits(p$sample_object, "Sample"))
  expect_true(is.function(p$sample_object$add_stratum))
  expect_true(is.function(p$sample_object$get_sample_table))
})

test_that("sampling_frame is a SamplingFrame and accessible via $sampling_frame", {
  p <- make_protocol()
  expect_true(inherits(p$sampling_frame, "SamplingFrame"))
  expect_true(is.data.frame(p$sampling_frame$log_df))
})

# ── access_nested integration with sample_object ───────────────────────────────

test_that("access_nested to sample_object$add_stratum works and updates modified_datetime", {
  p <- make_protocol()
  t_before <- p$metadata$modified_datetime
  Sys.sleep(0.01)
  p$access_nested(
    field  = "sample_object",
    member = "add_stratum",
    stratum_id       = "s1",
    stratum_name     = "S1",
    sampling_method_site = "simple_random",
    n_sites          = 2
  )
  expect_true(p$metadata$modified_datetime > t_before)
  expect_equal(
    p$access_nested(field = "sample_object", member = "get_sampling_methods"),
    "simple_random"
  )
  expect_equal(
    p$access_nested(field = "sample_object", member = "get_strata_names"),
    "S1"
  )
  st <- p$access_nested(field = "sample_object", member = "get_sample_table")
  expect_equal(nrow(st), 1L)
})

test_that("access_nested to sample_object$remove_stratum removes stratum", {
  p <- make_protocol()
  p$access_nested(
    field  = "sample_object",
    member = "add_stratum",
    stratum_id       = "s1",
    stratum_name     = "S1",
    sampling_method_site = "simple_random",
    n_sites          = 2
  )
  p$access_nested(field = "sample_object", member = "remove_stratum", "S1")
  st <- p$access_nested(field = "sample_object", member = "get_sample_table")
  expect_equal(nrow(st), 0L)
})

# ── SurveyProtocol sync: strata_names, sampling_methods are synced ─────────────

test_that("strata_names field is synced after adding strata", {
  p <- make_protocol()
  add_simple_stratum(p, stratum_id = "s1", stratum_name = "North")
  expect_true("North" %in% p$strata_names)
})

test_that("sampling_methods field is synced after adding strata", {
  p <- make_protocol()
  add_simple_stratum(p)
  expect_equal(p$sampling_methods, "simple_random")
})

test_that("sample_table field is synced after adding strata", {
  p <- make_protocol()
  add_simple_stratum(p)
  expect_true(is.data.frame(p$sample_table))
  expect_equal(nrow(p$sample_table), 1L)
})

# ── SurveyProtocol active bindings ─────────────────────────────────────────────

test_that(".site_selection_srs is TRUE when simple_random method is used", {
  p <- make_protocol()
  expect_false(isTRUE(p$.site_selection_srs))
  add_simple_stratum(p)
  expect_true(isTRUE(p$.site_selection_srs))
})

test_that(".site_selection_purposive is TRUE when purposive method is used", {
  p <- make_protocol()
  p$sample_object$add_stratum(
    stratum_id = "s1", stratum_name = "S1",
    sampling_method_site = "purposive", n_sites = 5
  )
  expect_true(isTRUE(p$.site_selection_purposive))
})

test_that(".multiple_methods is TRUE when multiple site methods are used", {
  p <- make_protocol()
  expect_false(isTRUE(p$.multiple_methods))
  add_simple_stratum(p, stratum_id = "s1")
  p$sample_object$add_stratum(
    stratum_id = "s2", stratum_name = "S2",
    sampling_method_site = "purposive", n_sites = 5
  )
  expect_true(isTRUE(p$.multiple_methods))
})

test_that(".multiple_strata is FALSE with one stratum and TRUE with two", {
  p <- make_protocol()
  expect_false(isTRUE(p$.multiple_strata))
  add_simple_stratum(p, stratum_id = "s1")
  expect_false(isTRUE(p$.multiple_strata))
  add_simple_stratum(p, stratum_id = "s2", stratum_name = "S2")
  expect_true(isTRUE(p$.multiple_strata))
})

test_that(".hh_selection_rlc is TRUE when rlc household method is used", {
  p <- make_protocol()
  expect_false(isTRUE(p$.hh_selection_rlc))
  p$sample_object$add_stratum(
    stratum_id       = "s1",
    stratum_name     = "S1",
    sampling_method_site = "simple_random",
    sampling_method_hh   = "rlc",
    n_sites          = 5
  )
  expect_true(isTRUE(p$.hh_selection_rlc))
})

test_that(".total_population_size reflects population from sampling frame", {
  p <- make_protocol()
  expect_equal(p$.total_population_size, 0)
  frame <- data.frame(
    stratum         = rep("s1", 3),
    psu             = paste0("psu_", seq_len(3)),
    population_size = c(100, 200, 300),
    inclusion       = TRUE,
    stringsAsFactors = FALSE
  )
  p$set_sampling_frame(frame)
  expect_equal(p$.total_population_size, 600)
})

test_that(".num_strata_units reflects number of strata in sample table", {
  p <- make_protocol()
  expect_equal(p$.num_strata_units, 0L)
  add_simple_stratum(p, stratum_id = "s1")
  expect_equal(p$.num_strata_units, 1L)
  add_simple_stratum(p, stratum_id = "s2", stratum_name = "S2")
  expect_equal(p$.num_strata_units, 2L)
})

test_that(".stratified_strata_names_srs_srs returns strata using simple_random/simple_random", {
  p <- make_protocol()
  add_simple_stratum(p, stratum_id = "s1", stratum_name = "Urban")
  p$sample_object$add_stratum(
    stratum_id = "s2", stratum_name = "Rural",
    sampling_method_site = "purposive", n_sites = 5
  )
  srs_names <- p$.stratified_strata_names_srs_srs
  expect_true("Urban" %in% srs_names)
  expect_false("Rural" %in% srs_names)
})

# ── get_quarto_params for SurveyProtocol ──────────────────────────────────────

test_that("SurveyProtocol$get_quarto_params returns expected sampling keys", {
  p <- make_protocol()
  params <- p$get_quarto_params()
  expect_true(is.list(params))
  expect_true("rate_survey"              %in% names(params))
  expect_true("site_selection_srs"       %in% names(params))
  expect_true("multiple_methods"         %in% names(params))
  expect_true("total_population_size"    %in% names(params))
  expect_true("strata_names"             %in% names(params))
  # Inherits assessment_title from Protocol
  expect_true("assessment_title"         %in% names(params))
})

# ── draw_sample_psu_* utility tests ───────────────────────────────────────────

test_that("draw_sample_psu_srs selects n_psu main PSUs with correct RC count for n<=10", {
  result <- draw_sample_psu_srs(
    data.frame(population_size = rep(100, 30)), n_psu = 5, sample_size = 50, seed = 7
  )
  selected <- result[!is.na(result$sampled_psu), ]
  # n_main=5 (<=10) -> 3 RC; total=8
  expect_equal(sum(selected$sampled_psu == "RC"), 3L)
  main_nums <- suppressWarnings(as.integer(selected$sampled_psu[selected$sampled_psu != "RC"]))
  expect_equal(sort(main_nums), 1:5)
})

test_that("draw_sample_psu_srs RC count correct for n_main > 10 and > 20", {
  # n_main = 15 -> 4 RC
  result_15 <- draw_sample_psu_srs(
    data.frame(population_size = rep(100, 30)), n_psu = 15, sample_size = 150, seed = 7
  )
  selected <- result_15[!is.na(result_15$sampled_psu), ]
  expect_equal(sum(selected$sampled_psu == "RC"), 4L)

  # n_main = 25 -> 5 RC
  result_25 <- draw_sample_psu_srs(
    data.frame(population_size = rep(100, 50)), n_psu = 25, sample_size = 250, seed = 7
  )
  selected25 <- result_25[!is.na(result_25$sampled_psu), ]
  expect_equal(sum(selected25$sampled_psu == "RC"), 5L)
})

test_that("draw_sample_psu_rlc errors when n_sites is missing", {
  frame <- data.frame(population_size = rep(100, 20))
  expect_error(
    draw_sample_psu_rlc(frame, sample_size = 60, n_sites = NULL),
    regexp = "n_sites is required"
  )
})

test_that("draw_sample_psu_rlc restricts cluster allocation to n_sites pre-selected PSUs", {
  frame <- data.frame(population_size = c(100, 200, 150, 300, 250,
                                          120, 180, 90, 400, 110,
                                          220, 130, 170, 310, 240,
                                          80, 160, 270, 190, 350,
                                          100, 200, 150, 300, 250,
                                          120, 180, 90, 400, 110))
  result <- draw_sample_psu_rlc(frame, sample_size = 60, n_sites = 5, cluster_size = 3, seed = 42)
  selected <- result[!is.na(result$sampled_psu), ]
  expect_lte(nrow(selected), 5L)
  all_labels <- unlist(strsplit(selected$sampled_psu, ",\\s*"))
  expect_true(any(trimws(all_labels) == "RC"))
})

test_that("draw_sample_psu_rlc distributes clusters evenly across selected sites", {
  frame <- data.frame(population_size = rep(100L, 20))
  result <- draw_sample_psu_rlc(frame, sample_size = 30, n_sites = 3, cluster_size = 3, seed = 42)
  selected <- result[!is.na(result$sampled_psu), ]
  expect_lte(nrow(selected), 3L)
  slot_counts <- vapply(selected$sampled_psu, function(s) {
    length(trimws(strsplit(s, ",\\s*")[[1]]))
  }, integer(1))
  # n_clusters = ceiling(30/3) = 10; n_reserve = 3 (<=10); n_total = 13
  expect_equal(sum(slot_counts), 13L)
  expect_lte(max(slot_counts) - min(slot_counts), 1L)
})

test_that("draw_sample_psu_srs_rlc selects n_sites PSUs and allocates all cluster slots", {
  frame <- data.frame(population_size = seq(100L, 300L, by = 10L))  # 21 PSUs
  result <- draw_sample_psu_srs_rlc(frame, sample_size = 30, n_sites = 4, cluster_size = 3, seed = 7)
  selected <- result[!is.na(result$sampled_psu), ]
  expect_lte(nrow(selected), 4L)
  all_labels <- unlist(strsplit(selected$sampled_psu, ",\\s*"))
  expect_true(any(trimws(all_labels) == "RC"))
  # Total slots: n_clusters = ceiling(30/3) = 10, n_reserve = 3, n_total = 13
  expect_equal(length(all_labels), 13L)
})

test_that("draw_sample_psu_srs_rlc distributes evenly when population_size absent", {
  frame <- data.frame(psu = paste0("p", seq_len(15)))  # no population_size column
  result <- draw_sample_psu_srs_rlc(frame, sample_size = 30, n_sites = 3, cluster_size = 3, seed = 1)
  selected <- result[!is.na(result$sampled_psu), ]
  slot_counts <- vapply(selected$sampled_psu, function(s) {
    length(trimws(strsplit(s, ",\\s*")[[1]]))
  }, integer(1))
  expect_equal(sum(slot_counts), 13L)
  expect_lte(max(slot_counts) - min(slot_counts), 1L)
})

test_that("draw_sample_psu_systematic_rlc selects n_sites PSUs and allocates all cluster slots", {
  frame <- data.frame(population_size = seq(100L, 300L, by = 10L))  # 21 PSUs
  result <- draw_sample_psu_systematic_rlc(frame, sample_size = 30, n_sites = 4, cluster_size = 3, seed = 7)
  selected <- result[!is.na(result$sampled_psu), ]
  expect_lte(nrow(selected), 4L)
  all_labels <- unlist(strsplit(selected$sampled_psu, ",\\s*"))
  expect_true(any(trimws(all_labels) == "RC"))
  expect_equal(length(all_labels), 13L)
})

test_that("draw_sample_psu_systematic_rlc distributes evenly when population_size absent", {
  frame <- data.frame(psu = paste0("p", seq_len(20)))
  result <- draw_sample_psu_systematic_rlc(frame, sample_size = 30, n_sites = 3, cluster_size = 3, seed = 5)
  selected <- result[!is.na(result$sampled_psu), ]
  slot_counts <- vapply(selected$sampled_psu, function(s) {
    length(trimws(strsplit(s, ",\\s*")[[1]]))
  }, integer(1))
  expect_equal(sum(slot_counts), 13L)
  expect_lte(max(slot_counts) - min(slot_counts), 1L)
})

test_that("draw_sample_psu_proportional_rlc selects all PSUs and allocates all cluster slots", {
  frame <- data.frame(population_size = c(300L, 100L, 200L, 150L, 250L))
  result <- draw_sample_psu_proportional_rlc(frame, sample_size = 30, cluster_size = 3, seed = 42)
  expect_equal(nrow(result[!is.na(result$sampled_psu), ]), nrow(frame))
  all_labels <- unlist(strsplit(result$sampled_psu, ",\\s*"))
  expect_true(any(trimws(all_labels) == "RC"))
  # n_clusters = ceiling(30/3) = 10; n_reserve = 3; n_total = 13
  expect_equal(length(all_labels), 13L)
})

test_that("draw_sample_psu_proportional_rlc allocates proportional to population size", {
  frame <- data.frame(population_size = c(900L, 100L, 100L, 100L, 100L))
  result <- draw_sample_psu_proportional_rlc(frame, sample_size = 30, cluster_size = 3, seed = 1)
  slot_counts <- vapply(result$sampled_psu, function(s) {
    length(trimws(strsplit(s, ",\\s*")[[1]]))
  }, integer(1))
  expect_gt(slot_counts[1], slot_counts[2])
})

test_that("draw_sample_psu_proportional_rlc errors when population_size is missing", {
  frame <- data.frame(psu = paste0("p", seq_len(5)))
  expect_error(
    draw_sample_psu_proportional_rlc(frame, sample_size = 30),
    regexp = "population_size"
  )
})

# ── edge cases: n_sites missing at draw time triggers skip ─────────────────────

test_that("sampling_frame$draw_sample skips simple_random stratum when n_sites is NA in table", {
  p <- make_protocol()
  add_simple_stratum(p, n_sites = 5, sample_size = 30)
  # Corrupt the strata table to remove n_sites
  st <- p$get_sample_table()
  st$n_sites <- NA_real_
  p$sample_object$set_sample_table(st)

  frame <- make_frame(n_psu = 20)
  p$set_sampling_frame(frame)
  expect_warning(
    p$sampling_frame$draw_sample(strata_table = p$get_sample_table()),
    regexp = "skipped"
  )
  expect_true(all(is.na(p$sampling_frame$log_df$sampled_psu)))
})
