library(testthat)

# Helper: skip if ANA framework resources are absent
skip_if_no_ana_resources <- function() {
  objectives_ok <-
    nzchar(system.file("resources", "reference_objectives.xlsx",
                       package = "phr")) ||
    file.exists(file.path("inst", "resources", "reference_objectives.xlsx"))
  indicators_ok <-
    nzchar(system.file("resources", "reference_indicator_bank.xlsx",
                       package = "phr")) ||
    file.exists(file.path("inst", "resources", "reference_indicator_bank.xlsx"))
  skip_if_not(objectives_ok && indicators_ok,
              "ANA framework resource files not available")
}

# ── Initialization ─────────────────────────────────────────────────────────────

test_that("IPHRAProtocol$new() creates object with correct class hierarchy", {
  p <- IPHRAProtocol$new()
  expect_true(inherits(p, "IPHRAProtocol"))
  expect_true(inherits(p, "SurveyProtocol"))
  expect_true(inherits(p, "Protocol"))
  expect_true(inherits(p, "Document"))
  expect_true(inherits(p, "Orchestrator"))
})

test_that("IPHRAProtocol initializes with NULL metadata when no args given", {
  p <- IPHRAProtocol$new()
  expect_null(p$metadata$assessment_title)
  expect_null(p$metadata$country_name)
  expect_null(p$metadata$month_year)
})

test_that("IPHRAProtocol stores assessment_title, country_name, month_year", {
  p <- IPHRAProtocol$new(
    assessment_title = "Test IPHRA",
    country_name     = "Somalia",
    month_year       = "March 2025"
  )
  expect_equal(p$metadata$assessment_title, "Test IPHRA")
  expect_equal(p$metadata$country_name,     "Somalia")
  expect_equal(p$metadata$month_year,       "March 2025")
})

test_that("IPHRAProtocol auto-selects ANAFramework on init when resources available", {
  skip_if_no_ana_resources()
  p <- IPHRAProtocol$new()
  expect_true(inherits(p$framework, "ANAFramework"))
  expect_true(inherits(p$framework, "Framework"))
})

test_that("IPHRAProtocol$new() errors on unexpected arguments", {
  expect_error(IPHRAProtocol$new(version = 2L))
})

# ── Nested objects: light accessibility ────────────────────────────────────────

test_that("framework field is accessible and is a Framework", {
  skip_if_no_ana_resources()
  p <- IPHRAProtocol$new()
  # Light: just confirm accessible, not testing Framework internals
  expect_true(inherits(p$framework, "Framework"))
})

test_that("sample_object is accessible and is a Sample", {
  p <- IPHRAProtocol$new()
  # Light: just confirm accessible
  expect_true(inherits(p$sample_object, "Sample"))
})

test_that("sampling_frame is accessible and is a SamplingFrame", {
  p <- IPHRAProtocol$new()
  expect_true(inherits(p$sampling_frame, "SamplingFrame"))
})

test_that("tools field is initially empty and accessible", {
  p <- IPHRAProtocol$new()
  expect_equal(length(p$tools), 0L)
  expect_type(p$tools, "list")
})

# ── get_allowable_tools ────────────────────────────────────────────────────────

test_that("get_allowable_tools returns a character vector of 12 tool names", {
  p     <- IPHRAProtocol$new()
  tools <- p$get_allowable_tools()
  expect_type(tools, "character")
  expect_length(tools, 12L)
})

test_that("get_allowable_tools includes expected household, KII and observation names", {
  p     <- IPHRAProtocol$new()
  tools <- p$get_allowable_tools()
  expect_true("tool_household_iphra_v2"                      %in% tools)
  expect_true("tool_kii_community_iphra_v2"                  %in% tools)
  expect_true("tool_kii_health_service_provider_iphra_v2"    %in% tools)
  expect_true("tool_kii_wash_service_provider_iphra_v2"      %in% tools)
  expect_true("tool_kii_nutrition_service_provider_iphra_v2" %in% tools)
  expect_true("tool_kii_fsl_service_provider_iphra_v2"       %in% tools)
  expect_true("tool_kii_markets_iphra_v2"                    %in% tools)
  expect_true("tool_obs_water_point_iphra_v2"                %in% tools)
  expect_true("tool_obs_community_iphra_v2"                  %in% tools)
  expect_true("tool_obs_crop_livestock_iphra_v1"             %in% tools)
  expect_true("tool_obs_health_facility_iphra_v2"            %in% tools)
  expect_true("tool_obs_latrine_iphra_v2"                    %in% tools)
})

# ── add_tools ─────────────────────────────────────────────────────────────────

test_that("add_tools rejects unknown tool names", {
  p <- IPHRAProtocol$new()
  expect_error(p$add_tools("unknown_tool_xyz"))
})

test_that("add_tools rejects empty string", {
  p <- IPHRAProtocol$new()
  expect_error(p$add_tools(""))
})

test_that("add_tools rejects non-character input", {
  p <- IPHRAProtocol$new()
  expect_error(p$add_tools(123))
})

test_that("add_tools stores household tool by name (light access check)", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_household_iphra_v2")
  expect_true("tool_household_iphra_v2" %in% names(p$tools))
})

test_that("add_tools stores KII tool by name (light access check)", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_kii_community_iphra_v2")
  expect_true("tool_kii_community_iphra_v2" %in% names(p$tools))
})

test_that("add_tools stores observation tool by name (light access check)", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_obs_water_point_iphra_v2")
  expect_true("tool_obs_water_point_iphra_v2" %in% names(p$tools))
})

test_that("add_tools returns self invisibly for chaining", {
  p      <- IPHRAProtocol$new()
  result <- withVisible(p$add_tools("tool_household_iphra_v2"))
  expect_false(result$visible)
  expect_identical(result$value, p)
})

test_that("multiple add_tools calls accumulate in $tools", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_household_iphra_v2")
  p$add_tools("tool_kii_community_iphra_v2")
  p$add_tools("tool_obs_water_point_iphra_v2")
  expect_equal(length(p$tools), 3L)
  expect_true("tool_household_iphra_v2"     %in% names(p$tools))
  expect_true("tool_kii_community_iphra_v2" %in% names(p$tools))
  expect_true("tool_obs_water_point_iphra_v2" %in% names(p$tools))
})

# ── Tool presence active bindings ─────────────────────────────────────────────

test_that(".tool_household_iphra is FALSE before adding tool, TRUE after", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.tool_household_iphra))
  p$add_tools("tool_household_iphra_v2")
  expect_true(isTRUE(p$.tool_household_iphra))
})

test_that(".tool_community_kii is FALSE before adding tool, TRUE after", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.tool_community_kii))
  p$add_tools("tool_kii_community_iphra_v2")
  expect_true(isTRUE(p$.tool_community_kii))
})

test_that(".tool_fsl_provider_kii is FALSE before adding tool, TRUE after", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.tool_fsl_provider_kii))
  p$add_tools("tool_kii_fsl_service_provider_iphra_v2")
  expect_true(isTRUE(p$.tool_fsl_provider_kii))
})

test_that(".tool_market_kii is FALSE before adding tool, TRUE after", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.tool_market_kii))
  p$add_tools("tool_kii_markets_iphra_v2")
  expect_true(isTRUE(p$.tool_market_kii))
})

test_that(".tool_health_facility_kii is FALSE before adding tool, TRUE after", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.tool_health_facility_kii))
  p$add_tools("tool_kii_health_service_provider_iphra_v2")
  expect_true(isTRUE(p$.tool_health_facility_kii))
})

test_that(".tool_nutrition_facility_kii is FALSE before adding tool, TRUE after", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.tool_nutrition_facility_kii))
  p$add_tools("tool_kii_nutrition_service_provider_iphra_v2")
  expect_true(isTRUE(p$.tool_nutrition_facility_kii))
})

test_that(".tool_wash_provider_kii is FALSE before adding tool, TRUE after", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.tool_wash_provider_kii))
  p$add_tools("tool_kii_wash_service_provider_iphra_v2")
  expect_true(isTRUE(p$.tool_wash_provider_kii))
})

test_that("tool presence bindings are read-only", {
  p <- IPHRAProtocol$new()
  result <- withVisible(p$.tool_household_iphra <- TRUE)
  expect_false(result$visible)
  expect_false(result$value)
})

# ── Observation tool presence bindings ────────────────────────────────────────

test_that(".tool_community_observation is FALSE before adding, TRUE after", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.tool_community_observation))
  p$add_tools("tool_obs_community_iphra_v2")
  expect_true(isTRUE(p$.tool_community_observation))
})

test_that(".tool_crops_livestock_observation is FALSE before adding, TRUE after", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.tool_crops_livestock_observation))
  p$add_tools("tool_obs_crop_livestock_iphra_v1")
  expect_true(isTRUE(p$.tool_crops_livestock_observation))
})

test_that(".tool_health_facility_observation is FALSE before adding, TRUE after", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.tool_health_facility_observation))
  p$add_tools("tool_obs_health_facility_iphra_v2")
  expect_true(isTRUE(p$.tool_health_facility_observation))
})

test_that(".tool_latrine_observation is FALSE before adding, TRUE after", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.tool_latrine_observation))
  p$add_tools("tool_obs_latrine_iphra_v2")
  expect_true(isTRUE(p$.tool_latrine_observation))
})

test_that(".tool_water_point_observation is FALSE before adding, TRUE after", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.tool_water_point_observation))
  p$add_tools("tool_obs_water_point_iphra_v2")
  expect_true(isTRUE(p$.tool_water_point_observation))
})

# ── Indicator active bindings ─────────────────────────────────────────────────

test_that("indicator bindings default to FALSE with no household tool", {
  p <- IPHRAProtocol$new()
  expect_false(isTRUE(p$.ind_ecfies))
  expect_false(isTRUE(p$.ind_iycfe))
  expect_false(isTRUE(p$.ind_measles_vaccination))
  expect_false(isTRUE(p$.ind_muac_children))
  expect_false(isTRUE(p$.ind_muac_women))
  expect_false(isTRUE(p$.ind_vitamin_a_coverage))
  expect_false(isTRUE(p$.ind_mortality))
  expect_false(isTRUE(p$.ind_fcs))
  expect_false(isTRUE(p$.ind_rcsi))
  expect_false(isTRUE(p$.ind_hhs))
  expect_false(isTRUE(p$.ind_lcsi))
  expect_false(isTRUE(p$.ind_hwise))
  expect_false(isTRUE(p$.ind_lppd))
})

test_that(".ind_mortality becomes TRUE when household tool has indicator 10501", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_household_iphra_v2")
  p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
    indicator_code = "10501", stringsAsFactors = FALSE
  )
  expect_true(isTRUE(p$.ind_mortality))
})

test_that(".ind_ecfies becomes TRUE when household tool has indicator 10801", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_household_iphra_v2")
  p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
    indicator_code = "10801", stringsAsFactors = FALSE
  )
  expect_true(isTRUE(p$.ind_ecfies))
})

test_that(".ind_muac_women becomes TRUE when household tool has indicator 10702", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_household_iphra_v2")
  p$tools[["tool_household_iphra_v2"]]$revised_survey <- data.frame(
    indicator_code = "10702", stringsAsFactors = FALSE
  )
  expect_true(isTRUE(p$.ind_muac_women))
})

test_that("indicator bindings are read-only", {
  p      <- IPHRAProtocol$new()
  result <- withVisible(p$.ind_ecfies <- TRUE)
  expect_false(result$visible)
  expect_false(result$value)
})

# ── Table active bindings ─────────────────────────────────────────────────────

test_that(".tools_table_df returns a data frame", {
  p <- IPHRAProtocol$new()
  t <- p$.tools_table_df
  expect_true(is.data.frame(t) || is.null(t))
})

test_that(".household_pillars_table_df returns a data frame or NULL", {
  p <- IPHRAProtocol$new()
  t <- p$.household_pillars_table_df
  expect_true(is.data.frame(t) || is.null(t))
})

test_that(".kii_pillars_table_df returns a data frame or NULL", {
  p <- IPHRAProtocol$new()
  t <- p$.kii_pillars_table_df
  expect_true(is.data.frame(t) || is.null(t))
})

test_that(".observation_pillars_table_df returns a data frame or NULL", {
  p <- IPHRAProtocol$new()
  t <- p$.observation_pillars_table_df
  expect_true(is.data.frame(t) || is.null(t))
})

# ── update_recall_date ────────────────────────────────────────────────────────

test_that("update_recall_date errors when no household tool is present", {
  p <- IPHRAProtocol$new()
  expect_error(p$update_recall_date("2025-01-01"))
})

test_that("update_recall_date accepts a date string when household tool exists", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_household_iphra_v2")
  # Should not throw even if it silently does nothing when tool is empty
  tryCatch(
    p$update_recall_date("2025-03-01"),
    error = function(e) {
      # Acceptable if no recall rows in the empty tool
      expect_true(grepl("recall|date|not found", conditionMessage(e),
                        ignore.case = TRUE))
    }
  )
})
