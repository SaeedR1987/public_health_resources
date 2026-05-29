#!/usr/bin/env Rscript

#' Manual Test Script for Protocol and Tool Workflow

rm(list = ls())

# library(phr)
devtools::load_all()
library(tibble)
library(dplyr)
library(rsvg)

# SETUP: Dummy Assessment Information

assessment_title <- "Multi-Sector Humanitarian Needs Assessment – Dummy Country"
country_name     <- "Dummy Country"
month_year       <- "March 2025"

# Test 1: Create an IPHRAProtocol Object ####

protocol <- IPHRAProtocol$new(
  assessment_title = assessment_title,
  country_name     = country_name,
  month_year       = month_year
)

# Inspect initial state
protocol$metadata

# Validate the objective schema via the protocol method
(protocol$validate_objective_schema(protocol$framework$master_schema))

# Test 2: Framework ####

View(protocol$framework$master_schema)

protocol$get_schema(type = "master")

protocol$get_framework_svg(type = "master")

protocol$set_primary_objectives(objective_codes = c(105, 106, 108, 112, 113, 114, 115, 118, 147))
protocol$set_secondary_objectives(objective_codes = c(105, 107, 112))

protocol$modify_schema(objective_codes = c(105, 106, 108, 112, 113, 114, 115, 118, 147))
protocol$modify_svg()

protocol$get_framework_svg(type = "adjusted")
View(protocol$get_schema(type = "adjusted"))

# Test 3: Define Strata and Sample Sizes ####

protocol$access_nested(
  field = "sample_table",
  member = "add_stratum",
  stratum_id              = "strata_A",
  stratum_name            = "Urban North",
  population_size         = 45000,
  pop_design_effect       = 1.5,
  pop_precision           = 10,
  pop_expected_prevalence = 50,
  pop_nonresponse         = 10,
  ind_indicator           = "wasting_prevalence",
  ind_expected_prevalence = 15,
  ind_precision           = 5,
  ind_nonresponse         = 10,
  ind_design_effect       = 1.5,
  ind_avg_hh_size         = 5.2,
  ind_subpop_prop = 20,
  mort_indicator          = "crude_death_rate",
  mort_expected_death_rate = 0.5,
  mort_precision = 0.5,
  mort_avg_hh_size = 5.2,
  mort_design_effect = 2,
  mort_fpc = FALSE,
  mort_nonresponse = 10,
  teams = 5,
  enumerators_per_team = 1,
  start_time = "10:00", end_time = "18:00",
  clusters_per_day = 2, avg_interview_time = 30,
  avg_rest_time = 30, avg_travel_time = 60,
  sampling_method         = "systematic", n_sites = 10
)



protocol$access_nested(
  field = "sample_table",
  member = "add_stratum",
  stratum_id              = "strata_B",
  stratum_name            = "Peri-Urban East",
  population_size         = 28000,
  pop_design_effect       = 1.8,
  pop_precision           = 5,
  pop_expected_prevalence = 50,
  pop_nonresponse         = 10,
  ind_indicator           = "wasting_prevalence",
  mort_indicator          = "crude_death_rate",
  mort_expected_death_rate = 0.2,
  mort_precision = 0.5,
  mort_avg_hh_size = 5.2,
  mort_design_effect = 2,
  mort_fpc = FALSE,
  mort_nonresponse = 10,
  sampling_method         = "proportional",
  n_sites                 = 30
)

protocol$access_nested(
  field = "sample_table",
  member = "add_stratum",
  stratum_id              = "strata_C",
  stratum_name            = "Rural South",
  population_size         = 17000,
  pop_design_effect       = 2.0,
  pop_precision           = 7,
  pop_expected_prevalence = 50,
  pop_nonresponse         = 10,
  ind_indicator           = "wasting_prevalence",
  mort_indicator          = "crude_death_rate",
  sampling_method         = "simple_random_rlc",
  n_sites                 = 10
)

protocol$access_nested(field = "sample_table", member = "calculate_sample_sizes")
protocol$access_nested(field = "sample_table", member = "get_sample_table")

# Test 4: Build and Validate a Sampling Frame ####

set.seed(42)

make_psu_frame <- function(stratum_id, n_psu, pop_range) {
  tibble::tibble(
    stratum         = stratum_id,
    psu             = paste0(stratum_id, "_v", seq_len(n_psu)),
    population_size = sample(pop_range[1]:pop_range[2], n_psu, replace = TRUE),
    inclusion       = TRUE,
    sampled_psu     = NA,
    allocated_sample = NA
  )
}

frame_A <- make_psu_frame("strata_A", n_psu = 60, pop_range = c(400, 1200))
frame_B <- make_psu_frame("strata_B", n_psu = 45, pop_range = c(250, 800))
frame_C <- make_psu_frame("strata_C", n_psu = 30, pop_range = c(100, 500))

sampling_frame <- dplyr::bind_rows(frame_A, frame_B, frame_C)

protocol$set_sampling_frame(sampling_frame)

protocol$sampling_frame$validate()
protocol$sampling_frame$validated

# Test 5: Draw Sample ####

protocol$draw_sample(seed = 788)

View(protocol$drawn_sample_full)

# Test 6: Testing Tools ####

print(protocol$get_allowable_tools())

# Household Tool ####
protocol$add_tools(tool_name = "tool_household_iphra_v2")

View(protocol$tools$tool_household_iphra_v2$survey)
head(protocol$tools$tool_household_iphra_v2$choices)
head(protocol$tools$tool_household_iphra_v2$settings)

protocol$tools$tool_household_iphra_v2$change_default_language(language = "Arabic")
head(protocol$tools$tool_household_iphra_v2$revised_settings)
protocol$tools$tool_household_iphra_v2$change_default_language(language = "Spanish")
head(protocol$tools$tool_household_iphra_v2$revised_settings)
protocol$tools$tool_household_iphra_v2$change_default_language(language = "French")
head(protocol$tools$tool_household_iphra_v2$revised_settings)
protocol$tools$tool_household_iphra_v2$change_default_language(language = "English")
head(protocol$tools$tool_household_iphra_v2$revised_settings)

protocol$access_nested(
  field = "tools",
  name = "tool_household_iphra_v2",
  member = "filter_survey_by_indicator",
  indicator_codes = protocol$get_indicator_codes_from_schema(type = "adjusted")
)


View(protocol$access_nested(field = "tools", name = "tool_household_iphra_v2", member = "revised_survey"))

View(protocol$tools$tool_household_iphra_v2$revised_survey)
head(protocol$tools$tool_household_iphra_v2$revised_choices)
head(protocol$tools$tool_household_iphra_v2$revised_settings)

protocol$tools$tool_household_iphra_v2$validate() # this part still needs heavy development
protocol$tools$tool_household_iphra_v2$get_validation_errors() # this part still needs heavy development

# Community KII Tool ####
protocol$add_tools("tool_kii_community_iphra_v2")

head(protocol$tools$tool_kii_community_iphra_v2$survey)
head(protocol$tools$tool_kii_community_iphra_v2$choices)
head(protocol$tools$tool_kii_community_iphra_v2$settings)

protocol$access_nested(
  field = "tools",
  name = "tool_kii_community_iphra_v2",
  member = "filter_survey_by_indicator",
  indicator_codes = protocol$get_indicator_codes_from_schema(type = "adjusted")
)

nrow(protocol$tools$tool_kii_community_iphra_v2$survey)
nrow(protocol$tools$tool_kii_community_iphra_v2$revised_survey)

protocol$framework$adjusted_schema$indicator_code

View(protocol$tools$tool_kii_community_iphra_v2$revised_survey)
head(protocol$tools$tool_kii_community_iphra_v2$revised_choices)
head(protocol$tools$tool_kii_community_iphra_v2$revised_settings)

protocol$tools$tool_kii_community_iphra_v2$validate() # this part still needs heavy development
protocol$tools$tool_kii_community_iphra_v2$get_validation_errors() # this part still needs heavy development


# Community Obseration Tool ####

protocol$add_tools(tool_name = "tool_obs_community_iphra_v2")

head(protocol$tools$tool_obs_community_iphra_v2$survey)
head(protocol$tools$tool_obs_community_iphra_v2$choices)
head(protocol$tools$tool_obs_community_iphra_v2$settings)

protocol$tools$tool_obs_community_iphra_v2$filter_survey_by_indicator(indicator_codes = c(protocol$framework$adjusted_schema$indicator_code))

View(protocol$tools$tool_obs_community_iphra_v2$revised_survey)
head(protocol$tools$tool_obs_community_iphra_v2$revised_choices)
head(protocol$tools$tool_obs_community_iphra_v2$revised_settings)

protocol$tools$tool_obs_community_iphra_v2$validate() # this part still needs heavy development, breaking in one of the checks right now
protocol$tools$tool_obs_community_iphra_v2$get_validation_errors() # this part still needs heavy development

# FSL Service Provider KII Tool ####

protocol$add_tools(tool_name = "tool_kii_fsl_service_provider_iphra_v2")

head(protocol$tools$tool_kii_fsl_service_provider_iphra_v2$survey)
head(protocol$tools$tool_kii_fsl_service_provider_iphra_v2$choices)
head(protocol$tools$tool_kii_fsl_service_provider_iphra_v2$settings)

protocol$tools$tool_kii_fsl_service_provider_iphra_v2$filter_survey_by_indicator(indicator_codes = c(protocol$framework$adjusted_schema$indicator_code))

View(protocol$tools$tool_kii_fsl_service_provider_iphra_v2$revised_survey)
head(protocol$tools$tool_kii_fsl_service_provider_iphra_v2$revised_choices)
head(protocol$tools$tool_kii_fsl_service_provider_iphra_v2$revised_settings)

protocol$tools$tool_kii_fsl_service_provider_iphra_v2$validate() # this part still needs heavy development, breaking in one of the checks right now
protocol$tools$tool_kii_fsl_service_provider_iphra_v2$get_validation_errors() # this part still needs heavy development

# WASH Service Provider KII Tool ####

protocol$add_tools(tool_name = "tool_kii_wash_service_provider_iphra_v2")

head(protocol$tools$tool_kii_wash_service_provider_iphra_v2$survey)
head(protocol$tools$tool_kii_wash_service_provider_iphra_v2$choices)
head(protocol$tools$tool_kii_wash_service_provider_iphra_v2$settings)

protocol$tools$tool_kii_wash_service_provider_iphra_v2$filter_survey_by_indicator(indicator_codes = c(protocol$framework$adjusted_schema$indicator_code))

View(protocol$tools$tool_kii_wash_service_provider_iphra_v2$revised_survey)
head(protocol$tools$tool_kii_wash_service_provider_iphra_v2$revised_choices)
head(protocol$tools$tool_kii_wash_service_provider_iphra_v2$revised_settings)

protocol$tools$tool_kii_wash_service_provider_iphra_v2$validate() # this part still needs heavy development, breaking in one of the checks right now
protocol$tools$tool_kii_wash_service_provider_iphra_v2$get_validation_errors() # this part still needs heavy development

# Market Vendor KII Tool ####

protocol$add_tools(tool_name = "tool_kii_markets_iphra_v2")

head(protocol$tools$tool_kii_markets_iphra_v2$survey)
head(protocol$tools$tool_kii_markets_iphra_v2$choices)
head(protocol$tools$tool_kii_markets_iphra_v2$settings)

protocol$tools$tool_kii_markets_iphra_v2$filter_survey_by_indicator(indicator_codes = c(protocol$framework$adjusted_schema$indicator_code))

View(protocol$tools$tool_kii_markets_iphra_v2$revised_survey)
head(protocol$tools$tool_kii_markets_iphra_v2$revised_choices)
head(protocol$tools$tool_kii_markets_iphra_v2$revised_settings)

protocol$tools$tool_kii_markets_iphra_v2$validate() # this part still needs heavy development, breaking in one of the checks right now
protocol$tools$tool_kii_markets_iphra_v2$get_validation_errors() # this part still needs heavy development

# Nutrition Facility KII Tool ####

protocol$add_tools(tool_name = "tool_kii_nutrition_service_provider_iphra_v2")

head(protocol$tools$tool_kii_nutrition_service_provider_iphra_v2$survey)
head(protocol$tools$tool_kii_nutrition_service_provider_iphra_v2$choices)
head(protocol$tools$tool_kii_nutrition_service_provider_iphra_v2$settings)

protocol$tools$tool_kii_nutrition_service_provider_iphra_v2$filter_survey_by_indicator(indicator_codes = c(protocol$framework$adjusted_schema$indicator_code))

View(protocol$tools$tool_kii_nutrition_service_provider_iphra_v2$revised_survey)
head(protocol$tools$tool_kii_nutrition_service_provider_iphra_v2$revised_choices)
head(protocol$tools$tool_kii_nutrition_service_provider_iphra_v2$revised_settings)

protocol$tools$tool_kii_nutrition_service_provider_iphra_v2$validate() # this part still needs heavy development, breaking in one of the checks right now
protocol$tools$tool_kii_nutrition_service_provider_iphra_v2$get_validation_errors() # this part still needs heavy development


# Protocol Coherence Checks ####

(protocol$diagnose_coherence())
(protocol$issues_coherence)


# Test 3: Generate Word Report from IPHRAProtocol ####

# generate_reach_tor() uses officer + flextable (bundled template used when present).
# The standalone wrapper generate_protocol_report() dispatches to the method.

# -- 3a: Generate report with no tools (tools section shows placeholder text) --

protocol$metadata$pilot_date <- "2025-04-15"
protocol$metadata$data_start_date <- "2025-05-01"
protocol$metadata$data_end_date <- "2025-05-15"
protocol$metadata$analysis_date <- "2025-05-20"
protocol$metadata$data_validation_date <- "2025-05-18"
protocol$metadata$prelim_presentation_date <- "2025-05-25"
protocol$metadata$output_validation_date <- "2025-05-30"
protocol$metadata$output_published_date <- "2025-06-05"
protocol$metadata$final_presentation_date <- "2025-06-10"
protocol$metadata$pop.idpinformal <- TRUE
protocol$metadata$data_management_platform <- "IMPACT"

protocol$metadata$popsize_known_strata_unit <- TRUE

protocol$secondary_data <- list(
    "101" = "ACLED",
    "104" = "UNHCR"
)


# protocol$ <- "Internal"

# report_no_tools <- tempfile(fileext = ".docx")
protocol$generate_reach_tor(output_file = "report_no_tools.docx")
