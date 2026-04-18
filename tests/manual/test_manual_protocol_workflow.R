#!/usr/bin/env Rscript

#' Manual Test Script for Protocol and Tool Workflow
#'
#' This script demonstrates the complete workflow for the Protocol class
#' hierarchy (Protocol base class and SurveyProtocol subclass) together with
#' the Tool class hierarchy (Tool, HouseholdTool, KeyInformantTool, ObservationTool).
#'
#' Steps covered:
#'  1.  Create a base Protocol object (no sampling)
#'  2.  Define objectives and manage them on the base Protocol
#'  3.  Generate a Word report from the base Protocol (no tools yet, then with tools)
#'  4.  Create a SurveyProtocol object (with sampling support)
#'  5.  Define strata and sample sizes on the SurveyProtocol
#'  6.  Build and validate a sampling frame
#'  7.  Draw samples from the sampling frame
#'  8.  Instantiate and inspect Tool subclasses (Household, KII, Observation)
#'  9.  Modify tools: filter by modules, update choice lists, change language
#' 10.  Attach tools to the SurveyProtocol
#' 11.  Finalise and inspect the SurveyProtocol (export, save/load)
#' 12.  Generate a Word report from the SurveyProtocol
#'
#' Author: Auto-generated for iphRa protocol / tool manual testing
#' Date: 2025-01-01 (revised 2025-04-18)

rm(list = ls())

# library(phr)
devtools::load_all()
library(tibble)
library(dplyr)


# =============================================================================
# SETUP: Dummy Assessment Information
# =============================================================================

assessment_title <- "Multi-Sector Humanitarian Needs Assessment – Dummy Country"
country_name     <- "Dummy Country"
month_year       <- "March 2025"


# =============================================================================
# Test 1: Create a base Protocol Object ####
# =============================================================================
# Use create_protocol() factory (or Protocol$new() directly).
# The base Protocol handles objectives, tools, and report generation only.
# Use SurveyProtocol for anything sampling-related (Tests 4-7 below).

protocol <- create_protocol(
  assessment_title = assessment_title,
  country_name     = country_name,
  month_year       = month_year
)
stopifnot(inherits(protocol, "Protocol"))
stopifnot(!inherits(protocol, "SurveyProtocol"))

# Inspect initial state
protocol$metadata
protocol$get_protocol_summary()

# Inspect loaded objective schema
nrow(protocol$objective_schema)
names(protocol$objective_schema)
head(protocol$objective_schema)

# Validate the objective schema
validate_objective_schema(protocol$objective_schema)


# =============================================================================
# Test 2: Define Objectives ####
# =============================================================================

# -- 2a: Create objectives using create_objective() --
# data_source distinguishes primary vs secondary

obj_fsl_1 <- create_objective(
  sector          = "FSL",
  pillar          = "Food Security",
  sub_pillar      = "Food Access",
  short_objective = "P-FSL-01",
  text_objective  = "Estimate the prevalence of food insecurity among assessed households using HDDS and FCS indicators",
  data_source     = "primary"
)

obj_wash_1 <- create_objective(
  sector          = "WASH",
  pillar          = "Water",
  sub_pillar      = "Water Access",
  short_objective = "P-WASH-01",
  text_objective  = "Assess access to safe drinking water and sanitation facilities at household level",
  data_source     = "primary"
)

obj_health_1 <- create_objective(
  sector          = "Health",
  pillar          = "Child Health",
  sub_pillar      = "Morbidity",
  short_objective = "P-HEALTH-01",
  text_objective  = "Measure the prevalence of acute respiratory illness and malaria among children under 5",
  data_source     = "primary"
)

# -- 2b: Create secondary objectives --

obj_fsl_2 <- create_objective(
  sector          = "FSL",
  pillar          = "Food Security",
  sub_pillar      = "Coping Strategies",
  short_objective = "S-FSL-01",
  text_objective  = "Understand the key coping strategies adopted by households facing food gaps",
  data_source     = "secondary"
)

obj_shelter_1 <- create_objective(
  sector          = "Shelter",
  pillar          = "Shelter Quality",
  sub_pillar      = "Structural Adequacy",
  short_objective = "S-SHELTER-01",
  text_objective  = "Describe the quality and adequacy of shelter conditions across assessed strata",
  data_source     = "secondary"
)

# -- 2c: Set objectives on the Protocol using set_objectives() --
# Accepts a flat list; automatically nests by sector/pillar/sub_pillar/data_source

all_objs <- list(obj_fsl_1, obj_wash_1, obj_health_1, obj_fsl_2, obj_shelter_1)
protocol$set_objectives(all_objs)

# Or add individual objectives with add_objective()
extra_obj <- create_objective(
  sector          = "Nutrition",
  pillar          = "Child Nutrition",
  sub_pillar      = "Wasting",
  short_objective = "P-NUT-01",
  text_objective  = "Estimate the prevalence of global acute malnutrition among children 6-59 months",
  data_source     = "primary"
)
protocol$add_objective(extra_obj)

# Inspect objectives (nested structure)
protocol$objectives            # nested list
count_objectives(protocol$objectives)  # total count

# Flatten to a list of objectives
flat_objs <- flatten_objectives(protocol$objectives)
length(flat_objs)

# Validate objectives
validate_objectives(protocol$objectives)

# Summarise objectives
print_objectives_summary(protocol$objectives)

# Convert to data frame
objectives_to_df(protocol$objectives)

# Filter by sector
get_objectives_by_sector(protocol$objectives, "FSL")

# Filter by data source
get_objectives_by_data_source(protocol$objectives, "secondary")

# Remove an objective
protocol$remove_objective("P-NUT-01")
count_objectives(protocol$objectives)  # should be 5 again


# =============================================================================
# Test 3: Generate Word Report from base Protocol ####
# =============================================================================
# generate_report() uses officer + flextable (bundled template used when present).
# The standalone wrapper generate_protocol_report() dispatches to the method.

# -- 3a: Generate report with no tools (tools section shows placeholder text) --

report_no_tools <- tempfile(fileext = ".docx")
protocol$generate_report(output_file = report_no_tools)
stopifnot(file.exists(report_no_tools))
cat("Base protocol report (no tools) written to:", report_no_tools, "\n")

# -- 3b: Add a couple of list-based tools (no XLSForm backing) to the base protocol --
#         and verify they appear in the report

protocol$add_tools(tool_type = "household",    tool_name = "Main Household Survey")
protocol$add_tools(tool_type = "key_informant", tool_name = "Community KII")

report_with_tools <- tempfile(fileext = ".docx")
generate_protocol_report(protocol, output_file = report_with_tools)
stopifnot(file.exists(report_with_tools))
cat("Base protocol report (with tools) written to:", report_with_tools, "\n")

# Confirm summary reflects tools
base_summary <- protocol$get_protocol_summary()
base_summary$num_tools    # 2
base_summary$num_strata   # NULL — not a SurveyProtocol
stopifnot(is.null(base_summary$num_strata))

# Remove the dummy tools so we start fresh on the SurveyProtocol below
protocol$tools <- list()


# =============================================================================
# Test 4: Create a SurveyProtocol Object ####
# =============================================================================
# SurveyProtocol inherits all Protocol capabilities and adds sampling support.

survey_protocol <- create_survey_protocol(
  assessment_title = assessment_title,
  country_name     = country_name,
  month_year       = month_year
)
stopifnot(inherits(survey_protocol, "SurveyProtocol"))
stopifnot(inherits(survey_protocol, "Protocol"))

# Copy the objectives from the base protocol to the survey protocol
survey_protocol$set_objectives(flatten_objectives(protocol$objectives))

# Add target strata to metadata
survey_protocol$add_target_stratum("strata_A", "Urban North")
survey_protocol$add_target_stratum("strata_B", "Peri-Urban East")
survey_protocol$add_target_stratum("strata_C", "Rural South")

survey_protocol$metadata$target_strata


# =============================================================================
# Test 5: Define Strata and Sample Sizes ####
# =============================================================================

# -- 5a: Add strata to the master sample table --
# ind_indicator and mort_indicator are required columns

survey_protocol$add_stratum(
  stratum_id        = "strata_A",
  stratum_name      = "Urban North",
  population_size   = 45000,
  pop_design_effect = 1.5,
  pop_precision     = 0.05,
  ind_indicator     = "wasting_prevalence",
  mort_indicator    = "crude_death_rate",
  sampling_method   = "proportional"
)

survey_protocol$add_stratum(
  stratum_id        = "strata_B",
  stratum_name      = "Peri-Urban East",
  population_size   = 28000,
  pop_design_effect = 1.8,
  pop_precision     = 0.05,
  ind_indicator     = "wasting_prevalence",
  mort_indicator    = "crude_death_rate",
  sampling_method   = "proportional"
)

survey_protocol$add_stratum(
  stratum_id        = "strata_C",
  stratum_name      = "Rural South",
  population_size   = 17000,
  pop_design_effect = 2.0,
  pop_precision     = 0.07,
  ind_indicator     = "wasting_prevalence",
  mort_indicator    = "crude_death_rate",
  sampling_method   = "proportional"
)

# Inspect master sample table before sample sizes are calculated
survey_protocol$get_sample_table()
names(survey_protocol$get_sample_table())

# Validate the master strata table structure
strata_validation <- survey_protocol$validate_strata_table()
strata_validation$valid
strata_validation$message

# Also validate via standalone function
validate_strata_table(survey_protocol$sample_table)

# -- 5b: Calculate sample sizes for each stratum --

ss_A <- calculate_sample_size_general(
  expected_proportion = 50,
  desired_precision   = 5,
  non_response_rate   = 10,
  design              = "cluster",
  design_effect       = 1.5,
  fpc                 = TRUE,
  total_population    = 45000,
  confidence_level    = 0.95
)
cat("Stratum A – calculated sample size:", ss_A, "\n")

ss_B <- calculate_sample_size_general(
  expected_proportion = 50,
  desired_precision   = 5,
  non_response_rate   = 10,
  design              = "cluster",
  design_effect       = 1.8,
  fpc                 = TRUE,
  total_population    = 28000,
  confidence_level    = 0.95
)
cat("Stratum B – calculated sample size:", ss_B, "\n")

ss_C <- calculate_sample_size_general(
  expected_proportion = 50,
  desired_precision   = 7,
  non_response_rate   = 10,
  design              = "cluster",
  design_effect       = 2.0,
  fpc                 = TRUE,
  total_population    = 17000,
  confidence_level    = 0.95
)
cat("Stratum C – calculated sample size:", ss_C, "\n")

# -- 5c: Individual-level calculation for nutrition screening --

ss_nut <- calculate_sample_size_individual(
  expected_proportion    = 15,
  desired_precision      = 5,
  non_response_rate      = 10,
  design                 = "cluster",
  design_effect          = 1.5,
  average_household_size = 5.2,
  sub_population_percent = 20,
  confidence_level       = 0.95
)
cat("Nutrition screening – individuals needed:", ss_nut$sample_size_individuals, "\n")
cat("Nutrition screening – households needed: ", ss_nut$sample_size_households,  "\n")

# -- 5d: Write calculated sample sizes back into the sample table --

survey_protocol$sample_table$pop_result_dummy[survey_protocol$sample_table$stratum_id == "strata_A"] <- ss_A
survey_protocol$sample_table$pop_result_dummy[survey_protocol$sample_table$stratum_id == "strata_B"] <- ss_B
survey_protocol$sample_table$pop_result_dummy[survey_protocol$sample_table$stratum_id == "strata_C"] <- ss_C

survey_protocol$get_sample_table()
cat("Total planned sample size:", sum(survey_protocol$sample_table$pop_result_dummy, na.rm = TRUE), "\n")

# Summary includes sampling fields for SurveyProtocol
sp_summary <- survey_protocol$get_protocol_summary()
stopifnot("num_strata" %in% names(sp_summary))
stopifnot(sp_summary$num_strata == 3L)


# =============================================================================
# Test 6: Build and Validate a Sampling Frame ####
# =============================================================================

# -- 6a: Generate a dummy sampling frame (villages / enumeration areas) --

set.seed(42)

make_villages <- function(stratum_id, n_villages, pop_range) {
  tibble::tibble(
    psu             = paste0(stratum_id, "_v", seq_len(n_villages)),
    stratum         = stratum_id,
    village_name    = paste0(stratum_id, "_Village_", seq_len(n_villages)),
    population_size = sample(pop_range[1]:pop_range[2], n_villages, replace = TRUE)
  )
}

frame_A <- make_villages("strata_A", n_villages = 60, pop_range = c(400, 1200))
frame_B <- make_villages("strata_B", n_villages = 45, pop_range = c(250, 800))
frame_C <- make_villages("strata_C", n_villages = 30, pop_range = c(100, 500))

sampling_frame <- dplyr::bind_rows(frame_A, frame_B, frame_C)

cat("Sampling frame: ", nrow(sampling_frame), "villages across",
    length(unique(sampling_frame$stratum)), "strata\n")
head(sampling_frame)

# -- 6b: Validate the sampling frame using utility function --

frame_validation <- validate_sampling_frame(sampling_frame)
frame_validation$valid
frame_validation$message
frame_validation$summary

# -- 6c: Set sampling frame on the SurveyProtocol --

survey_protocol$set_sampling_frame(sampling_frame)

# Check protocol issues after setting frame (strata alignment)
survey_protocol$get_issues()


# =============================================================================
# Test 7: Draw Sample ####
# =============================================================================

# draw_sample() reads Sampling_Method, Final_HH_Sample_Size, n_psu,
# n_clusters, cluster_size, and n_sites from the strata table.

# -- 7a: Set Final_HH_Sample_Size for each stratum --

survey_protocol$sample_table$Final_HH_Sample_Size[survey_protocol$sample_table$stratum_id == "strata_A"] <- ss_A
survey_protocol$sample_table$Final_HH_Sample_Size[survey_protocol$sample_table$stratum_id == "strata_B"] <- ss_B
survey_protocol$sample_table$Final_HH_Sample_Size[survey_protocol$sample_table$stratum_id == "strata_C"] <- ss_C

# -- 7b: Draw using proportional method (reads from Sampling_Method column) --

survey_protocol$draw_sample(seed = 789)
nrow(survey_protocol$drawn_sample)       # selected PSUs
nrow(survey_protocol$drawn_sample_full)  # full frame with sampled_psu column
head(survey_protocol$drawn_sample[, c("psu", "stratum", "population_size", "sampled_psu", "allocated_sample")])

# -- 7c: Override to pps_cluster --

survey_protocol$sample_table$Sampling_Method <- "pps_cluster"
survey_protocol$sample_table$n_clusters      <- 30
survey_protocol$sample_table$cluster_size    <- 20

survey_protocol$draw_sample(seed = 456)
nrow(survey_protocol$drawn_sample)
head(survey_protocol$drawn_sample[, c("psu", "stratum", "population_size", "sampled_psu", "allocated_sample")])

# -- 7d: Purposive sampling — no PSUs auto-selected --

survey_protocol$sample_table$Sampling_Method <- "purposive"
survey_protocol$draw_sample(seed = 42)
nrow(survey_protocol$drawn_sample)                              # 0
sum(is.na(survey_protocol$drawn_sample_full$sampled_psu))       # all NA

# Restore proportional for remaining tests
survey_protocol$sample_table$Sampling_Method <- "proportional"
survey_protocol$draw_sample(seed = 789)

# -- 7e: Pass an explicit frame and strata_table --

custom_frame  <- sampling_frame[sampling_frame$stratum == "strata_A", ]
custom_frame$inclusion <- TRUE
custom_strata <- survey_protocol$sample_table[survey_protocol$sample_table$stratum_id == "strata_A", ]
custom_strata$Sampling_Method <- "srs"
custom_strata$n_psu            <- 15

survey_protocol$draw_sample(frame = custom_frame, strata_table = custom_strata, seed = 111)
nrow(survey_protocol$drawn_sample)

# Restore full proportional draw for report generation later
survey_protocol$draw_sample(seed = 789)

# -- 7f: Standalone PSU utility functions --

sample_result_A <- draw_sample_psu_pps_cluster(
  frame        = frame_A,
  n_clusters   = 12,
  cluster_size = 20,
  seed         = 789
)
nrow(sample_result_A)
sum(!is.na(sample_result_A$sampled_psu))
head(sample_result_A[!is.na(sample_result_A$sampled_psu), ])

purposive_result <- draw_sample_psu_purposive(frame_A)
sum(is.na(purposive_result$sampled_psu))  # all NA

sample_srs_A <- draw_sample_srs(frame_A, n = ss_A, seed = 789)
head(sample_srs_A)
attr(sample_srs_A, "sampling_method")
attr(sample_srs_A, "n_drawn")


# =============================================================================
# Test 8: Household Tool ####
# =============================================================================

# -- 8a: Instantiate via SurveyProtocol$add_tools() --

survey_protocol$add_tools(tool_type = "household", tool_name = "Main Household Survey")

length(survey_protocol$tools)
survey_protocol$tools[[1]]$get_name()
survey_protocol$tools[[1]]$get_tool_type()

# -- 8b: Directly instantiate a HouseholdTool --

hh_tool <- HouseholdTool$new(name = "Household Survey Tool – Strata A & B")
hh_tool$get_name()
hh_tool$get_tool_type()
hh_tool$has_roster()

master_survey  <- hh_tool$get_master_survey()
master_choices <- hh_tool$get_master_choices()
nrow(master_survey)
nrow(master_choices)
head(master_survey)

# -- 8c: Inspect settings --

hh_settings <- hh_tool$get_settings()
hh_settings

# -- 8d: Change default language --

hh_tool$change_default_language("French")
hh_tool$get_settings()

hh_tool$change_default_language("English")
hh_tool$get_settings()

# -- 8e: Filter survey by modules --

head(master_survey[[1]], 30)
unique(master_survey[[1]])

hh_tool$filter_survey_by_modules(modules = c("core", "FSL", "WASH"))

modified_survey <- hh_tool$get_modified_survey()
nrow(modified_survey)
head(modified_survey)

# -- 8f: Filter choices to match modified survey --

hh_tool$filter_choices_from_survey()

modified_choices <- hh_tool$get_modified_choices()
nrow(modified_choices)
unique(modified_choices$list_name)

# -- 8g: Validate tool --

is_valid <- hh_tool$validate_tool()
cat("Household tool valid:", is_valid, "\n")
hh_tool$get_validation_errors()

# -- 8h: Update a choice list with custom values --

new_admin1_choices <- data.frame(
  name  = c("north", "east", "south", "west"),
  label = c("North Region", "East Region", "South Region", "West Region"),
  stringsAsFactors = FALSE
)

hh_tool$update_choice_list(
  list_name   = "admin1",
  new_choices = new_admin1_choices
)

updated_choices <- hh_tool$get_modified_choices()
updated_choices[updated_choices$list_name == "admin1", ]

# -- 8i: Inspect roster groups --

roster_groups <- hh_tool$get_roster_groups()
nrow(roster_groups)
head(roster_groups)

if (nrow(roster_groups) > 0) {
  first_roster <- roster_groups$name[1]
  cat("First roster group:", first_roster, "\n")
  roster_qs <- hh_tool$get_roster_questions(first_roster)
  head(roster_qs)
}

# -- 8j: Add / update / remove a custom question --

hh_tool$add_question(
  type     = "integer",
  name     = "hh_monthly_income",
  label    = "Estimated monthly household income (USD)",
  required = FALSE
)

hh_tool$has_question("hh_monthly_income")
hh_tool$get_question("hh_monthly_income")

hh_tool$update_question("hh_monthly_income", label = "Monthly household income in USD (estimated)")
hh_tool$get_question("hh_monthly_income")

hh_tool$remove_question("hh_monthly_income")
hh_tool$has_question("hh_monthly_income")

# -- 8k: Update settings --

hh_tool$update_settings("form_title", "IPHRA Household Survey – Dummy Country 2025")
hh_tool$update_settings("version",    "2025-03-01")
hh_tool$get_settings()

# -- 8l: Set selected indicators --

hh_tool$set_selected_indicators(c("hdds_score", "fcs_score", "water_source"))
hh_tool$get_selected_indicators()


# =============================================================================
# Test 9: Key Informant Interview (KII) Tool ####
# =============================================================================

# -- 9a: Instantiate via add_tools() --

survey_protocol$add_tools(tool_type = "key_informant", tool_name = "Community KII")

survey_protocol$tools[[2]]$get_name()
survey_protocol$tools[[2]]$get_tool_type()

# -- 9b: Directly instantiate a KeyInformantTool --

kii_tool <- KeyInformantTool$new(
  name     = "Health Facility KII",
  kii_type = "health"
)

kii_tool$get_name()
kii_tool$get_tool_type()

kii_survey  <- kii_tool$get_master_survey()
kii_choices <- kii_tool$get_master_choices()
nrow(kii_survey)
nrow(kii_choices)
head(kii_survey)

kii_valid <- kii_tool$validate_tool()
cat("KII tool valid:", kii_valid, "\n")
kii_tool$get_validation_errors()

kii_tool$add_question(
  type     = "text",
  name     = "facility_name",
  label    = "Name of the health facility",
  required = TRUE
)

kii_tool$has_question("facility_name")
kii_tool$get_question("facility_name")


# =============================================================================
# Test 10: Observation and Generic Tools ####
# =============================================================================

# -- 10a: Observation Tool --

survey_protocol$add_tools(tool_type = "observation", tool_name = "Water Point Observation")

survey_protocol$tools[[3]]$get_name()
survey_protocol$tools[[3]]$get_tool_type()

obs_tool <- ObservationTool$new(
  name             = "Sanitation Facility Observation",
  observation_type = "latrine"
)

obs_tool$get_name()
obs_tool$get_tool_type()

obs_survey  <- obs_tool$get_master_survey()
obs_choices <- obs_tool$get_master_choices()
nrow(obs_survey)
nrow(obs_choices)

obs_valid <- obs_tool$validate_tool()
cat("Observation tool valid:", obs_valid, "\n")
obs_tool$get_validation_errors()

obs_tool$add_question(
  type     = "select_one yes_no",
  name     = "handwashing_station_present",
  label    = "Is a handwashing station present at the facility?"
)

obs_tool$has_question("handwashing_station_present")

# -- 10b: Generic Tool --

survey_protocol$add_tools(tool_type = "generic", tool_name = "Market Assessment Form")

survey_protocol$tools[[4]]$get_name()
survey_protocol$tools[[4]]$get_tool_type()

generic_tool <- Tool$new(name = "Focus Group Discussion Guide")
generic_tool$get_tool_type()

generic_tool$add_question(type = "text",    name = "community_name",    label = "Name of the community")
generic_tool$add_question(type = "integer", name = "participants_count", label = "Number of participants")
generic_tool$add_question(type = "select_one yes_no", name = "consent_obtained", label = "Was group consent obtained?")

nrow(generic_tool$get_survey())
generic_tool$has_question("community_name")

generic_valid <- generic_tool$validate_tool()
cat("Generic tool valid:", generic_valid, "\n")
generic_tool$get_validation_errors()


# =============================================================================
# Test 11: Finalise and Inspect the SurveyProtocol ####
# =============================================================================

# -- 11a: Check issues --

survey_protocol$get_issues()

# -- 11b: Summary includes sampling fields --

sp_full_summary <- survey_protocol$get_protocol_summary()
sp_full_summary
sp_full_summary$num_objectives
sp_full_summary$num_strata        # SurveyProtocol-only field
sp_full_summary$total_sample_size

# -- 11c: Export full protocol --

exported <- survey_protocol$export_protocol()

names(exported)
exported$metadata
exported$summary

count_objectives(exported$objectives)
objectives_to_df(exported$objectives)

nrow(exported$objective_schema)
names(exported$objective_schema)

# Sampling fields present in export
exported$sample_table
names(exported$sample_table)
nrow(exported$drawn_sample)
nrow(exported$drawn_sample_full)

# Tools
length(exported$tools)
sapply(exported$tools, function(t) t$get_name())

# -- 11d: print_protocol_summary() works for both classes --

print_protocol_summary(protocol)          # base Protocol (no num_strata)
print_protocol_summary(survey_protocol)   # SurveyProtocol (num_strata = 3)

# -- 11e: Save and reload via RDS --

tmp_file <- tempfile(fileext = ".rds")
save_protocol(survey_protocol, file = tmp_file)

reloaded <- load_protocol(tmp_file)
names(reloaded)
reloaded$metadata$assessment_title
reloaded$summary

# -- 11f: restore_protocol() auto-selects SurveyProtocol when sampling data present --

restored_sp <- restore_protocol(exported)
stopifnot(inherits(restored_sp, "SurveyProtocol"))

restored_base <- restore_protocol(protocol$export_protocol())
stopifnot(inherits(restored_base, "Protocol"))
stopifnot(!inherits(restored_base, "SurveyProtocol"))


# =============================================================================
# Test 12: Report Generation ####
# =============================================================================
# generate_report() / generate_protocol_report() write a .docx file.
# base Protocol: metadata + objectives + tools (skipped if empty)
# SurveyProtocol: adds Sampling Design section (strata table + selected PSUs)

# -- 12a: SurveyProtocol report after draw_sample() was called --

report_survey_path <- tempfile(fileext = ".docx")
survey_protocol$generate_report(output_file = report_survey_path)
stopifnot(file.exists(report_survey_path))
cat("SurveyProtocol report written to:", report_survey_path, "\n")

# Using the standalone wrapper
report_survey_path2 <- tempfile(fileext = ".docx")
generate_protocol_report(survey_protocol, output_file = report_survey_path2)
stopifnot(file.exists(report_survey_path2))
cat("SurveyProtocol report (wrapper) written to:", report_survey_path2, "\n")

# -- 12b: SurveyProtocol report when no sample has been drawn yet --

sp_no_draw <- create_survey_protocol(
  assessment_title = "Draft Survey – No Sample",
  country_name     = country_name,
  month_year       = "April 2026"
)
sp_no_draw$add_stratum(
  stratum_id      = "pilot",
  stratum_name    = "Pilot Area",
  population_size = 5000,
  ind_indicator   = "wasting_prevalence",
  mort_indicator  = "crude_death_rate",
  sampling_method = "srs",
  n_psu           = 10
)
report_no_draw <- tempfile(fileext = ".docx")
sp_no_draw$generate_report(output_file = report_no_draw)
stopifnot(file.exists(report_no_draw))
cat("SurveyProtocol report (no draw) written to:", report_no_draw, "\n")

# -- 12c: Base Protocol report (no tools) --

base_only <- create_protocol(
  assessment_title = "Secondary Data Review",
  country_name     = "Country X",
  month_year       = "April 2025"
)
base_only$add_objective(create_objective(
  sector          = "General",
  pillar          = "Context",
  sub_pillar      = "Background",
  short_objective = "GEN-01",
  text_objective  = "Review secondary data on humanitarian conditions"
))

report_base_path <- tempfile(fileext = ".docx")
generate_protocol_report(base_only, output_file = report_base_path)
stopifnot(file.exists(report_base_path))
cat("Base Protocol report written to:", report_base_path, "\n")

# -- 12d: validate_protocol() works on both classes --

validate_protocol(protocol)
validate_protocol(survey_protocol)

cat("\n=== Protocol and Tool workflow test completed successfully ===\n")
