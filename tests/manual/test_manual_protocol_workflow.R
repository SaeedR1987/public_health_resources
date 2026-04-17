#!/usr/bin/env Rscript

#' Manual Test Script for Protocol and Tool Workflow
#'
#' This script demonstrates the complete workflow for the Protocol class and
#' the Tool class hierarchy (Tool, HouseholdTool, KeyInformantTool, ObservationTool).
#'
#' Steps covered:
#'  1. Create dummy objectives and attach to a Protocol using the nested structure
#'  2. Define strata and sample sizes using dummy population data
#'  3. Build a dummy sampling frame and validate it
#'  4. Calculate sample sizes with different methods
#'  5. Draw samples from the sampling frame
#'  6. Instantiate and inspect Tool subclasses (Household, KII, Observation)
#'  7. Modify tools: filter by modules, update choice lists, change language
#'  8. Validate tools
#'  9. Attach tools to the Protocol
#' 10. Export and inspect the completed Protocol
#'
#' Author: Auto-generated for iphRa protocol / tool manual testing
#' Date: 2025-01-01

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
# Test 1: Create a Protocol Object ####
# =============================================================================

protocol <- Protocol$new(
  assessment_title = assessment_title,
  country_name     = country_name,
  month_year       = month_year
)

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
# data_source is used to distinguish primary vs secondary (replaces the 'type' param)

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
# Test 3: Define Strata and Sample Sizes ####
# =============================================================================

# -- 3a: Add target strata to protocol metadata --

protocol$add_target_stratum("strata_A", "Urban North")
protocol$add_target_stratum("strata_B", "Peri-Urban East")
protocol$add_target_stratum("strata_C", "Rural South")

protocol$metadata$target_strata

# -- 3b: Add strata to the master sample table with population parameters --
# ind_indicator and mort_indicator are now required columns

protocol$add_stratum(
  stratum_id              = "strata_A",
  stratum_name            = "Urban North",
  population_size         = 45000,
  pop_design_effect       = 1.5,
  pop_precision           = 0.05,
  ind_indicator           = "wasting_prevalence",
  mort_indicator          = "crude_death_rate",
  sampling_method         = "proportional"
)

protocol$add_stratum(
  stratum_id              = "strata_B",
  stratum_name            = "Peri-Urban East",
  population_size         = 28000,
  pop_design_effect       = 1.8,
  pop_precision           = 0.05,
  ind_indicator           = "wasting_prevalence",
  mort_indicator          = "crude_death_rate",
  sampling_method         = "proportional"
)

protocol$add_stratum(
  stratum_id              = "strata_C",
  stratum_name            = "Rural South",
  population_size         = 17000,
  pop_design_effect       = 2.0,
  pop_precision           = 0.07,
  ind_indicator           = "wasting_prevalence",
  mort_indicator          = "crude_death_rate",
  sampling_method         = "proportional"
)

# Inspect master sample table before sample sizes are calculated
protocol$get_sample_table()
names(protocol$get_sample_table())

# Validate the master strata table structure (now requires ind_indicator and mort_indicator)
strata_validation <- protocol$validate_strata_table()
strata_validation$valid
strata_validation$message

# Also validate via standalone function
validate_strata_table(protocol$sample_table)

# -- 3c: Calculate sample sizes for each stratum using utility functions --

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

# -- 3d: Also try the individual-level calculation for nutrition screening --

ss_nut <- calculate_sample_size_individual(
  expected_proportion    = 15,
  desired_precision      = 5,
  non_response_rate      = 10,
  design                 = "cluster",
  design_effect          = 1.5,
  average_household_size = 5.2,
  sub_population_percent = 20,  # children under 5 ~20% of population
  confidence_level       = 0.95
)
cat("Nutrition screening – individuals needed:", ss_nut$sample_size_individuals, "\n")
cat("Nutrition screening – households needed: ", ss_nut$sample_size_households,  "\n")

# -- 3e: Write calculated sample sizes back into the sample table --

protocol$sample_table$pop_result_dummy[protocol$sample_table$stratum_id == "strata_A"] <- ss_A
protocol$sample_table$pop_result_dummy[protocol$sample_table$stratum_id == "strata_B"] <- ss_B
protocol$sample_table$pop_result_dummy[protocol$sample_table$stratum_id == "strata_C"] <- ss_C

# Confirm sample table
protocol$get_sample_table()
cat("Total planned sample size:", sum(protocol$sample_table$pop_result_dummy, na.rm = TRUE), "\n")


# =============================================================================
# Test 4: Build and Validate a Sampling Frame ####
# =============================================================================

# -- 4a: Generate a dummy sampling frame (villages / enumeration areas) --

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

# -- 4b: Validate the sampling frame using utility function --

frame_validation <- validate_sampling_frame(sampling_frame)
frame_validation$valid
frame_validation$message
frame_validation$summary

# -- 4c: Set sampling frame on the Protocol --

protocol$set_sampling_frame(sampling_frame)

# Check protocol issues after setting frame (strata alignment)
protocol$get_issues()


# =============================================================================
# Test 5: Draw Sample ####
# =============================================================================

# The new draw_sample() API reads Sampling_Method, Final_HH_Sample_Size,
# n_psu, n_clusters, cluster_size, and n_sites directly from the strata table.

# -- 5a: Update sample table with sampling parameters before drawing --

# Set Final_HH_Sample_Size for each stratum (used by draw_sample)
protocol$sample_table$Final_HH_Sample_Size[protocol$sample_table$stratum_id == "strata_A"] <- ss_A
protocol$sample_table$Final_HH_Sample_Size[protocol$sample_table$stratum_id == "strata_B"] <- ss_B
protocol$sample_table$Final_HH_Sample_Size[protocol$sample_table$stratum_id == "strata_C"] <- ss_C

# -- 5b: Draw using proportional method (reads method from Sampling_Method column) --
# All three strata already have Sampling_Method = "proportional"

protocol$draw_sample(seed = 789)
nrow(protocol$drawn_sample)       # selected PSUs
nrow(protocol$drawn_sample_full)  # full frame with sampled_psu column
head(protocol$drawn_sample[, c("psu", "stratum", "population_size", "sampled_psu", "allocated_sample")])

# -- 5c: Override method to pps_cluster by updating the strata table --
# Set n_clusters and cluster_size in the table first

protocol$sample_table$Sampling_Method <- "pps_cluster"
protocol$sample_table$n_clusters      <- 30
protocol$sample_table$cluster_size    <- 20

protocol$draw_sample(seed = 456)
nrow(protocol$drawn_sample)
head(protocol$drawn_sample[, c("psu", "stratum", "population_size", "sampled_psu", "allocated_sample")])

# -- 5d: Purposive sampling — sampled_psu and allocated_sample are all NA --
# (User manually designates selected PSUs after this step.)

protocol$sample_table$Sampling_Method <- "purposive"
protocol$draw_sample(seed = 42)
nrow(protocol$drawn_sample)       # 0 — no PSUs auto-selected
sum(is.na(protocol$drawn_sample_full$sampled_psu))  # all NA

# Restore proportional for remaining tests
protocol$sample_table$Sampling_Method <- "proportional"

# -- 5e: Pass an explicit frame and strata_table (without touching protocol fields) --

custom_frame  <- sampling_frame[sampling_frame$stratum == "strata_A", ]
custom_frame$inclusion <- TRUE
custom_strata <- protocol$sample_table[protocol$sample_table$stratum_id == "strata_A", ]
custom_strata$Sampling_Method <- "srs"
custom_strata$n_psu            <- 15

protocol$draw_sample(frame = custom_frame, strata_table = custom_strata, seed = 111)
nrow(protocol$drawn_sample)

# -- 5f: Use standalone PSU utility functions directly --
# draw_sample_psu_* functions return the FULL modified frame (sampled_psu / allocated_sample cols);
# the legacy draw_sample_* helpers (draw_sample_srs, draw_sample_pps, etc.) return only selected rows.

sample_result_A <- draw_sample_psu_pps_cluster(
  frame        = frame_A,
  n_clusters   = 12,
  cluster_size = 20,
  seed         = 789
)
nrow(sample_result_A)
sum(!is.na(sample_result_A$sampled_psu))  # selected PSUs
head(sample_result_A[!is.na(sample_result_A$sampled_psu), ])

# Purposive standalone function
purposive_result <- draw_sample_psu_purposive(frame_A)
sum(is.na(purposive_result$sampled_psu))  # all NA — user fills manually

# Legacy helper: draw_sample_srs() returns only selected rows (not the full frame)
sample_srs_A <- draw_sample_srs(frame_A, n = ss_A, seed = 789)
head(sample_srs_A)
attr(sample_srs_A, "sampling_method")
attr(sample_srs_A, "n_drawn")


# =============================================================================
# Test 6: Household Tool ####
# =============================================================================

# -- 6a: Instantiate via Protocol$add_tools() --

protocol$add_tools(tool_type = "household", tool_name = "Main Household Survey")

# Inspect tools list
length(protocol$tools)
protocol$tools[[1]]$get_name()
protocol$tools[[1]]$get_tool_type()

# -- 6b: Directly instantiate a HouseholdTool --

hh_tool <- HouseholdTool$new(name = "Household Survey Tool – Strata A & B")
hh_tool$get_name()
hh_tool$get_tool_type()
hh_tool$has_roster()

# Inspect master survey and choices
master_survey  <- hh_tool$get_master_survey()
master_choices <- hh_tool$get_master_choices()
nrow(master_survey)
nrow(master_choices)
head(master_survey)

# -- 6c: Inspect settings --

hh_settings <- hh_tool$get_settings()
hh_settings

# -- 6d: Change default language --

hh_tool$change_default_language("French")
hh_tool$get_settings()

hh_tool$change_default_language("English")
hh_tool$get_settings()

# -- 6e: Filter survey by modules --

head(master_survey[[1]], 30)
unique(master_survey[[1]])

hh_tool$filter_survey_by_modules(modules = c("core", "FSL", "WASH"))

modified_survey <- hh_tool$get_modified_survey()
nrow(modified_survey)
head(modified_survey)

# -- 6f: Filter choices to match modified survey --

hh_tool$filter_choices_from_survey()

modified_choices <- hh_tool$get_modified_choices()
nrow(modified_choices)
unique(modified_choices$list_name)

# -- 6g: Validate tool --

is_valid <- hh_tool$validate_tool()
cat("Household tool valid:", is_valid, "\n")
hh_tool$get_validation_errors()

# -- 6h: Update a choice list with custom values --

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

# -- 6i: Inspect roster groups --

roster_groups <- hh_tool$get_roster_groups()
nrow(roster_groups)
head(roster_groups)

# -- 6j: Inspect questions in a specific roster --

if (nrow(roster_groups) > 0) {
  first_roster <- roster_groups$name[1]
  cat("First roster group:", first_roster, "\n")
  roster_qs <- hh_tool$get_roster_questions(first_roster)
  head(roster_qs)
}

# -- 6k: Add a custom question to the tool --

hh_tool$add_question(
  type     = "integer",
  name     = "hh_monthly_income",
  label    = "Estimated monthly household income (USD)",
  required = FALSE
)

hh_tool$has_question("hh_monthly_income")
hh_tool$get_question("hh_monthly_income")

# -- 6l: Update the custom question --

hh_tool$update_question("hh_monthly_income", label = "Monthly household income in USD (estimated)")
hh_tool$get_question("hh_monthly_income")

# -- 6m: Remove the custom question --

hh_tool$remove_question("hh_monthly_income")
hh_tool$has_question("hh_monthly_income")

# -- 6n: Update settings --

hh_tool$update_settings("form_title", "IPHRA Household Survey – Dummy Country 2025")
hh_tool$update_settings("version",    "2025-03-01")
hh_tool$get_settings()

# -- 6o: Set selected indicators --

hh_tool$set_selected_indicators(c("hdds_score", "fcs_score", "water_source"))
hh_tool$get_selected_indicators()


# =============================================================================
# Test 7: Key Informant Interview (KII) Tool ####
# =============================================================================

# -- 7a: Instantiate via Protocol$add_tools() --

protocol$add_tools(tool_type = "key_informant", tool_name = "Community KII")

protocol$tools[[2]]$get_name()
protocol$tools[[2]]$get_tool_type()

# -- 7b: Directly instantiate a KeyInformantTool --

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

# -- 7c: Validate KII tool --

kii_valid <- kii_tool$validate_tool()
cat("KII tool valid:", kii_valid, "\n")
kii_tool$get_validation_errors()

# -- 7d: Add and inspect a custom KII question --

kii_tool$add_question(
  type     = "text",
  name     = "facility_name",
  label    = "Name of the health facility",
  required = TRUE
)

kii_tool$has_question("facility_name")
kii_tool$get_question("facility_name")


# =============================================================================
# Test 8: Observation Tool ####
# =============================================================================

# -- 8a: Instantiate via Protocol$add_tools() --

protocol$add_tools(tool_type = "observation", tool_name = "Water Point Observation")

protocol$tools[[3]]$get_name()
protocol$tools[[3]]$get_tool_type()

# -- 8b: Directly instantiate an ObservationTool --

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
head(obs_survey)

# -- 8c: Validate observation tool --

obs_valid <- obs_tool$validate_tool()
cat("Observation tool valid:", obs_valid, "\n")
obs_tool$get_validation_errors()

# -- 8d: Add a custom observation question --

obs_tool$add_question(
  type     = "select_one yes_no",
  name     = "handwashing_station_present",
  label    = "Is a handwashing station present at the facility?"
)

obs_tool$has_question("handwashing_station_present")
obs_tool$get_question("handwashing_station_present")


# =============================================================================
# Test 9: Generic Tool ####
# =============================================================================

# -- 9a: Instantiate a generic Tool via Protocol$add_tools() --

protocol$add_tools(tool_type = "generic", tool_name = "Market Assessment Form")

protocol$tools[[4]]$get_name()
protocol$tools[[4]]$get_tool_type()

# -- 9b: Directly instantiate a generic Tool and populate it manually --

generic_tool <- Tool$new(name = "Focus Group Discussion Guide")
generic_tool$get_tool_type()

generic_tool$add_question(type = "text",    name = "community_name",    label = "Name of the community")
generic_tool$add_question(type = "integer", name = "participants_count", label = "Number of participants")
generic_tool$add_question(type = "select_one yes_no", name = "consent_obtained", label = "Was group consent obtained?")

nrow(generic_tool$get_survey())
generic_tool$has_question("community_name")
generic_tool$has_question("participants_count")

generic_valid <- generic_tool$validate_tool()
cat("Generic tool valid:", generic_valid, "\n")
generic_tool$get_validation_errors()


# =============================================================================
# Test 10: Finalise and Inspect the Protocol ####
# =============================================================================

# -- 10a: Check protocol issues --

protocol$get_issues()

# -- 10b: Get protocol summary --

summary_out <- protocol$get_protocol_summary()
summary_out
summary_out$num_objectives   # total objectives (replaces num_primary/secondary)

# -- 10c: Export full protocol to a list --

exported <- protocol$export_protocol()

names(exported)
exported$metadata
exported$summary

# Objectives in export (nested structure)
count_objectives(exported$objectives)
objectives_to_df(exported$objectives)

# Objective schema in export
nrow(exported$objective_schema)
names(exported$objective_schema)

# Sample table
exported$sample_table
names(exported$sample_table)

# Drawn sample
nrow(exported$drawn_sample)

# Tools in the protocol
length(exported$tools)
sapply(exported$tools, function(t) t$get_name())

# -- 10d: Save protocol to a temporary RDS file --

tmp_file <- tempfile(fileext = ".rds")
save_protocol(protocol, file = tmp_file)

# Reload and inspect
reloaded <- load_protocol(tmp_file)
names(reloaded)
reloaded$metadata$assessment_title
reloaded$summary

cat("\n=== Protocol and Tool workflow test completed successfully ===\n")
