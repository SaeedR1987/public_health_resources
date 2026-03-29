#!/usr/bin/env Rscript

#' Comprehensive Test Script for Household Data Workflow
#'
#' This script demonstrates the complete workflow for the HouseholdData class:
#' 1. Load sample datasets (good and bad)
#' 2. Create HouseholdData objects
#' 3. Validate data against schema
#' 4. Standardize data types and values
#' 5. Generate cleaning and deletion logs
#' 6. Test post-validation checks
#' 7. Export results
#'
#' Author: Auto-generated for iphRa household schema testing
#' Date: 2024-12-17

rm(list = ls())

# library(iphRa)
devtools::load_all()
library(tibble)
library(dplyr)
source("dev/generate_household_samples.R")


# Setup



# Test 1: Load Good Sample Dataset ####


household_data <- generate_household_dataset(n = 200)
# household_data <- introduce_errors(household_data)

# Generate linked datasets based on household data
hh_roster_data <- generate_hh_roster_dataset(household_data)
# hh_roster_data <- introduce_errors(hh_roster_data)

health_ind_data <- generate_health_ind_dataset(hh_roster_data)
# health_ind_data <- introduce_errors(health_ind_data)

water_count_loop_data <- generate_water_count_loop_dataset(household_data)
# water_count_loop_data <- introduce_errors(water_count_loop_data)

child_nutrition_data <- generate_child_nutrition_dataset(hh_roster_data)
# child_nutrition_data <- introduce_errors(child_nutrition_data)

women_data <- generate_women_dataset(hh_roster_data)
# women_data <- introduce_errors(women_data)

died_member_data <- generate_died_member_dataset(household_data)
# died_member_data <- introduce_errors(died_member_data)

# Create HouseholdData object

good_hh <- HouseholdData$new(
    data = household_data,
    dataset_name = "GoodHouseholdSample",
    metadata = list(
      source = "Generated test data",
      date_created = Sys.Date(),
      purpose = "Schema validation testing"
    )
  )

good_roster <- IndividualData$new(
  data = hh_roster_data,
  dataset_name = "GoodRosterSample",
  metadata = list(
    source = "Generated test data",
    date_created = Sys.Date(),
    purpose = "Schema validation testing"
  )
)

good_water_containers <- WaterContainerData$new(
  data = water_count_loop_data,
  dataset_name = "GoodWaterContainerSample",
  metadata = list(
    source = "Generated test data",
    date_created = Sys.Date(),
    purpose = "Schema validation testing"
  )
)

good_nutrition <- NutritionIndividualData$new(
  data = child_nutrition_data,
  dataset_name = "GoodNutSample",
  metadata = list(
    source = "Generated test data",
    date_created = Sys.Date(),
    purpose = "Schema validation testing"
  )
)

good_health <- HealthIndividualData$new(
  data = health_ind_data,
  dataset_name = "GoodHealthSample",
  metadata = list(
    source = "Generated test data",
    date_created = Sys.Date(),
    purpose = "Schema validation testing"
  )
)

good_deaths <- DeathIndividualData$new(
  data = died_member_data,
  recall_date = "2022-01-01",
  dataset_name = "GoodDeathSample",
  metadata = list(
    source = "Generated test data",
    date_created = Sys.Date(),
    purpose = "Schema validation testing"
  )
)



  # LINKING DATASETS

  good_hh$add_linked_dataset(
    name = "roster",
    data_object = good_roster)

  good_hh$add_linked_dataset(
    name = "water_containers",
    data_object = good_water_containers
  )

  good_hh$add_linked_dataset(
    name = "health",
    data_object = good_health
  )

  good_hh$add_linked_dataset(
    name = "deaths",
    data_object = good_deaths
  )

  good_hh$add_linked_dataset(
    name = "nutrition",
    data_object = good_nutrition
  )


  # CHECK CONTENTS AFTER INITIALIZATION
  good_hh$summary()
  good_hh$variable_map
  good_hh$value_map
  good_hh$variable_label
  good_hh$value_label

  variable_schema <- good_hh$export_variable_schema()
  View(variable_schema)

  dependency_schema <- good_hh$export_dependency_schema()
  View(dependency_schema)

  indicator_schema <- good_hh$export_indicator_schema()
  head(indicator_schema)

  # CHECK VALIDATE
  good_hh$validated
  good_hh$validate()
  good_hh$validated

  # CHECK STANDARDIZE
  good_hh$standardize()
  good_hh$standardized

  # Testing Cleaning Logs
  good_hh$generate_cleaning_log(stage = "standardized", overwrite = TRUE)

  log_cl <- good_hh$cleaning_log$log_df
  head(log_cl, 10)
  log_dl <- good_hh$deletion_log$log_df
  head(log_dl, 10)

  # TEST CLEAN()
  good_hh$clean()

  cleaneddata <- good_hh$get_data("clean")
  View(cleaneddata)
  # table_schema <- good_hh$get_variable_schema() %>% data_schema_to_table()



  # CLEAN

  good_hh$clean()
  good_hh$cleaned
  is.null(good_hh$clean_data)



  standardizeddata <- good_hh$get_data("standardized")

  rawdata <- good_hh$get_data("raw")

  diag <- good_hh$data_diagnostics
  diag
  good_hh$data_diagnose(stage = "standardized")
  View(good_hh$data_diagnostics)

  # Create Data Analytics Object ####

  hh_analytics <- good_hh$generate_data_analytics(stage = "standardized", type = "fsl")

  hh_analytics$data_analysis_plan$log_df

  hh_analytics$run_analysis()
  hh_analytics$run_quality_checks()
  hh_analytics$run_outputs()

  hh_analytics$analysis_results$base
  hh_analytics$analysis_results$survey_design

  hh_analytics$visualizations$survey_design

  hh_analytics$quality_issues_log

  hh_analytics$tables$plausibility$penalty_summary

  hh_analytics$visualizations$survey_design

  View(hh_analytics$analysis_results$survey_design)

  # Create Quality Object ####
#
#   hh_quality <- good_hh$generate_data_quality(stage = "standardized", type = "fsl")
#
#   View(hh_quality$data)
#
#   hh_quality$variable_map
#
#   hh_quality$value_map
#
#   tab <- hh_quality$schema_to_table()
#
#   hh_quality$run_quality_checks()
#
#   hh_quality$tables
#
#   table_quality_penalty_summary(results_df = hh_quality$results_to_table(), show_max_penalty = T, title_name = "FSL Plausibility Report")
#
#   View(hh_quality$results_to_table())
#
#   result <- hh_quality$execute_check(hh_quality$quality_schema[[2]])
#
#   result
#
#   hh_quality$run_outputs()
#
#   hh_quality$tables$plausibility$penalty_summary
#
#   hh_quality$visualizations$enumerator$`fcs_rcsi_hhs_correlogram_enum-enum_id.2`
#
#   hh_quality$visualizations$rcsi_overall
#
#   hh_quality$visualizations$rcsi_by_strata
#
#   # test_plot <- plot_correlogram(hh_quality$data, c("fsl_fcs_score", "fsl_hhs_score"))
#
#   hh_quality$visualizations$fcs_rcsi_hhs_correlogram
#
#   hh_quality$visualizations$fcs_ridge
#
#   hh_quality$visualizations$hhs_ipc_overall
#
#   hh_quality$visualizations
#
#   # CHECKING ANALYSIS OBJECT
#
#   hh_analysis <- good_hh$generate_data_analysis(
#     stage = "standardized",
#     type = "fsl"
#   )
#
#   hh_analysis$analysis_schema
#
#   hh_analysis$data_analysis_plan$log_df
#
#   hh_analysis$survey_design$variables
#
#
#
#   hh_analysis$export_outputs_schema()
#
#   hh_analysis$run_analysis()
#
#   View(hh_analysis$results$survey_design)
#
#   View(hh_analysis$results$base)
#
#   hh_analysis$export_outputs_schema()
#
#   hh_analysis$results
#
#   hh_analysis$run_outputs()
#
#   hh_analysis$visualizations
#
#   hh_analysis$tables
#


# Test 2: Roster Data ####



  good_roster <- IndividualData$new(
    data = hh_roster_data,
    dataset_name = "GoodRosterSample",
    metadata = list(
      source = "Generated test data",
      date_created = Sys.Date(),
      purpose = "Schema validation testing"
    )
  )

  # CHECK CONTENTS AFTER INITIALIZATION
  good_roster$summary()
  good_roster$variable_map
  good_roster$value_map

  # CHECK VALIDATE
  good_roster$validated
  good_roster$validate()
  good_roster$validated

  # CHECK STANDARDIZE
  good_roster$standardize()
  good_roster$standardized

  View(good_roster$data_quality_flags)

  nrow(good_roster$raw_data)

  View(good_roster$standardized_data)

  View(good_roster$standardized_data %>% dplyr::select(starts_with("flag_values")))


  # CHECK CLEANING

  good_roster$generate_cleaning_log(stage = "standardized", overwrite = TRUE)

  log_cl <- good_roster$cleaning_log$log_df

  head(log_cl, 10)

  View(log_cl)

  unique(log_cl$issue)
  unique(good_roster$standardized_data$sex)
  table(good_roster$standardized_data$sex)
  good_roster$value_map$sex

  # CHECKING SCHEMA
  variable_schema <- good_roster$export_variable_schema()
  head(variable_schema)

  dependency_schema <- good_roster$export_dependency_schema()
  head(dependency_schema)
  View(dependency_schema)

  indicator_schema <- good_roster$export_indicator_schema()
  head(indicator_schema)


# Test 3: Water Container Data ####

  good_water_containers <- WaterContainerData$new(
    data = water_count_loop_data,
    dataset_name = "GoodWaterContainerSample",
    metadata = list(
      source = "Generated test data",
      date_created = Sys.Date(),
      purpose = "Schema validation testing"
    )
  )

  # CHECK CONTENTS AFTER INITIALIZATION
  good_water_containers$summary()
  good_water_containers$variable_map
  good_water_containers$value_map

  # CHECK VALIDATE
  good_water_containers$validated
  good_water_containers$validate()
  good_water_containers$validated

  # CHECK STANDARDIZE
  good_water_containers$standardize()
  good_water_containers$standardized

  View(good_water_containers$standardized_data)

  # CHECK CLEANING

  good_water_containers$generate_cleaning_log(stage = "standardized", overwrite = TRUE)

  log_cl <- good_water_containers$cleaning_log$log_df

  head(log_cl, 10)

  View(log_cl)

  # CHECKING SCHEMA
  variable_schema <- good_water_containers$export_variable_schema()
  head(variable_schema)

  dependency_schema <- good_water_containers$export_dependency_schema()
  head(dependency_schema)

  indicator_schema <- good_water_containers$export_indicator_schema()
  head(indicator_schema)



# Test 4: Nutrition Data ####


  good_nutrition <- NutritionIndividualData$new(
    data = child_nutrition_data,
    dataset_name = "GoodNutSample",
    metadata = list(
      source = "Generated test data",
      date_created = Sys.Date(),
      purpose = "Schema validation testing"
    )
  )

  # CHECK CONTENTS AFTER INITIALIZATION
  good_nutrition$summary()
  good_nutrition$variable_map
  good_nutrition$value_map

  # CHECK VALIDATE
  good_nutrition$validated
  good_nutrition$validate()
  good_nutrition$validated

  # CHECK STANDARDIZE
  good_nutrition$standardize()
  good_nutrition$standardized

  View(good_nutrition$standardized_data)


  # CHECK CLEANING

  good_nutrition$generate_cleaning_log(stage = "standardized", overwrite = TRUE)

  log_cl <- good_nutrition$cleaning_log$log_df

  head(log_cl, 10)

  View(log_cl)

  # CHECKING SCHEMA
  variable_schema <- good_nutrition$export_variable_schema()
  head(variable_schema)

  dependency_schema <- good_nutrition$export_dependency_schema()
  head(dependency_schema)

  indicator_schema <- good_nutrition$export_indicator_schema()
  head(indicator_schema)

  diag <- good_nutrition$data_diagnostics
  diag
  good_nutrition$data_diagnose(stage = "raw")
  View(good_nutrition$data_diagnostics)

# Test 5: Health Data ####

  good_health <- HealthIndividualData$new(
    data = health_ind_data,
    dataset_name = "GoodHealthSample",
    metadata = list(
      source = "Generated test data",
      date_created = Sys.Date(),
      purpose = "Schema validation testing"
    )
  )

  # CHECK CONTENTS AFTER INITIALIZATION
  good_health$summary()
  good_health$variable_map
  good_health$value_map

  # CHECK VALIDATE
  good_health$validated
  good_health$validate()
  good_health$validated

  # CHECK STANDARDIZE
  good_health$standardize()
  good_health$standardized

  View(good_health$standardized_data)

  # CHECK CLEANING

  good_health$generate_cleaning_log(stage = "standardized", overwrite = TRUE)

  log_cl <- good_health$cleaning_log$log_df

  head(log_cl, 10)

  View(log_cl)

  # CHECKING SCHEMA
  variable_schema <- good_health$export_variable_schema()
  head(variable_schema)

  dependency_schema <- good_health$export_dependency_schema()
  head(dependency_schema)

  indicator_schema <- good_health$export_indicator_schema()
  head(indicator_schema)

  diag <- good_health$data_diagnostics
  diag
  good_health$data_diagnose(stage = "raw")
  View(good_health$data_diagnostics)

  # Test 6: Deaths Data ####

  good_deaths <- DeathIndividualData$new(
    data = died_member_data,
    recall_date = "2022-01-01",
    dataset_name = "GoodDeathSample",
    metadata = list(
      source = "Generated test data",
      date_created = Sys.Date(),
      purpose = "Schema validation testing"
    )
  )

  # CHECK CONTENTS AFTER INITIALIZATION
  good_deaths$summary()
  good_deaths$variable_map
  good_deaths$value_map

  # CHECK VALIDATE
  good_deaths$validated
  good_deaths$validate()
  good_deaths$validated

  # CHECK STANDARDIZE
  good_deaths$standardize()
  good_deaths$standardized


  View(good_deaths$standardized_data)
