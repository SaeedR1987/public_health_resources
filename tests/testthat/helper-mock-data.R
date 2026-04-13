#' Mock Data Generator Utilities for Testing
#'
#' @deprecated This file is deprecated. Please use the functions from
#' `dev/generate_household_samples.R` instead for more realistic test data
#' that matches the project's actual data structure.
#'
#' Migration guide:
#' - create_mock_household_data() → generate_household_dataset()
#' - create_mock_individual_data() → generate_hh_roster_dataset()
#' - create_mock_fsl_household_data() → generate_household_dataset() (FSL columns included)
#' - create_mock_wash_household_data() → generate_household_dataset() (WASH columns included)
#' - create_mock_health_household_data() → generate_household_dataset() (health columns included)
#' - create_mock_health_individual_data() → generate_health_ind_dataset()
#' - create_mock_nutrition_data() → generate_child_nutrition_dataset()
#' - create_mock_water_container_data() → generate_water_count_loop_dataset()
#' - create_mock_death_data() → generate_died_member_dataset()
#'
#' This file provides helper functions to create mock data for testing
#' the various data class implementations in the phr package.
#'

# ============================================================================
# BASE MOCK DATA GENERATORS
# ============================================================================

#' Create mock household data
#'
#' @param n Number of rows
#' @param seed Random seed for reproducibility
#' @return A tibble with standard household columns
create_mock_household_data <- function(n = 100, seed = 42) {
  set.seed(seed)
  tibble::tibble(
    uuid = paste0("hh_", sprintf("%03d", 1:n)),
    consent = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.95, 0.05)),
    interview_date = Sys.Date() - sample(0:30, n, replace = TRUE),
    enumerator_id = sample(paste0("E", sprintf("%02d", 1:10)), n, replace = TRUE),
    cluster_id = sample(paste0("C", sprintf("%02d", 1:20)), n, replace = TRUE),
    strata = sample(c("Urban", "Rural", "Camp"), n, replace = TRUE),
    weight = round(runif(n, 0.5, 2.5), 3),
    adm1 = sample(paste0("Region_", LETTERS[1:5]), n, replace = TRUE),
    adm2 = sample(paste0("District_", 1:15), n, replace = TRUE),
    gps_lat = runif(n, -10, 10),
    gps_lon = runif(n, 30, 50),
    hh_size = sample(1:12, n, replace = TRUE)
  )
}

#' Create mock individual data
#'
#' @param n Number of rows
#' @param hh_uuids Optional vector of household UUIDs to link to
#' @param seed Random seed for reproducibility
#' @return A tibble with standard individual columns
create_mock_individual_data <- function(n = 200, hh_uuids = NULL, seed = 42) {
  set.seed(seed)
  
  if (is.null(hh_uuids)) {
    hh_uuids <- paste0("hh_", sprintf("%03d", 1:min(n/2, 100)))
  }
  
  tibble::tibble(
    uuid = paste0("ind_", sprintf("%04d", 1:n)),
    hh_uuid = sample(hh_uuids, n, replace = TRUE),
    sex = sample(c("male", "female"), n, replace = TRUE),
    age_years = sample(0:90, n, replace = TRUE),
    age_months = ifelse(sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7)),
                        sample(0:59, n, replace = TRUE), NA),
    relationship = sample(c("head", "spouse", "child", "other_relative", "non_relative"),
                         n, replace = TRUE, prob = c(0.2, 0.15, 0.45, 0.15, 0.05))
  )
}

# ============================================================================
# DOMAIN-SPECIFIC MOCK DATA GENERATORS
# ============================================================================

#' Create mock FSL household data
#'
#' @param n Number of rows
#' @param seed Random seed
#' @return A tibble with FSL indicator columns
create_mock_fsl_household_data <- function(n = 100, seed = 42) {
  set.seed(seed)
  
  base <- create_mock_household_data(n, seed)
  
  fsl_cols <- tibble::tibble(
    # FCS components
    fsl_fcs_cereal = sample(0:7, n, replace = TRUE),
    fsl_fcs_legumes = sample(0:7, n, replace = TRUE),
    fsl_fcs_dairy = sample(0:7, n, replace = TRUE),
    fsl_fcs_meat = sample(0:7, n, replace = TRUE),
    fsl_fcs_veg = sample(0:7, n, replace = TRUE),
    fsl_fcs_fruit = sample(0:7, n, replace = TRUE),
    fsl_fcs_oil = sample(0:7, n, replace = TRUE),
    fsl_fcs_sugar = sample(0:7, n, replace = TRUE),
    
    # rCSI components
    fsl_rcsi_lessquality = sample(0:7, n, replace = TRUE),
    fsl_rcsi_borrow = sample(0:7, n, replace = TRUE),
    fsl_rcsi_mealsize = sample(0:7, n, replace = TRUE),
    fsl_rcsi_mealadult = sample(0:7, n, replace = TRUE),
    fsl_rcsi_mealnb = sample(0:7, n, replace = TRUE),
    
    # HHS components
    fsl_hhs_nofoodhh = sample(c("yes", "no"), n, replace = TRUE),
    fsl_hhs_nofoodhh_freq = sample(c("Rarely", "Sometimes", "Often", NA), n, replace = TRUE),
    fsl_hhs_sleephungry = sample(c("yes", "no"), n, replace = TRUE),
    fsl_hhs_sleephungry_freq = sample(c("Rarely", "Sometimes", "Often", NA), n, replace = TRUE),
    fsl_hhs_alldaynight = sample(c("yes", "no"), n, replace = TRUE),
    fsl_hhs_alldaynight_freq = sample(c("Rarely", "Sometimes", "Often", NA), n, replace = TRUE)
  )
  
  dplyr::bind_cols(base, fsl_cols)
}

#' Create mock WASH household data
#'
#' @param n Number of rows
#' @param seed Random seed
#' @return A tibble with WASH indicator columns
create_mock_wash_household_data <- function(n = 100, seed = 42) {
  set.seed(seed)
  
  base <- create_mock_household_data(n, seed)
  
  wash_cols <- tibble::tibble(
    # Water source
    wash_water_source_primary = sample(
      c("piped_dwelling", "piped_yard", "public_tap", "borehole", 
        "protected_well", "unprotected_well", "spring_protected",
        "spring_unprotected", "rainwater", "tanker", "cart_small_tank",
        "surface_water", "bottled", "other"),
      n, replace = TRUE
    ),
    wash_water_source_secondary = sample(
      c("none", "public_tap", "borehole", "surface_water", NA),
      n, replace = TRUE
    ),
    
    # Water treatment
    wash_water_treatment_yn = sample(c("yes", "no"), n, replace = TRUE),
    wash_water_treatment_methods = sample(
      c("boiling", "chlorine", "filter", "solar", "none", "other", NA),
      n, replace = TRUE
    ),
    
    # Water collection
    wash_water_collection_time_minutes = sample(c(0, 5, 15, 30, 60, 120, 180, NA), n, replace = TRUE),
    wash_water_sufficient_quantity = sample(c("yes", "no", "sometimes"), n, replace = TRUE),
    
    # Sanitation
    wash_sanitation_type = sample(
      c("flush_sewer", "flush_septic", "flush_pit", "flush_unknown",
        "pit_slab", "pit_no_slab", "composting", "bucket", 
        "hanging", "open_defecation", "other"),
      n, replace = TRUE
    ),
    wash_sanitation_shared = sample(c("yes", "no"), n, replace = TRUE),
    wash_sanitation_shared_households = sample(c(NA, 2, 3, 5, 10), n, replace = TRUE),
    
    # Handwashing
    wash_handwashing_facility = sample(
      c("fixed_observed", "mobile_observed", "not_observed", "none"),
      n, replace = TRUE
    ),
    wash_handwashing_water = sample(c("yes", "no", NA), n, replace = TRUE),
    wash_handwashing_soap = sample(c("yes", "no", NA), n, replace = TRUE),
    
    # Waste
    wash_waste_disposal = sample(
      c("collected_regular", "collected_irregular", "communal_bin",
        "buried", "burned", "open_dump", "other"),
      n, replace = TRUE
    )
  )
  
  dplyr::bind_cols(base, wash_cols)
}

#' Create mock health household data
#'
#' @param n Number of rows
#' @param seed Random seed
#' @return A tibble with health access indicator columns
create_mock_health_household_data <- function(n = 100, seed = 42) {
  set.seed(seed)
  
  base <- create_mock_household_data(n, seed)
  
  health_cols <- tibble::tibble(
    # Health facility access
    health_facility_distance_km = round(runif(n, 0.1, 50), 1),
    health_facility_time_minutes = sample(c(5, 15, 30, 60, 120, 180, 240), n, replace = TRUE),
    health_facility_access_barrier = sample(
      c("none", "distance", "cost", "quality", "wait_time", 
        "no_staff", "no_medicine", "security", "transport", "other"),
      n, replace = TRUE
    ),
    
    # Health care utilization
    health_primary_care_use_30d = sample(c("yes", "no"), n, replace = TRUE),
    health_care_reason = sample(
      c("illness", "injury", "pregnancy", "vaccination", "chronic", "other", NA),
      n, replace = TRUE
    ),
    
    # Health insurance
    health_insurance_coverage = sample(c("yes", "no"), n, replace = TRUE),
    health_insurance_type = sample(c("government", "private", "community", "employer", NA), n, replace = TRUE),
    
    # Health expenditure
    health_expenditure_30d = round(runif(n, 0, 500), 0),
    health_expenditure_catastrophic = sample(c("yes", "no", NA), n, replace = TRUE),
    
    # Maternal health
    health_maternal_anc_access = sample(c("yes", "no", "not_applicable", NA), n, replace = TRUE),
    health_delivery_location_last = sample(c("facility", "home", "other", NA), n, replace = TRUE)
  )
  
  dplyr::bind_cols(base, health_cols)
}

#' Create mock health individual data
#'
#' @param n Number of rows
#' @param hh_uuids Optional vector of household UUIDs
#' @param seed Random seed
#' @return A tibble with individual health status columns
create_mock_health_individual_data <- function(n = 200, hh_uuids = NULL, seed = 42) {
  set.seed(seed)
  
  base <- create_mock_individual_data(n, hh_uuids, seed)
  
  health_cols <- tibble::tibble(
    # Morbidity - 2 week recall
    health_diarrhea_2w = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.15, 0.85)),
    health_ari_2w = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.12, 0.88)),
    health_fever_2w = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.18, 0.82)),
    health_malaria_2w = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.08, 0.92)),
    health_skin_infection_2w = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.05, 0.95)),
    health_injury_2w = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.03, 0.97)),
    
    # Care seeking
    health_sought_care = sample(c("yes", "no", NA), n, replace = TRUE),
    health_care_provider = sample(
      c("public_facility", "private_facility", "pharmacy", "traditional", 
        "chw", "other", NA),
      n, replace = TRUE
    ),
    health_treatment_received = sample(c("yes", "no", NA), n, replace = TRUE),
    
    # Chronic conditions
    health_chronic_condition = sample(
      c("none", "hypertension", "diabetes", "respiratory", "other", NA),
      n, replace = TRUE, prob = c(0.7, 0.1, 0.05, 0.05, 0.05, 0.05)
    ),
    
    # Disability
    health_disability = sample(
      c("none", "seeing", "hearing", "mobility", "cognition", "self_care", "communication"),
      n, replace = TRUE, prob = c(0.85, 0.03, 0.02, 0.04, 0.02, 0.02, 0.02)
    ),
    
    # Vaccination (for children)
    health_vaccination_card = sample(c("yes_seen", "yes_not_seen", "no", NA), n, replace = TRUE),
    health_bcg_received = sample(c("yes", "no", NA), n, replace = TRUE),
    health_measles_received = sample(c("yes", "no", NA), n, replace = TRUE)
  )
  
  dplyr::bind_cols(base, health_cols)
}

#' Create mock water container data
#'
#' @param n Number of rows
#' @param hh_uuids Optional vector of household UUIDs
#' @param seed Random seed
#' @return A tibble with water container assessment columns
create_mock_water_container_data <- function(n = 200, hh_uuids = NULL, seed = 42) {
  set.seed(seed)
  
  if (is.null(hh_uuids)) {
    hh_uuids <- paste0("hh_", sprintf("%03d", 1:min(n/2, 100)))
  }
  
  tibble::tibble(
    uuid = paste0("cont_", sprintf("%04d", 1:n)),
    hh_uuid = sample(hh_uuids, n, replace = TRUE),
    container_number = sample(1:5, n, replace = TRUE),
    
    # Container characteristics
    container_type = sample(
      c("jerry_can", "bucket", "drum", "tank", "clay_pot", "other"),
      n, replace = TRUE
    ),
    container_material = sample(
      c("plastic", "metal", "clay", "other"),
      n, replace = TRUE
    ),
    container_capacity_liters = sample(c(5, 10, 20, 25, 50, 100, 200), n, replace = TRUE),
    
    # Storage practices
    container_cleanliness = sample(
      c("clean", "moderately_clean", "dirty"),
      n, replace = TRUE
    ),
    container_covered = sample(c("yes", "no", "partially"), n, replace = TRUE),
    container_protected = sample(c("yes", "no"), n, replace = TRUE),
    container_storage_location = sample(
      c("inside_dwelling", "outside_covered", "outside_uncovered"),
      n, replace = TRUE
    ),
    container_storage_hours = sample(c(1, 6, 12, 24, 48, 72), n, replace = TRUE),
    
    # Water source for this container
    container_water_source = sample(
      c("piped", "borehole", "well", "surface", "rainwater", "tanker"),
      n, replace = TRUE
    ),
    
    # Water quality (if tested)
    container_water_quality_tested = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.3, 0.7)),
    container_water_quality_ecoli = ifelse(
      sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7)),
      round(runif(n, 0, 500), 0),
      NA
    ),
    container_water_quality_turbidity = ifelse(
      sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7)),
      round(runif(n, 0, 20), 1),
      NA
    )
  )
}

#' Create mock nutrition individual data
#'
#' @param n Number of rows (typically children 6-59 months)
#' @param hh_uuids Optional vector of household UUIDs
#' @param seed Random seed
#' @return A tibble with nutrition/anthropometric columns
create_mock_nutrition_data <- function(n = 150, hh_uuids = NULL, seed = 42) {
  set.seed(seed)
  
  if (is.null(hh_uuids)) {
    hh_uuids <- paste0("hh_", sprintf("%03d", 1:min(n/2, 100)))
  }
  
  # Generate age in months (6-59 months for typical SMART surveys)
  age_months <- sample(6:59, n, replace = TRUE)
  
  tibble::tibble(
    uuid = paste0("nutr_", sprintf("%04d", 1:n)),
    hh_uuid = sample(hh_uuids, n, replace = TRUE),
    
    # Demographics
    nutr_sex = sample(c("male", "female"), n, replace = TRUE),
    nutr_age_months = age_months,
    nutr_age_days = age_months * 30 + sample(-15:15, n, replace = TRUE),
    nutr_dob = Sys.Date() - (age_months * 30),
    
    # Anthropometry
    nutr_muac_mm = round(rnorm(n, mean = 135, sd = 15), 0),
    nutr_weight_kg = round(rnorm(n, mean = 10, sd = 2.5), 2),
    nutr_height_cm = round(rnorm(n, mean = 85, sd = 10), 1),
    
    # Oedema
    nutr_oedema = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.02, 0.98)),
    
    # Child present
    nutr_child_present = sample(c("yes", "no"), n, replace = TRUE, prob = c(0.95, 0.05)),
    
    # Measurement quality
    nutr_measurement_quality = sample(
      c("good", "acceptable", "poor"),
      n, replace = TRUE, prob = c(0.85, 0.12, 0.03)
    ),
    
    # Infant feeding (for 6-23 months)
    nutr_breastfed_currently = sample(c("yes", "no", NA), n, replace = TRUE),
    nutr_breastfed_exclusive_6m = sample(c("yes", "no", NA), n, replace = TRUE),
    
    # Dietary diversity (simplified)
    nutr_dietary_diversity_score = sample(0:7, n, replace = TRUE),
    
    # Pre-calculated z-scores (normally would be calculated)
    nutr_whz = round(rnorm(n, mean = -0.5, sd = 1.2), 2),
    nutr_haz = round(rnorm(n, mean = -1.2, sd = 1.3), 2),
    nutr_waz = round(rnorm(n, mean = -0.8, sd = 1.1), 2),
    nutr_mfaz = round(rnorm(n, mean = -0.6, sd = 1.0), 2)
  )
}

#' Create mock death individual data
#'
#' @param n Number of rows
#' @param hh_uuids Optional vector of household UUIDs
#' @param recall_start Start date of recall period
#' @param seed Random seed
#' @return A tibble with death record columns
create_mock_death_data <- function(n = 30, hh_uuids = NULL, recall_start = NULL, seed = 42) {
  set.seed(seed)
  
  if (is.null(hh_uuids)) {
    hh_uuids <- paste0("hh_", sprintf("%03d", 1:min(n * 3, 100)))
  }
  
  if (is.null(recall_start)) {
    recall_start <- Sys.Date() - 90
  }
  
  tibble::tibble(
    uuid = paste0("death_", sprintf("%03d", 1:n)),
    hh_uuid = sample(hh_uuids, n, replace = TRUE),
    
    # Demographics of deceased
    sex = sample(c("male", "female"), n, replace = TRUE),
    age_years = sample(0:90, n, replace = TRUE),
    
    # Death timing
    estimated_dod = recall_start + sample(0:90, n, replace = TRUE),
    exact_dod = ifelse(
      sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.6, 0.4)),
      as.character(recall_start + sample(0:90, n, replace = TRUE)),
      NA
    ),
    
    # Cause of death
    cause_of_death = sample(
      c("illness_fever", "illness_diarrhea", "illness_respiratory",
        "injury_violence", "injury_accident", "maternal", "unknown", "other"),
      n, replace = TRUE
    ),
    
    # Location
    location_of_death = sample(
      c("current_residence", "previous_residence", "during_migration",
        "health_facility", "other"),
      n, replace = TRUE
    ),
    
    # Additional details
    death_details = sample(
      c("sudden", "prolonged_illness", "accident", "conflict_related", NA),
      n, replace = TRUE
    )
  )
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Add random missing values to a data frame
#'
#' @param df Data frame
#' @param cols Columns to add missing values to (NULL = all)
#' @param prop Proportion of values to make NA
#' @param seed Random seed
#' @return Data frame with added NA values
add_random_missing <- function(df, cols = NULL, prop = 0.05, seed = 42) {
  set.seed(seed)
  
  if (is.null(cols)) {
    cols <- names(df)
  }
  
  for (col in cols) {
    if (col %in% names(df)) {
      n <- length(df[[col]])
      na_idx <- sample(1:n, size = ceiling(n * prop), replace = FALSE)
      df[[col]][na_idx] <- NA
    }
  }
  
  df
}

#' Add data quality issues to mock data for testing validation
#'
#' @param df Data frame
#' @param issue_type Type of issue to introduce
#' @param col Column to affect
#' @param prop Proportion of rows to affect
#' @param seed Random seed
#' @return Data frame with introduced issues
add_data_quality_issues <- function(df, issue_type, col, prop = 0.05, seed = 42) {
  set.seed(seed)
  
  n <- nrow(df)
  affected_rows <- sample(1:n, size = ceiling(n * prop), replace = FALSE)
  
  switch(issue_type,
    "out_of_range" = {
      if (is.numeric(df[[col]])) {
        df[[col]][affected_rows] <- df[[col]][affected_rows] * 10
      }
    },
    "invalid_value" = {
      df[[col]][affected_rows] <- "INVALID_VALUE"
    },
    "duplicate" = {
      if (length(affected_rows) > 1) {
        df[[col]][affected_rows] <- df[[col]][affected_rows[1]]
      }
    },
    "whitespace" = {
      df[[col]][affected_rows] <- paste0("  ", df[[col]][affected_rows], "  ")
    }
  )
  
  df
}
