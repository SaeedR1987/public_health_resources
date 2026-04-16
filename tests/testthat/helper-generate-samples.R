# Helper: generate sample datasets for household schema testing
# Generated from phr_tool_v2.xlsx xlsform
# This file is auto-loaded by testthat before running tests.

library(dplyr)
library(tibble)
library(lubridate)

set.seed(42)  # For reproducibility

# Number of records for main dataset
n <- 500

# Generate main household dataset
generate_household_dataset <- function(n) {

  # Core IDs
  uuid <- paste0("HH-", sprintf("%05d", 1:n))

  # Dates - ensure end > start, both in past
  start_dates <- seq.POSIXt(
    from = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
    to   = as.POSIXct("2024-12-01 23:59:59", tz = "UTC"),
    length.out = n
  )

  # Optionally add a random time-of-day within each day
  start <- start_dates + sample(0:(24*60*60 - 1), n, replace = TRUE)

  # End is 8 to 30 minutes after start
  end <- start + sample(8:30, n, replace = TRUE) * 60

  # If you specifically need character columns:
  start_chr <- format(start, "%Y-%m-%d %H:%M:%S")
  end_chr   <- format(end,   "%Y-%m-%d %H:%M:%S")
  date_survey <- as.character(seq(from = Sys.Date() - 6, to = Sys.Date(), length.out = n))
  deviceid <- paste0('device-', sprintf("%04d", 1:n))
  audit <- paste0('audit_', uuid, '.csv')
  cluster_id_text <- paste0('cluster_id_text_', sample(1:100, n, replace = TRUE))
  cluster_geopoint <- paste(runif(n, -90, 90), runif(n, -180, 180), runif(n, 0, 100), runif(n, 0, 10), sep = ' ')
  enumerator <- sample(c('1', '2', '3', '4', '5'), n, replace = TRUE)
  stratum <- sample(c('Strata A','Strata B'),n,replace = TRUE)
  weight <- runif(n, min = 0.5, max = 1.5)
  admin1 <- sample(c('admin1a', 'admin1b', 'admin1c'), n, replace = TRUE)
  admin2 <- sample(c('admin2a', 'admin2b', 'admin2c'), n, replace = TRUE)
  admin3 <- sample(c('admin3a', 'admin3b', 'admin3c'), n, replace = TRUE)
  admin4 <- sample(c('admin4a', 'admin4b', 'admin4c'), n, replace = TRUE)
  k <- max(1, ceiling(n / 10))
  cluster <- sample(paste0("cluster", seq_len(k)), n, replace = TRUE)
  respondent_age <- sample(18:120, n, replace = TRUE)
  respondent_sex <- sample(c('m', 'f'), n, replace = TRUE)
  respondent_consent <- sample(c('yes', 'no'), n, replace = TRUE)
  # Generate `hohh_status`

  hohh_status <- sample(
    c('single', 'married', 'divorced', 'widowed', 'other', 'dont_know', 'prefer_not_to_answer'),
    n,
    replace = TRUE
  )

  # Generate `hohh_status_other` based on `hohh_status`
  hohh_status_other <- ifelse(
    hohh_status == 'other',
    paste0('hohh_status_other_', sample(1:100, n, replace = TRUE)),  # Sampled values if `hohh_status` is 'other'
    NA_character_  # Otherwise, NA
  )
  # Generate `residency_status`
  residency_status <- sample(
    c('host', 'idp', 'idp_returnee', 'refugee_returnee', 'refugee'),
    n,
    replace = TRUE
  )

  # Generate `dod_idp_returnee` only if `residency_status` is NOT 'host'
  dod_idp_returnee <- ifelse(
    residency_status != 'host',
    sample(c('yes', 'no'), n, replace = TRUE),  # Sampled values if not 'host'
    NA_character_  # Otherwise, NA
  )

  # Generate `date_dod_idp_returnee` only if `dod_idp_returnee` is not NA
  date_dod_idp_returnee <- ifelse(
    !is.na(dod_idp_returnee),
    as.character(start_dates - sample(0:365, n, replace = TRUE)),  # Sampled dates if `dod_idp_returnee` is valid
    NA_character_  # Otherwise, NA
  )

  # Generate `doa_idp_returnee` only if `residency_status` is NOT 'host'
  doa_idp_returnee <- ifelse(
    residency_status != 'host',
    sample(c('yes', 'no'), n, replace = TRUE),  # Sampled values if not 'host'
    NA_character_  # Otherwise, NA
  )

  # Generate `date_doa_idp_returnee` only if `doa_idp_returnee` is not NA
  date_doa_idp_returnee <- ifelse(
    !is.na(doa_idp_returnee),
    as.character(start_dates - sample(0:365, n, replace = TRUE)),  # Sampled dates if `doa_idp_returnee` is valid
    NA_character_  # Otherwise, NA
  )
  # Generate `first_priority_need` and conditionally generate `first_priority_need_other`
  first_priority_need <- sample(
    c('drinking_water', 'food', 'shelter_materials', 'shelter_repair_support',
      'wash_nfi', 'clothin_blankets', 'cooking_facilities', 'fuel',
      'healthcare', 'livelihoods_support', 'other'),  # Include 'other'
    n,
    replace = TRUE
  )

  first_priority_need_other <- ifelse(
    first_priority_need == 'other',
    paste0('first_priority_need_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other'
    NA_character_  # Otherwise, NA
  )

  # Generate `second_priority_need` and conditionally generate `second_priority_need_other`
  second_priority_need <- sample(
    c('drinking_water', 'food', 'shelter_materials', 'shelter_repair_support',
      'wash_nfi', 'clothin_blankets', 'cooking_facilities', 'fuel',
      'healthcare', 'livelihoods_support', 'other'),  # Include 'other'
    n,
    replace = TRUE
  )

  second_priority_need_other <- ifelse(
    second_priority_need == 'other',
    paste0('second_priority_need_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other'
    NA_character_  # Otherwise, NA
  )

  # Generate `third_priority_need` and conditionally generate `third_priority_need_other`
  third_priority_need <- sample(
    c('drinking_water', 'food', 'shelter_materials', 'shelter_repair_support',
      'wash_nfi', 'clothin_blankets', 'cooking_facilities', 'fuel',
      'healthcare', 'livelihoods_support', 'other'),  # Include 'other'
    n,
    replace = TRUE
  )

  third_priority_need_other <- ifelse(
    third_priority_need == 'other',
    paste0('third_priority_need_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other'
    NA_character_  # Otherwise, NA
  )
  num_hh <- sample(0:10, n, replace = TRUE)
  # Generate `hd_nest_rely_assistant` and conditionally generate `hd_nest_rely_assistant_num`
  hd_nest_rely_assistant <- sample(
    c('yes', 'no', 'dont_know'),
    n,
    replace = TRUE
  )

  hd_nest_rely_assistant_num <- ifelse(
    hd_nest_rely_assistant == 'yes',
    sample(0:100, n, replace = TRUE),  # Assign values only if `hd_nest_rely_assistant` is 'yes'
    NA_integer_  # Otherwise, NA
  )

  # Generate `hd_nest_assistive_device` and conditionally generate `hd_nest_assistive_device_num`
  hd_nest_assistive_device <- sample(
    c('yes', 'no', 'dont_know'),
    n,
    replace = TRUE
  )

  hd_nest_assistive_device_num <- ifelse(
    hd_nest_assistive_device == 'yes',
    sample(0:100, n, replace = TRUE),  # Assign values only if `hd_nest_assistive_device` is 'yes'
    NA_integer_  # Otherwise, NA
  )

  # Generate `hd_nest_assist_information` and conditionally generate `hd_nest_assist_information_num`
  hd_nest_assist_information <- sample(
    c('yes', 'no', 'dont_know'),
    n,
    replace = TRUE
  )

  hd_nest_assist_information_num <- ifelse(
    hd_nest_assist_information == 'yes',
    sample(0:100, n, replace = TRUE),  # Assign values only if `hd_nest_assist_information` is 'yes'
    NA_integer_  # Otherwise, NA
  )
  # Generate `left_yn_known` and conditionally generate `num_left`
  left_yn_known <- sample(
    c('yes', 'no', 'dont_know'),
    n,
    replace = TRUE
  )

  num_left <- ifelse(
    left_yn_known == 'yes',
    sample(0:100, n, replace = TRUE),  # Assign values if `left_yn_known` is 'yes'
    NA_integer_  # Otherwise, NA
  )

  # Generate `join_yn_known` and conditionally generate `num_join`
  join_yn_known <- sample(
    c('yes', 'no', 'dont_know'),
    n,
    replace = TRUE
  )

  num_join <- ifelse(
    join_yn_known == 'yes',
    sample(0:100, n, replace = TRUE),  # Assign values if `join_yn_known` is 'yes'
    NA_integer_  # Otherwise, NA
  )

  # Define barriers
  barriers <- c(
    "did_not_need_to_access_services",
    "no_functional_health_facility_nearby",
    "specific_service_sought_unavailable",
    "could_not_afford_cost_of_medication_not_price_increase",
    "could_not_afford_cost_of_medication_price_increased",
    "not_registered_with_a_local_doctor",
    "long_waiting_time_for_the_service",
    "could_not_afford_cost_of_consultationservice",
    "could_not_afford_transportation_to_health_facility",
    "health_facility_is_too_far_away",
    "other"
  )

  # Generate health_healthcare_barriers
  health_healthcare_barriers <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(barriers)), 1)  # k is a single integer: 1..3
    paste(sample(barriers, size = k, replace = FALSE), collapse = " ")
  })

  # Generate health_healthcare_barriers_other conditionally
  health_healthcare_barriers_other <- ifelse(
    grepl("\\bother\\b", health_healthcare_barriers),  # Check if 'other' is present in the barriers
    paste0("health_healthcare_barriers_other_", sample(1:100, n, replace = TRUE)),  # Assign a value if 'other' is present
    NA_character_  # Otherwise, NA
  )

  # Generate health_healthcare_travel_time
  health_healthcare_travel_time <- sample(
    c('num_minutes', 'range', 'dont_know', 'prefer_not_to_answer'),
    n,
    replace = TRUE
  )

  # Conditionally generate health_healthcare_travel_time_int
  health_healthcare_travel_time_int <- ifelse(
    health_healthcare_travel_time == 'num_minutes',
    sample(0:100, n, replace = TRUE),  # Assign values if `health_healthcare_travel_time` is 'num_minutes'
    NA_integer_  # Otherwise, NA
  )

  # Conditionally generate health_healthcare_travel_time_range
  health_healthcare_travel_time_range <- ifelse(
    health_healthcare_travel_time == 'dont_know',
    sample(c('under_30min', '30min_1hr', '1hr_halfday', 'more_halfday'), n, replace = TRUE),  # Assign values if `health_healthcare_travel_time` is 'dont_know'
    NA_character_  # Otherwise, NA
  )

  fsl_hdds_cereals <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hdds_tubers <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hdds_legumes <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hdds_veg <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hdds_fruit <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hdds_meat <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hdds_fish <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hdds_dairy <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hdds_eggs <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hdds_sugar <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hdds_oil <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hdds_condiments <- sample(c('yes', 'no'), n, replace = TRUE)
  # Generate `fsl_hhs_nofoodhh` and conditionally generate `fsl_hhs_nofoodhh_freq`
  fsl_hhs_nofoodhh <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hhs_nofoodhh_freq <- ifelse(
    fsl_hhs_nofoodhh == 'yes',
    sample(c('rarely', 'sometimes', 'often'), n, replace = TRUE),  # Assign values if `fsl_hhs_nofoodhh` is 'yes'
    NA_character_  # Otherwise, NA
  )

  # Generate `fsl_hhs_sleephungry` and conditionally generate `fsl_hhs_sleephungry_freq`
  fsl_hhs_sleephungry <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hhs_sleephungry_freq <- ifelse(
    fsl_hhs_sleephungry == 'yes',
    sample(c('rarely', 'sometimes', 'often'), n, replace = TRUE),  # Assign values if `fsl_hhs_sleephungry` is 'yes'
    NA_character_  # Otherwise, NA
  )

  # Generate `fsl_hhs_alldaynight` and conditionally generate `fsl_hhs_alldaynight_freq`
  fsl_hhs_alldaynight <- sample(c('yes', 'no'), n, replace = TRUE)
  fsl_hhs_alldaynight_freq <- ifelse(
    fsl_hhs_alldaynight == 'yes',
    sample(c('rarely', 'sometimes', 'often'), n, replace = TRUE),  # Assign values if `fsl_hhs_alldaynight` is 'yes'
    NA_character_  # Otherwise, NA
  )
  # Generate `fsl_first_food_sources` and conditionally generate `fsl_first_food_sources_other`
  fsl_first_food_sources <- sample(
    c('own_production', 'market', 'borrowing_debts', 'support_relatives',
      'exchange', 'bartering', 'hunting', 'fishing', 'gathering',
      'assistance', 'other'),  # Include 'other'
    n,
    replace = TRUE
  )

  fsl_first_food_sources_other <- ifelse(
    fsl_first_food_sources == 'other',
    paste0('fsl_first_food_sources_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other'
    NA_character_  # Otherwise, NA
  )

  # Generate `fsl_second_food_sources` and conditionally generate `fsl_second_food_sources_other`
  fsl_second_food_sources <- sample(
    c('own_production', 'market', 'borrowing_debts', 'support_relatives',
      'exchange', 'bartering', 'hunting', 'fishing', 'gathering',
      'assistance', 'other'),  # Include 'other'
    n,
    replace = TRUE
  )

  fsl_second_food_sources_other <- ifelse(
    fsl_second_food_sources == 'other',
    paste0('fsl_second_food_sources_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other'
    NA_character_  # Otherwise, NA
  )

  # Generate `fsl_third_food_sources` and conditionally generate `fsl_third_food_sources_other`
  fsl_third_food_sources <- sample(
    c('own_production', 'market', 'borrowing_debts', 'support_relatives',
      'exchange', 'bartering', 'hunting', 'fishing', 'gathering',
      'assistance', 'other'),  # Include 'other'
    n,
    replace = TRUE
  )

  fsl_third_food_sources_other <- ifelse(
    fsl_third_food_sources == 'other',
    paste0('fsl_third_food_sources_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other'
    NA_character_  # Otherwise, NA
  )

  food_source_barriers <- c(
    "none",
    "live_too_far_from_food_sources",
    "transportation_too_expensive",
    "not_enough_food",
    "damage_to_food_source",
    "security_travelling_to_and_from_food_sources",
    "socially_not_allowed",
    "other",
    "dont_know",
    "prefer_not_to_answer"
  )

  # Generate `fsl_food_sources_barriers`
  fsl_food_sources_barriers <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(food_source_barriers)), 1)  # Random number of barriers (1 to 3)
    paste(
      sample(food_source_barriers, size = k, replace = FALSE),  # Randomly select barriers
      collapse = " "  # Combine them into a single string
    )
  })

  # Conditionally assign `fsl_food_sources_barriers_other` based on presence of "other"
  fsl_food_sources_barriers_other <- ifelse(
    grepl("\\bother\\b", fsl_food_sources_barriers),  # Check if 'other' is present in the barriers
    paste0("fsl_food_sources_barriers_other_", sample(1:100, n, replace = TRUE)),  # Assign value if 'other' is present
    NA_character_  # Otherwise, NA
  )
  # Generate `fsl_hh_clean_water_preparation` and conditionally generate `fsl_hh_clean_water_preparation_other`
  fsl_hh_clean_water_preparation <- sample(
    c('piped_dwelling', 'piped_compound', 'piped_neighbour', 'tap',
      'borehole', 'protected_well', 'unprotected_well', 'well_spring',
      'unprotected_spring', 'rainwater_collection', 'other'),  # Include 'other'
    n,
    replace = TRUE
  )

  fsl_hh_clean_water_preparation_other <- ifelse(
    fsl_hh_clean_water_preparation == 'other',
    paste0('fsl_hh_clean_water_preparation_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other'
    NA_character_  # Otherwise, NA
  )

  # Generate `fsl_hh_access_cooking_energy` and conditionally generate `fsl_hh_access_cooking_energy_other`
  fsl_hh_access_cooking_energy <- sample(
    c('firewood', 'animal_dung', 'coal', 'electricity', 'biogas',
      'gas', 'straw', 'other', 'dont_know', 'prefer_not_to_answer'),  # Include 'other'
    n,
    replace = TRUE
  )

  fsl_hh_access_cooking_energy_other <- ifelse(
    fsl_hh_access_cooking_energy == 'other',
    paste0('fsl_hh_access_cooking_energy_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other'
    NA_character_  # Otherwise, NA
  )
  fsl_fcs_cereal <- sample(0:7, n, replace = TRUE)
  fsl_fcs_legumes <- sample(0:7, n, replace = TRUE)
  fsl_fcs_dairy <- sample(0:7, n, replace = TRUE)
  fsl_fcs_meat <- sample(0:7, n, replace = TRUE)
  fsl_fcs_veg <- sample(0:7, n, replace = TRUE)
  fsl_fcs_fruit <- sample(0:7, n, replace = TRUE)
  fsl_fcs_oil <- sample(0:7, n, replace = TRUE)
  fsl_fcs_sugar <- sample(0:7, n, replace = TRUE)
  fsl_fcs_condiments <- sample(0:7, n, replace = TRUE)
  fsl_rcsi_lessquality <- sample(0:7, n, replace = TRUE)
  fsl_rcsi_borrow <- sample(0:7, n, replace = TRUE)
  fsl_rcsi_mealsize <- sample(0:7, n, replace = TRUE)
  fsl_rcsi_mealadult <- sample(0:7, n, replace = TRUE)
  fsl_rcsi_mealnb <- sample(0:7, n, replace = TRUE)
  fsl_num_meals_above_5 <- sample(0:5, n, replace = TRUE)
  fsl_num_meals_under_5 <- sample(0:5, n, replace = TRUE)
  # Generate `fsl_first_income_types` and conditionally generate `fsl_first_income_types_other`
  fsl_first_income_types <- sample(
    c('salary', 'sell_agri_prod', 'sell_anim_prod', 'sell_collected',
      'trader', 'daily_labour_ag', 'daily_labour_skilled',
      'daily_labour_non', 'saving', 'pension', 'other'),  # Include 'other'
    n,
    replace = TRUE
  )

  fsl_first_income_types_other <- ifelse(
    fsl_first_income_types == 'other',
    paste0('fsl_first_income_types_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other'
    NA_character_  # Otherwise, NA
  )

  # Generate `fsl_second_income_types` and conditionally generate `fsl_second_income_types_other`
  fsl_second_income_types <- sample(
    c('salary', 'sell_agri_prod', 'sell_anim_prod', 'sell_collected',
      'trader', 'daily_labour_ag', 'daily_labour_skilled',
      'daily_labour_non', 'saving', 'pension', 'other'),  # Include 'other'
    n,
    replace = TRUE
  )

  fsl_second_income_types_other <- ifelse(
    fsl_second_income_types == 'other',
    paste0('fsl_second_income_types_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other'
    NA_character_  # Otherwise, NA
  )

  # Generate `fsl_third_income_types` and conditionally generate `fsl_third_income_types_other`
  fsl_third_income_types <- sample(
    c('salary', 'sell_agri_prod', 'sell_anim_prod', 'sell_collected',
      'trader', 'daily_labour_ag', 'daily_labour_skilled',
      'daily_labour_non', 'saving', 'pension', 'other'),  # Include 'other'
    n,
    replace = TRUE
  )

  fsl_third_income_types_other <- ifelse(
    fsl_third_income_types == 'other',
    paste0('fsl_third_income_types_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other'
    NA_character_  # Otherwise, NA
  )
  fsl_lcsi_stress1 <- sample(c('yes', 'no_had_no_need', 'no_exhausted', 'not_applicable'), n, replace = TRUE)
  fsl_lcsi_stress2 <- sample(c('yes', 'no_had_no_need', 'no_exhausted', 'not_applicable'), n, replace = TRUE)
  fsl_lcsi_stress3 <- sample(c('yes', 'no_had_no_need', 'no_exhausted', 'not_applicable'), n, replace = TRUE)
  fsl_lcsi_stress4 <- sample(c('yes', 'no_had_no_need', 'no_exhausted', 'not_applicable'), n, replace = TRUE)
  fsl_lcsi_crisis1 <- sample(c('yes', 'no_had_no_need', 'no_exhausted', 'not_applicable'), n, replace = TRUE)
  fsl_lcsi_crisis2 <- sample(c('yes', 'no_had_no_need', 'no_exhausted', 'not_applicable'), n, replace = TRUE)
  fsl_lcsi_crisis3 <- sample(c('yes', 'no_had_no_need', 'no_exhausted', 'not_applicable'), n, replace = TRUE)
  fsl_lcsi_emergency1 <- sample(c('yes', 'no_had_no_need', 'no_exhausted', 'not_applicable'), n, replace = TRUE)
  fsl_lcsi_emergency2 <- sample(c('yes', 'no_had_no_need', 'no_exhausted', 'not_applicable'), n, replace = TRUE)
  fsl_lcsi_emergency3 <- sample(c('yes', 'no_had_no_need', 'no_exhausted', 'not_applicable'), n, replace = TRUE)
  fsl_food_cash_voucher <- sample(c('yes', 'no', 'dont_know'), n, replace = TRUE)

  assistance_modalities <- c(
    "food_in_kind",
    "food_voucher",
    "inputs_voucher",
    "mpca",
    "cash_food",
    "cash_livelihoods",
    "other",
    "none",
    "prefer_not_to_answer"
  )

  # Define assistance modalities
  assistance_modalities <- c(
    "cash",
    "voucher",
    "in_kind_delivery",
    "technical_support",
    "other"
  )

  # Generate `fsl_assistance_modality`
  fsl_assistance_modality <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(assistance_modalities)), 1)  # Random number of modalities (1 to 3)
    paste(
      sample(assistance_modalities, size = k, replace = FALSE),  # Randomly select modalities
      collapse = " "  # Combine them into a single string
    )
  })

  # Conditionally generate `fsl_assistance_modality_other` based on the presence of "other"
  fsl_assistance_modality_other <- ifelse(
    grepl("\\bother\\b", fsl_assistance_modality),  # Check if 'other' is present as a whole word
    paste0("fsl_assistance_modality_other_", sample(1:100, n, replace = TRUE)),  # Assign value if 'other' is present
    NA_character_  # Otherwise, NA
  )
  # Generate `market_barriers` and conditionally generate `market_barriers_other`
  market_barriers <- sapply(1:n, function(i) paste(sample(c('opt1', 'opt2', 'other'), 1), collapse = ' '))

  market_barriers_other <- ifelse(
    grepl("\\bother\\b", market_barriers),  # Check if 'other' is present
    paste0('market_barriers_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other' is present
    NA_character_  # Otherwise, NA
  )

  # Generate `purchase_barriers` and conditionally generate `purchase_barriers_other`
  purchase_barriers <- sapply(1:n, function(i) paste(sample(c('opt1', 'opt2', 'other'), 1), collapse = ' '))

  purchase_barriers_other <- ifelse(
    grepl("\\bother\\b", purchase_barriers),  # Check if 'other' is present
    paste0('purchase_barriers_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other' is present
    NA_character_  # Otherwise, NA
  )

  # Generate `wash_water_source` and conditionally generate `wash_water_source_other`
  wash_water_source <- sample(
    c('piped_dwelling', 'piped_compound', 'piped_neighbour', 'tap',
      'borehole', 'protected_well', 'unprotected_well', 'well_spring',
      'unprotected_spring', 'rainwater_collection', 'other'),  # Include 'other'
    n,
    replace = TRUE
  )

  wash_water_source_other <- ifelse(
    wash_water_source == 'other',
    paste0('wash_water_source_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other' is present
    NA_character_  # Otherwise, NA
  )
  wash_other_water_sources <- sample(c('yes', 'no'), n, replace = TRUE)

  water_sources <- c(
    "piped_dwelling",
    "piped_compound",
    "piped_neighbour",
    "tap",
    "borehole",
    "protected_well",
    "unprotected_well",
    "well_spring",
    "unprotected_spring",
    "rainwater_collection"
  )

  wash_different_water_sources <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(water_sources)), 1)  # scalar size
    paste(
      sample(water_sources, size = k, replace = FALSE),
      collapse = " "
    )
  })

  wash_different_water_sources_other <- ifelse(runif(n) < 0.01, paste0('wash_different_water_sources_other_', sample(1:100, n, replace = TRUE)), NA_character_)

  water_source_uses <- c(
    "drinking",
    "cooking",
    "bathing",
    "laundry",
    "household_hygiene",
    "other"
  )

  wash_first_water_source_usage <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(water_source_uses)), 1)  # scalar size
    paste(
      sample(water_source_uses, size = k, replace = FALSE),
      collapse = " "
    )
  })

  wash_first_water_source_usage_other <- ifelse(runif(n) < 0.01, paste0('wash_first_water_source_usage_other_', sample(1:100, n, replace = TRUE)), NA_character_)

  water_source_uses <- c(
    "drinking",
    "cooking",
    "bathing",
    "laundry",
    "household_hygiene",
    "other"
  )

  wash_second_water_source_usage <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(water_source_uses)), 1)  # scalar size
    paste(
      sample(water_source_uses, size = k, replace = FALSE),
      collapse = " "
    )
  })

  wash_second_water_source_usage_other <- ifelse(runif(n) < 0.01, paste0('wash_second_water_source_usage_other_', sample(1:100, n, replace = TRUE)), NA_character_)

  water_source_uses <- c(
    "drinking",
    "cooking",
    "bathing",
    "laundry",
    "household_hygiene",
    "other"
  )

  wash_third_water_source_usage <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(water_source_uses)), 1)  # scalar size
    paste(
      sample(water_source_uses, size = k, replace = FALSE),
      collapse = " "
    )
  })

  wash_third_water_source_usage_other <- ifelse(runif(n) < 0.01, paste0('wash_third_water_source_usage_other_', sample(1:100, n, replace = TRUE)), NA_character_)
  # Generate `wash_water_collect_time` with the predefined probabilities
  wash_water_collect_time <- ifelse(
    runif(n) < 0.95,
    sample(c('on_premise', 'num_minutes', 'dont_know', 'prefer_not_to_answer'), n, replace = TRUE),
    NA_character_
  )

  # Generate `wash_water_collect_time_int` based on `wash_water_collect_time`
  wash_water_collect_time_int <- ifelse(
    wash_water_collect_time == 'num_minutes',
    sample(0:100, n, replace = TRUE),  # Sampled value if `wash_water_collect_time` is 'num_minutes'
    NA_integer_  # Otherwise, NA
  )

  # Generate `wash_water_collect_time_range` based on `wash_water_collect_time`
  wash_water_collect_time_range <- ifelse(
    wash_water_collect_time == 'dont_know',
    sample(c('under30min', '30min_1hr', '1hr_halfday', 'halfday', 'more_than_halfday', 'dont_know', 'prefer_not_to_answer'), n, replace = TRUE),  # Sampled value if `wash_water_collect_time` is 'dont_know'
    NA_character_  # Otherwise, NA
  )
  # Generate `wash_water_treatment`
  wash_water_treatment <- sample(
    c('none', 'boil', 'chlorine', 'filter_cloth', 'other', 'dont_know'),
    n,
    replace = TRUE
  )

  # Generate `wash_water_treatment_other` based on `wash_water_treatment`
  wash_water_treatment_other <- ifelse(
    wash_water_treatment == 'other',
    paste0('wash_water_treatment_other_', sample(1:100, n, replace = TRUE)),  # Sampled values if `wash_water_treatment` is 'other'
    NA_character_  # Otherwise, NA
  )
  # Generate `wash_water_interruption`
  wash_water_interruption <- sample(
    c('yes', 'no', 'dont_know'),
    n,
    replace = TRUE
  )

  # Conditionally generate `wash_water_interruption_num_days`
  wash_water_interruption_num_days <- ifelse(
    wash_water_interruption == 'yes',
    sample(0:100, n, replace = TRUE),  # Assign values if `wash_water_interruption` is 'yes'
    NA_integer_  # Otherwise, NA
  )
  # Generate `wash_containers`
  wash_containers <- sample(
    c('yes', 'no'),
    n,
    replace = TRUE
  )

  # Conditionally generate `wash_num_containers`
  wash_num_containers <- ifelse(
    wash_containers == 'yes',
    sample(0:5, n, replace = TRUE),  # Assign values if `wash_containers` is 'yes'
    NA_integer_  # Otherwise, NA
  )
  wash_soap_access <- sample(c('none', 'yes_confirmed', 'yes_not_confirmed', 'dont_know', 'prefer_not_to_answer'), n, replace = TRUE)
  wash_wise_worry <- sample(c('never', 'rarely', 'sometimes', 'often', 'always'), n, replace = TRUE)
  wash_wise_plans <- sample(c('never', 'rarely', 'sometimes', 'often', 'always'), n, replace = TRUE)
  wash_wise_hands <- sample(c('never', 'rarely', 'sometimes', 'often', 'always'), n, replace = TRUE)
  wash_wise_drink <- sample(c('never', 'rarely', 'sometimes', 'often', 'always'), n, replace = TRUE)
  wash_mosquito_net <- sample(c('yes', 'no'), n, replace = TRUE)
  wash_source_mosquito_net <- ifelse(runif(n) < 0.01, sample(c('mdc', 'gov_health_facility', 'priv_health_facility', 'pharmacy', 'shop_market', 'ngo', 'relig_institution', 'school', 'other', 'dont_know'), n, replace = TRUE), NA_character_)
  wash_sleep_mosquito_net_child <- ifelse(runif(n) < 0.01, sample(c('yes', 'no'), n, replace = TRUE), NA_character_)
  # Generate `wash_toilet_facility` and conditionally generate `wash_toilet_facility_other`
  wash_toilet_facility <- sample(
    c('flu_piped_sew', 'flu_sep_tank', 'flu_pit_lat', 'flu_ope_drai', 'flu_elsewhere',
      'flu_to_dnk_wh', 'pit_lat_with_slab', 'pit_lat_without_slab', 'compost_toi',
      'plast_bag', 'no_facility_bush', 'other'),  # Include 'other'
    n,
    replace = TRUE
  )

  wash_toilet_facility_other <- ifelse(
    wash_toilet_facility == 'other',
    paste0('wash_toilet_facility_other_', sample(1:100, n, replace = TRUE)),  # Value if 'other'
    NA_character_  # Otherwise, NA
  )

  # Conditionally generate `wash_share_toilet_facility` based on `wash_toilet_facility`
  wash_share_toilet_facility <- ifelse(
    wash_toilet_facility != 'no_facility_bush',
    sample(c('yes', 'no', 'dont_know'), n, replace = TRUE),  # Value if not 'no_facility_bush'
    NA_character_  # Otherwise, NA
  )

  # Conditionally generate `wash_num_share_toilet` based on `wash_share_toilet_facility`
  wash_num_share_toilet <- ifelse(
    wash_share_toilet_facility == 'yes',
    sample(0:100, n, replace = TRUE),  # Value if `wash_share_toilet_facility` is 'yes'
    NA_integer_  # Otherwise, NA
  )
  # Generate `shape_shelter` (rectangle or circle)
  shape_shelter <- sample(
    c('rectangle', 'circle'),
    n,
    replace = TRUE
  )

  # Generate `dimension_measure` (yes, no, dont_know)
  dimension_measure <- sample(
    c('yes', 'no', 'dont_know'),
    n,
    replace = TRUE
  )

  # Conditionally generate `length_long` based on `shape_shelter` and `dimension_measure`
  length_long <- ifelse(
    shape_shelter == 'rectangle' & dimension_measure == 'yes',
    runif(n, 0.0, 100.0),  # Assign values if conditions are met
    NA_real_  # Otherwise, NA
  )

  # Conditionally generate `width_larg` based on `shape_shelter` and `dimension_measure`
  width_larg <- ifelse(
    shape_shelter == 'rectangle' & dimension_measure == 'yes',
    runif(n, 0.0, 100.0),  # Assign values if conditions are met
    NA_real_  # Otherwise, NA
  )

  # Conditionally generate `diameter` based on `shape_shelter` and `dimension_measure`
  diameter <- ifelse(
    shape_shelter == 'circle' & dimension_measure == 'yes',
    runif(n, 0.0, 100.0),  # Assign values if conditions are met
    NA_real_  # Otherwise, NA
  )
  # Generate `shelter_type` with the option for 'other'
  shelter_type <- sample(
    c('solid_finished_house', 'solid_finished_apartment', 'unfinished_non_enclosed_building',
      'tent', 'makeshift_shelter', 'none_sleeping_in_open', 'other',
      'dont_know', 'prefer_not_to_answer'),
    n,
    replace = TRUE
  )

  # Conditionally generate `shelter_type_other` based on the value of `shelter_type`
  shelter_type_other <- ifelse(
    shelter_type == 'other',
    paste0('shelter_type_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other'
    NA_character_  # Otherwise, NA
  )

  # Define shelter issue options including 'other'
  shelter_issue_options <- c(
    "none",
    "minor_damage_roof",
    "major_damage_roof",
    "damage_windows",
    "damage_floors",
    "damage_walls",
    "lack_privacy",
    "lack_space",
    "lack_of_insulation_cold",
    "lack_insulation_hot",
    "other"  # Include 'other'
  )

  # Generate `shelter_issues`
  shelter_issues <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(shelter_issue_options)), 1)  # Random number of issues (1 to 3)
    paste(
      sample(shelter_issue_options, size = k, replace = FALSE),  # Randomly selected issues
      collapse = " "  # Combine into a single space-separated string
    )
  })

  # Conditionally generate `shelter_issues_other` based on whether 'other' is in `shelter_issues`
  shelter_issues_other <- ifelse(
    grepl("\\bother\\b", shelter_issues),  # Check if 'other' is a whole word in the string
    paste0('shelter_issues_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other' is present
    NA_character_  # Otherwise, NA
  )
  cooking_abilities <- sample(c('yes', 'yes_w_issues', 'no'), n, replace = TRUE)

  # Define cooking barrier options including 'other'
  cooking_barrier_options <- c(
    "insuf_ess_hh_items_cook",
    "lack_access_to_cook_facility",
    "unsafe_facilities",
    "inade_space_for_cook",
    "insufficient_space",
    "insufficient_fuel",
    "other",  # Include 'other'
    "prefer_not_to_answer"
  )

  # Generate `cooking_barriers`
  cooking_barriers <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(cooking_barrier_options)), 1)  # Random number of barriers (1 to 3)
    paste(
      sample(cooking_barrier_options, size = k, replace = FALSE),  # Randomly select barriers
      collapse = " "  # Combine them into a single string
    )
  })

  # Conditionally generate `cooking_barriers_other` based on the presence of 'other'
  cooking_barriers_other <- ifelse(
    grepl("\\bother\\b", cooking_barriers),  # Check if 'other' is a whole word in `cooking_barriers`
    paste0('cooking_barriers_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other' is present
    NA_character_  # Otherwise, NA
  )
  sleeping_abilities <- sample(c('yes', 'yes_w_issues', 'no'), n, replace = TRUE)

  # Define sleeping barrier options including 'other'
  sleeping_barrier_options <- c(
    "insuf_ess_hh_items_sleep",
    "insufficient_space",
    "unsafe_space",
    "inadequate_sleeping",
    "other",  # Include 'other'
    "prefer_not_to_answer"
  )

  # Generate `sleeping_barriers`
  sleeping_barriers <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(sleeping_barrier_options)), 1)  # Random number of barriers (1 to 3)
    paste(
      sample(sleeping_barrier_options, size = k, replace = FALSE),  # Randomly select barriers
      collapse = " "  # Combine them into a single string
    )
  })

  # Conditionally generate `sleeping_barriers_other` based on the presence of 'other'
  sleeping_barriers_other <- ifelse(
    grepl("\\bother\\b", sleeping_barriers),  # Check if 'other' is a whole word in `sleeping_barriers`
    paste0('sleeping_barriers_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other' is present
    NA_character_  # Otherwise, NA
  )
  storing_abilities <- sample(c('yes', 'yes_w_issues', 'no'), n, replace = TRUE)
  # Note: storing_barriers uses barriers_sleeping list per xlsxform (sleep-related options for storing question)

  # Define storing barrier options including 'other'
  storing_barrier_options <- c(
    "insuf_ess_hh_items_sleep",
    "insufficient_space",
    "unsafe_space",
    "inadequate_sleeping",
    "other",  # Include 'other'
    "prefer_not_to_answer"
  )

  # Generate `storing_barriers`
  storing_barriers <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(storing_barrier_options)), 1)  # Random number of barriers (1 to 3)
    paste(
      sample(storing_barrier_options, size = k, replace = FALSE),  # Randomly select barriers
      collapse = " "  # Combine them into a single string
    )
  })

  # Conditionally generate `storing_barriers_other` based on the presence of 'other'
  storing_barriers_other <- ifelse(
    grepl("\\bother\\b", storing_barriers),  # Check if 'other' is a whole word in `storing_barriers`
    paste0('storing_barriers_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other' is present
    NA_character_  # Otherwise, NA
  )
  lighting_abilities <- sample(c('yes', 'yes_w_issues', 'no'), n, replace = TRUE)

  # Define lighting barrier options including 'other'
  lighting_barrier_options <- c(
    "intermittent_insufficient",
    "no_electricity_or_lamp",
    "other",  # Include 'other'
    "prefer_not_to_answer"
  )

  # Generate `lighting_barriers`
  lighting_barriers <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(lighting_barrier_options)), 1)  # Random number of barriers (1 to 3)
    paste(
      sample(lighting_barrier_options, size = k, replace = FALSE),  # Randomly select barriers
      collapse = " "  # Combine them into a single string
    )
  })

  # Conditionally generate `lighting_barriers_other` based on the presence of 'other'
  lighting_barriers_other <- ifelse(
    grepl("\\bother\\b", lighting_barriers),  # Check if 'other' is a whole word in `lighting_barriers`
    paste0('lighting_barriers_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other' is present
    NA_character_  # Otherwise, NA
  )
  hygiene_abilities <- sample(c('yes', 'yes_w_issues', 'no'), n, replace = TRUE)

  # Define hygiene barrier options including 'other'
  hygiene_barrier_options <- c(
    "insuf_ess_hh_items_hygiene",
    "insufficient_space",
    "inade_space_lack_privacy",
    "unsafe_space",
    "no_hygiene_facility",
    "other",  # Include 'other'
    "prefer_not_to_answer"
  )

  # Generate `hygiene_barriers`
  hygiene_barriers <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(hygiene_barrier_options)), 1)  # Random number of barriers (1 to 3)
    paste(
      sample(hygiene_barrier_options, size = k, replace = FALSE),  # Randomly select barriers
      collapse = " "  # Combine them into a single string
    )
  })

  # Conditionally generate `hygiene_barriers_other` based on the presence of 'other'
  hygiene_barriers_other <- ifelse(
    grepl("\\bother\\b", hygiene_barriers),  # Check if 'other' is a whole word in `hygiene_barriers`
    paste0('hygiene_barriers_other_', sample(1:3, n, replace = TRUE)),  # Assign value if 'other' is present
    NA_character_  # Otherwise, NA
  )
  # Generate `death_any`
  death_any <- sample(
    c('yes', 'no'),
    n,
    replace = TRUE
  )

  # Conditionally generate `num_died` based on `death_any`
  num_died <- ifelse(
    death_any == 'yes',
    sample(0:2, n, replace = TRUE),  # Assign values if `death_any` is 'yes'
    NA_integer_  # Otherwise, NA
  )
  household_geopoint <- rep(NA_character_, n)

  # Create tibble
  data <- tibble(
    uuid = uuid,
    start = start,
    end = end,
    date_survey = date_survey,
    deviceid = deviceid,
    audit = audit,
    stratum = stratum,
    weight = weight,
    cluster_id_text = cluster_id_text,
    cluster_geopoint = cluster_geopoint,
    enumerator = enumerator,
    admin1 = admin1,
    admin2 = admin2,
    admin3 = admin3,
    admin4 = admin4,
    cluster = cluster,
    respondent_age = respondent_age,
    respondent_sex = respondent_sex,
    respondent_consent = respondent_consent,
    hohh_status = hohh_status,
    hohh_status_other = hohh_status_other,
    residency_status = residency_status,
    dod_idp_returnee = dod_idp_returnee,
    date_dod_idp_returnee = date_dod_idp_returnee,
    doa_idp_returnee = doa_idp_returnee,
    date_doa_idp_returnee = date_doa_idp_returnee,
    first_priority_need = first_priority_need,
    first_priority_need_other = first_priority_need_other,
    second_priority_need = second_priority_need,
    second_priority_need_other = second_priority_need_other,
    third_priority_need = third_priority_need,
    third_priority_need_other = third_priority_need_other,
    num_hh = num_hh,
    hd_nest_rely_assistant = hd_nest_rely_assistant,
    hd_nest_rely_assistant_num = hd_nest_rely_assistant_num,
    hd_nest_assistive_device = hd_nest_assistive_device,
    hd_nest_assistive_device_num = hd_nest_assistive_device_num,
    hd_nest_assist_information = hd_nest_assist_information,
    hd_nest_assist_information_num = hd_nest_assist_information_num,
    left_yn_known = left_yn_known,
    num_left = num_left,
    join_yn_known = join_yn_known,
    num_join = num_join,
    health_healthcare_barriers = health_healthcare_barriers,
    health_healthcare_barriers_other = health_healthcare_barriers_other,
    health_healthcare_travel_time = health_healthcare_travel_time,
    health_healthcare_travel_time_int = health_healthcare_travel_time_int,
    health_healthcare_travel_time_range = health_healthcare_travel_time_range,
    fsl_hdds_cereals = fsl_hdds_cereals,
    fsl_hdds_tubers = fsl_hdds_tubers,
    fsl_hdds_legumes = fsl_hdds_legumes,
    fsl_hdds_veg = fsl_hdds_veg,
    fsl_hdds_fruit = fsl_hdds_fruit,
    fsl_hdds_meat = fsl_hdds_meat,
    fsl_hdds_fish = fsl_hdds_fish,
    fsl_hdds_dairy = fsl_hdds_dairy,
    fsl_hdds_eggs = fsl_hdds_eggs,
    fsl_hdds_sugar = fsl_hdds_sugar,
    fsl_hdds_oil = fsl_hdds_oil,
    fsl_hdds_condiments = fsl_hdds_condiments,
    fsl_hhs_nofoodhh = fsl_hhs_nofoodhh,
    fsl_hhs_nofoodhh_freq = fsl_hhs_nofoodhh_freq,
    fsl_hhs_sleephungry = fsl_hhs_sleephungry,
    fsl_hhs_sleephungry_freq = fsl_hhs_sleephungry_freq,
    fsl_hhs_alldaynight = fsl_hhs_alldaynight,
    fsl_hhs_alldaynight_freq = fsl_hhs_alldaynight_freq,
    fsl_first_food_sources = fsl_first_food_sources,
    fsl_first_food_sources_other = fsl_first_food_sources_other,
    fsl_second_food_sources = fsl_second_food_sources,
    fsl_second_food_sources_other = fsl_second_food_sources_other,
    fsl_third_food_sources = fsl_third_food_sources,
    fsl_third_food_sources_other = fsl_third_food_sources_other,
    fsl_food_sources_barriers = fsl_food_sources_barriers,
    fsl_food_sources_barriers_other = fsl_food_sources_barriers_other,
    fsl_hh_clean_water_preparation = fsl_hh_clean_water_preparation,
    fsl_hh_clean_water_preparation_other = fsl_hh_clean_water_preparation_other,
    fsl_hh_access_cooking_energy = fsl_hh_access_cooking_energy,
    fsl_hh_access_cooking_energy_other = fsl_hh_access_cooking_energy_other,
    fsl_fcs_cereal = fsl_fcs_cereal,
    fsl_fcs_legumes = fsl_fcs_legumes,
    fsl_fcs_dairy = fsl_fcs_dairy,
    fsl_fcs_meat = fsl_fcs_meat,
    fsl_fcs_veg = fsl_fcs_veg,
    fsl_fcs_fruit = fsl_fcs_fruit,
    fsl_fcs_oil = fsl_fcs_oil,
    fsl_fcs_sugar = fsl_fcs_sugar,
    fsl_fcs_condiments = fsl_fcs_condiments,
    fsl_rcsi_lessquality = fsl_rcsi_lessquality,
    fsl_rcsi_borrow = fsl_rcsi_borrow,
    fsl_rcsi_mealsize = fsl_rcsi_mealsize,
    fsl_rcsi_mealadult = fsl_rcsi_mealadult,
    fsl_rcsi_mealnb = fsl_rcsi_mealnb,
    fsl_num_meals_above_5 = fsl_num_meals_above_5,
    fsl_num_meals_under_5 = fsl_num_meals_under_5,
    fsl_first_income_types = fsl_first_income_types,
    fsl_first_income_types_other = fsl_first_income_types_other,
    fsl_second_income_types = fsl_second_income_types,
    fsl_second_income_types_other = fsl_second_income_types_other,
    fsl_third_income_types = fsl_third_income_types,
    fsl_third_income_types_other = fsl_third_income_types_other,
    fsl_lcsi_stress1 = fsl_lcsi_stress1,
    fsl_lcsi_stress2 = fsl_lcsi_stress2,
    fsl_lcsi_stress3 = fsl_lcsi_stress3,
    fsl_lcsi_stress4 = fsl_lcsi_stress4,
    fsl_lcsi_crisis1 = fsl_lcsi_crisis1,
    fsl_lcsi_crisis2 = fsl_lcsi_crisis2,
    fsl_lcsi_crisis3 = fsl_lcsi_crisis3,
    fsl_lcsi_emergency1 = fsl_lcsi_emergency1,
    fsl_lcsi_emergency2 = fsl_lcsi_emergency2,
    fsl_lcsi_emergency3 = fsl_lcsi_emergency3,
    fsl_food_cash_voucher = fsl_food_cash_voucher,
    fsl_assistance_modality = fsl_assistance_modality,
    fsl_assistance_modality_other = fsl_assistance_modality_other,
    market_barriers = market_barriers,
    market_barriers_other = market_barriers_other,
    purchase_barriers = purchase_barriers,
    purchase_barriers_other = purchase_barriers_other,
    wash_water_source = wash_water_source,
    wash_water_source_other = wash_water_source_other,
    wash_other_water_sources = wash_other_water_sources,
    wash_different_water_sources = wash_different_water_sources,
    wash_different_water_sources_other = wash_different_water_sources_other,
    wash_first_water_source_usage = wash_first_water_source_usage,
    wash_first_water_source_usage_other = wash_first_water_source_usage_other,
    wash_second_water_source_usage = wash_second_water_source_usage,
    wash_second_water_source_usage_other = wash_second_water_source_usage_other,
    wash_third_water_source_usage = wash_third_water_source_usage,
    wash_third_water_source_usage_other = wash_third_water_source_usage_other,
    wash_water_collect_time = wash_water_collect_time,
    wash_water_collect_time_int = wash_water_collect_time_int,
    wash_water_collect_time_range = wash_water_collect_time_range,
    wash_water_treatment = wash_water_treatment,
    wash_water_treatment_other = wash_water_treatment_other,
    wash_water_interruption = wash_water_interruption,
    wash_water_interruption_num_days = wash_water_interruption_num_days,
    wash_containers = wash_containers,
    wash_num_containers = wash_num_containers,
    wash_soap_access = wash_soap_access,
    wash_wise_worry = wash_wise_worry,
    wash_wise_plans = wash_wise_plans,
    wash_wise_hands = wash_wise_hands,
    wash_wise_drink = wash_wise_drink,
    wash_mosquito_net = wash_mosquito_net,
    wash_source_mosquito_net = wash_source_mosquito_net,
    wash_sleep_mosquito_net_child = wash_sleep_mosquito_net_child,
    wash_toilet_facility = wash_toilet_facility,
    wash_toilet_facility_other = wash_toilet_facility_other,
    wash_share_toilet_facility = wash_share_toilet_facility,
    wash_num_share_toilet = wash_num_share_toilet,
    shape_shelter = shape_shelter,
    dimension_measure = dimension_measure,
    length_long = length_long,
    width_larg = width_larg,
    diameter = diameter,
    shelter_type = shelter_type,
    shelter_type_other = shelter_type_other,
    shelter_issues = shelter_issues,
    shelter_issues_other = shelter_issues_other,
    cooking_abilities = cooking_abilities,
    cooking_barriers = cooking_barriers,
    cooking_barriers_other = cooking_barriers_other,
    sleeping_abilities = sleeping_abilities,
    sleeping_barriers = sleeping_barriers,
    sleeping_barriers_other = sleeping_barriers_other,
    storing_abilities = storing_abilities,
    storing_barriers = storing_barriers,
    storing_barriers_other = storing_barriers_other,
    lighting_abilities = lighting_abilities,
    lighting_barriers = lighting_barriers,
    lighting_barriers_other = lighting_barriers_other,
    hygiene_abilities = hygiene_abilities,
    hygiene_barriers = hygiene_barriers,
    hygiene_barriers_other = hygiene_barriers_other,
    death_any = death_any,
    num_died = num_died,
    household_geopoint = household_geopoint
  )

  return(data)
}

# Function to introduce random errors (1-2 per variable)
introduce_errors <- function(data) {
  n <- nrow(data)
  vars <- setdiff(names(data), c('uuid', 'start', 'end'))

  for (var in vars) {
    # Introduce 1-2 errors per variable
    num_errors <- sample(1:2, 1)
    error_idx <- sample(1:n, min(num_errors, n), replace = FALSE)

    if (is.numeric(data[[var]])) {
      # For numeric: set to NA, negative, or very large value
      error_type <- sample(1:3, num_errors, replace = TRUE)
      for (i in seq_along(error_idx)) {
        if (error_type[i] == 1) {
          data[[var]][error_idx[i]] <- NA
        } else if (error_type[i] == 2 && is.integer(data[[var]])) {
          data[[var]][error_idx[i]] <- -999L
        } else {
          data[[var]][error_idx[i]] <- 99999
        }
      }
    } else {
      # For character: set to NA or invalid values
      error_type <- sample(1:3, num_errors, replace = TRUE)
      for (i in seq_along(error_idx)) {
        if (error_type[i] == 1) {
          data[[var]][error_idx[i]] <- NA_character_
        } else if (error_type[i] == 2) {
          data[[var]][error_idx[i]] <- "INVALID_VALUE"
        } else {
          data[[var]][error_idx[i]] <- ""
        }
      }
    }
  }

  return(data)
}

# Generate hh_roster repeat group dataset (100 rows)
# Now accepts household_data to match household size
generate_hh_roster_dataset <- function(household_data) {

  # Generate roster records based on household sizes
  roster_list <- lapply(seq_len(nrow(household_data)), function(i) {
    hh_row <- household_data[i, ]
    hh_size <- hh_row$num_hh

    if (is.na(hh_size) || hh_size <= 0) {
      return(NULL)
    }

    # Create roster entries for this household
    data.frame(
      hh_uuid = rep(hh_row$uuid, hh_size),
      ind_pos = seq_len(hh_size),
      stringsAsFactors = FALSE
    )
  })

  # Combine all roster records
  roster_base <- do.call(rbind, roster_list[!sapply(roster_list, is.null)])

  if (is.null(roster_base) || nrow(roster_base) == 0) {
    # If no valid households, create empty structure
    roster_base <- data.frame(
      hh_uuid = character(0),
      ind_pos = integer(0),
      stringsAsFactors = FALSE
    )
  }

  n <- nrow(roster_base)

  if (n == 0) {
    # Return empty tibble with correct structure
    return(tibble(
      hh_uuid = character(0),
      person_id = character(0),
      ind_pos = integer(0),
      date_survey = character(0),
      date_recall = character(0),
      sex = character(0),
      age_years = integer(0),
      respondent_age_roster = integer(0),
      respondent_sex_roster = character(0),
      know_dob = character(0),
      height_sticks = character(0),
      hh_roster_id = character(0),
      know_birth_date = character(0),
      ind_dob_exact = character(0),
      ind_dob_approx = character(0)
    ))
  }

  # Use roster_base for hh_uuid and ind_pos
  hh_uuid <- roster_base$hh_uuid
  ind_pos <- roster_base$ind_pos
  hh_roster_id <- paste0("HH_-", sprintf("%05d", 1:n))
  person_id <- paste0("PER-", sprintf("%05d", 1:n))

  # Base dates for context
  start_dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), length.out = n)
  recall_dates <- rep(as.Date("2023-10-01"), n)
  date_survey <- as.character(start_dates)
  date_recall <- as.character(recall_dates)
  sex <- sample(c('m', 'f', 'q'), n, replace = TRUE)
  respondent_age_roster <- sample(18:120, n, replace = TRUE)
  respondent_sex_roster <- sample(c('m', 'f'), n, replace = TRUE)
  know_dob <- sample(c('yes', 'no'), n, replace = TRUE)
  max_age_days <- 120 * 365  # Maximum age in days (120 years * 365 days)
  # Generate `know_birth_date`
  # Generate `age_years` values
  age_years <- sample(0:120, n, replace = TRUE)

  # Conditionally generate `know_birth_date` based on `age_years`
  know_birth_date <- ifelse(
    age_years <= 6,
    sample(c('yes', 'no'), n, replace = TRUE),  # Assign 'yes' or 'no' if age_years <= 6
    NA_character_  # Otherwise, NA
  )

  # Conditionally generate `ind_dob_exact` based on `know_birth_date` and `age_years`
  ind_dob_exact <- ifelse(
    age_years <= 6 & know_birth_date == 'yes',
    as.character(start_dates - sample(0:365, n, replace = TRUE)),  # Assign values if conditions are met
    NA_character_  # Otherwise, NA
  )

  # Conditionally generate `ind_dob_approx` based on `know_birth_date` and `age_years`
  ind_dob_approx <- ifelse(
    age_years <= 6 & know_birth_date == 'no',
    as.character(start_dates - sample(0:365, n, replace = TRUE)),  # Assign values if conditions are met
    NA_character_  # Otherwise, NA
  )

  # Conditionally generate `height_sticks` based on `know_birth_date`
  height_sticks <- ifelse(
    know_birth_date == 'no',
    sample(c('under6m', '6m_to_23m', '23m_to_59m', '60m_plus'), n, replace = TRUE),  # Assign values if `know_birth_date` is 'no'
    NA_character_  # Otherwise, NA
  )

  # Create tibble
  data <- tibble(
    hh_uuid = hh_uuid,
    person_id = person_id,
    ind_pos = ind_pos,
    date_survey = date_survey,
    date_recall = date_recall,
    sex = sex,
    age_years = age_years,
    respondent_age_roster = respondent_age_roster,
    respondent_sex_roster = respondent_sex_roster,
    know_dob = know_dob,
    height_sticks = height_sticks,
    hh_roster_id = hh_roster_id,
    know_birth_date = know_birth_date,
    ind_dob_exact = ind_dob_exact,
    ind_dob_approx = ind_dob_approx
  )

  return(data)
}

# Generate health_ind repeat group dataset (100 rows)
# Now accepts roster_data to match person demographics
generate_health_ind_dataset <- function(roster_data) {

  if (is.null(roster_data) || nrow(roster_data) == 0) {
    # Return empty tibble with correct structure
    return(tibble(
      hh_uuid = character(0),
      person_id = character(0),
      sex = character(0),
      age_years = integer(0),
      age_months = integer(0),
      health_ind_id = character(0),
      health_ind_illness = character(0),
      health_ind_symptom = character(0),
      health_ind_symptom_other = character(0),
      health_ind_received_healthcare = character(0),
      health_ind_healthcare_provider = character(0),
      health_ind_healthcare_provider_other = character(0),
      health_ind_health_care_alt_yn = character(0),
      health_ind_health_care_provider_alt = character(0),
      health_ind_health_care_provider_alt_other = character(0),
      health_ind_cholera_vaccination = character(0),
      health_ind_measles_vaccination = character(0),
      health_ind_vitamin_a_vaccination = character(0)
    ))
  }

  # Use roster data for person IDs, sex, and age
  n <- nrow(roster_data)
  hh_uuid <- roster_data$hh_uuid
  person_id <- roster_data$person_id
  health_ind_id <- paste0("HEA-", sprintf("%05d", 1:n))

  # Base dates for context
  start_dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), length.out = n)

  sex <- roster_data$sex
  age_years <- roster_data$age_years
  age_months <- age_years * 12 + sample(0:11, n, replace = TRUE)  # Calculate months from years
  health_ind_illness <- sample(c('yes', 'no', 'dont_know'), n, replace = TRUE)

  # Conditionally sample `health_ind_received_healthcare` only when `health_ind_illness` is 'yes'
  health_ind_received_healthcare <- ifelse(
    health_ind_illness == 'yes',
    sample(c('yes', 'no', 'dont_know'), n, replace = TRUE),  # Sample values if 'health_ind_illness' is 'yes'
    NA_character_  # Otherwise, assign NA
  )

  health_symptom_options <- c(
    "fever",
    "diarrhoea",
    "cough",
    "fast_difficult_breathing",
    "eye_infection",
    "skin_infection",
    "ear_infection",
    "rash",
    "other",
    "dont_know"
  )

  # Generate `health_ind_symptom`
  health_ind_symptom <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(health_symptom_options)), 1)  # Random number of symptoms (1 to 3)
    paste(
      sample(health_symptom_options, size = k, replace = FALSE),  # Randomly select symptoms
      collapse = " "  # Combine them into a single string
    )
  })



  # Conditionally generate `health_ind_symptom_other` based on the presence of 'other'
  health_ind_symptom_other <- ifelse(
    grepl("\\bother\\b", health_ind_symptom),  # Check if 'other' is a whole word in `health_ind_symptom`
    paste0('health_ind_symptom_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other' is present
    NA_character_  # Otherwise, NA
  )
  health_care_provider_options <- c(
    "government_hospital",
    "government_health_center",
    "government_health_post",
    "other_government_facility",
    "private_hospital",
    "private_clinic",
    "other_medical_facility",
    "ngo_hospital",
    "ngo_clinic",
    "traditional_healer"
  )
  # Generate `health_ind_healthcare_provider`
  health_ind_healthcare_provider <- sample(
    health_care_provider_options,
    n,
    replace = TRUE
  )

  # Conditionally generate `health_ind_healthcare_provider_other` based on `health_ind_healthcare_provider`
  health_ind_healthcare_provider_other <- ifelse(
    grepl("\\bother\\b", health_ind_healthcare_provider),  # Check if 'other' is part of the value
    paste0('health_ind_healthcare_provider_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other' is present
    NA_character_  # Otherwise, NA
  )
  health_ind_health_care_alt_yn <- ifelse(runif(n) < 0.01, sample(c('yes', 'no', 'dont_know'), n, replace = TRUE), NA_character_)


  health_ind_health_care_provider_alt <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(health_care_provider_options)), 1)  # scalar size
    paste(
      sample(health_care_provider_options, size = k, replace = FALSE),
      collapse = " "
    )
  })
  health_ind_health_care_provider_alt_other <- ifelse(runif(n) < 0.01, paste0('health_ind_health_care_provider_alt_other_', sample(1:100, n, replace = TRUE)), NA_character_)
  health_ind_cholera_vaccination <- ifelse(runif(n) < 0.01, sample(c('yes_recall', 'yes_card', 'no', 'dont_know'), n, replace = TRUE), NA_character_)
  health_ind_measles_vaccination <- ifelse(runif(n) < 0.01, sample(c('yes_recall', 'yes_card', 'no', 'dont_know'), n, replace = TRUE), NA_character_)
  health_ind_vitamin_a_vaccination <- ifelse(runif(n) < 0.01, sample(c('yes', 'no', 'dont_know'), n, replace = TRUE), NA_character_)

  # Create tibble
  data <- tibble(
    hh_uuid = hh_uuid,
    person_id = person_id,
    sex = sex,
    age_years = age_years,
    age_months = age_months,
    health_ind_id = health_ind_id,
    health_ind_illness = health_ind_illness,
    health_ind_symptom = health_ind_symptom,
    health_ind_symptom_other = health_ind_symptom_other,
    health_ind_received_healthcare = health_ind_received_healthcare,
    health_ind_healthcare_provider = health_ind_healthcare_provider,
    health_ind_healthcare_provider_other = health_ind_healthcare_provider_other,
    health_ind_health_care_alt_yn = health_ind_health_care_alt_yn,
    health_ind_health_care_provider_alt = health_ind_health_care_provider_alt,
    health_ind_health_care_provider_alt_other = health_ind_health_care_provider_alt_other,
    health_ind_cholera_vaccination = health_ind_cholera_vaccination,
    health_ind_measles_vaccination = health_ind_measles_vaccination,
    health_ind_vitamin_a_vaccination = health_ind_vitamin_a_vaccination
  )

  return(data)
}

# Generate water_count_loop repeat group dataset (100 rows)
# Now accepts household_data to match water container counts
generate_water_count_loop_dataset <- function(household_data) {

  # Generate water container records based on household wash_num_containers
  container_list <- lapply(seq_len(nrow(household_data)), function(i) {
    hh_row <- household_data[i, ]
    num_containers <- hh_row$wash_num_containers

    if (is.na(num_containers) || num_containers <= 0) {
      return(NULL)
    }

    # Create container entries for this household
    data.frame(
      hh_uuid = rep(hh_row$uuid, num_containers),
      stringsAsFactors = FALSE
    )
  })

  # Combine all container records
  container_base <- do.call(rbind, container_list[!sapply(container_list, is.null)])

  if (is.null(container_base) || nrow(container_base) == 0) {
    # Return empty tibble with correct structure
    return(tibble(
      hh_uuid = character(0),
      container_id = character(0),
      water_count_loop_id = character(0),
      wash_container_type = character(0),
      wash_container_type_other = character(0),
      wash_container_litre = integer(0),
      wash_container_num_journey = integer(0),
      wash_num_days_water_last_week = integer(0)
    ))
  }

  n <- nrow(container_base)

  # Link to parent household
  hh_uuid <- container_base$hh_uuid
  container_id <- paste0("CON-", sprintf("%05d", 1:n))
  water_count_loop_id <- paste0("WAT-", sprintf("%05d", 1:n))

  # Base dates for context
  start_dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), length.out = n)

  # Generate `wash_container_type` including the "other" option
  wash_container_type <- sample(
    c('bucket_20l', 'bucket_14l', 'rigig_jerry_can_20l', 'jerry_can_10l',
      'collapsible_jerry_can_5l', 'oil_jerry_can_5l', 'jug_2l', 'other'),  # Include 'other'
    n,
    replace = TRUE
  )

  # Conditionally generate `wash_container_type_other` based on the presence of "other"
  wash_container_type_other <- ifelse(
    wash_container_type == 'other',  # Check if 'other' is the value in `wash_container_type`
    paste0('wash_container_type_other_', sample(1:100, n, replace = TRUE)),  # Assign value if 'other'
    NA_character_  # Otherwise, NA
  )
  wash_container_litre <- sample(0:50, n, replace = TRUE)
  wash_container_num_journey <- sample(0:10, n, replace = TRUE)
  wash_num_days_water_last_week <- sample(0:7, n, replace = TRUE)

  # Create tibble
  data <- tibble(
    hh_uuid = hh_uuid,
    container_id = container_id,
    water_count_loop_id = water_count_loop_id,
    wash_container_type = wash_container_type,
    wash_container_type_other = wash_container_type_other,
    wash_container_litre = wash_container_litre,
    wash_container_num_journey = wash_container_num_journey,
    wash_num_days_water_last_week = wash_num_days_water_last_week
  )

  return(data)
}

# Generate child_nutrition repeat group dataset (100 rows)
# Now accepts roster_data to match under-5 children
generate_child_nutrition_dataset <- function(roster_data) {

  if (is.null(roster_data) || nrow(roster_data) == 0) {
    # Return empty tibble with correct structure
    return(tibble(
      hh_uuid = character(0),
      person_id = character(0),
      sex = character(0),
      age_months = integer(0),
      age_years = integer(0),
      child_nutrition_id = character(0),
      nut_child_present = character(0),
      nut_muac_cm = numeric(0),
      nut_muac_mm = numeric(0),
      nut_edema = character(0),
      nut_edema_confirm = character(0),
      nut_edema_image = character(0),
      nut_cmam_enrollment = character(0),
      nut_caregiver_present = character(0),
      nut_bf_yesterday = character(0),
      nut_bf_lack_reasons = character(0),
      nut_bf_lack_reasons_other = character(0),
      nut_food_child_consumptions = character(0),
      nut_food_child_consumptions_other = character(0),
      nut_complementary_feeding_challenges = character(0),
      nut_complementary_feeding_challenges_other = character(0),
      ecfies_s01 = character(0),
      ecfies_s02 = character(0),
      ecfies_s03 = character(0),
      ecfies_s04 = character(0),
      ecfies_s05 = character(0),
      ecfies_s06 = character(0),
      ecfies_s07 = character(0),
      ecfies_s08 = character(0)
    ))
  }

  # Filter roster to only children under 5 years old
  children_under_5 <- roster_data[!is.na(roster_data$age_years) & roster_data$age_years < 5, ]

  if (nrow(children_under_5) == 0) {
    # Return empty tibble with correct structure
    return(tibble(
      hh_uuid = character(0),
      person_id = character(0),
      sex = character(0),
      age_months = integer(0),
      age_years = integer(0),
      child_nutrition_id = character(0),
      nut_child_present = character(0),
      nut_muac_cm = numeric(0),
      nut_muac_mm = numeric(0),
      nut_edema = character(0),
      nut_edema_confirm = character(0),
      nut_edema_image = character(0),
      nut_cmam_enrollment = character(0),
      nut_caregiver_present = character(0),
      nut_bf_yesterday = character(0),
      nut_bf_lack_reasons = character(0),
      nut_bf_lack_reasons_other = character(0),
      nut_food_child_consumptions = character(0),
      nut_food_child_consumptions_other = character(0),
      nut_complementary_feeding_challenges = character(0),
      nut_complementary_feeding_challenges_other = character(0),
      ecfies_s01 = character(0),
      ecfies_s02 = character(0),
      ecfies_s03 = character(0),
      ecfies_s04 = character(0),
      ecfies_s05 = character(0),
      ecfies_s06 = character(0),
      ecfies_s07 = character(0),
      ecfies_s08 = character(0)
    ))
  }

  n <- nrow(children_under_5)

  # Use roster data for person IDs, sex, and age
  hh_uuid <- children_under_5$hh_uuid
  person_id <- children_under_5$person_id
  child_nutrition_id <- paste0("CHI-", sprintf("%05d", 1:n))

  # Base dates for context
  start_dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), length.out = n)

  sex <- children_under_5$sex
  age_years <- children_under_5$age_years
  # Sample age_months between 0 and 60, but respect age_years
  age_months <- age_years * 12 + sample(0:11, n, replace = TRUE)
  age_months <- pmin(age_months, 59)  # Cap at 59 months for under-5
  # Generate `nut_child_present`
  nut_child_present <- sample(
    c('yes', 'no', 'disabled'),
    n,
    replace = TRUE
  )

  nut_muac_cm <- ifelse(
    nut_child_present == 'yes',
    runif(n, 6, 20),  # Generate values if `nut_child_present` is 'yes'
    NA_real_  # Otherwise, NA
  )

  # Compute `nut_muac_mm` directly from `nut_muac_cm`
  nut_muac_mm <- ifelse(
    nut_child_present == 'yes',
    nut_muac_cm * 10,  # Convert cm to mm
    NA_real_  # Otherwise, NA
  )

  # Conditionally generate `nut_edema` based on `nut_child_present`
  nut_edema <- ifelse(
    nut_child_present == 'yes',
    sample(c('yes', 'no'), n, replace = TRUE),  # Assign values if `nut_child_present` is 'yes'
    NA_character_  # Otherwise, NA
  )

  # Conditionally generate `nut_edema_confirm` based on `nut_child_present`
  nut_edema_confirm <- ifelse(
    nut_child_present == 'yes',
    sample(c('yes', 'no'), n, replace = TRUE),  # Assign values if `nut_child_present` is 'yes'
    NA_character_  # Otherwise, NA
  )

  # Conditionally generate `nut_edema_image` based on `nut_child_present`
  nut_edema_image <- ifelse(
    nut_child_present == 'yes',
    paste0('image_', sample(1:100, n, replace = TRUE)),  # Assign values if `nut_child_present` is 'yes'
    NA_character_  # Otherwise, NA
  )
  nut_cmam_enrollment <- ifelse(runif(n) < 0.01, sample(c('none', 'otp', 'tsfp', 'sc', 'dontknow'), n, replace = TRUE), NA_character_)
  nut_caregiver_present <- sample(c('yes', 'no'), n, replace = TRUE)
  nut_bf_yesterday <- sample(c('yes', 'no', 'dont_know'), n, replace = TRUE)
  # Define breast-feeding lack reason options including "other"
  bf_lack_reason_options <- c(
    "mother_no_milk",
    "fed_other_substitutes",
    "fed_other_milk",
    "cultural_barriers",
    "mother_or_child_sick",
    "lack_of_time",
    "lack_of_info_import",
    "mother_pregnant",
    "influence_from_hh_memb",
    "other"  # Include 'other' option
  )

  # Generate `nut_bf_lack_reasons`
  nut_bf_lack_reasons <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(bf_lack_reason_options)), 1)  # Random number of reasons (1 to 3)
    paste(
      sample(bf_lack_reason_options, size = k, replace = FALSE),  # Randomly select reasons
      collapse = " "  # Combine them into a single string
    )
  })

  # Conditionally generate `nut_bf_lack_reasons_other` based on the presence of "other"
  nut_bf_lack_reasons_other <- ifelse(
    grepl("\\bother\\b", nut_bf_lack_reasons),  # Check if "other" is present as a whole word
    paste0('nut_bf_lack_reasons_other_', sample(1:100, n, replace = TRUE)),  # Assign value if "other" is present
    NA_character_  # Otherwise, NA
  )
  # Define child food consumption options including "other"
  child_food_consumption_options <- c(
    "breast_milk",
    "grain_tubers",
    "pulses_beans",
    "dairy",
    "meat",
    "eggs",
    "fruit_veg_vit_a",
    "fruit_veg_non_vit_a",
    "dont_know",
    "other"  # Include 'other' option
  )

  # Generate `nut_food_child_consumptions`
  nut_food_child_consumptions <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(child_food_consumption_options)), 1)  # Random number of consumption items (1 to 3)
    paste(
      sample(child_food_consumption_options, size = k, replace = FALSE),  # Randomly select consumption options
      collapse = " "  # Combine them into a single string
    )
  })

  # Conditionally generate `nut_food_child_consumptions_other` based on the presence of "other"
  nut_food_child_consumptions_other <- ifelse(
    grepl("\\bother\\b", nut_food_child_consumptions),  # Check if "other" is present as a whole word
    paste0('nut_food_child_consumptions_other_', sample(1:100, n, replace = TRUE)),  # Assign value if "other" is present
    NA_character_  # Otherwise, NA
  )
  # Define complementary feeding challenge options including "other"
  complementary_feeding_challenge_options <- c(
    "financial_barriers",
    "food_expensive",
    "lac_information_IYCF",
    "child_sick",
    "poor_hygien_lack_water",
    "lack_time_prepare_food",
    "lack_time_care_child",
    "lack_infor_import_feeding",
    "other",  # Include 'other' option
    "prefer_not_to_answer"
  )

  # Generate `nut_complementary_feeding_challenges`
  nut_complementary_feeding_challenges <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(complementary_feeding_challenge_options)), 1)  # Random number of challenges (1 to 3)
    paste(
      sample(complementary_feeding_challenge_options, size = k, replace = FALSE),  # Randomly select challenges
      collapse = " "  # Combine them into a single string
    )
  })

  # Conditionally generate `nut_complementary_feeding_challenges_other` based on the presence of "other"
  nut_complementary_feeding_challenges_other <- ifelse(
    grepl("\\bother\\b", nut_complementary_feeding_challenges),  # Check if "other" is present as a whole word
    paste0('nut_complementary_feeding_challenges_other_', sample(1:100, n, replace = TRUE)),  # Assign value if "other" is present
    NA_character_  # Otherwise, NA
  )
  ecfies_s01 <- sample(c('yes', 'no', 'dont_know', 'prefer_not_to_answer'), n, replace = TRUE)
  ecfies_s02 <- sample(c('yes', 'no', 'dont_know', 'prefer_not_to_answer'), n, replace = TRUE)
  ecfies_s03 <- sample(c('yes', 'no', 'dont_know', 'prefer_not_to_answer'), n, replace = TRUE)
  ecfies_s04 <- sample(c('yes', 'no', 'dont_know', 'prefer_not_to_answer'), n, replace = TRUE)
  ecfies_s05 <- sample(c('yes', 'no', 'dont_know', 'prefer_not_to_answer'), n, replace = TRUE)
  ecfies_s06 <- sample(c('yes', 'no', 'dont_know', 'prefer_not_to_answer'), n, replace = TRUE)
  ecfies_s07 <- sample(c('yes', 'no', 'dont_know', 'prefer_not_to_answer'), n, replace = TRUE)
  ecfies_s08 <- sample(c('yes', 'no', 'dont_know', 'prefer_not_to_answer'), n, replace = TRUE)

  # Create tibble
  data <- tibble(
    hh_uuid = hh_uuid,
    person_id = person_id,
    sex = sex,
    age_months = age_months,
    age_years = age_years,
    child_nutrition_id = child_nutrition_id,
    nut_child_present = nut_child_present,
    nut_muac_cm = nut_muac_cm,
    nut_muac_mm = nut_muac_mm,
    nut_edema = nut_edema,
    nut_edema_confirm = nut_edema_confirm,
    nut_edema_image = nut_edema_image,
    nut_cmam_enrollment = nut_cmam_enrollment,
    nut_caregiver_present = nut_caregiver_present,
    nut_bf_yesterday = nut_bf_yesterday,
    nut_bf_lack_reasons = nut_bf_lack_reasons,
    nut_bf_lack_reasons_other = nut_bf_lack_reasons_other,
    nut_food_child_consumptions = nut_food_child_consumptions,
    nut_food_child_consumptions_other = nut_food_child_consumptions_other,
    nut_complementary_feeding_challenges = nut_complementary_feeding_challenges,
    nut_complementary_feeding_challenges_other = nut_complementary_feeding_challenges_other,
    ecfies_s01 = ecfies_s01,
    ecfies_s02 = ecfies_s02,
    ecfies_s03 = ecfies_s03,
    ecfies_s04 = ecfies_s04,
    ecfies_s05 = ecfies_s05,
    ecfies_s06 = ecfies_s06,
    ecfies_s07 = ecfies_s07,
    ecfies_s08 = ecfies_s08
  )

  return(data)
}

# Generate women repeat group dataset (100 rows)
# Now accepts roster_data to match women aged 15-49
generate_women_dataset <- function(roster_data) {

  if (is.null(roster_data) || nrow(roster_data) == 0) {
    # Return empty tibble with correct structure
    return(tibble(
      uuid = character(0),
      person_id = character(0),
      women_id = character(0),
      woman_bf = character(0),
      woman_present = character(0),
      woman_muac_cm = numeric(0),
      plw_tsfp = character(0),
      food_distribution = character(0)
    ))
  }

  # Filter roster to only women aged 15-49
  # Assuming 'f' represents female in the sex column
  women_15_49 <- roster_data[!is.na(roster_data$sex) &
                             !is.na(roster_data$age_years) &
                             roster_data$sex == 'f' &
                             roster_data$age_years >= 15 &
                             roster_data$age_years <= 49, ]

  if (nrow(women_15_49) == 0) {
    # Return empty tibble with correct structure
    return(tibble(
      uuid = character(0),
      person_id = character(0),
      women_id = character(0),
      woman_bf = character(0),
      woman_present = character(0),
      woman_muac_cm = numeric(0),
      plw_tsfp = character(0),
      food_distribution = character(0)
    ))
  }

  n <- nrow(women_15_49)

  # Link to parent household (use hh_uuid from roster)
  uuid <- women_15_49$hh_uuid
  person_id <- women_15_49$person_id
  women_id <- paste0("WOM-", sprintf("%05d", 1:n))

  # Base dates for context
  start_dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), length.out = n)

  woman_bf_options <- c(
    "none",
    "current_pregnant",
    "bf_child_under_6m",
    "bf_child_under_23m"
  )

  woman_bf <- sapply(seq_len(n), function(i) {
    k <- sample.int(min(3, length(woman_bf_options)), 1)  # scalar size
    paste(
      sample(woman_bf_options, size = k, replace = FALSE),
      collapse = " "
    )
  })
  # Generate `woman_present`
  woman_present <- sample(
    c('yes', 'no'),
    n,
    replace = TRUE
  )

  # Conditionally generate `woman_muac_cm` based on `woman_present`
  woman_muac_cm <- ifelse(
    woman_present == 'yes',
    runif(n, 0.0, 100.0),  # Generate values if `woman_present` is 'yes'
    NA_real_  # Otherwise, NA
  )
  plw_tsfp <- sample(c('yes', 'no', 'dont_know'), n, replace = TRUE)
  food_distribution <- ifelse(runif(n) < 0.01, sample(c('yes', 'no', 'dont_know'), n, replace = TRUE), NA_character_)

  # Create tibble
  data <- tibble(
    uuid = uuid,
    women_id = women_id,
    woman_bf = woman_bf,
    woman_present = woman_present,
    woman_muac_cm = woman_muac_cm,
    plw_tsfp = plw_tsfp,
    food_distribution = food_distribution
  )

  return(data)
}

# Generate died_member repeat group dataset (100 rows)
# Now accepts household_data to match death counts
generate_died_member_dataset <- function(household_data) {

  # Generate death records based on household num_died
  death_list <- lapply(seq_len(nrow(household_data)), function(i) {
    hh_row <- household_data[i, ]
    num_deaths <- hh_row$num_died

    if (is.na(num_deaths) || num_deaths <= 0) {
      return(NULL)
    }

    # Create death entries for this household
    data.frame(
      hh_uuid = rep(hh_row$uuid, num_deaths),
      stringsAsFactors = FALSE
    )
  })

  # Combine all death records
  death_base <- do.call(rbind, death_list[!sapply(death_list, is.null)])

  if (is.null(death_base) || nrow(death_base) == 0) {
    # Return empty tibble with correct structure
    return(tibble(
      hh_uuid = character(0),
      death_id = character(0),
      recall_date = character(0),
      sex_died = character(0),
      age_died_years = integer(0),
      age_months_died = integer(0),
      known_dob_died = character(0),
      dob_died_exact = character(0),
      dob_died_approx = character(0),
      died_present = character(0),
      date_death_yn = character(0),
      date_death_exact = character(0),
      date_death_approx = character(0),
      cause_death = character(0),
      cause_death_other = character(0),
      location_death = character(0),
      location_death_other = character(0),
      died_healthcare_yn = character(0),
      died_healthcare_location = character(0),
      died_healthcare_location_other = character(0),
      died_no_healthcare = character(0),
      died_no_healthcare_other = character(0),
      death_details = character(0)
    ))
  }

  n <- nrow(death_base)

  # Link to parent household
  hh_uuid <- death_base$hh_uuid
  death_id <- paste0("DIE-", sprintf("%05d", 1:n))

  recall_date <- sample(c('2023-01-01'), n, replace = TRUE)
  # Base dates for context
  start_dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-01"), length.out = n)

  sex_died <- sample(c('m', 'f'), n, replace = TRUE)
  # Generate `age_died_years`
  # Generate `age_died_years`
  age_died_years <- sample(0:120, n, replace = TRUE)

  # Conditionally generate `known_dob_died` based on `age_died_years`
  known_dob_died <- ifelse(
    age_died_years <= 6,  # Only assign a value if `age_died_years` is 6 or less
    sample(c('yes', 'no'), n, replace = TRUE),
    NA_character_  # Otherwise, NA
  )

  # Generate `age_months_died` based on the given conditions
  age_months_died <- ifelse(
    age_died_years <= 6 & !is.na(known_dob_died),  # Only generate if `age_died_years` <= 6 and `known_dob_died` is not NA
    age_died_years * 12,  # Convert age in years to age in months
    NA_integer_  # Otherwise, NA
  )

  # Generate the exact or approximate date of birth based on `age_died_years` and `known_dob_died`
  dob_died_exact <- ifelse(
    known_dob_died == 'yes',  # If exact date of birth is known
    as.character(start_dates - (age_died_years * 365 + sample(0:30, n, replace = TRUE))),  # Adjust by age and random days for precision
    NA_character_  # Otherwise, NA
  )

  dob_died_approx <- ifelse(
    known_dob_died == 'no',  # If approximate date of birth is known
    as.character(start_dates - (age_died_years * 365 + sample(0:30, n, replace = TRUE))),  # Adjust by age and random variation
    NA_character_  # Otherwise, NA
  )

  # Update `age_months_died` based on the presence of exact or approximate date of birth
  age_months_died <- ifelse(
    !is.na(dob_died_exact) & known_dob_died == 'yes' & age_died_years <= 6,
    as.numeric(difftime(start_dates, as.Date(dob_died_exact), units = "weeks")) %/% 4.345,  # Use exact date
    ifelse(
      !is.na(dob_died_approx) & known_dob_died == 'no' & age_died_years <= 6,
      as.numeric(difftime(start_dates, as.Date(dob_died_approx), units = "weeks")) %/% 4.345,  # Use approximate date
      age_months_died  # Default to existing value if no conditions are met
    )
  )

  died_present <- sample(c('yes', 'no'), n, replace = TRUE)
  # Generate `date_death_yn`
  date_death_yn <- sample(
    c('yes', 'no'),
    n,
    replace = TRUE
  )

  # Conditionally generate `date_death_exact` based on `date_death_yn`
  date_death_exact <- ifelse(
    date_death_yn == 'yes',
    as.character(start_dates - sample(0:365, n, replace = TRUE)),  # Assign values if `date_death_yn` is 'yes'
    NA_character_  # Otherwise, NA
  )

  # Conditionally generate `date_death_approx` based on `date_death_yn`
  date_death_approx <- ifelse(
    date_death_yn == 'no',
    as.character(start_dates - sample(0:365, n, replace = TRUE)),  # Assign values if `date_death_yn` is 'no'
    NA_character_  # Otherwise, NA
  )
  # Generate `cause_death` including the "other" option
  cause_death <- sample(
    c('acute_disease', 'chronic_disease', 'violence', 'accident',
      'post_partum', 'during_pregnancy', 'during_delivery', 'other', 'dont_know'),
    n,
    replace = TRUE
  )

  # Conditionally generate `cause_death_other` based on `cause_death`
  cause_death_other <- ifelse(
    cause_death == 'other',  # Check if `cause_death` is "other"
    paste0('cause_death_other_', sample(1:100, n, replace = TRUE)),  # Assign value if "other"
    NA_character_  # Otherwise, NA
  )
  # Generate `location_death` including the "other" option
  location_death <- sample(
    c('current_location_residence', 'current_location_healthfacility', 'migration',
      'last_location_residence', 'last_location_healthfacility', 'other', 'dont_know'),
    n,
    replace = TRUE
  )

  # Conditionally generate `location_death_other` based on `location_death`
  location_death_other <- ifelse(
    location_death == 'other',  # Check if `location_death` is "other"
    paste0('location_death_other_', sample(1:100, n, replace = TRUE)),  # Assign value if "other"
    NA_character_  # Otherwise, NA
  )
  # Generate `died_healthcare_yn`
  died_healthcare_yn <- sample(
    c('yes', 'no'),
    n,
    replace = TRUE
  )

  # Conditionally generate `died_healthcare_location` based on `died_healthcare_yn`
  died_healthcare_location <- ifelse(
    died_healthcare_yn == 'yes',
    sample(c('government_hospital', 'government_health_center', 'government_health_post',
             'other_government_facility', 'private_hospital', 'private_clinic',
             'other_medical_facility', 'ngo_hospital', 'ngo_clinic', 'traditional_healer'),
           n, replace = TRUE),
    NA_character_  # If `died_healthcare_yn` is "no", set to NA
  )

  # Conditionally generate `died_healthcare_other` based on "other" in `died_healthcare_location`
  died_healthcare_other <- ifelse(
    grepl("\\bother\\b", died_healthcare_location),
    paste0('died_healthcare_other_', sample(1:100, n, replace = TRUE)),
    NA_character_
  )

  # Conditionally generate `died_no_healthcare` based on `died_healthcare_yn`
  died_no_healthcare <- ifelse(
    died_healthcare_yn == 'no',
    sample(c('death', 'too_expensive', 'too_sick', 'not_sick', 'too_far',
             'traditional_healer', 'no_time', 'no_trust', 'safety_issue', 'care_refused', 'other'),
           n, replace = TRUE),
    NA_character_  # If `died_healthcare_yn` is "yes", set to NA
  )

  # Conditionally generate `died_no_healthcare_other` based on "other" in `died_no_healthcare`
  died_no_healthcare_other <- ifelse(
    grepl("\\bother\\b", died_no_healthcare),
    paste0('died_no_healthcare_other_', sample(1:100, n, replace = TRUE)),
    NA_character_
  )
  death_details <- paste0('death_details_', sample(1:100, n, replace = TRUE))

  # Create tibble
  data <- tibble(
    hh_uuid = hh_uuid,
    death_id = death_id,
    recall_date = recall_date,
    sex_died = sex_died,
    age_died_years = age_died_years,
    age_months_died = age_months_died,
    known_dob_died = known_dob_died,
    dob_died_exact = dob_died_exact,
    dob_died_approx = dob_died_approx,
    died_present = died_present,
    date_death_yn = date_death_yn,
    date_death_exact = date_death_exact,
    date_death_approx = date_death_approx,
    cause_death = cause_death,
    cause_death_other = cause_death_other,
    location_death = location_death,
    location_death_other = location_death_other,
    died_healthcare_yn = died_healthcare_yn,
    died_healthcare_location = died_healthcare_location,
    died_healthcare_other = died_healthcare_other,
    died_no_healthcare = died_no_healthcare,
    died_no_healthcare_other = died_no_healthcare_other,
    death_details = death_details
  )

  return(data)
}

# # Generate and save datasets
# cat("Generating household dataset (500 records)...\n")
# household_data <- generate_household_dataset(n)
#
# cat("Introducing random errors (1-2 per variable)...\n")
# household_data <- introduce_errors(household_data)
#
# # Save main dataset
# output_dir <- "resources"
# if (!dir.exists(output_dir)) {
#   dir.create(output_dir, recursive = TRUE)
# }
#
# main_path <- file.path(output_dir, "household_sample_data.csv")
# write.csv(household_data, main_path, row.names = FALSE, na = "")
# cat(sprintf("Main dataset saved: %s (%d records, %d variables)\n", main_path, nrow(household_data), ncol(household_data)))
#
# # Generate repeat group datasets - now linked to household data
# cat("Generating hh_roster dataset (based on household sizes)...\n")
# hh_roster_data <- generate_hh_roster_dataset(household_data)
# hh_roster_data <- introduce_errors(hh_roster_data)
# hh_roster_path <- file.path(output_dir, "hh_roster_sample_data.csv")
# write.csv(hh_roster_data, hh_roster_path, row.names = FALSE, na = "")
# cat(sprintf("hh_roster dataset saved: %s (%d records, %d variables)\n", hh_roster_path, nrow(hh_roster_data), ncol(hh_roster_data)))
#
# cat("Generating health_ind dataset (based on roster)...\n")
# health_ind_data <- generate_health_ind_dataset(hh_roster_data)
# health_ind_data <- introduce_errors(health_ind_data)
# health_ind_path <- file.path(output_dir, "health_ind_sample_data.csv")
# write.csv(health_ind_data, health_ind_path, row.names = FALSE, na = "")
# cat(sprintf("health_ind dataset saved: %s (%d records, %d variables)\n", health_ind_path, nrow(health_ind_data), ncol(health_ind_data)))
#
# cat("Generating water_count_loop dataset (based on household container counts)...\n")
# water_count_loop_data <- generate_water_count_loop_dataset(household_data)
# water_count_loop_data <- introduce_errors(water_count_loop_data)
# water_count_loop_path <- file.path(output_dir, "water_count_loop_sample_data.csv")
# write.csv(water_count_loop_data, water_count_loop_path, row.names = FALSE, na = "")
# cat(sprintf("water_count_loop dataset saved: %s (%d records, %d variables)\n", water_count_loop_path, nrow(water_count_loop_data), ncol(water_count_loop_data)))
#
# cat("Generating child_nutrition dataset (based on roster, under-5 only)...\n")
# child_nutrition_data <- generate_child_nutrition_dataset(hh_roster_data)
# child_nutrition_data <- introduce_errors(child_nutrition_data)
# child_nutrition_path <- file.path(output_dir, "child_nutrition_sample_data.csv")
# write.csv(child_nutrition_data, child_nutrition_path, row.names = FALSE, na = "")
# cat(sprintf("child_nutrition dataset saved: %s (%d records, %d variables)\n", child_nutrition_path, nrow(child_nutrition_data), ncol(child_nutrition_data)))
#
# cat("Generating women dataset (based on roster, women 15-49 only)...\n")
# women_data <- generate_women_dataset(hh_roster_data)
# women_data <- introduce_errors(women_data)
# women_path <- file.path(output_dir, "women_sample_data.csv")
# write.csv(women_data, women_path, row.names = FALSE, na = "")
# cat(sprintf("women dataset saved: %s (%d records, %d variables)\n", women_path, nrow(women_data), ncol(women_data)))
#
# cat("Generating died_member dataset (based on household death counts)...\n")
# died_member_data <- generate_died_member_dataset(household_data)
# died_member_data <- introduce_errors(died_member_data)
# died_member_path <- file.path(output_dir, "died_member_sample_data.csv")
# write.csv(died_member_data, died_member_path, row.names = FALSE, na = "")
# cat(sprintf("died_member dataset saved: %s (%d records, %d variables)\n", died_member_path, nrow(died_member_data), ncol(died_member_data)))
#
# cat("\nAll datasets generated successfully!\n")

# ==============================================================================
# Smart Wrapper Functions for Backward Compatibility
# ==============================================================================
# These wrappers detect whether the old (numeric n) or new (data frame)
# parameter is provided and handle both cases appropriately.

# Store the original new functions with different names
generate_hh_roster_dataset_new <- generate_hh_roster_dataset
generate_health_ind_dataset_new <- generate_health_ind_dataset
generate_child_nutrition_dataset_new <- generate_child_nutrition_dataset
generate_women_dataset_new <- generate_women_dataset
generate_water_count_loop_dataset_new <- generate_water_count_loop_dataset
generate_died_member_dataset_new <- generate_died_member_dataset

# Override with smart wrappers

#' @title Generate roster dataset (smart wrapper)
#' @description Detects parameter type and calls appropriate function
#' @param household_data_or_n Either household data frame or numeric n
#' @return Roster dataset
generate_hh_roster_dataset <- function(household_data_or_n) {
  if (is.numeric(household_data_or_n)) {
    # Old style: n parameter
    n <- household_data_or_n
    households_needed <- ceiling(n / 5)
    mock_household <- tibble(
      uuid = paste0("HH-", sprintf("%05d", 1:households_needed)),
      num_hh = rep(ceiling(n / households_needed), households_needed)
    )
    # Adjust last household to get exact n
    total_so_far <- sum(mock_household$num_hh[-households_needed])
    mock_household$num_hh[households_needed] <- n - total_so_far
    generate_hh_roster_dataset_new(mock_household)
  } else {
    # New style: household data
    generate_hh_roster_dataset_new(household_data_or_n)
  }
}

#' @title Generate health individual dataset (smart wrapper)
#' @description Detects parameter type and calls appropriate function
#' @param roster_data_or_n Either roster data frame or numeric n
#' @param hh_uuids Optional household UUIDs (for old style)
#' @return Health individual dataset
generate_health_ind_dataset <- function(roster_data_or_n, hh_uuids = NULL) {
  if (is.numeric(roster_data_or_n)) {
    # Old style: n parameter
    n <- roster_data_or_n
    if (is.null(hh_uuids)) {
      households_needed <- ceiling(n / 5)
      hh_uuids <- paste0("HH-", sprintf("%05d", 1:households_needed))
    }
    mock_household <- tibble(
      uuid = hh_uuids,
      num_hh = rep(ceiling(n / length(hh_uuids)), length(hh_uuids))
    )
    roster <- generate_hh_roster_dataset_new(mock_household)
    roster <- roster[1:min(n, nrow(roster)), ]
    generate_health_ind_dataset_new(roster)
  } else {
    # New style: roster data
    generate_health_ind_dataset_new(roster_data_or_n)
  }
}

#' @title Generate child nutrition dataset (smart wrapper)
#' @description Detects parameter type and calls appropriate function
#' @param roster_data_or_n Either roster data frame or numeric n
#' @param hh_uuids Optional household UUIDs (for old style)
#' @return Child nutrition dataset
generate_child_nutrition_dataset <- function(roster_data_or_n, hh_uuids = NULL) {
  if (is.numeric(roster_data_or_n)) {
    # Old style: n parameter
    n <- roster_data_or_n
    if (is.null(hh_uuids)) {
      hh_uuids <- paste0("HH-", sprintf("%05d", 1:ceiling(n / 2)))
    }
    mock_roster <- tibble(
      hh_uuid = sample(hh_uuids, n, replace = TRUE),
      person_id = paste0("PER-", sprintf("%05d", 1:n)),
      sex = sample(c('m', 'f'), n, replace = TRUE),
      age_years = sample(0:4, n, replace = TRUE)
    )
    generate_child_nutrition_dataset_new(mock_roster)
  } else {
    # New style: roster data
    generate_child_nutrition_dataset_new(roster_data_or_n)
  }
}

#' @title Generate women dataset (smart wrapper)
#' @description Detects parameter type and calls appropriate function
#' @param roster_data_or_n Either roster data frame or numeric n
#' @param hh_uuids Optional household UUIDs (for old style)
#' @return Women dataset
generate_women_dataset <- function(roster_data_or_n, hh_uuids = NULL) {
  if (is.numeric(roster_data_or_n)) {
    # Old style: n parameter
    n <- roster_data_or_n
    if (is.null(hh_uuids)) {
      hh_uuids <- paste0("HH-", sprintf("%05d", 1:ceiling(n / 2)))
    }
    mock_roster <- tibble(
      hh_uuid = sample(hh_uuids, n, replace = TRUE),
      person_id = paste0("PER-", sprintf("%05d", 1:n)),
      sex = rep('f', n),
      age_years = sample(15:49, n, replace = TRUE)
    )
    generate_women_dataset_new(mock_roster)
  } else {
    # New style: roster data
    generate_women_dataset_new(roster_data_or_n)
  }
}

#' @title Generate water container dataset (smart wrapper)
#' @description Detects parameter type and calls appropriate function
#' @param household_data_or_n Either household data frame or numeric n
#' @param hh_uuids Optional household UUIDs (for old style)
#' @return Water container dataset
generate_water_count_loop_dataset <- function(household_data_or_n, hh_uuids = NULL) {
  if (is.numeric(household_data_or_n)) {
    # Old style: n parameter
    n <- household_data_or_n
    if (is.null(hh_uuids)) {
      households_needed <- ceiling(n / 3)
      hh_uuids <- paste0("HH-", sprintf("%05d", 1:households_needed))
    } else {
      households_needed <- length(hh_uuids)
    }
    mock_household <- tibble(
      uuid = hh_uuids,
      wash_num_containers = rep(ceiling(n / households_needed), households_needed)
    )
    # Adjust last household to get exact n
    total_so_far <- sum(mock_household$wash_num_containers[-households_needed])
    mock_household$wash_num_containers[households_needed] <- n - total_so_far
    generate_water_count_loop_dataset_new(mock_household)
  } else {
    # New style: household data
    generate_water_count_loop_dataset_new(household_data_or_n)
  }
}

#' @title Generate death dataset (smart wrapper)
#' @description Detects parameter type and calls appropriate function
#' @param household_data_or_n Either household data frame or numeric n
#' @param hh_uuids Optional household UUIDs (for old style)
#' @return Death dataset
generate_died_member_dataset <- function(household_data_or_n, hh_uuids = NULL) {
  if (is.numeric(household_data_or_n)) {
    # Old style: n parameter
    n <- household_data_or_n
    if (is.null(hh_uuids)) {
      households_needed <- ceiling(n / 2)
      hh_uuids <- paste0("HH-", sprintf("%05d", 1:households_needed))
    } else {
      households_needed <- length(hh_uuids)
    }
    mock_household <- tibble(
      uuid = hh_uuids,
      num_died = rep(ceiling(n / households_needed), households_needed)
    )
    # Adjust last household to get exact n
    total_so_far <- sum(mock_household$num_died[-households_needed])
    mock_household$num_died[households_needed] <- n - total_so_far
    generate_died_member_dataset_new(mock_household)
  } else {
    # New style: household data
    generate_died_member_dataset_new(household_data_or_n)
  }
}
