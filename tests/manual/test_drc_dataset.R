
rm(list = ls())

devtools::load_all()
library(tibble)
library(dplyr)
library(phrutils)
library(phrindicators)

#Install and setup

df_hh <- readxl::read_xlsx("REACH_DRC2603_Dataset_MSNA_2026.xlsx", sheet = "hh clean data") |>
  dplyr::mutate(today = phrutils::phr_convert_date(today),
                start = phrutils::phr_convert_datetime(start),
                end = phrutils::phr_convert_datetime(end))



df_roster <- readxl::read_xlsx("REACH_DRC2603_Dataset_MSNA_2026.xlsx", sheet = "demo clean data") |>
  dplyr::mutate(ind_gender = dplyr::case_when(
    ind_gender == "male" ~ "m",
    ind_gender == "female" ~ "f",
    TRUE ~ NA_character_
  ))

df_death <- readxl::read_xlsx("REACH_DRC2603_Dataset_MSNA_2026.xlsx", sheet = "died clean data") |>
  dplyr::mutate(sex_died = dplyr::case_when(
    sex_died == "male" ~ "m",
    sex_died == "female" ~ "f",
    TRUE ~ NA_character_
  ))

drc_mort <- phr::HouseholdData$new(data = df_hh, dataset_name = "DRC MSNA 2026 Household")

# write.csv(x = drc_mort$export_variable_schema(), file = "hh_variable_schema.csv")

drc_mort$import_variable_schema(df = read.csv(file = "hh_variable_schema.csv"))


roster <- phr::IndividualData$new(
  data = df_roster,
  dataset_name = "DRC MSNA 2026 Roster"
  )

roster$import_variable_schema(df = read.csv(file = "roster_variable_schema.csv"))

deaths <- DeathIndividualData$new(
  data = df_death,
  recall_date = "2026-01-01",
  dataset_name = "DRC MSNA 2026 Deaths",
  variable_map = list(
    uuid = "index",
    date_recall = "recall_date")
)


drc_mort$add_linked_dataset(
  name = "roster",
  data_object = roster
)

drc_mort$add_linked_dataset(
  name = "deaths",
  data_object = deaths
)


# write.csv(x = roster$export_variable_schema(), file = "roster_variable_schema.csv")
# View(roster$export_indicator_schema())
# View(roster$export_variable_schema())
# View(roster$data_diagnose(stage = "raw"))
# roster$standardize()
# View(roster$data_diagnose(stage = "standardized"))
# write.csv(x = drc_mort$export_variable_schema(), file = "hh_variable_schema.csv")
# write.csv(x = drc_mort$linked_objects$deaths$object$export_variable_schema(), file = "deaths_variable_schema.csv")



# Standardize and Transform Data

drc_mort$standardize()

# Check Mappings

View(drc_mort$data_diagnose(stage = "raw"))
View(drc_mort$linked_objects$roster$object$data_diagnose(stage = "raw"))
View(drc_mort$linked_objects$deaths$object$data_diagnose(stage = "raw"))

drc_mort$standardize()

View(drc_mort$data_diagnose(stage = "standardized"))
View(drc_mort$linked_objects$roster$object$data_diagnose(stage = "standardized"))
View(drc_mort$linked_objects$deaths$object$data_diagnose(stage = "standardized"))

# Cleaning

drc_mort$generate_cleaning_log()

View(drc_mort$cleaning_log$get(field = "log_df"))
View(drc_mort$linked_objects$roster$object$cleaning_log$get(field = "log_df"))
View(drc_mort$linked_objects$deaths$object$cleaning_log$get(field = "log_df"))

drc_mort$clean()

# Produce Data Analytics

mortality_analyics <- drc_mort$generate_data_analytics(
  stage = "standardized",
  type = "mortality"
)


mortality_analyics$analysis_diagnose()
View(mortality_analyics$analysis_plan_issues_log)
View(mortality_analyics$data_analysis_plan$get(field = "log_df"))

mortality_analyics$run_analysis()

mortality_analyics$quality_diagnose()
View(mortality_analyics$quality_issues_log)
mortality_analyics$run_quality_checks()

mortality_analyics$tables$plausibility$penalty_summary

mortality_analyics$tables$plausibility$penalty_summary_stratum_aru

mortality_analyics$outputs_diagnose()
View(mortality_analyics$outputs_issues_log)
mortality_analyics$run_outputs()

View(mortality_analyics$analysis_results$household$survey_design)
View(mortality_analyics$analysis_results$deaths$base)

mortality_analyics$analysis_plan_issue_log

mortality_analyics$tables$plausibility$penalty_summary
mortality_analyics$visualizations$deaths$weighted

mortality_analyics$tables$roster$mortality_analyics$visualizations$roster$age_pyramid_overall_unweighted
mortality_analyics$visualizations$roster$age_pyramid_overall_weighted
mortality_analyics$visualizations$roster$age_months_distribution_unweighted

mortality_analyics$tables$roster
mortality_analyics$tables$roster$basic_demo_table_weighted
mortality_analyics$tables$roster$basic_demo_table_strata_weighted


(a <- sum(mortality_analyics$data$linked_person_time))
(b <- sum(mortality_analyics$data$linked_person_time_female))
(c <- sum(mortality_analyics$data$linked_person_time_male))
(d <- sum(mortality_analyics$data$linked_person_time_under5))
