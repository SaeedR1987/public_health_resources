rm(list = ls())


library(tibble)
library(dplyr)
library(phrutils)
library(phrindicators)
devtools::load_all()

#Install and setup

df_hh <- readxl::read_xlsx(
  "REACH_DRC2603_Dataset_MSNA_2026.xlsx",
  sheet = "hh clean data"
) |>
  dplyr::mutate(
    today = phrutils::phr_convert_date(today),
    start = phrutils::phr_convert_datetime(start),
    end = phrutils::phr_convert_datetime(end)
  )


df_roster <- readxl::read_xlsx(
  "REACH_DRC2603_Dataset_MSNA_2026.xlsx",
  sheet = "demo clean data"
) |>
  dplyr::mutate(
    ind_gender = dplyr::case_when(
      ind_gender == "male" ~ "m",
      ind_gender == "female" ~ "f",
      TRUE ~ NA_character_
    )
  )

df_death <- readxl::read_xlsx(
  "REACH_DRC2603_Dataset_MSNA_2026.xlsx",
  sheet = "died clean data"
) |>
  dplyr::mutate(
    sex_died = dplyr::case_when(
      sex_died == "m" ~ "m",
      sex_died == "f" ~ "f",
      TRUE ~ NA_character_
    )
  )

drc_mort <- phr::HouseholdData$new(
  data = df_hh,
  dataset_name = "DRC MSNA 2026 Household"
)

# write.csv(x = drc_mort$export_variable_schema(), file = "hh_variable_schema.csv")

drc_mort$set(
  field = "..variable_schema",
  value = read.csv(file = "hh_variable_schema.csv")
)

roster <- phr::IndividualData$new(
  data = df_roster,
  dataset_name = "DRC MSNA 2026 Roster"
)

roster$set(
  field = "..variable_schema",
  value = read.csv(file = "roster_variable_schema.csv")
)

deaths <- DeathIndividualData$new(
  data = df_death,
  recall_date = "2026-01-01",
  dataset_name = "DRC MSNA 2026 Deaths",
  uuid = "index"
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
View(drc_mort$linked_objects$roster$object$data_diagnose(
  stage = "standardized"
))
View(drc_mort$linked_objects$deaths$object$data_diagnose(
  stage = "standardized"
))

View(drc_mort$linked_objects$deaths$object$standardized_data)

# Cleaning

drc_mort$generate_cleaning_log()

View(drc_mort$cleaning_log$get(field = "log_df"))
View(drc_mort$linked_objects$roster$object$cleaning_log$get(field = "log_df"))
View(drc_mort$linked_objects$deaths$object$cleaning_log$get(field = "log_df"))

drc_mort$clean()

# Produce Data Analytics

mortality_analytics <- drc_mort$generate_data_analytics(
  stage = "standardized",
  type = "mortality"
)

# testing analysis
mortality_analytics$analysis_diagnose()
head(mortality_analytics$analysis_plan_issues_log)
head(mortality_analytics$analysis_schema)
head(mortality_analytics$analysis_schema_roster)
head(mortality_analytics$analysis_schema_deaths)
head(mortality_analytics$data_analysis_plan$get(field = "log_df"))
mortality_analytics$run_analysis()

head(mortality_analytics$analysis_results$household$base)
head(mortality_analytics$analysis_results$household$survey_design)

head(mortality_analytics$analysis_results$roster$base)
head(mortality_analytics$analysis_results$roster$survey_design)

head(mortality_analytics$analysis_results$deaths$base)
head(mortality_analytics$analysis_results$deaths$survey_design)

# testing plausibility
mortality_analytics$quality_diagnose()
head(mortality_analytics$quality_issues_log)
head(mortality_analytics$quality_schema)

mortality_analytics$run_quality_checks()

mortality_analytics$tables$plausibility$penalty_summary
mortality_analytics$tables$plausibility$penalty_summary_stratum_aru

# testing outputs

mortality_analytics$outputs_diagnose()
head(mortality_analytics$outputs_issues_log)
head(mortality_analytics$outputs_schema)

head(mortality_analytics$default_outputs_schema())
head(mortality_analytics$outputs_schema_roster)
head(mortality_analytics$outputs_schema_deaths)

mortality_analytics$run_outputs()

mortality_analytics$tables$roster$unweighted$basic_demo_table
mortality_analytics$tables$roster$unweighted$basic_demo_table_strata
mortality_analytics$tables$roster$unweighted$basic_demo_table_weighted
mortality_analytics$tables$roster$unweighted$sex_age_cat_demo_table
mortality_analytics$tables$roster$unweighted$sex_age_months_cat_demo_table

mortality_analytics$visualizations$roster$unweighted$age_pyramid_overall_unweighted
mortality_analytics$visualizations$roster$unweighted$age_months_distribution_unweighted
mortality_analytics$visualizations$roster$unweighted$age_months_cat_donut_unweighted

mortality_analytics$tables$deaths$weighted


mortality_analytics$visualizations$roster$weighted$

mortality_analytics$visualizations$roster$weighted

mortality_analytics$visualizations$roster$unweighted$age_pyramid_overall_unweighted

mortality_analytics$variable_map$stratum
table(mortality_analytics$data$admin3)


View(mortality_analytics$quality_issues_log)


mortality_analytics$outputs_diagnose()
View(mortality_analytics$outputs_issues_log)


mortality_analytics$tables$deaths$weighted

mortality_analytics$tables$deaths


mortality_analytics$visualizations$roster$unweighted$age_months_distribution_unweighted
mortality_analytics$visualizations$roster$unweighted$age_months_cat_donut_unweighted

mortality_analytics$visualizations$deaths


mortality_analytics$analysis_plan_issue_log

mortality_analytics$tables$plausibility$penalty_summary
mortality_analytics$visualizations$deaths$weighted

mortality_analytics$tables$roster$mortality_analytics$visualizations$roster$age_pyramid_overall_unweighted
mortality_analytics$visualizations$roster$age_pyramid_overall_weighted
mortality_analytics$visualizations$roster$age_months_distribution_unweighted

mortality_analytics$tables$roster
mortality_analytics$tables$roster$basic_demo_table_weighted
mortality_analytics$tables$roster$basic_demo_table_strata_weighted


(a <- sum(mortality_analytics$data$linked_person_time))
(b <- sum(mortality_analytics$data$linked_person_time_female))
(c <- sum(mortality_analytics$data$linked_person_time_male))
(d <- sum(mortality_analytics$data$linked_person_time_under5))
