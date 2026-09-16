
rm(list = ls())

devtools::load_all()

#Install and setup

df_hh <- readxl::read_xlsx("REACH_DRC2603_Dataset_MSNA_2026.xlsx", sheet = "hh clean data")

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

roster <- phr::IndividualData$new(
  data = df_roster,
  dataset_name = "DRC MSNA 2026 Roster",
  variable_map = list(
    sex = "ind_gender",
    age_years = "ind_age",
    know_dob = "ind_under5_date_know",
    dob_exact = "ind_under5_date",
    dob_approx = "ind_under5_event",
    dob_final = "ind_dob_final"
  ))

roster$import_variable_schema(df = read.csv(file = "roster_variable_schema.csv"))

View(roster$data_diagnose(stage = "raw"))

roster$standardize()

View(roster$data_diagnose(stage = "standardized"))


roster$export

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

# write.csv(x = drc_mort$export_variable_schema(), file = "hh_variable_schema.csv")
#
# write.csv(x = drc_mort$linked_objects$roster$object$export_variable_schema(), file = "roster_variable_schema.csv")
#
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



# Produce Data Analytics




