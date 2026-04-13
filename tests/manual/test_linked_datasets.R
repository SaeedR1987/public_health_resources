# Manual test for linked datasets functionality in HouseholdData
# This test demonstrates the linked_objects feature (inherited from Data class)

library(phr)
library(dplyr)
library(tibble)

# Create sample household data
household_df <- tibble(
  uuid = c("hh1", "hh2", "hh3"),
  consent = c("yes", "yes", "yes"),
  date_data_collection = as.Date(c("2023-01-15", "2023-01-16", "2023-01-17")),
  enum_id = c("E1", "E2", "E1")
)

# Create sample roster/individual data
roster_df <- tibble(
  uuid = c("ind1", "ind2", "ind3", "ind4", "ind5", "ind6"),
  hh_uuid = c("hh1", "hh1", "hh1", "hh2", "hh2", "hh3"),
  sex = c("M", "F", "M", "F", "M", "F"),
  age = c(35, 30, 3, 25, 0.5, 40),
  person_time = c(365, 365, 365, 365, 180, 365),
  person_time_under5 = c(0, 0, 365, 0, 180, 0),
  person_time_male = c(365, 0, 365, 0, 180, 0),
  person_time_female = c(0, 365, 0, 365, 0, 365)
)

# Create sample deaths data
deaths_df <- tibble(
  uuid = c("death1", "death2"),
  hh_uuid = c("hh1", "hh2"),
  sex = c("M", "F"),
  age = c(2, 45),
  death = c(1, 1),
  death_under5 = c(1, 0),
  death_male = c(1, 0),
  death_female = c(0, 1),
  death_non_trauma = c(1, 0),
  death_trauma = c(0, 1),
  death_other = c(0, 0),
  death_current_location = c(1, 0),
  death_migration = c(0, 1),
  death_last_location = c(0, 0),
  person_time = c(60, 300),
  person_time_under5 = c(60, 0),
  person_time_male = c(60, 0),
  person_time_female = c(0, 300)
)

# Create sample water container data
water_df <- tibble(
  uuid = c("cont1", "cont2", "cont3", "cont4"),
  hh_uuid = c("hh1", "hh1", "hh2", "hh3"),
  wash_container_total_liters = c(20, 15, 25, 30)
)

cat("=== Testing Linked Datasets Functionality ===\n\n")

# Initialize the Data objects
cat("1. Creating HouseholdData object...\n")
hh_data <- HouseholdData$new(
  data = household_df,
  dataset_name = "TestHousehold"
)

cat("2. Creating IndividualData (roster) object...\n")
roster_data <- IndividualData$new(
  data = roster_df,
  dataset_name = "TestRoster"
)

cat("3. Creating DeathIndividualData object...\n")
deaths_data <- DeathIndividualData$new(
  data = deaths_df,
  dataset_name = "TestDeaths",
  recall_date = as.Date("2022-01-01")
)

cat("4. Creating WaterContainerData object...\n")
water_data <- WaterContainerData$new(
  data = water_df,
  dataset_name = "TestWater"
)

# Link datasets to household data
cat("\n5. Linking datasets to HouseholdData using add_linked_dataset()...\n")
hh_data$add_linked_dataset("roster", roster_data)
hh_data$add_linked_dataset("deaths", deaths_data)
hh_data$add_linked_dataset("water", water_data)

cat("   - Linked 'roster': IndividualData with", nrow(roster_df), "individuals\n")
cat("   - Linked 'deaths': DeathIndividualData with", nrow(deaths_df), "deaths\n")
cat("   - Linked 'water': WaterContainerData with", nrow(water_df), "containers\n")

# Standardize household data (this should trigger pre_standardize)
cat("\n6. Standardizing HouseholdData (this will trigger pre_standardize hook)...\n")
hh_data$standardize()

# Check the results
cat("\n7. Checking results in standardized household data...\n")
std_data <- hh_data$standardized_data

# Display column names
cat("\nColumn names in standardized household data:\n")
print(names(std_data))

# Check for linked columns
linked_cols <- grep("^linked_", names(std_data), value = TRUE)
cat("\n\nLinked columns added:\n")
if (length(linked_cols) > 0) {
  print(linked_cols)
  
  # Show data for first household
  cat("\n\nData for household 'hh1':\n")
  hh1_data <- std_data %>% filter(uuid == "hh1") %>% select(uuid, starts_with("linked_"))
  print(hh1_data)
  
  # Show summary statistics
  cat("\n\nSummary of linked columns:\n")
  print(summary(std_data %>% select(starts_with("linked_"))))
} else {
  cat("WARNING: No linked columns found!\n")
}

cat("\n\n=== Test Complete ===\n")
cat("Expected linked columns:\n")
cat("- linked_roster_household_size\n")
cat("- linked_roster_children_under2\n")
cat("- linked_roster_children_under5\n")
cat("- linked_roster_num_male\n")
cat("- linked_roster_num_female\n")
cat("- linked_roster_women_15to49\n")
cat("- linked_roster_person_time*\n")
cat("- linked_deaths_death*\n")
cat("- linked_deaths_person_time*\n")
cat("- linked_water_wash_container_total_liters\n")
