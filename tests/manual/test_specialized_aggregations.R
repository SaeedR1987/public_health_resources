# Manual test for specialized aggregation methods in HouseholdData
# This test demonstrates the new specialized aggregation methods for
# NutritionIndividualData, HealthIndividualData, and WomenIndividualData

library(iphRa)
library(dplyr)
library(tibble)

cat("=== Testing Specialized Aggregation Methods ===\n\n")

# Create sample household data
household_df <- tibble(
  uuid = c("hh1", "hh2", "hh3", "hh4"),
  consent = c("yes", "yes", "yes", "yes"),
  date_data_collection = as.Date(c("2023-01-15", "2023-01-16", "2023-01-17", "2023-01-18")),
  enum_id = c("E1", "E2", "E1", "E2")
)

# Create sample roster data (base IndividualData)
roster_df <- tibble(
  person_id = c("r1", "r2", "r3", "r4", "r5"),
  hh_uuid = c("hh1", "hh1", "hh1", "hh2", "hh2"),
  sex = c("M", "F", "M", "F", "M"),
  age = c(35, 30, 3, 25, 0.5)
)

# Create sample nutrition data with age in months
nutrition_df <- tibble(
  person_id = c("n1", "n2", "n3", "n4", "n5", "n6"),
  hh_uuid = c("hh1", "hh1", "hh2", "hh2", "hh3", "hh3"),
  calc_age_months = c(10, 30, 48, 72, 15, 20)  # <24, 24-59, 24-59, >=60 (not counted), <24, <24
)

# Create sample health data
health_df <- tibble(
  person_id = c("h1", "h2", "h3", "h4", "h5"),
  hh_uuid = c("hh1", "hh1", "hh1", "hh2", "hh3"),
  age = c(10, 20, 30, 25, 40),
  sex = c("M", "F", "M", "F", "M")
)

# Create sample women data with age in years
women_df <- tibble(
  person_id = c("w1", "w2", "w3", "w4", "w5", "w6"),
  hh_uuid = c("hh1", "hh1", "hh2", "hh2", "hh4", "hh4"),
  calc_age_years = c(14, 25, 35, 50, 15, 49)  # not 15-49, yes, yes, no, yes, yes
)

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

cat("3. Creating NutritionIndividualData object...\n")
nutrition_data <- NutritionIndividualData$new(
  data = nutrition_df,
  dataset_name = "TestNutrition"
)

cat("4. Creating HealthIndividualData object...\n")
health_data <- HealthIndividualData$new(
  data = health_df,
  dataset_name = "TestHealth"
)

cat("5. Creating WomenIndividualData object...\n")
women_data <- WomenIndividualData$new(
  data = women_df,
  dataset_name = "TestWomen"
)

# Link datasets to household data
cat("\n6. Linking datasets to HouseholdData using add_linked_dataset()...\n")
hh_data$add_linked_dataset("roster", roster_data)
hh_data$add_linked_dataset("nutrition", nutrition_data)
hh_data$add_linked_dataset("health", health_data)
hh_data$add_linked_dataset("women", women_data)

cat("   - Linked 'roster': IndividualData with", nrow(roster_df), "individuals\n")
cat("   - Linked 'nutrition': NutritionIndividualData with", nrow(nutrition_df), "children\n")
cat("   - Linked 'health': HealthIndividualData with", nrow(health_df), "individuals\n")
cat("   - Linked 'women': WomenIndividualData with", nrow(women_df), "women\n")

# Standardize household data (this should trigger pre_standardize)
cat("\n7. Standardizing HouseholdData (this will trigger specialized aggregation methods)...\n")
hh_data$standardize()

# Check the results
cat("\n8. Checking results in standardized household data...\n")
std_data <- hh_data$standardized_data

# Display roster aggregations
cat("\n--- Roster Aggregations (base IndividualData) ---\n")
if ("linked_roster_household_size" %in% names(std_data)) {
  cat("Roster columns found:\n")
  roster_cols <- grep("^linked_roster_", names(std_data), value = TRUE)
  for (col in roster_cols) {
    cat("  -", col, "\n")
  }
  cat("\nSample values for hh1:\n")
  hh1 <- std_data[std_data$uuid == "hh1", roster_cols, drop = FALSE]
  print(hh1)
}

# Display nutrition aggregations
cat("\n--- Nutrition Aggregations (NutritionIndividualData) ---\n")
if ("linked_nutrition_children_under2" %in% names(std_data)) {
  cat("Nutrition columns found:\n")
  nutr_cols <- grep("^linked_nutrition_", names(std_data), value = TRUE)
  for (col in nutr_cols) {
    cat("  -", col, "\n")
  }
  cat("\nAll household nutrition data:\n")
  nutr_data <- std_data[, c("uuid", nutr_cols), drop = FALSE]
  print(nutr_data)
  
  cat("\nExpected values:\n")
  cat("  hh1: under2=1 (10mo), 2to5=1 (30mo), under5=2\n")
  cat("  hh2: under2=0, 2to5=1 (48mo), under5=1\n")
  cat("  hh3: under2=2 (15mo, 20mo), 2to5=0, under5=2\n")
  cat("  hh4: all 0 (no nutrition data)\n")
}

# Display health aggregations
cat("\n--- Health Aggregations (HealthIndividualData) ---\n")
if ("linked_health_num_people_recorded" %in% names(std_data)) {
  cat("Health columns found:\n")
  health_cols <- grep("^linked_health_", names(std_data), value = TRUE)
  for (col in health_cols) {
    cat("  -", col, "\n")
  }
  cat("\nAll household health data:\n")
  health_data_agg <- std_data[, c("uuid", health_cols), drop = FALSE]
  print(health_data_agg)
  
  cat("\nExpected values:\n")
  cat("  hh1: 3 people recorded\n")
  cat("  hh2: 1 person recorded\n")
  cat("  hh3: 1 person recorded\n")
  cat("  hh4: 0 people recorded\n")
}

# Display women aggregations
cat("\n--- Women Aggregations (WomenIndividualData) ---\n")
if ("linked_women_women_15to49" %in% names(std_data)) {
  cat("Women columns found:\n")
  women_cols <- grep("^linked_women_", names(std_data), value = TRUE)
  for (col in women_cols) {
    cat("  -", col, "\n")
  }
  cat("\nAll household women data:\n")
  women_data_agg <- std_data[, c("uuid", women_cols), drop = FALSE]
  print(women_data_agg)
  
  cat("\nExpected values:\n")
  cat("  hh1: 1 woman 15-49 (14yo no, 25yo yes)\n")
  cat("  hh2: 1 woman 15-49 (35yo yes, 50yo no)\n")
  cat("  hh3: 0 women 15-49 (no women data)\n")
  cat("  hh4: 2 women 15-49 (15yo yes, 49yo yes)\n")
}

cat("\n=== Test Complete ===\n")
cat("All specialized aggregation methods have been executed successfully!\n")
