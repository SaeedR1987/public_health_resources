# Test to verify add_linked_dataset validation
# This test verifies that link names are validated and class types are checked

library(phr)
library(tibble)

cat("=== Testing add_linked_dataset Validation ===\n\n")

# Create sample household data
household_df <- tibble(
  uuid = c("hh1", "hh2"),
  consent = c("yes", "yes"),
  date_data_collection = as.Date(c("2023-01-15", "2023-01-16")),
  enum_id = c("E1", "E2")
)

# Create sample roster data
roster_df <- tibble(
  uuid = c("ind1", "ind2", "ind3"),
  hh_uuid = c("hh1", "hh1", "hh2"),
  sex = c("M", "F", "M"),
  age = c(35, 30, 25)
)

# Create sample deaths data
deaths_df <- tibble(
  uuid = c("death1"),
  hh_uuid = c("hh1"),
  sex = c("M"),
  age = c(2)
)

# Create HouseholdData object
hh_data <- HouseholdData$new(
  data = household_df,
  dataset_name = "TestHousehold"
)

# Create IndividualData object
roster_data <- IndividualData$new(
  data = roster_df,
  dataset_name = "TestRoster"
)

# Create DeathIndividualData object
deaths_data <- DeathIndividualData$new(
  data = deaths_df,
  dataset_name = "TestDeaths",
  recall_date = as.Date("2022-01-01")
)

cat("Test 1: Valid link names with correct classes\n")
cat("----------------------------------------------\n")

# Test valid link: roster with IndividualData
cat("1a. Adding 'roster' with IndividualData: ")
result <- tryCatch({
  hh_data$add_linked_dataset("roster", roster_data)
  cat("✅ SUCCESS\n")
  TRUE
}, error = function(e) {
  cat("❌ FAILED:", e$message, "\n")
  FALSE
})

# Test valid link: deaths with DeathIndividualData
cat("1b. Adding 'deaths' with DeathIndividualData: ")
result <- tryCatch({
  hh_data$add_linked_dataset("deaths", deaths_data)
  cat("✅ SUCCESS\n")
  TRUE
}, error = function(e) {
  cat("❌ FAILED:", e$message, "\n")
  FALSE
})

cat("\nTest 2: Invalid link names\n")
cat("---------------------------\n")

# Test invalid link name
cat("2a. Adding 'invalid_name' with IndividualData: ")
result <- tryCatch({
  hh_data2 <- HouseholdData$new(data = household_df, dataset_name = "TestHH2")
  hh_data2$add_linked_dataset("invalid_name", roster_data)
  cat("❌ FAILED: Should have thrown error\n")
  FALSE
}, error = function(e) {
  if (grepl("not allowed", e$message, ignore.case = TRUE)) {
    cat("✅ SUCCESS: Correctly rejected invalid name\n")
    TRUE
  } else {
    cat("❌ FAILED: Wrong error message:", e$message, "\n")
    FALSE
  }
})

cat("\nTest 3: Wrong class for link name\n")
cat("----------------------------------\n")

# Test wrong class: deaths name with IndividualData object
cat("3a. Adding 'deaths' with IndividualData (should be DeathIndividualData): ")
result <- tryCatch({
  hh_data3 <- HouseholdData$new(data = household_df, dataset_name = "TestHH3")
  hh_data3$add_linked_dataset("deaths", roster_data)
  cat("❌ FAILED: Should have thrown error\n")
  FALSE
}, error = function(e) {
  if (grepl("expects a DeathIndividualData", e$message)) {
    cat("✅ SUCCESS: Correctly rejected wrong class\n")
    TRUE
  } else {
    cat("❌ FAILED: Wrong error message:", e$message, "\n")
    FALSE
  }
})

# Test wrong class: roster name with DeathIndividualData object
cat("3b. Adding 'roster' with DeathIndividualData (should be IndividualData): ")
result <- tryCatch({
  hh_data4 <- HouseholdData$new(data = household_df, dataset_name = "TestHH4")
  hh_data4$add_linked_dataset("roster", deaths_data)
  cat("❌ FAILED: Should have thrown error\n")
  FALSE
}, error = function(e) {
  if (grepl("expects a IndividualData", e$message)) {
    cat("✅ SUCCESS: Correctly rejected wrong class\n")
    TRUE
  } else {
    cat("❌ FAILED: Wrong error message:", e$message, "\n")
    FALSE
  }
})

cat("\nTest 4: Allowed link names list\n")
cat("--------------------------------\n")
allowed_names <- c("roster", "deaths", "water_containers", "women", "nutrition", "health")
cat("Allowed link names:", paste(allowed_names, collapse=", "), "\n")
cat("Expected classes:\n")
cat("  - roster: IndividualData\n")
cat("  - deaths: DeathIndividualData\n")
cat("  - water_containers: WaterContainerData\n")
cat("  - women: IndividualData\n")
cat("  - nutrition: NutritionIndividualData\n")
cat("  - health: HealthIndividualData\n")

cat("\n=== Test Complete ===\n")
