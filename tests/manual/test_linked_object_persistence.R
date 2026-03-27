# Test to verify that linked object standardization persists
# This test verifies issue (2) from the comment

library(iphRa)
library(tibble)

cat("=== Testing Linked Object Standardization Persistence ===\n\n")

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

# Create HouseholdData and IndividualData objects
hh_data <- HouseholdData$new(
  data = household_df,
  dataset_name = "TestHousehold"
)

roster_data <- IndividualData$new(
  data = roster_df,
  dataset_name = "TestRoster"
)

# Check initial state
cat("1. Initial state of roster_data:\n")
cat("   - Standardized:", roster_data$standardized, "\n")
cat("   - Has standardized_data:", !is.null(roster_data$standardized_data), "\n\n")

# Link roster to household
cat("2. Linking roster to household data...\n")
hh_data$add_linked_dataset("roster", roster_data)

# Check state before household standardization
cat("3. State before household standardization:\n")
cat("   - Roster standardized:", roster_data$standardized, "\n")
cat("   - Has standardized_data:", !is.null(roster_data$standardized_data), "\n\n")

# Standardize household data (should trigger roster standardization)
cat("4. Standardizing household data...\n")
hh_data$standardize()

# Check state after household standardization
cat("\n5. State after household standardization:\n")
cat("   - Roster standardized:", roster_data$standardized, "\n")
cat("   - Has standardized_data:", !is.null(roster_data$standardized_data), "\n")

if (roster_data$standardized && !is.null(roster_data$standardized_data)) {
  cat("   - Standardized data rows:", nrow(roster_data$standardized_data), "\n")
  cat("\n✅ SUCCESS: Linked object standardization persists!\n")
} else {
  cat("\n❌ FAILURE: Linked object standardization did not persist!\n")
}

# Also verify that we can access the standardized data directly from the linked object
cat("\n6. Verifying direct access to linked object:\n")
if (identical(hh_data$linked_objects$roster$object$standardized, TRUE)) {
  cat("   ✅ Can access standardized status from linked_objects\n")
} else {
  cat("   ❌ Cannot access standardized status from linked_objects\n")
}

if (!is.null(hh_data$linked_objects$roster$object$standardized_data)) {
  cat("   ✅ Can access standardized_data from linked_objects\n")
  cat("   - Rows:", nrow(hh_data$linked_objects$roster$object$standardized_data), "\n")
} else {
  cat("   ❌ Cannot access standardized_data from linked_objects\n")
}

cat("\n=== Test Complete ===\n")
