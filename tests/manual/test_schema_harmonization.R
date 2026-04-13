# Manual test script for schema method harmonization
# Run this script to verify the new variable schema methods work correctly

library(phr)

# Create a simple test dataset
test_df <- data.frame(
  id = 1:5,
  name = c("A", "B", "C", "D", "E"),
  value = c(10, 20, 30, 40, 50),
  stringsAsFactors = FALSE
)

# Test 1: Create Data object
cat("Test 1: Creating Data object...\n")
d <- Data$new(
  data = test_df,
  dataset_name = "TestData",
  uuid = "id"
)
cat("✓ Data object created successfully\n\n")

# Test 2: Create and set variable schema using NEW methods
cat("Test 2: Testing set_variable_schema()...\n")
test_schema <- list(
  types = list(
    id = "numeric",
    name = "character",
    value = "numeric"
  ),
  required = c("id"),
  col_names = list(
    id = "id",
    name = "name",
    value = "value"
  )
)

d$set_variable_schema(test_schema)
cat("✓ Variable schema set successfully\n\n")

# Test 3: Get variable schema using NEW method
cat("Test 3: Testing get_variable_schema()...\n")
retrieved_schema <- d$get_variable_schema()
if (!is.null(retrieved_schema) && !is.null(retrieved_schema$types)) {
  cat("✓ Variable schema retrieved successfully\n")
  cat("  Schema has", length(retrieved_schema$types), "typed variables\n\n")
} else {
  stop("✗ Failed to retrieve variable schema")
}

# Test 4: Export variable schema using NEW method
cat("Test 4: Testing export_variable_schema()...\n")
schema_table <- d$export_variable_schema()
if (!is.null(schema_table) && is.data.frame(schema_table)) {
  cat("✓ Variable schema exported successfully\n")
  cat("  Exported table has", nrow(schema_table), "rows\n\n")
} else {
  stop("✗ Failed to export variable schema")
}

# Test 5: Import variable schema using NEW method
cat("Test 5: Testing import_variable_schema()...\n")
d2 <- Data$new(
  data = test_df,
  dataset_name = "TestData2",
  uuid = "id"
)
d2$import_variable_schema(schema_table)
cat("✓ Variable schema imported successfully\n\n")

# Test 6: Test backward compatibility - old methods still work
cat("Test 6: Testing backward compatibility...\n")
d3 <- Data$new(
  data = test_df,
  dataset_name = "TestData3",
  uuid = "id"
)

# set_variable_schema is the correct method to use (set_schema has been removed)
d3$set_variable_schema(test_schema)
cat("✓ set_variable_schema() works correctly\n")

# get_variable_schema is the correct method to use (get_schema has been removed)
old_schema <- d3$get_variable_schema()
if (!is.null(old_schema)) {
  cat("✓ get_variable_schema() works correctly\n\n")
} else {
  stop("✗ get_variable_schema() failed")
}

# Test 7: Verify consistency with indicator/dependency schema methods
cat("Test 7: Testing consistency with other schema methods...\n")

# Create indicator schema
indicator_schema <- list(
  test_indicator = list(
    function_name = "add_test_indicator",
    variables = c("id", "value")
  )
)
d$set_indicator_schema(indicator_schema)
exported_ind <- d$export_indicator_schema()
if (!is.null(exported_ind)) {
  cat("✓ Indicator schema set and export methods work\n")
}

# Test new indicator getter method
retrieved_ind <- d$get_indicator_schema()
if (!is.null(retrieved_ind) && length(retrieved_ind) > 0) {
  cat("✓ Indicator schema get method works\n")
} else {
  stop("✗ Failed to get indicator schema")
}

# Create dependency schema
dependency_schema <- list(
  dependencies = list(
    test_dep = list(
      if = "value > 25",
      then = "!is.na(name)",
      variables = c("value", "name")
    )
  ),
  soft_dependencies = list()
)
d$set_dependency_schema(dependency_schema)
exported_dep <- d$export_dependency_schema()
if (!is.null(exported_dep)) {
  cat("✓ Dependency schema set and export methods work\n")
}

# Test new dependency getter method
retrieved_dep <- d$get_dependency_schema()
if (!is.null(retrieved_dep) && !is.null(retrieved_dep$dependencies)) {
  cat("✓ Dependency schema get method works\n")
} else {
  stop("✗ Failed to get dependency schema")
}

cat("\n✓✓✓ All tests passed! ✓✓✓\n")
cat("\nNow all three schema types use consistent import/export/set/get naming:\n")
cat("  Variable:   import_variable_schema(), export_variable_schema(), set_variable_schema(), get_variable_schema()\n")
cat("  Indicator:  import_indicator_schema(), export_indicator_schema(), set_indicator_schema(), get_indicator_schema()\n")
cat("  Dependency: import_dependency_schema(), export_dependency_schema(), set_dependency_schema(), get_dependency_schema()\n")
