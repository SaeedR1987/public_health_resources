# Test helpers for the consolidated private ..schema_to_table() method.
#
# The exported utilities `data_schema_to_table()`, `indicator_schema_to_table()`
# and `dependency_schema_to_table()` were removed from the public API and their
# logic was inlined into `Data$..schema_to_table(schema_type, schema_list)`.
# These helpers provide backward-compatible entry points that construct a
# minimal Data object and dispatch through the private method so the existing
# behaviour-focused tests keep exercising the same logic.

.schema_to_table_test_data <- function() {
  Data$new(
    data         = data.frame(id = character(0), stringsAsFactors = FALSE),
    uuid         = "id",
    dataset_name = "schema_to_table_test"
  )
}

data_schema_to_table <- function(schema_list) {
  d <- .schema_to_table_test_data()
  d$call(
    field       = "..schema_to_table",
    schema_type = "variable",
    schema_list = schema_list
  )
}

indicator_schema_to_table <- function(indicator_schema_list) {
  d <- .schema_to_table_test_data()
  d$call(
    field       = "..schema_to_table",
    schema_type = "indicator",
    schema_list = indicator_schema_list
  )
}

dependency_schema_to_table <- function(dependency_schema_list) {
  d <- .schema_to_table_test_data()
  d$call(
    field       = "..schema_to_table",
    schema_type = "dependency",
    schema_list = dependency_schema_list
  )
}
