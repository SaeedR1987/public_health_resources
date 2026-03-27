
library(testthat)
library(tibble)

# ============================================================================
# DataQuality Base Class Tests
# ============================================================================

test_that("DataQuality initializes with valid data", {
  df <- tibble::tibble(id = 1:10, x = rnorm(10))

  dq <- DataQuality$new(data = df, dataset_name = "TestDQ")

  expect_s3_class(dq, "DataQuality")
  expect_equal(dq$dataset_name, "TestDQ")
  expect_equal(nrow(dq$data), 10)
  expect_null(dq$parent_data_object)
  expect_true(is.list(dq$quality_schema))
  expect_true(is.list(dq$results))
})

test_that("DataQuality errors when data is NULL", {
  expect_error(
    DataQuality$new(data = NULL, dataset_name = "NullData"),
    regexp = "No data provided"
  )
})

test_that("DataQuality errors when data is not a data frame", {
  expect_error(
    DataQuality$new(data = c(1, 2, 3), dataset_name = "NotDF"),
    regexp = "Expected a data frame"
  )
})

test_that("DataQuality stores parent_data_object reference", {
  df <- tibble::tibble(id = 1:5)
  parent <- Data$new(data = df, uuid = "id", dataset_name = "ParentData")

  dq <- DataQuality$new(
    data = df,
    parent_data_object = parent,
    dataset_name = "ChildDQ"
  )

  expect_equal(dq$parent_data_object, parent)
  expect_equal(dq$metadata$parent_name, "ParentData")
})

test_that("DataQuality default schema is empty but valid", {
  df <- tibble::tibble(id = 1:5)
  dq <- DataQuality$new(data = df)

  schema <- dq$get_quality_schema()

  expect_true(is.list(schema))
  # Schema is now the checks list itself (no wrapper with $checks)
  expect_equal(length(schema), 0)  # Empty schema
})

test_that("DataQuality can set custom quality schema", {
  df <- tibble::tibble(id = 1:5, val = 1:5)
  dq <- DataQuality$new(data = df)

  # Schema is now just the checks list (no metadata wrapper)
  custom_schema <- list(
    test_check = list(
      check_name = "test_check",
      check_label = "Test Check",
      statistical_test = "range_violation",
      variables = c("val"),
      thresholds = list(
        list(expression = "test_statistic <= 5", penalty = 0),
        list(expression = "test_statistic > 5", penalty = 5)
      ),
      test_params = list(min_value = 0, max_value = 10)
    )
  )

  dq$set_quality_schema(custom_schema)

  expect_equal(dq$quality_schema$test_check$check_name, "test_check")
  expect_equal(length(dq$quality_schema$test_check$thresholds), 2)
  expect_equal(dq$quality_schema$test_check$thresholds[[1]]$expression, "test_statistic <= 5")
  expect_equal(dq$quality_schema$test_check$thresholds[[1]]$penalty, 0)
})

test_that("DataQuality run_quality_checks returns empty list when no checks defined", {
  df <- tibble::tibble(id = 1:5)
  dq <- DataQuality$new(data = df)

  expect_warning(
    results <- dq$run_quality_checks(),
    regexp = "No quality checks defined"
  )

  expect_true(is.list(results))
  expect_length(results, 0)
})

test_that("DataQuality compute_summary_stats works", {
  df <- tibble::tibble(
    id = 1:10,
    val = c(1, 2, NA, 4, 5, NA, 7, 8, 9, 10)
  )
  dq <- DataQuality$new(data = df)

  stats <- dq$compute_summary_stats()

  expect_equal(stats$n_records, 10)
  expect_equal(stats$n_columns, 2)
  expect_equal(stats$missing_by_column[["val"]], 2)
  expect_equal(stats$missing_pct_by_column[["val"]], 20)
})

test_that("DataQuality summary returns expected structure", {
  df <- tibble::tibble(id = 1:5)
  dq <- DataQuality$new(data = df, dataset_name = "SummaryTest")

  s <- dq$summary()

  expect_true(is.list(s))
  expect_equal(s$dataset_name, "SummaryTest")
  expect_equal(s$n_records, 5)
  expect_equal(s$n_columns, 1)
})

test_that("DataQuality results_to_table returns tibble", {
  df <- tibble::tibble(id = 1:5)
  dq <- DataQuality$new(data = df)

  tbl <- dq$results_to_table()

  expect_s3_class(tbl, "tbl_df")
  expect_true("check_name" %in% names(tbl))
  expect_equal(nrow(tbl), 0)  # No results yet
})

test_that("DataQuality generate_report includes all sections", {
  df <- tibble::tibble(id = 1:10, x = rnorm(10))
  dq <- DataQuality$new(data = df)

  report <- dq$generate_report()

  expect_true(is.list(report))
  expect_true("summary" %in% names(report))
  expect_true("summary_stats" %in% names(report))
  expect_true("results" %in% names(report))
  expect_true("overall_score" %in% names(report))
  expect_true("generated_at" %in% names(report))
})

# ============================================================================
# Variable Mapping Tests
# ============================================================================

test_that("DataQuality translates canonical variable names using variable_map", {
  # Create data with actual column names
  df <- tibble::tibble(
    hh_id = 1:10,
    q1_household_size = sample(1:10, 10, replace = TRUE),
    q2_monthly_income = rnorm(10, mean = 1000, sd = 200)
  )
  
  # Create variable_map: canonical -> actual
  var_map <- list(
    household_size = "q1_household_size",
    monthly_income = "q2_monthly_income"
  )
  
  dq <- DataQuality$new(
    data = df,
    variable_map = var_map,
    dataset_name = "TestDQ"
  )
  
  # Create schema using canonical names
  custom_schema <- list(
    check_household_size = list(
      check_name = "check_household_size",
      check_label = "Check Household Size Range",
      statistical_test = "range_violation",
      variables = c("household_size"),  # Canonical name
      thresholds = list(
        list(expression = "test_statistic <= 10", penalty = 0),
        list(expression = "test_statistic > 10", penalty = 5)
      ),
      test_params = list(min_value = 1, max_value = 15)
    )
  )
  
  dq$set_quality_schema(custom_schema)
  
  # Run checks - should use variable_map to translate canonical names
  results <- dq$run_quality_checks()
  
  expect_true(is.list(results))
  expect_equal(length(results), 1)
  expect_true("check_household_size" %in% names(results))
  
  # Check should execute successfully (not return "variables not found")
  expect_false(grepl("not found", results$check_household_size$message, ignore.case = TRUE))
  expect_true(!is.na(results$check_household_size$test_statistic))
})

test_that("DataQuality handles missing variable_map gracefully", {
  # Create data with canonical names
  df <- tibble::tibble(
    id = 1:10,
    household_size = sample(1:10, 10, replace = TRUE)
  )
  
  # No variable_map provided
  dq <- DataQuality$new(data = df, dataset_name = "TestDQ")
  
  # Create schema using exact column names
  custom_schema <- list(
    check_size = list(
      check_name = "check_size",
      check_label = "Check Size",
      statistical_test = "range_violation",
      variables = c("household_size"),  # Exact column name
      thresholds = list(
        list(expression = "test_statistic <= 10", penalty = 0)
      ),
      test_params = list(min_value = 1, max_value = 15)
    )
  )
  
  dq$set_quality_schema(custom_schema)
  results <- dq$run_quality_checks()
  
  # Should work with exact column names when no mapping exists
  expect_true(!is.na(results$check_size$test_statistic))
})

test_that("DataQuality handles unmapped variables in schema", {
  # Create data
  df <- tibble::tibble(
    id = 1:10,
    q1_size = sample(1:10, 10, replace = TRUE)
  )
  
  # Partial variable_map
  var_map <- list(
    household_size = "q1_size"
  )
  
  dq <- DataQuality$new(
    data = df,
    variable_map = var_map,
    dataset_name = "TestDQ"
  )
  
  # Schema references a variable not in variable_map and not in data
  custom_schema <- list(
    check_missing = list(
      check_name = "check_missing",
      check_label = "Check Missing Variable",
      statistical_test = "range_violation",
      variables = c("nonexistent_var"),  # Not in variable_map or data
      thresholds = list(
        list(expression = "test_statistic <= 10", penalty = 0)
      ),
      test_params = list(min_value = 1, max_value = 15)
    )
  )
  
  dq$set_quality_schema(custom_schema)
  results <- dq$run_quality_checks()
  
  # Should report variables not found
  expect_true(grepl("not found", results$check_missing$message, ignore.case = TRUE))
  expect_true(is.na(results$check_missing$test_statistic))
})

test_that("DataQuality requires ALL variables to be present, not just some", {
  # Create data with only one of two required variables
  df <- tibble::tibble(
    id = 1:10,
    q1_var1 = rnorm(10)
    # q2_var2 is missing
  )
  
  # Variable map
  var_map <- list(
    var1 = "q1_var1",
    var2 = "q2_var2"
  )
  
  dq <- DataQuality$new(
    data = df,
    variable_map = var_map,
    dataset_name = "TestDQ"
  )
  
  # Schema requires both variables
  custom_schema <- list(
    check_correlation = list(
      check_name = "check_correlation",
      check_label = "Check Correlation",
      statistical_test = "correlation",
      variables = c("var1", "var2"),  # Both required
      thresholds = list(
        list(expression = "abs(test_statistic) < 0.5", penalty = 0)
      ),
      test_params = list(method = "pearson")
    )
  )
  
  dq$set_quality_schema(custom_schema)
  results <- dq$run_quality_checks()
  
  # Should fail because var2 (q2_var2) is missing
  expect_true(grepl("not found", results$check_correlation$message, ignore.case = TRUE))
  expect_true(grepl("q2_var2", results$check_correlation$message))
  expect_true(is.na(results$check_correlation$test_statistic))
})

test_that("DataQuality translates multiple variables using variable_map", {
  # Create data
  df <- tibble::tibble(
    id = 1:10,
    q1_var1 = rnorm(10),
    q2_var2 = rnorm(10)
  )
  
  # Variable map
  var_map <- list(
    var1 = "q1_var1",
    var2 = "q2_var2"
  )
  
  dq <- DataQuality$new(
    data = df,
    variable_map = var_map,
    dataset_name = "TestDQ"
  )
  
  # Schema with multiple canonical variables
  custom_schema <- list(
    check_correlation = list(
      check_name = "check_correlation",
      check_label = "Check Correlation",
      statistical_test = "correlation",
      variables = c("var1", "var2"),  # Canonical names
      thresholds = list(
        list(expression = "abs(test_statistic) < 0.5", penalty = 0),
        list(expression = "abs(test_statistic) >= 0.5", penalty = 3)
      ),
      test_params = list(method = "pearson")
    )
  )
  
  dq$set_quality_schema(custom_schema)
  results <- dq$run_quality_checks()
  
  # Check should execute successfully
  expect_false(grepl("not found", results$check_correlation$message, ignore.case = TRUE))
  expect_true(!is.na(results$check_correlation$test_statistic))
})

test_that("get_variable method works consistently with Data class", {
  df <- tibble::tibble(
    id = 1:5,
    actual_col1 = 1:5,
    actual_col2 = 6:10
  )
  
  var_map <- list(
    canonical1 = "actual_col1",
    canonical2 = "actual_col2"
  )
  
  dq <- DataQuality$new(data = df, variable_map = var_map)
  
  # Test get_variable for mapped variables
  expect_equal(dq$get_variable("canonical1"), "actual_col1")
  expect_equal(dq$get_variable("canonical2"), "actual_col2")
  
  # Test get_variable for unmapped variable (should return NULL)
  expect_null(dq$get_variable("unmapped"))
  
  # Test with no variable_map
  dq_no_map <- DataQuality$new(data = df)
  expect_null(dq_no_map$get_variable("canonical1"))
  
  # Test with NULL or empty input
  expect_null(dq$get_variable(NULL))
  expect_null(dq$get_variable(character(0)))
})

test_that(".translate_canonical_to_actual_vars helper method works correctly", {
  df <- tibble::tibble(
    id = 1:5,
    actual_col1 = 1:5,
    actual_col2 = 6:10
  )
  
  var_map <- list(
    canonical1 = "actual_col1",
    canonical2 = "actual_col2"
  )
  
  dq <- DataQuality$new(data = df, variable_map = var_map)
  
  # Test translation
  result <- dq$.translate_canonical_to_actual_vars(c("canonical1", "canonical2"))
  expect_equal(result, c("actual_col1", "actual_col2"))
  
  # Test with unmapped variable (should return as-is)
  result2 <- dq$.translate_canonical_to_actual_vars(c("canonical1", "unmapped"))
  expect_equal(result2, c("actual_col1", "unmapped"))
  
  # Test with no variable_map
  dq_no_map <- DataQuality$new(data = df)
  result3 <- dq_no_map$.translate_canonical_to_actual_vars(c("canonical1", "canonical2"))
  expect_equal(result3, c("canonical1", "canonical2"))
})

# ============================================================================
# Quality Test Function Discovery Tests
# ============================================================================

test_that("All quality_test functions can be discovered by execute_check", {
  # List of all quality test functions that should be available
  test_functions <- c(
    "quality_test_correlation",
    "quality_test_ttest",
    "quality_test_chisq",
    "quality_test_flag_percentage",
    "quality_test_missing_percentage",
    "quality_test_outlier_percentage",
    "quality_test_coefficient_variation",
    "quality_test_range_violation",
    "quality_test_sd",
    "quality_test_sd_across_percentage",
    "quality_test_any_flag_percentage"
  )
  
  # Check that all functions are exported and available
  for (func_name in test_functions) {
    # Try to get the function using the same logic as execute_check
    test_function <- NULL
    
    # Try package namespace first
    if (requireNamespace("iphRa", quietly = TRUE)) {
      if (exists(func_name, where = asNamespace("iphRa"), mode = "function")) {
        test_function <- get(func_name, envir = asNamespace("iphRa"), mode = "function")
      }
    }
    
    # If not found in package namespace, try parent environment
    if (is.null(test_function) && exists(func_name, mode = "function", inherits = TRUE)) {
      test_function <- get(func_name, mode = "function")
    }
    
    # Function should be found
    expect_false(is.null(test_function), 
                 info = paste("Function", func_name, "should be discoverable"))
    expect_true(is.function(test_function),
                info = paste(func_name, "should be a function"))
  }
})

test_that("execute_check can find and execute quality_test_sd", {
  # Create test data with reproducible seed
  set.seed(12345)
  df <- tibble::tibble(
    id = 1:20,
    values = rnorm(20, mean = 100, sd = 15)
  )
  
  dq <- DataQuality$new(data = df, dataset_name = "TestDQ")
  
  # Create schema with sd test
  custom_schema <- list(
    check_sd = list(
      check_name = "check_sd",
      check_label = "Check Standard Deviation",
      statistical_test = "sd",
      variables = c("values"),
      thresholds = list(
        list(expression = "test_statistic < 10", penalty = 5),
        list(expression = "test_statistic >= 10", penalty = 0)
      )
    )
  )
  
  dq$set_quality_schema(custom_schema)
  results <- dq$run_quality_checks()
  
  # Check should execute successfully
  expect_false(is.na(results$check_sd$test_statistic))
  expect_false(grepl("not found", results$check_sd$message, ignore.case = TRUE))
  expect_false(grepl("Unknown test", results$check_sd$message, ignore.case = TRUE))
})

test_that("execute_check can find and execute quality_test_sd_across_percentage", {
  # Create test data with multiple columns with reproducible seed
  set.seed(12346)
  df <- tibble::tibble(
    id = 1:20,
    col1 = rnorm(20, mean = 5, sd = 0.5),
    col2 = rnorm(20, mean = 5, sd = 0.5),
    col3 = rnorm(20, mean = 5, sd = 0.5)
  )
  
  dq <- DataQuality$new(data = df, dataset_name = "TestDQ")
  
  # Create schema with sd_across_percentage test
  custom_schema <- list(
    check_sd_across = list(
      check_name = "check_sd_across",
      check_label = "Check SD Across Columns",
      statistical_test = "sd_across_percentage",
      variables = c("col1", "col2", "col3"),
      thresholds = list(
        list(expression = "test_statistic > 50", penalty = 5),
        list(expression = "test_statistic <= 50", penalty = 0)
      ),
      test_params = list(threshold = 0.8)
    )
  )
  
  dq$set_quality_schema(custom_schema)
  results <- dq$run_quality_checks()
  
  # Check should execute successfully
  expect_false(is.na(results$check_sd_across$test_statistic))
  expect_false(grepl("not found", results$check_sd_across$message, ignore.case = TRUE))
  expect_false(grepl("Unknown test", results$check_sd_across$message, ignore.case = TRUE))
})

test_that("execute_check can find and execute quality_test_any_flag_percentage", {
  # Create test data with flag columns with reproducible seed
  set.seed(12347)
  df <- tibble::tibble(
    id = 1:20,
    flag1 = sample(c(0, 1), 20, replace = TRUE),
    flag2 = sample(c(0, 1), 20, replace = TRUE),
    flag3 = sample(c(0, 1), 20, replace = TRUE)
  )
  
  dq <- DataQuality$new(data = df, dataset_name = "TestDQ")
  
  # Create schema with any_flag_percentage test
  custom_schema <- list(
    check_any_flag = list(
      check_name = "check_any_flag",
      check_label = "Check Any Flag Percentage",
      statistical_test = "any_flag_percentage",
      variables = c("flag1", "flag2", "flag3"),
      thresholds = list(
        list(expression = "test_statistic > 80", penalty = 5),
        list(expression = "test_statistic <= 80", penalty = 0)
      ),
      test_params = list(flag_value = 1)
    )
  )
  
  dq$set_quality_schema(custom_schema)
  results <- dq$run_quality_checks()
  
  # Check should execute successfully
  expect_false(is.na(results$check_any_flag$test_statistic))
  expect_false(grepl("not found", results$check_any_flag$message, ignore.case = TRUE))
  expect_false(grepl("Unknown test", results$check_any_flag$message, ignore.case = TRUE))
})

test_that("execute_check returns helpful error for truly unknown test", {
  df <- tibble::tibble(id = 1:10, x = rnorm(10))
  dq <- DataQuality$new(data = df, dataset_name = "TestDQ")
  
  # Create schema with non-existent test
  custom_schema <- list(
    check_bad = list(
      check_name = "check_bad",
      check_label = "Check with Bad Test",
      statistical_test = "nonexistent_test_xyz",
      variables = c("x"),
      thresholds = list(
        list(expression = "test_statistic > 0", penalty = 5)
      )
    )
  )
  
  dq$set_quality_schema(custom_schema)
  results <- dq$run_quality_checks()
  
  # Should return error message
  expect_true(grepl("Unknown test", results$check_bad$message))
  expect_true(grepl("nonexistent_test_xyz", results$check_bad$message))
  expect_true(grepl("not found", results$check_bad$message))
  expect_true(is.na(results$check_bad$test_statistic))
})


# ============================================================================
# Outputs Schema Tests
# ============================================================================

test_that("DataQuality initializes with outputs_schema, visualizations, and tables fields", {
  df <- tibble::tibble(id = 1:10, x = rnorm(10))
  
  dq <- DataQuality$new(data = df, dataset_name = "TestDQ")
  
  # Check that new fields exist
  expect_true(is.list(dq$outputs_schema))
  expect_true(is.list(dq$visualizations))
  expect_true(is.list(dq$tables))
  
  # They should be initialized as empty lists
  expect_length(dq$visualizations, 0)
  expect_length(dq$tables, 0)
})

test_that("DataQuality default_outputs_schema loads template", {
  df <- tibble::tibble(id = 1:10, x = rnorm(10))
  dq <- DataQuality$new(data = df)
  
  schema <- dq$get_outputs_schema()
  
  expect_true(is.list(schema))
  # Should have loaded from template (or be empty if template doesn't exist)
  # We just test it's a valid list structure
})

test_that("DataQuality can set and validate custom outputs schema", {
  df <- tibble::tibble(id = 1:5, val = 1:5)
  dq <- DataQuality$new(data = df)
  
  custom_outputs <- list(
    plot1 = list(
      output_name = "plot1",
      output_title = "Test Visualization",
      output_subtitle = "A test plot",
      variables = c("val"),
      disaggregation = NULL,
      output_func_name = "plot_histogram",
      test_params = list(bins = 30),
      output_type = "visualization"
    ),
    table1 = list(
      output_name = "table1",
      output_title = "Test Table",
      output_subtitle = "A test table",
      variables = c("val"),
      disaggregation = NULL,
      output_func_name = "generate_summary_table",
      test_params = NULL,
      output_type = "table"
    )
  )
  
  dq$set_outputs_schema(custom_outputs)
  
  expect_equal(dq$outputs_schema$plot1$output_name, "plot1")
  expect_equal(dq$outputs_schema$plot1$output_type, "visualization")
  expect_equal(dq$outputs_schema$table1$output_name, "table1")
  expect_equal(dq$outputs_schema$table1$output_type, "table")
})

test_that("DataQuality validates output_type field", {
  df <- tibble::tibble(id = 1:5)
  dq <- DataQuality$new(data = df)
  
  # Invalid output_type should error
  invalid_outputs <- list(
    bad_output = list(
      output_name = "bad_output",
      output_type = "invalid_type"
    )
  )
  
  expect_error(
    dq$set_outputs_schema(invalid_outputs),
    regexp = "invalid output_type"
  )
})

test_that("DataQuality outputs_schema_to_table converts schema to table", {
  df <- tibble::tibble(id = 1:5)
  dq <- DataQuality$new(data = df)
  
  custom_outputs <- list(
    plot1 = list(
      output_name = "plot1",
      output_title = "Test Visualization",
      output_subtitle = "A test plot",
      variables = c("val1", "val2"),
      disaggregation = c("group"),
      output_func_name = "plot_histogram",
      test_params = list(bins = 30, color = "blue"),
      output_type = "visualization"
    )
  )
  
  dq$set_outputs_schema(custom_outputs)
  
  tbl <- dq$export_outputs_schema()
  
  expect_s3_class(tbl, "data.frame")
  expect_true("output_name" %in% names(tbl))
  expect_true("output_type" %in% names(tbl))
  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$output_name[1], "plot1")
  expect_equal(tbl$output_type[1], "visualization")
})

test_that("DataQuality import_outputs_schema converts table to schema", {
  df <- tibble::tibble(id = 1:5)
  dq <- DataQuality$new(data = df)
  
  outputs_table <- tibble::tibble(
    output_name = c("plot1", "table1"),
    output_title = c("Title 1", "Title 2"),
    output_subtitle = c("Subtitle 1", "Subtitle 2"),
    variables = c("val1,val2", "val3"),
    disaggregation = c("group", NA),
    output_func_name = c("plot_histogram", "generate_table"),
    test_params = c("bins=30", NA),
    output_type = c("visualization", "table")
  )
  
  dq$import_outputs_schema(outputs_table)
  
  expect_equal(dq$outputs_schema$plot1$output_name, "plot1")
  expect_equal(dq$outputs_schema$plot1$output_type, "visualization")
  expect_equal(length(dq$outputs_schema$plot1$variables), 2)
  expect_equal(dq$outputs_schema$table1$output_name, "table1")
  expect_equal(dq$outputs_schema$table1$output_type, "table")
})

test_that("DataQuality outputs_schema includes outputs_group and outputs_per_group fields", {
  df <- tibble::tibble(id = 1:5, val = 1:5, enum_id = c(1, 1, 2, 2, 3))
  dq <- DataQuality$new(data = df)

  custom_outputs <- list(
    grouped_plot = list(
      output_name = "grouped_plot",
      output_title = "Grouped Plot",
      output_subtitle = "A grouped plot",
      variables = c("val"),
      disaggregation = NULL,
      output_func_name = "plot_histogram",
      test_params = list(bins = 10),
      output_type = "visualization",
      outputs_group = "my_group",
      outputs_per_group = "@variable_map$enum_id"
    )
  )

  dq$set_outputs_schema(custom_outputs)

  expect_equal(dq$outputs_schema$grouped_plot$outputs_group, "my_group")
  expect_equal(dq$outputs_schema$grouped_plot$outputs_per_group, "@variable_map$enum_id")

  # Export and re-import to verify round-trip
  tbl <- dq$export_outputs_schema()
  expect_true("outputs_group" %in% names(tbl))
  expect_true("outputs_per_group" %in% names(tbl))
  expect_equal(tbl$outputs_group[1], "my_group")
  expect_equal(tbl$outputs_per_group[1], "@variable_map$enum_id")
})

test_that("DataQuality run_outputs stores visualizations in nested group structure", {
  simple_plot_fn <- function(data) list(type = "plot", nrows = nrow(data))

  df <- tibble::tibble(id = 1:6, val = 1:6)
  dq <- DataQuality$new(data = df)

  custom_outputs <- list(
    grouped_plot = list(
      output_name = "grouped_plot",
      output_title = "my_plot",
      output_subtitle = NULL,
      variables = c("val"),
      disaggregation = NULL,
      output_func_name = "simple_plot_fn",
      test_params = NULL,
      output_type = "visualization",
      outputs_group = "my_group",
      outputs_per_group = NULL
    )
  )

  dq$set_outputs_schema(custom_outputs)

  assign("simple_plot_fn", simple_plot_fn, envir = globalenv())
  on.exit(rm("simple_plot_fn", envir = globalenv()), add = TRUE)

  dq$run_outputs()

  expect_true(is.list(dq$visualizations$my_group))
  expect_true("my_plot" %in% names(dq$visualizations$my_group))
})

test_that("DataQuality run_outputs creates per-group outputs using outputs_per_group", {
  simple_plot_fn <- function(data) list(type = "plot", nrows = nrow(data))

  df <- tibble::tibble(
    id = 1:6,
    val = 1:6,
    enum_id = c(1L, 1L, 2L, 2L, 3L, 3L)
  )
  dq <- DataQuality$new(
    data = df,
    variable_map = list(enum_id = "enum_id")
  )

  custom_outputs <- list(
    per_group_plot = list(
      output_name = "per_group_plot",
      output_title = "my_bar",
      output_subtitle = NULL,
      variables = c("val"),
      disaggregation = NULL,
      output_func_name = "simple_plot_fn",
      test_params = NULL,
      output_type = "visualization",
      outputs_group = "checks_group",
      outputs_per_group = "@variable_map$enum_id"
    )
  )

  dq$set_outputs_schema(custom_outputs)

  assign("simple_plot_fn", simple_plot_fn, envir = globalenv())
  on.exit(rm("simple_plot_fn", envir = globalenv()), add = TRUE)

  dq$run_outputs()

  group_vis <- dq$visualizations$checks_group
  expect_true(is.list(group_vis))
  expect_true("my_bar-enum_id.1" %in% names(group_vis))
  expect_true("my_bar-enum_id.2" %in% names(group_vis))
  expect_true("my_bar-enum_id.3" %in% names(group_vis))
  expect_equal(group_vis$`my_bar-enum_id.1`$nrows, 2L)
})




test_that("DataQuality results_to_table includes check_group column", {
  df <- tibble::tibble(id = 1:5)
  dq <- DataQuality$new(data = df)

  tbl <- dq$results_to_table()

  expect_s3_class(tbl, "tbl_df")
  expect_true("check_group" %in% names(tbl))
  expect_equal(nrow(tbl), 0)
})

test_that("DataQuality execute_check propagates check_group to results", {
  df <- tibble::tibble(id = 1:10, val = 1:10)
  dq <- DataQuality$new(data = df)

  custom_schema <- list(
    check_a = list(
      check_name       = "check_a",
      check_label      = "Check A",
      check_group      = "group1",
      variables        = c("val"),
      statistical_test = "missing_prop",
      thresholds = list(
        list(threshold_expression = "test_statistic < 50", penalty_score = 0),
        list(threshold_expression = "test_statistic >= 50", penalty_score = 5)
      ),
      test_params = list()
    )
  )

  dq$set_quality_schema(custom_schema)
  dq$run_quality_checks()

  tbl <- dq$results_to_table()

  expect_true("check_group" %in% names(tbl))
  expect_equal(tbl$check_group[tbl$check_name == "check_a"], "group1")
})

# ============================================================
# run_quality_checks plausibility tables tests
# ============================================================

test_that("run_quality_checks stores penalty_summary in tables[['plausibility']]", {
  df <- tibble::tibble(id = 1:10, val = rnorm(10))

  custom_schema <- list(
    check_val = list(
      check_name       = "check_val",
      check_label      = "Check Val",
      check_group      = "group1",
      statistical_test = "missing_prop",
      variables        = c("val"),
      thresholds       = list(
        list(expression = "test_statistic < 50", penalty = 0),
        list(expression = "test_statistic >= 50", penalty = 5)
      ),
      test_params = list()
    )
  )

  dq <- DataQuality$new(data = df, dataset_name = "TestDQ")
  dq$set_quality_schema(custom_schema)
  dq$run_quality_checks()

  expect_true(is.list(dq$tables[["plausibility"]]))
  expect_true("penalty_summary" %in% names(dq$tables[["plausibility"]]))
  expect_s3_class(dq$tables[["plausibility"]][["penalty_summary"]], "flextable")
})

test_that("run_quality_checks stores penalty_summary_by_enum_id when enum_id is in variable_map", {
  set.seed(1)
  df <- tibble::tibble(
    id       = 1:20,
    val      = rnorm(20),
    enum_col = rep(c("E001", "E002"), each = 10)
  )

  var_map <- list(enum_id = "enum_col")

  custom_schema <- list(
    check_val = list(
      check_name       = "check_val",
      check_label      = "Check Val",
      statistical_test = "missing_prop",
      variables        = c("val"),
      thresholds       = list(
        list(expression = "test_statistic < 50", penalty = 0)
      ),
      test_params = list()
    )
  )

  dq <- DataQuality$new(
    data         = df,
    variable_map = var_map,
    dataset_name = "TestDQ"
  )
  dq$set_quality_schema(custom_schema)
  dq$run_quality_checks()

  expect_true("penalty_summary_by_enum_id" %in% names(dq$tables[["plausibility"]]))
  expect_s3_class(
    dq$tables[["plausibility"]][["penalty_summary_by_enum_id"]],
    "flextable"
  )
})

test_that("run_quality_checks stores penalty_summary_by_stratum when stratum is in variable_map", {
  set.seed(2)
  df <- tibble::tibble(
    id          = 1:20,
    val         = rnorm(20),
    stratum_col = rep(c("urban", "rural"), each = 10)
  )

  var_map <- list(stratum = "stratum_col")

  custom_schema <- list(
    check_val = list(
      check_name       = "check_val",
      check_label      = "Check Val",
      statistical_test = "missing_prop",
      variables        = c("val"),
      thresholds       = list(
        list(expression = "test_statistic < 50", penalty = 0)
      ),
      test_params = list()
    )
  )

  dq <- DataQuality$new(
    data         = df,
    variable_map = var_map,
    dataset_name = "TestDQ"
  )
  dq$set_quality_schema(custom_schema)
  dq$run_quality_checks()

  expect_true("penalty_summary_by_stratum" %in% names(dq$tables[["plausibility"]]))
  expect_s3_class(
    dq$tables[["plausibility"]][["penalty_summary_by_stratum"]],
    "flextable"
  )
})

test_that("run_quality_checks does not create by-group tables when enum_id/stratum absent", {
  df <- tibble::tibble(id = 1:10, val = rnorm(10))

  custom_schema <- list(
    check_val = list(
      check_name       = "check_val",
      check_label      = "Check Val",
      statistical_test = "missing_prop",
      variables        = c("val"),
      thresholds       = list(
        list(expression = "test_statistic < 50", penalty = 0)
      ),
      test_params = list()
    )
  )

  dq <- DataQuality$new(data = df, dataset_name = "TestDQ")
  dq$set_quality_schema(custom_schema)
  dq$run_quality_checks()

  plaus <- dq$tables[["plausibility"]]
  expect_false("penalty_summary_by_enum_id" %in% names(plaus))
  expect_false("penalty_summary_by_stratum"  %in% names(plaus))
})

test_that("run_quality_checks stores individual per-enumerator penalty summary tables", {
  set.seed(3)
  df <- tibble::tibble(
    id       = 1:20,
    val      = rnorm(20),
    enum_col = rep(c("E001", "E002"), each = 10)
  )

  var_map <- list(enum_id = "enum_col")

  custom_schema <- list(
    check_val = list(
      check_name       = "check_val",
      check_label      = "Check Val",
      statistical_test = "missing_prop",
      variables        = c("val"),
      thresholds       = list(
        list(expression = "test_statistic < 50", penalty = 0)
      ),
      test_params = list()
    )
  )

  dq <- DataQuality$new(
    data         = df,
    variable_map = var_map,
    dataset_name = "TestDQ"
  )
  dq$set_quality_schema(custom_schema)
  dq$run_quality_checks()

  plaus <- dq$tables[["plausibility"]]
  # Individual per-enumerator tables should be stored
  expect_true("penalty_summary_enum_id_E001" %in% names(plaus))
  expect_true("penalty_summary_enum_id_E002" %in% names(plaus))
  expect_s3_class(plaus[["penalty_summary_enum_id_E001"]], "flextable")
  expect_s3_class(plaus[["penalty_summary_enum_id_E002"]], "flextable")
})

test_that("run_quality_checks stores individual per-stratum penalty summary tables", {
  set.seed(4)
  df <- tibble::tibble(
    id          = 1:20,
    val         = rnorm(20),
    stratum_col = rep(c("urban", "rural"), each = 10)
  )

  var_map <- list(stratum = "stratum_col")

  custom_schema <- list(
    check_val = list(
      check_name       = "check_val",
      check_label      = "Check Val",
      statistical_test = "missing_prop",
      variables        = c("val"),
      thresholds       = list(
        list(expression = "test_statistic < 50", penalty = 0)
      ),
      test_params = list()
    )
  )

  dq <- DataQuality$new(
    data         = df,
    variable_map = var_map,
    dataset_name = "TestDQ"
  )
  dq$set_quality_schema(custom_schema)
  dq$run_quality_checks()

  plaus <- dq$tables[["plausibility"]]
  # Individual per-stratum tables should be stored
  expect_true("penalty_summary_stratum_rural" %in% names(plaus))
  expect_true("penalty_summary_stratum_urban" %in% names(plaus))
  expect_s3_class(plaus[["penalty_summary_stratum_rural"]], "flextable")
  expect_s3_class(plaus[["penalty_summary_stratum_urban"]], "flextable")
})
