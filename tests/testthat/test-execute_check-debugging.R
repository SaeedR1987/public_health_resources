library(testthat)
library(tibble)

# ============================================================================
# Tests for execute_check function debugging issues
# ============================================================================

test_that("execute_check properly finds quality_test functions and returns results", {
  # Create test data with reproducible seed
  set.seed(12345)
  df <- tibble::tibble(
    id = 1:20,
    values = rnorm(20, mean = 100, sd = 15)
  )
  
  dq <- DataQuality$new(data = df, dataset_name = "TestDQ")
  
  # Create schema with sd test - this was failing with "Unknown test" message
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
  
  # Test that execute_check found the function and executed it
  expect_true(!is.null(results$check_sd), "Result should not be NULL")
  expect_false(is.na(results$check_sd$test_statistic), "Test statistic should not be NA")
  expect_false(grepl("Unknown test", results$check_sd$message, ignore.case = TRUE), 
               "Should not get 'Unknown test' message")
  expect_false(grepl("not found", results$check_sd$message, ignore.case = TRUE),
               "Should not get 'not found' message")
  expect_true(is.numeric(results$check_sd$test_statistic), "Test statistic should be numeric")
  expect_true(results$check_sd$test_statistic > 0, "Test statistic should be positive")
})

test_that("execute_check returns result object even when function is truly not found", {
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
  
  # Result should be a list, not NULL
  expect_true(!is.null(results$check_bad), "Result should not be NULL even for bad test")
  expect_true(is.list(results$check_bad), "Result should be a list")
  expect_true("message" %in% names(results$check_bad), "Result should have a message field")
  expect_true(grepl("Unknown test", results$check_bad$message), "Should have 'Unknown test' message")
  expect_true(grepl("nonexistent_test_xyz", results$check_bad$message), "Message should mention test name")
  expect_true(is.na(results$check_bad$test_statistic), "Test statistic should be NA for bad test")
})

test_that("execute_check can find all exported quality test functions", {
  # List of all quality test functions that should be available
  test_names <- c(
    "correlation",
    "ttest", 
    "chisq",
    "flag_percentage",
    "missing_percentage",
    "outlier_percentage",
    "coefficient_variation",
    "range_violation",
    "sd",
    "sd_across_percentage",
    "any_flag_percentage"
  )
  
  # Create test data
  set.seed(123)
  df <- tibble::tibble(
    id = 1:20,
    x = rnorm(20),
    y = rnorm(20),
    flag1 = sample(c(0, 1), 20, replace = TRUE),
    flag2 = sample(c(0, 1), 20, replace = TRUE),
    flag3 = sample(c(0, 1), 20, replace = TRUE)
  )
  
  dq <- DataQuality$new(data = df, dataset_name = "TestDQ")
  
  for (test_name in test_names) {
    # Create appropriate schema for each test type
    if (test_name == "correlation") {
      # Correlation requires exactly 2 numeric variables
      schema <- list(
        test_check = list(
          check_name = paste0("check_", test_name),
          check_label = paste("Check", test_name),
          statistical_test = test_name,
          variables = c("x", "y"),
          thresholds = list(list(expression = "TRUE", penalty = 0))
        )
      )
    } else if (test_name %in% c("sd_across_percentage", "any_flag_percentage")) {
      # These tests operate across multiple columns
      # sd_across_percentage: checks if SD across columns is below threshold (0.8)
      # any_flag_percentage: checks if any column has the flag value (1)
      test_params_for_type <- if (test_name == "sd_across_percentage") {
        list(threshold = 0.8)  # Threshold for flagging low SD
      } else {
        list(flag_value = 1)  # Value to count as a flag
      }
      
      schema <- list(
        test_check = list(
          check_name = paste0("check_", test_name),
          check_label = paste("Check", test_name),
          statistical_test = test_name,
          variables = c("flag1", "flag2", "flag3"),
          thresholds = list(list(expression = "TRUE", penalty = 0)),
          test_params = test_params_for_type
        )
      )
    } else {
      # Single-variable tests (sd, outlier_percentage, etc.)
      schema <- list(
        test_check = list(
          check_name = paste0("check_", test_name),
          check_label = paste("Check", test_name),
          statistical_test = test_name,
          variables = c("x"),
          thresholds = list(list(expression = "TRUE", penalty = 0))
        )
      )
    }
    
    dq$set_quality_schema(schema)
    results <- dq$run_quality_checks()
    
    # Check that function was found and executed
    expect_true(!is.null(results$test_check), 
                info = paste("Result should not be NULL for test:", test_name))
    expect_false(grepl("Unknown test", results$test_check$message, ignore.case = TRUE),
                info = paste("Should not get 'Unknown test' for:", test_name))
    expect_false(grepl("not found", results$test_check$message, ignore.case = TRUE),
                info = paste("Should not get 'not found' for:", test_name))
  }
})

test_that("execute_check properly handles errors without returning NULL", {
  # Create test data with a variable that will cause issues
  df <- tibble::tibble(
    id = 1:10,
    x = rep(5, 10)  # All same value - will cause issues with some tests
  )
  
  dq <- DataQuality$new(data = df, dataset_name = "TestDQ")
  
  # Test 1: Variable with zero variance (should succeed with SD = 0)
  custom_schema <- list(
    check_sd = list(
      check_name = "check_sd",
      check_label = "Check SD",
      statistical_test = "sd",
      variables = c("x"),
      thresholds = list(
        list(expression = "test_statistic > 0", penalty = 5)
      )
    )
  )
  
  dq$set_quality_schema(custom_schema)
  results <- dq$run_quality_checks()
  
  # Even with problematic data, execute_check should return a result, not NULL
  expect_true(!is.null(results$check_sd), "Result should not be NULL even with problematic data")
  expect_true(is.list(results$check_sd), "Result should be a list")
  expect_true("check_name" %in% names(results$check_sd), "Result should have check_name")
  expect_equal(results$check_sd$test_statistic, 0, "SD of constant values should be 0")
  
  # Test 2: Missing required variable (should return result with error message, not NULL)
  custom_schema2 <- list(
    check_missing = list(
      check_name = "check_missing",
      check_label = "Check Missing Variable", 
      statistical_test = "sd",
      variables = c("nonexistent_column"),
      thresholds = list(
        list(expression = "test_statistic > 0", penalty = 5)
      )
    )
  )
  
  dq$set_quality_schema(custom_schema2)
  results2 <- dq$run_quality_checks()
  
  # Should return a result object with error message, not NULL
  expect_true(!is.null(results2$check_missing), "Result should not be NULL for missing variable")
  expect_true(is.list(results2$check_missing), "Result should be a list")
  expect_true(grepl("not found", results2$check_missing$message, ignore.case = TRUE),
              "Should have 'not found' error message")
  expect_true(is.na(results2$check_missing$test_statistic), 
              "Test statistic should be NA for missing variable")
  
  # Test 3: Test function that might fail internally (insufficient data)
  df3 <- tibble::tibble(
    id = 1:2,
    x = c(1, NA)  # Only 1 non-NA value
  )
  
  dq3 <- DataQuality$new(data = df3, dataset_name = "TestDQ3")
  
  custom_schema3 <- list(
    check_sd_fail = list(
      check_name = "check_sd_fail",
      check_label = "Check SD with insufficient data",
      statistical_test = "sd",
      variables = c("x"),
      thresholds = list(
        list(expression = "test_statistic > 0", penalty = 5)
      )
    )
  )
  
  dq3$set_quality_schema(custom_schema3)
  results3 <- dq3$run_quality_checks()
  
  # Should return a result object, not NULL, even if internal calculation fails
  expect_true(!is.null(results3$check_sd_fail), 
              "Result should not be NULL even when test function fails internally")
  expect_true(is.list(results3$check_sd_fail), "Result should be a list")
  # Test statistic should be NA if calculation failed
  expect_true(is.na(results3$check_sd_fail$test_statistic) || results3$check_sd_fail$test_statistic == 0,
              "Test statistic should be NA or 0 when calculation fails")
})
