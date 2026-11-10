# QUALITY_SCHEMA_TO_TABLE ####

test_that("quality_schema_to_table converts a normal QC schema correctly", {

  qc_schema <- list(
    completeness = list(
      check_name  = "completeness",
      check_label = "Check completeness",
      variables   = c("age", "sex"),
      statistical_test = "missing_percentage",
      thresholds = list(
        list(threshold_expression = "test_statistic < 5", penalty_score = 5)
      ),
      test_params = list()
    )
  )

  tab <- quality_schema_to_table(qc_schema)

  expect_equal(nrow(tab), 1)

  # Core metadata
  expect_equal(tab$check_name,  "completeness")
  expect_equal(tab$check_label, "Check completeness")

  # Vectors collapsed to comma-separated
  expect_equal(tab$variables,  "age,sex")

  # Methods + thresholds
  expect_equal(tab$statistical_test, "missing_percentage")
  expect_true(grepl("test_statistic < 5", tab$threshold_expression))

})

# EMPTY SCHEMA

test_that("quality_schema_to_table returns an empty table for an empty schema", {

  qc_schema <- list()  # no checks

  tab <- quality_schema_to_table(qc_schema)
  expect_equal(nrow(tab), 0)
})

test_that("quality_schema_to_table expands multiple QC checks", {

  qc_schema <- list(
    CheckA = list(
      check_name  = "CheckA",
      check_label = "A label",
      variables   = c("x"),
      statistical_test = "missing_percentage",
      thresholds = list(
        list(threshold_expression = "test_statistic < 1", penalty_score = 2)
      ),
      test_params = list()
    ),
    CheckB = list(
      check_name  = "CheckB",
      check_label = "B label",
      variables   = c("y", "z"),
      statistical_test = "flag_percentage",
      thresholds = list(
        list(threshold_expression = "test_statistic < 2", penalty_score = 4)
      ),
      test_params = list()
    )
  )

  tab <- quality_schema_to_table(qc_schema)

  expect_equal(nrow(tab), 2)

  # check names are correct
  expect_setequal(tab$check_name, c("CheckA", "CheckB"))

  # variables collapsed correctly
  expect_equal(tab$variables[tab$check_name == "CheckB"], "y,z")

})


# QUALITY_VALIDATE_SCHEMA_TO_TABLE ####

test_that("quality_validate_schema_to_table accepts a normal schema", {

  qc_schema <- list(
    Completeness = list(
      check_name  = "Completeness",
      check_label = "Label",
      variables   = c("age", "sex"),
      statistical_test = "missing_percentage",
      thresholds = list(
        list(threshold_expression = "test_statistic <= 0.1", penalty_score = 4)
      ),
      test_params = list()
    )
  )

  expect_silent(quality_validate_schema_to_table(qc_schema))
})

test_that("quality_validate_schema_to_table accepts multiple checks", {

  qc_schema <- list(
    A = list(
      check_name  = "A",
      check_label = "Check A",
      variables   = c("x"),
      statistical_test = "missing_percentage",
      thresholds = list(
        list(threshold_expression = "test_statistic == 1", penalty_score = 2)
      ),
      test_params = list()
    ),
    B = list(
      check_name  = "B",
      check_label = "Label B",
      variables   = c("y", "z"),
      statistical_test = "flag_percentage",
      thresholds = list(
        list(threshold_expression = "test_statistic < 5", penalty_score = 5)
      ),
      test_params = list()
    )
  )

  expect_silent(quality_validate_schema_to_table(qc_schema))
})

test_that("quality_validate_schema_to_table errors on empty threshold_expression", {

  qc_schema <- list(
    EmptyCase = list(
      check_name  = "EmptyCase",
      check_label = "Empty thresholds",
      variables   = NULL,
      statistical_test = "missing_percentage",
      thresholds = list(
        # character(0) and numeric(0) are invalid — threshold_expression must be a single string
        list(threshold_expression = character(0), penalty_score = numeric(0))
      ),
      test_params = list()
    )
  )

  expect_error(
    quality_validate_schema_to_table(qc_schema),
    regexp = "threshold_expression.*must be a single character string",
    class = "phr_error"
  )
})

test_that("quality_validate_schema_to_table errors if top-level is not a list", {

  expect_error(
    quality_validate_schema_to_table(NULL),
    regexp = "Quality schema must be a list object"
  )
})

test_that("quality_validate_schema_to_table returns TRUE for empty schema", {

  qc_schema <- list()

  expect_true(quality_validate_schema_to_table(qc_schema))
})

test_that("quality_validate_schema_to_table errors when a check is not a list", {

  qc_schema <- list(
    invalid = "not a list"
  )

  expect_error(
    quality_validate_schema_to_table(qc_schema),
    regexp = "must be a list"
  )
})

test_that("quality_validate_schema_to_table errors when required fields are missing", {

  qc_schema <- list(
    BadCase = list(
      check_name = "BadCase"
      # missing check_label, statistical_test, thresholds, test_params
    )
  )

  expect_error(
    quality_validate_schema_to_table(qc_schema),
    regexp = "missing required fields"
  )
})

test_that("quality_validate_schema_to_table errors when list name does not match check_name", {

  qc_schema <- list(
    list_name_here = list(
      check_name  = "different_check_name",
      check_label = "Label",
      variables   = c("x"),
      statistical_test = "missing_percentage",
      thresholds = list(
        list(threshold_expression = "test_statistic < 1", penalty_score = 5)
      ),
      test_params = list()
    )
  )

  expect_error(
    quality_validate_schema_to_table(qc_schema),
    regexp = "does not match check_name"
  )
})

test_that("quality_validate_schema_to_table errors on wrong type for check_name", {

  qc_schema <- list(
    t1 = list(
      check_name  = 123,  # invalid
      check_label = "label",
      variables = c("x"),
      statistical_test = "missing_percentage",
      thresholds = list(
        list(threshold_expression = "test_statistic == 1", penalty_score = 5)
      ),
      test_params = list()
    )
  )

  expect_error(
    quality_validate_schema_to_table(qc_schema),
    regexp = "check_name.*must be a single character"
  )
})

test_that("quality_validate_schema_to_table errors on wrong type for variables", {

  qc_schema <- list(
    A = list(
      check_name  = "A",
      check_label = "Label",
      variables   = list(a = 1),  # invalid: must be character vector
      statistical_test = "missing_percentage",
      thresholds = list(
        list(threshold_expression = "test_statistic == 1", penalty_score = 5)
      ),
      test_params = list()
    )
  )

  expect_error(
    quality_validate_schema_to_table(qc_schema),
    regexp = "variables.*character vector"
  )
})

test_that("quality_validate_schema_to_table errors on wrong type for threshold expression", {

  qc_schema <- list(
    Bad = list(
      check_name  = "Bad",
      check_label = "Label",
      variables = c("x"),
      statistical_test = "missing_percentage",
      thresholds = list(
        list(threshold_expression = list(),  # invalid: must be character string
             penalty_score = 5)
      ),
      test_params = list()
    )
  )

  expect_error(
    quality_validate_schema_to_table(qc_schema),
    regexp = "*must be a single character string"
  )
})

test_that("quality_validate_schema_to_table errors on wrong type for penalty_score", {

  qc_schema <- list(
    Bad = list(
      check_name  = "Bad",
      check_label = "Label",
      variables = c("x"),
      statistical_test = "missing_percentage",
      thresholds = list(
        list(
          threshold_expression = "test_statistic < 1",
          penalty_score = "not numeric"   # invalid
        )
      ),
      test_params = list()
    )
  )

  expect_error(
    quality_validate_schema_to_table(qc_schema),
    regexp = "penalty_score.*numeric"
  )
})

# QUALITY_TABLE_TO_SCHEMA ####

test_that("quality_table_to_schema converts a normal table correctly", {

  df <- tibble::tibble(
    check_name  = "completeness",
    check_label = "Check completeness",
    variables   = "age,sex",
    statistical_test = "missing_percentage",
    threshold_expression  = "test_statistic <= 5",
    penalty_score = 5,
    test_params = NA
  )

  out <- quality_table_to_schema(df)

  expect_true(is.list(out$checks))
  expect_equal(names(out$checks), "completeness")

  chk <- out$checks$completeness

  expect_equal(chk$check_label, "Check completeness")
  expect_equal(chk$variables, c("age", "sex"))
  expect_equal(chk$statistical_test, "missing_percentage")
  # --- revised expectations for thresholds ---
  expect_true(is.list(chk$thresholds))
  expect_length(chk$thresholds, 1)

  expect_equal(
    chk$thresholds[[1]]$threshold_expression,
    "test_statistic <= 5"
  )
  expect_equal(
    chk$thresholds[[1]]$penalty_score,
    5
  )
})

test_that("quality_table_to_schema converts multiple checks correctly", {

  df <- tibble::tibble(
    check_name  = c("A", "A", "B"),
    check_label = c("Label A", "Label A", "Label B"),
    variables   = c("x,y", "x,y", "p,q,r"),
    statistical_test = c("missing_percentage", "missing_percentage", "flag_percentage"),
    threshold_expression = c("test_statistic <= 5", "test_statistic > 5", "test_statistic < 10"),
    penalty_score = c(1, 2, 5),
    test_params = NA
  )

  out <- quality_table_to_schema(df)

  expect_setequal(names(out$checks), c("A","B"))

  expect_equal(out$checks$A$variables, c("x","y"))
  expect_equal(out$checks$B$variables, c("p","q","r"))

  expect_length(out$checks$A$thresholds, 2)
  expect_length(out$checks$B$thresholds, 1)

  expect_equal(
    out$checks$A$thresholds[[1]]$threshold_expression,
    "test_statistic <= 5"
  )
  expect_equal(
    out$checks$A$thresholds[[1]]$penalty_score,
    1
  )
})

test_that("quality_table_to_schema errors on NA values in required fields (except test_params)", {

  df <- tibble::tibble(
    check_name  = "C",
    check_label = "Label C",
    variables   = NA_character_,
    statistical_test = "methodC",
    threshold_expression  = NA_character_,
    penalty_score = NA_real_,
    test_params = NA_character_
  )

  expect_error(
    quality_table_to_schema(df),
    regexp = "Missing values \\(NA\\) detected in required fields"
  )
})

test_that("quality_table_to_schema errors when required columns are missing", {

  df <- tibble::tibble(
    check_name = "X"
  )

  expect_error(
    quality_table_to_schema(df),
    regexp = "required"
  )
})

test_that("quality_table_to_schema errors on non-dataframe input", {

  expect_error(
    quality_table_to_schema(list()),
    regexp = "data frame"
  )
})

test_that("quality_table_to_schema handles empty tables", {

  df <- tibble::tibble(
    check_name = character(0),
    check_label = character(0),
    variables = character(0),
    statistical_test = character(0),
    threshold_expression = character(0),
    penalty_score = numeric(0),
    test_params = character(0)
  )

  out <- quality_table_to_schema(df)

  expect_equal(out$checks, list())
})

test_that("quality_schema_to_table and quality_table_to_schema round-trip correctly", {

  original <- list(
    A = list(
      check_name  = "A",
      check_label = "Label A",
      variables = c("x","y"),
      statistical_test = "missing_percentage",
      thresholds = list(
        list(threshold_expression = "test_statistic <= 5", penalty_score = 1),
        list(threshold_expression = "test_statistic > 5", penalty_score = 3)
      ),
      test_params = NULL
    )
  )

  tab  <- quality_schema_to_table(original)
  back <- quality_table_to_schema(tab)

  expect_equal(names(back$checks), "A")
  expect_equal(back$checks$A$variables, c("x","y"))

  expect_length(back$checks$A$thresholds, 2)
  expect_equal(
    back$checks$A$thresholds[[2]]$penalty_score,
    3
  )
})




# QUALITY_VALIDATE_TABLE_TO_SCHEMA ####

test_that("quality_validate_table_to_schema accepts a valid QC table", {

  df <- tibble::tibble(
    check_name  = c("CheckA", "CheckB"),
    check_label = c("Label A", "Label B"),
    variables   = c("age,sex", "y,z"),
    statistical_test = c("missing_percentage", "flag_percentage"),
    threshold_expression = c("test_statistic <= 5", "test_statistic < 10"),
    penalty_score = c(1, 2),
    test_params = c(NA, NA)
  )

  expect_true(quality_validate_table_to_schema(df))
})

test_that("quality_validate_table_to_schema errors when required columns are missing", {

  df <- tibble::tibble(
    check_name = "X"
  )

  expect_error(
    quality_validate_table_to_schema(df),
    regexp = "required|missing"
  )
})

test_that("quality_validate_table_to_schema errors on duplicate identical rows", {

  df <- tibble::tibble(
    check_name  = c("A", "A"),
    check_label = c("Label A", "Label A"),
    variables   = c("v1", "v1"),
    statistical_test = c("missing_percentage", "missing_percentage"),
    threshold_expression = c("test_statistic < 5", "test_statistic < 5"),
    penalty_score = c(1, 1),
    test_params = c(NA, NA)
  )

  expect_error(
    quality_validate_table_to_schema(df),
    regexp = "Duplicate rows detected|Duplicate"
  )
})

test_that("quality_validate_table_to_schema errors on NA values in required fields", {

  df <- tibble::tibble(
    check_name  = "CheckX",
    check_label = "Label X",
    variables   = NA_character_,
    statistical_test = "flag_percentage",
    threshold_expression = "test_statistic < 10",
    penalty_score = 4,
    test_params = NA
  )

  expect_error(
    quality_validate_table_to_schema(df),
    regexp = "Missing values \\(NA\\) detected in required fields"
  )
})

test_that("quality_validate_table_to_schema errors when input is not a data frame", {

  expect_error(
    quality_validate_table_to_schema("not a df"),
    regexp = "data frame"
  )
})

test_that("quality_validate_table_to_schema errors on NA threshold_expression and penalty_score", {

  df <- tibble::tibble(
    check_name  = "C1",
    check_label = "Label",
    variables   = "x,y",
    statistical_test = "flag_percentage",
    threshold_expression = NA_character_,
    penalty_score = NA_real_,
    test_params = NA
  )

  expect_error(
    quality_validate_table_to_schema(df),
    regexp = "Missing values \\(NA\\) detected in required fields"
  )
})


test_that("quality_validate_table_to_schema errors on empty table", {

  df <- tibble::tibble()

  expect_error(
    quality_validate_table_to_schema(df),
    regexp = "required|missing"
  )
})

test_that("quality_table_to_schema correctly parses test_params with whitespace", {
  # Test that test_params are properly parsed even with extra whitespace
  # This tests the fix for the issue where parameter names had trailing spaces
  
  df <- tibble::tibble(
    check_name = "test_check",
    check_label = "Test Check with Params",
    variables = "var1",
    statistical_test = "flag_percentage",
    threshold_expression = "test_statistic > 5",
    penalty_score = 5,
    test_params = "flag_value = 1"  # Note the spaces around =
  )
  
  schema <- quality_table_to_schema(df)
  
  expect_true(is.list(schema$checks))
  expect_true("test_check" %in% names(schema$checks))
  
  check <- schema$checks$test_check
  
  # Verify test_params were parsed correctly
  expect_true(is.list(check$test_params))
  expect_true("flag_value" %in% names(check$test_params))
  expect_equal(check$test_params$flag_value, 1)
  
  # Verify no trailing spaces in parameter names
  expect_false("flag_value " %in% names(check$test_params))
  expect_false(" flag_value" %in% names(check$test_params))
})

test_that("quality_table_to_schema handles multiple test_params with various whitespace", {
  # Test with multiple parameters and various whitespace patterns
  
  df <- tibble::tibble(
    check_name = "test_check",
    check_label = "Test Check",
    variables = "var1,var2",
    statistical_test = "correlation",
    threshold_expression = "test_statistic > 0.5",
    penalty_score = 3,
    test_params = "method = pearson, min_value= 0 , max_value =10"  # Various whitespace patterns
  )
  
  schema <- quality_table_to_schema(df)
  check <- schema$checks$test_check
  
  # Verify all parameters were parsed correctly without extra spaces
  expect_true(is.list(check$test_params))
  expect_equal(length(check$test_params), 3)
  expect_true("method" %in% names(check$test_params))
  expect_true("min_value" %in% names(check$test_params))
  expect_true("max_value" %in% names(check$test_params))
  
  # Verify no spaces in parameter names
  for (param_name in names(check$test_params)) {
    expect_false(grepl("^\\s|\\s$", param_name), 
                 info = paste("Parameter name should not have leading/trailing spaces:", param_name))
  }
  
  # Verify values are correct
  expect_equal(check$test_params$method, "pearson")
  expect_equal(check$test_params$min_value, 0)
  expect_equal(check$test_params$max_value, 10)
})

test_that("quality_table_to_schema parses vectored c(...) test_params correctly", {
  # Regression test: expected_ratio = c(0.4118, 0.5882) was previously broken
  # by a naive comma-split that treated the comma inside c() as a separator.

  df <- tibble::tibble(
    check_name = "plaus_ageratio_01_25",
    check_label = "Age Ratio 0-1 vs 2-5",
    variables = "age_0_1,age_2_5",
    statistical_test = "ageratio_count",
    threshold_expression = "p_value >= 0.1",
    penalty_score = 0,
    test_params = "expected_ratio = c(0.4118, 0.5882)"
  )

  schema <- quality_table_to_schema(df)
  check <- schema$checks$plaus_ageratio_01_25

  expect_true(is.list(check$test_params))
  expect_true("expected_ratio" %in% names(check$test_params))
  # The value should be preserved as the c(...) string for later resolution
  expect_equal(check$test_params$expected_ratio, "c(0.4118, 0.5882)")
})



# CHECK_GROUP SUPPORT ####

test_that("quality_schema_to_table includes check_group column", {

  qc_schema <- list(
    fcs_check = list(
      check_name       = "fcs_check",
      check_label      = "FCS Check",
      check_group      = "fcs",
      variables        = c("fcs_score"),
      statistical_test = "missing_prop",
      thresholds = list(
        list(threshold_expression = "test_statistic < 5", penalty_score = 0)
      ),
      test_params = list()
    )
  )

  tab <- quality_schema_to_table(qc_schema)

  expect_true("check_group" %in% names(tab))
  expect_equal(tab$check_group, "fcs")
})

test_that("quality_schema_to_table outputs NA check_group when not provided", {

  qc_schema <- list(
    no_group_check = list(
      check_name       = "no_group_check",
      check_label      = "No Group Check",
      variables        = c("score"),
      statistical_test = "missing_prop",
      thresholds = list(
        list(threshold_expression = "test_statistic < 5", penalty_score = 0)
      ),
      test_params = list()
    )
  )

  tab <- quality_schema_to_table(qc_schema)

  expect_true("check_group" %in% names(tab))
  expect_true(is.na(tab$check_group))
})

test_that("quality_table_to_schema reads check_group from table", {

  df <- tibble::tibble(
    check_group      = "fcs",
    check_name       = "fcs_check",
    check_label      = "FCS Check",
    variables        = "fcs_score",
    statistical_test = "missing_prop",
    threshold_expression = "test_statistic < 5",
    penalty_score    = 0,
    test_params      = NA
  )

  schema <- quality_table_to_schema(df)
  checks <- schema$checks

  expect_equal(checks$fcs_check$check_group, "fcs")
})

test_that("quality_table_to_schema handles table without check_group column", {

  df <- tibble::tibble(
    check_name       = "check_a",
    check_label      = "Check A",
    variables        = "var1",
    statistical_test = "missing_prop",
    threshold_expression = "test_statistic < 5",
    penalty_score    = 0,
    test_params      = NA
  )

  schema <- quality_table_to_schema(df)
  checks <- schema$checks

  # Should succeed and check_group should be NULL
  expect_null(checks$check_a$check_group)
})

test_that("quality_validate_schema_to_table allows optional check_group field", {

  qc_schema <- list(
    check_with_group = list(
      check_name       = "check_with_group",
      check_label      = "Check With Group",
      check_group      = "mygroup",
      variables        = c("var1"),
      statistical_test = "missing_prop",
      thresholds = list(
        list(threshold_expression = "test_statistic < 5", penalty_score = 0)
      ),
      test_params = list()
    )
  )

  expect_true(quality_validate_schema_to_table(qc_schema))
})

test_that("quality_validate_schema_to_table errors on invalid check_group type", {

  qc_schema <- list(
    bad_group = list(
      check_name       = "bad_group",
      check_label      = "Bad Group",
      check_group      = c("group1", "group2"),  # invalid: vector
      variables        = c("var1"),
      statistical_test = "missing_prop",
      thresholds = list(
        list(threshold_expression = "test_statistic < 5", penalty_score = 0)
      ),
      test_params = list()
    )
  )

  expect_error(
    quality_validate_schema_to_table(qc_schema),
    regexp = "check_group.*must be a single character string"
  )
})

test_that("check_group round-trips through schema_to_table and table_to_schema", {

  original_schema <- list(
    fcs_mean = list(
      check_name       = "fcs_mean",
      check_label      = "FCS Mean Check",
      check_group      = "fcs",
      variables        = c("fcs_score"),
      statistical_test = "ttest",
      thresholds = list(
        list(threshold_expression = "p_value >= 0.05", penalty_score = 0),
        list(threshold_expression = "p_value < 0.05", penalty_score = 10)
      ),
      test_params = list(mu = 45)
    )
  )

  tab <- quality_schema_to_table(original_schema)
  reconstructed <- quality_table_to_schema(tab)

  expect_equal(reconstructed$checks$fcs_mean$check_group, "fcs")
})
