test_that("Initialization works", {
  obj <- QuantDataAnalysis$new()
  expect_true(is.list(obj$results))
  expect_true(tibble::is_tibble(obj$analysis_plan_issue_log))
  expect_s3_class(obj$data_analysis_plan, "QuantDataAnalysisPlanLog")
  expect_true(tibble::is_tibble(obj$data_analysis_plan$log_df))
  expect_null(obj$survey_design)

})

test_that("add_indicator adds rows correctly", {
  obj <- QuantDataAnalysis$new()

  obj$add_indicator_dap(
    indicator_name = "Test",
    calculation = "prop",
    var_name = "x"
  )

  print(obj$data_analysis_plan$log_df)

  expect_equal(nrow(obj$data_analysis_plan$log_df), 1)
  expect_equal(obj$data_analysis_plan$log_df$indicator_name, "Test")
})

test_that("remove_indicator removes the correct row", {
  obj <- QuantDataAnalysis$new()

  obj$add_indicator_dap("A", "prop", "x")
  obj$add_indicator_dap("B", "mean", "y")
  obj$remove_indicator_dap("A")

  expect_equal(nrow(obj$data_analysis_plan$log_df), 1)
  expect_equal(obj$data_analysis_plan$log_df$indicator_name, "B")
})

test_that("to_list_schema converts correctly", {
  obj <- QuantDataAnalysis$new()
  schema <- tibble::tibble(
    indicator_name = c("A","B"),
    calculation = c("prop","mean"),
    var_name = c("x","y"),
    denom_var = NA,
    disaggregation = NA,
    multiplier = 100,
    indicator_unit = "%"
  )
  obj$analysis_schema <- schema

  lst <- obj$to_list_schema()
  expect_equal(names(lst), c("A","B"))
  expect_equal(lst$A$indicator_name, "A")
})

test_that("generate_dap_from_schema filters missing vars", {
  obj <- QuantDataAnalysis$new()

  obj$analysis_schema <- tibble::tibble(
    indicator_name = c("A","B"),
    calculation = c("prop","ratio"),
    var_name = c("x","y"),
    denom_var = c(NA,"z"),
    disaggregation = c(NA, NA),
    multiplier = c(1,1),
    indicator_unit = c("%","%")
  )

  obj$survey_design <- list(
    variables = list(x = NULL, z = NULL)
  )

  obj$generate_dap_from_schema()
  expect_equal(nrow(obj$data_analysis_plan$log_df), 1)   # only "A" valid
  expect_equal(obj$data_analysis_plan$log_df$indicator_name, "A")
})

test_that("validate_plan catches invalid calculation", {
  obj <- QuantDataAnalysis$new()

  obj$data_analysis_plan$log_df <- tibble::tibble(
    indicator_name = "A",
    calculation = "weird",
    var_name = "x",
    denom_var = NA_character_,
    disaggregation = NA_character_,
    multiplier = 100,
    indicator_unit = "%"
  )

  obj$validate_plan()

  issues <- obj$analysis_plan_issue_log

  # Expect at least one invalid calculation issue
  expect_true(any(grepl("Invalid calculation", issues$issue)))

  # Check that the invalid calculation is associated with indicator A
  expect_true(
    any(issues$indicator_name == "A" &
          grepl("Invalid calculation", issues$issue))
  )
})

test_that("run_analysis runs with mocked calc and stores dual results", {
  skip_if_not(requireNamespace("pkgload", quietly = TRUE), "pkgload not available")
  skip_if(
    tryCatch(
      { pkgload::dev_packages(); FALSE },  # if it works, don't skip
      error = function(e) TRUE             # if it errors (not exported), skip
    ),
    "pkgload::dev_packages not available"
  )
  skip_if(
    tryCatch(
      length(pkgload::dev_packages()) == 0,
      error = function(e) TRUE
    ),
    "no dev package loaded by pkgload"
  )
  
  obj <- QuantDataAnalysis$new()

  obj$survey_design <- list(variables = list(x = NULL))
  # data is NULL so the base design branch won't run
  obj$data_analysis_plan$log_df <- tibble::tibble(
    indicator_name = "A",
    calculation = "mean",
    var_name = "x",
    denom_var = NA_character_,
    disaggregation = NA_character_,
    multiplier = 100,
    indicator_unit = "%"
  )

  local_mocked_bindings(
    phr_calc_survey_from_plan = function(design, analysis_plan, ...) {
      tibble::tibble(indicator_name = "A", est = 3.14)
    }
  )

  obj$run_analysis()

  # results should now be a list with survey_design and base slots
  expect_true(is.list(obj$results))
  expect_true("survey_design" %in% names(obj$results))
  expect_equal(obj$results$survey_design$est, 3.14)
})

test_that("export_results creates files", {
  obj <- QuantDataAnalysis$new()
  obj$results <- list(
    survey_design = tibble::tibble(a = 1),
    base          = tibble::tibble(a = 2)
  )

  tmp_xlsx <- tempfile(fileext = ".xlsx")
  obj$export_results(tmp_xlsx)
  expect_true(file.exists(tmp_xlsx))

  tmp_csv <- tempfile(fileext = ".csv")
  obj$export_results(tmp_csv, format = "csv")
  expect_true(file.exists(tmp_csv))
})


# ============================================================================
# Outputs Schema Tests for QuantDataAnalysis
# ============================================================================

test_that("QuantDataAnalysis initializes with outputs_schema, visualizations, and tables fields", {
  obj <- QuantDataAnalysis$new()
  
  # Check that new fields exist
  expect_true(is.list(obj$outputs_schema))
  expect_true(is.list(obj$visualizations))
  expect_true(is.list(obj$tables))
  
  # They should be initialized as empty lists
  expect_length(obj$visualizations, 0)
  expect_length(obj$tables, 0)
})

test_that("QuantDataAnalysis default_outputs_schema loads template", {
  obj <- QuantDataAnalysis$new()
  
  # outputs_schema should be a list (possibly empty if template doesn't exist)
  expect_true(is.list(obj$outputs_schema))
})

test_that("QuantDataAnalysis set_outputs_schema removed; direct assignment of outputs_schema works", {
  obj <- QuantDataAnalysis$new()
  # set_outputs_schema has been removed from the class
  expect_false("set_outputs_schema" %in% names(obj))
  # Direct assignment is the new pattern
  custom_schema <- list(
    plot1 = list(
      output_name = "plot1",
      output_type = "visualization",
      output_func_name = "plot_histogram",
      test_params = list(bins = 30)
    )
  )
  obj$outputs_schema <- custom_schema
  expect_equal(obj$outputs_schema$plot1$output_name, "plot1")
  expect_equal(obj$outputs_schema$plot1$output_type, "visualization")
})

test_that("QuantDataAnalysis outputs_schema_to_table converts schema to table via export_outputs_schema", {
  obj <- QuantDataAnalysis$new()
  
  obj$outputs_schema <- list(
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
  
  tbl <- obj$export_outputs_schema()
  
  expect_s3_class(tbl, "data.frame")
  expect_true("output_name" %in% names(tbl))
  expect_true("output_type" %in% names(tbl))
  expect_equal(nrow(tbl), 1)
  expect_equal(tbl$output_name[1], "plot1")
  expect_equal(tbl$output_type[1], "visualization")
})

test_that("QuantDataAnalysis import_outputs_schema converts table to schema", {
  obj <- QuantDataAnalysis$new()
  
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
  
  obj$import_outputs_schema(outputs_table)
  
  expect_equal(obj$outputs_schema$plot1$output_name, "plot1")
  expect_equal(obj$outputs_schema$plot1$output_type, "visualization")
  expect_equal(length(obj$outputs_schema$plot1$variables), 2)
  expect_equal(obj$outputs_schema$table1$output_name, "table1")
  expect_equal(obj$outputs_schema$table1$output_type, "table")
})

test_that("QuantDataAnalysis export_state_object includes outputs fields", {
  obj <- QuantDataAnalysis$new()
  
  # Add some outputs
  obj$visualizations <- list(plot1 = "test_plot")
  obj$tables <- list(table1 = data.frame(x = 1:5))
  
  state <- obj$export_state_object()
  
  expect_true("outputs_schema" %in% names(state))
  expect_true("visualizations" %in% names(state))
  expect_true("tables" %in% names(state))
  expect_equal(state$visualizations$plot1, "test_plot")
  expect_equal(nrow(state$tables$table1), 5)
})

test_that("QuantDataAnalysis load_state_object restores outputs fields", {
  obj <- QuantDataAnalysis$new()
  
  # Create a state object with outputs (results is now a list)
  state <- list(
    data_analysis_plan = obj$data_analysis_plan,
    results = list(survey_design = tibble::tibble(), base = tibble::tibble()),
    analysis_plan_issue_log = tibble::tibble(),
    analysis_schema = tibble::tibble(),
    outputs_schema = list(plot1 = list(output_name = "plot1")),
    visualizations = list(plot1 = "test_plot"),
    tables = list(table1 = data.frame(x = 1:5))
  )
  
  obj$load_state_object(state)
  
  expect_equal(obj$visualizations$plot1, "test_plot")
  expect_equal(nrow(obj$tables$table1), 5)
  expect_equal(obj$outputs_schema$plot1$output_name, "plot1")
})

# ============================================================================
# Analysis Schema Import / Export Tests
# ============================================================================

test_that("export_analysis_schema writes an xlsx file", {
  obj <- QuantDataAnalysis$new()
  obj$analysis_schema <- tibble::tibble(
    indicator_name = "Test",
    calculation    = "prop",
    var_name       = "x",
    denom_var      = NA_character_,
    disaggregation = NA_character_,
    multiplier     = 100,
    indicator_unit = "%"
  )

  tmp <- tempfile(fileext = ".xlsx")
  obj$export_analysis_schema(tmp)
  expect_true(file.exists(tmp))
})

test_that("export_analysis_schema writes a csv file", {
  obj <- QuantDataAnalysis$new()
  obj$analysis_schema <- tibble::tibble(
    indicator_name = "Test",
    calculation    = "prop",
    var_name       = "x",
    denom_var      = NA_character_,
    disaggregation = NA_character_,
    multiplier     = 100,
    indicator_unit = "%"
  )

  tmp <- tempfile(fileext = ".csv")
  obj$export_analysis_schema(tmp, format = "csv")
  expect_true(file.exists(tmp))
})

test_that("import_analysis_schema reads back a csv file", {
  tmp <- tempfile(fileext = ".csv")
  schema <- tibble::tibble(
    indicator_name = c("A", "B"),
    calculation    = c("prop", "mean"),
    var_name       = c("x", "y"),
    denom_var      = NA_character_,
    disaggregation = NA_character_,
    multiplier     = 100,
    indicator_unit = "%"
  )
  readr::write_csv(schema, tmp)

  obj <- QuantDataAnalysis$new()
  obj$import_analysis_schema(tmp)

  expect_equal(nrow(obj$analysis_schema), 2)
  expect_equal(obj$analysis_schema$indicator_name, c("A", "B"))
})

test_that("import_analysis_schema warns on missing columns", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(indicator_name = "A"), tmp)

  obj <- QuantDataAnalysis$new()
  # phr_warning() issues an R warning when columns are missing
  suppressWarnings(obj$import_analysis_schema(tmp))

  # The schema should still be set even with missing columns
  expect_equal(nrow(obj$analysis_schema), 1)
})


test_that("QuantDataAnalysis has data field (not data_stage)", {
  obj <- QuantDataAnalysis$new()
  expect_true("data" %in% names(obj))
  expect_false("data_stage" %in% names(obj))
  expect_null(obj$data)
})

test_that("QuantDataAnalysis initialize stores data in data field", {
  df <- data.frame(x = 1:5)
  obj <- QuantDataAnalysis$new(data = df)
  expect_identical(obj$data, df)
  expect_equal(obj$data_stage_name, NULL)
})

test_that("QuantDataAnalysis initialize stores data_stage_name", {
  df <- data.frame(x = 1:5)
  obj <- QuantDataAnalysis$new(data = df, data_stage_name = "standardized")
  expect_equal(obj$data_stage_name, "standardized")
})

test_that("QuantDataAnalysis initialize no longer accepts design parameter", {
  # The initialize method should not have a design parameter
  # It now only takes 'data' as the dataset argument
  obj <- QuantDataAnalysis$new()
  expect_null(obj$survey_design)
  expect_null(obj$data)
})

test_that("QuantDataAnalysisGeneral can be instantiated", {
  obj <- QuantDataAnalysisGeneral$new()
  expect_s3_class(obj, "QuantDataAnalysisGeneral")
  expect_s3_class(obj, "QuantDataAnalysis")
  expect_null(obj$data)
  expect_null(obj$survey_design)
})

# ============================================================================
# create_survey_design tests
# ============================================================================

test_that("create_survey_design returns NULL when data is NULL", {
  obj <- QuantDataAnalysis$new()
  expect_null(obj$survey_design)
})

test_that("create_survey_design works with cluster_id_numeric in variable_map and data", {
  skip_if_not(requireNamespace("srvyr", quietly = TRUE), "srvyr not available")

  df <- data.frame(
    cluster_id_numeric = c(1L, 1L, 2L, 2L, 3L),
    weight_col         = c(1.0, 1.0, 1.5, 1.5, 2.0),
    x                  = c(10, 20, 30, 40, 50)
  )
  vmap <- list(
    cluster_id_numeric = "cluster_id_numeric",
    weight             = "weight_col"
  )

  obj <- QuantDataAnalysis$new(
    data         = df,
    variable_map = vmap
  )

  expect_false(is.null(obj$survey_design))
})

test_that("create_survey_design falls back to ids=1 when no cluster column is in data", {
  skip_if_not(requireNamespace("srvyr", quietly = TRUE), "srvyr not available")

  df <- data.frame(
    weight_col = c(1.0, 1.5, 2.0),
    x          = c(10, 20, 30)
  )
  # variable_map claims cluster_id_numeric but column is absent from data
  vmap <- list(
    cluster_id_numeric = "cluster_id_numeric",
    weight             = "weight_col"
  )

  obj <- QuantDataAnalysis$new(
    data         = df,
    variable_map = vmap
  )

  # Should not error; survey_design is created with ids=1
  expect_false(is.null(obj$survey_design))
})

test_that("create_survey_design works with no cluster or weight columns at all", {
  skip_if_not(requireNamespace("srvyr", quietly = TRUE), "srvyr not available")

  df <- data.frame(x = 1:5)
  obj <- QuantDataAnalysis$new(data = df, variable_map = list())

  expect_false(is.null(obj$survey_design))
})

# ============================================================================
# Dual results from run_analysis
# ============================================================================

test_that("run_analysis produces survey_design and base result sets", {
  skip_if_not(requireNamespace("srvyr", quietly = TRUE), "srvyr not available")

  df <- data.frame(
    x  = c(1, 0, 1, 1, 0),
    wt = c(1, 1, 2, 1, 1)
  )

  obj <- QuantDataAnalysis$new(
    data         = df,
    variable_map = list(weight = "wt"),
    dap = tibble::tibble(
      indicator_name = "PropX",
      calculation    = "prop",
      var_name       = "x",
      denom_var      = NA_character_,
      disaggregation = NA_character_,
      multiplier     = 100,
      indicator_unit = "%"
    )
  )

  obj$run_analysis()

  expect_true(is.list(obj$results))
  expect_true("survey_design" %in% names(obj$results))
  expect_true("base" %in% names(obj$results))

  # Both should be tibbles
  expect_s3_class(obj$results$survey_design, "tbl_df")
  expect_s3_class(obj$results$base, "tbl_df")

  # plan_row should be the first column in both
  expect_equal(names(obj$results$survey_design)[1], "plan_row")
  expect_equal(names(obj$results$base)[1], "plan_row")
})

test_that("get_results returns the dual-results list", {
  skip_if_not(requireNamespace("srvyr", quietly = TRUE), "srvyr not available")

  df <- data.frame(x = c(1, 0, 1), wt = c(1, 1, 1))
  obj <- QuantDataAnalysis$new(
    data         = df,
    variable_map = list(weight = "wt"),
    dap = tibble::tibble(
      indicator_name = "P", calculation = "prop", var_name = "x",
      denom_var = NA_character_, disaggregation = NA_character_,
      multiplier = 100, indicator_unit = "%"
    )
  )
  obj$run_analysis()

  res <- obj$get_results()
  expect_true(is.list(res))
  expect_true("survey_design" %in% names(res))
  expect_true("base" %in% names(res))
})
