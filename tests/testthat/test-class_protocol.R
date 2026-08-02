library(testthat)

make_valid_protocol_objective_schema <- function(
    short_objective = "Short objective",
    indicator_code = "IND1"
) {
  data.frame(
    sector = "Health",
    pillar = "Pillar 1",
    sub_pillar = "Sub-pillar 1",
    text_objective = "Objective text",
    short_objective = short_objective,
    objective_code = "OBJ1",
    indicator_code = indicator_code,
    stringsAsFactors = FALSE
  )
}

ana_protocol_resources_available <- function() {
  objectives_path <- system.file(
    "resources", "reference_objectives.xlsx", package = "phr"
  )
  indicators_path <- system.file(
    "resources", "reference_indicator_bank.xlsx", package = "phr"
  )
  objectives_available <-
    (nzchar(objectives_path) && file.exists(objectives_path)) ||
    file.exists(file.path("inst", "resources", "reference_objectives.xlsx"))
  indicators_available <-
    (nzchar(indicators_path) && file.exists(indicators_path)) ||
    file.exists(file.path("inst", "resources", "reference_indicator_bank.xlsx"))
  objectives_available && indicators_available
}

# ── Inheritance ────────────────────────────────────────────────────────────────

test_that("Protocol inherits Document and Orchestrator", {
  p <- Protocol$new()
  expect_true(inherits(p, "Protocol"))
  expect_true(inherits(p, "Document"))
  expect_true(inherits(p, "Orchestrator"))
})

test_that("Protocol has Orchestrator metadata timestamps", {
  p <- Protocol$new()
  expect_s3_class(p$metadata$created_datetime, "POSIXct")
  expect_s3_class(p$metadata$modified_datetime, "POSIXct")
})

test_that("Protocol inherits Document .r_version active binding", {
  p <- Protocol$new()
  expect_type(p$.r_version, "character")
  expect_true(nzchar(p$.r_version))
})

# ── Initialization ─────────────────────────────────────────────────────────────

test_that("Protocol$new() with no args sets NULL metadata fields", {
  p <- Protocol$new()
  expect_null(p$metadata$assessment_title)
  expect_null(p$metadata$country_name)
  expect_null(p$metadata$month_year)
  expect_equal(p$metadata$protocol_version, "1.0")
  expect_equal(p$tools, list())
  expect_equal(p$issues, list())
})

test_that("Protocol stores assessment_title, country_name, month_year from constructor", {
  p <- Protocol$new(
    assessment_title = "Test Survey",
    country_name     = "Kenya",
    month_year       = "January 2024"
  )
  expect_equal(p$metadata$assessment_title, "Test Survey")
  expect_equal(p$metadata$country_name, "Kenya")
  expect_equal(p$metadata$month_year, "January 2024")
})

test_that("framework_type = 'none' creates a Framework (not ANAFramework)", {
  p <- Protocol$new(framework_type = "none")
  expect_true(inherits(p$framework, "Framework"))
  expect_false(inherits(p$framework, "ANAFramework"))
})

test_that("framework_type = 'ana' creates ANAFramework when resources available", {
  skip_if_not(ana_protocol_resources_available(),
              "ANA framework resources not available")
  p <- Protocol$new(framework_type = "ana")
  expect_true(inherits(p$framework, "ANAFramework"))
})

test_that("framework_type invalid value errors", {
  expect_error(Protocol$new(framework_type = "invalid"))
})

# ── Nested objects: light accessibility ────────────────────────────────────────

test_that("framework field is accessible and is a Framework", {
  p <- Protocol$new()
  expect_true(inherits(p$framework, "Framework"))
  # Light: just confirm it is accessible, not testing Framework internals
  expect_true(is.data.frame(p$framework$modified_objectives_schema) ||
                is.null(p$framework$modified_objectives_schema))
})

test_that("tools field initializes as empty list and is accessible", {
  p <- Protocol$new()
  expect_equal(length(p$tools), 0L)
  expect_type(p$tools, "list")
})

test_that("add_tools populates $tools with the right named entry", {
  p <- Protocol$new()
  p$add_tools(tool_type = "household")
  # Light: confirm the tool is accessible; don't test Tool internals
  expect_true("household" %in% names(p$tools))
  expect_true(is.environment(p$tools$household))
})

# ── Tool management ────────────────────────────────────────────────────────────

test_that("get_tool_names returns character(0) before any tools are added", {
  p <- Protocol$new()
  expect_equal(p$get_tool_names(), character(0))
})

test_that("add_tools + get_tool_names + is_tool_included work together", {
  p <- Protocol$new()
  expect_false(p$is_tool_included("household"))
  p$add_tools(tool_type = "household")
  expect_true(p$is_tool_included("household"))
  expect_equal(p$get_tool_names(), "household")
})

test_that("add_tools supports all valid tool types", {
  p <- Protocol$new()
  p$add_tools(tool_type = "household")
  p$add_tools(tool_type = "key_informant")
  p$add_tools(tool_type = "observation")
  expect_equal(sort(p$get_tool_names()), sort(c("household", "key_informant", "observation")))
})

test_that("add_tools auto-names second tool of same type with suffix", {
  p <- Protocol$new()
  p$add_tools(tool_type = "household")
  p$add_tools(tool_type = "household")
  expect_true("household"   %in% p$get_tool_names())
  expect_true("household_2" %in% p$get_tool_names())
})

test_that("add_tools uses custom tool_name when provided", {
  p <- Protocol$new()
  p$add_tools(tool_type = "household", tool_name = "my_tool")
  expect_true("my_tool" %in% p$get_tool_names())
})

test_that("add_tools rejects invalid tool_type", {
  p <- Protocol$new()
  expect_error(p$add_tools(tool_type = "invalid_type"))
})

test_that("get_issues returns a list", {
  p <- Protocol$new()
  expect_type(p$get_issues(), "list")
})

# ── validate_objective_schema ──────────────────────────────────────────────────

test_that("validate_objective_schema returns TRUE for a valid schema", {
  p <- Protocol$new()
  expect_true(p$validate_objective_schema(make_valid_protocol_objective_schema()))
})

test_that("validate_objective_schema errors on NULL input", {
  p <- Protocol$new()
  expect_error(p$validate_objective_schema(NULL))
})

test_that("validate_objective_schema errors on empty data frame", {
  p <- Protocol$new()
  empty_schema <- make_valid_protocol_objective_schema()[0, , drop = FALSE]
  expect_error(p$validate_objective_schema(empty_schema))
})

test_that("validate_objective_schema errors when required columns are missing", {
  p <- Protocol$new()
  schema <- make_valid_protocol_objective_schema()
  bad <- schema[, setdiff(names(schema), "indicator_code"), drop = FALSE]
  expect_error(p$validate_objective_schema(bad))
})

test_that("validate_objective_schema errors when all sector values are NA", {
  p <- Protocol$new()
  schema <- make_valid_protocol_objective_schema()
  schema$sector <- NA_character_
  expect_error(p$validate_objective_schema(schema))
})

test_that("validate_objective_schema soft=TRUE returns invisible(FALSE) instead of erroring", {
  p <- Protocol$new()
  empty_schema <- make_valid_protocol_objective_schema()[0, , drop = FALSE]
  result <- withVisible(p$validate_objective_schema(empty_schema, soft = TRUE))
  expect_false(result$visible)
  expect_false(result$value)
})

# ── diagnose_coherence ─────────────────────────────────────────────────────────

test_that("diagnose_coherence returns self invisibly", {
  p <- Protocol$new()
  result <- withVisible(p$diagnose_coherence())
  expect_false(result$visible)
  expect_identical(result$value, p)
})

test_that("diagnose_coherence populates issues_coherence when no tools cover objectives", {
  p <- Protocol$new()
  p$framework$modified_objectives_schema <- make_valid_protocol_objective_schema()
  p$diagnose_coherence()
  # No tools -> objectives have no indicator coverage
  expect_true("objectives_without_indicators" %in% names(p$issues_coherence))
})

test_that("diagnose_coherence clears objectives_without_indicators when tool has matching indicator", {
  p <- Protocol$new()
  p$framework$modified_objectives_schema <- make_valid_protocol_objective_schema(indicator_code = "IND1")
  p$add_tools(tool_type = "household")
  p$tools$household$revised_survey <- data.frame(
    indicator_code = "IND1", stringsAsFactors = FALSE
  )
  p$diagnose_coherence()
  expect_false("objectives_without_indicators" %in% names(p$issues_coherence))
})

# ── Active bindings ────────────────────────────────────────────────────────────

test_that(".release_date returns a Date", {
  p <- Protocol$new()
  expect_s3_class(p$.release_date, "Date")
})

test_that(".release_date is read-only (assignment returns invisible FALSE)", {
  p <- Protocol$new()
  result <- withVisible(p$.release_date <- as.Date("2024-01-01"))
  expect_false(result$visible)
  expect_false(result$value)
})

# ── get_quarto_params ─────────────────────────────────────────────────────────

test_that("get_quarto_params returns a named list with expected keys", {
  p <- Protocol$new(
    assessment_title = "My Protocol",
    country_name     = "Somalia",
    month_year       = "March 2025"
  )
  params <- p$get_quarto_params()
  expect_type(params, "list")
  expect_true("assessment_title" %in% names(params))
  expect_true("country_name"     %in% names(params))
  expect_true("month_year"       %in% names(params))
  expect_equal(params$assessment_title, "My Protocol")
  expect_equal(params$country_name, "Somalia")
  # r_version inherited from Document
  expect_true("r_version" %in% names(params))
  expect_identical(params$r_version, p$.r_version)
})

# ── Metadata fields ────────────────────────────────────────────────────────────

test_that("Protocol metadata fields can be set and read back", {
  p <- Protocol$new()
  p$metadata$assessment_title <- "Updated title"
  p$metadata$country_name     <- "Somalia"
  expect_equal(p$metadata$assessment_title, "Updated title")
  expect_equal(p$metadata$country_name, "Somalia")
})

# ── get_dap_table edge cases ───────────────────────────────────────────────────

test_that("get_dap_table errors on empty string", {
  p <- Protocol$new()
  expect_error(p$get_dap_table(""))
})

test_that("get_dap_table errors on non-character input", {
  p <- Protocol$new()
  expect_error(p$get_dap_table(123))
})

test_that("get_dap_table returns NULL when tool does not exist", {
  p <- Protocol$new()
  expect_null(p$get_dap_table("nonexistent_tool"))
})

test_that("access_nested on framework returns modified_objectives_schema", {
  p <- Protocol$new()
  p$framework$modified_objectives_schema <- make_valid_protocol_objective_schema()
  schema <- p$access_nested("framework", member = "modified_objectives_schema")
  expect_true(is.data.frame(schema))
  expect_true(nrow(schema) > 0)
})

