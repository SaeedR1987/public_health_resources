library(tibble)

# DATA_DIAGNOSE Testing ####

test_that("data_diagnose returns NULL when no variable schema is defined", {
  df <- tibble(id = 1:3, name = c("a", "b", "c"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  expect_warning(
    result <- d$data_diagnose(stage = "raw"),
    regexp = "No variable schema defined"
  )

  expect_null(result)
})

test_that("data_diagnose returns NULL when stage data is NULL", {
  df <- tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  # Set a schema but remove standardized data
  d$set(field = "..variable_schema", value = list(types = list(id = "numeric")))

  expect_warning(
    result <- d$data_diagnose(stage = "standardized"),
    regexp = "No data available"
  )

  expect_null(result)
})

# Helper: build a flat variable_schema data frame (the current storage
# format for private$..variable_schema) directly, one row per
# variable/canonical-value combination, for use in data_diagnose() tests.
.make_variable_schema_df <- function(rows) {
  do.call(rbind, lapply(rows, function(r) {
    data.frame(
      rule_type = "variable",
      variable = r$variable,
      value = if (is.null(r$value)) NA_character_ else r$value,
      required = isTRUE(r$required),
      type = if (is.null(r$type)) NA_character_ else r$type,
      question_type = if (is.null(r$question_type)) NA_character_ else r$question_type,
      allowed = if (is.null(r$allowed)) NA_character_ else r$allowed,
      col_names = if (is.null(r$col_names)) r$variable else r$col_names,
      unique = isTRUE(r$unique),
      comment = if (is.null(r$comment)) NA_character_ else r$comment,
      stringsAsFactors = FALSE
    )
  }))
}

test_that("data_diagnose generates basic diagnostic table with types only", {
  df <- tibble(id = 1:3, age = c("20", "25", "30"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- .make_variable_schema_df(list(
    list(variable = "id", type = "numeric"),
    list(variable = "age", type = "numeric")
  ))

  d$set(field = "..variable_schema", value = schema)
  d$set(field = "..variable_map", value = list(id = "id", age = "age"))

  suppressWarnings(suppressMessages(result <- d$data_diagnose(stage = "raw")))

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 2)
  expect_true(all(
    c(
      "required_variable",
      "required_type",
      "mapped_variable",
      "safely_coercible",
      "issues"
    ) %in%
      names(result)
  ))

  # Check that id and age are in the diagnostic table
  expect_true("id" %in% result$required_variable)
  expect_true("age" %in% result$required_variable)

  # Both should be safely coercible to numeric
  id_row <- result[result$required_variable == "id", ]
  expect_true(id_row$safely_coercible)
  expect_equal(id_row$issues, "ok")
})

test_that("data_diagnose detects unmapped variables", {
  df <- tibble(id = 1:3, age = 20:22)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- .make_variable_schema_df(list(
    list(variable = "id", type = "numeric"),
    list(variable = "age", type = "numeric")
  ))

  d$set(field = "..variable_schema", value = schema)
  d$set(field = "..variable_map", value = list(id = "id")) # age not mapped

  suppressWarnings(suppressMessages(result <- d$data_diagnose(stage = "raw")))

  age_row <- result[result$required_variable == "age", ]
  expect_true(grepl("variable not mapped", age_row$issues))
})

test_that("data_diagnose detects mapped variables not in dataset", {
  df <- tibble(id = 1:3)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- .make_variable_schema_df(list(
    list(variable = "id", type = "numeric"),
    list(variable = "age", type = "numeric")
  ))

  d$set(field = "..variable_schema", value = schema)
  d$set(field = "..variable_map", value = list(id = "id", age = "age_col")) # age_col doesn't exist

  suppressWarnings(suppressMessages(result <- d$data_diagnose(stage = "raw")))

  age_row <- result[result$required_variable == "age", ]
  expect_true(grepl("mapped variable not in dataset", age_row$issues))
})

test_that("data_diagnose detects type coercion issues", {
  df <- tibble(id = 1:3, status = c("active", "inactive", "pending"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- .make_variable_schema_df(list(
    list(variable = "id", type = "numeric"),
    list(variable = "status", type = "numeric") # Can't coerce text to numeric
  ))

  d$set(field = "..variable_schema", value = schema)
  d$set(field = "..variable_map", value = list(id = "id", status = "status"))

  suppressWarnings(suppressMessages(result <- d$data_diagnose(stage = "raw")))

  status_row <- result[result$required_variable == "status", ]
  expect_false(status_row$safely_coercible)
  expect_true(grepl("not safely coercible", status_row$issues))
})

test_that("data_diagnose handles value maps with nested format", {
  df <- tibble(id = 1:3, status = c("A", "I", "A"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- .make_variable_schema_df(list(
    list(variable = "id", type = "numeric"),
    list(variable = "status", value = "active", type = "character", allowed = "A, active, yes"),
    list(variable = "status", value = "inactive", type = "character", allowed = "I, inactive, no")
  ))

  d$set(field = "..variable_schema", value = schema)
  d$set(field = "..variable_map", value = list(id = "id", status = "status"))
  d$set(field = "..value_map", value = list(status = list(active = c("A"), inactive = c("I"))))

  suppressWarnings(suppressMessages(result <- d$data_diagnose(stage = "raw")))

  # Should have rows for each value mapping
  status_rows <- result[result$required_variable == "status", ]
  expect_true(nrow(status_rows) >= 2)
  expect_true("active" %in% status_rows$required_value)
  expect_true("inactive" %in% status_rows$required_value)
})

test_that("data_diagnose follows schema auto-mapping for select_multiple values", {
  d <- Data$new(
    data.frame(id = 1:3, skills = c("reading writing", "math reading", NA)),
    uuid = "id"
  )
  d$set(field = "..variable_schema", value = .make_variable_schema_df(list(
    list(
      variable = "skills", value = "literacy", type = "character",
      question_type = "select_multiple", allowed = "reading, writing"
    ),
    list(
      variable = "skills", value = "numeracy", type = "character",
      question_type = "select_multiple", allowed = "math, mathematics"
    )
  )))
  d$call(field = "..map_schema_vars", stage = "raw")

  result <- d$data_diagnose(stage = "raw")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expect_setequal(result$required_value, c("literacy", "numeracy"))
  expect_true(all(result$mapped_variable == "skills"))
  expect_true(all(result$issues == "ok"))
  expect_identical(
    result$mapped_value[result$required_value == "numeracy"],
    "math"
  )
  expect_identical(d$get(field = "..data_diagnostics"), result)
})

test_that("data_diagnose detects unmapped values", {
  df <- tibble(id = 1:3, status = c("A", "I", "A"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- .make_variable_schema_df(list(
    list(variable = "status", value = "active", type = "character", allowed = "A"),
    list(variable = "status", value = "inactive", type = "character", allowed = "I")
  ))

  d$set(field = "..variable_schema", value = schema)
  d$set(field = "..variable_map", value = list(id = "id", status = "status"))
  # Don't set value_map - values are not mapped

  suppressWarnings(suppressMessages(result <- d$data_diagnose(stage = "raw")))

  status_rows <- result[result$required_variable == "status", ]
  # Should have issues about unmapped values
  expect_true(any(grepl("value not mapped", status_rows$issues)))
})

test_that("data_diagnose detects mapped values not in dataset", {
  df <- tibble(id = 1:3, status = c("A", "A", "A"))
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- .make_variable_schema_df(list(
    list(variable = "status", value = "active", type = "character", allowed = "A"),
    list(variable = "status", value = "inactive", type = "character", allowed = "I, X") # X not in data
  ))

  d$set(field = "..variable_schema", value = schema)
  d$set(field = "..variable_map", value = list(id = "id", status = "status"))
  d$set(field = "..value_map", value = list(status = list(active = c("A"), inactive = c("I", "X"))))

  suppressWarnings(suppressMessages(result <- d$data_diagnose(stage = "raw")))

  status_rows <- result[result$required_variable == "status", ]
  inactive_row <- status_rows[status_rows$required_value == "inactive", ]
  expect_true(grepl("mapped values not in dataset", inactive_row$issues))
})

test_that("data_diagnose stores result in data_diagnostics field", {
  df <- tibble(id = 1:3, age = 20:22)
  d <- suppressMessages(
    Data$new(data = df, uuid = "id")
  )

  schema <- .make_variable_schema_df(list(
    list(variable = "id", type = "numeric"),
    list(variable = "age", type = "numeric")
  ))

  d$set(field = "..variable_schema", value = schema)
  d$set(field = "..variable_map", value = list(id = "id", age = "age"))

  suppressWarnings(suppressMessages(result <- d$data_diagnose(stage = "raw")))

  expect_s3_class(result, "data.frame")
  expect_named(result, c(
    "required_variable", "required_value", "required_type", "mapped_variable",
    "mapped_value", "safely_coercible", "comment", "issues"
  ))
  expect_identical(d$get(field = "..data_diagnostics"), result)
  expect_s3_class(d$get(field = "..data_diagnostics"), "data.frame")
  expect_equal(nrow(result), 2L)
  expect_true(all(result$issues == "ok"))
  expect_match(
    d$get(field = "..change_log")$log_message[[1]],
    "Generated diagnostic table with 2 row\\(s\\)"
  )
})

test_that("data_diagnose private row builder resolves its own metadata helper", {
  d <- Data$new(data.frame(id = 1:2), uuid = "id")
  schema <- .make_variable_schema_df(list(
    list(variable = "id", type = "", comment = ""),
    list(variable = "id", type = "numeric", comment = "Identifier")
  ))

  rows <- d$call(
    field = "..data_diagnose_build_table",
    var_role = "id",
    sch_vars = schema,
    vm = list(id = "id"),
    vmap = list(),
    df = d$get_data("raw")
  )

  expect_length(rows, 1L)
  expect_identical(rows[[1]]$required_type, "numeric")
  expect_identical(rows[[1]]$comment, "Identifier")
  expect_identical(rows[[1]]$issues, "ok")
})

test_that("data_diagnose returns and stores diagnostics for each selected stage", {
  d <- Data$new(data.frame(id = 1:2, age = c("20", "invalid")), uuid = "id")
  d$set(field = "..variable_schema", value = .make_variable_schema_df(list(
    list(variable = "age", type = "numeric")
  )))
  d$set(field = "..variable_map", value = list(age = "age"))
  d$set(field = "..standardized_data", value = data.frame(id = 1:2, age = 20:21))
  d$set(field = "..clean_data", value = data.frame(id = 1:2, age = 22:23))

  for (stage in c("raw", "standardized", "clean")) {
    result <- d$data_diagnose(stage = stage)

    expect_s3_class(result, "data.frame")
    expect_identical(d$get(field = "..data_diagnostics"), result)
    expect_identical(result$safely_coercible, stage != "raw")
    expect_identical(
      result$issues,
      if (stage == "raw") "not safely coercible to required type" else "ok"
    )
  }
})

test_that("data_diagnose returns a typed empty table for blank schema variables", {
  d <- Data$new(data.frame(id = 1:2), uuid = "id")
  d$set(field = "..variable_schema", value = .make_variable_schema_df(list(
    list(variable = "", type = "numeric"),
    list(variable = NA_character_, type = "numeric")
  )))

  result <- d$data_diagnose()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_equal(ncol(result), 8L)
  expect_type(result$safely_coercible, "logical")
  expect_true(all(vapply(result[-6], is.character, logical(1))))
  expect_identical(d$get(field = "..data_diagnostics"), result)
})

test_that("data_diagnose leaves previous diagnostics unchanged when skipped", {
  d <- Data$new(data.frame(id = 1:2), uuid = "id")
  d$set(field = "..variable_schema", value = .make_variable_schema_df(list(
    list(variable = "id", type = "numeric")
  )))
  d$set(field = "..variable_map", value = list(id = "id"))
  previous <- d$data_diagnose()

  expect_warning(
    result <- d$data_diagnose(stage = "standardized"),
    "No data available"
  )
  expect_null(result)
  expect_identical(d$get(field = "..data_diagnostics"), previous)

  for (schema in list(NULL, list(), data.frame(), data.frame(rule_type = "dependency"))) {
    d$set(field = "..variable_schema", value = schema)
    expect_warning(result <- d$data_diagnose(), "No variable schema defined")
    expect_null(result)
    expect_identical(d$get(field = "..data_diagnostics"), previous)
  }
})

test_that("data_diagnose preserves diagnostics when building rows fails", {
  d <- Data$new(data.frame(id = 1:2), uuid = "id")
  d$set(field = "..variable_schema", value = .make_variable_schema_df(list(
    list(variable = "id", type = "numeric")
  )))
  previous <- d$data_diagnose()
  d$set(field = "..variable_map", value = list(id = c("id", "invalid")))

  result <- d$data_diagnose()

  expect_true(phrutils::phr_failed(result))
  expect_match(result$step, "Build diagnostic rows")
  expect_identical(d$get(field = "..data_diagnostics"), previous)
})
