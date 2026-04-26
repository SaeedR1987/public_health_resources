
library(testthat)

# =========================================================
# Tests for R/utils_tools_validation.R
# =========================================================


# ---- 1. xlsform_extract_variables -----------------------

test_that("xlsform_extract_variables extracts single variable", {
  expect_equal(xlsform_extract_variables("${age}"), "age")
})

test_that("xlsform_extract_variables extracts multiple variables", {
  result <- xlsform_extract_variables("${age} > 18 and ${consent} = 'yes'")
  expect_equal(result, c("age", "consent"))
})

test_that("xlsform_extract_variables returns character(0) when no matches", {
  expect_equal(xlsform_extract_variables("no variables here"), character(0))
})

test_that("xlsform_extract_variables returns character(0) for NA input", {
  expect_equal(xlsform_extract_variables(NA_character_), character(0))
})

test_that("xlsform_extract_variables returns character(0) for NULL input", {
  expect_equal(xlsform_extract_variables(NULL), character(0))
})

test_that("xlsform_extract_variables returns character(0) for non-character input", {
  expect_equal(xlsform_extract_variables(42), character(0))
})

test_that("xlsform_extract_variables handles variables with underscores and digits", {
  result <- xlsform_extract_variables("${hh_size_01} + ${num_children_05}")
  expect_equal(result, c("hh_size_01", "num_children_05"))
})


# ---- 2. xlsform_collect_variables -----------------------

test_that("xlsform_collect_variables returns unique variables across rows", {
  df <- data.frame(
    relevant = c(
      "${age} > 5",
      "${consent} = 'yes' and ${age} > 0",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
  result <- xlsform_collect_variables(df, "relevant")
  expect_setequal(result, c("age", "consent"))
})

test_that("xlsform_collect_variables returns all occurrences when only_unique = FALSE", {
  df <- data.frame(
    relevant = c("${age} > 5", "${age} < 99"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_collect_variables(df, "relevant", only_unique = FALSE)
  expect_equal(result, c("age", "age"))
})

test_that("xlsform_collect_variables returns character(0) when no references exist", {
  df <- data.frame(relevant = c("no vars", NA_character_), stringsAsFactors = FALSE)
  expect_equal(xlsform_collect_variables(df, "relevant"), character(0))
})

test_that("xlsform_collect_variables errors on missing column", {
  df <- data.frame(other = "value", stringsAsFactors = FALSE)
  expect_error(xlsform_collect_variables(df, "relevant"))
})

test_that("xlsform_collect_variables errors when df is not a data frame", {
  expect_error(xlsform_collect_variables(list(a = 1), "a"))
})


# ---- 3. xlsform_is_valid_varname ------------------------

test_that("xlsform_is_valid_varname returns TRUE for simple names", {
  expect_true(xlsform_is_valid_varname("age"))
  expect_true(xlsform_is_valid_varname("hh_size"))
  expect_true(xlsform_is_valid_varname("_private"))
  expect_true(xlsform_is_valid_varname("var123"))
})

test_that("xlsform_is_valid_varname returns FALSE for names with spaces", {
  expect_false(xlsform_is_valid_varname("my variable"))
})

test_that("xlsform_is_valid_varname returns FALSE for names with apostrophes", {
  expect_false(xlsform_is_valid_varname("it's"))
})

test_that("xlsform_is_valid_varname returns FALSE when starting with a digit", {
  expect_false(xlsform_is_valid_varname("123start"))
})

test_that("xlsform_is_valid_varname returns FALSE for names with hyphens", {
  expect_false(xlsform_is_valid_varname("my-var"))
})

test_that("xlsform_is_valid_varname returns FALSE for NA", {
  expect_false(xlsform_is_valid_varname(NA_character_))
})

test_that("xlsform_is_valid_varname returns FALSE for empty string", {
  expect_false(xlsform_is_valid_varname(""))
})

test_that("xlsform_is_valid_varname returns FALSE for non-character input", {
  expect_false(xlsform_is_valid_varname(123))
})


# ---- 4. xlsform_varname_in_survey -----------------------

test_that("xlsform_varname_in_survey returns TRUE when variable is declared", {
  declared <- c("age", "sex", "consent", "hh_size")
  expect_true(xlsform_varname_in_survey("age", declared))
})

test_that("xlsform_varname_in_survey returns FALSE when variable is not declared", {
  declared <- c("age", "sex")
  expect_false(xlsform_varname_in_survey("weight", declared))
})

test_that("xlsform_varname_in_survey returns FALSE and warns for NA varname", {
  expect_warning(
    result <- xlsform_varname_in_survey(NA_character_, c("age")),
    regexp = NULL
  )
  expect_false(result)
})

test_that("xlsform_varname_in_survey errors when name_vector is not character", {
  expect_error(xlsform_varname_in_survey("age", 1:3))
})


# ---- 5. xlsform_check_brackets --------------------------

test_that("xlsform_check_brackets returns TRUE for balanced brackets", {
  result <- xlsform_check_brackets("(a + b) * [c - d]")
  expect_true(result["parens_ok"])
  expect_true(result["brackets_ok"])
})

test_that("xlsform_check_brackets detects unmatched opening parenthesis", {
  result <- xlsform_check_brackets("if(a > 1, 'yes'")
  expect_false(result["parens_ok"])
  expect_true(result["brackets_ok"])
})

test_that("xlsform_check_brackets detects unmatched closing parenthesis", {
  result <- xlsform_check_brackets("a + b)")
  expect_false(result["parens_ok"])
})

test_that("xlsform_check_brackets detects unmatched opening square bracket", {
  result <- xlsform_check_brackets("selected(${q}, [1)")
  expect_false(result["brackets_ok"])
})

test_that("xlsform_check_brackets returns TRUE for NA cell", {
  result <- xlsform_check_brackets(NA_character_)
  expect_true(result["parens_ok"])
  expect_true(result["brackets_ok"])
})

test_that("xlsform_check_brackets returns TRUE for empty cell", {
  result <- xlsform_check_brackets("")
  expect_true(result["parens_ok"])
  expect_true(result["brackets_ok"])
})

test_that("xlsform_check_brackets handles nested parentheses", {
  result <- xlsform_check_brackets("((a + b) * (c + d))")
  expect_true(result["parens_ok"])
})


# ---- 6. xlsform_orphan_square_brackets ------------------

test_that("xlsform_orphan_square_brackets returns FALSE for valid ${ } syntax", {
  expect_false(xlsform_orphan_square_brackets("${age} > 5"))
})

test_that("xlsform_orphan_square_brackets returns TRUE for standalone bracket", {
  expect_true(xlsform_orphan_square_brackets("[1]"))
})

test_that("xlsform_orphan_square_brackets returns TRUE for bracket after non-$ char", {
  expect_true(xlsform_orphan_square_brackets("var[0]"))
})

test_that("xlsform_orphan_square_brackets returns FALSE for no brackets", {
  expect_false(xlsform_orphan_square_brackets("${age} > 5 and ${consent} = 'yes'"))
})

test_that("xlsform_orphan_square_brackets returns FALSE for NA", {
  expect_false(xlsform_orphan_square_brackets(NA_character_))
})

test_that("xlsform_orphan_square_brackets returns FALSE for empty string", {
  expect_false(xlsform_orphan_square_brackets(""))
})


# ---- 7. xlsform_check_group_repeats ---------------------

test_that("xlsform_check_group_repeats returns valid for well-formed survey", {
  survey <- data.frame(
    type = c("text", "begin_group", "text", "end_group",
             "begin_repeat", "text", "end_repeat"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_true(result$valid)
  expect_equal(length(result$issues), 0L)
})

test_that("xlsform_check_group_repeats detects unclosed begin_group", {
  survey <- data.frame(
    type = c("begin_group", "text"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_false(result$valid)
  expect_true(any(grepl("begin_group", result$issues)))
})

test_that("xlsform_check_group_repeats detects unclosed begin_repeat", {
  survey <- data.frame(
    type = c("begin_repeat", "text"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_false(result$valid)
  expect_true(any(grepl("begin_repeat", result$issues)))
})

test_that("xlsform_check_group_repeats detects end_group without begin_group", {
  survey <- data.frame(
    type = c("text", "end_group"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_false(result$valid)
  expect_true(any(grepl("end_group", result$issues)))
})

test_that("xlsform_check_group_repeats detects end_repeat without begin_repeat", {
  survey <- data.frame(
    type = c("text", "end_repeat"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_false(result$valid)
  expect_true(any(grepl("end_repeat", result$issues)))
})

test_that("xlsform_check_group_repeats handles nested groups correctly", {
  survey <- data.frame(
    type = c("begin_group", "begin_group", "text", "end_group", "end_group"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_true(result$valid)
})

test_that("xlsform_check_group_repeats handles nested groups with missing inner end", {
  survey <- data.frame(
    type = c("begin_group", "begin_group", "text", "end_group"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_false(result$valid)
  expect_equal(length(result$issues), 1L)
})

test_that("xlsform_check_group_repeats accepts 'begin group' space variant", {
  survey <- data.frame(
    type = c("begin group", "text", "end group"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_true(result$valid)
})

test_that("xlsform_check_group_repeats errors when df is not a data frame", {
  expect_error(xlsform_check_group_repeats(list(type = "begin_group")))
})

test_that("xlsform_check_group_repeats errors when type column is missing", {
  survey <- data.frame(name = c("q1", "q2"), stringsAsFactors = FALSE)
  expect_error(xlsform_check_group_repeats(survey))
})

test_that("xlsform_check_group_repeats returns valid for survey with no groups", {
  survey <- data.frame(
    type = c("text", "integer", "select_one yn"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_group_repeats(survey)
  expect_true(result$valid)
  expect_equal(length(result$issues), 0L)
})
