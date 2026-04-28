
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


# ---- 8. xlsform_check_required_sheet_cols ---------------

test_that("xlsform_check_required_sheet_cols passes for complete survey sheet", {
  df <- data.frame(type = "text", name = "q1", label = "Q1",
                   stringsAsFactors = FALSE)
  result <- xlsform_check_required_sheet_cols(df, sheet = "survey")
  expect_true(result$valid)
  expect_equal(length(result$missing), 0L)
})

test_that("xlsform_check_required_sheet_cols flags missing columns in survey", {
  df <- data.frame(type = "text", stringsAsFactors = FALSE)
  result <- xlsform_check_required_sheet_cols(df, sheet = "survey")
  expect_false(result$valid)
  expect_setequal(result$missing, c("name", "label"))
})

test_that("xlsform_check_required_sheet_cols passes for complete choices sheet", {
  df <- data.frame(list_name = "yn", name = "yes", label = "Yes",
                   stringsAsFactors = FALSE)
  result <- xlsform_check_required_sheet_cols(df, sheet = "choices")
  expect_true(result$valid)
})

test_that("xlsform_check_required_sheet_cols flags missing list_name in choices", {
  df <- data.frame(name = "yes", label = "Yes", stringsAsFactors = FALSE)
  result <- xlsform_check_required_sheet_cols(df, sheet = "choices")
  expect_false(result$valid)
  expect_true("list_name" %in% result$missing)
})

test_that("xlsform_check_required_sheet_cols accepts custom required_cols", {
  df <- data.frame(a = 1, b = 2, stringsAsFactors = FALSE)
  result <- xlsform_check_required_sheet_cols(df, required_cols = c("a", "c"))
  expect_false(result$valid)
  expect_equal(result$missing, "c")
})

test_that("xlsform_check_required_sheet_cols errors when df is not a data frame", {
  expect_error(xlsform_check_required_sheet_cols(list(type = "text"), sheet = "survey"))
})


# ---- 9. xlsform_check_duplicate_names -------------------

test_that("xlsform_check_duplicate_names returns valid for unique names", {
  df <- data.frame(type = c("text", "integer"), name = c("q1", "q2"),
                   stringsAsFactors = FALSE)
  result <- xlsform_check_duplicate_names(df)
  expect_true(result$valid)
  expect_equal(length(result$duplicates), 0L)
})

test_that("xlsform_check_duplicate_names detects duplicate names", {
  df <- data.frame(type = c("text", "text", "integer"),
                   name = c("q1", "q1", "q2"),
                   stringsAsFactors = FALSE)
  result <- xlsform_check_duplicate_names(df)
  expect_false(result$valid)
  expect_true("q1" %in% result$duplicates)
})

test_that("xlsform_check_duplicate_names ignores structural rows", {
  df <- data.frame(
    type = c("begin_group", "text", "text", "end_group"),
    name = c("grp", "q1", "q2", "grp"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_duplicate_names(df)
  expect_true(result$valid)
})

test_that("xlsform_check_duplicate_names ignores NA names", {
  df <- data.frame(type = c("text", "text"), name = c("q1", NA),
                   stringsAsFactors = FALSE)
  result <- xlsform_check_duplicate_names(df)
  expect_true(result$valid)
})

test_that("xlsform_check_duplicate_names errors when name column is missing", {
  df <- data.frame(type = "text", stringsAsFactors = FALSE)
  expect_error(xlsform_check_duplicate_names(df))
})


# ---- 10. xlsform_is_valid_type --------------------------

test_that("xlsform_is_valid_type returns TRUE for basic types", {
  expect_true(xlsform_is_valid_type("text"))
  expect_true(xlsform_is_valid_type("integer"))
  expect_true(xlsform_is_valid_type("decimal"))
  expect_true(xlsform_is_valid_type("date"))
  expect_true(xlsform_is_valid_type("calculate"))
  expect_true(xlsform_is_valid_type("note"))
  expect_true(xlsform_is_valid_type("geopoint"))
})

test_that("xlsform_is_valid_type returns TRUE for select types with list names", {
  expect_true(xlsform_is_valid_type("select_one yes_no"))
  expect_true(xlsform_is_valid_type("select_multiple region"))
  # space-separated variants
  expect_true(xlsform_is_valid_type("select one yes_no"))
  expect_true(xlsform_is_valid_type("select multiple region"))
})

test_that("xlsform_is_valid_type returns TRUE for structural types", {
  expect_true(xlsform_is_valid_type("begin_group"))
  expect_true(xlsform_is_valid_type("end_group"))
  expect_true(xlsform_is_valid_type("begin_repeat"))
  expect_true(xlsform_is_valid_type("end_repeat"))
  # space variants
  expect_true(xlsform_is_valid_type("begin group"))
  expect_true(xlsform_is_valid_type("end group"))
})

test_that("xlsform_is_valid_type returns FALSE for non-standard types", {
  expect_false(xlsform_is_valid_type("freetext"))
  expect_false(xlsform_is_valid_type("number"))
  expect_false(xlsform_is_valid_type("dropdown"))
})

test_that("xlsform_is_valid_type returns FALSE for NA or empty input", {
  expect_false(xlsform_is_valid_type(NA_character_))
  expect_false(xlsform_is_valid_type(""))
  expect_false(xlsform_is_valid_type(NULL))
})


# ---- 11. xlsform_check_choice_references ----------------

test_that("xlsform_check_choice_references passes when all lists exist", {
  survey <- data.frame(
    type = c("text", "select_one yes_no", "select_multiple region"),
    stringsAsFactors = FALSE
  )
  choices <- data.frame(
    list_name = c("yes_no", "yes_no", "region", "region"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_choice_references(survey, choices)
  expect_true(result$valid)
  expect_equal(length(result$missing_lists), 0L)
})

test_that("xlsform_check_choice_references flags missing list", {
  survey <- data.frame(
    type = c("select_one yes_no", "select_one not_defined"),
    stringsAsFactors = FALSE
  )
  choices <- data.frame(
    list_name = c("yes_no", "yes_no"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_choice_references(survey, choices)
  expect_false(result$valid)
  expect_true("not_defined" %in% result$missing_lists)
})

test_that("xlsform_check_choice_references returns valid when no selects present", {
  survey  <- data.frame(type = c("text", "integer"), stringsAsFactors = FALSE)
  choices <- data.frame(list_name = "unused", stringsAsFactors = FALSE)
  result  <- xlsform_check_choice_references(survey, choices)
  expect_true(result$valid)
})

test_that("xlsform_check_choice_references errors when type column is missing", {
  survey  <- data.frame(name = "q1", stringsAsFactors = FALSE)
  choices <- data.frame(list_name = "yn", stringsAsFactors = FALSE)
  expect_error(xlsform_check_choice_references(survey, choices))
})

test_that("xlsform_check_choice_references errors when list_name column is missing", {
  survey  <- data.frame(type = "select_one yn", stringsAsFactors = FALSE)
  choices <- data.frame(name = "yes", stringsAsFactors = FALSE)
  expect_error(xlsform_check_choice_references(survey, choices))
})

test_that("xlsform_check_choice_references handles space-variant select types", {
  survey <- data.frame(
    type = c("select one yes_no", "select multiple region"),
    stringsAsFactors = FALSE
  )
  choices <- data.frame(
    list_name = c("yes_no", "region"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_choice_references(survey, choices)
  expect_true(result$valid)
})


# ---- 12. xlsform_check_label_presence -------------------

test_that("xlsform_check_label_presence passes when all questions have labels", {
  survey <- data.frame(
    type  = c("text", "integer"),
    name  = c("q1", "q2"),
    label = c("Question 1", "Question 2"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_label_presence(survey)
  expect_true(result$valid)
  expect_equal(length(result$unlabelled_rows), 0L)
})

test_that("xlsform_check_label_presence flags rows with NA label", {
  survey <- data.frame(
    type  = c("text", "integer"),
    name  = c("q1", "q2"),
    label = c("Question 1", NA),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_label_presence(survey)
  expect_false(result$valid)
  expect_equal(result$unlabelled_rows, 2L)
})

test_that("xlsform_check_label_presence does not flag calculate or structural rows", {
  survey <- data.frame(
    type  = c("text", "calculate", "begin_group", "end_group"),
    name  = c("q1", "calc1", "grp", "grp"),
    label = c("Q1", NA, NA, NA),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_label_presence(survey)
  expect_true(result$valid)
})

test_that("xlsform_check_label_presence flags all user-facing rows when label column absent", {
  survey <- data.frame(
    type = c("text", "integer", "calculate"),
    name = c("q1", "q2", "c1"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_label_presence(survey)
  expect_false(result$valid)
  expect_equal(result$unlabelled_rows, c(1L, 2L))
})

test_that("xlsform_check_label_presence errors when type column is missing", {
  df <- data.frame(name = "q1", label = "Q1", stringsAsFactors = FALSE)
  expect_error(xlsform_check_label_presence(df))
})


# ---- 13. xlsform_check_calculate_expression -------------

test_that("xlsform_check_calculate_expression passes when all calculate rows have expressions", {
  survey <- data.frame(
    type        = c("text", "calculate"),
    name        = c("q1", "age_yrs"),
    calculation = c(NA, "${age} div 1"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_calculate_expression(survey)
  expect_true(result$valid)
  expect_equal(length(result$empty_rows), 0L)
})

test_that("xlsform_check_calculate_expression flags missing calculation", {
  survey <- data.frame(
    type        = c("text", "calculate", "calculate"),
    name        = c("q1", "c1", "c2"),
    calculation = c(NA, "${age} div 1", NA),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_calculate_expression(survey)
  expect_false(result$valid)
  expect_equal(result$empty_rows, 3L)
})

test_that("xlsform_check_calculate_expression returns valid when no calculate rows exist", {
  survey <- data.frame(
    type        = c("text", "integer"),
    name        = c("q1", "q2"),
    calculation = c(NA, NA),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_calculate_expression(survey)
  expect_true(result$valid)
})

test_that("xlsform_check_calculate_expression flags all calculate rows when column is absent", {
  survey <- data.frame(
    type = c("text", "calculate"),
    name = c("q1", "c1"),
    stringsAsFactors = FALSE
  )
  result <- xlsform_check_calculate_expression(survey)
  expect_false(result$valid)
  expect_equal(result$empty_rows, 2L)
})

test_that("xlsform_check_calculate_expression errors when type column is missing", {
  df <- data.frame(name = "q1", calculation = "x", stringsAsFactors = FALSE)
  expect_error(xlsform_check_calculate_expression(df))
})
