# ---------------------------------------------------------------
# XLSForm Tool Validation Helpers
# ---------------------------------------------------------------
# Helper functions for validating XLSForm tool structure and coding.
# These utilities operate on XLSForm survey sheets represented as
# data frames where columns such as 'name', 'type', and 'relevant'
# follow standard XLSForm conventions.
# ---------------------------------------------------------------


# ---- 1. Extract variable names from a cell ---------------------

#' @title Extract XLSForm Variable Names from a Cell
#'
#' @description
#' Identifies and extracts variable names that are wrapped in the XLSForm
#' `${}` syntax (e.g. `${my_variable}`) within a single character string.
#' Only the variable names are returned, without the surrounding `${}` wrapper.
#'
#' @param cell A single character string (one XLSForm cell value).
#'
#' @return A character vector of variable names found in the cell.
#'   Returns `character(0)` if no matches are found, or if `cell` is
#'   `NA`, `NULL`, or not a single character string.
#'
#' @examples
#' xlsform_extract_variables("${age} > 18 and ${consent} = 'yes'")
#' # Returns: c("age", "consent")
#'
#' xlsform_extract_variables("no variables here")
#' # Returns: character(0)
#'
#' @export
xlsform_extract_variables <- function(cell) {
  origin <- "xlsform_extract_variables"

  if (!is.character(cell) || length(cell) != 1L || is.na(cell)) {
    return(character(0))
  }

  matches <- gregexpr("\\$\\{([^}]+)\\}", cell, perl = TRUE)
  raw <- regmatches(cell, matches)[[1]]

  if (length(raw) == 0L) {
    return(character(0))
  }

  # Strip the ${ prefix and } suffix
  gsub("^\\$\\{|\\}$", "", raw)
}


# ---- 2. Collect all wrapped variables across a column ----------

#' @title Collect XLSForm Variable References Across a Data Frame Column
#'
#' @description
#' Iterates through every row of a character column in a data frame and
#' builds a single character vector containing every variable name that
#' appears inside `${}` wrappers. Duplicate names are included once each
#' by default, reflecting all unique references found.
#'
#' @param df A data frame containing XLSForm data.
#' @param col A character string naming the column to scan.
#' @param only_unique Logical; if `TRUE` (default) return only unique variable
#'   names. Set to `FALSE` to return all occurrences.
#'
#' @return A character vector of variable names found across the column.
#'   Returns `character(0)` when no `${}` references exist.
#'
#' @examples
#' survey <- data.frame(
#'   relevant = c(
#'     "${age} > 5",
#'     "${consent} = 'yes' and ${age} > 0",
#'     NA_character_
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' xlsform_collect_variables(survey, "relevant")
#' # Returns: c("age", "consent")
#'
#' @export
xlsform_collect_variables <- function(df, col, only_unique = TRUE) {
  origin <- "xlsform_collect_variables"

  phr_assert(is.data.frame(df), "Argument `df` must be a data frame.", origin = origin)
  phr_assert(
    is.character(col) && length(col) == 1L && col %in% names(df),
    paste0("Column '", col, "' not found in `df`."),
    origin = origin,
    hint = "Check column name spelling and that the data frame has been loaded correctly."
  )

  column_values <- df[[col]]

  all_vars <- unlist(lapply(column_values, xlsform_extract_variables), use.names = FALSE)

  if (is.null(all_vars) || length(all_vars) == 0L) {
    return(character(0))
  }

  if (only_unique) {
    return(unique(all_vars))
  }

  all_vars
}


# ---- 3. Validate a variable name -------------------------------

#' @title Check Whether an XLSForm Variable Name Is Valid
#'
#' @description
#' A valid XLSForm variable name must:
#' \itemize{
#'   \item Contain only letters (ASCII), digits, and underscores (`_`).
#'   \item Start with a letter or underscore (not a digit).
#'   \item Not contain spaces, apostrophes, hyphens, or any other characters
#'         that would make it non-machine-readable.
#' }
#'
#' @param varname A single character string to validate.
#'
#' @return `TRUE` if the name is valid; `FALSE` otherwise.
#'   Returns `FALSE` for `NA`, `NULL`, or non-character input.
#'
#' @examples
#' xlsform_is_valid_varname("my_variable")   # TRUE
#' xlsform_is_valid_varname("my variable")   # FALSE – space
#' xlsform_is_valid_varname("it's")          # FALSE – apostrophe
#' xlsform_is_valid_varname("123start")      # FALSE – starts with digit
#' xlsform_is_valid_varname("_ok_name")      # TRUE
#'
#' @export
xlsform_is_valid_varname <- function(varname) {

  if (!is.character(varname) || length(varname) != 1L || is.na(varname) || nchar(varname) == 0L) {
    return(FALSE)
  }

  grepl("^[A-Za-z_][A-Za-z0-9_]*$", varname, perl = TRUE)
}


# ---- 4. Check whether a variable name appears in a name vector -

#' @title Check Whether an XLSForm Variable Name Is Defined in the Survey
#'
#' @description
#' Looks up a variable name in a reference vector of names — typically the
#' `name` column of the XLSForm survey sheet — to confirm the variable has
#' been declared.
#'
#' @param varname A single character string: the variable name to look up.
#' @param name_vector A character vector of declared variable names to search
#'   within (e.g. `survey$name`).
#'
#' @return `TRUE` if `varname` is found in `name_vector`; `FALSE` otherwise.
#'   Returns `FALSE` for invalid inputs.
#'
#' @examples
#' declared <- c("age", "sex", "consent", "hh_size")
#' xlsform_varname_in_survey("age", declared)     # TRUE
#' xlsform_varname_in_survey("weight", declared)  # FALSE
#'
#' @export
xlsform_varname_in_survey <- function(varname, name_vector) {
  origin <- "xlsform_varname_in_survey"

  if (!is.character(varname) || length(varname) != 1L || is.na(varname)) {
    phr_warning(
      "Argument `varname` must be a single non-NA character string.",
      origin = origin
    )
    return(FALSE)
  }

  phr_assert(
    is.character(name_vector),
    "Argument `name_vector` must be a character vector.",
    origin = origin,
    hint = "Typically pass the 'name' column of the XLSForm survey sheet."
  )

  varname %in% name_vector
}


# ---- 5. Detect unmatched parentheses or square brackets --------

#' @title Check a Cell for Unmatched Parentheses or Square Brackets
#'
#' @description
#' Scans a single character string for bracket balance issues:
#' unmatched opening or closing parentheses `()` and unmatched opening
#' or closing square brackets `[]`.
#'
#' @param cell A single character string (one XLSForm cell value).
#'
#' @return A named logical vector with two elements:
#' \describe{
#'   \item{`parens_ok`}{`TRUE` when parentheses are balanced.}
#'   \item{`brackets_ok`}{`TRUE` when square brackets are balanced.}
#' }
#' Both values are `TRUE` when the cell is `NA`, `NULL`, or an empty string
#' (nothing to be unbalanced).
#'
#' @examples
#' xlsform_check_brackets("(a + b) * (c - d)")
#' # parens_ok: TRUE, brackets_ok: TRUE
#'
#' xlsform_check_brackets("if(a > 1, 'yes'")
#' # parens_ok: FALSE, brackets_ok: TRUE
#'
#' xlsform_check_brackets("selected(${q1}, 'opt_a')")
#' # parens_ok: TRUE, brackets_ok: TRUE
#'
#' @export
xlsform_check_brackets <- function(cell) {

  ok <- c(parens_ok = TRUE, brackets_ok = TRUE)

  if (!is.character(cell) || length(cell) != 1L || is.na(cell) || nchar(cell) == 0L) {
    return(ok)
  }

  chars <- strsplit(cell, "", fixed = TRUE)[[1]]

  paren_depth   <- 0L
  bracket_depth <- 0L
  paren_ok      <- TRUE
  bracket_ok    <- TRUE

  for (ch in chars) {
    if (ch == "(") {
      paren_depth <- paren_depth + 1L
    } else if (ch == ")") {
      paren_depth <- paren_depth - 1L
      if (paren_depth < 0L) {
        paren_ok <- FALSE
        break
      }
    } else if (ch == "[") {
      bracket_depth <- bracket_depth + 1L
    } else if (ch == "]") {
      bracket_depth <- bracket_depth - 1L
      if (bracket_depth < 0L) {
        bracket_ok <- FALSE
        break
      }
    }
  }

  c(
    parens_ok   = paren_ok   && paren_depth == 0L,
    brackets_ok = bracket_ok && bracket_depth == 0L
  )
}


# ---- 6. Detect orphaned square brackets (no preceding $) -------

#' @title Detect Square Brackets Not Preceded by a Dollar Sign
#'
#' @description
#' In XLSForm logic, square brackets are only valid inside the `${}` variable
#' reference syntax. A `[` that appears on its own (not immediately after `$`)
#' is invalid coding and will cause evaluation errors.
#'
#' This helper scans a single cell string and returns `TRUE` when at least one
#' orphaned `[` is found.
#'
#' @param cell A single character string (one XLSForm cell value).
#'
#' @return `TRUE` if orphaned square brackets are found; `FALSE` otherwise.
#'   Returns `FALSE` when `cell` is `NA`, `NULL`, or an empty string.
#'
#' @examples
#' xlsform_orphan_square_brackets("${var}")    # FALSE – no brackets outside ${}
#' xlsform_orphan_square_brackets("[1]")          # TRUE  – standalone
#' xlsform_orphan_square_brackets("${age} > 5")  # FALSE – no brackets
#'
#' @export
xlsform_orphan_square_brackets <- function(cell) {

  if (!is.character(cell) || length(cell) != 1L || is.na(cell) || nchar(cell) == 0L) {
    return(FALSE)
  }

  # Temporarily remove ${ ... } blocks so that legitimate brackets inside them
  # are not flagged.  Replace with a neutral placeholder.
  stripped <- gsub("\\$\\{[^}]*\\}", "VARREF", cell, perl = TRUE)

  # Any remaining [ is orphaned
  grepl("\\[", stripped, fixed = TRUE)
}


# ---- 7. Detect unclosed begin_group / begin_repeat blocks ------

#' @title Check XLSForm Survey for Unclosed Group or Repeat Blocks
#'
#' @description
#' In an XLSForm survey each `begin_group` or `begin_repeat` row in the
#' `type` column must be matched by a subsequent `end_group` or `end_repeat`
#' row respectively.  This helper analyses a data frame representing the survey
#' sheet and reports any blocks that are opened but never closed, or closed
#' without having been opened first.
#'
#' @param df A data frame representing the XLSForm survey sheet. Must contain
#'   a `type` column.
#' @param type_col A character string naming the column that holds question
#'   types (default `"type"`).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if all groups and repeats are properly closed.}
#'   \item{`issues`}{A character vector describing each problem found.
#'     Empty when `valid` is `TRUE`.}
#' }
#'
#' @examples
#' survey <- data.frame(
#'   type = c("text", "begin_group", "text", "end_group",
#'            "begin_repeat", "text", "end_repeat"),
#'   stringsAsFactors = FALSE
#' )
#' xlsform_check_group_repeats(survey)
#' # $valid: TRUE, $issues: character(0)
#'
#' bad_survey <- data.frame(
#'   type = c("begin_group", "text", "begin_repeat", "text"),
#'   stringsAsFactors = FALSE
#' )
#' xlsform_check_group_repeats(bad_survey)
#' # $valid: FALSE, issues describing unclosed blocks
#'
#' @export
xlsform_check_group_repeats <- function(df, type_col = "type") {
  origin <- "xlsform_check_group_repeats"

  phr_assert(is.data.frame(df), "Argument `df` must be a data frame.", origin = origin)
  phr_assert(
    is.character(type_col) && length(type_col) == 1L && type_col %in% names(df),
    paste0("Column '", type_col, "' not found in `df`."),
    origin = origin,
    hint = "Ensure the data frame contains a 'type' column or pass the correct column name."
  )

  types  <- df[[type_col]]
  issues <- character(0)

  # Separate stacks for groups and repeats
  group_stack  <- integer(0)   # row indices of unmatched begin_group
  repeat_stack <- integer(0)   # row indices of unmatched begin_repeat

  for (i in seq_along(types)) {
    raw_type <- trimws(as.character(types[[i]]))

    # Normalize: XLSForm also allows "begin group" (space variant)
    norm_type <- tolower(gsub("[[:space:]]+", "_", raw_type))

    if (norm_type == "begin_group") {
      group_stack <- c(group_stack, i)

    } else if (norm_type == "end_group") {
      if (length(group_stack) == 0L) {
        issues <- c(issues, paste0("Row ", i, ": 'end_group' found without a matching 'begin_group'."))
      } else {
        group_stack <- group_stack[-length(group_stack)]
      }

    } else if (norm_type == "begin_repeat") {
      repeat_stack <- c(repeat_stack, i)

    } else if (norm_type == "end_repeat") {
      if (length(repeat_stack) == 0L) {
        issues <- c(issues, paste0("Row ", i, ": 'end_repeat' found without a matching 'begin_repeat'."))
      } else {
        repeat_stack <- repeat_stack[-length(repeat_stack)]
      }
    }
  }

  # Any unclosed openers remaining in the stacks
  for (row_i in group_stack) {
    issues <- c(issues, paste0("Row ", row_i, ": 'begin_group' has no matching 'end_group'."))
  }

  for (row_i in repeat_stack) {
    issues <- c(issues, paste0("Row ", row_i, ": 'begin_repeat' has no matching 'end_repeat'."))
  }

  list(
    valid  = length(issues) == 0L,
    issues = issues
  )
}


# ---- 8. Validate required columns in a sheet -------------------

#' @title Check That an XLSForm Sheet Contains Its Required Columns
#'
#' @description
#' Every XLSForm sheet has a set of mandatory columns.  By convention:
#' \itemize{
#'   \item The **survey** sheet requires `type`, `name`, and `label`.
#'   \item The **choices** sheet requires `list_name`, `name`, and `label`.
#'   \item The **settings** sheet requires `form_title` and `form_id`.
#' }
#' You can also supply a fully custom set of required column names via the
#' `required_cols` argument, which takes precedence over the named-sheet
#' defaults.
#'
#' @param df A data frame representing the XLSForm sheet.
#' @param sheet A character string identifying the sheet type: one of
#'   `"survey"`, `"choices"`, or `"settings"`.  Ignored when `required_cols`
#'   is supplied.
#' @param required_cols An optional character vector of column names that must
#'   be present.  When provided, `sheet` is ignored.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if all required columns are present.}
#'   \item{`missing`}{A character vector of column names that are absent.
#'     Empty when `valid` is `TRUE`.}
#' }
#'
#' @examples
#' survey <- data.frame(type = "text", name = "q1", label = "Question 1",
#'                      stringsAsFactors = FALSE)
#' xlsform_check_required_sheet_cols(survey, sheet = "survey")
#' # $valid: TRUE, $missing: character(0)
#'
#' bad <- data.frame(type = "text", stringsAsFactors = FALSE)
#' xlsform_check_required_sheet_cols(bad, sheet = "survey")
#' # $valid: FALSE, $missing: c("name", "label")
#'
#' @export
xlsform_check_required_sheet_cols <- function(df, sheet = c("survey", "choices", "settings"),
                                               required_cols = NULL) {
  origin <- "xlsform_check_required_sheet_cols"

  phr_assert(is.data.frame(df), "Argument `df` must be a data frame.", origin = origin)

  if (!is.null(required_cols)) {
    phr_assert(
      is.character(required_cols) && length(required_cols) >= 1L,
      "Argument `required_cols` must be a non-empty character vector.",
      origin = origin
    )
    cols_needed <- required_cols
  } else {
    sheet <- match.arg(sheet)
    cols_needed <- switch(
      sheet,
      survey   = c("type", "name", "label"),
      choices  = c("list_name", "name", "label"),
      settings = c("form_title", "form_id")
    )
  }

  missing_cols <- setdiff(cols_needed, names(df))

  list(
    valid   = length(missing_cols) == 0L,
    missing = missing_cols
  )
}


# ---- 9. Detect duplicate variable names ------------------------

#' @title Check for Duplicate Variable Names in an XLSForm Survey
#'
#' @description
#' Each question row in an XLSForm must have a unique value in its `name`
#' column.  Duplicate names cause conversion errors and ambiguous references
#' in logic expressions.
#'
#' Structural rows such as `begin_group`, `end_group`, `begin_repeat`, and
#' `end_repeat` are excluded from the check by default because they do not
#' always have meaningful names in all tool conventions.
#'
#' @param df A data frame representing the XLSForm survey sheet.
#' @param name_col A character string naming the column that contains variable
#'   names (default `"name"`).
#' @param type_col A character string naming the column that contains question
#'   types (default `"type"`).  Structural rows are excluded automatically.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if all (non-structural) variable names are unique.}
#'   \item{`duplicates`}{A character vector of variable names that appear more
#'     than once.  Empty when `valid` is `TRUE`.}
#' }
#'
#' @examples
#' survey <- data.frame(
#'   type = c("text", "integer", "text"),
#'   name = c("q1", "q2", "q1"),
#'   stringsAsFactors = FALSE
#' )
#' xlsform_check_duplicate_names(survey)
#' # $valid: FALSE, $duplicates: "q1"
#'
#' @export
xlsform_check_duplicate_names <- function(df, name_col = "name", type_col = "type") {
  origin <- "xlsform_check_duplicate_names"

  phr_assert(is.data.frame(df), "Argument `df` must be a data frame.", origin = origin)
  phr_assert(
    is.character(name_col) && length(name_col) == 1L && name_col %in% names(df),
    paste0("Column '", name_col, "' not found in `df`."),
    origin = origin,
    hint = "Ensure the data frame contains a 'name' column or pass the correct column name."
  )

  structural_types <- c(
    "begin_group", "end_group", "begin_repeat", "end_repeat",
    "begin group", "end group", "begin repeat", "end repeat"
  )

  # Exclude structural rows if the type column is present
  if (type_col %in% names(df)) {
    norm_types    <- tolower(trimws(as.character(df[[type_col]])))
    is_structural <- norm_types %in% structural_types
    names_to_check <- df[[name_col]][!is_structural]
  } else {
    names_to_check <- df[[name_col]]
  }

  # Remove NA values before counting
  names_to_check <- names_to_check[!is.na(names_to_check) & nchar(trimws(as.character(names_to_check))) > 0L]

  name_counts <- table(as.character(names_to_check))
  dup_names   <- names(name_counts[name_counts > 1L])

  list(
    valid      = length(dup_names) == 0L,
    duplicates = dup_names
  )
}


# ---- 10. Validate a question type string -----------------------

#' @title Check Whether a String Is a Recognized XLSForm Question Type
#'
#' @description
#' XLSForm defines a standard set of question types.  This helper checks
#' whether a given type string — after stripping a trailing list-name suffix
#' for `select_one` and `select_multiple` — is a recognized type.
#'
#' Recognized types include:
#' `text`, `integer`, `decimal`, `range`, `date`, `time`, `datetime`,
#' `select_one`, `select_multiple`, `note`, `calculate`, `geopoint`,
#' `geotrace`, `geoshape`, `file`, `image`, `audio`, `video`, `barcode`,
#' `acknowledge`, `hidden`, `xml-external`,
#' `begin_group`, `end_group`, `begin_repeat`, `end_repeat`
#' (and their space-separated variants, e.g. `"begin group"`).
#'
#' @param type_str A single character string representing one cell value from
#'   the `type` column of an XLSForm survey sheet.
#'
#' @return `TRUE` if the base type is recognized; `FALSE` otherwise.
#'   Returns `FALSE` for `NA`, `NULL`, or empty input.
#'
#' @examples
#' xlsform_is_valid_type("text")                    # TRUE
#' xlsform_is_valid_type("select_one yn")            # TRUE – list name stripped
#' xlsform_is_valid_type("select_multiple choices")  # TRUE
#' xlsform_is_valid_type("freetext")                 # FALSE – not a standard type
#'
#' @export
xlsform_is_valid_type <- function(type_str) {

  if (!is.character(type_str) || length(type_str) != 1L || is.na(type_str) || nchar(trimws(type_str)) == 0L) {
    return(FALSE)
  }

  # Normalize: lower-case, collapse multiple spaces, and replace underscores with
  # spaces so that "select_one" and "select one" are treated identically.
  norm <- tolower(trimws(gsub("[[:space:]]+", " ", gsub("_", " ", type_str))))

  # select_one / select_multiple carry a trailing list name — handle them first.
  if (grepl("^select (one|multiple)(\\s+\\S.*|$)", norm, perl = TRUE)) {
    return(TRUE)
  }

  valid_types <- c(
    # Basic answer types
    "text", "integer", "decimal", "range",
    "date", "time", "datetime",
    # Structural
    "begin group", "end group", "begin repeat", "end repeat",
    # Media
    "file", "image", "audio", "video", "barcode",
    # Derived / special
    "note", "calculate", "acknowledge", "hidden", "xml-external",
    # Geo
    "geopoint", "geotrace", "geoshape"
  )

  norm %in% valid_types
}


# ---- 11. Verify choice list references -------------------------

#' @title Check That Select Questions Reference Valid Choice Lists
#'
#' @description
#' For every `select_one` and `select_multiple` question in the survey sheet,
#' the type cell contains a list name (e.g. `"select_one yes_no"`).  This
#' helper verifies that each referenced list name is present in the
#' `list_name` column of the choices sheet.
#'
#' @param survey_df A data frame representing the XLSForm survey sheet.
#'   Must contain a `type` column.
#' @param choices_df A data frame representing the XLSForm choices sheet.
#'   Must contain a `list_name` column.
#' @param type_col Character string naming the type column in `survey_df`
#'   (default `"type"`).
#' @param list_name_col Character string naming the list-name column in
#'   `choices_df` (default `"list_name"`).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if every referenced list name is found in the
#'     choices sheet.}
#'   \item{`missing_lists`}{A character vector of list names that are
#'     referenced in the survey but absent from choices.  Empty when
#'     `valid` is `TRUE`.}
#' }
#'
#' @examples
#' survey <- data.frame(
#'   type = c("text", "select_one yes_no", "select_multiple region"),
#'   stringsAsFactors = FALSE
#' )
#' choices <- data.frame(
#'   list_name = c("yes_no", "yes_no", "region", "region"),
#'   stringsAsFactors = FALSE
#' )
#' xlsform_check_choice_references(survey, choices)
#' # $valid: TRUE, $missing_lists: character(0)
#'
#' @export
xlsform_check_choice_references <- function(survey_df, choices_df,
                                             type_col      = "type",
                                             list_name_col = "list_name") {
  origin <- "xlsform_check_choice_references"

  phr_assert(is.data.frame(survey_df),  "Argument `survey_df` must be a data frame.",  origin = origin)
  phr_assert(is.data.frame(choices_df), "Argument `choices_df` must be a data frame.", origin = origin)
  phr_assert(
    type_col %in% names(survey_df),
    paste0("Column '", type_col, "' not found in `survey_df`."),
    origin = origin,
    hint = "Pass the correct column name via `type_col`."
  )
  phr_assert(
    list_name_col %in% names(choices_df),
    paste0("Column '", list_name_col, "' not found in `choices_df`."),
    origin = origin,
    hint = "Pass the correct column name via `list_name_col`."
  )

  # Normalize: lower-case, collapse spaces, replace underscores with spaces so that
  # "select_one" and "select one" are treated identically.
  types <- trimws(tolower(gsub("[[:space:]]+", " ", gsub("_", " ", as.character(survey_df[[type_col]])))))

  # Identify select rows: after normalization the pattern is always
  # "select one <listname>" or "select multiple <listname>"
  select_mask  <- grepl("^select (one|multiple)\\s+\\S", types, perl = TRUE)
  select_types <- types[select_mask]

  if (length(select_types) == 0L) {
    return(list(valid = TRUE, missing_lists = character(0)))
  }

  # Extract the list name: always the 3rd token after normalization
  # (tokens: "select", "one"/"multiple", "<listname>")
  referenced_lists <- vapply(select_types, function(t) {
    tokens <- strsplit(t, "[[:space:]]+")[[1]]
    if (length(tokens) >= 3L) tokens[3L] else NA_character_
  }, character(1L), USE.NAMES = FALSE)

  referenced_lists <- unique(referenced_lists[!is.na(referenced_lists)])

  available_lists  <- unique(as.character(choices_df[[list_name_col]]))
  available_lists  <- available_lists[!is.na(available_lists)]

  missing_lists <- setdiff(referenced_lists, available_lists)

  list(
    valid         = length(missing_lists) == 0L,
    missing_lists = missing_lists
  )
}


# ---- 12. Check that questions have labels ----------------------

#' @title Flag XLSForm Questions Missing a Label
#'
#' @description
#' Every user-facing question in an XLSForm should have a value in the
#' `label` column.  Structural and non-visible row types (`begin_group`,
#' `end_group`, `begin_repeat`, `end_repeat`, `calculate`, `hidden`,
#' `note`) do not require a label and are excluded from the check.
#'
#' @param df A data frame representing the XLSForm survey sheet.
#' @param type_col Character string naming the type column (default `"type"`).
#' @param label_col Character string naming the label column (default
#'   `"label"`).  The column may be absent; in that case every non-structural
#'   row is flagged.
#' @param name_col Character string naming the name column (default `"name"`),
#'   used in diagnostic messages.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if every user-facing question has a label.}
#'   \item{`unlabelled_rows`}{An integer vector of row indices where a label
#'     is missing.  Empty when `valid` is `TRUE`.}
#' }
#'
#' @examples
#' survey <- data.frame(
#'   type  = c("text", "integer", "calculate"),
#'   name  = c("q1", "q2", "calc1"),
#'   label = c("Your name", NA, NA),
#'   stringsAsFactors = FALSE
#' )
#' xlsform_check_label_presence(survey)
#' # $valid: FALSE – row 2 (integer 'q2') is missing a label
#'
#' @export
xlsform_check_label_presence <- function(df,
                                          type_col  = "type",
                                          label_col = "label",
                                          name_col  = "name") {
  origin <- "xlsform_check_label_presence"

  phr_assert(is.data.frame(df), "Argument `df` must be a data frame.", origin = origin)
  phr_assert(
    type_col %in% names(df),
    paste0("Column '", type_col, "' not found in `df`."),
    origin = origin
  )

  # Types that do not require a user-visible label
  no_label_types <- c(
    "begin_group", "end_group", "begin_repeat", "end_repeat",
    "begin group", "end group", "begin repeat", "end repeat",
    "calculate", "hidden", "note"
  )

  types      <- trimws(tolower(as.character(df[[type_col]])))
  base_types <- vapply(types, function(t) strsplit(t, "[[:space:]]+")[[1]][1], character(1L))

  needs_label <- !base_types %in% no_label_types

  if (!label_col %in% names(df)) {
    # Label column entirely absent — all user-facing rows are unlabelled
    unlabelled <- which(needs_label)
  } else {
    labels <- df[[label_col]]
    label_missing <- is.na(labels) | trimws(as.character(labels)) == ""
    unlabelled    <- which(needs_label & label_missing)
  }

  list(
    valid          = length(unlabelled) == 0L,
    unlabelled_rows = as.integer(unlabelled)
  )
}


# ---- 13. Verify calculate rows have an expression --------------

#' @title Check That All 'calculate' Rows Have a Calculation Expression
#'
#' @description
#' In XLSForm, a row of type `calculate` must have a non-empty value in the
#' `calculation` column.  A missing or empty calculation makes the question
#' evaluate to `NULL` and is almost certainly a coding error.
#'
#' @param df A data frame representing the XLSForm survey sheet.
#' @param type_col Character string naming the type column (default `"type"`).
#' @param calculation_col Character string naming the calculation column
#'   (default `"calculation"`).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`valid`}{`TRUE` if every `calculate` row has a non-empty
#'     calculation expression.}
#'   \item{`empty_rows`}{An integer vector of row indices where the
#'     calculation is missing.  Empty when `valid` is `TRUE`.}
#' }
#'
#' @examples
#' survey <- data.frame(
#'   type        = c("text", "calculate", "calculate"),
#'   name        = c("q1", "age_years", "bmi"),
#'   calculation = c(NA, "${age} div 1", NA),
#'   stringsAsFactors = FALSE
#' )
#' xlsform_check_calculate_expression(survey)
#' # $valid: FALSE, $empty_rows: 3
#'
#' @export
xlsform_check_calculate_expression <- function(df,
                                                type_col        = "type",
                                                calculation_col = "calculation") {
  origin <- "xlsform_check_calculate_expression"

  phr_assert(is.data.frame(df), "Argument `df` must be a data frame.", origin = origin)
  phr_assert(
    type_col %in% names(df),
    paste0("Column '", type_col, "' not found in `df`."),
    origin = origin
  )

  types      <- trimws(tolower(as.character(df[[type_col]])))
  calc_rows  <- which(types == "calculate")

  if (length(calc_rows) == 0L) {
    return(list(valid = TRUE, empty_rows = integer(0)))
  }

  if (!calculation_col %in% names(df)) {
    # Calculation column is entirely absent — all calculate rows are empty
    return(list(valid = FALSE, empty_rows = as.integer(calc_rows)))
  }

  calculations <- df[[calculation_col]]
  is_empty <- is.na(calculations) | trimws(as.character(calculations)) == ""
  empty_rows <- calc_rows[is_empty[calc_rows]]

  list(
    valid      = length(empty_rows) == 0L,
    empty_rows = as.integer(empty_rows)
  )
}
