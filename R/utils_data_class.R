#' Convert a canonical schema list into a flat table format
#' Helper function to safely convert boolean values from Excel/CSV
#'
#' Handles boolean values that may come in different formats when reading from
#' external sources like Excel files. Excel and readxl may convert TRUE/FALSE
#' to logical, string, or numeric types depending on the cell format.
#'
#' @param val A value to convert to boolean. Can be:
#'   - logical: TRUE/FALSE
#'   - character: "TRUE", "true", "True", "T", "1" (treated as TRUE)
#'   - numeric: 1 (TRUE), 0 (FALSE)
#'   - NA (treated as FALSE)
#'
#' @return Logical TRUE or FALSE
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' .safe_bool(TRUE)        # TRUE
#' .safe_bool("TRUE")      # TRUE
#' .safe_bool("true")      # TRUE
#' .safe_bool(1)           # TRUE
#' .safe_bool(FALSE)       # FALSE
#' .safe_bool("FALSE")     # FALSE
#' .safe_bool(0)           # FALSE
#' .safe_bool(NA)          # FALSE
#' .safe_bool("")          # FALSE
#' }
.safe_bool <- function(val) {
  # Check for NA first (before any other operations)
  if (is.na(val)) {
    return(FALSE)
  }
  # Check for empty string or "NA" string
  if (is.character(val) && (val == "" || val == "NA")) {
    return(FALSE)
  }
  # Handle logical values directly
  if (is.logical(val)) {
    return(isTRUE(val))
  }
  # Handle string values
  if (is.character(val)) {
    return(
      val == "TRUE" ||
        val == "true" ||
        val == "True" ||
        val == "T" ||
        val == "1"
    )
  }
  # Handle numeric values (1 = TRUE, 0 or other values = FALSE)
  # Note: Only 1 is treated as TRUE to match Excel's boolean cell conversion (1=TRUE, 0=FALSE)
  # Other numeric values are explicitly treated as FALSE for safety
  if (is.numeric(val)) {
    return(val == 1)
  }
  return(FALSE)
}

#' Resolve the household linkage column of a linked dataset
#'
#' Determines which column of a linked dataset (roster, deaths, ...) holds the
#' parent household identifier. Resolution order:
#' 1. the column mapped to the `hh_uuid` / `household_uuid` role in the linked
#'    object's `variable_map`;
#' 2. the `col_names` aliases declared for those roles in the linked object's
#'    variable schema (in schema preference order);
#' 3. the role names themselves used as literal column names.
#'
#' Candidates are only accepted when the column is actually present in
#' `data_cols`, so a stale or unmapped `variable_map` cannot make the lookup
#' fall back to a column that does not exist.
#'
#' @param linked_obj A `Data` object linked to a `HouseholdData` object.
#' @param data_cols Character vector of column names of the linked dataset.
#' @param roles Character vector of variable roles to try, in order of preference.
#'
#' @return A list with `column` (the resolved column name, or `NULL` when no
#'   candidate exists in the data), `role` (the role it was resolved from) and
#'   `candidates` (every candidate that was considered).
#'
#' @keywords internal
.resolve_linked_hh_column <- function(
  linked_obj,
  data_cols,
  roles = c("hh_uuid", "household_uuid")
) {
  valid <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }

  variable_map <- linked_obj$get(field = "..variable_map")
  if (!is.list(variable_map)) {
    variable_map <- list()
  }

  schema <- linked_obj$get(field = "..variable_schema")

  candidates <- character(0)
  candidate_roles <- character(0)

  add_candidates <- function(cols, role) {
    cols <- cols[vapply(cols, valid, logical(1))]
    cols <- setdiff(cols, candidates)
    candidates <<- c(candidates, cols)
    candidate_roles <<- c(candidate_roles, rep(role, length(cols)))
  }

  for (role in roles) {
    # 1. explicit variable_map entry
    add_candidates(variable_map[[role]], role)

    # 2. schema col_names aliases, in the order declared by the schema
    if (
      is.data.frame(schema) &&
        all(c("variable", "col_names") %in% names(schema))
    ) {
      rows <- which(schema$variable == role)
      if ("rule_type" %in% names(schema)) {
        rows <- rows[schema$rule_type[rows] %in% c("variable", NA)]
      }
      for (row in rows) {
        aliases <- schema$col_names[[row]]
        if (valid(aliases)) {
          add_candidates(trimws(strsplit(aliases, ",")[[1]]), role)
        }
      }
    }

    # 3. the role name used as a literal column name
    add_candidates(role, role)
  }

  match_idx <- which(candidates %in% data_cols)[1]

  list(
    column = if (is.na(match_idx)) NULL else candidates[[match_idx]],
    role = if (is.na(match_idx)) NULL else candidate_roles[[match_idx]],
    candidates = candidates
  )
}

#' Expand select_multiple column into dummy variables
#'
#' @description
#' Takes a select_multiple column (with space-separated values) and expands it
#' into multiple dummy columns, one for each unique response option found.
#'
#' @param column A character vector containing space-separated values
#' @param column_name The name of the original column (used for naming dummies)
#' @param separator The separator used in the select_multiple values (default: " ")
#'
#' @return A data frame with dummy columns named \{column_name\}_\{value\} containing 1
#'   if value present, 0 if not. Returns empty data frame (0 rows, 0 columns) if no
#'   valid values found.
#'
#' @keywords internal
expand_select_multiple <- function(column, column_name, separator = " ") {
  # Handle NA and empty values
  column <- ifelse(is.na(column) | column == "", NA_character_, column)

  # Split each cell by separator and get unique values across all rows
  all_values <- unlist(strsplit(
    column[!is.na(column)],
    separator,
    fixed = TRUE
  ))
  all_values <- trimws(all_values)
  all_values <- all_values[all_values != ""]
  unique_values <- sort(unique(all_values))

  # If no unique values found, return empty data frame
  if (length(unique_values) == 0) {
    return(data.frame())
  }

  # Create dummy columns
  dummy_df <- data.frame(matrix(
    0,
    nrow = length(column),
    ncol = length(unique_values)
  ))
  colnames(dummy_df) <- paste0(column_name, ".", unique_values)

  # Fill in 1s where values are present
  for (i in seq_along(column)) {
    if (!is.na(column[i]) && column[i] != "") {
      values_in_row <- trimws(strsplit(column[i], separator, fixed = TRUE)[[1]])
      values_in_row <- values_in_row[values_in_row != ""]

      for (val in values_in_row) {
        if (val %in% unique_values) {
          col_idx <- which(unique_values == val)
          dummy_df[i, col_idx] <- 1
        }
      }
    }
  }

  return(dummy_df)
}


#' Identify and expand all select_multiple columns in a dataset
#'
#' @description
#' Helper function that processes a dataset based on schema question_types,
#' identifying select_multiple columns and expanding them into dummy variables.
#'
#' @param data A data frame to process
#' @param schema A schema list with question_types field (can be NULL or without
#'   question_types, in which case the function returns the original data unchanged)
#'
#' @return A list with 'data' (modified data frame), 'expanded_columns' (character
#'   vector of new column names), and 'other_related_columns' (list tracking columns
#'   related to "other" responses in select_multiple questions)
#'
#' @keywords internal
process_select_multiple_columns <- function(data, schema) {
  result <- list(
    data = data,
    expanded_columns = character(0),
    other_related_columns = list()
  )

  # Check if schema has question_types
  if (
    is.null(schema) ||
      is.null(schema$question_types) ||
      length(schema$question_types) == 0
  ) {
    return(result)
  }

  # Find select_multiple columns
  question_types <- schema$question_types
  select_mult_vars <- names(question_types)[question_types == "select_multiple"]

  if (length(select_mult_vars) == 0) {
    return(result)
  }

  # Process each select_multiple column
  for (var in select_mult_vars) {
    if (var %in% names(data)) {
      # Expand the column
      dummy_df <- expand_select_multiple(data[[var]], var)

      if (ncol(dummy_df) > 0) {
        # Add dummy columns to the data
        result$data <- cbind(result$data, dummy_df)
        result$expanded_columns <- c(
          result$expanded_columns,
          colnames(dummy_df)
        )

        # Check if "other" is one of the values in this select_multiple
        other_dummy_col <- paste0(var, ".other")
        if (other_dummy_col %in% colnames(dummy_df)) {
          # Look for a corresponding open text "other" column
          # Common naming patterns: var_other_text, var_other_specify, var_other_value
          potential_other_text_cols <- c(
            paste0(var, "_other_text"),
            paste0(var, "_other_specify"),
            paste0(var, "_other"),
            paste0(var, "_autre"),
            paste0(var, "_text")
          )

          # Find which one exists in the data
          other_text_col <- NULL
          for (potential_col in potential_other_text_cols) {
            if (potential_col %in% names(result$data)) {
              other_text_col <- potential_col
              break
            }
          }

          # Track all three columns related to "other" response
          result$other_related_columns[[var]] <- list(
            original_column = var,
            dummy_other_column = other_dummy_col,
            text_other_column = other_text_col # May be NULL if not found
          )
        }
      }
    }
  }

  return(result)
}


# INDICATOR SCHEMA FUNCTIONS ####

#' Parse indicator arguments string respecting parentheses and quoted strings
#'
#' Internal helper to split argument strings like "arg1=val1,arg2=c(a,b,c),arg3=val3"
#' properly by respecting parentheses and quoted strings so that c(...) vectors
#' and quoted values containing commas are not split incorrectly.
#'
#' This function is intentionally called twice during indicator processing:
#' 1. First call: Parse the full argument string into individual argument pairs
#' 2. Second call: Parse the content inside c(...) vectors into individual elements
#' This two-stage parsing is necessary because arguments and vector elements have
#' different semantic meanings and resolution rules.
#'
#' @param arg_string Character string containing arguments
#' @return Character vector of argument pairs
#' @keywords internal
.parse_indicator_arguments <- function(arg_string) {
  if (is.null(arg_string) || is.na(arg_string) || arg_string == "") {
    return(character(0))
  }

  result <- character(0)
  current_arg <- ""
  paren_depth <- 0L
  in_quote <- FALSE
  quote_char <- ""
  chars <- strsplit(arg_string, "")[[1]]

  for (char in chars) {
    if (in_quote) {
      current_arg <- paste0(current_arg, char)
      if (char == quote_char) {
        in_quote <- FALSE
        quote_char <- ""
      }
    } else if (char %in% c('"', "'")) {
      in_quote <- TRUE
      quote_char <- char
      current_arg <- paste0(current_arg, char)
    } else if (char == "(") {
      paren_depth <- paren_depth + 1L
      current_arg <- paste0(current_arg, char)
    } else if (char == ")") {
      paren_depth <- paren_depth - 1L
      current_arg <- paste0(current_arg, char)
    } else if (char == "," && paren_depth == 0L) {
      # This is a separator between arguments, not within a vector or quoted string
      if (nchar(current_arg) > 0) {
        result <- c(result, trimws(current_arg))
        current_arg <- ""
      }
    } else {
      current_arg <- paste0(current_arg, char)
    }
  }

  # Add the last argument
  if (nchar(current_arg) > 0) {
    result <- c(result, trimws(current_arg))
  }

  result
}
