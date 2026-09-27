#' Optimized select_multiple helpers (benchmark copies)
#'
#' @description
#' Performance-optimized counterparts of `expand_select_multiple()` and
#' `process_select_multiple_columns()` (see `R/utils_data_class.R`), used
#' exclusively by `DataOptimized`/`HouseholdDataOptimized` so that the
#' original and optimized implementations can be benchmarked side-by-side
#' on the same dummy data (see `tests/manual/benchmark_standardize_clean.R`).
#'
#' Behavioral parity with the originals is intended; only the internal
#' implementation is changed:
#' * `expand_select_multiple_optimized()` builds the 0/1 dummy-column matrix
#'   with a single vectorized indexed assignment (`mat[cbind(row_idx, col_idx)] <- 1L`)
#'   instead of a nested `for` loop over rows and tokens with per-cell
#'   `data.frame` assignment.
#' * `process_select_multiple_columns_optimized()` accumulates the expanded
#'   dummy data frames for each `select_multiple` column in a list and
#'   `cbind()`s them onto the source data once, instead of calling `cbind()`
#'   inside the loop (which re-copies the growing data frame on every
#'   iteration).
#'
#' @name utils_data_class_optimized
#' @keywords internal
NULL

#' Expand a select_multiple column into 0/1 dummy columns (optimized)
#'
#' @description
#' Vectorized replacement for `expand_select_multiple()`. See that function's
#' documentation for parameter/return details; behavior is intended to match
#' exactly, only the internal implementation differs.
#'
#' @param column Character vector containing space-separated select_multiple values
#' @param column_name Character scalar; base name used to construct dummy column names
#' @param separator Character scalar; token separator (default: `" "`)
#'
#' @return A data frame of 0/1 dummy columns, one per unique token found in `column`
#'
#' @keywords internal
expand_select_multiple_optimized <- function(column, column_name, separator = " ") {
  # Handle NA and empty values
  column <- ifelse(is.na(column) | column == "", NA_character_, column)

  n_row <- length(column)
  non_na_idx <- which(!is.na(column))

  if (length(non_na_idx) == 0) {
    return(data.frame())
  }

  # Split each non-NA cell once, trim tokens, and drop empties -- vectorized
  # over the "split" step (strsplit already vectorizes across cells) with a
  # single lapply pass to clean up each cell's tokens.
  split_tokens <- lapply(
    strsplit(column[non_na_idx], separator, fixed = TRUE),
    function(toks) {
      toks <- trimws(toks)
      toks[toks != ""]
    }
  )

  all_values <- unlist(split_tokens, use.names = FALSE)

  if (length(all_values) == 0) {
    return(data.frame())
  }

  unique_values <- sort(unique(all_values))

  # Build (row, col) index pairs for every token occurrence in one shot,
  # then fill the whole presence matrix with a single indexed assignment
  # instead of looping cell-by-cell.
  tokens_per_row <- lengths(split_tokens)
  row_idx <- rep(non_na_idx, tokens_per_row)
  col_idx <- match(all_values, unique_values)

  mat <- matrix(0L, nrow = n_row, ncol = length(unique_values))
  mat[cbind(row_idx, col_idx)] <- 1L

  dummy_df <- as.data.frame(mat)
  colnames(dummy_df) <- paste0(column_name, ".", unique_values)

  dummy_df
}


#' Identify and expand all select_multiple columns in a dataset (optimized)
#'
#' @description
#' Vectorized/batched replacement for `process_select_multiple_columns()`.
#' Uses `expand_select_multiple_optimized()` for each `select_multiple`
#' column, and accumulates all resulting dummy data frames in a list to
#' `cbind()` onto the source data once at the end (instead of `cbind()`-ing
#' inside the loop, which re-copies the whole data frame every iteration).
#'
#' @param data A data frame to process
#' @param schema A schema list with question_types field (can be NULL or
#'   without question_types, in which case the function returns the
#'   original data unchanged)
#'
#' @return A list with 'data' (modified data frame), 'expanded_columns'
#'   (character vector of new column names), and 'other_related_columns'
#'   (list tracking columns related to "other" responses in select_multiple
#'   questions)
#'
#' @keywords internal
process_select_multiple_columns_optimized <- function(data, schema) {
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
  select_mult_vars <- names(question_types)[
    question_types == "select_multiple"
  ]

  if (length(select_mult_vars) == 0) {
    return(result)
  }

  # Accumulate dummy data frames for all select_multiple columns and bind
  # them onto the source data only once, after the loop.
  dummy_df_list <- list()

  for (var in select_mult_vars) {
    if (var %in% names(data)) {
      dummy_df <- expand_select_multiple_optimized(data[[var]], var)

      if (ncol(dummy_df) > 0) {
        dummy_df_list[[var]] <- dummy_df
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

          # Find which one exists in the (original) data; dummy columns are
          # bound after this loop, but "other" text columns are always
          # original source columns, so checking `data` (not `result$data`)
          # here preserves the original lookup semantics.
          other_text_col <- NULL
          for (potential_col in potential_other_text_cols) {
            if (potential_col %in% names(data)) {
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

  if (length(dummy_df_list) > 0) {
    result$data <- do.call(cbind, c(list(result$data), dummy_df_list))
  }

  return(result)
}
