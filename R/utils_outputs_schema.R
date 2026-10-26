#' Outputs Schema Utility Functions
#'
#' Functions for validating, normalising and parsing the outputs schema.
#'
#' The outputs schema is a data frame: one row per output, with the
#' comma-separated `variables` / `disaggregation` cells and the `test_params`
#' cell parsed on demand, row by row. There is no nested-list representation.
#'
#' @name outputs_schema_utils
NULL


# Columns every outputs schema row must define.
.outputs_schema_required_cols <- c(
  "output_title",
  "output_name",
  "output_subtitle",
  "variables",
  "disaggregation",
  "output_func_name",
  "test_params",
  "output_type"
)

# Optional columns; added as NA_character_ by outputs_normalize_schema().
.outputs_schema_optional_cols <- c(
  "outputs_group",
  "outputs_per_group",
  "dataset_type",
  "output_title_english",
  "output_title_french",
  "output_title_arabic"
)

# Accepted values for the output_type and dataset_type columns.
.outputs_schema_valid_output_types <- c("visualization", "table")
.outputs_schema_valid_dataset_types <- c("base", "survey_design")


#' Parse a comma-separated outputs schema cell into a character vector
#'
#' Used for the `variables` and `disaggregation` columns. Commas inside
#' parentheses or quotes are not treated as separators.
#'
#' @param x A single character value, or NA/NULL.
#' @return A character vector, or NULL when the cell is empty.
#' @export
outputs_parse_list_field <- function(x) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))) {
    return(NULL)
  }

  parts <- trimws(.parse_indicator_arguments(as.character(x)))
  parts <- parts[nzchar(parts)]

  if (length(parts) == 0) NULL else parts
}


#' Parse an outputs schema `test_params` cell into a named list
#'
#' Splits a `"key=value,key2=value2"` string into a named list, coercing
#' numeric and logical values. Commas inside parentheses (e.g. `c(a, b)`) or
#' quotes are preserved.
#'
#' @param x A single character value, or NA/NULL.
#' @return A named list of parameters, or NULL when the cell is empty.
#' @export
outputs_parse_test_params <- function(x) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))) {
    return(NULL)
  }

  pairs <- .parse_indicator_arguments(as.character(x))
  params <- list()

  for (pair in pairs) {
    pair <- trimws(pair)
    eq_pos <- regexpr("=", pair)[[1]]

    if (eq_pos > 1) {
      key <- trimws(substr(pair, 1, eq_pos - 1))
      value_str <- trimws(substr(pair, eq_pos + 1, nchar(pair)))
      value <- suppressWarnings(as.numeric(value_str))

      if (is.na(value)) {
        if (toupper(value_str) %in% c("TRUE", "FALSE")) {
          value <- as.logical(value_str)
        } else {
          value <- value_str
        }
      }

      params[[key]] <- value
    }
  }

  if (length(params) == 0) NULL else params
}


#' Validate an outputs schema data frame
#'
#' Checks that the required columns are present and that `output_type`,
#' `dataset_type` and `output_title` hold usable values. Warns (but does not
#' error) when two rows would store their result under the same key.
#'
#' @param df A data frame holding the outputs schema.
#' @param origin Origin string for error messages.
#' @return TRUE if valid, otherwise throws an error.
#' @export
outputs_validate_schema <- function(df, origin = "outputs_validate_schema") {

  phrutils::phr_try({

    phrutils::phr_validate_dataframe(df, origin = origin, soft = FALSE)

    phrutils::phr_validate_columns(
      df,
      required_cols = .outputs_schema_required_cols,
      origin = origin,
      hint = phr_txt("Outputs schema must include all required fields."),
      soft = FALSE
    )

    invalid_types <- df$output_type[
      !is.na(df$output_type) &
        !df$output_type %in% .outputs_schema_valid_output_types
    ]

    if (length(invalid_types) > 0) {
      phrutils::phr_error(
        origin = origin,
        message = phr_txt(
          glue::glue(
            "Invalid output_type values: {paste(unique(invalid_types), collapse = ', ')}"
          )
        ),
        hint = phr_txt("output_type must be either 'visualization' or 'table'.")
      )
    }

    if ("dataset_type" %in% names(df)) {
      invalid_datasets <- df$dataset_type[
        !is.na(df$dataset_type) &
          nzchar(df$dataset_type) &
          !df$dataset_type %in% .outputs_schema_valid_dataset_types
      ]

      if (length(invalid_datasets) > 0) {
        phrutils::phr_error(
          origin = origin,
          message = phr_txt(
            glue::glue(
              "Invalid dataset_type values: {paste(unique(invalid_datasets), collapse = ', ')}"
            )
          ),
          hint = phr_txt(
            "dataset_type must be 'base' (unweighted) or 'survey_design' (weighted)."
          )
        )
      }
    }

    missing_titles <- is.na(df$output_title) | !nzchar(df$output_title)

    if (any(missing_titles)) {
      phrutils::phr_error(
        origin = origin,
        message = phr_txt(
          glue::glue(
            "Missing output_title in row(s): {paste(which(missing_titles), collapse = ', ')}"
          )
        ),
        hint = phr_txt("Every outputs schema row must define an output_title.")
      )
    }

    # Each row is run independently, so a duplicate output_title is harmless
    # (it only affects the auto-generated display title). A genuine collision
    # requires the same output_name within the same outputs_group, since that
    # is the key results are stored under.
    if ("output_name" %in% names(df)) {
      storage_key <- paste(
        df$output_name,
        if ("outputs_group" %in% names(df)) df$outputs_group else "",
        sep = "\r"
      )

      duplicate_keys <- unique(df$output_name[duplicated(storage_key)])

      if (length(duplicate_keys) > 0) {
        phrutils::phr_warning(
          origin = origin,
          message = phr_txt(
            glue::glue(
              "Duplicate output_name value(s) within the same outputs_group: ",
              "{paste(duplicate_keys, collapse = ', ')}. ",
              "Each row still runs, but later results will overwrite earlier ones."
            )
          )
        )
      }
    }

    TRUE

  }, on_error = "abort", origin = origin)
}


#' Normalise an outputs schema data frame
#'
#' Validates the schema, adds any missing optional columns as `NA_character_`
#' and returns the columns in a stable order as a tibble.
#'
#' @param df A data frame holding the outputs schema.
#' @param origin Origin string for error messages.
#' @return A tibble holding the normalised outputs schema.
#' @export
outputs_normalize_schema <- function(df, origin = "outputs_normalize_schema") {

  phrutils::phr_try({

    outputs_validate_schema(df, origin = origin)

    df <- tibble::as_tibble(df)

    for (col in .outputs_schema_optional_cols) {
      if (!col %in% names(df)) {
        df[[col]] <- NA_character_
      }
    }

    known_cols <- c(
      .outputs_schema_required_cols,
      .outputs_schema_optional_cols
    )

    for (col in known_cols) {
      df[[col]] <- as.character(df[[col]])
    }

    dplyr::select(
      df,
      dplyr::all_of(known_cols),
      dplyr::everything()
    )

  }, on_error = "abort", origin = origin)
}
