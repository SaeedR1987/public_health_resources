#' Outputs Schema Utility Functions
#'
#' Functions for converting between outputs schema table and nested list format.
#' These functions follow the same patterns as data_schema_to_table and data_table_to_schema
#' for consistency across the iphRa package.
#'
#' @name outputs_schema_utils
NULL


#' Convert outputs schema to table format
#'
#' Converts a nested list outputs schema to a flat table format.
#' This follows the same pattern as data_schema_to_table().
#'
#' @param outputs_schema A nested list outputs schema
#' @return A data frame with outputs schema
#' @export
outputs_schema_to_table <- function(outputs_schema) {

  outputs_validate_schema_to_table(
    outputs_schema,
    origin = "outputs_schema_to_table"
  )

  rows <- list()

  for (nm in names(outputs_schema)) {
    x <- outputs_schema[[nm]]

    output_name <- x$output_name %||% nm
    output_title <- x$output_title %||% NA_character_
    output_subtitle <- x$output_subtitle %||% NA_character_

    variables <- if (!is.null(x$variables)) {
      paste(x$variables, collapse = ",")
    } else NA_character_

    disaggregation <- if (!is.null(x$disaggregation)) {
      paste(x$disaggregation, collapse = ",")
    } else NA_character_

    output_func_name <- x$output_func_name %||% NA_character_

    test_params <- if (!is.null(x$test_params) && length(x$test_params) > 0) {
      paste(names(x$test_params), x$test_params, sep = "=", collapse = ",")
    } else NA_character_

    output_type <- x$output_type %||% NA_character_

    outputs_group <- x$outputs_group %||% NA_character_

    outputs_per_group <- x$outputs_per_group %||% NA_character_

    dataset_type <- x$dataset_type %||% NA_character_

    rows <- append(rows, list(tibble::tibble(
      output_name = output_name,
      output_title = output_title,
      output_subtitle = output_subtitle,
      variables = variables,
      disaggregation = disaggregation,
      output_func_name = output_func_name,
      test_params = test_params,
      output_type = output_type,
      outputs_group = outputs_group,
      outputs_per_group = outputs_per_group,
      dataset_type = dataset_type
    )))
  }

  dplyr::bind_rows(rows)
}


#' Convert outputs schema table back to nested list
#'
#' Converts a flat table format back into a nested list structure.
#' This follows the same pattern as data_table_to_schema().
#'
#' @param df A data frame with outputs schema definitions
#' @return A nested list outputs schema
#' @export
outputs_table_to_schema <- function(df) {

  iphra_validate_dataframe(df, origin = "outputs_table_to_schema", soft = FALSE)
  outputs_validate_table_to_schema(df)

  iphra_validate_columns(
    df,
    required_cols = c(
      "output_name",
      "output_title",
      "output_subtitle",
      "variables",
      "disaggregation",
      "output_func_name",
      "test_params",
      "output_type"
    ),
    origin = "outputs_table_to_schema",
    hint = iphra_txt("Outputs table must include all required schema fields."),
    soft = FALSE
  )

  outputs <- list()

  for (i in seq_len(nrow(df))) {
    row <- df[i, ]

    output_name <- row$output_name

    variables <- if (!is.na(row$variables) && nzchar(row$variables)) {
      strsplit(row$variables, ",")[[1]] |> trimws()
    } else NULL

    disaggregation <- if (!is.na(row$disaggregation) && nzchar(row$disaggregation)) {
      strsplit(row$disaggregation, ",")[[1]] |> trimws()
    } else NULL

    test_params <- if (!is.na(row$test_params) && nzchar(row$test_params)) {
      pairs <- .parse_indicator_arguments(row$test_params)
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
      params
    } else NULL

    outputs_group <- if ("outputs_group" %in% names(row) &&
                         !is.na(row$outputs_group) && nzchar(row$outputs_group)) {
      row$outputs_group
    } else NULL

    outputs_per_group <- if ("outputs_per_group" %in% names(row) &&
                              !is.na(row$outputs_per_group) && nzchar(row$outputs_per_group)) {
      row$outputs_per_group
    } else NULL

    dataset_type <- if ("dataset_type" %in% names(row) &&
                          !is.na(row$dataset_type) && nzchar(row$dataset_type)) {
      row$dataset_type
    } else NULL

    outputs[[output_name]] <- list(
      output_name = output_name,
      output_title = row$output_title,
      output_subtitle = row$output_subtitle,
      variables = variables,
      disaggregation = disaggregation,
      output_func_name = row$output_func_name,
      test_params = test_params,
      output_type = row$output_type,
      outputs_group = outputs_group,
      outputs_per_group = outputs_per_group,
      dataset_type = dataset_type
    )
  }

  outputs
}


#' Validate outputs schema table before conversion
#'
#' Validates table structure before converting to nested list.
#' This follows the same pattern as data_validate_table_to_schema().
#'
#' @param df Data frame with outputs definitions
#' @return TRUE if valid, otherwise throws error
#' @export
outputs_validate_table_to_schema <- function(df) {

  iphra_try({

    iphra_validate_dataframe(
      df,
      origin = "outputs_validate_table_to_schema",
      soft = FALSE
    )

    required_cols <- c(
      "output_name",
      "output_title",
      "output_subtitle",
      "variables",
      "disaggregation",
      "output_func_name",
      "test_params",
      "output_type"
    )

    iphra_validate_columns(
      df,
      required_cols = required_cols,
      origin = "outputs_validate_table_to_schema",
      hint = iphra_txt("Outputs schema table must include all required fields."),
      soft = FALSE
    )

    # Validate output_type values
    valid_types <- c("visualization", "table")
    invalid_types <- df$output_type[!is.na(df$output_type) &
                                     !df$output_type %in% valid_types]
    
    if (length(invalid_types) > 0) {
      iphra_error(
        origin = "outputs_validate_table_to_schema",
        message = iphra_txt(
          glue::glue("Invalid output_type values: {paste(unique(invalid_types), collapse=', ')}")
        ),
        hint = iphra_txt("output_type must be either 'visualization' or 'table'.")
      )
    }

    TRUE

  }, on_error = "abort", origin = "outputs_validate_table_to_schema")
}


#' Validate outputs schema structure
#'
#' Validates nested list schema structure before converting to table.
#' This follows the same pattern as data_validate_schema_to_table().
#'
#' @param outputs_schema A list containing outputs definitions
#' @param origin Origin string for error messages
#' @return Invisibly returns TRUE if valid, otherwise throws error
#' @export
outputs_validate_schema_to_table <- function(outputs_schema, origin = "outputs_validate_schema_to_table") {

  iphra_try({

    if (is.null(outputs_schema) || !is.list(outputs_schema)) {
      iphra_error(
        origin  = origin,
        message = iphra_txt("Outputs schema must be a list object."),
        hint    = iphra_txt("Ensure outputs_schema is a named list.")
      )
    }

    purrr::iwalk(outputs_schema, function(out, name) {

      if (!is.list(out)) {
        iphra_error(
          origin  = origin,
          message = iphra_txt(glue::glue("Output '{name}' must be a list."))
        )
      }

      # output_name
      if (!is.character(out$output_name) || length(out$output_name) != 1) {
        iphra_error(
          origin  = origin,
          message = iphra_txt(glue::glue("`output_name` in '{name}' must be a single character string."))
        )
      }

      if (out$output_name != name) {
        iphra_error(
          origin  = origin,
          message = iphra_txt(
            glue::glue(
              "Output list name '{name}' does not match output_name field value '{out$output_name}'."
            )
          ),
          hint = iphra_txt(
            "The name used in the outputs list must be identical to the output_name field value."
          )
        )
      }

      # output_type validation
      if (!is.character(out$output_type) || length(out$output_type) != 1) {
        iphra_error(
          origin  = origin,
          message = iphra_txt(glue::glue("`output_type` in '{name}' must be a single character string."))
        )
      }

      valid_types <- c("visualization", "table")
      if (!out$output_type %in% valid_types) {
        iphra_error(
          origin  = origin,
          message = iphra_txt(
            glue::glue(
              "`output_type` in '{name}' must be either 'visualization' or 'table', got '{out$output_type}'."
            )
          )
        )
      }

      # variables (optional)
      if (!is.null(out$variables) &&
          !(is.character(out$variables) && is.atomic(out$variables))) {
        iphra_error(
          origin  = origin,
          message = iphra_txt(glue::glue("`variables` in '{name}' must be a character vector."))
        )
      }

      # test_params (optional)
      if (!is.null(out$test_params) && !is.list(out$test_params)) {
        iphra_error(
          origin  = origin,
          message = iphra_txt(glue::glue("`test_params` in '{name}' must be a list."))
        )
      }
    })

    invisible(TRUE)

  }, on_error = "abort", origin = origin)
}
