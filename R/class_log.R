#' Log: Log Base Class
#'
#' @description
#' A foundational R6 class for managing structured log data with validation, schema enforcement,
#' and export capabilities. Used as a base class for specialized logs (CleaningLog, DeletionLog).
#'
#' @details
#' The Log class provides:
#' * Storage and management of structured log entries in a data.frame
#' * Schema definition and validation (column types, allowed values)
#' * Required column enforcement
#' * Metadata tracking (timestamps, validation status)
#' * Export to multiple formats (CSV, RDS, XLSX)
#' * Issue tracking during validation
#' * Hook methods for customization (pre_validate, post_validate)
#' * Safe getter and setter methods for accessing private fields
#'
#' Private fields (accessible via get() and set() methods):
#' * log_df: A tibble containing the log entries
#' * log_name: Character name of the log for display purposes
#' * required_columns: Character vector of columns that must exist
#' * schema: List with 'types' and 'allowed_values' for validation
#' * validated: Logical indicating if log has passed validation
#' * metadata: List of metadata including update timestamps
#'
#' @field issues List of validations issues found
#'
#' @importFrom R6 R6Class
#' @export
Log <- R6::R6Class(
  classname = "Log",
  inherit = Asset,

  public = list(
    # Fields

    #' @description
    #' Creates a new Log object with optional initial data, required columns, and schema
    #'
    #' @param log_df Optional data.frame of log entries; if NULL, creates empty log
    #' @param log_name Character name for the log (default: "Log")
    #' @param required_columns Character vector of required column names
    #' @param schema List with 'types' (column_name = type) and 'allowed_values' (column_name = vector)
    #'
    #' @return A new Log R6 object
    #'
    #' @details
    #' Initialization process:
    #' 1. Creates empty log template if log_df is NULL
    #' 2. Validates that log_df is a data.frame
    #' 3. Adds missing required columns (filled with NA)
    #' 4. Reorders columns (required columns first)
    #' 5. Initializes metadata with current timestamp
    initialize = function(
      log_df = NULL,
      log_name = "Log",
      required_columns = NULL,
      schema = NULL
    ) {

      phrutils::phr_try({

        # Step 1: Super Initalize

        phrutils::phr_try_step({
          super$initialize(name = log_name)
        },
        step = phrutils::phr_txt("Log$initialize - super$initialize"),
        hint = phrutils::phr_txt("Check whether a valid log_name was passed on initialization.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        # Step 2: Set fields

        phrutils::phr_try_step({
          self$set(field = "..log_name", value = log_name)
          self$set(field = "..required_columns", value = required_columns %||% character(0))
          self$set(field = "..schema", value = schema %||% list())
          self$set(field = "..issues", value = list())
        },
        step = phrutils::phr_txt("Log$initialize - set fields"),
        hint = phrutils::phr_txt("Check whether log_name, required columns or schema are valid inputs for {log_name}.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        # Step 3: Check and Set log_df

        phrutils::phr_try_step({

          if (is.null(log_df)) {
            self$set(field = "..log_df", value = private$..empty_log_template())
          }

          phrutils::phr_validate_dataframe(
            log_df,
            origin = paste0(log_name, "$initialize"),
            soft = FALSE
          )

          # ---- Ensure required columns exist (add empty ones if missing)
          for (col in private$..required_columns) {
            if (!col %in% names(log_df)) {
              log_df[[col]] <- NA_character_
            }
          }

          # reorder columns (required first)
          log_df <- log_df[,
                           unique(c(private$..required_columns, names(log_df))),
                           drop = FALSE
          ]

          self$set(field = "..log_df", value = tibble::as_tibble(log_df))

        },
        step = phrutils::phr_txt("Log$initialize - set fields"),
        hint = phrutils::phr_txt("Check whether log_name, required columns or schema are valid inputs for {log_name}.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        private$..touch(log = TRUE,
                        touch = TRUE,
                        name = name,
                        message = phrutils::phr_txt("{log_name} initialized with {nrow(log_df)} entries."))

      },
      on_error = "abort",
      origin = paste0(log_name, "$initialize")
      )

    },

    #' Validate Log
    #'
    #' @description
    #' Validates log structure, required columns, and schema compliance
    #'
    #' @param schema_override Optional schema list to use instead of self$schema
    #'
    #' @return Invisible list of validation issues (empty list if valid)
    #'
    #' @details
    #' Validation steps:
    #' 1. Checks log_df is a valid data.frame
    #' 2. Verifies all required columns exist
    #' 3. Type checking and safe coercion for schema types
    #' 4. Allowed values checking for categorical columns
    #'
    #' Sets self$validated to TRUE if no issues found, updates self$issues.
    #' Attempts safe type coercion (e.g., "123" to 123) when possible.
    validate = function(schema_override = NULL) {
      phrutils::phr_try(
        {
          schema_to_use <- schema_override %||% private$..schema
          issues <- list()

          # 1. BASIC STRUCTURAL VALIDATION

          result <- phrutils::phr_try_step({
            phrutils::phr_validate_dataframe(
              private$..log_df,
              origin = private$..log_name,
              soft = TRUE
            )

            # Required columns must exist
            missing_req <- setdiff(private$..required_columns, names(private$..log_df))
            if (length(missing_req) > 0) {
              issues$missing_required_columns <- missing_req
            }

            issues
          },
          step = phrutils::phr_txt("Basic structural checks - Log$validate"),
          hint = phrutils::phr_txt("Check if log_df is a valid data.frame and contains all required columns.")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          issues <- result


          # 2. Pre Validation Hook

          result <- phrutils::phr_try_step({

          private$..pre_validate()

          },
          step = phrutils::phr_txt("Pre-validation hook - Log$validate"),
          hint = phrutils::phr_txt("Check whether log_name, required columns or schema are valid inputs for {log_name}.")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          pre_issues <- result

          if (!is.null(pre_issues) && length(pre_issues) > 0) {
            issues <- c(issues, pre_issues)
          }

          # 3. SCHEMA VALIDATION (types + allowed_values)

          result <- phrutils::phr_try_step({

            if (!is.null(schema_to_use) && length(schema_to_use) > 0) {
              # ---- TYPE CHECKS
              if (!is.null(schema_to_use$types)) {
                for (nm in names(schema_to_use$types)) {
                  if (!nm %in% names(private$..log_df)) {
                    next
                  }

                  want <- schema_to_use$types[[nm]]
                  got <- class(private$..log_df[[nm]])[1]

                  # Safe coercion classification
                  coercible <- phrutils::is_safely_coercible(private$..log_df[[nm]], want)

                  # Soft-coercion when safe
                  if (!identical(got, want) && coercible) {
                    new_vec <- tryCatch(
                      {
                        if (want == "numeric") {
                          suppressWarnings(as.numeric(private$..log_df[[nm]]))
                        } else if (want == "character") {
                          as.character(private$..log_df[[nm]])
                        } else if (want == "logical") {
                          suppressWarnings(as.logical(private$..log_df[[nm]]))
                        } else if (want == "date" || want == "Date") {
                          phrutils::phr_convert_date(private$..log_df[[nm]])
                        } else {
                          private$..log_df[[nm]]
                        }
                      },
                      error = function(e) private$..log_df[[nm]]
                    )

                    private$..log_df[[nm]] <- new_vec

                    phrutils::phr_message(
                      phrutils::phr_txt(
                        "Coerced column '{nm}' to type '{want}'."
                      )
                    )
                  }

                  # Re-check type after possible coercion
                  final_type <- class(private$..log_df[[nm]])[1]

                  if (!identical(final_type, want)) {
                    issues$type_mismatch[[nm]] <- paste0(
                      "want=",
                      want,
                      ", got=",
                      final_type
                    )
                  }
                }
              }

              # ---- ALLOWED VALUES CHECK
              if (!is.null(schema_to_use$allowed_values)) {
                for (nm in names(schema_to_use$allowed_values)) {
                  if (!nm %in% names(private$..log_df)) {
                    next
                  }

                  allowed <- schema_to_use$allowed_values[[nm]]
                  vals <- unique(private$..log_df[[nm]])

                  bad <- setdiff(vals, allowed)

                  if (length(bad) > 0) {
                    issues$disallowed_values[[nm]] <- bad
                  }
                }
              }
            }
            issues
          },
          step = phrutils::phr_txt("Pre-validation hook - Log$validate"),
          hint = phrutils::phr_txt("Check whether log_name, required columns or schema are valid inputs for {log_name}.")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          issues <- result

          # 4. Post Validation Hook
          result <- phrutils::phr_try_step({

          private$..post_validate()

          },
          step = phrutils::phr_txt("Post-validation hook - Log$validate"),
          hint = phrutils::phr_txt("Check whether log_name, required columns or schema are valid inputs for {log_name}.")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          post_issues <- result

          if (!is.null(post_issues) && length(post_issues) > 0) {
            issues <- c(issues, pre_issues)
          }

          # 5. FINALIZE and Set Fields
          result <- phrutils::phr_try_step({

            self$set(field = "..validated", value = length(issues) == 0)
            self$set(field = "..issues", value = issues %||% list())

          },
          step = phrutils::phr_txt("Finalize and set fields - Log$validate"),
          hint = phrutils::phr_txt("Check whether log_name, required columns or schema are valid inputs for {log_name}.")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          issues <- result

          if (length(issues) > 0) {

            msg <- glue::glue("Log validation completed with issues: {paste(names(issues), collapse=', ')}")

            phrutils::phr_warning(
              private$..log_name,
              phrutils::phr_txt(
                msg
              )
            )
          } else {
            msg <- glue::glue("Log validation completed successfully with no issues.")
          }

          private$..touch(log = TRUE,
                          touch = TRUE,
                          name = private$..log_name,
                          message = phrutils::phr_txt(msg))

          invisible(issues)
        },
        on_error = "abort",
        origin = paste0(private$..log_name, "$validate")
      )
    },

    #' Append Log Entry
    #'
    #' @description
    #' Adds a new row to the log
    #'
    #' @param row_list Named list with column_name = value pairs
    #'
    #' @return Invisible TRUE
    #'
    #' @details
    #' Validates that all required columns are present in row_list.
    #' Converts list to data.frame row and appends via dplyr::bind_rows().
    #' Updates metadata timestamp.
    append_entry = function(row_list) {

      phrutils::phr_try({

      if (!is.list(row_list)) {
        phrutils::phr_error(
          private$..log_name,
          phrutils::phr_txt("append_entry() requires a named list.")
        )
      }

      missing_cols <- setdiff(private$..required_columns, names(row_list))
      if (length(missing_cols) > 0) {
        phrutils::phr_error(
          private$..log_name,
          phrutils::phr_txt(
            "Missing required fields in new log entry: {paste(missing_cols, collapse=', ')}"
          )
        )
      }

      # align names
      df_row <- as.data.frame(row_list, stringsAsFactors = FALSE)

      self$set(field = "..log_df", value = dplyr::bind_rows(private$..log_df, df_row))

      private$..touch(log = TRUE,
                      touch = TRUE,
                      name = private$..log_name,
                      message = phrutils::phr_txt("Added new entry."))

      },
      on_error = "abort",
      origin = "Log$append_entry"
      )

      invisible(TRUE)
    },

    #' Clear Log
    #'
    #' @description
    #' Removes all entries from the log while preserving structure
    #'
    #' @return Invisible TRUE
    #'
    #' @details
    #' Keeps column structure intact but sets row count to 0.
    #' Updates metadata timestamp.
    clear = function() {

      phrutils::phr_try({

      self$set(field = "..log_df", value = private$..log_df[0, , drop = FALSE])

      private$..touch(log = TRUE,
                      touch = TRUE,
                      name = private$..log_name,
                      message = phrutils::phr_txt("{private$..log_name} cleared."))

      invisible(TRUE)

      },
      on_error = "abort",
      origin = paste0(private$..log_name, "$clear")
      )

    },

    #' Export Log
    #'
    #' @description
    #' Exports log data to file in specified format
    #'
    #' @param path Character file path for export
    #' @param format Character; one of "csv", "rds", or "xlsx"
    #'
    #' @return Invisible path to exported file
    #'
    #' @details
    #' Supported formats:
    #' * csv: Comma-separated values via utils::write.csv
    #' * rds: R data structure via saveRDS
    #' * xlsx: Excel workbook via openxlsx (requires openxlsx package)
    export = function(path, format = c("csv", "rds", "xlsx")) {
      format <- match.arg(format)

      phrutils::phr_try(
        {
          if (format == "csv") {
            utils::write.csv(private$..log_df, path, row.names = FALSE)
          }
          if (format == "rds") {
            saveRDS(private$..log_df, path)
          }
          if (format == "xlsx") {
            if (!requireNamespace("openxlsx", quietly = TRUE)) {
              phrutils::phr_error(
                private$..log_name,
                phrutils::phr_txt("Package 'openxlsx' is required for XLSX export.")
              )
            }
            openxlsx::write.xlsx(private$..log_df, file = path)
          }

          phrutils::phr_message(phrutils::phr_txt(
            "Exported {private$..log_name} to {path}."
          ))
          invisible(path)
        },
        on_error = "abort",
        origin = paste0(private$..log_name, "$export")
      )
    },

    #' Get Summary
    #'
    #' @description
    #' Returns a summary list with key log information
    #'
    #' @return Named list with:
    #'   * log_name: Name of the log
    #'   * n_entries: Number of rows
    #'   * n_columns: Number of columns
    #'   * validated: Validation status
    #'   * required_columns: Vector of required column names
    #'   * schema_attached: Whether schema is defined
    #'   * last_updated: Timestamp of last modification
    summary = function() {
      list(
        log_name = private$..log_name,
        n_entries = nrow(private$..log_df),
        n_columns = ncol(private$..log_df),
        validated = private$..validated,
        required_columns = private$..required_columns,
        schema_attached = !is.null(private$..schema),
        last_updated = private$..metadata$modified_datetime
      )
    }
    ),

  private = list(
    # Private fields
    ..issues = NULL,
    ..log_df = NULL,
    ..log_name = NULL,
    ..required_columns = NULL,
    ..schema = NULL,
    ..validated = FALSE,

    #' Additional Accessible Fields Hook
    #'
    #' @description
    #' Returns subclass-specific fields accessible via `get()`.
    #'
    #' @return Character vector of field names.
    ..additional_get_fields = function() {
      c(
        "..issues",
        "..log_df",
        "..log_name",
        "..required_columns",
        "..schema",
        "..validated"
      )
    },

    #' Additional Accessible Fields Hook
    #'
    #' @description
    #' Returns subclass-specific fields accessible via `get()`.
    #'
    #' @return Character vector of field names.
    ..additional_call_fields = function() {
      c(
        "validate",
        "append_entry",
        "clear",
        "export",
        "summary"
      )
    },

    #' Additional Settable Fields Hook
    #'
    #' @description
    #' Returns subclass-specific fields that may be modified via `set()`.
    #'
    #' @return Character vector of field names.
    ..additional_set_fields = function() {
      c(
        "..issues",
        "..log_df",
        "..log_name",
        "..required_columns",
        "..schema",
        "..validated"
      )
    },


    #' Pre-Validation Hook
    #'
    #' @description
    #' Hook method called before validation; override in subclasses for custom logic
    #' Should return a list of issues.
    #'
    #' @return NULL (default implementation is empty)
    ..pre_validate = function() {},

    #' Post-Validation Hook
    #'
    #' @description
    #' Hook method called after validation; override in subclasses for custom logic
    #' Should return a list of issues.
    #'
    #' @return NULL (default implementation is empty)
    ..post_validate = function() {},

    # Build Empty Log Template
    #
    # Creates an empty tibble with correct column types from schema
    #
    # Returns: Empty tibble with required columns and proper types
    #
    # Uses schema$types to create appropriately typed empty vectors for each
    # required column. Defaults to character type if no schema type specified.
    ..empty_log_template = function() {

      phrutils::phr_try({

        cols <- private$..required_columns %||% character(0)

        # If schema has types, use them for empty vectors
        if (!is.null(private$..schema$types)) {
          df <- lapply(cols, function(col) {
            type <- private$schema$types[[col]] %||% "character"

            if (type == "numeric") {
              return(numeric())
            } else if (type == "logical") {
              return(logical())
            } else if (type == "date") {
              return(as.Date(character()))
            } else {
              return(character())
            }
          })
        } else {
          # default all-character template
          df <- lapply(cols, function(col) character())
        }

        names(df) <- cols
        tibble::as_tibble(df)

      },
      on_error = "abort",
      origin = paste0(private$..log_name, "$..empty_log_template")
      )
    }
  )

)
