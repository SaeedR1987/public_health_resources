#' CleaningLog: Cleaning Log Class
#'
#' @description
#' A specialized Log class for tracking data cleaning operations. Records changes
#' made to survey data including the old value, new value, reason for change, and
#' whether the change was applied.
#'
#' @details
#' The CleaningLog extends the Log base class with specific functionality for:
#' * Tracking data changes at the row and column level
#' * Validating that UUIDs, enumerator IDs, and column names exist in dataset
#' * Verifying that old.value matches current data (before applying changes)
#' * Enforcing required fields for data quality assurance
#'
#' Required columns:
#' * uuid: Unique identifier for the record to be changed
#' * enum_id: Enumerator/interviewer identifier
#' * device_id: Device identifier where data was collected
#' * question.name: Variable/column name to be changed
#' * issue: Description of the data quality issue
#' * feedback: Resolution or feedback on the issue
#' * changed: "yes" or "no" indicating if change should be applied
#' * old.value: Current value in the dataset
#' * new.value: Proposed new value
#'
#' @keywords internal
CleaningLog <- R6::R6Class(
  classname = "CleaningLog",
  inherit = Log,

  public = list(
    #' @description
    #' Creates a new CleaningLog with default schema and required columns
    #'
    #' @param log_df Optional data.frame of existing cleaning log entries
    #' @param log_name Character name for the log (default: "Cleaning Log")
    #' @param required_columns Character vector; defaults to standard cleaning log columns
    #' @param schema List; defaults to standard cleaning log schema
    #'
    #' @return A new CleaningLog R6 object
    #'
    #' @details
    #' Sets up default schema with all character types and "changed" restricted to "yes"/"no".
    #' Calls parent Log$initialize() to complete setup.
    initialize = function(
      log_df = NULL,
      log_name = "Cleaning Log",
      required_columns = NULL,
      schema = NULL
    ) {

      phrutils::phr_try({

      # Step 1: Setting default inputs

      result <- phrutils::phr_try_step({

      required_columns <- required_columns %||%
        c(
          "uuid",
          "enum_id",
          "device_id",
          "question.name",
          "issue",
          "feedback",
          "changed",
          "old.value",
          "new.value"
        )

      schema <- schema %||%
        list(
          types = list(
            uuid = "character",
            enum_id = "character",
            device_id = "character",
            question.name = "character",
            issue = "character",
            feedback = "character",
            changed = "character",
            old.value = "character",
            new.value = "character"
          ),
          allowed_values = list(
            changed = c("yes", "no")
          )
        )

        results <- list(
          required_columns = required_columns,
          schema = schema
        )

        results

        },
        step = phrutils::phr_txt("Setting default required columns and schema - CleaningLog$initialize"),
        hint = phrutils::phr_txt("check for improper inputs for required columns or schema.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        required_columns <- result$required_columns
        schema <- result$schema

        # Step 2: Super initialize

        result <- phrutils::phr_try_step({

        super$initialize(
          log_df = log_df,
          log_name = log_name,
          required_columns = required_columns,
          schema = schema
        )

        },
        step = phrutils::phr_txt("Super initialize - CleaningLog$initialize"),
        hint = phrutils::phr_txt("check for improper inputs for required columns or schema.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

      },
      on_error = "abort",
      origin = paste0(log_name, " - CleaningLog$initialize")
      )

    },

    #' Validate Cleaning Log
    #'
    #' @description
    #' Validates cleaning log structure and optionally checks alignment with dataset
    #'
    #' @param data_obj Optional Data object to validate log entries against
    #' @param stage Character; data stage to validate against ("raw", "standardized", "clean")
    #'
    #' @return Invisible list of validation issues (empty if valid)
    #'
    #' @details
    #' Validation steps:
    #' 1. Calls parent Log$validate() for basic structure and schema validation
    #' 2. Checks that critical fields (uuid, question.name, changed, old.value, new.value)
    #'    are non-empty
    #' 3. If data_obj provided, validates that:
    #'    - UUIDs exist in the dataset
    #'    - enum_id values exist (if mapped)
    #'    - question.name columns exist in dataset
    #'    - old.value matches current dataset values
    #'
    #' Sets self$validated and self$issues based on results.
    validate = function(data_obj = NULL, stage = "clean") {
      phrutils::phr_try(
        {

          # 1. Start with super validation
          result <- phrutils::phr_try_step({

          super_issues <- super$validate()
          if (is.null(super_issues)) {
            super_issues <- list()
          }
          super_issues
          },
          step = phrutils::phr_txt("Super validation - CleaningLog$validate"),
          hint = phrutils::phr_txt("check for validation issues, required column names missing, etc.")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          super_issues <- result

          # 2. Cleaning-log specific completeness
          #    Always require uuid, question.name, and changed to be present.
          #    Require old.value only for rows where changed == "yes" — we need to
          #    know the current value to apply a correction.
          #    Do NOT require new.value: it is legitimately blank/NA at log-generation
          #    time while awaiting human review.

          # 2. Validate cleaning log rows
          result <- phrutils::phr_try_step({

            required_always <- c("uuid", "question.name", "changed")


            df <- private$..log_df

            incomplete_always <- vapply(
              required_always,
              function(col) any(is.na(df[[col]]) | trimws(df[[col]]) == ""),
              logical(1)
            )

            bad_cols <- required_always[incomplete_always]

            # For old.value, only flag rows where changed == "yes"
            changed_yes <- !is.na(df$changed) & df$changed == "yes"
            if (
              any(changed_yes) &&
              any(
                is.na(df$old.value[changed_yes]) |
                trimws(df$old.value[changed_yes]) == ""
              )
            ) {
              bad_cols <- c(bad_cols, "old.value")
            }

            if (length(bad_cols) > 0) {
              super_issues$missing_or_empty <- bad_cols

              phrutils::phr_warning(
                private$..log_name,
                phrutils::phr_txt(
                  "Cleaning log contains missing/empty values in: {paste(bad_cols, collapse=', ')}."
                )
              )
            }

            super_issues

          },
          step = "Cleaning-log specific completeness - CleaningLog$validate",
          hint = "check for missing/empty values in required fields (uuid, question.name, changed, old.value)"
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          super_issues <- result

          # 3. POST VALIDATE (dataset alignment)

          phrutils::phr_try_step({

            post_issues <- list()

            if (!is.null(data_obj)) {
              # try-safe block: collect issues instead of stopping
              post_issues <- private$..post_validate(data_obj, stage = stage)
              if (is.null(post_issues)) post_issues <- list()
            }

            # 4. Merge issues and update state

            all_issues <- c(super_issues, post_issues)
            self$set(field = "..issues", value = all_issues)
            self$set(field = "..validated", value = length(private$..issues) == 0)

          },
          step = "Cleaning-log post-validate - CleaningLog$validate",
          hint = "Additional validation against dataset failed; check for UUIDs, enum_id, question.name, and old.value mismatches"
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

          invisible(private$..issues)
        },
        on_error = "abort",
        origin = "CleaningLog$validate"
      )
    },

    #' Add Change Entry
    #'
    #' @description
    #' Helper method to add a cleaning change entry with all required fields
    #'
    #' @param uuid Character; unique identifier of record to change
    #' @param enum_id Character; enumerator identifier
    #' @param device_id Character; device identifier
    #' @param question.name Character; variable/column name to change
    #' @param issue Character; description of the data quality issue
    #' @param feedback Character; resolution or feedback
    #' @param changed Character; "yes" or "no" indicating if change should be applied
    #' @param old.value Character; current value in dataset
    #' @param new.value Character; proposed new value
    #'
    #' @return Invisible TRUE
    #'
    #' @details
    #' Coerces all inputs to character and calls append_entry() to add to log.
    #' Normalizes 'changed' to lowercase for consistency.
    add_change = function(
      uuid,
      enum_id,
      device_id,
      question.name,
      issue,
      feedback,
      changed,
      old.value,
      new.value
    ) {

      phrutils::phr_try({

      entry <- list(
        uuid = as.character(uuid),
        enum_id = as.character(enum_id),
        device_id = as.character(device_id),
        question.name = as.character(question.name),
        issue = as.character(issue),
        feedback = as.character(feedback),
        changed = tolower(as.character(changed)),
        old.value = as.character(old.value),
        new.value = as.character(new.value)
      )

      self$append_entry(entry)

    },
    on_error = "warn",
    origin = "CleaningLog$add_change",
    hint = "Check that all required fields are provided and of correct type."
    )

    }
  ),

  private = list(
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
        super$..additional_call_fields(),
        "add_change"
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

    #' Post-Validate Against Dataset
    #'
    #' @description
    #' Validates cleaning log entries against an actual dataset to ensure integrity
    #'
    #' @param data_obj Data object containing the dataset to validate against
    #' @param stage Character; which data stage to use ("raw", "standardized", "clean")
    #'
    #' @return List of validation issues (empty list if no issues)
    #'
    #' @details
    #' Checks for:
    #' * Missing UUIDs: UUIDs in log that don't exist in dataset
    #' * Missing enum_id: Enumerator IDs that don't exist in dataset
    #' * Unknown columns: question.name values not found as columns in dataset
    #' * Value mismatches: old.value doesn't match actual current value in dataset
    #'
    #' All issues are returned as warnings, not errors, to allow inspection.
    ..post_validate = function(data_obj, stage = "clean") {

      phrutils::phr_try({

      # If log is empty, nothing to validate
      if (nrow(private$..log_df) == 0) {
        return(list()) # no issues
      }

      issues <- list()

      # 1. POST VALIDATE Setup

      result <- phrutils::phr_try_step({

        df_log <- private$..log_df
        df <- data_obj$get_data(stage)

        # catastrophic error only
        if (is.null(df)) {
          phrutils::phr_error(
            "Dataset is NULL — cannot validate CleaningLog against dataset.",
            origin = "CleaningLog$post_validate"
          )
        }

        results <- list(
          df_log = df_log,
          df = df
        )

        results

      },
      step = "Post-validate setup - CleaningLog$post_validate",
      hint =  "Check that data_obj is a valid Data object and that the specified stage exists in the dataset."
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      df_log <- result$df_log
      df <- result$df

      # 2. UUID CHECKS

      result <- phrutils::phr_try_step({

        uuid_col <- data_obj$get(field = "..uuid")

        if (!uuid_col %in% names(df)) {
          phrutils::phr_error(
            message = paste0("Dataset missing UUID column '", uuid_col, "'."),
            origin = "CleaningLog$post_validate"
          )
        }

        missing_uuid <- setdiff(df_log$uuid, as.character(df[[uuid_col]]))
        if (length(missing_uuid) > 0) {
          issues$uuid_not_found <- missing_uuid
          phrutils::phr_warning(
            private$..log_name,
            phrutils::phr_txt(
              "Unknown UUID(s) in cleaning log: {paste(missing_uuid, collapse=', ')}"
            )
          )
        }

        issues

      },
      step = "UUID checks - CleaningLog$post_validate",
      hint =  "Check that data_obj is a valid Data object, mapped uuid exists in the dataset, and that the specified stage exists in the dataset."
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      issues <- result

      # 3. ENUM_ID CHECKS

      result <- phrutils::phr_try_step({

      data_variable_map <- data_obj$get(field = "..variable_map")
      if ("enum_id" %in% names(data_variable_map)) {
        enum_col <- data_variable_map$enum_id

        if (enum_col %in% names(df)) {
          missing_enum <- setdiff(df_log$enum_id, df[[enum_col]])

          if (length(missing_enum) > 0) {
            issues$enum_id_not_found <- missing_enum
            phrutils::phr_warning(
              private$..log_name,
              phrutils::phr_txt(
                "Unknown enum_id(s): {paste(missing_enum, collapse=', ')}"
              )
            )
          }
        }
      }

      issues

      },
      step = "ENUM_ID checks - CleaningLog$post_validate",
      hint =  "Check that data_obj is a valid Data object, mapped enum_id exists in the dataset, and that the specified stage exists in the dataset."
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      issues <- result

      # 4. QUESTION.NAME CHECKS
      result <- phrutils::phr_try_step({
        missing_q <- setdiff(df_log$`question.name`, names(df))
        if (length(missing_q) > 0) {
          issues$unknown_question_names <- missing_q
          phrutils::phr_warning(
            private$..log_name,
            phrutils::phr_txt(
              "Unknown question.name columns: {paste(missing_q, collapse=', ')}"
            )
          )
        }

        issues
      },
      step = "QUESTION.NAME checks - CleaningLog$post_validate",
      hint =  "Check that data_obj is a valid Data object, mapped question.name exists in the dataset, and that the specified stage exists in the dataset."
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      issues <- result

      # old.value matches data before change

      # 5. Old.value matches current data
      phrutils::phr_try_step({
        for (i in seq_len(nrow(df_log))) {
          row <- df_log[i, ]
          col_name <- row$`question.name`
          uuid_val <- row$uuid
          old_val <- as.character(row$`old.value`)

          # Skip validation if column doesn't exist in dataset
          if (!col_name %in% names(df)) {
            next
          }

          idx <- which(df[[uuid_col]] == uuid_val)

          if (length(idx) == 1) {
            actual <- as.character(df[[col_name]][idx])

            if (!identical(actual, old_val)) {
              issues$old_value_mismatch <- c(
                issues$old_value_mismatch,
                paste0("uuid=", uuid_val, ", col=", col_name)
              )

              phrutils::phr_warning(
                private$..log_name,
                phrutils::phr_txt(
                  "old.value mismatch for uuid {uuid_val}, column {col_name}: expected '{old_val}', found '{actual}'."
                )
              )
            }
          }
        }
        issues
      },
      step = "Old.value checks - CleaningLog$post_validate",
      hint =  "Check whether old.value matches the current data."
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      issues <- result

      },
      on_error = "abort",
      origin = "CleaningLog$post_validate"
      )

      return(issues)
    }
  )
)
