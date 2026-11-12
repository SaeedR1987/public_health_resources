#' DeletionLog: Deletion Log Class
#'
#' @description
#' A specialized Log class for tracking records marked for deletion from the dataset.
#' Records which observations should be removed and the reasons for deletion.
#'
#' @details
#' The DeletionLog extends the Log base class with specific functionality for:
#' * Recording which survey records should be deleted
#' * Tracking reasons for deletion (data quality issues, duplicates, etc.)
#' * Validating that UUIDs exist in the dataset before deletion
#' * Providing audit trail for removed observations
#'
#' Required columns:
#' * uuid: Unique identifier for the record to be deleted
#' * enum_id: Enumerator/interviewer identifier
#' * device_id: Device identifier where data was collected
#' * issue: Description of why the record should be deleted
#' * feedback: Additional comments or resolution notes
#'
#' @keywords internal
DeletionLog <- R6::R6Class(
  classname = "DeletionLog",
  inherit = Log,

  public = list(


    #' @description
    #' Creates a new DeletionLog with default schema and required columns
    #'
    #' @param log_df Optional data.frame of existing deletion log entries
    #' @param log_name Character name for the log (default: "Deletion Log")
    #' @param required_columns Character vector; defaults to standard deletion log columns
    #' @param schema List; defaults to standard deletion log schema (all character types)
    #'
    #' @return A new DeletionLog R6 object
    #'
    #' @details
    #' Sets up default schema with all character types for required columns.
    #' Calls parent Log$initialize() to complete setup.
    initialize = function(log_df = NULL,
                          log_name = "Deletion Log",
                          required_columns = NULL,
                          schema = NULL) {

      phrutils::phr_try({

        # Step 1: Setting default inputs

        result <- phrutils::phr_try_step({

      required_columns <- required_columns %||% c(
        "uuid",
        "enum_id",
        "device_id",
        "issue",
        "feedback"
      )

      schema <- schema %||% list(
        types = list(
          uuid      = "character",
          enum_id   = "character",
          device_id = "character",
          issue     = "character",
          feedback  = "character"
        )
      )
      results <- list(
        required_columns = required_columns,
        schema = schema
      )
      results

        },
      step = phrutils::phr_txt("Setting default required columns and schema - DeletionLog$initialize"),
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
      step = phrutils::phr_txt("Super initialize - DeletionLog$initialize"),
      hint = phrutils::phr_txt("check for improper inputs for required columns or schema.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

      },
      on_error = "abort",
      origin = paste0(log_name, " - DeletionLog$initialize")
      )
    },

    #' Add Deletion Entry
    #'
    #' @description
    #' Helper method to add a deletion entry with required fields
    #'
    #' @param uuid Character; unique identifier of record to delete
    #' @param enum_id Character; enumerator identifier (optional, defaults to NA)
    #' @param device_id Character; device identifier (optional, defaults to NA)
    #' @param issue Character; description of why record should be deleted
    #' @param feedback Character; additional comments (optional, defaults to NA)
    #'
    #' @return Invisible TRUE
    #'
    #' @details
    #' Coerces all inputs to character and calls append_entry() to add to log.
    add_deletion = function(uuid,
                            enum_id   = NA_character_,
                            device_id = NA_character_,
                            issue,
                            feedback  = NA_character_) {

      phrutils::phr_try({

      entry <- list(
        uuid      = as.character(uuid),
        enum_id   = as.character(enum_id),
        device_id = as.character(device_id),
        issue     = as.character(issue),
        feedback  = as.character(feedback)
      )

      self$append_entry(entry)

      },
      on_error = "warn",
      origin = paste0(private$..log_name, " - DeletionLog$add_deletion")
      )
    },

    #' Validate Deletion Log Against Dataset
    #'
    #' @description
    #' Validates deletion log entries against an actual dataset
    #'
    #' @param data_obj Data object containing the dataset to validate against
    #' @param stage Character; which data stage to use ("raw", "standardized", "clean")
    #'
    #' @return List of validation issues (empty list if no issues)
    #'
    #' @details
    #' Checks for:
    #' * Missing UUIDs: UUIDs in deletion log that don't exist in dataset
    #'
    #' Helps identify if the deletion log references records that are already removed
    #' or never existed. Issues are returned as warnings for review.
    validate_dl_against_dataset = function(data_obj, stage = "clean") {

      phrutils::phr_try({

        # If log is empty, nothing to validate
        if (nrow(private$..log_df) == 0) {
          return(list())   # no issues
        }

        issues <- list()

        # 1. POST VALIDATE Setup

        result <- phrutils::phr_try_step({

          df_log <- private$..log_df
          data_df <- data_obj$get_data(stage)

          # ---- Dataset must have UUID column
          uuid_col <- data_obj$get(field = "..uuid")
          if (!uuid_col %in% names(data_df)) {
            phrutils::phr_error(
              message = paste0("UUID column '", uuid_col, "' not found in dataset."),
              origin = "DeletionLog$validate_dl_against_dataset"
            )
          }
          results <- list(
            df_log = df_log,
            df = data_df
          )

          results
        },
        step = paste0("Post-validate setup - ", private$..log_name, "$validate_dl_against_dataset"),
        hint =  phrutils::phr_txt("Check that data_obj is a valid Data object and that the specified stage exists in the dataset.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        df_log <- result$df_log
        df <- result$data_df



        # 2. UUID CHECKS

        result <- phrutils::phr_try_step({
          missing_uuid <- setdiff(df_log$uuid, unique(as.character(data_df[[uuid_col]])))
          if (length(missing_uuid) > 0) {
            issues$missing_uuid <- missing_uuid

            phrutils::phr_warning(
              "DeletionLog",
              phrutils::phr_txt(
                "The following UUIDs in deletion log do not exist in dataset: {paste(missing_uuid, collapse=', ')}"
              )
            )
          }

          issues
        },
        step = "UUID checks - DeletionLog$validate_dl_against_dataset",
        hint =  "Check that data_obj is a valid Data object, mapped uuid exists in the dataset, and that the specified stage exists in the dataset."
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        issues <- result

        invisible(issues)

      },
      on_error = "abort",
      origin = paste0(private$..log_name, "$validate_dl_against_dataset"))
    }
  ),

  private = list(
    # Additional Accessible Fields Hook
    #
    # @description
    # Returns subclass-specific fields accessible via `get()`.
    #
    # @return Character vector of field names.
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

    # Additional Accessible Fields Hook
    #
    # @description
    # Returns subclass-specific fields accessible via `get()`.
    #
    # @return Character vector of field names.
    ..additional_call_fields = function() {
      c(
        super$..additional_call_fields(),
        "add_deletion"
      )
    },

    # Additional Settable Fields Hook
    #
    # @description
    # Returns subclass-specific fields that may be modified via `set()`.
    #
    # @return Character vector of field names.
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

    # Deletion-log specific checks (completeness + dataset alignment) and
    # state update. Returns invisible issues list, or a failed step result.
    ..post_validate = function() {

      issues <- list()

      phrutils::phr_try({

      # 2. Validate deletion log rows
      result <- phrutils::phr_try_step({

        df <- private$..log_df
        required_nonempty <- c("uuid", "issue")

        incomplete_cols <- c()
        for (col in required_nonempty) {
          if (any(is.na(df[[col]]) | trimws(df[[col]]) == "")) {
            incomplete_cols <- c(incomplete_cols, col)
          }
        }

        if (length(incomplete_cols) > 0) {
          issues$missing_or_empty <- incomplete_cols

          phrutils::phr_warning(
            private$..log_name,
            phrutils::phr_txt(
              "Deletion log contains missing or empty values in: {paste(incomplete_cols, collapse=', ')}."
            )
          )
        }

        issues
      },
      step = paste0("Deletion-log specific completeness - ", private$..log_name, "$validate"),
      hint = phrutils::phr_txt("check for missing/empty values in required fields (uuid, issue)")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }
      issues <- result

      # 4. Set fields
      result <- phrutils::phr_try_step({

        existing_issues <- self$get(field = "..issues")

        all_issues <- issues

        if (!is.null(existing_issues) && length(existing_issues) > 0) {
          all_issues <- c(existing_issues, issues)
        }

        self$set(field = "..issues", value = all_issues)
        self$set(field = "..validated", value = length(all_issues) == 0)

      },
      step = paste0("Deletion-log set state - ", private$..log_name, "$..post_validate"),
      hint = "Failed to set ..issues / ..validated fields"
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      },
      on_error = "abort",
      origin = paste0(private$..log_name, "$..post_validate"))

      invisible(private$..issues)
    }
  )
)
