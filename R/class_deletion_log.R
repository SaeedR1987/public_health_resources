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

    #' Validate Deletion Log
    #'
    #' @description
    #' Validates deletion log structure and optionally checks alignment with dataset
    #'
    #' @param check_against Optional Data object to validate log entries against
    #' @param stage Character; data stage to validate against ("raw", "standardized", "clean")
    #'
    #' @return Invisible list of validation issues (empty if valid)
    #'
    #' @details
    #' Validation steps:
    #' 1. Calls parent Log$validate() for basic structure and schema validation
    #' 2. Checks that critical fields (uuid, issue) are non-empty
    #' 3. If check_against provided, validates that:
    #'    - UUIDs in deletion log exist in the dataset
    #'    - Issues are recorded for each deletion
    #'
    #' Sets self$validated and self$issues based on results.
    validate = function(check_against = NULL, stage = "clean") {

      phrutils::phr_try({

        # 1. Start with super validation
        result <- phrutils::phr_try_step({

        super_issues <- super$validate()

        if (is.null(super_issues)) super_issues <- list()

        super_issues

        },
        step = phrutils::phr_txt("Super validation - DeletionLog$validate"),
        hint = phrutils::phr_txt("check for validation issues, required column names missing, etc.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        super_issues <- result

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
          super_issues$missing_or_empty <- incomplete_cols

          phrutils::phr_warning(
            private$..log_name,
            phrutils::phr_txt(
              "Deletion log contains missing or empty values in: {paste(incomplete_cols, collapse=', ')}."
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

          if (!is.null(check_against)) {

            post_issues <- private$..post_validate(check_against, stage = stage)

            # integrate issues (post_validate returns list or NULL)
            if (!is.null(post_issues)) {
              for (nm in names(post_issues)) {
                super_issues[[nm]] <- post_issues[[nm]]
              }
            }
          }
          super_issues
        },
        step = "Deletion-log post-validate - DeletionLog$validate",
        hint = "Additional validation against dataset failed; check for UUIDs, enum_id mismatches"
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        super_issues <- result

        # 4. Set fields
        phrutils::phr_try_step({

        self$set(field = "..issues", value = super_issues)
        self$set(field = "..validated", value = length(super_issues) == 0)

        },
        step = "Deletion-log post-validate - DeletionLog$validate",
        hint = "Additional validation against dataset failed; check for UUIDs, enum_id mismatches"
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        invisible(private$..issues)

      }, on_error = "abort", origin = "DeletionLog$validate")
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
        "add_deletion"
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
    ..post_validate = function(data_obj, stage = "clean") {

      phrutils::phr_try({

        # If log is empty, nothing to validate
        if (nrow(private$..log_df) == 0) {
          return(list())   # no issues
        }

        issues <- list()

        # 1. POST VALIDATE Setup

        result <- phrutils::phr_try_step({

          df <- private$..log_df
          data_df <- data_obj$get_data(stage)

          # ---- Dataset must have UUID column
          uuid_col <- data_obj$get(field = "..uuid")
          if (!uuid_col %in% names(data_df)) {
            phrutils::phr_error(
              message = paste0("UUID column '", uuid_col, "' not found in dataset."),
              origin = "DeletionLog$..post_validate"
            )
          }
          results <- list(
            df_log = df_log,
            df = df
          )

          results
        },
        step = "Post-validate setup - DeletionLog$..post_validate",
        hint =  "Check that data_obj is a valid Data object and that the specified stage exists in the dataset."
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        df_log <- result$df_log
        df <- result$df



        # 2. UUID CHECKS

        result <- phrutils::phr_try_step({
          missing_uuid <- setdiff(df$uuid, unique(as.character(data_df[[uuid_col]])))
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
        step = "UUID checks - DeletionLog$..post_validate",
        hint =  "Check that data_obj is a valid Data object, mapped uuid exists in the dataset, and that the specified stage exists in the dataset."
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        issues <- result

        invisible(issues)

      },
      on_error = "abort",
      origin = "DeletionLog$..post_validate")
    }
  )
)
