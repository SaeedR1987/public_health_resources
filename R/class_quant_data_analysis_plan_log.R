#' QuantDataAnalysisPlanLog: Quant Data Analysis Plan Log Class
#'
#' @description
#' A specialized Log class for managing quantitative data analysis plans.
#' Enforces the column structure defined in phr_analysis_plan_template.csv
#' and provides validation for analysis plan entries.
#'
#' @details
#' The QuantDataAnalysisPlanLog extends the Log base class with specific functionality for:
#' * Tracking planned quantitative analyses (indicators and calculations)
#' * Enforcing required analysis plan columns
#' * Validating calculation types and variable names
#' * Ensuring data analysis plans follow the standard template structure
#'
#' Required columns (matching phr_analysis_plan_template.csv):
#' * indicator_name: Name/label of the indicator
#' * calculation: Type of calculation (prop, mean, median, ratio)
#' * var_name: Primary variable name in the dataset
#' * denom_var: Denominator variable (for ratios)
#' * disaggregation: Disaggregation variable(s)
#' * multiplier: Multiplier for the calculation (e.g., 100 for percentages)
#' * indicator_unit: Unit of measurement for the indicator
#'
#' @keywords internal
QuantDataAnalysisPlanLog <- R6::R6Class(
  classname = "QuantDataAnalysisPlanLog",
  inherit = Log,

  public = list(

    #' @description
    #' Creates a new QuantDataAnalysisPlanLog with default schema and required columns
    #'
    #' @param log_df Optional data.frame of existing analysis plan entries
    #' @param log_name Character name for the log (default: "Quant Data Analysis Plan")
    #' @param required_columns Character vector; defaults to standard analysis plan columns
    #' @param schema List; defaults to standard analysis plan schema
    #'
    #' @return A new QuantDataAnalysisPlanLog R6 object
    #'
    #' @details
    #' Sets up default schema with appropriate types for each column.
    #' Calls parent Log$initialize() to complete setup.
    initialize = function(log_df = NULL,
                          log_name = "Quant Data Analysis Plan",
                          required_columns = NULL,
                          schema = NULL) {

      required_columns <- required_columns %||% c(
        "indicator_name",
        "calculation",
        "var_name",
        "denom_var",
        "disaggregation",
        "multiplier",
        "indicator_unit"
      )

      schema <- schema %||% list(
        types = list(
          indicator_name = "character",
          calculation = "character",
          var_name = "character",
          denom_var = "character",
          disaggregation = "character",
          multiplier = "numeric",
          indicator_unit = "character"
        ),
        allowed_values = list(
          calculation = c("prop", "mean", "median", "ratio", "cat", "categorical", "select_multiple_cat")
        )
      )

      super$initialize(
        log_df = log_df,
        log_name = log_name,
        required_columns = required_columns,
        schema = schema
      )
    },

    #' Validate Quant Data Analysis Plan Log against Dataset
    #'
    #' @description
    #' Validates quant data analysis plan log entries against an actual dataset to ensure integrity
    #'
    #' @param data_obj Data object containing the dataset to validate against
    #' @param stage Character; which data stage to use ("raw", "standardized", "clean")
    #'
    #' @return List of validation issues (empty list if no issues)
    #'
    #' @details
    #' For the `var_name`, `denom_var` and `disaggregation` columns, checks that:
    #' * each referenced variable is mapped in the data object's variable map
    #' * the mapped column exists in the dataset at the requested stage
    #'
    #' `denom_var` and `disaggregation` may be missing (skipped); multiple
    #' disaggregation variables may be separated by commas or semicolons.
    #' All issues are returned as warnings, not errors, to allow inspection.
    validate_dap_against_dataset = function(data_obj, stage = "clean") {

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
              "Dataset is NULL — cannot validate QuantDataAnalysisPlanLog against dataset.",
              origin = "QuantDataAnalysisPlanLog$validate_dap_against_dataset"
            )
          }

          results <- list(
            df_log = df_log,
            df = df
          )

          results

        },
        step = "Post-validate setup - QuantDataAnalysisPlanLog$validate_dap_against_dataset",
        hint =  "Check that data_obj is a valid Data object and that the specified stage exists in the dataset."
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        df_log <- result$df_log
        df <- result$df



        # VARIABLE MAP + DATASET CHECKS for var_name, denom_var, disaggregation
        result <- phrutils::phr_try_step({

          variable_map <- data_obj$get(field = "..variable_map")
          issues <- list()

          for (col in c("var_name", "denom_var", "disaggregation")) {
            if (!col %in% names(df_log)) next

            vars <- unique(trimws(unlist(
              strsplit(as.character(df_log[[col]]), "[,;]")
            )))
            vars <- vars[!is.na(vars) & nzchar(vars)]
            if (length(vars) == 0) next

            mapped <- vapply(
              vars,
              function(v) {
                if (is.null(variable_map) || !v %in% names(variable_map)) {
                  return(NA_character_)
                }
                as.character(variable_map[[v]])[1]
              },
              character(1)
            )

            unmapped <- vars[is.na(mapped)]
            if (length(unmapped) > 0) {
              issues[[paste0("unmapped_", col)]] <- unmapped
              phrutils::phr_warning(
                private$..log_name,
                phrutils::phr_txt(
                  "{col} values not mapped in variable map: {paste(unmapped, collapse=', ')}"
                )
              )
            }

            mapped_ok <- mapped[!is.na(mapped)]
            missing_in_data <- vars[!is.na(mapped)][!mapped_ok %in% names(df)]
            if (length(missing_in_data) > 0) {
              issues[[paste0("missing_in_data_", col)]] <- missing_in_data
              phrutils::phr_warning(
                private$..log_name,
                phrutils::phr_txt(
                  "{col} values whose mapped column is not in the dataset: {paste(missing_in_data, collapse=', ')}"
                )
              )
            }
          }

          issues
        },
        step = "Variable map and dataset checks - QuantDataAnalysisPlanLog$validate_dap_against_dataset",
        hint =  "Check that var_name, denom_var and disaggregation values are mapped in the variable map and that the mapped columns exist in the dataset."
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        result

      },
      on_error = "abort",
      origin = "QuantDataAnalysisPlanLog$validate_dap_against_dataset"
      )
    },

    #' Add Analysis Plan Entry
    #'
    #' @description
    #' Helper method to add an analysis plan entry with all required fields
    #'
    #' @param indicator_name Character; name/label of the indicator
    #' @param calculation Character; calculation type (prop, mean, median, ratio)
    #' @param var_name Character; primary variable name in the dataset
    #' @param denom_var Character; denominator variable (optional, for ratios)
    #' @param disaggregation Character; disaggregation variable(s) (optional)
    #' @param multiplier Numeric; multiplier for the calculation (default: 100)
    #' @param indicator_unit Character; unit of measurement (default: "\%")
    #'
    #' @return Invisible TRUE
    #'
    #' @details
    #' Validates inputs and calls append_entry() to add to log.
    add_indicator = function(indicator_name,
                             calculation,
                             var_name,
                             denom_var = NA_character_,
                             disaggregation = NA_character_,
                             multiplier = 100,
                             indicator_unit = "%") {

      entry <- list(
        indicator_name = as.character(indicator_name),
        calculation = as.character(calculation),
        var_name = as.character(var_name),
        denom_var = as.character(denom_var),
        disaggregation = as.character(disaggregation),
        multiplier = as.numeric(multiplier),
        indicator_unit = as.character(indicator_unit)
      )

      self$append_entry(entry)
    }
  ),

  private = list(

    #' Additional Accessible Fields Hook
    #'
    #' @description
    #' Returns subclass-specific fields accessible via `get()`.
    #'
    #' @return Character vector of field names.
    ..additional_call_fields = function() {
      c(
        super$..additional_call_fields(),
        "add_indicator"
      )
    },

    # Quant data analysis plan-log specific checks (indicator_name, calculation,
    # multiplier, indicator_unit) and state update. Dataset/variable-map checks
    # live in validate_dap_against_dataset(). Returns invisible issues list,
    # or a failed step result.
    ..post_validate = function() {

      df <- private$..log_df
      valid_calculations <- c(
        "prop", "mean", "median", "ratio", "cat", "categorical", "select_multiple_cat"
      )

      # indicator_name / indicator_unit: non-missing, valid character values
      result <- phrutils::phr_try_step({

        issues <- list()

        for (col in c("indicator_name", "indicator_unit")) {
          if (!col %in% names(df)) next
          x <- df[[col]]
          bad <- if (!is.character(x)) seq_along(x) else which(is.na(x) | trimws(x) == "")

          if (length(bad) > 0) {
            issues[[paste0("invalid_", col)]] <- bad
            phrutils::phr_warning(
              private$..log_name,
              phrutils::phr_txt(glue::glue("Analysis plan has missing/invalid {col} at rows: {paste(bad, collapse=', ')}."))
            )
          }
        }

        issues
      },
      step = "Indicator name/unit checks - QuantDataAnalysisPlanLog$..post_validate",
      hint = "Check that indicator_name and indicator_unit are non-missing character values."
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      issues <- result

      # calculation: must be one of the allowed values
      result <- phrutils::phr_try_step({

        if ("calculation" %in% names(df)) {
          calc <- as.character(df$calculation)
          bad <- which(is.na(calc) | !calc %in% valid_calculations)

          if (length(bad) > 0) {
            issues$invalid_calculation <- bad
            phrutils::phr_warning(
              private$..log_name,
              phrutils::phr_txt(glue::glue("Analysis plan has invalid calculation at rows: {paste(bad, collapse=', ')}. Allowed: {paste(valid_calculations, collapse=', ')}."))
            )
          }
        }

        issues
      },
      step = "Calculation checks - QuantDataAnalysisPlanLog$..post_validate",
      hint = "Check that calculation is one of the allowed values."
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      issues <- result

      # multiplier: numeric, non-missing and positive
      result <- phrutils::phr_try_step({

        if ("multiplier" %in% names(df)) {
          mult <- suppressWarnings(as.numeric(df$multiplier))
          invalid_mult <- which(is.na(mult) | mult <= 0)

          if (length(invalid_mult) > 0) {
            issues$invalid_multiplier <- invalid_mult

            phrutils::phr_warning(
              private$..log_name,
              phrutils::phr_txt(glue::glue("Analysis plan contains missing or non-positive multipliers at rows: {paste(invalid_mult, collapse=', ')}."))
            )
          }
        }

        issues

      },
      step = "Multiplier checks - QuantDataAnalysisPlanLog$..post_validate",
      hint = "Check that multiplier is a positive number."
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      issues <- result

      # 3. Dataset alignment + 4. merge issues and update state
      result <- phrutils::phr_try_step({

        existing_issues <- self$get(field = "..issues")

        all_issues <- issues

        if (!is.null(existing_issues) && length(existing_issues) > 0) {
          all_issues <- c(existing_issues, issues)
        }

        self$set(field = "..issues", value = all_issues)
        self$set(field = "..validated", value = length(all_issues) == 0)

      },
      step = "Setting issues and validation status - QuantDataAnalysisPlanLog$..post_validate",
      hint = "Issue merging with issues from previous validation steps."
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }



      invisible(private$..issues)

    }

  )
)

