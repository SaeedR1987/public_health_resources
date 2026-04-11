#' QuantDataAnalysis
#'
#' @description A class generator function for quantitative data analysis
#'
#' @noRd
QuantDataAnalysis <- R6::R6Class(
  classname = "QuantDataAnalysis",

  public = list(

    # 🧩 Fields

    data_analysis_plan = NULL,         # tibble of planned analyses
    survey_design       = NULL,        # survey design object
    results             = NULL,        # named list of completed results: survey_design, base
    analysis_plan_issue_log = NULL,    # tibble of plan validation issues

    # Fields from Data object
    parent_data_object = NULL,         # Reference to parent Data object
    dataset_name = NULL,               # Name for identification
    data = NULL,                       # Standardized or clean data from generating Data object
    data_stage_name = NULL,            # Name of the data stage (e.g. "standardized", "clean")
    data_hash = NULL,                  # Hash of the data
    variable_map = NULL,               # Variable mappings from Data object
    value_map = NULL,                  # Value mappings from Data object
    variable_label = NULL,             # Variable labels from Data object
    value_label = NULL,                # Value labels from Data object

    # Analysis schema - catalog of possible indicators (loaded from resource file)
    analysis_schema = NULL,

    # Outputs schema - catalog of visualizations and tables to create
    outputs_schema = NULL,

    # Output containers
    visualizations = NULL,             # List of ggplot2 graphics
    tables = NULL,                     # List of dataframes/formatted tables


    # ⚙️ Initialization

    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "QuantDataAnalysis",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL) {
      origin <- "QuantDataAnalysis$initialize"
      phr_message(origin, "Initializing quantitative analysis class...")

      self$parent_data_object <- parent_data_object
      self$dataset_name <- dataset_name
      self$data <- data
      self$data_stage_name <- data_stage_name
      self$data_hash <- data_hash
      self$variable_map <- variable_map %||% list()
      self$value_map <- value_map %||% list()
      self$variable_label <- variable_label %||% list()
      self$value_label <- value_label %||% list()

      self$results <- list()
      self$analysis_plan_issue_log <- tibble::tibble()
      self$visualizations <- list()
      self$tables <- list()

      # 1. Load analysis schema (master list of all possible indicators for this class)
      self$analysis_schema <- self$default_analysis_schema()

      # 2. Create survey design from the provided data and variable_map.
      if (!is.null(data)) {
        self$survey_design <- self$create_survey_design()
      }

      # 3. Populate data_analysis_plan.
      #    If a custom DAP is explicitly supplied, wrap it and use it as-is.
      #    Otherwise generate from analysis_schema, filtering to variables that
      #    are actually present in the dataset / survey design.
      if (!is.null(dap)) {
        self$data_analysis_plan <- QuantDataAnalysisPlanLog$new(
          log_df = dap,
          log_name = "Quant Data Analysis Plan"
        )
      } else {
        self$data_analysis_plan <- QuantDataAnalysisPlanLog$new(
          log_df = NULL,
          log_name = "Quant Data Analysis Plan"
        )
        if (!is.null(self$survey_design) || !is.null(self$data)) {
          self$generate_dap_from_schema()
        }
      }

      # 4. Load outputs schema
      self$outputs_schema <- self$default_outputs_schema()

      phr_message(origin, "Initialization complete.")
      invisible(self)
    },


    #' @description
    #' Load the default analysis schema from template file
    #'
    #' @return A tibble containing the analysis schema
    default_analysis_schema = function() {
      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_template.xlsx",
        package = "iphRa"
      )

      # If template file doesn't exist, try relative path (for development)
      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_template.xlsx")
        if (!file.exists(file)) {
          # Return default schema
          return(tibble::tibble(
            indicator_name = c(
              "Crude death rate",
              "Unmet health need",
              "Mean household size"
            ),
            calculation = c("ratio", "prop", "mean"),
            var_name = c("deaths", "unmet_health_need_yn", "hh_size"),
            denom_var = c("person_time", NA, NA),
            disaggregation = c("admin1", "admin1", NA),
            multiplier = c(10000, 100, 1),
            indicator_unit = c("deaths/10,000/day", "%", "persons")
          ))
        }
      }

      # Read the Excel template
      schema_tbl <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin  = "QuantDataAnalysis$default_analysis_schema",
        hint = "Check that analysis_schema_quant_data_analysis_template.xlsx is a valid Excel file."
      )

      # If reading failed or file is empty, return default schema
      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble(
          indicator_name = c(
            "Crude death rate",
            "Unmet health need",
            "Mean household size"
          ),
          calculation = c("ratio", "prop", "mean"),
          var_name = c("deaths", "unmet_health_need_yn", "hh_size"),
          denom_var = c("person_time", NA, NA),
          disaggregation = c("admin1", "admin1", NA),
          multiplier = c(10000, 100, 1),
          indicator_unit = c("deaths/10,000/day", "%", "persons")
        ))
      }

      return(schema_tbl)
    },


    #' @description
    #' Load the default outputs schema from template file
    #'
    #' @return A list containing the outputs schema
    default_outputs_schema = function() {
      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_template.xlsx",
        package = "iphRa"
      )

      # If template file doesn't exist, try relative path (for development)
      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_template.xlsx")
        if (!file.exists(file)) {
          return(list())  # Return empty list (no outputs)
        }
      }

      # Read the Excel template
      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin  = "QuantDataAnalysis$default_outputs_schema",
        hint = "Check that outputs_schema_data_analytics_template.xlsx is a valid Excel file."
      )

      # If reading failed or file is empty, return empty schema
      if (is.null(df) || nrow(df) == 0) {
        return(list())  # Return empty list (no outputs)
      }

      # Convert table to schema using outputs_table_to_schema
      outputs_schema <- outputs_table_to_schema(df)

      return(outputs_schema)
    },


    #' @description
    #' Create a survey design object from the stored data and variable_map.
    #' Checks variable_map for cluster_id_numeric (preferred), cluster_id (fallback),
    #' weight, and strata, using whichever are available.
    #' Subclasses may override this method to implement domain-specific design
    #' construction (e.g. individual-level designs).
    #'
    #' @return A \code{srvyr} survey design object, or \code{NULL} if the
    #'   required variables are not available.
    create_survey_design = function() {
      origin <- "QuantDataAnalysis$create_survey_design"

      if (!requireNamespace("srvyr", quietly = TRUE)) {
        phr_warning(origin, "Package 'srvyr' must be installed to create survey design objects.")
        return(NULL)
      }

      if (is.null(self$data)) {
        phr_warning(origin, "No data available to create survey design.")
        return(NULL)
      }

      data_cols <- names(self$data)

      # Prefer cluster_id_numeric; fall back to cluster_id
      cluster_col <- self$variable_map[["cluster_id_numeric"]]
      if (is.null(cluster_col) || !cluster_col %in% data_cols) {
        cluster_col <- self$variable_map[["cluster_id"]]
      }
      if (is.null(cluster_col) || !cluster_col %in% data_cols) {
        cluster_col <- NULL
      }

      weight_col <- self$variable_map[["weight"]]
      if (is.null(weight_col) || !weight_col %in% data_cols) weight_col <- NULL

      strata_col <- self$variable_map[["stratum"]]
      if (is.null(strata_col) || !strata_col %in% data_cols) strata_col <- NULL

      fpc_col <- self$variable_map[["fpc"]]
      if (is.null(fpc_col) || !fpc_col %in% data_cols) fpc_col <- NULL

      # Build the as_survey_design call with whichever variables are available
      if (is.null(cluster_col)) {
        phr_message(origin, "No cluster column found in variable_map; using ids = 1 (simple random sample design).")
      }

      # Pre-compute tidy-eval symbols before the srvyr call.
      # Using !!rlang::sym() inside an if() expression passed as a function
      # argument does not work correctly with rlang NSE (enquo captures the
      # unevaluated if-expression and !! is not processed as unquoting there).
      # Pre-computing symbols and then using !!sym directly is the safe pattern.
      ids_sym    <- if (!is.null(cluster_col)) rlang::sym(cluster_col) else 1
      strata_sym <- if (!is.null(strata_col))  rlang::sym(strata_col)  else NULL
      weight_sym <- if (!is.null(weight_col))  rlang::sym(weight_col)  else NULL
      fpc_sym    <- if (!is.null(fpc_col))     rlang::sym(fpc_col)     else NULL

      design <- phr_try(
        srvyr::as_survey_design(
          .data   = self$data,
          ids     = !!ids_sym,
          strata  = !!strata_sym,
          weights = !!weight_sym,
          fpc     = !!fpc_sym,
          nest    = TRUE
        ),
        on_error = "warn",
        origin = origin,
        hint = "Check that cluster_id_numeric (or cluster_id) and weight columns contain valid data."
      )

      if (!is.null(design)) {
        phr_message(origin, "Survey design created successfully.")
      }

      return(design)
    },


    #' @description
    #' Add an indicator to the data analysis plan
    #'
    #' Appends a new row to \code{self$data_analysis_plan$log_df} with the
    #' supplied indicator specification.
    #'
    #' @param indicator_name Character; human-readable label for the indicator.
    #' @param calculation Character; one of \code{"prop"}, \code{"mean"},
    #'   \code{"median"}, \code{"ratio"}, or \code{"categorical"}.
    #' @param var_name Character; dataset column name for the primary variable.
    #' @param denom_var Character or \code{NULL}; denominator column name
    #'   (required for \code{calculation = "ratio"}).
    #' @param disaggregation Character or \code{NULL}; column name to use for
    #'   stratified (disaggregated) analysis.
    #' @param multiplier Numeric; scale factor for the point estimate (default
    #'   \code{100} for percentages).
    #' @param indicator_unit Character; unit label (default \code{"\%"}).
    #' @param high_design_complexity Logical; reserved for future use.
    #' @param note Character or \code{NULL}; optional free-text note.
    #' @return Invisibly returns \code{self}.
    add_indicator_dap = function(indicator_name,
                             calculation,
                             var_name,
                             denom_var = NULL,
                             disaggregation = NULL,
                             multiplier = 100,
                             indicator_unit = "%",
                             high_design_complexity = FALSE,
                             note = NULL) {
      origin <- "QuantDataAnalysis$add_indicator_dap"

      phr_try({
        self$data_analysis_plan$add_indicator(
          indicator_name = indicator_name,
          calculation = calculation,
          var_name = var_name,
          denom_var = denom_var %||% NA_character_,
          disaggregation = disaggregation %||% NA_character_,
          multiplier = multiplier,
          indicator_unit = indicator_unit
        )

        phr_message(origin, paste("Added indicator:", indicator_name))
      },
      on_error = "warn",
      origin = origin,
      hint = "Check variable names and calculation type.")
    },


    #' @description
    #' Remove an indicator from the data analysis plan
    #'
    #' Deletes all rows where \code{indicator_name} matches the supplied value.
    #'
    #' @param indicator_name Character; label of the indicator to remove.
    #' @return Invisibly returns \code{self}.
    remove_indicator_dap = function(indicator_name) {
      origin <- "QuantDataAnalysis$remove_indicator_dap"

      phr_try({
        if (is.null(self$data_analysis_plan) || nrow(self$data_analysis_plan$log_df) == 0) {
          phr_warning(origin, "No data_analysis_plan loaded.")
          return(invisible(self))
        }

        self$data_analysis_plan$log_df <-
          self$data_analysis_plan$log_df[self$data_analysis_plan$log_df$indicator_name != indicator_name, ]

        phr_message(origin, paste("Removed indicator:", indicator_name))
      },
      on_error = "warn",
      origin = origin,
      hint = "Ensure indicator_name exists in data_analysis_plan.")
    },


    #' @description
    #' Convert the analysis schema tibble to a named list
    #'
    #' Each row of \code{self$analysis_schema} becomes a named element of the
    #' returned list, keyed by \code{indicator_name}.
    #'
    #' @return A named list of indicator specification lists, or an empty list
    #'   if the schema is empty.
    to_list_schema = function() {
      origin <- "QuantDataAnalysis$to_list_schema"
      phr_message(origin, "Converting indicator schema tibble to named list...")

      result <- phr_try({
        if (is.null(self$analysis_schema) || nrow(self$analysis_schema) == 0) {
          phr_warning(origin, "No analysis_schema loaded to convert.")
          return(list())
        }

        purrr::pmap(self$analysis_schema, function(...) list(...)) %>%
          purrr::set_names(self$analysis_schema$indicator_name)
      },
      on_error = "warn",
      origin = origin,
      hint = "Ensure analysis_schema is a valid tibble with 'indicator_name' column.")

      phr_message(origin, "Conversion complete.")
      return(result)
    },


    #' @description
    #' Generate the data analysis plan from the analysis schema
    #'
    #' Translates canonical variable names through \code{self$variable_map},
    #' checks that the resulting variable names exist in the survey design (or
    #' data), and populates \code{self$data_analysis_plan$log_df} with only the
    #' rows whose variables are available.  Indicators with missing variables are
    #' recorded in \code{self$analysis_plan_issue_log}.
    #'
    #' @return Invisibly returns \code{self}.
    generate_dap_from_schema = function() {
      origin <- "QuantDataAnalysis$generate_dap_from_schema"
      phr_message(origin, "Generating data_analysis_plan from schema...")

      phr_try({
        # Determine the column names available in the dataset.
        # Prefer the survey design variables; fall back to data column names.
        if (!is.null(self$survey_design)) {
          available_vars <- names(self$survey_design$variables)
        } else if (!is.null(self$data)) {
          available_vars <- names(self$data)
        } else {
          phr_error(origin, "Neither survey_design nor data is available.")
          return(invisible(self))
        }

        # Helper function to translate canonical name to actual dataset name
        translate_var <- function(canonical_name) {
          if (is.null(canonical_name) || is.na(canonical_name)) return(NA_character_)

          # Check if canonical name is in variable_map
          if (!is.null(self$variable_map) && canonical_name %in% names(self$variable_map)) {
            actual_name <- self$variable_map[[canonical_name]]
            if (!is.null(actual_name) && actual_name != "") {
              return(actual_name)
            }
          }

          # If not found in map, return canonical name (assume it's already the actual name)
          return(canonical_name)
        }

        # Guard: nothing to generate if the analysis schema is empty
        if (is.null(self$analysis_schema) || nrow(self$analysis_schema) == 0) {
          phr_warning(origin, "analysis_schema is empty; data_analysis_plan will remain empty.")
          return(invisible(self))
        }

        # Translate schema variables and filter by available variables
        schema_valid <- self$analysis_schema %>%
          dplyr::mutate(
            # Translate canonical names to actual dataset names
            var_name_actual = purrr::map_chr(.data$var_name, translate_var),
            denom_var_actual = purrr::map_chr(.data$denom_var, translate_var),
            # Check if translated names exist in survey design
            var_exists = .data$var_name_actual %in% available_vars,
            denom_exists = ifelse(!is.na(.data$denom_var_actual),
                                  .data$denom_var_actual %in% available_vars,
                                  TRUE)
          ) %>%
          dplyr::mutate(
            include = .data$var_exists & .data$denom_exists
          )

        # Store missing variables in issue log
        issues <- schema_valid %>%
          dplyr::filter(!.data$include) %>%
          dplyr::transmute(
            indicator_name = .data$indicator_name,
            issue = paste0(
              "Missing variable(s): ",
              ifelse(!.data$var_exists,
                     paste0(.data$var_name, " (maps to: ", .data$var_name_actual, ")"),
                     ""),
              ifelse(!.data$denom_exists & !is.na(.data$denom_var),
                     paste0(", ", .data$denom_var, " (maps to: ", .data$denom_var_actual, ")"),
                     "")
            )
          )

        # Assign valid indicators as new DAP with actual variable names
        dap_df <- schema_valid %>%
          dplyr::filter(.data$include) %>%
          dplyr::transmute(
            indicator_name = .data$indicator_name,
            calculation = .data$calculation,
            var_name = .data$var_name_actual,  # Use translated actual name
            denom_var = .data$denom_var_actual,  # Use translated actual name
            disaggregation = .data$disaggregation,
            multiplier = .data$multiplier,
            indicator_unit = .data$indicator_unit
          )

        # Update the log_df in the data_analysis_plan
        self$data_analysis_plan$log_df <- dap_df

        # Append to issue log if any problems
        if (nrow(issues) > 0) {
          self$analysis_plan_issue_log <- dplyr::bind_rows(
            self$analysis_plan_issue_log,
            issues
          )
          phr_warning(origin, paste0(nrow(issues), " indicators skipped due to missing variables."))
        } else {
          phr_message(origin, "All schema indicators found and added to data_analysis_plan.")
        }
      },
      on_error = "warn",
      origin = origin,
      hint = "Ensure survey_design$variables is accessible and schema structure is valid.")

      invisible(self)
    },

    #' @description
    #' Validate the analysis schema
    #'
    #' Checks that \code{self$analysis_schema} contains the required columns and
    #' that all referenced variables exist in the survey design.  Issues are
    #' stored in \code{self$analysis_plan_issue_log}.
    #'
    #' @return \code{TRUE} if validation passes, \code{FALSE} otherwise.
    validate_schema = function() {
      origin <- "QuantDataAnalysis$validate_schema"
      phr_message(origin, "Validating indicator schema...")

      issues <- tibble::tibble(
        indicator_name = character(),
        issue = character()
      )

      phr_try({


        # 1. Check schema exists

        if (is.null(self$analysis_schema) || nrow(self$analysis_schema) == 0) {
          issues <- dplyr::bind_rows(issues, tibble::tibble(
            indicator_name = NA_character_,
            issue = "Indicator schema is empty."
          ))
          self$analysis_plan_issue_log <- issues
          phr_warning(origin, "Schema validation FAILED: schema empty.")
          return(invisible(FALSE))
        }


        # 2. Required columns

        required_cols <- c(
          "indicator_name",
          "calculation",
          "var_name",
          "denom_var",
          "disaggregation",
          "multiplier",
          "indicator_unit"
        )

        missing_cols <- setdiff(required_cols, names(self$analysis_schema))
        if (length(missing_cols) > 0) {
          issues <- dplyr::bind_rows(issues, tibble::tibble(
            indicator_name = NA_character_,
            issue = paste("Schema missing required columns:",
                          paste(missing_cols, collapse = ", "))
          ))
        }


        # 3. Check survey design exists

        if (is.null(self$survey_design)) {
          issues <- dplyr::bind_rows(issues, tibble::tibble(
            indicator_name = NA_character_,
            issue = "Survey design not loaded. Cannot validate variables."
          ))
        } else {

          available_vars <- names(self$survey_design$variables)


          # 4. Validate variable existence

          var_issues <- self$analysis_schema %>%
            dplyr::mutate(
              var_exists = .data$var_name %in% available_vars,
              denom_exists = ifelse(
                !is.na(.data$denom_var) & .data$denom_var != "",
                .data$denom_var %in% available_vars,
                TRUE
              )
            ) %>%
            dplyr::filter(!.data$var_exists | !.data$denom_exists)

          if (nrow(var_issues) > 0) {
            issues <- dplyr::bind_rows(
              issues,
              var_issues %>%
                dplyr::transmute(
                  indicator_name,
                  issue = paste0(
                    "Missing variable(s): ",
                    ifelse(!var_exists, var_name, ""),
                    ifelse(!denom_exists & !is.na(denom_var),
                           paste0(", ", denom_var), "")
                  )
                )
            )
          }
        }

      },
      on_error = "warn",
      origin = origin,
      hint = "Ensure schema uses standard DAP fields and variables exist in dataset."
      )

      # Save issues
      self$analysis_plan_issue_log <- issues


      # Return FALSE if any issues

      if (nrow(issues) > 0) {
        phr_warning(origin,
                      paste("Schema validation FAILED with",
                            nrow(issues),
                            "issue(s)."))
        return(FALSE)
      }

      phr_message(origin, "Schema validation PASSED.")
      return(TRUE)
    },


    #' @description
    #' Validate the data analysis plan
    #'
    #' Checks that \code{self$data_analysis_plan$log_df} is non-empty, contains
    #' the required columns, and uses only valid \code{calculation} types.
    #' Issues are stored in \code{self$analysis_plan_issue_log}.
    #'
    #' @return Invisibly returns \code{self}.
    validate_plan = function() {
      origin <- "QuantDataAnalysis$validate_plan"
      phr_message(origin, "Validating analysis plan...")

      issues <- tibble::tibble(indicator_name = character(), issue = character())

      phr_try({
        if (is.null(self$data_analysis_plan) || nrow(self$data_analysis_plan$log_df) == 0) {
          issues <- dplyr::bind_rows(issues, tibble::tibble(
            indicator_name = NA_character_,
            issue = "Analysis plan is empty."
          ))
        } else {
          required_cols <- c(
            "indicator_name",
            "calculation",
            "var_name",
            "denom_var",
            "disaggregation",
            "multiplier",
            "indicator_unit"
          )
          missing_cols <- setdiff(required_cols, names(self$data_analysis_plan$log_df))

          if (length(missing_cols) > 0) {
            issues <- dplyr::bind_rows(issues, tibble::tibble(
              indicator_name = NA_character_,
              issue = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
            ))
          }

          invalid_calc <- self$data_analysis_plan$log_df %>%
            dplyr::filter(!.data$calculation %in% c("prop", "mean", "median", "ratio"))

          if (nrow(invalid_calc) > 0) {
            issues <- dplyr::bind_rows(issues, tibble::tibble(
              indicator_name = invalid_calc$indicator_name,
              issue = paste("Invalid calculation type:", invalid_calc$calculation)
            ))
          }
        }
      },
      on_error = "warn",
      origin = origin,
      hint = "Check data_analysis_plan structure and field names.")

      self$analysis_plan_issue_log <- issues
      if (nrow(issues) > 0) {
        phr_warning(origin, paste("Validation found", nrow(issues), "issue(s)."))
      } else {
        phr_message(origin, "Analysis plan validation passed with no issues.")
      }
      invisible(self)
    },


    #' @description
    #' Run the data analysis plan and store two sets of results
    #'
    #' Executes every indicator defined in \code{self$data_analysis_plan} twice:
    #' once using the full survey design (accounting for cluster, strata,
    #' weights, and FPC) and once using a simple unweighted design
    #' (\code{ids = 1}, no weights).  Both result sets are stored in
    #' \code{self$results} as a named list:
    #' \describe{
    #'   \item{\code{survey_design}}{Tibble of survey-weighted results.}
    #'   \item{\code{base}}{Tibble of simple unweighted results.}
    #' }
    #'
    #' @return Invisibly returns \code{self}.
    run_analysis = function() {
      origin <- "QuantDataAnalysis$run_analysis"
      phr_message(origin, "Running analysis plan...")

      if (is.null(self$survey_design)) {
        phr_error(origin, "Survey design not set.")
        return(invisible(self))
      }

      if (is.null(self$data_analysis_plan) || nrow(self$data_analysis_plan$log_df) == 0) {
        phr_error(origin, "No data_analysis_plan provided.")
        return(invisible(self))
      }

      # --- Survey-design results (weighted) ----------------------------------
      survey_design_results <- phr_try(
        phr_calc_survey_from_plan(
          design = self$survey_design,
          analysis_plan = self$data_analysis_plan$log_df
        ),
        on_error = "warn",
        origin = origin,
        hint = "Verify all variables exist and analysis plan is valid."
      )

      # --- Base results (simple unweighted design) ---------------------------
      base_results <- NULL
      if (!is.null(self$data)) {
        base_design <- phr_try(
          srvyr::as_survey_design(.data = self$data, ids = 1),
          on_error = "warn",
          origin = origin,
          hint = "Could not create base (unweighted) survey design from self$data."
        )

        if (!is.null(base_design)) {
          base_results <- phr_try(
            phr_calc_survey_from_plan(
              design = base_design,
              analysis_plan = self$data_analysis_plan$log_df
            ),
            on_error = "warn",
            origin = origin,
            hint = "Verify all variables exist in the base (unweighted) design."
          )
        }
      }

      self$results <- list(
        survey_design = survey_design_results,
        base          = base_results
      )

      phr_message(origin, "Analysis completed successfully.")
      invisible(self)
    },


    #' @description
    #' Retrieve analysis results
    #'
    #' Returns the \code{self$results} list produced by
    #' \code{\link{run_analysis}}.  The list contains two elements:
    #' \describe{
    #'   \item{\code{survey_design}}{Survey-weighted results tibble.}
    #'   \item{\code{base}}{Simple unweighted results tibble.}
    #' }
    #'
    #' @return A named list with elements \code{survey_design} and \code{base},
    #'   or \code{NULL} if \code{run_analysis()} has not been called yet.
    get_results = function() {
      origin <- "QuantDataAnalysis$get_results"
      phr_message(origin, "Retrieving results...")

      if (is.null(self$results) || length(self$results) == 0) {
        phr_warning(origin, "No results available yet. Run run_analysis() first.")
      }
      return(self$results)
    },


    #' @description
    #' Export analysis results to a file
    #'
    #' Exports both the \code{survey_design} and \code{base} result sets
    #' produced by \code{\link{run_analysis}}.  When \code{format = "xlsx"},
    #' each result set is written to its own sheet
    #' (\code{"survey_design"} and \code{"base"}).  When
    #' \code{format = "csv"}, only the \code{survey_design} results are
    #' written; to export the base results supply a separate path.
    #'
    #' @param path Character; file path for the output file.
    #' @param format Character; \code{"xlsx"} (default) or \code{"csv"}.
    #' @return Invisibly returns \code{self}.
    export_results = function(path, format = "xlsx") {
      origin <- "QuantDataAnalysis$export_results"

      phr_try({
        if (is.null(self$results) || length(self$results) == 0) {
          phr_warning(origin, "No results to export.")
          return(invisible(self))
        }

        phr_message(origin, paste("Exporting results to:", path))

        if (format == "xlsx") {
          sheets <- list()
          if (!is.null(self$results$survey_design)) {
            sheets[["survey_design"]] <- self$results$survey_design
          }
          if (!is.null(self$results$base)) {
            sheets[["base"]] <- self$results$base
          }
          openxlsx::write.xlsx(sheets, path)
        } else if (format == "csv") {
          tbl <- self$results$survey_design %||% tibble::tibble()
          readr::write_csv(tbl, path)
        } else {
          phr_warning(origin, paste("Unsupported export format:", format))
        }
      },
      on_error = "warn",
      origin = origin,
      hint = "Check file path and permissions.")

      invisible(self)
    },

    #' @description
    #' Export the internal state as an R list for session serialisation
    #'
    #' Captures all key fields (\code{data_analysis_plan}, \code{results},
    #' \code{analysis_plan_issue_log}, \code{analysis_schema},
    #' \code{outputs_schema}, \code{visualizations}, \code{tables}, and
    #' \code{survey_design}) into a plain R list that can be saved and later
    #' restored with \code{\link{load_state_object}}.
    #'
    #' @return A named list of internal state fields.
    export_state_object = function() {
      list(
        data_analysis_plan = self$data_analysis_plan,
        results = self$results,
        analysis_plan_issue_log = self$analysis_plan_issue_log,
        analysis_schema = self$analysis_schema,
        outputs_schema = self$outputs_schema,
        visualizations = self$visualizations,
        tables = self$tables,
        survey_design = self$survey_design
      )
    },

    #' @description
    #' Restore internal state from a previously exported state list
    #'
    #' Re-populates all key fields from a list produced by
    #' \code{\link{export_state_object}}.
    #'
    #' @param state A named list as returned by \code{\link{export_state_object}}.
    #' @return Invisibly returns \code{self}.
    load_state_object = function(state) {
      self$data_analysis_plan      <- state$data_analysis_plan
      self$results                 <- state$results
      self$analysis_plan_issue_log <- state$analysis_plan_issue_log
      self$analysis_schema        <- state$analysis_schema
      self$outputs_schema         <- state$outputs_schema
      self$visualizations         <- state$visualizations
      self$tables                 <- state$tables

      if (!is.null(state$survey_design))
        self$survey_design <- state$survey_design

      invisible(self)
    },

    # 📊 Outputs Schema Management

    #' @description
    #' Export Outputs Schema to Table
    #'
    #' Exports the current outputs schema as a data frame table.
    #'
    #' @return A tibble representing the outputs schema, or \code{NULL} if no
    #'   schema is defined.
    export_outputs_schema = function() {
      origin <- "QuantDataAnalysis$export_outputs_schema"

      phr_try({
        if (is.null(self$outputs_schema) || length(self$outputs_schema) == 0) {
          phr_warning(origin, "No outputs schema defined.")
          return(NULL)
        }

        outputs_schema_to_table(self$outputs_schema)
      },
      on_error = "abort",
      origin = origin)
    },

    #' @description
    #' Import Outputs Schema from a Table
    #'
    #' Converts a data frame representation of the outputs schema back into the
    #' nested list structure and stores it in \code{self$outputs_schema}.
    #'
    #' @param df A data frame with the outputs schema columns.
    #' @return Invisibly returns the imported schema list.
    import_outputs_schema = function(df) {
      origin <- "QuantDataAnalysis$import_outputs_schema"

      phr_try({
        phr_validate_dataframe(df, origin = origin, soft = FALSE)

        new_schema <- outputs_table_to_schema(df)
        self$outputs_schema <- new_schema

        phr_message(origin, paste("Outputs schema imported successfully with", length(new_schema), "output(s)."))

        invisible(new_schema)
      },
      on_error = "abort",
      origin = origin)
    },

    # 📋 Analysis Schema Import / Export

    #' @description
    #' Export the analysis schema to a file
    #'
    #' Writes \code{self$analysis_schema} to an Excel (.xlsx) or CSV file.
    #'
    #' @param path Character; destination file path (including extension).
    #' @param format Character; \code{"xlsx"} (default) or \code{"csv"}.
    #' @return Invisibly returns \code{self}.
    export_analysis_schema = function(path, format = "xlsx") {
      origin <- "QuantDataAnalysis$export_analysis_schema"

      phr_try({
        if (is.null(self$analysis_schema) || nrow(self$analysis_schema) == 0) {
          phr_warning(origin, "No analysis schema to export.")
          return(invisible(self))
        }

        phr_message(origin, paste("Exporting analysis schema to:", path))

        if (format == "xlsx") {
          openxlsx::write.xlsx(self$analysis_schema, path)
        } else if (format == "csv") {
          readr::write_csv(self$analysis_schema, path)
        } else {
          phr_warning(origin, paste("Unsupported export format:", format))
        }
      },
      on_error = "warn",
      origin = origin,
      hint = "Check file path and permissions.")

      invisible(self)
    },

    #' @description
    #' Import an analysis schema from a file
    #'
    #' Reads an Excel (.xlsx), CSV, or RDS file and stores the result in
    #' \code{self$analysis_schema}.  The file must contain at minimum the
    #' columns \code{indicator_name}, \code{calculation}, \code{var_name},
    #' \code{denom_var}, \code{disaggregation}, \code{multiplier}, and
    #' \code{indicator_unit}.
    #'
    #' @param path Character; path to the schema file (.xlsx, .csv, or .rds).
    #' @return Invisibly returns \code{self}.
    import_analysis_schema = function(path) {
      origin <- "QuantDataAnalysis$import_analysis_schema"
      phr_message(origin, paste("Importing analysis schema from:", path))

      phr_try({
        if (!file.exists(path)) {
          phr_error(origin, paste("File not found:", path))
          return(invisible(self))
        }

        ext <- tools::file_ext(path)

        schema_tbl <- switch(
          ext,
          "csv"  = readr::read_csv(path, show_col_types = FALSE),
          "xlsx" = readxl::read_xlsx(path),
          "rds"  = readRDS(path),
          {
            phr_error(origin, paste("Unsupported file type:", ext))
            return(invisible(self))
          }
        )

        required_cols <- c(
          "indicator_name", "calculation", "var_name",
          "denom_var", "disaggregation", "multiplier", "indicator_unit"
        )
        missing_cols <- setdiff(required_cols, names(schema_tbl))
        if (length(missing_cols) > 0) {
          phr_warning(origin, paste("Imported schema missing columns:", paste(missing_cols, collapse = ", ")))
        }

        self$analysis_schema <- schema_tbl

        phr_message(origin, paste("Analysis schema imported successfully with", nrow(schema_tbl), "row(s)."))
      },
      on_error = "warn",
      origin = origin,
      hint = "Ensure the file is a valid xlsx, csv, or rds with the required schema columns.")

      invisible(self)
    },

    #' @description
    #' Run all outputs defined in the outputs schema
    #'
    #' Iterates through the outputs schema, resolves \code{@variable_map},
    #' \code{@value_map}, \code{@variable_label}, and \code{@value_label}
    #' references in \code{test_params}, calls the specified output function,
    #' and stores results in \code{self$visualizations}
    #' (for \code{output_type = "visualization"}) or \code{self$tables}
    #' (for \code{output_type = "table"}) using \code{output_title}
    #' (or \code{output_name}) as the label/key.
    #'
    #' The survey design object (\code{self$survey_design}) is passed as the
    #' first positional argument to output functions, allowing table and plot
    #' functions to accept either a srvyr design object or a regular data frame.
    #'
    #' @return Invisibly returns a list with \code{visualizations} and \code{tables}
    run_outputs = function() {

      phr_try({

        if (is.null(self$outputs_schema) || length(self$outputs_schema) == 0) {
          phr_warning(
            message = "No outputs defined in outputs schema.",
            origin = self$dataset_name
          )
          return(invisible(list(visualizations = self$visualizations, tables = self$tables)))
        }

        phr_message(phr_txt(glue::glue("Running {length(self$outputs_schema)} output(s) for {self$dataset_name}...")))

        for (out_name in names(self$outputs_schema)) {

          out <- self$outputs_schema[[out_name]]

          phr_try({

            # Get function name
            func_name <- out$output_func_name

            if (is.null(func_name) || is.na(func_name) || !nzchar(func_name)) {
              phr_warning(
                message = phr_txt(glue::glue("Output '{out_name}' has no output_func_name specified. Skipping.")),
                origin = self$dataset_name
              )
              next
            }

            # Resolve function from package namespace or calling environment
            output_function <- NULL

            if (requireNamespace("iphRa", quietly = TRUE)) {
              tryCatch({
                ns <- asNamespace("iphRa")
                if (exists(func_name, envir = ns, mode = "function", inherits = FALSE)) {
                  output_function <- get(func_name, envir = ns, mode = "function", inherits = FALSE)
                }
              }, error = function(e) NULL)
            }

            if (is.null(output_function)) {
              tryCatch({
                if (exists(func_name, mode = "function", inherits = TRUE)) {
                  output_function <- get(func_name, mode = "function", inherits = TRUE)
                }
              }, error = function(e) NULL)
            }

            if (is.null(output_function)) {
              phr_warning(
                message = phr_txt(glue::glue("Function '{func_name}' for output '{out_name}' not found. Skipping.")),
                origin = self$dataset_name
              )
              next
            }

            # Build function arguments. For QuantDataAnalysis, the survey design
            # object is passed as the first positional argument so that table and
            # plot functions that accept either a srvyr design object or a plain
            # data frame receive the weighted design by default.
            func_args <- list(self$survey_design)

            if (!is.null(out$test_params) && length(out$test_params) > 0) {
              for (arg_name in names(out$test_params)) {
                arg_value <- out$test_params[[arg_name]]

                # Check if argument is a vector in c(...) format
                if (is.character(arg_value) && grepl("^c\\(", arg_value)) {
                  vec_content <- sub("^c\\(", "", arg_value)
                  vec_content <- sub("\\)$", "", vec_content)

                  vec_elements <- .parse_indicator_arguments(vec_content)

                  resolved_elements <- character()
                  for (elem in vec_elements) {
                    elem <- trimws(elem)

                    if (grepl("^@variable_map\\$", elem)) {
                      role <- sub("^@variable_map\\$", "", elem)
                      resolved <- self$variable_map[[role]]
                      if (!is.null(resolved)) {
                        resolved_elements <- c(resolved_elements, resolved)
                      } else {
                        phr_warning(
                          message = phr_txt(glue::glue("Variable map role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                          origin = self$dataset_name
                        )
                      }
                    } else if (grepl("^@value_map\\$", elem)) {
                      parts <- strsplit(sub("^@value_map\\$", "", elem), "\\$")[[1]]
                      if (length(parts) >= 1) {
                        role <- parts[1]
                        if (!is.null(self$value_map[[role]])) {
                          if (length(parts) == 2) {
                            canonical_val <- parts[2]
                            resolved <- self$value_map[[role]][[canonical_val]]
                            if (!is.null(resolved)) {
                              resolved_elements <- c(resolved_elements, resolved)
                            } else {
                              phr_warning(
                                message = phr_txt(glue::glue("Value map '{elem}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                                origin = self$dataset_name
                              )
                            }
                          } else {
                            resolved <- unlist(self$value_map[[role]], use.names = FALSE)
                            resolved_elements <- c(resolved_elements, resolved)
                          }
                        } else {
                          phr_warning(
                            message = phr_txt(glue::glue("Value map role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                            origin = self$dataset_name
                          )
                        }
                      }
                    } else if (grepl("^@variable_label\\$", elem)) {
                      role     <- sub("^@variable_label\\$", "", elem)
                      resolved <- self$variable_label[[role]]
                      if (!is.null(resolved)) {
                        resolved_elements <- c(resolved_elements, resolved)
                      } else {
                        phr_warning(
                          message = phr_txt(glue::glue("Variable label role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                          origin = self$dataset_name
                        )
                      }
                    } else if (grepl("^@value_label\\$", elem)) {
                      parts <- strsplit(sub("^@value_label\\$", "", elem), "\\$")[[1]]
                      if (length(parts) >= 1) {
                        role <- parts[1]
                        if (!is.null(self$value_label[[role]])) {
                          if (length(parts) == 2) {
                            resolved <- self$value_label[[role]][[parts[2]]]
                            if (!is.null(resolved)) {
                              resolved_elements <- c(resolved_elements, resolved)
                            } else {
                              phr_warning(
                                message = phr_txt(glue::glue("Value label '{elem}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                                origin = self$dataset_name
                              )
                            }
                          } else {
                            resolved_elements <- c(resolved_elements, unlist(self$value_label[[role]], use.names = FALSE))
                          }
                        } else {
                          phr_warning(
                            message = phr_txt(glue::glue("Value label role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'.")),
                            origin = self$dataset_name
                          )
                        }
                      }
                    } else {
                      elem <- gsub("^['\"]|['\"]$", "", elem)
                      resolved_elements <- c(resolved_elements, elem)
                    }
                  }

                  func_args[[arg_name]] <- resolved_elements
                }
                # Resolve @variable_label references
                else if (is.character(arg_value) && grepl("^@variable_label\\$", arg_value)) {
                  role <- sub("^@variable_label\\$", "", arg_value)
                  resolved <- self$variable_label[[role]]
                  if (!is.null(resolved)) {
                    func_args[[arg_name]] <- resolved
                  } else {
                    phr_warning(
                      message = phr_txt(glue::glue("Variable label role '{role}' not found for output '{out_name}'. Skipping argument '{arg_name}'.")),
                      origin = self$dataset_name
                    )
                  }
                }
                # Resolve @variable_map references
                else if (is.character(arg_value) && grepl("^@variable_map\\$", arg_value)) {
                  role <- sub("^@variable_map\\$", "", arg_value)
                  resolved <- self$variable_map[[role]]
                  if (!is.null(resolved)) {
                    func_args[[arg_name]] <- resolved
                  } else {
                    phr_warning(
                      message = phr_txt(glue::glue("Variable map role '{role}' not found for output '{out_name}'. Skipping argument '{arg_name}'.")),
                      origin = self$dataset_name
                    )
                  }
                }
                # Resolve @value_label references
                else if (is.character(arg_value) && grepl("^@value_label\\$", arg_value)) {
                  parts <- strsplit(sub("^@value_label\\$", "", arg_value), "\\$")[[1]]
                  if (length(parts) >= 1) {
                    role <- parts[1]
                    if (!is.null(self$value_label[[role]])) {
                      if (length(parts) == 2) {
                        canonical_val <- parts[2]
                        resolved <- self$value_label[[role]][[canonical_val]]
                      } else {
                        resolved <- self$value_label[[role]]
                      }
                      if (!is.null(resolved)) {
                        func_args[[arg_name]] <- resolved
                      } else {
                        phr_warning(
                          message = phr_txt(glue::glue("Value label '{arg_value}' not found for output '{out_name}'. Using original value.")),
                          origin = self$dataset_name
                        )
                        func_args[[arg_name]] <- arg_value
                      }
                    } else {
                      phr_warning(
                        message = phr_txt(glue::glue("Value label role '{role}' not found for output '{out_name}'. Using original value.")),
                        origin = self$dataset_name
                      )
                      func_args[[arg_name]] <- arg_value
                    }
                  }
                }
                # Resolve @value_map references
                else if (is.character(arg_value) && grepl("^@value_map\\$", arg_value)) {
                  parts <- strsplit(sub("^@value_map\\$", "", arg_value), "\\$")[[1]]
                  if (length(parts) >= 1) {
                    role <- parts[1]
                    if (!is.null(self$value_map[[role]])) {
                      if (length(parts) == 2) {
                        canonical_val <- parts[2]
                        resolved <- self$value_map[[role]][[canonical_val]]
                      } else {
                        resolved <- self$value_map[[role]]
                      }
                      if (!is.null(resolved)) {
                        func_args[[arg_name]] <- resolved
                      } else {
                        phr_warning(
                          message = phr_txt(glue::glue("Value map '{arg_value}' not found for output '{out_name}'. Using original value.")),
                          origin = self$dataset_name
                        )
                        func_args[[arg_name]] <- arg_value
                      }
                    } else {
                      phr_warning(
                        message = phr_txt(glue::glue("Value map role '{role}' not found for output '{out_name}'. Using original value.")),
                        origin = self$dataset_name
                      )
                      func_args[[arg_name]] <- arg_value
                    }
                  }
                }
                else {
                  # Literal value - strip surrounding quotes if present
                  if (is.character(arg_value)) {
                    arg_value <- gsub("^['\"]|['\"]$", "", arg_value)
                  }
                  func_args[[arg_name]] <- arg_value
                }
              }
            }

            # Use output_title as the label, fall back to output_name
            label <- if (!is.null(out$output_title) && !is.na(out$output_title) && nzchar(out$output_title)) {
              out$output_title
            } else {
              out_name
            }

            # Determine outputs_group (optional nested storage level)
            group <- if (!is.null(out$outputs_group) && !is.na(out$outputs_group) && nzchar(out$outputs_group)) {
              out$outputs_group
            } else NULL

            # Helper to store a single result under the correct location
            store_result <- function(result, key) {
              if (!is.null(out$output_type) && out$output_type == "table") {
                if (!is.null(group)) {
                  if (is.null(self$tables[[group]])) self$tables[[group]] <- list()
                  self$tables[[group]][[key]] <- result
                } else {
                  self$tables[[key]] <- result
                }
                phr_message(phr_txt(glue::glue("Table '{key}' stored successfully.")))
              } else if (!is.null(out$output_type) && out$output_type == "visualization") {
                if (!is.null(group)) {
                  if (is.null(self$visualizations[[group]])) self$visualizations[[group]] <- list()
                  self$visualizations[[group]][[key]] <- result
                } else {
                  self$visualizations[[key]] <- result
                }
                phr_message(phr_txt(glue::glue("Visualization '{key}' stored successfully.")))
              } else {
                phr_warning(
                  message = phr_txt(glue::glue(
                    "Output '{out_name}' has unrecognized output_type '{out$output_type}'. ",
                    "Expected 'visualization' or 'table'. Result not stored."
                  )),
                  origin = self$dataset_name
                )
              }
            }

            # Check whether to iterate over per-group values
            per_group_ref <- if (!is.null(out$outputs_per_group) &&
                                 !is.na(out$outputs_per_group) &&
                                 nzchar(out$outputs_per_group)) out$outputs_per_group else NULL

            if (!is.null(per_group_ref)) {

              # Resolve the column name from variable_map if @-syntax is used
              if (grepl("^@variable_map\\$", per_group_ref)) {
                role <- sub("^@variable_map\\$", "", per_group_ref)
                col_name <- self$variable_map[[role]]
                var_label <- role
                if (is.null(col_name)) {
                  phr_warning(
                    message = phr_txt(glue::glue("outputs_per_group role '{role}' not found in variable_map for output '{out_name}'. Skipping per-group iteration.")),
                    origin = self$dataset_name
                  )
                  col_name <- NULL
                }
              } else {
                col_name <- per_group_ref
                var_label <- per_group_ref
              }

              # Retrieve underlying data from the survey design to get unique values
              design_data <- if (!is.null(self$survey_design)) {
                tryCatch(self$survey_design$variables, error = function(e) NULL)
              } else NULL

              if (!is.null(col_name) && !is.null(design_data) && col_name %in% names(design_data)) {
                unique_vals <- unique(design_data[[col_name]])
                unique_vals <- unique_vals[!is.na(unique_vals)]

                phr_message(phr_txt(glue::glue("Calling {func_name} for output '{out_name}' across {length(unique_vals)} group(s) of '{var_label}'...")))

                for (val in unique_vals) {
                  filtered_design <- tryCatch(
                    dplyr::filter(self$survey_design, !!rlang::sym(col_name) == val),
                    error = function(e) {
                      phr_warning(
                        message = phr_txt(glue::glue("Failed to filter survey design for '{var_label}' == '{val}': {e$message}")),
                        origin = self$dataset_name
                      )
                      NULL
                    }
                  )

                  if (is.null(filtered_design)) next

                  per_group_args <- func_args
                  per_group_args[[1]] <- filtered_design

                  amended_label <- paste0(label, "-", var_label, ".", val)

                  phr_try({
                    output_result <- do.call(output_function, per_group_args)
                    store_result(output_result, amended_label)
                  }, on_error = "warn", origin = paste0(self$dataset_name, "$run_outputs$", out_name, "$", val))
                }

              } else if (!is.null(col_name)) {
                phr_warning(
                  message = phr_txt(glue::glue("Column '{col_name}' for outputs_per_group not found in survey design for output '{out_name}'. Skipping per-group iteration.")),
                  origin = self$dataset_name
                )
              }

            } else {
              # Standard single output
              phr_message(phr_txt(glue::glue("Calling {func_name} for output '{out_name}'...")))
              output_result <- do.call(output_function, func_args)
              store_result(output_result, label)
            }

          }, on_error = "warn", origin = paste0(self$dataset_name, "$run_outputs$", out_name))
        }

        phr_message(
          phr_txt(glue::glue(
            "run_outputs complete: {length(self$visualizations)} visualization(s), {length(self$tables)} table(s)."
          ))
        )

        invisible(list(visualizations = self$visualizations, tables = self$tables))

      }, on_error = "warn", origin = paste0(self$dataset_name, "$run_outputs"))
    }
  )
)
