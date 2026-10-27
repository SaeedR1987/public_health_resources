#' IPHRA DataAnalytics Class
#'
#' The `DataAnalytics` R6 class provides unified data quality checks and
#' quantitative analysis in a single analytics object. It maintains separate
#' schemas for quality checks and quantitative analysis, while sharing common
#' output containers.
#'
#' @description
#' This class provides:
#' * Data quality checks via `run_quality_checks()` using `quality_schema`
#' * Quantitative analysis via `run_analysis()` using `analysis_schema`
#' * All outputs (quality and analysis) via `run_outputs()` using `outputs_schema`
#' * Shared `visualizations` and `tables` containers for all outputs
#'
#' @details
#' Key design principles:
#' * `quality_schema` drives quality checks; `analysis_schema` drives analysis
#' * `outputs_schema` defines all visualizations/tables (quality and analysis combined)
#'   as a data frame, one row per output
#' * Each output row uses `dataset_type` to specify the data source
#'   ("base" for the unweighted design, "survey_design" for the weighted design)
#' * `results` stores quality check results; `analysis_results` stores analysis results
#' * All outputs store into the same `visualizations` and `tables` fields
#' * The `pre_run_quality_checks()`, `pre_run_analysis()` and `pre_run_outputs()`
#'   hooks return named sets of input field names; subclasses with multiple
#'   datasets/schemas (e.g. [MortalityDataAnalytics]) override them so the
#'   inherited `run_quality_checks()`, `run_analysis()` and `run_outputs()`
#'   iterate over each set and store results under the set's role name
#'
#' @field data Data frame containing the dataset (standardized or clean data, not raw)
#' @field parent_data_object Reference to the parent Data object
#' @field dataset_name Character name for identifying this analytics object
#' @field data_stage_name Name of the data stage (e.g., "standardized", "clean")
#' @field data_hash Hash of the data from parent Data object
#' @field variable_map Variable mappings from Data object
#' @field value_map Value mappings from Data object
#' @field variable_label Named list of variable labels
#' @field value_label Named list of value labels
#' @field quality_schema Quality check schema with test definitions, thresholds, and penalty scores
#' @field analysis_schema Analysis schema (catalog of possible indicators)
#' @field outputs_schema Unified outputs schema (data frame) for all visualizations and tables
#' @field visualizations List of ggplot2 graphics/visualizations (shared)
#' @field tables List of dataframes or formatted table objects (shared)
#' @field plausibility_results Results of quality checks execution
#' @field analysis_results Results of quantitative analysis (named list: survey_design, base)
#' @field data_analysis_plan QuantDataAnalysisPlanLog object holding the analysis plan
#' @field survey_design srvyr survey design object
#' @field base_survey_design Unfiltered base srvyr survey design object (used internally for analysis)
#' @field analysis_plan_issues_log Tibble of plan validation issues (internal)
#' @field quality_issues_log Tibble of quality schema diagnostic results from quality_diagnose()
#' @field analysis_plan_issues_log Tibble of analysis plan diagnostic results from analysis_diagnose()
#' @field outputs_issues_log Tibble of outputs schema diagnostic results from outputs_diagnose()
#' @field overall_score Overall data quality score
#' @field metadata Free-form metadata list
#'
#' @seealso [FSLDataAnalytics]
#' @export
DataAnalytics <- R6::R6Class(
  classname = "DataAnalytics",
  inherit = Asset,

  public = list(

    # Shared output containers
    visualizations = NULL,
    tables = NULL,

    # Initialization

    #' @description
    #' Initialize a new DataAnalytics object
    #'
    #' @param data A data frame (standardized or clean data, not raw)
    #' @param dap Optional data analysis plan (tibble); auto-generated from
    #'   analysis_schema if not supplied
    #' @param parent_data_object The Data object that generated this analytics object
    #' @param dataset_name A name for this analytics assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @param variable_label Variable labels from Data object
    #' @param value_label Value labels from Data object
    #' @param quality_schema Optional quality check schema
    #' @return A new DataAnalytics object
    initialize = function(
      data = NULL,
      dap = NULL,
      parent_data_object = NULL,
      dataset_name = "DataAnalytics",
      data_stage_name = NULL,
      data_hash = NULL,
      variable_map = NULL,
      value_map = NULL,
      variable_label = NULL,
      value_label = NULL,
      quality_schema = NULL
    ) {
      super$initialize()
      origin <- paste0(dataset_name, "$initialize")
      phrutils::phr_message(origin, "Initializing DataAnalytics class...")

      # --- Common fields
      private$..parent_data_object <- parent_data_object
      private$..parent_dataset_name <- dataset_name
      private$..parent_data_stage_name <- data_stage_name
      private$..parent_data_hash <- data_hash
      private$..variable_map <- variable_map %||% list()
      private$..value_map <- value_map %||% list()
      private$..variable_label <- variable_label %||% list()
      private$..value_label <- value_label %||% list()

      # --- Shared output containers
      self$visualizations <- list()
      self$tables <- list()

      # --- Quality-specific initialization
      if (!is.null(data)) {
        phrutils::phr_validate_dataframe(data, origin = origin, soft = FALSE)
        private$..data <- data
      }

      private$..plausibility_results <- list()
      private$..overall_score <- NA_real_

      # Diagnostic logs
      private$..quality_issues_log <- tibble::tibble()
      private$..analysis_plan_issues_log <- tibble::tibble()
      private$..outputs_issues_log <- tibble::tibble()

      # Loading quality, analysis, and outputs schemas
      private$..load_default_schema(
        filename = private$..default_quality_schema_filename,
        target_field = "..quality_schema",
        required_cols = c(
          "check_group",
          "check_name",
          "check_label",
          "variables",
          "statistical_test",
          "threshold_expression",
          "penalty_score",
          "test_params"
        ),
        schema_name = "quality",
        dataset_name = private$..dataset_name
      )

      private$..load_default_schema(
        filename = private$..default_analysis_schema_filename,
        target_field = "..analysis_schema",
        required_cols = c(
          "indicator_name",
          "calculation",
          "var_name",
          "denom_var",
          "disaggregation",
          "multiplier",
          "indicator_unit"
        ),
        schema_name = "analysis",
        dataset_name = private$..dataset_name
      )

      private$..load_default_schema(
        filename = private$..default_outputs_schema_filename,
        target_field = "..outputs_schema",
        required_cols = c(
          "output_title",
          "output_name",
          "output_subtitle",
          "variables",
          "disaggregation",
          "output_func_name",
          "test_params",
          "output_type",
          "outputs_group",
          "outputs_per_group",
          "dataset_type",
          "output_title_english",
          "output_title_french",
          "output_title_arabic"
        ),
        schema_name = "outputs",
        dataset_name = private$..dataset_name
      )

      # 2. Create survey design objects from data + variable_map.
      #    base_survey_design: simple unweighted design (ids = 1, no weights/strata/clusters)
      #    survey_design: full weighted design considering clusters, weights, and strata
      if (!is.null(data)) {
        private$..base_survey_design <- phrutils::phr_try(
          srvyr::as_survey_design(.data = data, ids = 1),
          on_error = "warn",
          origin = origin,
          hint = phrutils::phr_txt("Could not create base (unweighted) survey design from data.")
        )
        private$..survey_design <- private$..create_survey_design()
      }

      # 3. Populate data_analysis_plan
      if (!is.null(dap)) {
        private$..data_analysis_plan <- QuantDataAnalysisPlanLog$new(
          log_df = dap,
          log_name = paste0(dataset_name,"-Quant Data Analysis Plan")
        )
      } else {
        private$..data_analysis_plan <- QuantDataAnalysisPlanLog$new(
          log_df = NULL,
          log_name = paste0(dataset_name,"-Quant Data Analysis Plan")
        )
        if (!is.null(private$..survey_design) || !is.null(private$..data)) {

          result <- private$..build_dap_from_schema(
            analysis_schema = private$..analysis_schema,
            variable_map    = private$..variable_map %||% list(),
            available_vars  = names(private$..data),
            dataset_label   = "data"
          )

          private$..data_analysis_plan$set(field = "log_df", value = result$dap_df)

        }
      }

      # 4. Load outputs schema (combined quality and analysis outputs)
      # (already loaded above, no separate analysis outputs schema needed)

      phrutils::phr_message(origin, "Initialization complete.")
      invisible(self)
    },

    #' @description Get a summary of the analytics assessment
    #' @return A list with summary information
    summary = function() {
      list(
        dataset_name = private$..dataset_name,
        n_records = if (!is.null(private$..data)) nrow(private$..data) else NULL,
        n_columns = if (!is.null(private$..data)) ncol(private$..data) else NULL,
        n_checks = length(private$..quality_schema),
        n_results = length(private$..plausibility_results),
        overall_score = private$..overall_score,
        parent_object = if (!is.null(private$..parent_data_object)) {
          private$..parent_data_object$dataset_name
        } else {
          NULL
        },
        created_at = private$..metadata$created_datetime
      )
    },

    #' @description Run all quality checks defined in the quality schema(s).
    #'
    #' Iterates over every input set returned by
    #' \code{pre_run_quality_checks()}. For the default single \code{main} set
    #' the behaviour is unchanged: results are stored flat in
    #' \code{plausibility_results} and tables in
    #' \code{tables[["plausibility"]]}. When multiple sets are defined, results
    #' are nested under each set's role name (e.g.
    #' \code{plausibility_results$roster},
    #' \code{tables$roster$plausibility}).
    #'
    #' @return A list of check results (invisibly). Nested per role when
    #'   multiple input sets are defined.
    run_quality_checks = function() {
      phrutils::phr_try(
        {
          sets <- self$pre_run_quality_checks()

          if (is.null(sets) || length(sets) == 0) {
            phrutils::phr_warning(
              message = "pre_run_quality_checks() returned no input sets.",
              origin = self$dataset_name
            )
            return(invisible(list()))
          }

          set_names <- names(sets) %||% rep("", length(sets))
          nested <- !(length(sets) == 1 && identical(set_names, "main"))

          all_results <- list()
          combined_results <- list()

          for (i in seq_along(sets)) {
            set_role <- if (nzchar(set_names[i])) {
              set_names[i]
            } else {
              paste0("set_", i)
            }
            inputs <- private$..resolve_field_set(sets[[i]])
            results <- private$.run_quality_checks_for_set(
              inputs = inputs,
              set_role = set_role,
              nested = nested
            )
            all_results[[set_role]] <- results %||% list()
            combined_results <- c(combined_results, results %||% list())
          }

          self$plausibility_results <- if (nested) {
            all_results
          } else {
            all_results[[1]]
          }

          phrutils::phr_message(
            phrutils::phr_txt(glue::glue(
              "Ran {length(combined_results)} quality checks for {self$dataset_name}."
            ))
          )

          invisible(self$plausibility_results)
        },
        on_error = "warn",
        origin = paste0(self$dataset_name, "$run_quality_checks")
      )
    },

    # Quality Checks

    #' @description Pre-hook providing the input sets used by \code{run_quality_checks()}.
    #'
    #' The default implementation returns a single set (named \code{main})
    #' pointing at the core \code{data}, \code{quality_schema},
    #' \code{base_survey_design}, \code{survey_design} and \code{variable_map}
    #' fields. Subclasses that hold multiple datasets or quality schemas can
    #' override this method to return one entry per dataset/schema. Each entry
    #' must be a named list whose elements give the names of the public fields
    #' holding the respective inputs.
    #'
    #' When more than one set is returned (or the single set is not named
    #' \code{main}), quality results are stored under the set's role name in
    #' \code{plausibility_results} and penalty tables under
    #' \code{tables[[role]][["plausibility"]]}.
    #'
    #' @return A named list of field sets.
    pre_run_quality_checks = function() {
      list(
        main = list(
          data = "data",
          quality_schema = "quality_schema",
          base_survey_design = "base_survey_design",
          survey_design = "survey_design",
          variable_map = "variable_map"
        )
      )
    },

    # Diagnose Methods

    #' @description
    #' Diagnose issues in the quality schema against the current dataset.
    #'
    #' For every check defined in \code{self$quality_schema}, this method
    #' verifies that (a) the required variables exist in \code{self$data},
    #' (b) the statistical test function can be found, and (c) each
    #' threshold expression is syntactically valid. The results mirror the
    #' flat quality schema table and add a \code{status} column ("ok" or a
    #' description of the issue).  Results are stored in
    #' \code{self$quality_issues_log}.
    #'
    #' @return A tibble (invisibly) with one row per quality check.
    quality_diagnose = function() {
      origin <- paste0(self$dataset_name, "DataAnalytics$quality_diagnose")

      phrutils::phr_try(
        {
          if (
            is.null(private$..quality_schema) || length(private$..quality_schema) == 0
          ) {
            phrutils::phr_warning(
              message = phrutils::phr_txt("No quality_schema defined. Cannot diagnose."),
              origin = origin
            )
            empty <- tibble::tibble(
              check_group = character(),
              check_name = character(),
              check_label = character(),
              variables = character(),
              statistical_test = character(),
              test_params = character(),
              n_thresholds = integer(),
              variables_in_data = logical(),
              missing_variables = character(),
              function_available = logical(),
              thresholds_valid = logical(),
              status = character()
            )
            private$..quality_issues_log <- empty
            return(invisible(empty))
          }

          data_cols <- if (!is.null(private$..data)) {
            names(private$..data)
          } else {
            character(0)
          }
          rows <- list()

          for (check_name in names(private$..quality_schema)) {
            check <- private$..quality_schema[[check_name]]

            # --- basic fields
            check_group <- check$check_group %||% NA_character_
            check_label <- check$check_label %||% NA_character_
            statistical_test <- check$statistical_test %||% NA_character_
            variables <- check$variables %||% character(0)
            test_params <- check$test_params %||% list()
            thresholds <- check$thresholds %||% list()

            # --- 1. variables present in data
            mapped_vars <- private$..translate_canonical_to_actual_vars(variables)
            missing_vars <- setdiff(mapped_vars, data_cols)
            vars_in_data <- length(missing_vars) == 0

            # --- 2. test function available
            func_available <- FALSE
            if (!is.na(statistical_test) && nzchar(statistical_test)) {
              func_name <- paste0("quality_test_", statistical_test)
              if (requireNamespace("phr", quietly = TRUE)) {
                tryCatch(
                  {
                    ns <- asNamespace("phr")
                    func_available <- exists(
                      func_name,
                      envir = ns,
                      mode = "function",
                      inherits = FALSE
                    )
                  },
                  error = function(e) {}
                )
              }
              if (!func_available) {
                tryCatch(
                  {
                    func_available <- exists(
                      func_name,
                      mode = "function",
                      inherits = TRUE
                    )
                  },
                  error = function(e) {}
                )
              }
            }

            # --- 3. threshold expressions parseable
            thresholds_valid <- TRUE
            if (length(thresholds) > 0) {
              for (thr in thresholds) {
                expr_str <- thr$threshold_expression %||% thr$expression
                if (
                  !is.null(expr_str) && !is.na(expr_str) && nzchar(expr_str)
                ) {
                  parsed <- tryCatch(
                    parse(text = expr_str),
                    error = function(e) NULL
                  )
                  if (is.null(parsed)) {
                    thresholds_valid <- FALSE
                    break
                  }
                }
              }
            }

            # --- build status string
            issues <- character(0)
            if (!vars_in_data) {
              issues <- c(
                issues,
                paste0(
                  phrutils::phr_txt("missing variables: "),
                  paste(missing_vars, collapse = ", ")
                )
              )
            }
            if (!func_available) {
              issues <- c(
                issues,
                paste0(
                  phrutils::phr_txt("function not found: "), "quality_test_",
                  statistical_test %||% "NA"
                )
              )
            }
            if (!thresholds_valid) {
              issues <- c(issues, phrutils::phr_txt("invalid threshold expression(s)"))
            }
            status <- if (length(issues) == 0) {
              "ok"
            } else {
              paste(issues, collapse = "; ")
            }

            rows[[length(rows) + 1]] <- tibble::tibble(
              check_group = check_group,
              check_name = check_name,
              check_label = check_label,
              variables = paste(variables, collapse = ", "),
              statistical_test = statistical_test %||% NA_character_,
              test_params = if (length(test_params) > 0) {
                paste(
                  names(test_params),
                  test_params,
                  sep = "=",
                  collapse = ", "
                )
              } else {
                NA_character_
              },
              n_thresholds = length(thresholds),
              variables_in_data = vars_in_data,
              missing_variables = if (length(missing_vars) > 0) {
                paste(missing_vars, collapse = ", ")
              } else {
                NA_character_
              },
              function_available = func_available,
              thresholds_valid = thresholds_valid,
              status = status
            )
          }

          result <- if (length(rows) > 0) {
            dplyr::bind_rows(rows)
          } else {
            tibble::tibble(
              check_group = character(),
              check_name = character(),
              check_label = character(),
              variables = character(),
              statistical_test = character(),
              test_params = character(),
              n_thresholds = integer(),
              variables_in_data = logical(),
              missing_variables = character(),
              function_available = logical(),
              thresholds_valid = logical(),
              status = character()
            )
          }

          self$quality_issues_log <- result

          n_issues <- sum(result$status != "ok", na.rm = TRUE)
          phrutils::phr_message(phrutils::phr_txt(
            "quality_diagnose complete: {nrow(result)} check(s) reviewed, {n_issues} issue(s) found for {self$dataset_name}."
          ))

          invisible(result)
        },
        on_error = "warn",
        origin = origin
      )
    },

    #' @description
    #' Diagnose issues in the data analysis plan against the current dataset.
    #'
    #' For every indicator row in \code{self$data_analysis_plan$get("log_df")}, this
    #' method verifies that the primary variable (\code{var_name}), the
    #' denominator variable (\code{denom_var}), and the disaggregation variable
    #' exist in the dataset (or survey design), and that the \code{calculation}
    #' type is valid. The results mirror the analysis plan tibble and add
    #' diagnostic columns. Results are stored in
    #' \code{self$analysis_plan_issues_log}.
    #'
    #' @return A tibble (invisibly) with one row per analysis plan indicator.
    analysis_diagnose = function() {
      origin <- paste0(self$dataset_name, "DataAnalytics$analysis_diagnose")

      valid_calcs <- c(
        "prop",
        "mean",
        "median",
        "ratio",
        "cat",
        "categorical",
        "select_multiple_cat"
      )

      empty_result <- tibble::tibble(
        indicator_name = character(),
        calculation = character(),
        var_name = character(),
        denom_var = character(),
        disaggregation = character(),
        multiplier = numeric(),
        indicator_unit = character(),
        var_name_in_data = logical(),
        denom_var_in_data = logical(),
        disaggregation_in_data = logical(),
        calculation_valid = logical(),
        status = character()
      )

      phrutils::phr_try(
        {
          schema_df <- if (!is.null(private$..analysis_schema)) {
            private$..analysis_schema
          } else {
            NULL
          }

          if (is.null(schema_df) || nrow(schema_df) == 0) {
            phrutils::phr_warning(
              message = phrutils::phr_txt("No analysis schema defined. Cannot diagnose."),
              origin = origin
            )
            private$..analysis_plan_issues_log <- empty_result
            return(invisible(empty_result))
          }

          available_vars <- if (!is.null(private$..survey_design)) {
            names(private$..survey_design$variables)
          } else if (!is.null(private$..data)) {
            names(private$..data)
          } else {
            character(0)
          }

          rows <- lapply(seq_len(nrow(schema_df)), function(i) {
            row <- schema_df[i, ]

            # Helper: resolve a canonical name through variable_map and check availability
            resolve_var <- function(val) {
              v <- val %||% NA_character_
              if (is.na(v) || !nzchar(v)) {
                return(v)
              }
              resolved <- private$..translate_canonical_to_actual_vars(v)
              if (length(resolved) > 0) resolved[[1]] else v
            }

            # Helper: an optional variable field is ok if blank/NA, or if the col exists in data
            optional_var_ok <- function(val) {
              v <- val %||% NA_character_
              is.na(v) || !nzchar(v) || resolve_var(v) %in% available_vars
            }

            resolved_var_name <- resolve_var(row$var_name)
            var_name_ok <- !is.na(row$var_name) &&
              nzchar(row$var_name) &&
              resolved_var_name %in% available_vars
            denom_ok <- optional_var_ok(row$denom_var)
            disagg_ok <- optional_var_ok(row$disaggregation)
            calc_valid <- !is.na(row$calculation) &&
              row$calculation %in% valid_calcs

            issues <- character(0)
            if (!var_name_ok) {
              issues <- c(
                issues,
                paste0("var_name '", row$var_name, phrutils::phr_txt("' not found in data"))
              )
            }
            if (!denom_ok) {
              issues <- c(
                issues,
                paste0("denom_var '", row$denom_var, phrutils::phr_txt("' not found in data"))
              )
            }
            if (!disagg_ok) {
              issues <- c(
                issues,
                paste0(
                  phrutils::phr_txt("disaggregation '"),
                  row$disaggregation,
                  phrutils::phr_txt("' not found in data")
                )
              )
            }
            if (!calc_valid) {
              issues <- c(
                issues,
                paste0(phrutils::phr_txt("invalid calculation '"), row$calculation %||% "NA", "'")
              )
            }
            status <- if (length(issues) == 0) {
              "ok"
            } else {
              paste(issues, collapse = "; ")
            }

            tibble::tibble(
              indicator_name = row$indicator_name %||% NA_character_,
              calculation = row$calculation %||% NA_character_,
              var_name = row$var_name %||% NA_character_,
              denom_var = row$denom_var %||% NA_character_,
              disaggregation = row$disaggregation %||% NA_character_,
              multiplier = as.numeric(row$multiplier %||% NA),
              indicator_unit = row$indicator_unit %||% NA_character_,
              var_name_in_data = var_name_ok,
              denom_var_in_data = denom_ok,
              disaggregation_in_data = disagg_ok,
              calculation_valid = calc_valid,
              status = status
            )
          })

          result <- dplyr::bind_rows(rows)
          private$..analysis_plan_issues_log <- result

          n_issues <- sum(result$status != "ok", na.rm = TRUE)
          phrutils::phr_message(
            phrutils::phr_txt(
            "analysis_diagnose complete: {nrow(result)} indicator(s) reviewed, {n_issues} issue(s) found for {self$dataset_name}."
          ))

          invisible(result)
        },
        on_error = "warn",
        origin = origin
      )
    },

    #' @description
    #' Diagnose issues in the unified outputs schema against the current dataset.
    #'
    #' For every output defined in \code{self$outputs_schema}, this method checks
    #' that (a) the output function exists, (b) any \code{variables} listed exist
    #' in the data or survey design, and (c) required schema fields
    #' (\code{output_title}, \code{output_func_name}, \code{output_type}) are
    #' non-empty. The results mirror the outputs schema table and add a
    #' \code{status} column. Results are stored in \code{self$outputs_issues_log}.
    #'
    #' @return A tibble (invisibly) with one row per output definition.
    outputs_diagnose = function() {
      origin <- paste0(private$..dataset_name, "DataAnalytics$outputs_diagnose")

      # Combine column names from both data and survey_design for comprehensive checking
      all_cols <- unique(c(
        if (!is.null(private$..data)) names(private$..data) else character(0),
        if (!is.null(private$..survey_design)) {
          names(private$..survey_design$variables)
        } else {
          character(0)
        }
      ))

      # private$.diagnose_outputs_schema(
      #   schema = self$outputs_schema,
      #   schema_name = "outputs_schema",
      #   log_field = "outputs_issues_log",
      #   data_cols = all_cols,
      #   origin = origin
      # )

      phrutils::phr_try(
        {
          empty_result <- tibble::tibble(
            output_title = character(),
            output_name = character(),
            output_func_name = character(),
            output_type = character(),
            variables = character(),
            test_params = character(),
            outputs_group = character(),
            function_available = logical(),
            variables_in_data = logical(),
            missing_variables = character(),
            required_fields_ok = logical(),
            status = character()
          )

          if (is.null(private$..outputs_schema) || nrow(private$..outputs_schema) == 0) {
            phrutils::phr_warning(
              message = phrutils::phr_txt(
                "No {schema_name} defined. Cannot diagnose."
              ),
              origin = origin
            )
            private$..outputs_issues_log <- empty_result
            return(invisible(empty_result))
          }

          # Read a schema cell, returning NA_character_ when absent or empty.
          cell <- function(col, i) {
            if (!col %in% names(private$..outputs_schema)) {
              return(NA_character_)
            }
            value <- private$..outputs_schema[[col]][[i]]
            if (is.null(value) || length(value) != 1 || is.na(value)) {
              return(NA_character_)
            }
            value <- as.character(value)
            if (!nzchar(value)) NA_character_ else value
          }

          rows <- list()

          for (i in seq_len(nrow(private$..outputs_schema))) {
            out_name <- cell("output_title", i)
            func_name <- cell("output_func_name", i)
            output_type <- cell("output_type", i)
            output_name <- cell("output_name", i)
            outputs_group <- cell("outputs_group", i)
            test_params <- cell("test_params", i)
            variables <- outputs_parse_list_field(cell("variables", i)) %||%
              character(0)

            # --- 1. required fields present
            req_ok <- !is.na(func_name) && !is.na(output_type)

            # --- 2. function available
            func_available <- FALSE
            if (req_ok) {
              if (requireNamespace("phr", quietly = TRUE)) {
                tryCatch(
                  {
                    ns <- asNamespace("phr")
                    func_available <- exists(
                      func_name,
                      envir = ns,
                      mode = "function",
                      inherits = FALSE
                    )
                  },
                  error = function(e) {}
                )
              }
              if (!func_available) {
                tryCatch(
                  {
                    func_available <- exists(
                      func_name,
                      mode = "function",
                      inherits = TRUE
                    )
                  },
                  error = function(e) {}
                )
              }
            }

            # --- 3. variables present in data
            # Skip @variable_map and @value_map refs; resolve canonical names via variable_map
            literal_vars <- variables[!grepl("^@", variables)]
            resolved_vars <- private$..translate_canonical_to_actual_vars(
              literal_vars
            )
            missing_vars <- setdiff(resolved_vars, data_cols)
            vars_in_data <- length(missing_vars) == 0

            # --- build status
            issues <- character(0)
            if (!req_ok) {
              issues <- c(
                issues,
                "missing required fields (output_func_name or output_type)"
              )
            }
            if (!func_available && req_ok) {
              issues <- c(
                issues,
                paste0("function '", func_name, "' not found")
              )
            }
            if (!vars_in_data) {
              issues <- c(
                issues,
                paste0(
                  "missing variables: ",
                  paste(missing_vars, collapse = ", ")
                )
              )
            }
            status <- if (length(issues) == 0) {
              "ok"
            } else {
              paste(issues, collapse = "; ")
            }

            rows[[length(rows) + 1]] <- tibble::tibble(
              output_title = out_name,
              output_name = output_name,
              output_func_name = func_name,
              output_type = output_type,
              variables = if (length(variables) > 0) {
                paste(variables, collapse = ", ")
              } else {
                NA_character_
              },
              test_params = test_params,
              outputs_group = outputs_group,
              function_available = func_available,
              variables_in_data = vars_in_data,
              missing_variables = if (length(missing_vars) > 0) {
                paste(missing_vars, collapse = ", ")
              } else {
                NA_character_
              },
              required_fields_ok = req_ok,
              status = status
            )
          }

          result <- if (length(rows) > 0) {
            dplyr::bind_rows(rows)
          } else {
            empty_result
          }
          private$..outputs_log_issues <- result

          n_issues <- sum(result$status != "ok", na.rm = TRUE)
          phrutils::phr_message(
            phrutils::phr_txt(
            "{schema_name} diagnose complete: {nrow(result)} output(s) reviewed, {n_issues} issue(s) found for {self$dataset_name}."
          ))

          invisible(result)
        },
        on_error = "warn",
        origin = origin
      )


    },

    #' @description Pre-hook providing the input sets used by \code{run_outputs()}.
    #'
    #' The default implementation returns a single set (named \code{main})
    #' pointing at the core \code{data}, \code{outputs_schema},
    #' \code{base_survey_design}, \code{survey_design} and \code{variable_map}
    #' fields. Subclasses that hold multiple datasets (e.g. linked roster or
    #' deaths data) can override this method to return one entry per dataset.
    #' Each entry must be a named list whose elements give the names of the
    #' public fields holding the respective inputs. The \code{variable_map}
    #' entry is used to resolve \code{@variable_map$} references in each
    #' output's \code{test_params}, so sets pointing at a linked dataset must
    #' name that dataset's own variable map.
    #'
    #' When more than one set is returned (or the single set is not named
    #' \code{main}), outputs are stored under the set's role name in
    #' \code{visualizations} and \code{tables} (e.g.
    #' \code{visualizations$roster}, \code{tables$deaths}).
    #'
    #' @return A named list of field sets.
    pre_run_outputs = function() {
      list(
        main = list(
          data = "data",
          outputs_schema = "outputs_schema",
          base_survey_design = "base_survey_design",
          survey_design = "survey_design",
          variable_map = "variable_map"
        )
      )
    },

    #' @description Run all outputs defined in the outputs schema(s).
    #'
    #' Iterates over every input set returned by \code{pre_run_outputs()}.
    #' Each set's outputs schema is a data frame and is processed row by row:
    #' the required survey design is resolved from the row's
    #' \code{dataset_type}, the specified output function is called, and the
    #' result is stored in \code{self$visualizations} or \code{self$tables}.
    #' For the default single \code{main} set results are stored flat; when
    #' multiple sets are defined, results are nested under each set's role name
    #' (e.g. \code{tables$roster}, \code{visualizations$deaths}).
    #'
    #' The \code{output_name} column controls the key used when storing results
    #' in \code{visualizations} or \code{tables}, and \code{outputs_group}
    #' optionally groups them one level deeper. Outputs whose function fails
    #' return \code{NULL}; these are reported as warnings and are not stored, so
    #' no empty \code{outputs_group} node is left behind.
    #'
    #' The \code{output_title} column and the language-specific title columns
    #' (\code{output_title_english}, \code{output_title_french},
    #' \code{output_title_arabic}) are used to auto-generate the
    #' \code{title_name} argument passed to \code{plot_*} or \code{table_*}
    #' functions. The language-specific title for the chosen \code{language}
    #' takes precedence; if absent the generic \code{output_title} value is used
    #' as a fallback.
    #'
    #' Supported \code{dataset_type} values:
    #' \describe{
    #'   \item{"base"}{uses the set's base (unweighted) survey design; this is the default}
    #'   \item{"survey_design"}{uses the set's weighted survey design}
    #' }
    #'
    #' @param language Character string specifying the language for auto-generated
    #'   titles. One of \code{"english"} (default), \code{"french"}, or
    #'   \code{"arabic"}. Falls back to the generic \code{output_title} when the
    #'   language-specific field is absent.
    #' @return Invisibly returns a list with \code{visualizations} and \code{tables}
    run_outputs = function(language = "english") {
      phrutils::phr_try(
        {
          sets <- self$pre_run_outputs()

          if (is.null(sets) || length(sets) == 0) {
            phrutils::phr_warning(
              message = phrutils::phr_txt("pre_run_outputs() returned no input sets."),
              origin = private$..dataset_name
            )
            return(invisible(list(
              visualizations = self$visualizations,
              tables = self$tables
            )))
          }

          set_names <- names(sets) %||% rep("", length(sets))
          nested <- !(length(sets) == 1 && identical(set_names, "main"))

          for (i in seq_along(sets)) {
            set_role <- if (nzchar(set_names[i])) {
              set_names[i]
            } else {
              paste0("set_", i)
            }
            inputs <- private$..resolve_field_set(sets[[i]])

            private$..run_outputs_for_set(
              inputs = inputs,
              set_role = set_role,
              nested = nested,
              language = language
            )
          }

          phrutils::phr_message(
            phrutils::phr_txt(
            "run_outputs complete: {length(self$visualizations)} visualization(s), {length(self$tables)} table(s)."
          ))

          invisible(list(
            visualizations = self$visualizations,
            tables = self$tables
          ))
        },
        on_error = "warn",
        origin = paste0(self$dataset_name, "DataAnalytics$run_outputs")
      )
    },

    #' @description Add all data columns to the data analysis plan(s).
    #'
    #' For every field set returned by \code{pre_add_all_to_dap()}, this
    #' method lists all column names in the data and classifies them as:
    #' (1) already present in both the variable_map and the
    #' data_analysis_plan, (2) present in the variable_map but not the
    #' data_analysis_plan, or (3) present in neither. Columns in group (2)
    #' are added to the data_analysis_plan using their existing variable_map
    #' reference; columns in group (3) are first added to the variable_map as
    #' novel entries keyed by their column name and then added to the
    #' data_analysis_plan. The calculation type is best-guess estimated as
    #' "prop", "mean", "cat", or "select_multiple_cat" based on the column
    #' values. Character columns with more than 20 unique values that are not
    #' comma- or space-separated are skipped (likely metadata or uuid fields).
    #' If a valid stratum column is defined in the variable_map, each newly
    #' added row is replicated with the stratum column in the disaggregation
    #' field.
    #'
    #' @return Invisibly returns self.
    add_all_to_dap = function() {
      origin <- paste0(self$dataset_name, "$add_all_to_dap")

      phrutils::phr_try(
        {
          sets <- self$pre_add_all_to_dap()

          if (is.null(sets) || length(sets) == 0) {
            phrutils::phr_warning(
              origin,
              phrutils::phr_txt("pre_add_all_to_dap() returned no field sets. Nothing to do.")
            )
            return(invisible(self))
          }

          set_names <- names(sets) %||% rep("", length(sets))

          for (i in seq_along(sets)) {
            set_name <- if (nzchar(set_names[i])) {
              set_names[i]
            } else {
              paste0("set_", i)
            }
            private$..add_all_to_dap_single(sets[[i]], set_name, origin)
          }
        },
        on_error = "warn",
        origin = origin,
        hint = phrutils::phr_txt("Check that pre_add_all_to_dap() returns valid field-name sets.")
      )

      invisible(self)
    },

    #' @description Pre-hook providing the field sets used by \code{add_all_to_dap()}.
    #'
    #' The default implementation returns a single set pointing at the core
    #' \code{data}, \code{variable_map} and \code{data_analysis_plan} fields.
    #' Subclasses that hold multiple datasets (e.g. linked roster or deaths
    #' data) can override this method to return one entry per dataset. Each
    #' entry must be a named list with elements \code{data},
    #' \code{variable_map} and \code{data_analysis_plan}, giving the names of
    #' the public fields that hold the dataset, its variable map, and its
    #' data analysis plan respectively.
    #'
    #' @return A named list of field sets.
    pre_add_all_to_dap = function() {
      list(
        main = list(
          data = "data",
          variable_map = "variable_map",
          data_analysis_plan = "data_analysis_plan"
        )
      )
    },

    #' @description Run the data analysis plan(s) and store results in analysis_results
    #'
    #' Iterates over every input set returned by \code{pre_run_analysis()}.
    #' Each set's indicators are executed twice: once using the set's full
    #' survey design (weighted) and once using a simple unweighted design.
    #' For the default single \code{main} set both result sets are stored flat
    #' in \code{self$analysis_results} as a named list with elements
    #' \code{survey_design} and \code{base} (unchanged behaviour). When
    #' multiple sets are defined, results are nested under each set's role
    #' name (e.g. \code{analysis_results$roster$survey_design}).
    #'
    #' After storing the results, \code{self$post_run_analysis()} is called to
    #' allow subclasses to perform additional analysis steps.
    #'
    #' @return Invisibly returns self.
    run_analysis = function() {
      origin <- paste0(self$dataset_name, "$run_analysis")
      phrutils::phr_message(origin, phrutils::phr_txt("Running analysis plan..."))

      sets <- self$pre_run_analysis()

      if (is.null(sets) || length(sets) == 0) {
        phrutils::phr_error(origin, phrutils::phr_txt("pre_run_analysis() returned no input sets."))
        return(invisible(self))
      }

      set_names <- names(sets) %||% rep("", length(sets))
      nested <- !(length(sets) == 1 && identical(set_names, "main"))

      all_results <- list()

      for (i in seq_along(sets)) {
        set_role <- if (nzchar(set_names[i])) {
          set_names[i]
        } else {
          paste0("set_", i)
        }
        inputs <- private$..resolve_field_set(sets[[i]])
        set_results <- private$..run_analysis_for_set(
          inputs = inputs,
          set_role = set_role,
          nested = nested,
          origin = origin
        )
        if (!is.null(set_results)) {
          all_results[[set_role]] <- set_results
        }
      }

      private$..analysis_results <- if (nested) {
        all_results
      } else {
        all_results[[1]] %||% list(survey_design = NULL, base = NULL)
      }

      phrutils::phr_message(origin, phrutils::phr_txt("Analysis completed successfully."))

      # Call the post-analysis hook so subclasses can perform additional steps.
      self$post_run_analysis()

      invisible(self)
    },

    #' @description Pre-hook providing the input sets used by \code{run_analysis()}.
    #'
    #' The default implementation returns a single set (named \code{main})
    #' pointing at the core \code{data}, \code{survey_design} and
    #' \code{data_analysis_plan} fields. Subclasses that hold multiple datasets
    #' (e.g. linked roster or deaths data) can override this method to return
    #' one entry per dataset. Each entry must be a named list whose elements
    #' give the names of the public fields holding the respective inputs.
    #'
    #' When more than one set is returned (or the single set is not named
    #' \code{main}), analysis results are stored under the set's role name in
    #' \code{analysis_results} (e.g. \code{analysis_results$roster}).
    #'
    #' @return A named list of field sets.
    pre_run_analysis = function() {
      list(
        main = list(
          data = "data",
          survey_design = "survey_design",
          data_analysis_plan = "data_analysis_plan"
        )
      )
    },

    #' @description Post-analysis hook called at the end of \code{run_analysis()}.
    #'
    #' The default implementation is a no-op.  Subclasses can override this
    #' method to perform additional analysis steps after the standard analysis
    #' plan has been executed and stored in \code{self$analysis_results}.
    #'
    #' @return Invisibly returns self.
    post_run_analysis = function() {
      invisible(self)
    }
  ),

  # Private helpers

  private = list(

    # Core fields (shared)
    ..data = NULL,
    ..parent_data_object = NULL,
    ..dataset_name = NULL,
    ..data_stage_name = NULL,
    ..data_hash = NULL,
    ..variable_map = NULL,
    ..value_map = NULL,
    ..variable_label = NULL,
    ..value_label = NULL,

    # Analysis-specific fields
    ..data_analysis_plan = NULL,
    ..survey_design = NULL,
    ..base_survey_design = NULL,
    ..analysis_results = NULL, # Quantitative analysis results
    ..analysis_schema = NULL,

    # Quality-specific fields
    ..quality_schema = NULL,
    ..plausibility_results = NULL, # Quality check results
    ..overall_score = NULL,

    # Unified outputs schema
    ..outputs_schema = NULL,

    # Diagnostic logs
    ..quality_issues_log = NULL,
    ..analysis_plan_issues_log = NULL,
    ..outputs_issues_log = NULL,

    # @field ..default_quality_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_quality_schema_filename = c(
      "resources",
      "quality_schema_data_household_template.xlsx"
    ),

    # @field ..default_outputs_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_outputs_schema_filename = c(
      "resources",
      "outputs_schema_data_household_template.xlsx"
    ),

    # @field ..default_analysis_schema_filename Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_analysis_schema_filename = c(
      "resources",
      "analysis_schema_data_household_template.xlsx"
    ),

    #' Load, validate, and assign a default schema.
    #'
    #' Internal helper used during object initialization to load a schema
    #' definition from an Excel file, validate its structure against a set of
    #' required columns, and store the result in the specified active binding or
    #' field. If the schema cannot be loaded or fails validation, an empty tibble
    #' containing the required columns is created and stored instead.
    #'
    #' Validation is performed using `private$..validate_table_schema()`. Any
    #' errors encountered during loading or validation are captured and reported
    #' through `phrutils::phr_warning()`.
    #'
    #' @param filename A list or character vector of arguments passed to
    #'   [system.file()] identifying the schema file to load from the package
    #'   resources.
    #' @param target_field A character string giving the name of the field or
    #'   active binding where the validated schema should be stored via
    #'   `self$set()`.
    #' @param required_cols A character vector of column names required for the
    #'   schema to be considered valid.
    #' @param schema_name A character string identifying the schema type (e.g.
    #'   `"quality"`, `"analysis"`, `"outputs"`). Used to construct warning and
    #'   logging origins.
    #' @param dataset_name Optional character string identifying the dataset type.
    #'   When supplied, it is prefixed to warning and initialization origins.
    #'
    #' @return Invisibly updates the specified field on the object. No value is
    #'   returned.
    #'
    #' @details
    #' If validation fails, an empty tibble is generated with one zero-length
    #' character column for each value in `required_cols`. This ensures downstream
    #' code always receives a valid schema object with the expected structure.
    #'
    #' @keywords internal
    #' @noRd
    ..load_default_schema = function(
    filename,
    target_field,
    required_cols,
    schema_name,
    dataset_name = NULL
    ) {

      phrutils::phr_try(
        {

          df_schema <- tryCatch(
            readxl::read_xlsx(
              do.call(
                system.file,
                c(
                  as.list(filename),
                  package = "phr"
                )
              )
            ),
            error = function(e) {

              phrutils::phr_warning(
                origin = paste0(
                  "DataAnalytics$default_",
                  schema_name,
                  "_schema"
                ),
                message = phrutils::phr_txt(
                  phrutils::phr_txt("Failed to read {filename}: {e$message}")
                )
              )

              return(NULL)
            }
          )

          if (
            isTRUE(
              private$..validate_table_schema(
                df_schema,
                required_cols = required_cols,
                origin = paste0(
                  "DataAnalytics$validate_",
                  schema_name,
                  "_schema"
                )
              )
            )
          ) {

            self$set(
              field = target_field,
              value = df_schema
            )

          } else {

            empty_schema <- tibble::as_tibble(
              stats::setNames(
                replicate(
                  length(required_cols),
                  character(0),
                  simplify = FALSE
                ),
                required_cols
              )
            )

            self$set(
              field = target_field,
              value = empty_schema
            )

            if (is.null(df_schema)) {
              msg <- paste0(
                phrutils::phr_txt("The loaded "),
                schema_name,
                phrutils::phr_txt(" schema was or coerced to NULL. "),
                phrutils::phr_txt("A blank valid dataframe was stored instead.")
              )
            } else {
              msg <- paste0(
                phrutils::phr_txt("The loaded "),
                schema_name,
                phrutils::phr_txt(" schema did not have the correct required columns. "),
                phrutils::phr_txt("A blank valid dataframe was stored instead.")
              )
            }

            phrutils::phr_warning(
              message = phrutils::phr_txt(msg),
              origin = paste0(
                dataset_name,
                "DataAnalytics$loading_",
                schema_name,
                "_schema"
              ),
              hint = paste0(
                phrutils::phr_txt("Check required columns or format of the "),
                schema_name,
                phrutils::phr_txt(" schema input file.")
              )
            )
          }

        },
        on_error = "warn",
        origin = paste0(
          dataset_name,
          "DataAnalytics$initialize_",
          schema_name,
          "_schema"
        )
      )
    },

    #' @description Translate canonical variable names to actual column names using variable_map
    #' @param canonical_vars Character vector of canonical variable names
    #' @param variable_map Optional named list mapping canonical names to actual
    #'   column names. Defaults to \code{self$variable_map} when \code{NULL}.
    #' @return Character vector of actual column names
    ..translate_canonical_to_actual_vars = function(
    canonical_vars,
    variable_map = NULL
    ) {
      if (is.null(canonical_vars) || length(canonical_vars) == 0) {
        return(character(0))
      }

      vm <- variable_map %||% private$..variable_map

      if (is.null(vm) || length(vm) == 0) {
        return(canonical_vars)
      }

      actual_vars <- sapply(
        canonical_vars,
        function(canonical_name) {
          actual_name <- if (canonical_name %in% names(vm)) {
            vm[[canonical_name]]
          } else {
            NULL
          }
          if (!is.null(actual_name) && nzchar(actual_name)) {
            actual_name
          } else {
            canonical_name
          }
        },
        USE.NAMES = FALSE
      )

      return(actual_vars)
    },

    #' @description Evaluate threshold expressions to determine penalty
    #' @param test_statistic The test statistic value
    #' @param p_value The p-value (if applicable)
    #' @param thresholds List of threshold definitions with expression and penalty
    #' @return List with penalty, max_penalty, threshold_expression, and message
    ..evaluate_threshold_expressions = function(
    test_statistic,
    p_value,
    thresholds
    ) {
      result <- list(
        penalty = 0,
        max_penalty = 0,
        threshold_expression = "none",
        message = ""
      )

      if (is.null(thresholds) || length(thresholds) == 0) {
        return(result)
      }

      # Short-circuit when test_statistic is NA — threshold evaluation is meaningless
      if (
        (length(test_statistic) == 1 && is.na(test_statistic)) ||
        all(is.na(test_statistic))
      ) {
        all_penalties <- sapply(thresholds, function(x) {
          x$penalty %||% x$penalty_score %||% 0
        })
        result$max_penalty <- max(all_penalties, na.rm = TRUE)
        result$message <- "Test statistic is NA; threshold evaluation skipped (insufficient data or invalid inputs for the quality check)"
        return(result)
      }

      all_penalties <- sapply(thresholds, function(x) {
        x$penalty %||% x$penalty_score %||% 0
      })
      result$max_penalty <- max(all_penalties, na.rm = TRUE)

      for (i in seq_along(thresholds)) {
        threshold_def <- thresholds[[i]]
        expression <- threshold_def$expression %||%
          threshold_def$threshold_expression
        penalty <- threshold_def$penalty %||% threshold_def$penalty_score %||% 0

        if (is.null(expression) || is.na(expression) || !nzchar(expression)) {
          next
        }

        phrutils::phr_try(
          {
            eval_env <- new.env()
            eval_env$test_statistic <- test_statistic
            eval_env$p_value <- p_value
            meets_threshold <- eval(parse(text = expression), envir = eval_env)

            if (isTRUE(meets_threshold)) {
              result$penalty <- penalty
              result$threshold_expression <- expression
              result$message <- paste0(
                phrutils::phr_txt("Threshold met: "),
                expression,
                phrutils::phr_txt(" (test_statistic="),
                round(test_statistic, 3),
                if (!is.na(p_value)) {
                  paste0(", p_value=", round(p_value, 4))
                } else {
                  ""
                },
                phrutils::phr_txt("), penalty="),
                penalty
              )
              break
            }
          },
          on_error = "warn",
          origin = "..evaluate_threshold_expressions"
        )
      }

      if (result$threshold_expression == "none") {
        result$message <- paste0(
          phrutils::phr_txt("No threshold met (test_statistic="),
          round(test_statistic, 3),
          if (!is.na(p_value)) paste0(", p_value=", round(p_value, 4)) else "",
          ")"
        )
      }

      return(result)
    },

    #' @description Execute a single quality check using statistical tests
    #' @param check A single check definition from the schema
    #' @param survey_design Optional srvyr survey design to use. Defaults to
    #'   \code{self$base_survey_design} (or \code{self$survey_design} as fallback)
    #'   when \code{NULL}.
    #' @param data Optional data frame to check variables against. Defaults to
    #'   \code{self$data} when \code{NULL}.
    #' @param variable_map Optional named list mapping canonical names to actual
    #'   column names. Defaults to \code{self$variable_map} when \code{NULL}.
    #' @return A list containing the check result
    ..execute_check = function(
    check,
    survey_design = NULL,
    data = NULL,
    variable_map = NULL
    ) {
      phrutils::phr_try(
        {
          check_data <- data %||% private$..data

          result <- list(
            check_name = check$check_name %||% "unknown",
            check_label = check$check_label %||% "",
            check_group = check$check_group %||% NA_character_,
            test_statistic = NA,
            p_value = NA,
            penalty = 0,
            max_penalty = 0,
            threshold_expression = NA_character_,
            message = ""
          )

          test_name <- check$statistical_test
          variables <- check$variables
          thresholds <- check$thresholds
          test_params <- check$test_params

          if (is.null(variables) || length(variables) == 0) {
            result$message <- "No variables specified"
            return(result)
          }

          mapped_vars <- private$..translate_canonical_to_actual_vars(
            variables,
            variable_map = variable_map
          )
          available_vars <- intersect(mapped_vars, names(check_data))
          if (length(available_vars) != length(mapped_vars)) {
            missing_vars <- setdiff(mapped_vars, available_vars)
            result$message <- paste0(
              phrutils::phr_txt("Required variables not found in data: "),
              paste(missing_vars, collapse = ", ")
            )
            return(result)
          }

          test_func_name <- paste0("quality_test_", test_name)
          test_function <- NULL

          if (requireNamespace("phr", quietly = TRUE)) {
            tryCatch(
              {
                ns <- asNamespace("phr")
                if (
                  exists(
                    test_func_name,
                    envir = ns,
                    mode = "function",
                    inherits = FALSE
                  )
                ) {
                  test_function <- get(
                    test_func_name,
                    envir = ns,
                    mode = "function",
                    inherits = FALSE
                  )
                }
              },
              error = function(e) {}
            )
          }

          if (is.null(test_function)) {
            tryCatch(
              {
                if (
                  exists(test_func_name, mode = "function", inherits = TRUE)
                ) {
                  test_function <- get(
                    test_func_name,
                    mode = "function",
                    inherits = TRUE
                  )
                }
              },
              error = function(e) {}
            )
          }

          if (is.null(test_function)) {
            result$message <- paste0(
              phrutils::phr_txt("Unknown test: "),
              test_name,
              " (function ",
              test_func_name,
              phrutils::phr_txt(" not found)")
            )
            return(result)
          }

          effective_design <- survey_design %||%
            private$..base_survey_design %||%
            private$..survey_design
          test_args <- list(
            survey_design = effective_design,
            variables = available_vars
          )
          if (!is.null(test_params)) {
            test_args <- private$..resolve_output_params(
              func_args = test_args,
              test_params = test_params,
              out_name = check$check_name %||% "unknown",
              skip_results_table = TRUE
            )
          }

          test_result <- do.call(test_function, test_args)

          if (is.null(test_result)) {
            result$message <- paste0(
              phrutils::phr_txt("Test function '"),
              test_func_name,
              phrutils::phr_txt("' returned NULL (internal error occurred)")
            )
            return(result)
          }

          if (
            is.list(test_result) &&
            any(c("statistic", "test_statistic") %in% names(test_result))
          ) {
            result$test_statistic <- test_result$statistic %||%
              test_result$test_statistic
            if ("p_value" %in% names(test_result)) {
              result$p_value <- test_result$p_value
            }
          } else {
            if (!is.null(test_result)) {
              result$test_statistic <- test_result
            }
          }

          if (!is.null(thresholds) && length(thresholds) > 0) {
            penalty_result <- private$..evaluate_threshold_expressions(
              test_statistic = result$test_statistic,
              p_value = result$p_value,
              thresholds = thresholds
            )
            result$penalty <- penalty_result$penalty
            result$max_penalty <- penalty_result$max_penalty
            result$threshold_expression <- penalty_result$threshold_expression
            result$message <- penalty_result$message
          }

          return(result)
        },
        on_error = "warn",
        origin = paste0(self$dataset_name, "$..execute_check")
      )
    },

    # Analysis Methods

    #' @description
    #' Create a survey design object from the stored data and variable_map.
    #' @return A srvyr survey design object, or NULL
    ..create_survey_design = function() {
      origin <- paste0(self$dataset_name, "$create_survey_design")

      if (!requireNamespace("srvyr", quietly = TRUE)) {
        phrutils::phr_warning(
          origin,
          phrutils::phr_txt("Package 'srvyr' must be installed to create survey design objects.")
        )
        return(NULL)
      }

      if (is.null(private$..data)) {
        phrutils::phr_warning(
          origin,
          phrutils::phr_txt("No data available to create survey design.")
        )
        return(NULL)
      }

      data_cols <- names(private$..data)

      cluster_col <- private$..variable_map[["cluster_id_numeric"]]
      if (is.null(cluster_col) || !cluster_col %in% data_cols) {
        cluster_col <- private$..variable_map[["cluster_id"]]
      }
      if (is.null(cluster_col) || !cluster_col %in% data_cols) {
        cluster_col <- NULL
      }

      weight_col <- private$..variable_map[["weight"]]
      if (is.null(weight_col) || !weight_col %in% data_cols) {
        weight_col <- NULL
      }

      strata_col <- private$..variable_map[["stratum"]]
      if (is.null(strata_col) || !strata_col %in% data_cols) {
        strata_col <- NULL
      }

      fpc_col <- private$..variable_map[["fpc"]]
      if (is.null(fpc_col) || !fpc_col %in% data_cols) {
        fpc_col <- NULL
      }

      if (is.null(cluster_col)) {
        phrutils::phr_message(
          origin,
          phrutils::phr_txt("No cluster column found; using ids = 1 (simple random sample design).")
        )
      }

      ids_sym <- if (!is.null(cluster_col)) rlang::sym(cluster_col) else 1
      strata_sym <- if (!is.null(strata_col)) rlang::sym(strata_col) else NULL
      weight_sym <- if (!is.null(weight_col)) rlang::sym(weight_col) else NULL
      fpc_sym <- if (!is.null(fpc_col)) rlang::sym(fpc_col) else NULL

      design <- phrutils::phr_try(
        srvyr::as_survey_design(
          .data = self$data,
          ids = !!ids_sym,
          strata = !!strata_sym,
          weights = !!weight_sym,
          fpc = !!fpc_sym,
          nest = TRUE
        ),
        on_error = "warn",
        origin = origin,
        hint = phrutils::phr_txt("Check that cluster_id_numeric (or cluster_id) and weight columns contain valid data.")
      )

      if (!is.null(design)) {
        phrutils::phr_message(origin, phrutils::phr_txt("Survey design created successfully."))
      }

      return(design)
    },

    #' @description Convert quality results to a tabular format
    #' @param results Optional flat named list of check results to convert.
    #'   Defaults to \code{self$plausibility_results} when \code{NULL}.
    #' @return A tibble with quality check results
    ..results_to_table = function(results = NULL) {
      phrutils::phr_try(
        {
          table_results <- results %||% private$..plausibility_results

          if (length(table_results) == 0) {
            return(tibble::tibble(
              check_name = character(),
              check_label = character(),
              check_group = character(),
              test_statistic = numeric(),
              p_value = numeric(),
              penalty = numeric(),
              max_penalty = numeric(),
              threshold_expression = character(),
              message = character()
            ))
          }

          rows <- lapply(names(table_results), function(nm) {
            r <- table_results[[nm]]
            tibble::tibble(
              check_name = r$check_name %||% nm,
              check_label = r$check_label %||% NA_character_,
              check_group = r$check_group %||% NA_character_,
              test_statistic = as.numeric(r$test_statistic %||% NA),
              p_value = as.numeric(r$p_value %||% NA),
              penalty = as.numeric(r$penalty %||% 0),
              max_penalty = as.numeric(r$max_penalty %||% 0),
              threshold_expression = r$threshold_expression %||% NA_character_,
              message = r$message %||% NA_character_
            )
          })

          dplyr::bind_rows(rows)
        },
        on_error = "warn",
        origin = paste0(self$dataset_name, "DataAnalytics$..results_to_table")
      )
    },

    #' @description Run quality checks on data subsets for each unique value of a grouping column
    #' @param group_col Character string. The actual column name in the data to group by.
    #' @param data Optional data frame to subset. Defaults to \code{self$data}.
    #' @param quality_schema Optional quality schema to execute. Defaults to
    #'   \code{self$quality_schema}.
    #' @param variable_map Optional named list mapping canonical names to actual
    #'   column names. Defaults to \code{self$variable_map}.
    #' @return A tibble with per-group check results, or NULL
    ..compute_results_by_group = function(
    group_col,
    data = NULL,
    quality_schema = NULL,
    variable_map = NULL
    ) {
      phrutils::phr_try(
        {
          full_data <- data %||% private$..data
          schema <- quality_schema %||% private$..quality_schema

          if (
            is.null(group_col) ||
            !nzchar(group_col) ||
            is.null(full_data) ||
            !group_col %in% names(full_data)
          ) {
            return(NULL)
          }

          group_values <- unique(full_data[[group_col]])
          group_values <- group_values[!is.na(group_values)]

          if (length(group_values) == 0) {
            return(NULL)
          }

          all_group_results <- list()

          for (gv in group_values) {
            subset_data <- full_data[full_data[[group_col]] == gv, ]
            subset_design <- phrutils::phr_try(
              srvyr::as_survey_design(.data = subset_data, ids = 1),
              on_error = "warn",
              origin = paste0(self$dataset_name, "DataAnalytics$compute_results_by_group"),
              hint = paste0(phrutils::phr_txt("Could not create survey design for group: "), gv)
            )

            group_rows <- lapply(
              names(schema),
              function(check_name) {
                check <- schema[[check_name]]
                result <- self$..execute_check(
                  check,
                  survey_design = subset_design,
                  data = subset_data,
                  variable_map = variable_map
                )
                tibble::tibble(
                  group_value = as.character(gv),
                  check_name = result$check_name %||% check_name,
                  check_label = result$check_label %||% NA_character_,
                  check_group = result$check_group %||% NA_character_,
                  test_statistic = as.numeric(result$test_statistic %||% NA),
                  p_value = as.numeric(result$p_value %||% NA),
                  penalty = as.numeric(result$penalty %||% 0),
                  max_penalty = as.numeric(result$max_penalty %||% 0)
                )
              }
            )

            all_group_results <- c(all_group_results, group_rows)
          }

          dplyr::bind_rows(all_group_results)
        },
        on_error = "warn",
        origin = paste0(self$dataset_name, "DataAnalytics$..compute_results_by_group")
      )
    },

    #' @description Generate the data analysis plan from the analysis schema
    #' @return Invisibly returns self.
    ..generate_dap_from_schema = function() {
      origin <- paste0(private$..dataset_name, "DataAnalytics$generate_dap_from_schema")

      phrutils::phr_try(
        {
          if (!is.null(private$..survey_design)) {
            available_vars <- names(private$..survey_design$variables)
          } else if (!is.null(private$..data)) {
            available_vars <- names(private$..data)
          } else {
            phrutils::phr_error(origin, phrutils::phr_txt("Neither survey_design nor data is available."))
            return(invisible(self))
          }

          translate_var <- function(canonical_name) {
            if (is.null(canonical_name) || is.na(canonical_name)) {
              return(NA_character_)
            }
            if (
              !is.null(private$..variable_map) &&
              canonical_name %in% names(self$variable_map)
            ) {
              actual_name <- private$..variable_map[[canonical_name]]
              if (!is.null(actual_name) && actual_name != "") {
                return(actual_name)
              }
            }
            return(canonical_name)
          }

          if (
            is.null(private$..analysis_schema) || nrow(private$..analysis_schema) == 0
          ) {
            phrutils::phr_warning(
              origin,
              phrutils::phr_txt("analysis_schema is empty; data_analysis_plan will remain empty.")
            )
            return(invisible(self))
          }

          schema_valid <- private$..analysis_schema |>
            dplyr::mutate(
              var_name_actual = purrr::map_chr(.data$var_name, translate_var),
              denom_var_actual = purrr::map_chr(.data$denom_var, translate_var),
              var_exists = .data$var_name_actual %in% available_vars,
              denom_exists = ifelse(
                !is.na(.data$denom_var_actual),
                .data$denom_var_actual %in% available_vars,
                TRUE
              )
            ) |>
            dplyr::mutate(include = .data$var_exists & .data$denom_exists)

          issues <- schema_valid |>
            dplyr::filter(!.data$include) |>
            dplyr::transmute(
              indicator_name = .data$indicator_name,
              issue = paste0(
                phrutils::phr_txt("Missing variable(s): "),
                ifelse(
                  !.data$var_exists,
                  paste0(
                    .data$var_name,
                    phrutils::phr_txt(" (maps to: "),
                    .data$var_name_actual,
                    ")"
                  ),
                  ""
                ),
                ifelse(
                  !.data$denom_exists & !is.na(.data$denom_var),
                  paste0(
                    ", ",
                    .data$denom_var,
                    phrutils::phr_txt(" (maps to: "),
                    .data$denom_var_actual,
                    ")"
                  ),
                  ""
                )
              )
            )

          dap_df <- schema_valid |>
            dplyr::filter(.data$include) |>
            dplyr::transmute(
              indicator_name = .data$indicator_name,
              calculation = .data$calculation,
              var_name = .data$var_name_actual,
              denom_var = .data$denom_var_actual,
              disaggregation = .data$disaggregation,
              multiplier = .data$multiplier,
              indicator_unit = .data$indicator_unit
            )

          private$..data_analysis_plan$set(field = "log_df", value = dap_df)

          if (nrow(issues) > 0) {
            private$..analysis_plan_issues_log <- dplyr::bind_rows(
              private$..analysis_plan_issues_log,
              issues
            )
            phrutils::phr_warning(
              origin,
              paste0(
                nrow(issues),
                phrutils::phr_txt(" indicators skipped due to missing variables.")
              )
            )
          } else {
            phrutils::phr_message(
              origin,
              phrutils::phr_txt("All schema indicators found and added to data_analysis_plan.")
            )
          }
        },
        on_error = "warn",
        origin = origin,
        hint = phrutils::phr_txt("Ensure survey_design$variables is accessible and schema structure is valid.")
      )

      invisible(self)
    },

    #' @description
    #' Resolve a "variable_map" role reference to its mapped dataset column name.
    #'
    #' Mirrors \code{Data$..resolve_variable_map_reference()}, but accepts an
    #' explicit \code{variable_map} argument so that \code{DataAnalytics}
    #' subclasses holding more than one variable map (e.g.
    #' \code{linked_ind_roster_variable_map}, \code{linked_ind_health_variable_map})
    #' can specify which map to resolve against.
    #'
    #' @param ref Character string to check/resolve. When \code{tags = TRUE},
    #'   this is expected to be a \code{@variable_map$role} tag (e.g.
    #'   "@variable_map$fever"). When \code{tags = FALSE}, \code{ref} is treated
    #'   directly as the role name, with no \code{@} stripping.
    #' @param tags Logical. If \code{TRUE}, \code{ref} is parsed as a
    #'   \code{@variable_map$role} tag. If \code{FALSE} (default), \code{ref} is
    #'   resolved directly as a role name.
    #' @param variable_map Optional named list mapping canonical roles to actual
    #'   column names. Defaults to \code{self$variable_map} when \code{NULL},
    #'   allowing callers to specify an alternative map (e.g. a linked dataset's
    #'   variable map) when this class holds more than one.
    #'
    #' @return A list with:
    #'   * `is_reference` - TRUE if `ref` uses the `@variable_map$` tag syntax
    #'     (when `tags = TRUE`), or always TRUE (when `tags = FALSE`).
    #'   * `role` - the extracted role name (NULL if not a reference).
    #'   * `value` - the resolved column name, or NULL if the role is not
    #'     mapped (or not a valid/non-empty mapping).
    ..resolve_variable_map_reference = function(
      ref,
      tags = FALSE,
      variable_map = NULL
    ) {
      vm <- variable_map %||% private$..variable_map

      if (!is.character(ref) || length(ref) != 1) {
        return(list(is_reference = FALSE, role = NULL, value = NULL))
      }

      if (tags) {
        if (!grepl("^@variable_map\\$", ref)) {
          return(list(is_reference = FALSE, role = NULL, value = NULL))
        }
        role <- sub("^@variable_map\\$", "", ref)
      } else {
        role <- ref
      }

      value <- vm[[role]]

      if (is.null(value) || identical(value, "")) {
        value <- NULL
      }

      list(is_reference = TRUE, role = role, value = value)
    },

    #' @description
    #' Resolve a "value_map" role (or "role$canonical_value") reference to its
    #' mapped dataset value(s).
    #'
    #' Mirrors \code{Data$..resolve_value_map_reference()}, but accepts an
    #' explicit \code{value_map} argument so that \code{DataAnalytics}
    #' subclasses holding more than one value map (e.g.
    #' \code{linked_ind_roster_value_map}, \code{linked_ind_health_value_map})
    #' can specify which map to resolve against.
    #'
    #' @param ref Character string to check/resolve. When \code{tags = TRUE},
    #'   this is expected to be a \code{@value_map$role} or
    #'   \code{@value_map$role$canonical_value} tag (e.g.
    #'   "@value_map$status$yes"). When \code{tags = FALSE}, \code{ref} is
    #'   treated directly as "role" or "role$canonical_value", with no \code{@}
    #'   stripping.
    #' @param tags Logical. If \code{TRUE}, \code{ref} is parsed as a
    #'   \code{@value_map$...} tag. If \code{FALSE} (default), \code{ref} is
    #'   resolved directly.
    #' @param value_map Optional named list mapping canonical roles to dataset
    #'   value(s). Defaults to \code{self$value_map} when \code{NULL}, allowing
    #'   callers to specify an alternative map (e.g. a linked dataset's value
    #'   map) when this class holds more than one.
    #'
    #' @return A list with:
    #'   * `is_reference` - TRUE if `ref` uses the `@value_map$` tag syntax
    #'     (when `tags = TRUE`), or always TRUE (when `tags = FALSE`).
    #'   * `role` - the extracted role name (NULL if not a reference).
    #'   * `canonical_value` - the extracted canonical value name, if provided
    #'     (NULL otherwise).
    #'   * `role_found` - TRUE if `role` exists in `value_map`.
    #'   * `value` - the resolved value(s): a single mapping (when
    #'     `canonical_value` is given), the entire role's value map (when it
    #'     is not), or NULL if the role/canonical value could not be resolved.
    ..resolve_value_map_reference = function(
      ref,
      tags = FALSE,
      value_map = NULL
    ) {
      vm <- value_map %||% private$..value_map

      if (!is.character(ref) || length(ref) != 1) {
        return(list(
          is_reference = FALSE,
          role = NULL,
          canonical_value = NULL,
          role_found = FALSE,
          value = NULL
        ))
      }

      if (tags) {
        if (!grepl("^@value_map\\$", ref)) {
          return(list(
            is_reference = FALSE,
            role = NULL,
            canonical_value = NULL,
            role_found = FALSE,
            value = NULL
          ))
        }
        ref <- sub("^@value_map\\$", "", ref)
      }

      parts <- strsplit(ref, "\\$")[[1]]
      role <- parts[1]
      canonical_value <- if (length(parts) == 2) parts[2] else NULL
      role_map <- vm[[role]]

      if (is.null(role_map)) {
        return(list(
          is_reference = TRUE,
          role = role,
          canonical_value = canonical_value,
          role_found = FALSE,
          value = NULL
        ))
      }

      value <- if (!is.null(canonical_value)) {
        role_map[[canonical_value]]
      } else {
        role_map
      }

      list(
        is_reference = TRUE,
        role = role,
        canonical_value = canonical_value,
        role_found = TRUE,
        value = value
      )
    },

    #' @description
    #' Execute a data analysis plan against a survey design
    #'
    #' Loops through each row of \code{analysis_plan}, dispatches the appropriate
    #' survey calculation function (\code{prop}, \code{mean}, \code{median},
    #' \code{ratio}, or \code{categorical}), handles optional disaggregation, and
    #' binds all results into a single tibble.
    #'
    #' Results columns are returned in the following order:
    #' \code{plan_row}, \code{variable}, \code{indicator_name},
    #' \code{indicator_unit}, \code{group_name}, \code{disaggregation_value},
    #' \code{calculation}, \code{point.estimate}, \code{lower_ci},
    #' \code{upper_ci}, \code{ci_method}, \code{n_unweighted},
    #' \code{n_weighted}, \code{denom_unweighted}, \code{denom_weighted},
    #' \code{n_eff}, \code{deff}, \code{note}.
    #'
    #' @param design A \code{srvyr} or \code{survey} design object.
    #' @param analysis_plan A data frame with columns \code{indicator_name},
    #'   \code{calculation}, \code{var_name}, \code{denom_var},
    #'   \code{disaggregation}, \code{multiplier}, and \code{indicator_unit}.
    #' @param high_design_complexity Logical; if \code{TRUE}, triggers stricter
    #'   CI selection (e.g. logit-transformed CIs for proportions).
    #' @param variable_map Optional named list mapping canonical variable roles to
    #'   actual column names, used to resolve \code{var_name}, \code{denom_var}
    #'   and \code{disaggregation} row values via
    #'   \code{private$..resolve_variable_map_reference()}. Defaults to
    #'   \code{self$variable_map} when \code{NULL}.
    #' @param value_map Optional named list mapping canonical value roles to
    #'   dataset value(s), used as a fallback to resolve \code{var_name},
    #'   \code{denom_var} and \code{disaggregation} row values via
    #'   \code{private$..resolve_value_map_reference()} when they are not found
    #'   in \code{variable_map}. Defaults to \code{self$value_map} when
    #'   \code{NULL}.
    #'
    #' @return A tibble of analysis results, one row per indicator (or one row per
    #'   disaggregation group when disaggregation is specified).
    #' @noRd
    ..phr_calc_survey_from_plan = function(
      design,
      analysis_plan,
      high_design_complexity = FALSE,
      variable_map = NULL,
      value_map = NULL
    ) {
      origin <- "phr_calc_survey_from_plan"
      phrutils::phr_message(
        origin,
        phrutils::phr_txt("Starting execution of data analysis plan...")
      )

      # --- Validation
      phrutils::phr_try(
        {
          if (is.null(design)) {
            phr_error(origin, phrutils::phr_txt("Survey design object is NULL."))
          }
          if (!inherits(analysis_plan, "data.frame")) {
            phrutils::phr_error(origin, phrutils::phr_txt("analysis_plan must be a data.frame or tibble."))
          }

          required_cols <- c(
            "indicator_name",
            "calculation",
            "var_name",
            "denom_var",
            "disaggregation",
            "multiplier",
            "indicator_unit"
          )
          missing_cols <- setdiff(required_cols, names(analysis_plan))
          if (length(missing_cols) > 0) {
            phrutils::phr_error(
              origin,
              paste(
                phrutils::phr_txt("Analysis plan missing columns:"),
                paste(missing_cols, collapse = ", ")
              )
            )
          }
        },
        on_error = "abort",
        origin = origin,
        hint = phrutils::phr_txt("Check that your data_analysis_plan follows the expected template.")
      )

      # --- Initialize results holder
      results <- list()

      # --- Loop through analysis plan
      for (i in seq_len(nrow(analysis_plan))) {
        row <- analysis_plan[i, ]
        indicator <- row$indicator_name
        calc_type <- tolower(trimws(row$calculation))

        # Resolve canonical variable names referenced in var_name, denom_var and
        # disaggregation via variable_map (preferred) or value_map (fallback).
        # No tag-stripping is performed here (tags = FALSE); the values are
        # already expected to be bare canonical names, not "@variable_map$..."
        # style tags.
        resolve_ref <- function(ref) {
          if (is.null(ref) || is.na(ref) || !nzchar(ref)) {
            return(ref)
          }
          var_ref <- private$..resolve_variable_map_reference(
            ref,
            tags = FALSE,
            variable_map = variable_map
          )
          if (!is.null(var_ref$value)) {
            return(var_ref$value)
          }
          val_ref <- private$..resolve_value_map_reference(
            ref,
            tags = FALSE,
            value_map = value_map
          )
          if (
            isTRUE(val_ref$role_found) &&
              !is.null(val_ref$value) &&
              is.character(val_ref$value) &&
              length(val_ref$value) == 1
          ) {
            return(val_ref$value)
          }
          ref
        }

        var_name <- resolve_ref(row$var_name)
        denom_var <- if (!is.null(row$denom_var) && !is.na(row$denom_var)) {
          resolve_ref(row$denom_var)
        } else {
          row$denom_var
        }
        disagg <- if (
          !is.null(row$disaggregation) && !is.na(row$disaggregation)
        ) {
          resolve_ref(row$disaggregation)
        } else {
          NULL
        }
        mult <- ifelse(
          is.null(row$multiplier) || is.na(row$multiplier),
          1,
          row$multiplier
        )
        unit <- ifelse(
          is.null(row$indicator_unit) || is.na(row$indicator_unit),
          "",
          row$indicator_unit
        )

        phrutils::phr_message(
          origin,
          paste0(
            phrutils::phr_txt("Running ["),
            i,
            "/",
            nrow(analysis_plan),
            "]: ",
            indicator,
            " (",
            calc_type,
            ")"
          )
        )

        # --- Internal helper for one calculation
        run_single_calc <- function(
          design_subset,
          group_value = NA_character_
        ) {
          result_i <- phrutils::phr_try(
            {
              if (calc_type %in% c("prop", "proportion")) {
                phr_calc_survey_prop_single(
                  design = design_subset,
                  var_name = var_name,
                  indicator_name = indicator,
                  indicator_unit = unit,
                  multiplier = mult,
                  group_name_label = group_value,
                  high_design_complexity = high_design_complexity
                )
              } else if (calc_type %in% c("mean", "average")) {
                phr_calc_survey_mean_single(
                  design = design_subset,
                  var_name = var_name,
                  indicator_name = indicator,
                  indicator_unit = unit,
                  multiplier = mult,
                  group_name_label = group_value,
                  high_design_complexity = high_design_complexity
                )
              } else if (calc_type %in% c("median")) {
                phr_calc_survey_median_single(
                  design = design_subset,
                  var_name = var_name,
                  indicator_name = indicator,
                  indicator_unit = unit,
                  multiplier = mult,
                  group_name_label = group_value,
                  high_design_complexity = high_design_complexity
                )
              } else if (calc_type %in% c("ratio", "rate")) {
                phr_calc_survey_ratio_single(
                  design = design_subset,
                  numerator_var = var_name,
                  denominator_var = denom_var,
                  indicator_name = indicator,
                  indicator_unit = unit,
                  multiplier = mult,
                  group_name_label = group_value,
                  high_design_complexity = high_design_complexity
                )
              } else if (calc_type %in% c("categorical", "category", "cat")) {
                phr_calc_survey_categorical_single(
                  design = design_subset,
                  var_name = var_name,
                  indicator_name = indicator,
                  indicator_unit = unit,
                  multiplier = mult,
                  group_name_label = group_value,
                  high_design_complexity = high_design_complexity
                )
              } else if (calc_type %in% c("select_multiple_cat")) {
                phr_calc_multiple_choice_cat(
                  design = design_subset,
                  var_name = var_name,
                  indicator_name = indicator,
                  indicator_unit = unit,
                  multiplier = mult,
                  group_name_label = group_value,
                  high_design_complexity = high_design_complexity
                )
              } else {
                phrutils::phr_warning(
                  origin,
                  paste(phrutils::phr_txt("Unknown calculation type for indicator:"), indicator)
                )
                tibble::tibble(
                  variable = var_name,
                  indicator_name = indicator,
                  indicator_unit = unit,
                  point.estimate = NA_real_,
                  lower_ci = NA_real_,
                  upper_ci = NA_real_,
                  ci_method = NA_character_,
                  n_unweighted = NA_real_,
                  n_weighted = NA_real_,
                  denom_unweighted = NA_real_,
                  denom_weighted = NA_real_,
                  n_eff = NA_real_,
                  deff = NA_real_,
                  group_name = group_value,
                  note = phrutils::phr_txt("unknown calculation type")
                )
              }
            },
            on_error = "warn",
            origin = paste0(origin, ":", indicator),
            hint = paste(
              phrutils::phr_txt("Check variable"),
              var_name,
              phrutils::phr_txt("and denominator"),
              denom_var,
              phrutils::phr_txt("if applicable.")
            )
          )

          # Add traceability columns and reorder to canonical column order:
          # plan_row, variable, indicator_name, indicator_unit, group_name,
          # disaggregation_value, calculation, point.estimate, lower_ci,
          # upper_ci, ci_method, n_unweighted, n_weighted, denom_unweighted,
          # denom_weighted, n_eff, deff, note
          result_i$plan_row <- i
          result_i$disaggregation_value <- group_value
          result_i$calculation <- calc_type

          col_order <- c(
            "plan_row",
            "variable",
            "indicator_name",
            "indicator_unit",
            "group_name",
            "disaggregation_value",
            "calculation",
            "point.estimate",
            "lower_ci",
            "upper_ci",
            "ci_method",
            "n_unweighted",
            "n_weighted",
            "denom_unweighted",
            "denom_weighted",
            "n_eff",
            "deff",
            "note"
          )
          present <- col_order[col_order %in% names(result_i)]
          extra <- setdiff(names(result_i), col_order)
          result_i[c(present, extra)]
        }

        # --- Handle disaggregated analysis
        if (!is.null(disagg) && disagg %in% names(design$variables)) {
          group_levels <- unique(na.omit(design$variables[[disagg]]))
          phrutils::phr_message(
            origin,
            paste(
              phrutils::phr_txt("Disaggregating by:"),
              disagg,
              "(",
              length(group_levels),
              phrutils::phr_txt("groups )")
            )
          )

          group_results <- purrr::map_dfr(group_levels, function(g) {
            subset_design <- tryCatch(
              subset(design, design$variables[[disagg]] == g),
              error = function(e) {
                phrutils::phr_warning(
                  origin,
                  paste(phrutils::phr_txt("Subset failed for"), disagg, "=", g)
                )
                NULL
              }
            )
            if (!is.null(subset_design)) {
              run_single_calc(subset_design, group_value = as.character(g))
            } else {
              tibble::tibble()
            }
          })

          results[[i]] <- group_results
        } else {
          results[[i]] <- run_single_calc(design, group_value = "Overall")
        }
      }

      # --- Bind and return
      out <- tryCatch(
        dplyr::bind_rows(results),
        error = function(e) {
          phrutils::phr_warning(origin, paste(phrutils::phr_txt("Binding failed:"), e$message))
          dplyr::tibble()
        }
      )

      phrutils::phr_message(
        origin,
        paste(phrutils::phr_txt("Completed"), nrow(analysis_plan), phrutils::phr_txt("indicators successfully."))
      )
      return(out)
    },

    # Resolve a field-name set (as returned by the pre_run_* hooks) into the
    # actual objects stored on self.
    #
    # Each element of `set` is normally a length-1 character string naming a
    # public field on self. Elements listed in `keep_as_is` are passed through
    # unchanged, as are non-character elements, allowing hooks to supply
    # objects directly.
    #
    # @param set A named list of field names (or literal objects).
    # @param keep_as_is Character vector of element names passed through as-is.
    # @return A named list of resolved objects.
    ..resolve_field_set = function(set, keep_as_is = character(0)) {
      resolved <- list()

      for (nm in names(set)) {
        val <- set[[nm]]

        if (is.null(val)) {
          next
        } else if (nm %in% keep_as_is) {
          resolved[[nm]] <- val
        } else if (is.character(val) && length(val) == 1) {
          if (!is.null(self[[val]])) {
            resolved[[nm]] <- self[[val]]
          } else if (!is.null(private[[val]])) {
            resolved[[nm]] <- private[[val]]
          }
        } else {
          resolved[[nm]] <- val
        }
      }

      resolved
    },

    # Run all quality checks for a single resolved input set.
    #
    # Executes every check in the set's quality schema against the set's data
    # and (unweighted) survey design, then generates the penalty summary
    # tables. Tables are stored in self$tables[["plausibility"]] for the
    # default single main set, or in self$tables[[set_role]][["plausibility"]]
    # when nested.
    #
    # @param inputs Named list of resolved inputs: data, quality_schema,
    #   base_survey_design, survey_design and variable_map.
    # @param set_role Character role name for this input set.
    # @param nested Logical; TRUE when results are stored under set_role.
    # @return A named list of check results.
    .run_quality_checks_for_set = function(inputs, set_role, nested) {
      quality_schema <- inputs$quality_schema

      if (is.null(quality_schema) || length(quality_schema) == 0) {
        phrutils::phr_warning(
          message = phrutils::phr_txt(
            "No quality checks defined in schema for '{set_role}'."
          ),
          origin = private$..dataset_name
        )
        return(list())
      }

      check_data <- inputs$data
      vm <- inputs$variable_map %||% private$..variable_map %||% list()

      base_design <- inputs$base_survey_design
      if (is.null(base_design) && !is.null(check_data)) {
        base_design <- phrutils::phr_try(
          srvyr::as_survey_design(.data = check_data, ids = 1),
          on_error = "warn",
          origin = paste0(self$dataset_name, "$run_quality_checks"),
          hint = paste0(
            phrutils::phr_txt("Could not create base (unweighted) survey design for '"),
            set_role,
            "'."
          )
        )
      }
      effective_design <- base_design %||% inputs$survey_design

      results <- list()

      for (check_name in names(quality_schema)) {
        check <- quality_schema[[check_name]]
        result <- self$..execute_check(
          check,
          survey_design = effective_design,
          data = check_data,
          variable_map = vm
        )
        results[[check_name]] <- result
      }

      # --- Plausibility tables
      store_tbl <- function(key, tbl) {
        if (is.null(tbl)) {
          return(invisible(NULL))
        }
        if (nested) {
          if (is.null(self$tables[[set_role]])) {
            self$tables[[set_role]] <- list()
          }
          if (is.null(self$tables[[set_role]][["plausibility"]])) {
            self$tables[[set_role]][["plausibility"]] <- list()
          }
          self$tables[[set_role]][["plausibility"]][[key]] <- tbl
        } else {
          if (is.null(self$tables[["plausibility"]])) {
            self$tables[["plausibility"]] <- list()
          }
          self$tables[["plausibility"]][[key]] <- tbl
        }
        invisible(NULL)
      }

      if (!nested && is.null(self$tables[["plausibility"]])) {
        self$tables[["plausibility"]] <- list()
      }

      results_df <- private$..results_to_table(results = results)
      penalty_tbl <- table_quality_penalty_summary(
        results_df,
        title_name = phrutils::phr_txt("Data Quality Penalty Summary")
      )
      store_tbl("penalty_summary", penalty_tbl)

      for (vm_role in c("enum_id", "stratum")) {
        col_name <- vm[[vm_role]]
        if (
          !is.null(col_name) &&
            nzchar(col_name) &&
            !is.null(check_data) &&
            col_name %in% names(check_data)
        ) {
          per_group_df <- private$..compute_results_by_group(
            col_name,
            data = check_data,
            quality_schema = quality_schema,
            variable_map = vm
          )
          if (!is.null(per_group_df) && nrow(per_group_df) > 0) {
            tbl_key <- paste0("penalty_summary_by_", vm_role)
            group_label <- if (vm_role == "enum_id") {
              phrutils::phr_txt("Enumerator ID")
            } else {
              phrutils::phr_txt("Stratum")
            }
            tbl_title <- if (vm_role == "enum_id") {
              phrutils::phr_txt("Data Quality Penalty Summary by Enumerator")
            } else {
              phrutils::phr_txt("Data Quality Penalty Summary by Stratum")
            }
            per_group_tbl <- table_quality_penalty_summary_by_group(
              per_group_df,
              group_col = "group_value",
              group_label = group_label,
              title_name = tbl_title
            )
            store_tbl(tbl_key, per_group_tbl)

            group_values <- sort(unique(per_group_df$group_value))
            for (gv in group_values) {
              gv_label <- as.character(gv)
              gv_safe <- gsub("[^A-Za-z0-9]", "_", gv_label)
              gv_results <- per_group_df |>
                dplyr::filter(.data$group_value == gv) |>
                dplyr::select(-"group_value")
              gv_title <- if (vm_role == "enum_id") {
                paste0(
                  phrutils::phr_txt("Data Quality Penalty Summary - Enumerator: "),
                  gv_label
                )
              } else {
                paste0(phrutils::phr_txt("Data Quality Penalty Summary - Stratum: "), gv_label)
              }
              gv_tbl_key <- paste0("penalty_summary_", vm_role, "_", gv_safe)
              gv_tbl <- table_quality_penalty_summary(
                gv_results,
                title_name = gv_title
              )
              store_tbl(gv_tbl_key, gv_tbl)
            }
          }
        }
      }

      results
    },

    # Run the analysis plan for a single resolved input set.
    #
    # Executes every indicator twice: once using the set's full survey design
    # (weighted) and once using a simple unweighted design built from the
    # set's data.
    #
    # @param inputs Named list of resolved inputs: data, survey_design and
    #   data_analysis_plan.
    # @param set_role Character role name for this input set.
    # @param nested Logical; TRUE when multiple sets are being processed.
    # @param origin Character origin label for messages.
    # @return A named list with elements survey_design and base, or NULL when
    #   the set could not be analysed.
    ..run_analysis_for_set = function(inputs, set_role, nested, origin) {
      if (is.null(inputs$survey_design)) {
        if (nested) {
          phrutils::phr_warning(
            origin,
            phrutils::phr_txt(
              "Survey design not set for '{set_role}'. Skipping."
            )
          )
        } else {
          phrutils::phr_error(origin, phrutils::phr_txt("Survey design not set."))
        }
        return(NULL)
      }

      dap <- inputs$data_analysis_plan
      dap_df <- if (is.data.frame(dap)) {
        dap
      } else if (!is.null(dap) && !is.null(dap$get("log_df"))) {
        dap$get("log_df")
      } else {
        NULL
      }

      if (is.null(dap_df) || nrow(dap_df) == 0) {
        if (nested) {
          phrutils::phr_warning(
            origin,
            phrutils::phr_txt(
              "No data_analysis_plan provided for '{set_role}'. Skipping."
            )
          )
        } else {
          phrutils::phr_error(origin, phrutils::phr_txt("No data_analysis_plan provided."))
        }
        return(NULL)
      }

      survey_design_results <- phrutils::phr_try(
        private$..phr_calc_survey_from_plan(
          design = inputs$survey_design,
          analysis_plan = dap_df,
          variable_map = inputs$variable_map
        ),
        on_error = "warn",
        origin = origin,
        hint = phrutils::phr_txt("Verify all variables exist and analysis plan is valid.")
      )

      base_results <- NULL
      if (!is.null(inputs$data)) {
        base_design <- phrutils::phr_try(
          srvyr::as_survey_design(.data = inputs$data, ids = 1),
          on_error = "warn",
          origin = origin,
          hint = phrutils::phr_txt("Could not create base (unweighted) survey design from the set's data.")
        )

        if (!is.null(base_design)) {
          base_results <- phrutils::phr_try(
            private$..phr_calc_survey_from_plan(
              design = base_design,
              analysis_plan = dap_df,
              variable_map = inputs$variable_map
            ),
            on_error = "warn",
            origin = origin,
            hint = phrutils::phr_txt("Verify all variables exist in the base (unweighted) design.")
          )
        }
      }

      list(
        survey_design = survey_design_results,
        base = base_results
      )
    },

    # Run all outputs for a single resolved input set.
    #
    # Contains the per-output logic used by \code{run_outputs()}: resolves the
    # first positional argument from \code{dataset_type}, resolves
    # \code{test_params} references, calls the output function, and stores
    # results in \code{self$visualizations} / \code{self$tables} (nested under
    # \code{set_role} when \code{nested} is TRUE).
    #
    # The outputs schema is a data frame and is iterated row by row; the
    # comma-separated \code{variables} / \code{disaggregation} cells and the
    # \code{test_params} cell are parsed per row.
    #
    # @param inputs Named list of resolved inputs: data, outputs_schema,
    #   base_survey_design, survey_design and variable_map.
    # @param set_role Character role name for this input set.
    # @param nested Logical; TRUE when results are stored under set_role.
    # @param language Character language for auto-generated titles.
    # @return Invisibly returns NULL.
    ..run_outputs_for_set = function(
      inputs,
      set_role,
      nested,
      language = "english"
    ) {
      outputs_schema <- inputs$outputs_schema

      if (is.null(outputs_schema) || nrow(outputs_schema) == 0) {
        phrutils::phr_warning(
          message = phrutils::phr_txt(
            "No outputs defined in outputs schema for '{set_role}'. Skipping."
          ),
          origin = self$dataset_name
        )
        return(invisible(NULL))
      }

      base_design <- inputs$base_survey_design
      if (is.null(base_design) && !is.null(inputs$data)) {
        base_design <- phrutils::phr_try(
          srvyr::as_survey_design(.data = inputs$data, ids = 1),
          on_error = "warn",
          origin = paste0(self$dataset_name, "$run_outputs"),
          hint = paste0(
            phrutils::phr_txt("Could not create base (unweighted) survey design for '"),
            set_role,
            "'."
          )
        )
      }
      weighted_design <- inputs$survey_design

      vm <- inputs$variable_map %||% private$..variable_map %||% list()

      # Resolve the plausibility results for this set (used by @results_table).
      plaus_results <- private$..plausibility_results
      if (
        nested &&
          is.list(plaus_results) &&
          set_role %in% names(plaus_results)
      ) {
        plaus_results <- plaus_results[[set_role]]
      }

      phrutils::phr_message(
        phrutils::phr_txt(
        "Running {nrow(outputs_schema)} output(s) for {self$dataset_name} ('{set_role}')..."
      ))

      # Read a single schema cell, returning NULL when the column is absent or
      # the value is missing/empty so downstream guards stay simple.
      cell <- function(col, i) {
        if (!col %in% names(outputs_schema)) {
          return(NULL)
        }
        value <- outputs_schema[[col]][[i]]
        if (is.null(value) || length(value) != 1 || is.na(value)) {
          return(NULL)
        }
        if (is.character(value) && !nzchar(value)) {
          return(NULL)
        }
        value
      }

      for (i in seq_len(nrow(outputs_schema))) {
        out_name <- cell("output_title", i) %||% paste0("output_", i)
        output_type <- cell("output_type", i)
        test_params <- outputs_parse_test_params(cell("test_params", i))

        phrutils::phr_try(
          {
            func_name <- cell("output_func_name", i)
            if (is.null(func_name)) {
              phrutils::phr_warning(
                message = phrutils::phr_txt(
                  "Output '{out_name}' has no output_func_name specified. Skipping."
                ),
                origin = self$dataset_name
              )
              next
            }

            output_function <- NULL
            if (requireNamespace("phr", quietly = TRUE)) {
              tryCatch(
                {
                  ns <- asNamespace("phr")
                  if (
                    exists(
                      func_name,
                      envir = ns,
                      mode = "function",
                      inherits = FALSE
                    )
                  ) {
                    output_function <- get(
                      func_name,
                      envir = ns,
                      mode = "function",
                      inherits = FALSE
                    )
                  }
                },
                error = function(e) NULL
              )
            }
            if (is.null(output_function)) {
              tryCatch(
                {
                  if (exists(func_name, mode = "function", inherits = TRUE)) {
                    output_function <- get(
                      func_name,
                      mode = "function",
                      inherits = TRUE
                    )
                  }
                },
                error = function(e) NULL
              )
            }
            if (is.null(output_function)) {
              phrutils::phr_warning(
                message = phrutils::phr_txt(
                  "Function '{func_name}' for output '{out_name}' not found. Skipping."
                ),
                origin = self$dataset_name
              )
              next
            }

            # Determine first positional argument based on dataset_type column.
            # Supported values:
            #   "base"          – base_design (unweighted); also the default
            #   "survey_design" – weighted_design (weighted)
            dataset_type <- cell("dataset_type", i) %||% "base"

            first_arg <- switch(
              dataset_type,
              "base" = base_design,
              "survey_design" = weighted_design,
              base_design # default: "base" (unweighted survey design)
            )

            func_args <- list(first_arg)

            # Check if any test_param uses @results_table; if so, replace first arg
            if (length(test_params) > 0) {
              for (arg_name_check in names(test_params)) {
                arg_val_check <- test_params[[arg_name_check]]
                if (
                  is.character(arg_val_check) &&
                    arg_val_check == "@results_table"
                ) {
                  func_args[[1]] <- private$..results_to_table(
                    results = plaus_results
                  )
                  break
                }
              }
            }

            if (length(test_params) > 0) {
              func_args <- private$..resolve_output_params(
                func_args = func_args,
                test_params = test_params,
                out_name = out_name,
                skip_results_table = TRUE,
                variable_map = vm
              )
            }

            # Storage key: use output_name field; fall back to the output title
            label <- cell("output_name", i) %||% out_name

            # Auto-inject title_name unless already supplied in test_params
            if (is.null(func_args[["title_name"]])) {
              title_field <- switch(
                language,
                "french" = "output_title_french",
                "arabic" = "output_title_arabic",
                "output_title_english"
              )
              auto_title <- cell(title_field, i) %||% cell("output_title", i)
              if (!is.null(auto_title)) {
                func_args[["title_name"]] <- auto_title
              }
            }

            group <- cell("outputs_group", i)

            store_result <- function(result, key) {
              # Output functions wrap their bodies in phr_try(on_error = "warn")
              # and therefore return NULL on failure. Assigning NULL into a list
              # deletes the element, which would silently leave behind an empty
              # output_group node, so bail out before any node is created.
              if (is.null(result)) {
                phrutils::phr_warning(
                  message = phrutils::phr_txt(
                    "Output '{out_name}' returned NULL (the output function failed). ",
                    "Nothing stored for '{key}'."
                  ),
                  origin = self$dataset_name
                )
                return(invisible(NULL))
              }

              if (!is.null(output_type) && output_type == "table") {
                if (nested) {
                  if (is.null(self$tables[[set_role]])) {
                    self$tables[[set_role]] <- list()
                  }
                  if (!is.null(group)) {
                    if (is.null(self$tables[[set_role]][[group]])) {
                      self$tables[[set_role]][[group]] <- list()
                    }
                    self$tables[[set_role]][[group]][[key]] <- result
                  } else {
                    self$tables[[set_role]][[key]] <- result
                  }
                } else if (!is.null(group)) {
                  if (is.null(self$tables[[group]])) {
                    self$tables[[group]] <- list()
                  }
                  self$tables[[group]][[key]] <- result
                } else {
                  self$tables[[key]] <- result
                }
                phrutils::phr_message(phrutils::phr_txt(
                  "Table '{key}' stored successfully."
                ))
              } else if (
                !is.null(output_type) &&
                  output_type == "visualization"
              ) {
                if (nested) {
                  if (is.null(self$visualizations[[set_role]])) {
                    self$visualizations[[set_role]] <- list()
                  }
                  if (!is.null(group)) {
                    if (is.null(self$visualizations[[set_role]][[group]])) {
                      self$visualizations[[set_role]][[group]] <- list()
                    }
                    self$visualizations[[set_role]][[group]][[key]] <- result
                  } else {
                    self$visualizations[[set_role]][[key]] <- result
                  }
                } else if (!is.null(group)) {
                  if (is.null(self$visualizations[[group]])) {
                    self$visualizations[[group]] <- list()
                  }
                  self$visualizations[[group]][[key]] <- result
                } else {
                  self$visualizations[[key]] <- result
                }
                phrutils::phr_message(phrutils::phr_txt(
                  "Visualization '{key}' stored successfully."
                ))
              } else {
                phrutils::phr_warning(
                  message = phrutils::phr_txt(
                    "Output '{out_name}' has unrecognized output_type '{output_type}'. ",
                    "Expected 'visualization' or 'table'. Result not stored."
                  ),
                  origin = self$dataset_name
                )
              }
            }

            per_group_ref <- cell("outputs_per_group", i)

            if (!is.null(per_group_ref)) {
              if (grepl("^@variable_map\\$", per_group_ref)) {
                role <- sub("^@variable_map\\$", "", per_group_ref)
                col_name <- vm[[role]]
                var_label <- role
                if (is.null(col_name)) {
                  phrutils::phr_warning(
                    message = phrutils::phr_txt(
                      "outputs_per_group role '{role}' not found in variable_map for output '{out_name}'. Skipping."
                    ),
                    origin = self$dataset_name
                  )
                  col_name <- NULL
                }
              } else {
                col_name <- per_group_ref
                var_label <- per_group_ref
              }

              # Determine the data frame used for finding unique group values
              # by extracting the variables tibble from the relevant design.
              design_obj <- if (dataset_type == "survey_design") {
                weighted_design
              } else {
                base_design
              }
              source_df <- if (!is.null(design_obj)) {
                tryCatch(design_obj$variables, error = function(e) NULL)
              } else {
                NULL
              }

              if (
                !is.null(col_name) &&
                  !is.null(source_df) &&
                  col_name %in% names(source_df)
              ) {
                unique_vals <- unique(source_df[[col_name]])
                unique_vals <- unique_vals[!is.na(unique_vals)]

                phrutils::phr_message(phrutils::phr_txt(
                  "Calling {func_name} for output '{out_name}' across {length(unique_vals)} group(s) of '{var_label}'..."
                ))

                for (val in unique_vals) {
                  design_to_filter <- if (dataset_type == "survey_design") {
                    weighted_design
                  } else {
                    base_design
                  }
                  filtered_first_arg <- tryCatch(
                    dplyr::filter(
                      design_to_filter,
                      !!rlang::sym(col_name) == val
                    ),
                    error = function(e) {
                      phrutils::phr_warning(
                        message = phrutils::phr_txt(
                          "Failed to filter survey design for '{var_label}' == '{val}': {e$message}"
                        ),
                        origin = self$dataset_name
                      )
                      NULL
                    }
                  )

                  if (is.null(filtered_first_arg)) {
                    next
                  }

                  per_group_args <- func_args
                  per_group_args[[1]] <- filtered_first_arg
                  amended_label <- paste0(label, "-", var_label, ".", val)

                  phrutils::phr_try(
                    {
                      output_result <- do.call(
                        output_function,
                        per_group_args
                      )
                      store_result(output_result, amended_label)
                    },
                    on_error = "warn",
                    origin = paste0(
                      self$dataset_name,
                      "DataAnalytics$run_outputs$",
                      out_name,
                      "$",
                      val
                    )
                  )
                }
              } else if (!is.null(col_name)) {
                phrutils::phr_warning(
                  message = phrutils::phr_txt(
                    "Column '{col_name}' for outputs_per_group not found in source data ({dataset_type}) for output '{out_name}'. Skipping."
                  ),
                  origin = self$dataset_name
                )
              }
            } else {
              phrutils::phr_message(phrutils::phr_txt(
                "Calling {func_name} for output '{out_name}'..."
              ))
              output_result <- do.call(output_function, func_args)
              store_result(output_result, label)
            }
          },
          on_error = "warn",
          origin = paste0(
            self$dataset_name,
            "DataAnalytics$run_outputs$",
            set_role,
            "$",
            out_name
          )
        )
      }
      invisible(NULL)
    },

    # Amend one data / variable_map / data_analysis_plan field set for
    # add_all_to_dap().
    #
    # @param set Named list with elements data, variable_map and
    #   data_analysis_plan giving public field names on self.
    # @param set_name Character; label for the set (used in messages).
    # @param origin Character; origin label for messages/warnings.
    # @return Invisibly NULL.
    ..add_all_to_dap_single = function(set, set_name, origin) {
      if (
        !is.list(set) ||
          is.null(set$data) ||
          is.null(set$variable_map) ||
          is.null(set$data_analysis_plan)
      ) {
        phrutils::phr_warning(
          origin,
          paste0(
            phrutils::phr_txt("Field set '"),
            set_name,
            phrutils::phr_txt("' must contain 'data', 'variable_map' and 'data_analysis_plan' field names. Skipping.")
          )
        )
        return(invisible(NULL))
      }

      df <- self[[set$data]]
      if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
        phrutils::phr_warning(
          origin,
          paste0(
            phrutils::phr_txt("Field set '"),
            set_name,
            phrutils::phr_txt("': field '"),
            set$data,
            phrutils::phr_txt("' is not a non-empty data frame. Skipping.")
          )
        )
        return(invisible(NULL))
      }

      vm <- self[[set$variable_map]] %||% list()

      if (is.null(self[[set$data_analysis_plan]])) {
        self[[set$data_analysis_plan]] <- QuantDataAnalysisPlanLog$new()
      }
      dap <- self[[set$data_analysis_plan]]

      status <- private$..classify_columns_for_dap(df, vm, dap)

      phrutils::phr_message(
        origin,
        paste0(
          phrutils::phr_txt("Field set '"),
          set_name,
          "': ",
          length(status$all_cols),
          phrutils::phr_txt(" column(s) in data. "),
          length(status$in_both),
          phrutils::phr_txt(" already in variable_map and data_analysis_plan ("),
          paste(status$in_both, collapse = ", "),
          "); ",
          length(status$in_map_not_dap),
          phrutils::phr_txt(" in variable_map only ("),
          paste(status$in_map_not_dap, collapse = ", "),
          "); ",
          length(status$in_neither),
          phrutils::phr_txt(" in neither ("),
          paste(status$in_neither, collapse = ", "),
          ")."
        )
      )

      # Resolve a valid stratum column (if defined in the variable_map) so
      # newly added rows can be replicated with stratum as disaggregation.
      stratum_col <- vm[["stratum"]]
      if (
        !is.character(stratum_col) ||
          length(stratum_col) != 1 ||
          !nzchar(stratum_col) ||
          !stratum_col %in% names(df)
      ) {
        stratum_col <- NULL
      }

      add_with_stratum <- function(indicator_name, guess, col) {
        dap$add_indicator(
          indicator_name = indicator_name,
          calculation = guess$calculation,
          var_name = col,
          multiplier = guess$multiplier,
          indicator_unit = guess$indicator_unit
        )
        if (!is.null(stratum_col) && !identical(col, stratum_col)) {
          dap$add_indicator(
            indicator_name = indicator_name,
            calculation = guess$calculation,
            var_name = col,
            disaggregation = stratum_col,
            multiplier = guess$multiplier,
            indicator_unit = guess$indicator_unit
          )
        }
      }

      # (c) In variable_map but not in the data_analysis_plan: add to the
      #     plan using the existing variable_map reference.
      for (col in status$in_map_not_dap) {
        guess <- private$..guess_dap_calculation(df[[col]])
        if (is.null(guess)) {
          phrutils::phr_message(
            origin,
            paste0(
              phrutils::phr_txt("Skipping column '"),
              col,
              phrutils::phr_txt("' (character with > 20 unique values; likely metadata or uuid).")
            )
          )
          next
        }
        add_with_stratum(status$map_roles[[col]], guess, col)
      }

      # (d) In neither: add as a novel variable_map entry keyed by the
      #     column name, then add to the data_analysis_plan.
      for (col in status$in_neither) {
        # Skip flag_ columns
        if (startsWith(col, "flag_")) {
          next
        }
        guess <- private$..guess_dap_calculation(df[[col]])
        if (is.null(guess)) {
          phrutils::phr_message(
            origin,
            paste0(
              phrutils::phr_txt("Skipping column '"),
              col,
              phrutils::phr_txt("' (character with > 20 unique values; likely metadata or uuid).")
            )
          )
          next
        }
        vm[[col]] <- col
        add_with_stratum(col, guess, col)
      }

      self[[set$variable_map]] <- vm

      invisible(NULL)
    },

    # Classify data columns by their presence in the variable_map and the
    # data_analysis_plan.
    #
    # @param df Data frame whose columns should be classified.
    # @param variable_map Named list mapping canonical roles to column names.
    # @param dap QuantDataAnalysisPlanLog object (or NULL).
    # @return A list with elements all_cols, in_both, in_map_not_dap,
    #   in_neither, and map_roles (named list of column name -> canonical role).
    ..classify_columns_for_dap = function(df, variable_map, dap) {
      all_cols <- names(df)

      map_roles <- list()
      for (role in names(variable_map)) {
        cols <- variable_map[[role]]
        if (is.null(cols)) {
          next
        }
        for (col in as.character(cols)) {
          if (nzchar(col) && is.null(map_roles[[col]])) {
            map_roles[[col]] <- role
          }
        }
      }

      dap_vars <- character(0)
      if (!is.null(dap)) {
        dap_df <- dap$get("log_df")
        if (
          !is.null(dap_df) &&
            nrow(dap_df) > 0 &&
            "var_name" %in% names(dap_df)
        ) {
          dap_vars <- unique(dap_df$var_name)
          dap_vars <- dap_vars[!is.na(dap_vars)]
        }
      }

      in_map <- all_cols[all_cols %in% names(map_roles)]
      in_dap <- all_cols[all_cols %in% dap_vars]
      in_both <- intersect(in_map, in_dap)
      in_map_not_dap <- setdiff(in_map, in_dap)
      in_neither <- setdiff(all_cols, union(in_map, in_dap))

      list(
        all_cols = all_cols,
        in_both = in_both,
        in_map_not_dap = in_map_not_dap,
        in_neither = in_neither,
        map_roles = map_roles
      )
    },

    # Best-guess the calculation type for a data column.
    #
    # Rules:
    # * numeric (or safely coercible to numeric) with only 0/1 values -> prop
    # * numeric (or safely coercible to numeric) beyond 0/1           -> mean
    # * character with comma- or space-separated values -> select_multiple_cat
    # * character otherwise                             -> cat
    # * character, not comma/space separated, with more than 20 unique
    #   values (likely metadata or uuid fields)         -> NULL (skip)
    #
    # @param values Vector of column values.
    # @return A list with elements calculation, multiplier, indicator_unit,
    #   or NULL when the column should be skipped.
    ..guess_dap_calculation = function(values) {
      # Skip date/time columns entirely
      if (inherits(values, c("Date", "POSIXct", "POSIXt"))) {
        return(NULL)
      }

      cat_guess <- list(
        calculation = "cat",
        multiplier = 100,
        indicator_unit = "%"
      )

      if (is.list(values)) {
        # List columns hold multiple values per row, so they are treated as
        # select-multiple categorical variables.
        return(list(
          calculation = "select_multiple_cat",
          multiplier = 100,
          indicator_unit = "%"
        ))
      }

      vals <- values[!is.na(values)]
      if (length(vals) == 0) {
        return(cat_guess)
      }

      vals_chr <- trimws(as.character(vals))
      vals_chr <- vals_chr[nzchar(vals_chr)]
      if (length(vals_chr) == 0) {
        return(cat_guess)
      }

      # Skip geopoint-style values: four space-separated numeric fields
      if (
        all(grepl(
          "^\\s*-?\\d+(\\.\\d+)?(\\s+-?\\d+(\\.\\d+)?){3}\\s*$",
          vals_chr
        ))
      ) {
        return(NULL)
      }

      vals_num <- suppressWarnings(as.numeric(vals_chr))
      if (!anyNA(vals_num)) {
        if (all(vals_num %in% c(0, 1))) {
          return(list(
            calculation = "prop",
            multiplier = 100,
            indicator_unit = "%"
          ))
        }
        return(list(
          calculation = "mean",
          multiplier = 1,
          indicator_unit = "score"
        ))
      }

      if (any(grepl("[ ,]", vals_chr))) {
        return(list(
          calculation = "select_multiple_cat",
          multiplier = 100,
          indicator_unit = "%"
        ))
      }

      if (length(unique(vals_chr)) > 20) {
        return(NULL)
      }

      cat_guess
    },

    # Resolve @ references in test_params into concrete values for do.call()
    #
    # @param func_args Named list; positional/named args accumulated so far.
    # @param test_params Named list of raw parameter values (may contain @-refs).
    # @param out_name Character; output name used in warning messages.
    # @param skip_results_table Logical; if TRUE, @results_table entries are
    #   skipped as named args (they were already handled as first positional arg).
    # @param variable_map Named list mapping canonical roles to column names.
    #   Defaults to self$variable_map. Subclasses holding several datasets (e.g.
    #   linked roster / deaths) must pass the map belonging to the input set
    #   being processed, otherwise @variable_map$ references resolve against the
    #   wrong dataset's columns.
    # @return Updated func_args list.
    ..resolve_output_params = function(
      func_args,
      test_params,
      out_name,
      skip_results_table = FALSE,
      variable_map = NULL
    ) {
      variable_map <- variable_map %||% private$..variable_map %||% list()

      for (arg_name in names(test_params)) {
        arg_value <- test_params[[arg_name]]

        # c(...) vector format
        if (is.character(arg_value) && grepl("^c\\(", arg_value)) {
          vec_content <- sub("^c\\(", "", sub("\\)$", "", arg_value))
          vec_elements <- .parse_indicator_arguments(vec_content)

          resolved_elements <- character()
          for (elem in vec_elements) {
            elem <- trimws(elem)
            if (grepl("^@variable_map\\$", elem)) {
              role <- sub("^@variable_map\\$", "", elem)
              resolved <- variable_map[[role]]
              if (!is.null(resolved)) {
                resolved_elements <- c(resolved_elements, resolved)
              } else {
                phrutils::phr_warning(
                  message = phrutils::phr_txt(
                    "Variable map role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'."
                  ),
                  origin = self$dataset_name
                )
              }
            } else if (grepl("^@value_map\\$", elem)) {
              parts <- strsplit(sub("^@value_map\\$", "", elem), "\\$")[[1]]
              if (length(parts) >= 1) {
                role <- parts[1]
                if (!is.null(self$value_map[[role]])) {
                  if (length(parts) == 2) {
                    resolved <- self$value_map[[role]][[parts[2]]]
                    if (!is.null(resolved)) {
                      resolved_elements <- c(resolved_elements, resolved)
                    }
                  } else {
                    resolved_elements <- c(
                      resolved_elements,
                      unlist(self$value_map[[role]], use.names = FALSE)
                    )
                  }
                }
              }
            } else if (grepl("^@variable_label\\$", elem)) {
              role <- sub("^@variable_label\\$", "", elem)
              resolved <- self$variable_label[[role]]
              if (!is.null(resolved)) {
                resolved_elements <- c(resolved_elements, resolved)
              } else {
                phrutils::phr_warning(
                  message = phrutils::phr_txt(
                    "Variable label role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'."
                  ),
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
                      phrutils::phr_warning(
                        message = phr_txt(glue::glue(
                          "Value label '{elem}' not found in vector argument '{arg_name}' for output '{out_name}'."
                        )),
                        origin = self$dataset_name
                      )
                    }
                  } else {
                    resolved_elements <- c(
                      resolved_elements,
                      unlist(self$value_label[[role]], use.names = FALSE)
                    )
                  }
                } else {
                  phrutils::phr_warning(
                    message = phrutils::phr_txt(
                      "Value label role '{role}' not found in vector argument '{arg_name}' for output '{out_name}'."
                    ),
                    origin = self$dataset_name
                  )
                }
              }
            } else {
              resolved_elements <- c(
                resolved_elements,
                gsub("^['\"]|['\"]$", "", elem)
              )
            }
          }

          numeric_attempt <- suppressWarnings(as.numeric(resolved_elements))
          if (length(resolved_elements) > 0 && !any(is.na(numeric_attempt))) {
            func_args[[arg_name]] <- numeric_attempt
          } else if (
            length(resolved_elements) > 0 &&
              all(toupper(resolved_elements) %in% c("TRUE", "FALSE"))
          ) {
            func_args[[arg_name]] <- as.logical(resolved_elements)
          } else {
            func_args[[arg_name]] <- resolved_elements
          }
        } else if (
          is.character(arg_value) && grepl("^@variable_label\\$", arg_value)
        ) {
          role <- sub("^@variable_label\\$", "", arg_value)
          resolved <- private$..variable_label[[role]]
          if (!is.null(resolved)) {
            func_args[[arg_name]] <- resolved
          } else {
            phrutils::phr_warning(
              message = phrutils::phr_txt(
                "Variable label role '{role}' not found for output '{out_name}'. Skipping argument '{arg_name}'."
              ),
              origin = private$..dataset_name
            )
          }
        } else if (is.character(arg_value) && arg_value == "@results_table") {
          if (!skip_results_table) {
            func_args[[arg_name]] <- private$..results_to_table()
          }
          # else: already handled as first positional arg in run_outputs
        } else if (
          is.character(arg_value) && grepl("^@value_label\\$", arg_value)
        ) {
          parts <- strsplit(sub("^@value_label\\$", "", arg_value), "\\$")[[1]]
          if (length(parts) >= 1) {
            role <- parts[1]
            if (!is.null(private$..value_label[[role]])) {
              resolved <- if (length(parts) == 2) {
                private$..value_label[[role]][[parts[2]]]
              } else {
                private$..value_label[[role]]
              }
              func_args[[arg_name]] <- resolved %||% arg_value
            } else {
              func_args[[arg_name]] <- arg_value
            }
          }
        } else if (
          is.character(arg_value) && grepl("^@variable_map\\$", arg_value)
        ) {
          role <- sub("^@variable_map\\$", "", arg_value)
          resolved <- variable_map[[role]]
          if (!is.null(resolved)) func_args[[arg_name]] <- resolved
        } else if (
          is.character(arg_value) && grepl("^@value_map\\$", arg_value)
        ) {
          parts <- strsplit(sub("^@value_map\\$", "", arg_value), "\\$")[[1]]
          if (length(parts) >= 1) {
            role <- parts[1]
            if (!is.null(private$..value_map[[role]])) {
              resolved <- if (length(parts) == 2) {
                private$..value_map[[role]][[parts[2]]]
              } else {
                private$..value_map[[role]]
              }
              func_args[[arg_name]] <- resolved %||% arg_value
            } else {
              func_args[[arg_name]] <- arg_value
            }
          }
        } else {
          if (is.character(arg_value)) {
            arg_value <- gsub("^['\"]|['\"]$", "", arg_value)
          }
          func_args[[arg_name]] <- arg_value
        }
      }

      return(func_args)
    },

    # Build a DAP tibble from an analysis schema, resolving canonical variable names.
    #
    # Translates each \code{var_name} and \code{denom_var} in \code{analysis_schema}
    # through \code{variable_map}, then keeps only indicators whose resolved columns
    # are present in \code{available_vars}.
    #
    # @param analysis_schema A tibble with analysis schema columns.
    # @param variable_map Named list mapping canonical names to actual column names.
    # @param available_vars Character vector of column names available in the target dataset.
    # @param dataset_label Character label used in the returned issues tibble (e.g. "roster").
    # @return A named list with elements \code{dap_df} (tibble) and \code{issues} (tibble).
    ..build_dap_from_schema = function(analysis_schema, variable_map, available_vars,
                                       dataset_label = "dataset") {

      vm <- variable_map %||% list()

      translate_var <- function(canonical_name) {
        if (is.null(canonical_name) || is.na(canonical_name)) return(NA_character_)
        if (canonical_name %in% names(vm)) {
          actual <- vm[[canonical_name]]
          if (!is.null(actual) && nzchar(actual)) return(actual)
        }
        return(canonical_name)
      }

      schema_valid <- analysis_schema |>
        dplyr::mutate(
          var_name_actual  = purrr::map_chr(.data$var_name,  translate_var),
          denom_var_actual = purrr::map_chr(.data$denom_var, translate_var),
          var_exists       = .data$var_name_actual %in% available_vars,
          denom_exists     = ifelse(
            !is.na(.data$denom_var_actual),
            .data$denom_var_actual %in% available_vars,
            TRUE
          )
        ) |>
        dplyr::mutate(include = .data$var_exists & .data$denom_exists)

      issues <- schema_valid |>
        dplyr::filter(!.data$include) |>
        dplyr::transmute(
          dataset        = dataset_label,
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

      dap_df <- schema_valid |>
        dplyr::filter(.data$include) |>
        dplyr::transmute(
          indicator_name = .data$indicator_name,
          calculation    = .data$calculation,
          var_name       = .data$var_name_actual,
          denom_var      = .data$denom_var_actual,
          disaggregation = .data$disaggregation,
          multiplier     = .data$multiplier,
          indicator_unit = .data$indicator_unit
        )

      list(dap_df = dap_df, issues = issues)
    },

    #' @description Add an indicator to the data analysis plan
    #' @param indicator_name Character; human-readable label for the indicator.
    #' @param calculation Character; one of "prop", "mean", "median", "ratio", or "categorical".
    #' @param var_name Character; dataset column name for the primary variable.
    #' @param denom_var Character or NULL; denominator column name.
    #' @param disaggregation Character or NULL; column name to use for stratified analysis.
    #' @param multiplier Numeric; scale factor for the point estimate.
    #' @param indicator_unit Character; unit label.
    #' @param high_design_complexity Logical; reserved for future use.
    #' @param note Character or NULL; optional free-text note.
    #' @return Invisibly returns self.
    ..add_indicator_dap = function(
    indicator_name,
    calculation,
    var_name,
    denom_var = NULL,
    disaggregation = NULL,
    multiplier = 100,
    indicator_unit = "%",
    high_design_complexity = FALSE,
    note = NULL
    ) {
      origin <- paste0(self$dataset_name, "DataAnalytics$add_indicator_dap")

      phrutils::phr_try(
        {
          private$..data_analysis_plan$add_indicator(
            indicator_name = indicator_name,
            calculation = calculation,
            var_name = var_name,
            denom_var = denom_var %||% NA_character_,
            disaggregation = disaggregation %||% NA_character_,
            multiplier = multiplier,
            indicator_unit = indicator_unit
          )
          phrutils::phr_message(
            origin,
            paste(phrutils::phr_txt("Added indicator:"), indicator_name)
          )
        },
        on_error = "warn",
        origin = origin,
        hint = phrutils::phr_txt("Check variable names and calculation type.")
      )
    },

    #' @description Remove an indicator from the data analysis plan
    #' @param indicator_name Character; label of the indicator to remove.
    #' @return Invisibly returns self.
    ..remove_indicator_dap = function(indicator_name) {
      origin <- paste0(self$dataset_name, "DataAnalytics$remove_indicator_dap")

      phrutils::phr_try(
        {
          if (
            is.null(private$..data_analysis_plan) ||
            nrow(private$..data_analysis_plan$get("log_df")) == 0
          ) {
            phrutils::phr_warning(origin, phrutils::phr_txt("No data_analysis_plan loaded."))
            return(invisible(self))
          }
          dap_df <- private$..data_analysis_plan$get("log_df")
          private$..data_analysis_plan$set(
            field = "log_df",
            value = dap_df[dap_df$indicator_name != indicator_name, ]
          )
          phrutils::phr_message(
            origin,
            paste(phrutils::phr_txt("Removed indicator:"), indicator_name)
          )
        },
        on_error = "warn",
        origin = origin,
        hint = phtutils::phr_txt("Ensure indicator_name exists in data_analysis_plan.")
      )
    },

    # Create a proper survey design for an arbitrary data frame using variable_map.
    #
    # Applies the same logic as \code{create_survey_design()} (which operates on
    # \code{self$data} and \code{self$variable_map}) but accepts any data frame and
    # variable map. Reads \code{cluster_id_numeric} / \code{cluster_id},
    # \code{weight}, \code{stratum}, and \code{fpc} from \code{variable_map} and
    # builds an \code{srvyr} survey design accordingly. Falls back to a simple
    # random sample (\code{ids = 1}) when none of those columns are present.
    #
    # @param data A data frame for the linked dataset.
    # @param variable_map Named list mapping canonical names to actual column names.
    # @param origin Character; caller label used in log/warning messages.
    # @return An \code{srvyr} survey design object, or NULL on failure.
    ..create_survey_design_for_dataset = function(data, variable_map, origin = NULL) {

      origin <- origin %||% paste0(self$dataset_name, "$.create_survey_design_for_dataset")

      if (is.null(data) || nrow(data) == 0) {
        phrutils::phr_warning(origin, phrutils::phr_txt("No data available to create survey design for linked dataset."))
        return(NULL)
      }

      vm        <- variable_map %||% list()
      data_cols <- names(data)

      cluster_col <- vm[["cluster_id_numeric"]]
      if (is.null(cluster_col) || !cluster_col %in% data_cols) {
        cluster_col <- vm[["cluster_id"]]
      }
      if (is.null(cluster_col) || !cluster_col %in% data_cols) {
        cluster_col <- NULL
      }

      weight_col <- vm[["weight"]]
      if (is.null(weight_col) || !weight_col %in% data_cols) weight_col <- NULL

      strata_col <- vm[["stratum"]]
      if (is.null(strata_col) || !strata_col %in% data_cols) strata_col <- NULL

      fpc_col    <- vm[["fpc"]]
      if (is.null(fpc_col) || !fpc_col %in% data_cols) fpc_col <- NULL

      if (is.null(cluster_col)) {
        phrutils::phr_message(origin, phrutils::phr_txt("No cluster column found for linked dataset; using ids = 1 (simple random sample design)."))
      }

      ids_sym    <- if (!is.null(cluster_col)) rlang::sym(cluster_col) else 1
      strata_sym <- if (!is.null(strata_col))  rlang::sym(strata_col)  else NULL
      weight_sym <- if (!is.null(weight_col))  rlang::sym(weight_col)  else NULL
      fpc_sym    <- if (!is.null(fpc_col))     rlang::sym(fpc_col)     else NULL

      design <- phrutils::phr_try(
        srvyr::as_survey_design(
          .data   = data,
          ids     = !!ids_sym,
          strata  = !!strata_sym,
          weights = !!weight_sym,
          fpc     = !!fpc_sym,
          nest    = TRUE
        ),
        on_error = "warn",
        origin   = origin,
        hint     = phrutils::phr_txt("Check that cluster_id_numeric (or cluster_id) and weight columns contain valid data in the linked dataset.")
      )

      if (!is.null(design)) {
        phrutils::phr_message(origin, "Survey design created successfully for linked dataset.")
      }

      design
    }
  )
)
