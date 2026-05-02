#' IPHRA Nutrition Data Analytics Class
#'
#' The `NutritionDataAnalytics` R6 class extends `DataAnalytics` for
#' Nutrition-specific quantitative analysis, including anthropometric data.
#'
#' @description
#' This class provides Nutrition-specific (including anthropometric) functionality:
#' * Quantitative analysis indicators via analysis_schema
#' * Quality checks via two separate schemas:
#'   - Anthropometric plausibility (`quality_schema_anthro`)
#'   - IYCF plausibility (`quality_schema_iycf`)
#' * All visualizations/tables via outputs_schema
#' * MUAC age-adjusted weights via `weights_muac_alt` (ratio of expected to
#'   observed proportion of 6-23 vs 24-59 month children)
#'
#' @field quality_schema_anthro Quality check schema for anthropometric data
#' @field quality_schema_iycf Quality check schema for IYCF data
#' @field plausibility_results_anthro Results of anthropometric quality checks
#' @field plausibility_results_iycf Results of IYCF quality checks
#' @field weights_muac_alt Column name in \code{self$data} holding the
#'   MUAC age-adjustment weights (expected proportion / sample proportion for
#'   each 6-23 month and 24-59 month age group).
#'
#' @seealso [DataAnalytics]
#' @export
NutritionDataAnalytics <- R6::R6Class(
  classname = "NutritionDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    # Nutrition-specific quality schema fields
    quality_schema_anthro    = NULL,
    quality_schema_iycf      = NULL,

    # Nutrition-specific plausibility result fields
    plausibility_results_anthro = NULL,
    plausibility_results_iycf   = NULL,

    # MUAC age-adjustment weight column name
    weights_muac_alt = NULL,

    #' @description
    #' Initialize a new NutritionDataAnalytics object
    #'
    #' @param data A data frame (standardized or clean nutrition data)
    #' @param dap Optional data analysis plan (tibble)
    #' @param parent_data_object The Data object that generated this
    #' @param dataset_name A name for this analytics assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @param variable_label Variable labels from Data object
    #' @param value_label Value labels from Data object
    #' @param expected_prop_6_23 Numeric (0, 1); expected proportion of children
    #'   aged 6-23 months among all 6-59 month children. Defaults to \code{2/3}.
    #'   The complementary proportion \code{1 - expected_prop_6_23} is used for
    #'   the 24-59 month group.
    #' @return A new NutritionDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "NutritionDataAnalytics",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          expected_prop_6_23 = 2 / 3) {

      super$initialize(
        data = data,
        dap = dap,
        parent_data_object = parent_data_object,
        dataset_name = dataset_name,
        data_stage_name = data_stage_name,
        data_hash = data_hash,
        variable_map = variable_map,
        value_map = value_map,
        variable_label = variable_label,
        value_label = value_label
      )

      # Load the two nutrition-specific quality schemas
      self$quality_schema_anthro       <- self$default_quality_anthro_schema()
      self$quality_schema_iycf         <- self$default_quality_iycf_schema()

      # Initialize separate result containers
      self$plausibility_results_anthro <- list()
      self$plausibility_results_iycf   <- list()

      # Compute MUAC age-adjustment weights and attach to data
      private$.compute_weights_muac_alt(expected_prop_6_23)

      phr_message(
        phr_txt(glue::glue("{dataset_name} initialized as NutritionDataAnalytics object."))
      )
    },

    #' @description
    #' Run the quantitative analysis plan, with optional MUAC age-adjustment weights.
    #'
    #' When \code{muac_age_weights = TRUE}, any indicator in the analysis plan
    #' whose \code{var_name} resolves to a \code{variable_map} role containing
    #' \code{"muac_cat"} is analysed using a combined weight column equal to the
    #' original survey weight multiplied by \code{weights_muac_alt}.  All other
    #' indicators are analysed with the standard survey design unchanged.
    #'
    #' @param muac_age_weights Logical (default \code{FALSE}).  Set to \code{TRUE}
    #'   to apply MUAC age-adjustment weights for \code{muac_cat} indicators.
    #' @return Invisibly returns \code{self}.
    run_analysis = function(muac_age_weights = FALSE) {

      origin <- paste0(self$dataset_name, "$run_analysis")

      # ---- Guard: fall back to base class when age-weights not requested ------
      if (!isTRUE(muac_age_weights) || is.null(self$weights_muac_alt)) {
        if (isTRUE(muac_age_weights) && is.null(self$weights_muac_alt)) {
          phr_warning(
            message = "muac_age_weights requested but weights_muac_alt could not be computed (age column missing or insufficient data). Proceeding without age adjustment.",
            origin  = origin
          )
        }
        return(super$run_analysis())
      }

      phr_message(origin, "Running analysis plan with MUAC age-adjustment weights...")

      if (is.null(self$survey_design)) {
        phr_error(origin, "Survey design not set.")
        return(invisible(self))
      }

      if (is.null(self$data_analysis_plan) || nrow(self$data_analysis_plan$log_df) == 0) {
        phr_error(origin, "No data_analysis_plan provided.")
        return(invisible(self))
      }

      # ---- Identify muac_cat variable column names via variable_map -----------
      muac_cat_cols <- character(0)
      if (!is.null(self$variable_map) && length(self$variable_map) > 0) {
        muac_roles <- names(self$variable_map)[grepl("muac_cat", names(self$variable_map), fixed = TRUE)]
        if (length(muac_roles) > 0) {
          mapped <- unlist(self$variable_map[muac_roles])
          muac_cat_cols <- unique(mapped[!is.na(mapped) & nzchar(mapped)])
        }
      }
      # Also treat any DAP var_name that itself contains "muac_cat"
      dap_muac_direct <- self$data_analysis_plan$log_df$var_name[
        grepl("muac_cat", self$data_analysis_plan$log_df$var_name, fixed = TRUE)
      ]
      muac_cat_cols <- unique(c(muac_cat_cols, dap_muac_direct))

      dap_full     <- self$data_analysis_plan$log_df
      muac_rows    <- dap_full[dap_full$var_name %in% muac_cat_cols, , drop = FALSE]
      non_muac_rows <- dap_full[!dap_full$var_name %in% muac_cat_cols, , drop = FALSE]

      # ---- Build the combined-weight survey design ----------------------------
      combined_survey_design <- private$.build_combined_weight_design()

      # ---- Run analysis on non-muac rows with standard design -----------------
      sd_results_non_muac <- NULL
      if (nrow(non_muac_rows) > 0 && !is.null(self$survey_design)) {
        sd_results_non_muac <- phr_try(
          phr_calc_survey_from_plan(
            design        = self$survey_design,
            analysis_plan = non_muac_rows
          ),
          on_error = "warn",
          origin   = origin,
          hint     = "Verify all variables exist and analysis plan is valid."
        )
      }

      # ---- Run analysis on muac rows with combined-weight design --------------
      sd_results_muac <- NULL
      if (nrow(muac_rows) > 0 && !is.null(combined_survey_design)) {
        sd_results_muac <- phr_try(
          phr_calc_survey_from_plan(
            design        = combined_survey_design,
            analysis_plan = muac_rows
          ),
          on_error = "warn",
          origin   = origin,
          hint     = "Verify muac_cat variables and weights_muac_alt column are valid."
        )
      }

      # ---- Merge survey design results ----------------------------------------
      sd_parts <- Filter(Negate(is.null), list(sd_results_non_muac, sd_results_muac))
      survey_design_results <- if (length(sd_parts) > 0) dplyr::bind_rows(sd_parts) else NULL

      # ---- Base (unweighted) analysis – run full plan -------------------------
      base_results <- NULL
      if (!is.null(self$data)) {
        base_design <- phr_try(
          srvyr::as_survey_design(.data = self$data, ids = 1),
          on_error = "warn",
          origin   = origin,
          hint     = "Could not create base (unweighted) survey design from self$data."
        )
        if (!is.null(base_design)) {
          base_results <- phr_try(
            phr_calc_survey_from_plan(
              design        = base_design,
              analysis_plan = dap_full
            ),
            on_error = "warn",
            origin   = origin,
            hint     = "Verify all variables exist in the base (unweighted) design."
          )
        }
      }

      self$analysis_results <- list(
        survey_design = survey_design_results,
        base          = base_results
      )

      phr_message(origin, "Analysis with MUAC age-adjustment weights completed successfully.")
      invisible(self)
    },

    #' @description
    #' Diagnose issues in both anthropometric and IYCF quality schemas.
    #'
    #' Runs the standard quality_diagnose logic on both \code{quality_schema_anthro}
    #' and \code{quality_schema_iycf} and returns a combined tibble with a
    #' \code{schema_type} column indicating which schema each check belongs to.
    #' Results are stored in \code{self$quality_issues_log}.
    #'
    #' @return A tibble (invisibly) with one row per quality check, covering
    #'   both anthropometric and IYCF schemas.
    quality_diagnose = function() {

      origin <- paste0(self$dataset_name, "$quality_diagnose")

      phr_try({

        empty_row <- tibble::tibble(
          schema_type         = character(),
          check_group         = character(),
          check_name          = character(),
          check_label         = character(),
          variables           = character(),
          statistical_test    = character(),
          test_params         = character(),
          n_thresholds        = integer(),
          variables_in_data   = logical(),
          missing_variables   = character(),
          function_available  = logical(),
          thresholds_valid    = logical(),
          status              = character()
        )

        data_cols <- if (!is.null(self$data)) names(self$data) else character(0)

        # Internal helper: diagnose one schema
        diagnose_schema <- function(schema, schema_label) {

          if (is.null(schema) || length(schema) == 0) {
            phr_warning(
              message = glue::glue("No {schema_label} quality schema defined. Skipping."),
              origin  = origin
            )
            return(empty_row)
          }

          rows <- list()

          for (check_name in names(schema)) {
            check <- schema[[check_name]]

            check_group      <- check$check_group      %||% NA_character_
            check_label      <- check$check_label      %||% NA_character_
            statistical_test <- check$statistical_test %||% NA_character_
            variables        <- check$variables        %||% character(0)
            test_params      <- check$test_params      %||% list()
            thresholds       <- check$thresholds       %||% list()

            mapped_vars  <- self$.translate_canonical_to_actual_vars(variables)
            missing_vars <- setdiff(mapped_vars, data_cols)
            vars_in_data <- length(missing_vars) == 0

            func_available <- FALSE
            if (!is.na(statistical_test) && nzchar(statistical_test)) {
              func_name <- paste0("quality_test_", statistical_test)
              if (requireNamespace("phr", quietly = TRUE)) {
                tryCatch({
                  ns <- asNamespace("phr")
                  func_available <- exists(func_name, envir = ns, mode = "function", inherits = FALSE)
                }, error = function(e) {})
              }
              if (!func_available) {
                tryCatch({
                  func_available <- exists(func_name, mode = "function", inherits = TRUE)
                }, error = function(e) {})
              }
            }

            thresholds_valid <- TRUE
            if (length(thresholds) > 0) {
              for (thr in thresholds) {
                expr_str <- thr$threshold_expression %||% thr$expression
                if (!is.null(expr_str) && !is.na(expr_str) && nzchar(expr_str)) {
                  parsed <- tryCatch(parse(text = expr_str), error = function(e) NULL)
                  if (is.null(parsed)) {
                    thresholds_valid <- FALSE
                    break
                  }
                }
              }
            }

            issues <- character(0)
            if (!vars_in_data)     issues <- c(issues, paste0("missing variables: ", paste(missing_vars, collapse = ", ")))
            if (!func_available)   issues <- c(issues, paste0("function not found: quality_test_", statistical_test %||% "NA"))
            if (!thresholds_valid) issues <- c(issues, "invalid threshold expression(s)")
            status <- if (length(issues) == 0) "ok" else paste(issues, collapse = "; ")

            rows[[length(rows) + 1]] <- tibble::tibble(
              schema_type        = schema_label,
              check_group        = check_group,
              check_name         = check_name,
              check_label        = check_label,
              variables          = paste(variables, collapse = ", "),
              statistical_test   = statistical_test %||% NA_character_,
              test_params        = if (length(test_params) > 0) paste(names(test_params), test_params, sep = "=", collapse = ", ") else NA_character_,
              n_thresholds       = length(thresholds),
              variables_in_data  = vars_in_data,
              missing_variables  = if (length(missing_vars) > 0) paste(missing_vars, collapse = ", ") else NA_character_,
              function_available = func_available,
              thresholds_valid   = thresholds_valid,
              status             = status
            )
          }

          if (length(rows) > 0) dplyr::bind_rows(rows) else empty_row
        }

        anthro_result <- diagnose_schema(self$quality_schema_anthro, "anthropometric")
        iycf_result   <- diagnose_schema(self$quality_schema_iycf,   "iycf")

        result <- dplyr::bind_rows(anthro_result, iycf_result)
        self$quality_issues_log <- result

        n_issues <- sum(result$status != "ok", na.rm = TRUE)
        phr_message(phr_txt(glue::glue(
          "quality_diagnose complete: {nrow(result)} check(s) reviewed ({nrow(anthro_result)} anthropometric, {nrow(iycf_result)} IYCF), {n_issues} issue(s) found for {self$dataset_name}."
        )))

        invisible(result)

      }, on_error = "warn", origin = origin)
    },

    #' @description Load the default anthropometric quality schema from template file
    #' @return A list of anthropometric quality checks
    default_quality_anthro_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_anthropometric_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_anthropometric_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "NutritionDataAnalytics$default_quality_anthro_schema",
        hint     = "Check that quality_schema_data_quality_anthropometric_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      schema_with_metadata <- quality_table_to_schema(df)

      if (!is.null(schema_with_metadata) && !is.null(schema_with_metadata$checks)) {
        return(schema_with_metadata$checks)
      }

      return(list())
    },

    #' @description Load the default IYCF quality schema from template file
    #' @return A list of IYCF quality checks
    default_quality_iycf_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_iycf_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_iycf_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "NutritionDataAnalytics$default_quality_iycf_schema",
        hint     = "Check that quality_schema_data_quality_iycf_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      schema_with_metadata <- quality_table_to_schema(df)

      if (!is.null(schema_with_metadata) && !is.null(schema_with_metadata$checks)) {
        return(schema_with_metadata$checks)
      }

      return(list())
    },

    #' @description Run quality checks for both anthropometric and IYCF schemas
    #'
    #' Executes all checks defined in `quality_schema_anthro` and
    #' `quality_schema_iycf` separately, storing results in
    #' `plausibility_results_anthro` and `plausibility_results_iycf`
    #' respectively. Combined results are also written to `plausibility_results`
    #' so that inherited helpers (e.g. `calculate_overall_score`,
    #' `results_to_table`) continue to work as expected.
    #'
    #' @return A named list with elements `anthro` and `iycf`, each containing
    #'   the check results for that schema (invisibly)
    run_quality_checks = function() {

      phr_try({

        anthro_results <- private$.run_checks_for_schema(
          schema          = self$quality_schema_anthro,
          table_namespace = "plausibility_anthro",
          schema_label    = "Anthropometric"
        )

        iycf_results <- private$.run_checks_for_schema(
          schema          = self$quality_schema_iycf,
          table_namespace = "plausibility_iycf",
          schema_label    = "IYCF"
        )

        self$plausibility_results_anthro <- anthro_results
        self$plausibility_results_iycf   <- iycf_results

        # Combine into the inherited plausibility_results so that
        # calculate_overall_score() and results_to_table() still work
        self$plausibility_results <- c(anthro_results, iycf_results)
        self$calculate_overall_score()

        phr_message(
          phr_txt(glue::glue("Ran {length(anthro_results)} anthropometric and {length(iycf_results)} IYCF quality checks for {self$dataset_name}."))
        )

        invisible(list(anthro = anthro_results, iycf = iycf_results))

      }, on_error = "warn", origin = paste0(self$dataset_name, "$run_quality_checks"))
    },

    #' @description Load the default Nutrition analysis schema from template file
    #' @return A tibble containing the Nutrition analysis schema
    default_analysis_schema = function() {

      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_nutrition_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_nutrition_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "NutritionDataAnalytics$default_analysis_schema",
        hint     = "Check that analysis_schema_quant_data_analysis_nutrition_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description Load the default Nutrition unified outputs schema from template file
    #' @return A list of Nutrition-specific outputs definitions
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_nutrition_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_nutrition_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "NutritionDataAnalytics$default_outputs_schema",
        hint     = "Check that outputs_schema_data_analytics_nutrition_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    }
  ),

  private = list(

    # Run all checks in `schema` and generate penalty tables under
    # `self$tables[[table_namespace]]`.  Returns the named results list.
    .run_checks_for_schema = function(schema, table_namespace, schema_label) {

      if (is.null(schema) || length(schema) == 0) {
        phr_warning(
          message = glue::glue("No quality checks defined in {schema_label} schema."),
          origin  = self$dataset_name
        )
        return(list())
      }

      results <- list()

      for (check_name in names(schema)) {
        check            <- schema[[check_name]]
        result           <- self$execute_check(check)
        results[[check_name]] <- result
      }

      # --- Penalty tables --------------------------------------------------
      if (is.null(self$tables[[table_namespace]])) {
        self$tables[[table_namespace]] <- list()
      }

      # Temporarily swap quality_schema so that inherited helpers
      # (results_to_table, .compute_results_by_group) operate on this schema
      original_schema  <- self$quality_schema
      original_results <- self$plausibility_results
      self$quality_schema      <- schema
      self$plausibility_results <- results
      on.exit({
        self$quality_schema       <- original_schema
        self$plausibility_results <- original_results
      }, add = TRUE)

      results_df  <- self$results_to_table()
      penalty_tbl <- table_quality_penalty_summary(
        results_df,
        title_name = glue::glue("{schema_label} Data Quality Penalty Summary")
      )
      if (!is.null(penalty_tbl)) {
        self$tables[[table_namespace]][["penalty_summary"]] <- penalty_tbl
      }

      for (role in c("enum_id", "stratum")) {
        col_name <- self$get_variable(role)
        if (!is.null(col_name) && nzchar(col_name) &&
            col_name %in% names(self$data)) {
          per_group_df <- self$.compute_results_by_group(col_name)
          if (!is.null(per_group_df) && nrow(per_group_df) > 0) {
            tbl_key     <- paste0("penalty_summary_by_", role)
            group_label <- if (role == "enum_id") "Enumerator ID" else "Stratum"
            tbl_title   <- if (role == "enum_id") {
              glue::glue("{schema_label} Data Quality Penalty Summary by Enumerator")
            } else {
              glue::glue("{schema_label} Data Quality Penalty Summary by Stratum")
            }
            per_group_tbl <- table_quality_penalty_summary_by_group(
              per_group_df,
              group_col   = "group_value",
              group_label = group_label,
              title_name  = tbl_title
            )
            if (!is.null(per_group_tbl)) {
              self$tables[[table_namespace]][[tbl_key]] <- per_group_tbl
            }

            group_values <- sort(unique(per_group_df$group_value))
            for (gv in group_values) {
              gv_label   <- as.character(gv)
              gv_safe    <- gsub("[^A-Za-z0-9]", "_", gv_label)
              gv_results <- per_group_df |>
              dplyr::filter(.data$group_value == gv) |>
                dplyr::select(-"group_value")
              gv_title <- if (role == "enum_id") {
                glue::glue("{schema_label} Data Quality Penalty Summary - Enumerator: {gv_label}")
              } else {
                glue::glue("{schema_label} Data Quality Penalty Summary - Stratum: {gv_label}")
              }
              gv_tbl_key <- paste0("penalty_summary_", role, "_", gv_safe)
              gv_tbl <- table_quality_penalty_summary(gv_results, title_name = gv_title)
              if (!is.null(gv_tbl)) {
                self$tables[[table_namespace]][[gv_tbl_key]] <- gv_tbl
              }
            }
          }
        }
      }

      results
    },

    # Compute the weights_muac_alt column and register it in data / variable_map.
    #
    # For children aged 6-23 months:
    #   weights_muac_alt = expected_prop_6_23 / sample_prop_6_23
    # For children aged 24-59 months:
    #   weights_muac_alt = (1 - expected_prop_6_23) / sample_prop_24_59
    # All other rows receive NA.
    #
    # After adding the column, self$data is updated and the survey_design is
    # regenerated so the new column is available downstream.
    .compute_weights_muac_alt = function(expected_prop_6_23 = 2 / 3) {

      origin <- paste0(self$dataset_name, "$initialize$.compute_weights_muac_alt")

      if (is.null(self$data)) {
        return(invisible(NULL))
      }

      # Resolve the age-in-months column via variable_map
      age_col <- self$variable_map[["age_months"]]
      if (is.null(age_col) || !age_col %in% names(self$data)) {
        phr_message(
          origin,
          "No age_months column found in data; weights_muac_alt will not be computed."
        )
        return(invisible(NULL))
      }

      age_vec <- suppressWarnings(as.numeric(self$data[[age_col]]))

      in_6_23  <- !is.na(age_vec) & age_vec >= 6  & age_vec < 24
      in_24_59 <- !is.na(age_vec) & age_vec >= 24 & age_vec < 60

      n_6_23  <- sum(in_6_23)
      n_24_59 <- sum(in_24_59)
      n_total <- n_6_23 + n_24_59

      if (n_total == 0) {
        phr_message(
          origin,
          "No children aged 6-59 months found; weights_muac_alt will not be computed."
        )
        return(invisible(NULL))
      }

      sample_prop_6_23  <- n_6_23  / n_total
      sample_prop_24_59 <- n_24_59 / n_total

      expected_prop_24_59 <- 1 - expected_prop_6_23

      alt_weights <- rep(NA_real_, nrow(self$data))

      if (sample_prop_6_23 > 0) {
        alt_weights[in_6_23]  <- expected_prop_6_23  / sample_prop_6_23
      }
      if (sample_prop_24_59 > 0) {
        alt_weights[in_24_59] <- expected_prop_24_59 / sample_prop_24_59
      }

      self$data[["weights_muac_alt"]] <- alt_weights

      # Register the new column in variable_map
      self$variable_map[["weights_muac_alt"]] <- "weights_muac_alt"

      # Store the column name in the public field
      self$weights_muac_alt <- "weights_muac_alt"

      # Refresh the survey design to include the new column
      self$survey_design <- self$create_survey_design()

      phr_message(
        origin,
        glue::glue(
          "weights_muac_alt computed: {n_6_23} children 6-23 months ",
          "(sample prop: {round(sample_prop_6_23, 3)}, expected: {round(expected_prop_6_23, 3)}), ",
          "{n_24_59} children 24-59 months ",
          "(sample prop: {round(sample_prop_24_59, 3)}, expected: {round(expected_prop_24_59, 3)})."
        )
      )

      invisible(NULL)
    },

    # Build a survey design that uses the combined weight:
    #   combined_weight = original_weight * weights_muac_alt
    #
    # Returns NULL when essential columns are missing.
    .build_combined_weight_design = function() {

      origin <- paste0(self$dataset_name, "$run_analysis$.build_combined_weight_design")

      if (is.null(self$data)) return(NULL)
      if (is.null(self$weights_muac_alt)) return(NULL)

      alt_col <- self$weights_muac_alt  # column name = "weights_muac_alt"
      if (!alt_col %in% names(self$data)) return(NULL)

      weight_col <- self$variable_map[["weight"]]
      if (!is.null(weight_col) && weight_col %in% names(self$data)) {
        combined_wt <- self$data[[weight_col]] * self$data[[alt_col]]
      } else {
        combined_wt <- self$data[[alt_col]]
      }

      tmp_col <- ".combined_muac_weight"
      modified_data <- self$data
      modified_data[[tmp_col]] <- combined_wt

      cluster_col <- self$variable_map[["cluster_id_numeric"]]
      if (is.null(cluster_col) || !cluster_col %in% names(modified_data)) {
        cluster_col <- self$variable_map[["cluster_id"]]
      }
      if (is.null(cluster_col) || !cluster_col %in% names(modified_data)) {
        cluster_col <- NULL
      }

      strata_col <- self$variable_map[["stratum"]]
      if (is.null(strata_col) || !strata_col %in% names(modified_data)) strata_col <- NULL

      fpc_col <- self$variable_map[["fpc"]]
      if (is.null(fpc_col) || !fpc_col %in% names(modified_data)) fpc_col <- NULL

      ids_sym    <- if (!is.null(cluster_col)) rlang::sym(cluster_col) else 1
      strata_sym <- if (!is.null(strata_col))  rlang::sym(strata_col)  else NULL
      weight_sym <- rlang::sym(tmp_col)
      fpc_sym    <- if (!is.null(fpc_col))     rlang::sym(fpc_col)     else NULL

      design <- phr_try(
        srvyr::as_survey_design(
          .data   = modified_data,
          ids     = !!ids_sym,
          strata  = !!strata_sym,
          weights = !!weight_sym,
          fpc     = !!fpc_sym,
          nest    = TRUE
        ),
        on_error = "warn",
        origin   = origin,
        hint     = "Check that the combined weight column and cluster/strata columns contain valid data."
      )

      return(design)
    }
  )
)
