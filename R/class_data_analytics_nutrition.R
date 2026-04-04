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
#'
#' @field quality_schema_anthro Quality check schema for anthropometric data
#' @field quality_schema_iycf Quality check schema for IYCF data
#' @field plausibility_results_anthro Results of anthropometric quality checks
#' @field plausibility_results_iycf Results of IYCF quality checks
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
                          value_label = NULL) {

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

      # Initialise separate result containers
      self$plausibility_results_anthro <- list()
      self$plausibility_results_iycf   <- list()

      phr_message(
        phr_txt(glue::glue("{dataset_name} initialized as NutritionDataAnalytics object."))
      )
    },

    #' @description Load the default anthropometric quality schema from template file
    #' @return A list of anthropometric quality checks
    default_quality_anthro_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_anthropometric_template.xlsx",
        package = "iphRa"
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
        package = "iphRa"
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
          phr_txt(glue::glue(
            "Ran {length(anthro_results)} anthropometric and ",
            "{length(iycf_results)} IYCF quality checks for {self$dataset_name}."
          ))
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
        package = "iphRa"
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
        package = "iphRa"
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
              gv_results <- per_group_df %>%
                dplyr::filter(group_value == gv) %>%
                dplyr::select(-group_value)
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
    }
  )
)
