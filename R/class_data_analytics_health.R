#' IPHRA Health Data Analytics Class
#'
#' The `HealthDataAnalytics` R6 class extends `DataAnalytics` to provide
#' Health-specific quality checks and quantitative analysis in a single
#' integrated object.
#'
#' @description
#' This class provides Health-specific:
#' * Quality checks (facility access, utilization, insurance, maternal health) via quality_schema
#' * Quantitative analysis indicators via analysis_schema
#' * All visualizations/tables (quality and analysis) via outputs_schema
#'
#' @field linked_ind_roster_data Optional linked roster dataframe
#' @field linked_ind_roster_data_stage_name Name of linked roster data stage
#' @field linked_ind_roster_data_hash Hash of linked roster data
#' @field linked_ind_roster_variable_map Variable mappings for linked roster data
#' @field linked_ind_roster_value_map Value mappings for linked roster data
#' @field linked_ind_health_data Optional linked health individual dataframe
#' @field linked_ind_health_data_stage_name Name of linked health individual data stage
#' @field linked_ind_health_data_hash Hash of linked health individual data
#' @field linked_ind_health_variable_map Variable mappings for linked health individual data
#' @field linked_ind_health_value_map Value mappings for linked health individual data
#'
#' @seealso [DataAnalytics]
#' @export
HealthDataAnalytics <- R6::R6Class(
  classname = "HealthDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    #' @description
    #' Initialize a new HealthDataAnalytics object
    #'
    #' @param data A data frame (standardized or clean health data)
    #' @param dap Optional data analysis plan (tibble)
    #' @param parent_data_object The Data object that generated this
    #' @param dataset_name A name for this analytics assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @param variable_label Variable labels from Data object
    #' @param value_label Value labels from Data object
    #' @param quality_schema Optional quality check schema
    #' @param linked_ind_roster_data Optional linked roster dataframe
    #' @param linked_ind_roster_data_stage_name Name of linked roster data stage
    #' @param linked_ind_roster_data_hash Hash of linked roster data
    #' @param linked_ind_roster_variable_map Variable mappings for linked roster data
    #' @param linked_ind_roster_value_map Value mappings for linked roster data
    #' @param linked_ind_health_data Optional linked health individual dataframe
    #' @param linked_ind_health_data_stage_name Name of linked health data stage
    #' @param linked_ind_health_data_hash Hash of linked health data
    #' @param linked_ind_health_variable_map Variable mappings for linked health data
    #' @param linked_ind_health_value_map Value mappings for linked health data
    #' @return A new HealthDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "HealthDataAnalytics",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          quality_schema = NULL,
                          linked_ind_roster_data = NULL,
                          linked_ind_roster_data_stage_name = NULL,
                          linked_ind_roster_data_hash = NULL,
                          linked_ind_roster_variable_map = NULL,
                          linked_ind_roster_value_map = NULL,
                          linked_ind_health_data = NULL,
                          linked_ind_health_data_stage_name = NULL,
                          linked_ind_health_data_hash = NULL,
                          linked_ind_health_variable_map = NULL,
                          linked_ind_health_value_map = NULL) {

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
        value_label = value_label,
        quality_schema = quality_schema
      )

      private$..linked_ind_roster_data              <- linked_ind_roster_data
      private$..linked_ind_roster_data_stage_name   <- linked_ind_roster_data_stage_name
      private$..linked_ind_roster_data_hash         <- linked_ind_roster_data_hash
      private$..linked_ind_roster_variable_map      <- linked_ind_roster_variable_map
      private$..linked_ind_roster_value_map         <- linked_ind_roster_value_map

      private$..linked_ind_health_data              <- linked_ind_health_data
      private$..linked_ind_health_data_stage_name   <- linked_ind_health_data_stage_name
      private$..linked_ind_health_data_hash         <- linked_ind_health_data_hash
      private$..linked_ind_health_variable_map      <- linked_ind_health_variable_map
      private$..linked_ind_health_value_map         <- linked_ind_health_value_map

      private$..load_default_schema(
        filename = private$..default_analysis_schema_roster_filename,
        target_field = "..analysis_schema_roster",
        required_cols = c(
          "indicator_name",
          "calculation",
          "var_name",
          "denom_var",
          "disaggregation",
          "multiplier",
          "indicator_unit"
        ),
        schema_name = "analysis_roster",
        dataset_name = private$..dataset_name
      )

      private$..load_default_schema(
        filename = private$..default_outputs_schema_roster_filename,
        target_field = "..outputs_schema_roster",
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
        schema_name = "outputs_roster",
        dataset_name = private$..dataset_name
      )

      private$..load_default_schema(
        filename = private$..default_analysis_schema_health_filename,
        target_field = "..analysis_schema_health",
        required_cols = c(
          "indicator_name",
          "calculation",
          "var_name",
          "denom_var",
          "disaggregation",
          "multiplier",
          "indicator_unit"
        ),
        schema_name = "analysis_health",
        dataset_name = private$..dataset_name
      )

      private$..load_default_schema(
        filename = private$..default_outputs_schema_health_filename,
        target_field = "..outputs_schema_health",
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
        schema_name = "outputs_health",
        dataset_name = private$..dataset_name
      )

      # Generate per-dataset analysis plans for roster and deaths
      private$..data_analysis_plan_roster <- QuantDataAnalysisPlanLog$new(
        log_df   = NULL,
        log_name = phrutils::phr_txt("Quant Data Analysis Plan (Roster)")
      )
      private$..data_analysis_plan_health <- QuantDataAnalysisPlanLog$new(
        log_df   = NULL,
        log_name = phrutils::phr_txt("Quant Data Analysis Plan (Health)")
      )

      # Build the DAP for roster dataset from available columns and analysis_schema_roster
      if (!is.null(linked_ind_roster_data)) {

        result <- private$..build_dap_from_schema(
          analysis_schema = private$..analysis_schema_roster,
          variable_map    = private$..variable_map_roster %||% list(),
          available_vars  = names(private$..linked_ind_roster_data),
          dataset_label   = "roster"
        )

        private$..data_analysis_plan_roster$set(field = "log_df", value = result$dap_df)

      }

      # Build the DAP for deaths dataset from available columns and analysis_schema_deaths
      if (!is.null(linked_ind_health_data)) {

        result <- private$..build_dap_from_schema(
          analysis_schema = private$..analysis_schema_health,
          variable_map    = private$..variable_map_health %||% list(),
          available_vars  = names(private$..linked_ind_health_data),
          dataset_label   = "health"
        )

        private$..data_analysis_plan_health$set(field = "log_df", value = result$dap_df)

      }

      # Create and store per-dataset survey designs for roster and deaths
      if (!is.null(linked_ind_roster_data)) {
        private$..base_survey_design_roster <- phrutils::phr_try(
          srvyr::as_survey_design(.data = linked_ind_roster_data, ids = 1),
          on_error = "warn",
          origin   = paste0(dataset_name, "$initialize"),
          hint     = phrutils::phr_txt("Could not create base (unweighted) survey design for roster data.")
        )
        private$..survey_design_roster <- private$..create_survey_design_for_dataset(
          data         = linked_ind_roster_data,
          variable_map = linked_ind_roster_variable_map %||% ..linked_ind_roster_variable_map,
          origin       = paste0(dataset_name, "$initialize")
        )
      }

      if (!is.null(linked_ind_deaths_data)) {
        private$..base_survey_design_health <- phrutils::phr_try(
          srvyr::as_survey_design(.data = linked_ind_deaths_data, ids = 1),
          on_error = "warn",
          origin   = paste0(dataset_name, "$initialize"),
          hint     = phrutils::phr_txt("Could not create base (unweighted) survey design for health data.")
        )
        private$..survey_design_health <- private$..create_survey_design_for_dataset(
          data         = linked_ind_deaths_data,
          variable_map = linked_ind_deaths_variable_map %||% private$..linked_ind_health_variable_map,
          origin       = paste0(dataset_name, "$initialize")
        )
      }

      msg_parts <- c()
      if (!is.null(linked_ind_roster_data)) msg_parts <- c(msg_parts, "roster")
      if (!is.null(linked_ind_health_data))  msg_parts <- c(msg_parts, "health individual")
      if (length(msg_parts) > 0) {
        phrutils::phr_message(
          phrutils::phr_txt(
            "HealthDataAnalytics initialized with linked {paste(msg_parts, collapse=' and ')} data."
          )
        )
      } else {
        phrutils::phr_message(
          phrutils::phr_txt("{dataset_name} initialized as HealthDataAnalytics object.")
        )
      }
    },


  ),

  private = list(

    # Fields for linked roster data
    ..linked_ind_roster_data = NULL,
    ..linked_ind_roster_data_stage_name = NULL,
    ..linked_ind_roster_data_hash = NULL,
    ..linked_ind_roster_variable_map = NULL,
    ..linked_ind_roster_value_map = NULL,
    ..linked_ind_roster_variable_label = NULL,
    ..linked_ind_roster_value_label = NULL,

    # Fields for linked deaths data
    ..linked_ind_health_data = NULL,
    ..linked_ind_health_data_stage_name = NULL,
    ..linked_ind_health_data_hash = NULL,
    ..linked_ind_health_variable_map = NULL,
    ..linked_ind_health_value_map = NULL,
    ..linked_ind_health_variable_label = NULL,
    ..linked_ind_health_value_label = NULL,

    # Per-dataset schemas for roster and deaths
    ..analysis_schema_roster = NULL,
    ..analysis_schema_health = NULL,
    ..outputs_schema_roster  = NULL,
    ..outputs_schema_health  = NULL,

    # Per-dataset data analysis plans
    ..data_analysis_plan_roster = NULL,
    ..data_analysis_plan_health = NULL,

    # Per-dataset survey design objects
    ..survey_design_roster = NULL,
    ..survey_design_health = NULL,
    ..base_survey_design_roster = NULL,
    ..base_survey_design_health = NULL,

    # @field ..default_quality_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_quality_schema_filename = c(
      "resources",
      "quality_schema_data_quality_health_template.xlsx"
    ),

    # @field ..default_outputs_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_outputs_schema_filename = c(
      "resources",
      "outputs_schema_data_analytics_health_template.xlsx"
    ),

    # @field ..default_analysis_schema_filename Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_analysis_schema_filename = c(
      "resources",
      "analysis_schema_quant_data_analysis_health_template.xlsx"
    ),

    #' @description
    #' Pre-hook providing the input sets used by the inherited \code{run_analysis()}.
    #'
    #' Returns one input set per available mortality dataset:
    #' \describe{
    #'   \item{household}{Always included, using the core \code{data},
    #'     \code{survey_design} and \code{data_analysis_plan} fields. Results
    #'     stored in \code{analysis_results$household}.}
    #'   \item{roster}{Included when \code{linked_ind_roster_data} is available and
    #'     \code{data_analysis_plan_roster} contains at least one indicator. Results
    #'     stored in \code{analysis_results$roster}.}
    #'   \item{deaths}{Included when \code{linked_ind_deaths_data} is available and
    #'     \code{data_analysis_plan_deaths} contains at least one indicator. Results
    #'     stored in \code{analysis_results$deaths}.}
    #' }
    #'
    #' @return A named list of field sets.
    ..pre_run_analysis = function() {
      origin <- paste0(private$..dataset_name, "$pre_run_analysis")

      sets <- super$pre_run_analysis()

      if (!is.null(private$..linked_ind_roster_data)) {
        if (is.null(private$..data_analysis_plan_roster$get(field = "log_df"))) {
          sets$roster <- list(
            data = "linked_ind_roster_data",
            survey_design = "survey_design_roster",
            data_analysis_plan = "data_analysis_plan_roster"
          )
        } else {
          phrutils::phr_message(origin, phrutils::phr_txt("Linked roster data present but data_analysis_plan_roster is empty. Skipping roster analysis."))
        }
      }

      if (!is.null(private$..linked_ind_health_data)) {
        if (is.null(private$..data_analysis_plan_health$get(field = "log_df"))) {
          sets$health <- list(
            data = "linked_ind_health_data",
            survey_design = "survey_design_health",
            data_analysis_plan = "data_analysis_plan_health"
          )
        } else {
          phrutils::phr_message(origin, phrutils::phr_txt("Linked health data present but data_analysis_plan_health is empty. Skipping health analysis."))
        }
      }

      sets
    },

    #' @description
    #' Pre-hook providing the input sets used by the inherited \code{run_outputs()}.
    #'
    #' Returns one input set per available mortality dataset so that outputs are
    #' generated independently for each dataset and stored in named sub-lists
    #' (\code{visualizations$household} / \code{tables$household},
    #' \code{visualizations$roster} / \code{tables$roster},
    #' \code{visualizations$deaths} / \code{tables$deaths}). Each set passes
    #' the appropriate survey design objects for the dataset, so output entries
    #' with \code{dataset_type = "survey_design"} receive the weighted design and
    #' \code{"base"} entries receive the unweighted base design. Each set also
    #' names the dataset's own variable map, so \code{@variable_map$} references
    #' in \code{test_params} resolve against the correct dataset.
    #'
    #' @return A named list of field sets.
    ..pre_run_outputs = function() {
      origin <- paste0(private$..dataset_name, "$pre_run_outputs")

      sets <- list(
        household = list(
          data = "data",
          outputs_schema = "outputs_schema",
          base_survey_design = "base_survey_design",
          survey_design = "survey_design",
          variable_map = "variable_map"
        )
      )

      if (!is.null(private$..linked_ind_roster_data)) {
        if (!is.null(private$..outputs_schema_roster) && nrow(private$..outputs_schema_roster) > 0) {
          sets$roster <- list(
            data = "linked_ind_roster_data",
            outputs_schema = "outputs_schema_roster",
            base_survey_design = "base_survey_design_roster",
            survey_design = "survey_design_roster",
            variable_map = "linked_ind_roster_variable_map"
          )
        } else {
          phrutils::phr_message(origin, "Linked roster data present but outputs_schema_roster is empty. Skipping roster outputs.")
        }
      }

      if (!is.null(private$..linked_ind_health_data)) {
        if (!is.null(private$..outputs_schema_health) && nrow(private$..outputs_schema_health) > 0) {
          sets$health <- list(
            data = "linked_ind_health_data",
            outputs_schema = "outputs_schema_health",
            base_survey_design = "base_survey_design_health",
            survey_design = "survey_design_health",
            variable_map = "linked_ind_health_variable_map"
          )
        } else {
          phrutils::phr_message(origin, "Linked health data present but outputs_schema_health is empty. Skipping health outputs.")
        }
      }

      sets
    }
  )
)
