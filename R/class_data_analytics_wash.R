#' IPHRA WASH Data Analytics Class
#'
#' The `WASHDataAnalytics` R6 class extends `DataAnalytics` to provide
#' Water, Sanitation and Hygiene specific quality checks and quantitative
#' analysis in a single integrated object.
#'
#' @description
#' This class provides WASH-specific:
#' * Quality checks (water source, JMP ladder, sanitation, handwashing) via quality_schema
#' * Quantitative analysis indicators via analysis_schema
#' * All visualizations/tables (quality and analysis) via outputs_schema
#'
#' @field linked_containers_data Optional linked WaterContainerData dataframe
#' @field linked_containers_data_stage_name Name of linked containers data stage
#' @field linked_containers_data_hash Hash of linked containers data
#' @field linked_containers_variable_map Variable mappings for linked containers data
#' @field linked_containers_value_map Value mappings for linked containers data
#'
#' @seealso [DataAnalytics]
#' @export
WASHDataAnalytics <- R6::R6Class(
  classname = "WASHDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    #' @description
    #' Initialize a new WASHDataAnalytics object
    #'
    #' @param data A data frame (standardized or clean WASH data)
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
    #' @param linked_containers_data Optional linked WaterContainerData dataframe
    #' @param linked_containers_data_stage_name Name of linked containers data stage
    #' @param linked_containers_data_hash Hash of linked containers data
    #' @param linked_containers_variable_map Variable mappings for linked containers data
    #' @param linked_containers_value_map Value mappings for linked containers data
    #' @return A new WASHDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "WASHDataAnalytics",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          quality_schema = NULL,
                          linked_containers_data = NULL,
                          linked_containers_data_stage_name = NULL,
                          linked_containers_data_hash = NULL,
                          linked_containers_variable_map = NULL,
                          linked_containers_value_map = NULL) {

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

      private$..linked_containers_data              <- linked_containers_data
      private$..linked_containers_data_stage_name   <- linked_containers_data_stage_name
      private$..linked_containers_data_hash         <- linked_containers_data_hash
      private$..linked_containers_variable_map      <- linked_containers_variable_map
      private$..linked_containers_value_map         <- linked_containers_value_map

      private$..load_default_schema(
        filename = private$..default_analysis_schema_water_container_filename,
        target_field = "..analysis_schema_water_container",
        required_cols = c(
          "indicator_name",
          "calculation",
          "var_name",
          "denom_var",
          "disaggregation",
          "multiplier",
          "indicator_unit"
        ),
        schema_name = "analysis_water_container",
        dataset_name = private$..dataset_name
      )

      private$..load_default_schema(
        filename = private$..default_outputs_schema_water_container_filename,
        target_field = "..outputs_schema_water_container",
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
        schema_name = "outputs_water_container",
        dataset_name = private$..dataset_name
      )

      # Generate per-dataset analysis plans for roster and deaths
      private$..data_analysis_plan_water_container <- QuantDataAnalysisPlanLog$new(
        log_df   = NULL,
        log_name = phrutils::phr_txt("Quant Data Analysis Plan (Roster)")
      )

      # Build the DAP for roster dataset from available columns and analysis_schema_roster
      if (!is.null(linked_container_data)) {

        result <- private$..build_dap_from_schema(
          analysis_schema = private$..analysis_schema_water_container,
          variable_map    = private$..variable_map_water_container %||% list(),
          available_vars  = names(private$..linked_containers_data),
          dataset_label   = "water_containers"
        )

        private$..data_analysis_plan_roster$set(field = "log_df", value = result$dap_df)

      }

      # Create and store per-dataset survey designs for roster and deaths
      if (!is.null(linked_containers_data)) {
        private$..base_survey_design_water_container <- phrutils::phr_try(
          srvyr::as_survey_design(.data = linked_containers_data, ids = 1),
          on_error = "warn",
          origin   = paste0(dataset_name, "$initialize"),
          hint     = phrutils::phr_txt("Could not create base (unweighted) survey design for roster data.")
        )
        private$..survey_design_water_container <- private$..create_survey_design_for_dataset(
          data         = linked_containers_data,
          variable_map = linked_containers_variable_map %||% private$..linked_containers_variable_map,
          origin       = paste0(dataset_name, "$initialize")
        )
      }

      if (!is.null(linked_containers_data)) {
        phrutils::phr_message(
          phrutils::phr_txt("WASHDataAnalytics initialized with linked water container data.")
        )
      } else {
        phrutils::phr_message(
          phrutils::phr_txt("{dataset_name} initialized as WASHDataAnalytics object.")
        )
      }
    }
  ),

  private = list(

    # Fields for linked water container data
    ..linked_containers_data = NULL,
    ..linked_containers_data_stage_name = NULL,
    ..linked_containers_data_hash = NULL,
    ..linked_containers_variable_map = NULL,
    ..linked_containers_value_map = NULL,
    ..linked_containers_variable_label = NULL,
    ..linked_containers_value_label = NULL,

    ..data_analysis_plan_water_container = NULL,
    ..analysis_schema_water_container = NULL,
    ..outputs_schema_water_container = NULL,

    ..base_survey_design_water_container = NULL,
    ..survey_design_water_container = NULL,


    # @field ..default_quality_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_quality_schema_filename = c(
      "resources",
      "quality_schema_data_quality_wash_template.xlsx"
    ),

    # @field ..default_outputs_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_outputs_schema_filename = c(
      "resources",
      "outputs_schema_data_analytics_wash_template.xlsx"
    ),

    # @field ..default_analysis_schema_filename Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_analysis_schema_filename = c(
      "resources",
      "analysis_schema_quant_data_analysis_wash_template.xlsx"
    ),

    # @field ..default_outputs_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_outputs_schema_water_container_filename = c(
      "resources",
      "outputs_schema_data_analytics_water_container_template.xlsx"
    ),

    # @field ..default_analysis_schema_filename Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_analysis_schema_water_container_filename = c(
      "resources",
      "analysis_schema_quant_data_analysis_water_container_template.xlsx"
    )
  )
)
