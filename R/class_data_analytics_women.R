#' IPHRA Maternal Health Data Analytics Class
#'
#' The `WomanDataAnalytics` R6 class extends `DataAnalytics` to provide
#' Maternal health-specific quality checks and quantitative analysis in a single
#' integrated object.
#'
#' @description
#' This class provides Health-specific:
#' * Quality checks (maternal health) via quality_schema
#' * Quantitative analysis indicators via analysis_schema
#' * All visualizations/tables (quality and analysis) via outputs_schema
#'
#' @section Accessible fields:
#' * `linked_ind_roster_data`: Optional linked roster dataframe
#' * `linked_ind_roster_data_stage_name`: Name of linked roster data stage
#' * `linked_ind_roster_data_hash`: Hash of linked roster data
#' * `linked_ind_roster_variable_map`: Variable mappings for linked roster data
#' * `linked_ind_roster_value_map`: Value mappings for linked roster data
#' * `linked_ind_woman_data`: Optional linked woman individual dataframe
#' * `linked_ind_woman_data_stage_name`: Name of linked woman individual data stage
#' * `linked_ind_woman_data_hash`: Hash of linked woman individual data
#' * `linked_ind_woman_variable_map`: Variable mappings for linked woman individual data
#' * `linked_ind_woman_value_map`: Value mappings for linked woman individual data
#'
#' @seealso [DataAnalytics]
#' @export
WomanDataAnalytics <- R6::R6Class(
  classname = "WomanDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    #' @description
    #' Initialize a new WomanDataAnalytics object
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
    #' @param linked_ind_woman_data Optional linked health individual dataframe
    #' @param linked_ind_woman_data_stage_name Name of linked health data stage
    #' @param linked_ind_woman_data_hash Hash of linked health data
    #' @param linked_ind_woman_variable_map Variable mappings for linked health data
    #' @param linked_ind_woman_value_map Value mappings for linked health data
    #' @return A new WomanDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "WomanDataAnalytics",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          quality_schema = NULL) {

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = dataset_name,
        message = private$..phr_txt("Started initialization for {dataset_name}.")
      )

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

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = dataset_name,
        message = private$..phr_txt("{dataset_name} initialized successfully.")
      )
    }
  ),

  private = list(

    # @field ..default_quality_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_quality_schema_filename = c(
      "resources",
      "quality_schema_data_quality_woman_template.xlsx"
    ),

    # @field ..default_outputs_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_outputs_schema_filename = c(
      "resources",
      "outputs_schema_data_analytics_woman_template.xlsx"
    ),

    # @field ..default_analysis_schema_filename Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_analysis_schema_filename = c(
      "resources",
      "analysis_schema_quant_data_analysis_woman_template.xlsx"
    )
  )
)
