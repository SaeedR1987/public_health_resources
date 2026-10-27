#' IPHRA General Data Analytics Class
#'
#' The `GeneralDataAnalytics` R6 class extends `DataAnalytics` for general
#' quantitative analysis covering indicators not specific to other sectoral
#' subclasses (FSL, WASH, Health, Mortality, Nutrition, Demographics).
#'
#' @description
#' This class provides:
#' * A general-purpose analysis schema (non-sectoral indicators) via analysis_schema
#' * All visualizations/tables via outputs_schema (inherits from base)
#' * An empty quality schema (no sector-specific quality checks)
#'
#' @seealso [DataAnalytics]
#' @export
GeneralDataAnalytics <- R6::R6Class(
  classname = "GeneralDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    #' @description
    #' Initialize a new GeneralDataAnalytics object
    #'
    #' @param data A data frame (standardized or clean data)
    #' @param dap Optional data analysis plan (tibble)
    #' @param parent_data_object The Data object that generated this
    #' @param dataset_name A name for this analytics assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @param variable_label Variable labels from Data object
    #' @param value_label Value labels from Data object
    #' @return A new GeneralDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "GeneralDataAnalytics",
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

      phrutils::phr_message(
        phrutils::phr_txt(glue::glue("{dataset_name} initialized as GeneralDataAnalytics object."))
      )
    }

  ),

  private = list(
    # @field ..default_quality_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_quality_schema_filename = c(
      "resources",
      "quality_schema_data_quality_template.xlsx"
    ),

    # @field ..default_outputs_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_outputs_schema_filename = c(
      "resources",
      "outputs_schema_data_analytics_template.xlsx"
    ),

    # @field ..default_analysis_schema_filename Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_analysis_schema_filename = c(
      "resources",
      "analysis_schema_quant_data_analysis_template.xlsx"
    )
  )
)
