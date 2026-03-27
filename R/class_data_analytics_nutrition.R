#' IPHRA Nutrition Data Analytics Class
#'
#' The `NutritionDataAnalytics` R6 class extends `DataAnalytics` for
#' Nutrition-specific quantitative analysis, including anthropometric data.
#'
#' @description
#' This class provides Nutrition-specific (including anthropometric) functionality:
#' * Quantitative analysis indicators via analysis_schema
#' * Quality checks (e.g. anthropometric z-score plausibility) via quality_schema
#' * All visualizations/tables via outputs_schema
#'
#' @seealso [DataAnalytics]
#' @export
NutritionDataAnalytics <- R6::R6Class(
  classname = "NutritionDataAnalytics",
  inherit = DataAnalytics,

  public = list(

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

      iphra_message(
        iphra_txt(glue::glue("{dataset_name} initialized as NutritionDataAnalytics object."))
      )
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

      schema_tbl <- iphra_try(
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

      df <- iphra_try(
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
  )
)
