#' NutritionAnalysis
#'
#' @description A class for nutrition-specific quantitative data analysis
#'
#' @noRd
NutritionAnalysis <- R6::R6Class(
  classname = "NutritionAnalysis",
  inherit = QuantDataAnalysis,

  public = list(

    #' @description
    #' Load the default analysis schema from the Nutrition-specific template file
    #'
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
        origin  = "NutritionAnalysis$default_analysis_schema",
        hint = "Check that analysis_schema_quant_data_analysis_nutrition_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description
    #' Load the default outputs schema from template file for Nutrition analysis
    #'
    #' @return A list containing the outputs schema
    default_outputs_schema = function() {
      file <- system.file(
        "resources",
        "outputs_analysis_schema_quant_data_analysis_nutrition_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_analysis_schema_quant_data_analysis_nutrition_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin  = "NutritionAnalysis$default_outputs_schema",
        hint = "Check that outputs_analysis_schema_quant_data_analysis_nutrition_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_schema <- outputs_table_to_schema(df)
      return(outputs_schema)
    }
  )
)
