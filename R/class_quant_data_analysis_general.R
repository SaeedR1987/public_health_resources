#' QuantDataAnalysisGeneral
#'
#' @description A class for general quantitative data analysis covering
#' indicators not specific to other sectoral subclasses (FSL, WASH, Health,
#' Mortality, Nutrition, Demographics).
#'
#' @noRd
QuantDataAnalysisGeneral <- R6::R6Class(
  classname = "QuantDataAnalysisGeneral",
  inherit = QuantDataAnalysis,

  public = list(

    #' @description
    #' Load the default analysis schema from the General-specific template file
    #'
    #' @return A tibble containing the General analysis schema
    default_analysis_schema = function() {
      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_general_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_general_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- iphra_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin  = "QuantDataAnalysisGeneral$default_analysis_schema",
        hint = "Check that analysis_schema_quant_data_analysis_general_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description
    #' Load the default outputs schema from template file for General analysis
    #'
    #' @return A list containing the outputs schema
    default_outputs_schema = function() {
      file <- system.file(
        "resources",
        "outputs_analysis_schema_quant_data_analysis_general_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_analysis_schema_quant_data_analysis_general_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- iphra_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin  = "QuantDataAnalysisGeneral$default_outputs_schema",
        hint = "Check that outputs_analysis_schema_quant_data_analysis_general_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_schema <- outputs_table_to_schema(df)
      return(outputs_schema)
    }
  )
)
