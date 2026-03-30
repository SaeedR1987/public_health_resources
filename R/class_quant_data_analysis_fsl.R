#' QuantDataAnalysisFSL
#'
#' @description A class generator function
#'
#' @noRd
QuantDataAnalysisFSL <- R6::R6Class(
  classname = "QuantDataAnalysisFSL",
  inherit = QuantDataAnalysis,

  public = list(

    #------------------------------------------------------------
    # Initialization
    #------------------------------------------------------------
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "QuantDataAnalysisFSL",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL) {
      origin <- "QuantDataAnalysisFSL$initialize"
      phr_message(origin, "Initializing FSL quantitative analysis class...")

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

      phr_message(origin,
                    paste("Loaded FSL indicator schema with",
                          nrow(self$analysis_schema), "indicators.")
      )

      invisible(self)
    },

    #' @description
    #' Load the default analysis schema from the FSL-specific template file
    #'
    #' @return A tibble containing the FSL analysis schema
    default_analysis_schema = function() {
      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_fsl_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_fsl_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin  = "QuantDataAnalysisFSL$default_analysis_schema",
        hint = "Check that analysis_schema_quant_data_analysis_fsl_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description
    #' Load the default outputs schema from template file for FSL analysis
    #'
    #' @return A list containing the outputs schema
    default_outputs_schema = function() {
      file <- system.file(
        "resources",
        "outputs_analysis_schema_quant_data_analysis_fsl_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_analysis_schema_quant_data_analysis_fsl_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin  = "QuantDataAnalysisFSL$default_outputs_schema",
        hint = "Check that outputs_analysis_schema_quant_data_analysis_fsl_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_schema <- outputs_table_to_schema(df)
      return(outputs_schema)
    }


  )
)
