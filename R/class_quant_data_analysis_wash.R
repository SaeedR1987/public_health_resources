#' WASHAnalysis
#'
#' @description A class for WASH-specific quantitative data analysis
#'
#' @noRd
WASHAnalysis <- R6::R6Class(
  classname = "WASHAnalysis",
  inherit = QuantDataAnalysis,

  public = list(

    # Fields for linked water container data
    linked_containers_data = NULL,
    linked_containers_data_stage_name = NULL,
    linked_containers_data_hash = NULL,
    linked_containers_variable_map = NULL,
    linked_containers_value_map = NULL,

    #' @description
    #' Initialize a new WASHAnalysis object
    #'
    #' @param data Standardized or clean data from the generating Data object
    #' @param dap Data analysis plan
    #' @param parent_data_object Parent Data object
    #' @param dataset_name Name for this analysis
    #' @param data_stage_name Name of data stage (e.g. "standardized", "clean")
    #' @param data_hash Hash of the data
    #' @param variable_map Variable mappings
    #' @param value_map Value mappings
    #' @param linked_containers_data Optional linked WaterContainerData dataframe
    #' @param linked_containers_data_stage_name Name of linked containers data stage
    #' @param linked_containers_data_hash Hash of linked containers data
    #' @param linked_containers_variable_map Variable mappings for linked containers data
    #' @param linked_containers_value_map Value mappings for linked containers data
    #' @return A new WASHAnalysis object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "WASHAnalysis",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          linked_containers_data = NULL,
                          linked_containers_data_stage_name = NULL,
                          linked_containers_data_hash = NULL,
                          linked_containers_variable_map = NULL,
                          linked_containers_value_map = NULL) {

      # Call parent constructor
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

      # Store linked data information
      self$linked_containers_data <- linked_containers_data
      self$linked_containers_data_stage_name <- linked_containers_data_stage_name
      self$linked_containers_data_hash <- linked_containers_data_hash
      self$linked_containers_variable_map <- linked_containers_variable_map
      self$linked_containers_value_map <- linked_containers_value_map

      if (!is.null(linked_containers_data)) {
        phr_message(
          phr_txt("WASHAnalysis initialized with linked water container data.")
        )
      }

      invisible(self)
    },

    #' @description
    #' Load the default analysis schema from the WASH-specific template file
    #'
    #' @return A tibble containing the WASH analysis schema
    default_analysis_schema = function() {
      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_wash_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_wash_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin  = "WASHAnalysis$default_analysis_schema",
        hint = "Check that analysis_schema_quant_data_analysis_wash_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description
    #' Load the default outputs schema from template file for WASH analysis
    #'
    #' @return A list containing the outputs schema
    default_outputs_schema = function() {
      file <- system.file(
        "resources",
        "outputs_analysis_schema_quant_data_analysis_wash_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_analysis_schema_quant_data_analysis_wash_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin  = "WASHAnalysis$default_outputs_schema",
        hint = "Check that outputs_analysis_schema_quant_data_analysis_wash_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_schema <- outputs_table_to_schema(df)
      return(outputs_schema)
    }
  )
)
