#' MortalityAnalysis
#'
#' @description A class for mortality-specific quantitative data analysis
#'
#' @noRd
MortalityAnalysis <- R6::R6Class(
  classname = "MortalityAnalysis",
  inherit = QuantDataAnalysis,

  public = list(

    # Fields for linked roster data
    linked_ind_roster_data = NULL,
    linked_ind_roster_data_stage_name = NULL,
    linked_ind_roster_data_hash = NULL,
    linked_ind_roster_variable_map = NULL,
    linked_ind_roster_value_map = NULL,

    # Fields for linked deaths data
    linked_ind_deaths_data = NULL,
    linked_ind_deaths_data_stage_name = NULL,
    linked_ind_deaths_data_hash = NULL,
    linked_ind_deaths_variable_map = NULL,
    linked_ind_deaths_value_map = NULL,

    #' @description
    #' Initialize a new MortalityAnalysis object
    #'
    #' @param data Standardized or clean data from the generating Data object
    #' @param dap Data analysis plan
    #' @param parent_data_object Parent Data object
    #' @param dataset_name Name for this analysis
    #' @param data_stage_name Name of data stage (e.g. "standardized", "clean")
    #' @param data_hash Hash of the data
    #' @param variable_map Variable mappings
    #' @param value_map Value mappings
    #' @param linked_ind_roster_data Optional linked roster dataframe
    #' @param linked_ind_roster_data_stage_name Name of linked roster data stage
    #' @param linked_ind_roster_data_hash Hash of linked roster data
    #' @param linked_ind_roster_variable_map Variable mappings for linked roster data
    #' @param linked_ind_roster_value_map Value mappings for linked roster data
    #' @param linked_ind_deaths_data Optional linked deaths dataframe
    #' @param linked_ind_deaths_data_stage_name Name of linked deaths data stage
    #' @param linked_ind_deaths_data_hash Hash of linked deaths data
    #' @param linked_ind_deaths_variable_map Variable mappings for linked deaths data
    #' @param linked_ind_deaths_value_map Value mappings for linked deaths data
    #' @return A new MortalityAnalysis object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "MortalityAnalysis",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          linked_ind_roster_data = NULL,
                          linked_ind_roster_data_stage_name = NULL,
                          linked_ind_roster_data_hash = NULL,
                          linked_ind_roster_variable_map = NULL,
                          linked_ind_roster_value_map = NULL,
                          linked_ind_deaths_data = NULL,
                          linked_ind_deaths_data_stage_name = NULL,
                          linked_ind_deaths_data_hash = NULL,
                          linked_ind_deaths_variable_map = NULL,
                          linked_ind_deaths_value_map = NULL) {

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

      # Store linked roster data information
      self$linked_ind_roster_data <- linked_ind_roster_data
      self$linked_ind_roster_data_stage_name <- linked_ind_roster_data_stage_name
      self$linked_ind_roster_data_hash <- linked_ind_roster_data_hash
      self$linked_ind_roster_variable_map <- linked_ind_roster_variable_map
      self$linked_ind_roster_value_map <- linked_ind_roster_value_map

      # Store linked deaths data information
      self$linked_ind_deaths_data <- linked_ind_deaths_data
      self$linked_ind_deaths_data_stage_name <- linked_ind_deaths_data_stage_name
      self$linked_ind_deaths_data_hash <- linked_ind_deaths_data_hash
      self$linked_ind_deaths_variable_map <- linked_ind_deaths_variable_map
      self$linked_ind_deaths_value_map <- linked_ind_deaths_value_map

      if (!is.null(linked_ind_roster_data) || !is.null(linked_ind_deaths_data)) {
        msg_parts <- c()
        if (!is.null(linked_ind_roster_data)) msg_parts <- c(msg_parts, "roster")
        if (!is.null(linked_ind_deaths_data)) msg_parts <- c(msg_parts, "deaths")
        phr_message(
          phr_txt(paste0("MortalityAnalysis initialized with linked ", paste(msg_parts, collapse = " and "), " data."))
        )
      }

      invisible(self)
    },

    #' @description
    #' Load the default analysis schema from the Mortality-specific template file
    #'
    #' @return A tibble containing the Mortality analysis schema
    default_analysis_schema = function() {
      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_mortality_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_mortality_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin  = "MortalityAnalysis$default_analysis_schema",
        hint = "Check that analysis_schema_quant_data_analysis_mortality_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description
    #' Load the default outputs schema from template file for Mortality analysis
    #'
    #' @return A list containing the outputs schema
    default_outputs_schema = function() {
      file <- system.file(
        "resources",
        "outputs_analysis_schema_quant_data_analysis_mortality_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_analysis_schema_quant_data_analysis_mortality_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin  = "MortalityAnalysis$default_outputs_schema",
        hint = "Check that outputs_analysis_schema_quant_data_analysis_mortality_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_schema <- outputs_table_to_schema(df)
      return(outputs_schema)
    }
  )
)
