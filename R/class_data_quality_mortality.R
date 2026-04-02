#' IPHRA Mortality Data Quality Class
#'
#' The `MortalityDataQuality` R6 class extends `DataQuality` to provide
#' Mortality-specific quality checks and analytics.
#'
#' @description
#' This class provides specialized quality checks for mortality data including:
#' * Death recording consistency
#' * Mortality rate plausibility
#' * Age and sex distribution validation
#' * Cause of death consistency
#'
#' @seealso [DataQuality]
#' @export
MortalityDataQuality <- R6::R6Class(
  classname = "MortalityDataQuality",
  inherit = DataQuality,

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
    #' Initialize a new MortalityDataQuality object
    #'
    #' @param data A data frame (standardized or clean mortality data)
    #' @param parent_data_object The Data object that generated this
    #' @param dataset_name A name for this quality assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
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
    initialize = function(data = NULL,
                          parent_data_object = NULL,
                          dataset_name = "MortalityDataQuality",
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
          phr_txt(paste0("MortalityDataQuality initialized with linked ", paste(msg_parts, collapse = " and "), " data."))
        )
      } else {
        phr_message(
          phr_txt(glue::glue("{dataset_name} initialized as MortalityDataQuality object."))
        )
      }
    },

    #' @description
    #' Get the default Mortality quality schema from template file
    #'
    #' @return A list of mortality-specific quality checks (the schema itself)
    default_quality_schema = function() {
      
      file <- system.file(
        "resources",
        "quality_schema_data_quality_mortality_template.xlsx",
        package = "iphRa"
      )
      
      # If template file doesn't exist, try relative path (for development)
      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_mortality_template.xlsx")
        if (!file.exists(file)) {
          # Return empty schema (no checks)
          return(list())
        }
      }
      
      # Read the Excel template
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_warning(
            origin  = "MortalityDataQuality$default_quality_schema",
            message = phr_txt(glue::glue("Failed to read quality_schema_data_quality_mortality_template.xlsx: {e$message}"))
          )
          return(NULL)
        }
      )
      
      # If reading failed or file is empty, return empty schema
      if (is.null(df) || nrow(df) == 0) {
        return(list())  # Return empty list (no checks)
      }
      
      # Convert table to schema
      schema_with_metadata <- quality_table_to_schema(df)
      
      # Return only the checks (the schema itself), not the metadata wrapper
      if (!is.null(schema_with_metadata) && !is.null(schema_with_metadata$checks)) {
        return(schema_with_metadata$checks)
      }
      
      return(list())  # Return empty list if conversion failed
    },

    #' @description
    #' Get the default Mortality outputs schema from template file
    #'
    #' Reads the outputs_quality_schema_data_quality_mortality_template.xlsx file from package resources
    #' and converts it to a nested list of outputs definitions.
    #'
    #' @return A list of mortality-specific outputs definitions
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_quality_schema_data_quality_mortality_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_quality_schema_data_quality_mortality_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_warning(
            origin  = "MortalityDataQuality$default_outputs_schema",
            message = phr_txt(glue::glue("Failed to read outputs_quality_schema_data_quality_mortality_template.xlsx: {e$message}"))
          )
          return(NULL)
        }
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    }
  )
)
