#' IPHRA Demographics Data Quality Class
#'
#' The `DemographicsDataQuality` R6 class extends `DataQuality` to provide
#' Demographics-specific quality checks and analytics.
#'
#' @description
#' This class provides specialized quality checks for demographics data including:
#' * Age distribution plausibility
#' * Sex ratio validation
#' * Household size consistency
#' * Age-sex pyramid checks
#' * Dependency ratio validation
#'
#' @seealso [DataQuality]
#' @export
DemographicsDataQuality <- R6::R6Class(
  classname = "DemographicsDataQuality",
  inherit = DataQuality,

  public = list(

    #' @description
    #' Initialize a new DemographicsDataQuality object
    #'
    #' @param data A data frame (standardized or clean demographics data)
    #' @param parent_data_object The Data object that generated this
    #' @param dataset_name A name for this quality assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @return A new DemographicsDataQuality object
    initialize = function(data = NULL,
                          parent_data_object = NULL,
                          dataset_name = "DemographicsDataQuality",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL) {

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

      phr_message(
        phr_txt(glue::glue("{dataset_name} initialized as DemographicsDataQuality object."))
      )
    },

    #' @description
    #' Get the default Demographics quality schema from template file
    #'
    #' @return A list of demographics-specific quality checks (the schema itself)
    default_quality_schema = function() {
      
      file <- system.file(
        "resources",
        "quality_schema_data_quality_demographics_template.xlsx",
        package = "iphRa"
      )
      
      # If template file doesn't exist, try relative path (for development)
      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_demographics_template.xlsx")
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
            origin  = "DemographicsDataQuality$default_quality_schema",
            message = phr_txt(glue::glue("Failed to read quality_schema_data_quality_demographics_template.xlsx: {e$message}"))
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
    #' Get the default Demographics outputs schema from template file
    #'
    #' Reads the outputs_schema_data_analytics_demographics_template.xlsx file from package resources
    #' and converts it to a nested list of outputs definitions.
    #'
    #' @return A list of demographics-specific outputs definitions
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_demographics_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_demographics_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_warning(
            origin  = "DemographicsDataQuality$default_outputs_schema",
            message = phr_txt(glue::glue("Failed to read outputs_schema_data_analytics_demographics_template.xlsx: {e$message}"))
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
