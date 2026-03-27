#' IPHRA FSL Data Quality Class
#'
#' The `FSLDataQuality` R6 class extends `DataQuality` to provide
#' Food Security and Livelihood specific quality checks and analytics.
#'
#' @description
#' This class provides specialized quality checks for FSL data including:
#' * FCS (Food Consumption Score) validity checks
#' * HHS (Household Hunger Scale) consistency checks
#' * rCSI (reduced Coping Strategy Index) range validation
#' * HDDS (Household Dietary Diversity Score) validation
#' * Component-level consistency checks
#' * Indicator calculation validation
#'
#' @details
#' FSL-specific quality metrics:
#' * FCS component values (0-7 range)
#' * FCS total score plausibility (0-112 range)
#' * HHS consistency between questions and frequencies
#' * rCSI component and total score validation
#' * HDDS count and category consistency
#'
#' @seealso [DataQuality], [FSLHouseholdData]
#' @export
FSLDataQuality <- R6::R6Class(
  classname = "FSLDataQuality",
  inherit = DataQuality,

  public = list(

    #' @description
    #' Initialize a new FSLDataQuality object
    #'
    #' @param data A data frame (standardized or clean FSL data)
    #' @param parent_data_object The FSLHouseholdData object that generated this
    #' @param dataset_name A name for this quality assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @param quality_schema Optional quality check schema
    initialize = function(data = NULL,
                          parent_data_object = NULL,
                          dataset_name = "FSLDataQuality",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          quality_schema = NULL) {

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
        value_label = value_label,
        quality_schema = quality_schema
      )

      iphra_message(
        iphra_txt(glue::glue("{dataset_name} initialized as FSLDataQuality object."))
      )
    },

    #' @description
    #' Get the default FSL quality schema from template file
    #'
    #' Reads the quality_schema_data_quality_fsl_template.xlsx file from package resources
    #' and converts it to a nested list of quality checks (schema only, no metadata).
    #'
    #' @return A list of FSL-specific quality checks (the schema itself)
    default_quality_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_fsl_template.xlsx",
        package = "iphRa"
      )

      # If template file doesn't exist, try relative path (for development)
      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_fsl_template.xlsx")
        if (!file.exists(file)) {
          # Return empty schema (no checks)
          return(list())
        }
      }

      # Read the Excel template
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          iphra_warning(
            origin  = "FSLDataQuality$default_quality_schema",
            message = iphra_txt(glue::glue("Failed to read quality_schema_data_quality_fsl_template.xlsx: {e$message}"))
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
    #' Get the default FSL outputs schema from template file
    #'
    #' Reads the outputs_quality_schema_data_quality_fsl_template.xlsx file from package resources
    #' and converts it to a nested list of outputs definitions.
    #'
    #' @return A list of FSL-specific outputs definitions
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_quality_schema_data_quality_fsl_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_quality_schema_data_quality_fsl_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          iphra_warning(
            origin  = "FSLDataQuality$default_outputs_schema",
            message = iphra_txt(glue::glue("Failed to read outputs_quality_schema_data_quality_fsl_template.xlsx: {e$message}"))
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
