#' IPHRA Health Data Quality Class
#'
#' The `HealthDataQuality` R6 class extends `DataQuality` to provide
#' Health-specific quality checks and analytics.
#'
#' @description
#' This class provides specialized quality checks for health data including:
#' * Health facility access metric validation
#' * Healthcare utilization consistency
#' * Insurance coverage consistency
#' * Maternal health indicator validation
#' * Health expenditure plausibility checks
#'
#' @seealso [DataQuality], [HealthHouseholdData]
#' @export
HealthDataQuality <- R6::R6Class(
  classname = "HealthDataQuality",
  inherit = DataQuality,

  public = list(

    # Fields for linked roster data
    linked_ind_roster_data = NULL,
    linked_ind_roster_data_stage_name = NULL,
    linked_ind_roster_data_hash = NULL,
    linked_ind_roster_variable_map = NULL,
    linked_ind_roster_value_map = NULL,

    # Fields for linked health individual data
    linked_ind_health_data = NULL,
    linked_ind_health_data_stage_name = NULL,
    linked_ind_health_data_hash = NULL,
    linked_ind_health_variable_map = NULL,
    linked_ind_health_value_map = NULL,

    #' @description
    #' Initialize a new HealthDataQuality object
    #'
    #' @param data A data frame (standardized or clean health data)
    #' @param parent_data_object The HealthHouseholdData object that generated this
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
    #' @param linked_ind_health_data Optional linked health individual dataframe
    #' @param linked_ind_health_data_stage_name Name of linked health data stage
    #' @param linked_ind_health_data_hash Hash of linked health data
    #' @param linked_ind_health_variable_map Variable mappings for linked health data
    #' @param linked_ind_health_value_map Value mappings for linked health data
    initialize = function(data = NULL,
                          parent_data_object = NULL,
                          dataset_name = "HealthDataQuality",
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
                          linked_ind_health_data = NULL,
                          linked_ind_health_data_stage_name = NULL,
                          linked_ind_health_data_hash = NULL,
                          linked_ind_health_variable_map = NULL,
                          linked_ind_health_value_map = NULL) {

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

      # Store linked health individual data information
      self$linked_ind_health_data <- linked_ind_health_data
      self$linked_ind_health_data_stage_name <- linked_ind_health_data_stage_name
      self$linked_ind_health_data_hash <- linked_ind_health_data_hash
      self$linked_ind_health_variable_map <- linked_ind_health_variable_map
      self$linked_ind_health_value_map <- linked_ind_health_value_map

      if (!is.null(linked_ind_roster_data) || !is.null(linked_ind_health_data)) {
        msg_parts <- c()
        if (!is.null(linked_ind_roster_data)) msg_parts <- c(msg_parts, "roster")
        if (!is.null(linked_ind_health_data)) msg_parts <- c(msg_parts, "health individual")
        phr_message(
          phr_txt(paste0("HealthDataQuality initialized with linked ", paste(msg_parts, collapse = " and "), " data."))
        )
      } else {
        phr_message(
          phr_txt(glue::glue("{dataset_name} initialized as HealthDataQuality object."))
        )
      }
    },

    #' @description
    #' Get the default Health quality schema from template file
    #'
    #' Reads the quality_schema_data_quality_health_template.xlsx file from package resources
    #' and converts it to a nested list of quality checks (schema only, no metadata).
    #'
    #' @return A list of health-specific quality checks (the schema itself)
    default_quality_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_health_template.xlsx",
        package = "iphRa"
      )

      # If template file doesn't exist, try relative path (for development)
      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_health_template.xlsx")
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
            origin  = "HealthDataQuality$default_quality_schema",
            message = phr_txt(glue::glue("Failed to read quality_schema_data_quality_health_template.xlsx: {e$message}"))
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
    #' Get the default Health outputs schema from template file
    #'
    #' Reads the outputs_quality_schema_data_quality_health_template.xlsx file from package resources
    #' and converts it to a nested list of outputs definitions.
    #'
    #' @return A list of health-specific outputs definitions
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_quality_schema_data_quality_health_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_quality_schema_data_quality_health_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_warning(
            origin  = "HealthDataQuality$default_outputs_schema",
            message = phr_txt(glue::glue("Failed to read outputs_quality_schema_data_quality_health_template.xlsx: {e$message}"))
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


