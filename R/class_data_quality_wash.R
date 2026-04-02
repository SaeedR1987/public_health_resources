#' IPHRA WASH Data Quality Class
#'
#' The `WASHDataQuality` R6 class extends `DataQuality` to provide
#' Water, Sanitation and Hygiene specific quality checks and analytics.
#'
#' @description
#' This class provides specialized quality checks for WASH data including:
#' * Water source validation
#' * Water collection time plausibility
#' * Sanitation type validation
#' * JMP ladder consistency checks
#' * Handwashing facility validation
#'
#' @seealso [DataQuality], [WASHHouseholdData]
#' @export
WASHDataQuality <- R6::R6Class(
  classname = "WASHDataQuality",
  inherit = DataQuality,

  public = list(

    # Fields for linked water container data
    linked_containers_data = NULL,
    linked_containers_data_stage_name = NULL,
    linked_containers_data_hash = NULL,
    linked_containers_variable_map = NULL,
    linked_containers_value_map = NULL,

    #' @description
    #' Initialize a new WASHDataQuality object
    #'
    #' @param data A data frame (standardized or clean WASH data)
    #' @param parent_data_object The WASHHouseholdData object that generated this
    #' @param dataset_name A name for this quality assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @param linked_containers_data Optional linked WaterContainerData dataframe
    #' @param linked_containers_data_stage_name Name of linked containers data stage
    #' @param linked_containers_data_hash Hash of linked containers data
    #' @param linked_containers_variable_map Variable mappings for linked containers data
    #' @param linked_containers_value_map Value mappings for linked containers data
    initialize = function(data = NULL,
                          parent_data_object = NULL,
                          dataset_name = "WASHDataQuality",
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
          phr_txt("WASHDataQuality initialized with linked water container data.")
        )
      } else {
        phr_message(
          phr_txt(glue::glue("{dataset_name} initialized as WASHDataQuality object."))
        )
      }
    },

    #' @description
    #' Get the default WASH quality schema from template file
    #'
    #' Reads the quality_schema_data_quality_wash_template.xlsx file from package resources
    #' and converts it to a nested list of quality checks (schema only, no metadata).
    #'
    #' @return A list of WASH-specific quality checks (the schema itself)
    default_quality_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_wash_template.xlsx",
        package = "iphRa"
      )

      # If template file doesn't exist, try relative path (for development)
      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_wash_template.xlsx")
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
            origin  = "WASHDataQuality$default_quality_schema",
            message = phr_txt(glue::glue("Failed to read quality_schema_data_quality_wash_template.xlsx: {e$message}"))
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
    #' Compute WASH-specific summary statistics
    #'
    #' @return A list of WASH summary statistics
    compute_summary_stats = function() {

      # Call parent method first
      stats <- super$compute_summary_stats()

      phr_try({

        df <- self$data

        # Water source distribution
        if ("wash_water_source_primary" %in% names(df)) {
          stats$water_source <- table(df$wash_water_source_primary, useNA = "ifany")
        }

        # Water collection time
        if ("wash_water_collection_time_minutes" %in% names(df)) {
          time_vals <- as.numeric(df$wash_water_collection_time_minutes)
          stats$collection_time <- list(
            mean = round(mean(time_vals, na.rm = TRUE), 1),
            median = round(median(time_vals, na.rm = TRUE), 1),
            min = min(time_vals, na.rm = TRUE),
            max = max(time_vals, na.rm = TRUE),
            pct_under_30min = round(sum(time_vals <= 30, na.rm = TRUE) / sum(!is.na(time_vals)) * 100, 1)
          )
        }

        # Sanitation type distribution
        if ("wash_sanitation_type" %in% names(df)) {
          stats$sanitation_type <- table(df$wash_sanitation_type, useNA = "ifany")
        }

        # JMP ladders
        if ("wash_jmp_water_ladder" %in% names(df)) {
          stats$jmp_water <- table(df$wash_jmp_water_ladder, useNA = "ifany")
        }

        if ("wash_jmp_sanitation_ladder" %in% names(df)) {
          stats$jmp_sanitation <- table(df$wash_jmp_sanitation_ladder, useNA = "ifany")
        }

        if ("wash_jmp_hygiene_ladder" %in% names(df)) {
          stats$jmp_hygiene <- table(df$wash_jmp_hygiene_ladder, useNA = "ifany")
        }

        # Handwashing
        if ("wash_handwashing_facility" %in% names(df)) {
          stats$handwashing <- table(df$wash_handwashing_facility, useNA = "ifany")
        }

        self$summary_stats <- stats

        phr_message(
          phr_txt(glue::glue("Computed WASH summary statistics for {self$dataset_name}."))
        )

        invisible(stats)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$compute_summary_stats"))
    },

    #' @description
    #' Visualize WASH data quality results
    #'
    #' @param type The type of visualization
    #' @return A ggplot object or NULL
    visualize = function(type = "summary") {

      phr_try({

        if (!requireNamespace("ggplot2", quietly = TRUE)) {
          phr_warning(
            message = "Package 'ggplot2' required for visualization.",
            origin = self$dataset_name
          )
          return(invisible(NULL))
        }

        phr_message(
          phr_txt(glue::glue("WASH visualization type '{type}' - implementation pending."))
        )

        invisible(NULL)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$visualize"))
    },

    #' @description
    #' Get the default WASH outputs schema from template file
    #'
    #' Reads the outputs_quality_schema_data_quality_wash_template.xlsx file from package resources
    #' and converts it to a nested list of outputs definitions.
    #'
    #' @return A list of WASH-specific outputs definitions
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_quality_schema_data_quality_wash_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_quality_schema_data_quality_wash_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_warning(
            origin  = "WASHDataQuality$default_outputs_schema",
            message = phr_txt(glue::glue("Failed to read outputs_quality_schema_data_quality_wash_template.xlsx: {e$message}"))
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
