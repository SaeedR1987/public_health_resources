#' IPHRA Water Container Data Class
#'
#' The `WaterContainerData` R6 class extends the base [Data] class to represent
#' water container assessment data. Each record corresponds to one water container
#' within a household.
#'
#' @description
#' This subclass enforces domain-specific validation and schema rules for
#' water container data, including:
#' * Required fields for `uuid` (unique container ID), `hh_uuid` (household linkage)
#' * Container characteristics, storage practices, water source
#' * Water quality testing results
#' * Risk assessment calculations
#'
#' @field household_link Optional linkage back to a HouseholdData object
#' @field optional_columns Character vector of optional columns for water container data
#'
#' @seealso [Data], [HouseholdData]
#' @export
WaterContainerData <- R6::R6Class(
  classname = "WaterContainerData",
  inherit   = Data,

  public = list(

    #' @description
    #' Initialize a new WaterContainerData object
    #'
    #' @param data A data frame containing water container assessment records
    #' @param dataset_name A name for this dataset
    #' @param metadata Optional list of metadata
    #' @param variable_map Optional named list mapping standard variable names to data columns
    #' @return A new WaterContainerData object
    initialize = function(data,
                          dataset_name = "WaterContainerData",
                          uuid = NULL,
                          hh_uuid = NULL) {

      phrutils::phr_try({

        private$..touch(
          log = TRUE,
          touch = FALSE,
          name = private$..dataset_name,
          message = phrutils::phr_txt("Starting initialization for WaterContainerData object {dataset_name}.")
        )

        # Default mapping for water container data
        default_map <- list(
          # Identifiers
          uuid = "container_id",           # unique container identifier
          hh_uuid = "hh_uuid"     # household linkage

        )

        # Step 1: Super-initialize
        result <- phrutils::phr_try_step(
          {
            # Call parent initializer
            super$initialize(
              data         = data,
              dataset_name = dataset_name,
              uuid = if (is.null(uuid)) {default_map$uuid} else {uuid},
              hh_uuid = if (is.null(hh_uuid)) {default_map$hh_uuid} else {hh_uuid}
            )
          },
          step = phrutils::phr_txt("Super initialize of WaterContainerData"),
          hint = phrutils::phr_txt("Check initialization in parent Asset or Data classes.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        # Step 2: Map schema vars
        result <- phrutils::phr_try_step(
          {
            private$..map_schema_vars(stage = "raw")
          },
          step = phrutils::phr_txt("Map schema vars for WaterContainerData"),
          hint = phrutils::phr_txt("Check variable schema, variable or value maps.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        # Step 3: Set optional columns
        result <- phrutils::phr_try_step(
          {
            # Optional (mapped) columns
            self$set(field = "..optional_columns", value = c(
              private$..optional_columns,
              private$..variable_map$container_number,
              private$..variable_map$container_type,
              private$..variable_map$container_journey_count,
              private$..variable_map$container_capacity_liters
            ))
          },
          step = phrutils::phr_txt("Setting optional columns for WaterContainerData"),
          hint = phrutils::phr_txt("Check for presence of optional variables")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        private$..touch(
          log = TRUE,
          touch = TRUE,
          name = private$..dataset_name,
          message = phrutils::phr_txt("Initialization for {dataset_name} as WaterContainerData object completed successfully with {nrow(data)} records.")
        )

      }, on_error = "abort", origin = "WaterContainerData$initialize")
    }
  ),

  private = list(
    # @field ..default_variable_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_variable_schema_filename = c(
      "resources",
      "variable_schema_data_water_container_template.xlsx"
    ),

    # @field ..default_dependency_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_dependency_schema_filename = c(
      "resources",
      "dependency_schema_data_water_container_template.xlsx"
    ),

    # @field ..value_label Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_indicator_schema_filename = c(
      "resources",
      "indicator_schema_data_water_container_template.xlsx",
    ),
  )
)
