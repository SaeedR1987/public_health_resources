#' IPHRA Individual Death Data Class
#'
#' Extends IndividualData to represent individual-level death/mortality data
#' with recall period handling and death-specific validation.
#'
#' @description
#' This class provides specialized handling for mortality data including:
#' * Recall date validation for retrospective mortality assessments
#' * Death cause and location mapping
#' * Death timing and age at death calculations
#' * Mortality-specific schema and indicator support
#'
#' @details
#' The class requires a recall_date parameter to establish the reference period
#' for mortality data collection. This is essential for calculating mortality
#' rates and validating death timing information.
#'
#' @field required_columns Character vector of required column names for death data
#' @field optional_columns Character vector of optional columns for death data
#' @field recall_date Reference date for mortality recall period
#'
#' @seealso [IndividualData], [Data]
#' @export
DeathIndividualData <- R6::R6Class(
  classname = "DeathIndividualData",
  inherit   = IndividualData,

  public = list(

    # ---- Additional fields
    required_columns = NULL,
    optional_columns = NULL,
    recall_date = NULL,     # reference date if missing death timing


    #' @description
    #' Initialize a new DeathIndividualData object
    #'
    #' @param data A data frame containing individual death records
    #' @param dataset_name A name for this dataset
    #' @param variable_map Optional named list mapping standard variable names to data columns
    #' @param recall_date Reference date for mortality recall period (required, e.g., '2025-01-01')
    #' @param uuid Character name of the person-level unique ID column
    #' @param hh_uuid Character name of the household linkage column
    #' @return A new DeathIndividualData object
    initialize = function(data,
                          dataset_name = "DeathIndividualData",
                          recall_date = NULL,
                          uuid = NULL,
                          hh_uuid = NULL) {

      phrutils::phr_try({

        private$..touch(
          log = TRUE,
          touch = FALSE,
          name = private$..dataset_name,
          message = phrutils::phr_txt("Starting initialization for Data object {dataset_name}.")
        )

        # --- Default mapping for death-related variables
        default_death_map <- list(
          uuid      = "death_id",
          hh_uuid    = "hh_uuid"
        )

        # Step 1: Super-initialize
        result <- phrutils::phr_try_step(
          {
            # Direct super call (inherits uuid, hh_uuid, sex, age, etc.)
            super$initialize(
              data         = data,
              dataset_name = dataset_name,
              uuid = if (is.null(uuid)) {default_map$uuid} else {uuid},
              hh_uuid = if (is.null(hh_uuid)) {default_map$hh_uuid} else {hh_uuid}
            )
          },
          step = phrutils::phr_txt("Super initialize of DeathIndividualData"),
          hint = phrutils::phr_txt("Check initialization in parent Asset, Data, or IndividualData classes.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        # Step 2: Map schema vars
        result <- phrutils::phr_try_step(
          {
            # Re-map schema variables now that the death schema is loaded.
            private$..map_schema_vars(stage = "raw")
          },
          step = phrutils::phr_txt("Map schema vars for individual death data"),
          hint = phrutils::phr_txt("Check the variable schema for individual death data")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        # Step 3: Set recall date
        result <- phrutils::phr_try_step(
          {
            if(!is.null(recall_date) && is.character(recall_date)) {
              date_recall <- recall_date
            } else if(!is.null(private$..variable_map[["date_recall"]])) {
              recall_col <- private$..variable_map[["date_recall"]]
              date_recall <- unique(
                stats::na.omit(private$..raw_data[[recall_col]])
              )
            } else {
              date_recall <- "2025-01-01"
            }

            # Convert recall_date safely
            recall_date <- phrutils::phr_convert_date(recall_date)
            self$set(field = "..recall_date", value = recall_date)

          },
        step = phrutils::phr_txt("Setting recall dates"),
        hint = phrutils::phr_txt("Check for column with recall date in the dataset.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        # Step 4: Set required columns
        result <- phrutils::phr_try_step(
          {

            # --- Required columns
            self$set(field = "..required_columns", value = unique(c(
              private$..required_columns  # keep inherited ones (uuid, hh_uuid)
            )))

          },
        step = phrutils::phr_txt("Setting subclass specific required variables"),
        hint = phrutils::phr_txt("Check for subclass specific required columns in the dataset.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        private$..touch(
          log = TRUE,
          touch = TRUE,
          name = private$..dataset_name,
          message = phrutils::phr_txt("Initializaiton of {dataset_name} as a DeathIndividualData object completed successfully with {nrow(data)} records.")
        )

      }, on_error = "abort", origin = "DeathIndividualData$initialize")
    }
  ),

  private = list(

    # @field ..default_variable_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_variable_schema_filename = c(
      "resources",
      "variable_schema_data_individual_death_template.xlsx"
    ),

    # @field ..default_dependency_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_dependency_schema_filename = c(
      "resources",
      "dependency_schema_data_individual_death_template.xlsx"
    ),

    # @field ..value_label Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_indicator_schema_filename = c(
      "resources",
      "indicator_schema_data_individual_death_template.xlsx"
    ),

    ..analytics_registry = function() {

      registry <- super$..analytics_registry()

      c(
        registry,
        list(
          demographics = function(data,
                            stage,
                            analysis_config,
                            parent) {

            DemographicsDataAnalytics$new(
              data = data,
              dap = analysis_config,
              parent_data_object = parent,
              dataset_name = paste0(private$..dataset_name, "_DemographicsDataAnalytics"),
              data_stage_name = stage,
              data_hash = parent$get(field = "..metadata", role = "hash_id"),
              variable_map = parent$get(field = "..variable_map"),
              value_map = parent$get(field = "..value_map"),
              variable_label = parent$get(field = "..variable_label"),
              value_label = parent$get(field = "..value_label")
            )
          }
        )
      )
    }

  )
)
