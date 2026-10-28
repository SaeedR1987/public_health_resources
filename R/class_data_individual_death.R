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

    # ---- Additional fields -------------------------------------------
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
    #' @return A new DeathIndividualData object
    initialize = function(data,
                          dataset_name = "DeathIndividualData",
                          recall_date = NULL,
                          uuid = NULL) {

      phrutils::phr_try({

        # --- Validate recall date early
        if (is.null(recall_date)) {
          phrutils::phr_error(
            phrutils::phr_txt("A recall_date must be specified (e.g. '2025-01-01')."),
            origin = "DeathIndividualData$initialize"
          )
        }

        # Convert recall_date safely
        recall_date <- phrutils::phr_convert_date(recall_date)
        self$set(field = "recall_date", value = recall_date)

        # --- Default mapping for death-related variables
        default_death_map <- list(
          uuid      = "death_id",
          hh_uuid    = "hh_uuid"
        )

        # variable_map <- modifyList(default_death_map, variable_map %||% list())

        if(is.null(uuid)) {uuid <- default_death_map$uuid}

        # Direct super call (inherits uuid, hh_uuid, sex, age, etc.)
        super$initialize(
          data         = data,
          dataset_name = dataset_name,
          uuid = uuid
        )

        # --- Required and optional columns
        self$set(field = "..required_columns", value = unique(c(
          private$..required_columns  # keep inherited ones (uuid, hh_uuid)
        )))

        # Re-map schema variables now that the death schema is loaded.
        # Data$initialize() already mapped against the (still empty) parent
        # schema, so roles such as hh_uuid would otherwise stay unmapped until
        # the first standardize() call.
        private$..map_schema_vars(stage = "raw")

        phrutils::phr_message(phrutils::phr_txt("{dataset_name} initialized as DeathIndividualData."))

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
      "indicator_schema_data_individual_death_template.xlsx",
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
              dataset_name = paste0(self$dataset_name, "_DemographicsDataAnalytics"),
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
