#' IPHRA Individual Health Data Class
#'
#' Extends IndividualData by adding a comprehensive schema for
#' individual-level health status and morbidity data including:
#' morbidity indicators, care-seeking behavior, treatment received,
#' chronic conditions, disability status, and vaccination.
#'
#' @export
HealthIndividualData <- R6::R6Class(
  classname = "HealthIndividualData",
  inherit   = IndividualData,

  public = list(


    #' @description
    #' Initialize a new HealthIndividualData object
    #'
    #' @param data A data frame containing individual health records
    #' @param dataset_name A name for this dataset
    #' @param uuid column name for uuid in the dataset
    #' @param hh_uuid Character name of the household linkage column
    #' @return A new HealthIndividualData object
    initialize = function(data,
                          dataset_name = "HealthIndividualData",
                          uuid = NULL,
                          hh_uuid = NULL) {

      phrutils::phr_try({

        private$..touch(
          log = TRUE,
          touch = FALSE,
          name = private$..dataset_name,
          message = private$..phr_txt("Started initialization for Data object {dataset_name}.")
        )

        health_map <- list(
          uuid      = "person_id",
          hh_uuid    = "hh_uuid"

        )

        # Step 1: Super-initialize
        result <- phrutils::phr_try_step(
          {
            # Call parent
            super$initialize(
              data         = data,
              dataset_name = dataset_name,
              uuid = if(is.null(uuid)) {health_map$uuid} else {uuid},
              hh_uuid = if(is.null(hh_uuid)) {health_map$hh_uuid} else {hh_uuid}
              )
          },
        step = private$..phr_txt("Super initialize of IndividualHealthData"),
        hint = private$..phr_txt("Check initialization in parent Asset, Data, or IndividualData classes.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        # Step 2: Map schema vars
        result <- phrutils::phr_try_step(
          {
            private$..map_schema_vars(stage = "raw")
          },
        step = private$..phr_txt("Mapping variables and value maps for IndividualHealthData"),
        hint = private$..phr_txt("Check variable schema, variable and value maps.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        private$..touch(
          log = TRUE,
          touch = TRUE,
          name = private$..dataset_name,
          message = private$..phr_txt("Initialization for {dataset_name} as a HealthInidividualData object completed succesfully with {nrow(data)} records.")
        )

      }, on_error = "abort", origin = "HealthIndividualData$initialize")
    }
  ),

  private = list(

    # @field ..default_variable_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_variable_schema_filename = c(
      "resources",
      "variable_schema_data_individual_health_template.xlsx"
    ),

    # @field ..default_dependency_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_dependency_schema_filename = c(
      "resources",
      "dependency_schema_data_individual_health_template.xlsx"
    ),

    # @field ..value_label Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_indicator_schema_filename = c(
      "resources",
      "indicator_schema_data_individual_health_template.xlsx"
    ),

    ..analytics_registry = function() {

      registry <- super$..analytics_registry()

      c(
        registry,
        list(
          health = function(data,
                            stage,
                            analysis_config,
                            parent) {

            HealthDataAnalytics$new(
              data = data,
              dap = analysis_config,
              parent_data_object = parent,
              dataset_name = paste0(private$..dataset_name, "_HealthData"),
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
