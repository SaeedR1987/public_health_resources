#' IPHRA Nutrition Individual Data Class
#'
#' Extends IndividualData by adding a comprehensive schema for
#' individual-level anthropometric and nutrition assessment data including:
#' MUAC measurements, weight/height, z-score calculations,
#' malnutrition classification, and infant feeding indicators.
#'
#' This class incorporates MUAC-for-age z-score calculations and
#' provides data quality checks and visualization capabilities.
#'
#' @export
NutritionIndividualData <- R6::R6Class(
  classname = "NutritionIndividualData",
  inherit   = IndividualData,

  public = list(

    #' @description
    #' Initialize a new NutritionIndividualData object
    #'
    #' @param data A data frame containing individual nutrition/anthropometry records
    #' @param dataset_name A name for this dataset
    #' @param metadata Optional list of metadata
    #' @param variable_map Optional named list mapping standard variable names to data columns
    #' @return A new NutritionIndividualData object
    initialize = function(data,
                          dataset_name = "NutritionIndividualData",
                          metadata = NULL,
                          variable_map = NULL) {

      phrutils::phr_try({

        nutrition_map <- list(
          # Demographics (use generic names that map to data columns)
          uuid      = "person_id",
          hh_uuid    = "hh_uuid"
        )

        variable_map <- modifyList(nutrition_map, variable_map %||% list())

        # Call parent
        super$initialize(
          data         = data,
          dataset_name = dataset_name,
          metadata     = metadata,
          variable_map = variable_map
        )

        # Extend optional columns
        private$..optional_columns <- c(private$..optional_columns, unlist(nutrition_map))

        private$..map_schema_vars(stage = "raw")

        phrutils::phr_message(
          phrutils::phr_txt("{dataset_name} initialized as NutritionIndividualData object.")
        )

      }, on_error = "abort", origin = "NutritionIndividualData$initialize")
    }
  ),

  private = list(

    # @field ..default_variable_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_variable_schema_filename = c(
      "resources",
      "variable_schema_data_individual_nutrition_template.xlsx"
    ),

    # @field ..default_dependency_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_dependency_schema_filename = c(
      "resources",
      "dependency_schema_data_individual_nutrition_template.xlsx"
    ),

    # @field ..value_label Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_indicator_schema_filename = c(
      "resources",
      "indicator_schema_data_individual_nutrition_template.xlsx",
    ),

    ..analytics_registry = function() {

      registry <- super$..analytics_registry()

      c(
        registry,
        list(
          nutrition = function(data,
                              stage,
                              analysis_config,
                              parent) {

            NutritionDataAnalytics$new(
              data = data,
              dap = analysis_config,
              parent_data_object = parent,
              dataset_name = paste0(private$..dataset_name, "_NutritionData"),
              data_stage_name = stage,
              data_hash = parent$get(field = "..metadata", role = "hash_id"),
              variable_map = parent$get(field = "..variable_map"),
              value_map = parent$get(field = "..value_map"),
              variable_label = parent$get(field = "..variable_label"),
              value_label = parent$get(field = "..value_label")
            )
          },

        iycf = function(data,
                        stage,
                        analysis_config,
                        parent) {

          IYCFDataAnalytics$new(
            data               = data,
            dap                = analysis_config,
            parent_data_object = parent,
            dataset_name       = paste0(private$..dataset_name, "_IYCFDataAnalytics"),
            data_stage_name    = stage,
            data_hash          = parent$get(field = "..metadata", role = "hash_id"),
            variable_map       = parent$get(field = "..variable_map"),
            value_map          = parent$get(field = "..value_map"),
            variable_label     = parent$get(field = "..variable_label"),
            value_label        = parent$get(field = "..value_label")
          )

        }

        )
      )
    }

  )
)
