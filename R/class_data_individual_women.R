#' IPHRA Individual Woman Data Class
#'
#' Extends IndividualData by adding a comprehensive schema for
#' individual-level health data for women of reproductive age including:
#' sexual and reproductive health service coverage, dietary diversity,
#' and MUAC indicators.
#'
#' @export
WomanIndividualData <- R6::R6Class(
  classname = "WomanIndividualData",
  inherit   = IndividualData,
  public = list(

    #' @description
    #' Initialize a new WomanIndividualData object
    #'
    #' @param data A data frame containing individual health records
    #' @param dataset_name A name for this dataset
    #' @param metadata Optional list of metadata
    #' @param variable_map Optional named list mapping standard variable names to data columns
    #' @return A new HealthIndividualData object
    initialize = function(data,
                          dataset_name = "WomanIndividualData",
                          metadata = NULL,
                          variable_map = NULL) {

      phrutils::phr_try({

        private$..touch(
          log = TRUE,
          touch = FALSE,
          name = dataset_name,
          message = phrutils::phr_txt("Starting initialization for WomanIndividualData object {dataset_name}.")
        )

        woman_map <- list(
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
              uuid = if (is.null(uuid)) {woman_map$uuid} else {uuid},
              hh_uuid = if (is.null(hh_uuid)) {woman_map$hh_uuid} else {hh_uuid}
            )
          },
          step = phrutils::phr_txt("Super initialize of WomanIndividualData"),
          hint = phrutils::phr_txt("Check initialization in parent Asset, Data or IndividualData classes.")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        # Step 2: Setting optional columns
        result <- phrutils::phr_try_step(
          {
            self$set(field = "..optional_columns",
                     value = c(private$..optional_columns, unlist(woman_map)))

          },
          step = phrutils::phr_txt("Loading health specific fields in initialize"),
          hint = phrutils::phr_txt("Check water container data and inputs")
          )
          if (phrutils::phr_failed(result)) {
            return(result)
          }

        # Step 2: Initial mapping of schema vars for woman individual data
        result <- phrutils::phr_try_step(
          {
            private$..map_schema_vars(stage = "raw")
          },
          step = phrutils::phr_txt("Populate variable and value maps for woman data"),
          hint = phrutils::phr_txt("Check variable and value maps for woman variables")
        )
        if (phrutils::phr_failed(result)) {
          return(result)
        }

        private$..touch(
          log = TRUE,
          touch = FALSE,
          name = dataset_name,
          message = phrutils::phr_txt("Initialization for {dataset_name} as WomanIndividualData object completed successfully with {nrow(data)} records.")
        )

      }, on_error = "abort", origin = "WomanIndividualData$initialize")
    },
  ),

  private = list(

    # @field ..default_variable_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_variable_schema_filename = c(
      "resources",
      "variable_schema_data_individual_maternalhealth_template.xlsx"
    ),

    # @field ..default_dependency_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_dependency_schema_filename = c(
      "resources",
      "dependency_schema_data_individual_maternalhealth_template.xlsx"
    ),

    # @field ..value_label Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_indicator_schema_filename = c(
      "resources",
      "indicator_schema_data_individual_maternalhealth_template.xlsx",
    ),

    ..analytics_registry = function() {

      registry <- super$..analytics_registry()

      c(
        registry,
        list(
          woman = function(data,
                            stage,
                            analysis_config,
                            parent) {

            WomanDataAnalytics$new(
              data = data,
              dap = analysis_config,
              parent_data_object = parent,
              dataset_name = paste0(private$..dataset_name, "_WomanData"),
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
