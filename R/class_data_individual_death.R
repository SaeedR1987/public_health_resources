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

        # --- Required and optional columns ---------------------------
        self$set(field = "..required_columns", value = unique(c(
          private$..required_columns  # keep inherited ones (uuid, hh_uuid)
        )))

        # --- Schema merge --------------------------------------------
        self$set(field = "..variable_schema", value = self$default_schema())

        # Load default indicator schema
        default_ind_schema <- self$default_indicator_schema()
        if (is.data.frame(default_ind_schema) && nrow(default_ind_schema) > 0) {
          self$set(field = "..indicator_schema", value = default_ind_schema)
          phrutils::phr_message(
            phrutils::phr_txt("Loaded default indicator schema with {nrow(default_ind_schema)} indicator(s).")
          )
        }

        # Load default dependency schema
        default_dep_schema <- self$default_dependency_schema()
        if (is.data.frame(default_dep_schema) && nrow(default_dep_schema) > 0) {

          self$set(field = "..dependency_schema", value = default_dep_schema)

          phrutils::phr_message(
            phrutils::phr_txt("Loaded default dependency schema with {nrow(default_dep_schema)} dependency/ies.")
          )
        }

        phrutils::phr_message(phrutils::phr_txt("{dataset_name} initialized as DeathIndividualData."))

      }, on_error = "abort", origin = "DeathIndividualData$initialize")
    },

    #' @description
    #' Load the default variable schema for death data
    #'
    #' Reads the variable_schema_data_individual_death_template.xlsx file from package resources
    #' and converts it to the canonical flat variable-schema data frame.
    #'
    #' @return A data frame containing the death data variable schema
    default_schema = function() {

      file <- system.file(
        "resources",
        "variable_schema_data_individual_death_template.xlsx",
        package = "phr"
      )

      if (!file.exists(file)) {
        phrutils::phr_error(
          origin  = "DeathIndividualData$default_schema",
          message = phrutils::phr_txt("variable_schema_data_individual_death_template.xlsx not found in package resources."),
          hint    = phrutils::phr_txt("Place the schema file under inst/resources/ before building the package.")
        )
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_error(
            origin  = "DeathIndividualData$default_schema",
            message = phrutils::phr_txt("Failed to read variable_schema_data_individual_death_template.xlsx"),
            hint    = e$message
          )
        }
      )

      # The xlsx template is already in the canonical flat variable-schema
      # table format (the format expected by `data_diagnose()` and other
      # schema consumers), so validate and store it directly rather than
      # round-tripping it through the nested-list representation.
      private$..validate_table_schema(
        df,
        required_cols = c(
          "rule_type", "variable", "value", "required", "type", "allowed",
          "col_names", "unique",
          "label", "comment",
          "question_type", "is_other", "other_column_link"
        ),
        origin = "DeathIndividualData$default_schema"
      )

      return(df)
    },

    #' @description
    #' Load the default indicator schema for death data
    #'
    #' Reads the indicator_schema_data_individual_death_template.xlsx file from package resources
    #' and converts it to a nested list of indicator definitions (e.g., mortality rates).
    #'
    #' @return A list containing the death data indicator schema
    default_indicator_schema = function() {

      file <- system.file(
        "resources",
        "indicator_schema_data_individual_death_template.xlsx",
        package = "phr"
      )

      empty_indicator_schema <- function() {
        tibble::tibble(
          indicator_name = character(0),
          function_name = character(0),
          variables = character(0),
          arguments = character(0),
          label = character(0),
          comment = character(0)
        )
      }

      if (!file.exists(file)) {
        phrutils::phr_warning(
          origin  = "DeathIndividualData$default_indicator_schema",
          message = phrutils::phr_txt("indicator_schema_data_individual_death_template.xlsx not found in package resources. Continuing without default indicator schema.")
        )
        return(empty_indicator_schema())
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "DeathIndividualData$default_indicator_schema",
            message = phrutils::phr_txt("Failed to read indicator_schema_data_individual_death_template.xlsx: {e$message}")
          )
          return(NULL)
        }
      )

      if (is.null(df)) return(empty_indicator_schema())

      # The xlsx template is already in the canonical flat indicator-schema
      # table format; validate and store it directly rather than converting
      # to the nested-list representation.
      private$..validate_table_schema(
        df,
        required_cols = c(
          "indicator_name",
          "function_name",
          "variables",
          "arguments",
          "label",
          "comment"
        ),
        origin = "DeathIndividualData$default_indicator_schema"
      )

      return(df)
    },

    #' @description
    #' Load the default dependency schema for death data
    #'
    #' Reads the dependency_schema_data_individual_death_template.xlsx file from package resources
    #' and converts it to a nested list of variable dependency rules.
    #'
    #' @return A list containing the death data dependency schema
    default_dependency_schema = function() {

      file <- system.file(
        "resources",
        "dependency_schema_data_individual_death_template.xlsx",
        package = "phr"
      )

      empty_dependency_schema <- function() {
        tibble::tibble(
          rule_type = character(0),
          dep_name = character(0),
          variables = character(0),
          condition_if = character(0),
          then = character(0),
          action = character(0),
          label = character(0),
          comment = character(0)
        )
      }

      if (!file.exists(file)) {
        phrutils::phr_warning(
          origin  = "DeathIndividualData$default_dependency_schema",
          message = phr_txt("dependency_schema_data_individual_death_template.xlsx not found in package resources. Continuing without default dependency schema.")
        )
        return(empty_dependency_schema())
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phrutils::phr_warning(
            origin  = "DeathIndividualData$default_dependency_schema",
            message = phr_txt("Failed to read dependency_schema_data_individual_death_template.xlsx: {e$message}")
          )
          return(NULL)
        }
      )

      if (is.null(df)) return(empty_dependency_schema())

      # The xlsx template is already in the canonical flat dependency-schema
      # table format; validate and store it directly rather than converting
      # to the nested-list representation.
      private$..validate_table_schema(
        df,
        required_cols = c(
          "rule_type",
          "dep_name",
          "variables",
          "condition_if",
          "then",
          "action",
          "label",
          "comment"
        ),
        origin = "DeathIndividualData$default_dependency_schema"
      )

      return(df)
    },


    #' @description
    #' Generate a DataAnalytics object for death individual data.
    #' Combines quality checks and quantitative analysis in one object.
    #'
    #' @param stage The data stage to use ("standardized" or "clean")
    #' @param analysis_config Optional data analysis plan (tibble)
    #' @return A DataAnalytics object or NULL
    generate_data_analytics = function(stage = c("standardized", "clean"),
                                       analysis_config = NULL) {

      stage <- match.arg(stage)

      phrutils::phr_try({

        df <- self$get_data(stage)

        if (is.null(df)) {
          phrutils::phr_warning(
            self$dataset_name,
            phrutils::phr_txt("No {stage} data available for DataAnalytics generation.")
          )
          return(NULL)
        }

        data_hash    <- self$get(field = "..metadata", role = "hash_id")
        variable_map <- private$..variable_map
        value_map    <- private$..value_map

        analytics <- DataAnalytics$new(
          data               = df,
          dap                = analysis_config,
          parent_data_object = self,
          dataset_name       = paste0(self$dataset_name, "_DataAnalytics"),
          data_stage_name    = stage,
          data_hash          = data_hash,
          variable_map       = variable_map,
          value_map          = value_map,
          variable_label     = private$..variable_label,
          value_label        = private$..value_label
        )

        phrutils::phr_message(
          phrutils::phr_txt("Generated DataAnalytics object for {self$dataset_name}.")
        )

        return(analytics)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$generate_data_analytics"))
    }


  )
)
