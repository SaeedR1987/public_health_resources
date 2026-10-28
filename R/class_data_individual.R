#' IPHRA Individual Data Class
#'
#' The `IndividualData` R6 class extends the base [Data] class to represent
#' person-level survey data. Each record corresponds to one individual
#' enumerated within a household roster.
#'
#' @description
#' This subclass enforces domain-specific validation and schema rules for
#' individual data, including:
#' * Required fields for `uuid` (unique person ID), `hh_uuid` (household linkage), `sex`, and `age`
#' * Optional fields for estimated or exact date of birth, age in months/days,
#'   and household joining status
#' * Schema-based validation of column types and value domains
#' * Optional linkage back to a Household dataset
#'
#' @field household_link Optional linkage back to a HouseholdData object
#' @field optional_columns Character vector of optional columns for individual data
#'
#' @seealso [Data], [HouseholdData]
#' @export
IndividualData <- R6::R6Class(
  classname = "IndividualData",
  inherit   = Data,

  public = list(



    #' @description
    #' Creates a new IndividualData object with person-level survey data
    #'
    #' @param data Data frame containing individual-level survey data
    #' @param dataset_name Character name for the dataset (default: "IndividualData")
    #' @param metadata Optional list of metadata attributes
    #' @param variable_map Optional named list mapping variable roles to column names.
    #'   Common roles: uuid (person_id), hh_uuid, sex, age, est_dob, exact_dob, age_months, age_days, joined_hh
    #'
    #' @return A new IndividualData R6 object
    #'
    #' @details
    #' Initialization process:
    #' 1. Merges user variable_map with defaults (uuid defaults to "person_id")
    #' 2. Calls parent Data class initializer
    #' 3. Establishes required columns: uuid, hh_uuid, sex, age
    #' 4. Establishes optional columns: est_dob, exact_dob, age_months, age_days, joined_hh
    #' 5. Loads and merges individual-specific variable schema
    #' 6. Loads default indicator and dependency schemas
    #' 7. Auto-maps schema variables based on column names
    initialize = function(data,
                          dataset_name = "IndividualData",
                          uuid = NULL) {

      phrutils::phr_try({
        # Default mapping for individual-level data
        default_map <- list(
          uuid        = "person_id"        # unique individual ID
          # hh_uuid     = "hh_uuid"     # household linkage

        )

        # Merge user-specified map over defaults
        # variable_map <- modifyList(default_map, variable_map %||% list())

        if(is.null(uuid)) {uuid <- default_map$uuid}

        # Call parent (Data) initializer — uuid now refers to individual identifier
        super$initialize(
          data         = data,
          dataset_name = dataset_name,
          uuid         = uuid
        )

        # ---- Required & optional columns
        # Merge individual-required columns into Data$required_columns
        # (ensure we store *column names*, not roles)
        ind_required <- c(
          private$..variable_map$uuid,     # unique person identifier
          private$..variable_map$hh_uuid,  # household linkage (duplicates allowed)
          private$..variable_map$sex,
          private$..variable_map$age
        )
        ind_required <- unique(ind_required[!is.na(ind_required) & ind_required != ""])

        # Data$initialize already set private$..required_columns <- uuid
        # We extend that with the individual-specific requirements
        private$..required_columns <- unique(c(private$..required_columns, ind_required))

        # Optional (mapped) columns
        private$..optional_columns <- c(
          private$..variable_map$est_dob,
          private$..variable_map$exact_dob,
          private$..variable_map$age_months,
          private$..variable_map$age_days,
          private$..variable_map$joined_hh
        )

        # Re-map schema variables now that the individual schema is loaded.
        # Data$initialize() already mapped against the (still empty) parent
        # schema, so roles such as hh_uuid would otherwise stay unmapped until
        # the first standardize() call.
        private$..map_schema_vars(stage = "raw")

        phrutils::phr_message(phrutils::phr_txt("{dataset_name} initialized as IndividualData object."))

      }, on_error = "abort", origin = "IndividualData$initialize")
    }

  ),

  private = list(

    # Additional fields

    ..household_link   = NULL,  # linkage back to HouseholdData (optional)
    ..optional_columns = NULL,

    # @field ..default_variable_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_variable_schema_filename = c(
      "resources",
      "variable_schema_data_individual_roster_template.xlsx"
    ),

    # @field ..default_dependency_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_dependency_schema_filename = c(
      "resources",
      "dependency_schema_data_individual_roster_template.xlsx"
    ),

    # @field ..value_label Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_indicator_schema_filename = c(
      "resources",
      "indicator_schema_data_individual_roster_template.xlsx",
    ),

    #' Post-Validation Hook for Individual Data
    #'
    #' @description
    #' Hook method called after core validation completes. Performs individual-specific validation checks.
    #'
    #' @param df Data frame that was validated. If NULL, uses raw data from self$get_data("raw")
    #'
    #' @return Logical indicating validation success (TRUE if no issues, FALSE if warnings occurred)
    #'
    #' @details
    #' Performs individual-specific checks:
    #' * Household linkage validation (informational - duplicate hh_uuid expected for multi-member households)
    #' * Age validation (checks for negative ages)
    #' * Optional household link integrity check if household_link field is set
    ..post_validate = function(df = NULL) {

      nm <- private$..dataset_name

      # Track whether any issues occurred
      had_issues <- FALSE

      phrutils::phr_try({

        if (is.null(df)) df <- self$get_data("raw")

        # Household linkage may duplicate — informational only
        hh_uuid_col <- private$..variable_map$hh_uuid
        if (!is.null(hh_uuid_col) && hh_uuid_col %in% names(df)) {
          dup_hh <- df[[hh_uuid_col]][duplicated(df[[hh_uuid_col]])]
          if (length(dup_hh) > 0) {
            phrutils::phr_message(nm, phrutils::phr_txt(
              "Duplicate household linkages detected (expected for multi-member households)."
            ))
          }
        }

        # Check that age is non-negative
        age_col <- private$..variable_map$age
        if (!is.null(age_col) && age_col %in% names(df)) {
          if (any(df[[age_col]] < 0, na.rm = TRUE)) {
            phrutils::phr_warning(nm, phrutils::phr_txt("Negative ages detected."))
            had_issues <- TRUE
          }
        }

        # Optional link integrity check
        if (!is.null(private$..household_link)) {
          phrutils::phr_message(phrutils::phr_txt("Validating linked HouseholdData (placeholder)."))
        }

        phrutils::phr_message(phrutils::phr_txt("Post-validation for {nm} complete."))

      }, on_error = "warn", origin = paste0(private$..dataset_name, "$post_validate"))

      # Return TRUE (good) or FALSE (warnings occurred)
      return(!had_issues)
    },

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
