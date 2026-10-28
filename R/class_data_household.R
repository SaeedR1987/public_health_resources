#' IPHRA Household Data Class
#'
#' The `HouseholdData` R6 class extends the base [Data] class to represent
#' household-level survey data. Each record corresponds to one household,
#' providing essential metadata for enumeration, sampling, and linkage.
#'
#' @description
#' This subclass enforces domain-specific validation and schema rules for
#' household data, including:
#' * Required fields for `uuid`, `consent`, `interview_date`, `enumerator_id`
#' * Optional fields for `cluster_id`, `strata`, `weight`, `adm1`, `adm2`,
#'   `gps_lat`, and `gps_lon`
#' * Schema-based validation of column types and value domains
#' * Household-specific post-validation (e.g., GPS range checks, weights)
#' * Generation of `srvyr` survey design objects for weighted analyses
#' * Support for linked datasets via inherited `linked_objects` field from Data class
#'   with automatic aggregation during standardization
#'
#' @details
#' The `linked_objects` field (inherited from Data class) is a named list that can
#' contain Data class objects (or subclasses). Use `add_linked_dataset(name, data_object)`
#' to add linked datasets.
#'
#' **Allowed link names and their expected Data subclasses:**
#' * `roster`: IndividualData - household roster data
#' * `deaths`: DeathIndividualData - death records
#' * `water_containers`: WaterContainerData - water container assessments
#' * `women`: WomenIndividualData - women's questionnaire data
#' * `nutrition`: NutritionIndividualData - nutrition assessments
#' * `health`: HealthIndividualData - health assessments
#'
#' When `standardize()` is called, the `pre_standardize()` hook will:
#' 1. Generate survey weights via `generate_weights()` when a `SamplingFrame` is
#'    stored in `private$..sampling_frame` and a stratum role is mapped in `variable_map`
#'    (skipped silently if either is unavailable).
#' 2. Merge household-level variables to all linked datasets (if available):
#'    `enum_id`, `device_id`, `date_survey`, `date_recall`, `weight`, `stratum`,
#'    `cluster_id`, `site`, `admin1`, `admin2`, `admin3`, `admin4`. Columns are
#'    added to the linked dataset under their canonical role names and registered
#'    in the linked object's `variable_map`; existing variables with those names
#'    are overwritten with household values.
#' 3. Standardize the linked datasets, re-standardizing any that were already
#'    standardized when household variables were merged into them
#' 4. Aggregate data from linked datasets and add columns to household data with
#'    a naming convention like `linked_<name>_<column>` to show the source
#'
#' **Supported aggregations:**
#' * DeathIndividualData: Sums death counts and person_time by household. If
#'   `calc_date_birth_final` is available and recall_date exists, also counts births
#'   occurring in the recall period.
#' * WaterContainerData: Sums total liters by household and counts number of containers
#' * IndividualData (roster): Calculates household size, demographics (children,
#'   male/female counts, women 15-49), and person_time by household. If
#'   `calc_date_birth_final` is available and household has recall_date and survey_date
#'   columns, also counts births occurring in the recall period.
#' * NutritionIndividualData: Aggregates children under 2 years, children 2 to <5 years,
#'   and total children <5 years based on age in months
#' * HealthIndividualData: Counts total number of people recorded (rows)
#' * WomenIndividualData: Counts women aged 15-49 years based on age
#'
#' Utility methods such as `summarize_weights()` provide quick field diagnostics.
#'
#' @field optional_columns Character vector of optional columns commonly present in household data
#' @field sampling_frame Optional \code{\link{SamplingFrame}} object used by \code{generate_weights()}
#'   to derive survey weights from population totals per stratum.
#'
#' @seealso [Data], [IndividualData], [DeathIndividualData], [WaterContainerData],
#'   [NutritionIndividualData], [HealthIndividualData]
#' @export
HouseholdData <- R6::R6Class(
  classname = "HouseholdData",
  inherit = Data,

  public = list(
    # Additional fields

    # linked_datasets removed - now using inherited linked_objects from Data class
    optional_columns = NULL, # non-required but commonly present columns

    #' @description
    #' Creates a new HouseholdData object with household-level survey data
    #'
    #' @param data Data frame containing household-level survey data
    #' @param dataset_name Character name for the dataset (default: "HouseholdData")
    #' @param variable_map Optional named list mapping variable roles to column names.
    #'   Common roles: uuid, consent, date_survey, enum_id, cluster_id, stratum, weight, admin1, admin2, gps_lat, gps_lon
    #'
    #' @return A new HouseholdData R6 object
    #'
    #' @details
    #' Initialization process:
    #' 1. Merges user variable_map with defaults
    #' 2. Calls parent Data class initializer
    #' 3. Loads and merges household-specific variable schema
    #' 4. Loads default indicator and dependency schemas
    #' 5. Auto-maps schema variables based on column names
    #' 6. Establishes required and optional columns
    initialize = function(
      data,
      dataset_name = "HouseholdData"
    ) {
      phrutils::phr_try(
        {
          # Default variable map for common household fields (roles -> columns)
          default_map <- list(
            uuid = "uuid" # inherited identifier
          )

          # Merge user-specified map over defaults
          # variable_map <- modifyList(default_map, variable_map %||% list())

          # Call parent initializer first
          super$initialize(
            data = data,
            dataset_name = dataset_name,
            uuid = default_map$uuid
          )

          # 5) Merge household-required columns into Data$required_columns
          #    (ensure we store *column names*, not roles)

          hh_required <- c(
            private$..variable_map$uuid,
            private$..variable_map$consent,
            private$..variable_map$date_survey,
            private$..variable_map$enum_id
          )
          hh_required <- unique(hh_required[
            !is.na(hh_required) & hh_required != ""
          ])

          # Data$initialize already set private$..required_columns <- uuid
          # We extend that with the household-specific requirements

          self$set(
            field = "..required_columns",
            value = unique(c(private$..required_columns, hh_required))
          )

          # private$..required_columns <- unique(c(private$..required_columns, hh_required))

          # Optional (mapped) columns

          self$set(
            field = "optional_columns",
            value = c(
              private$..variable_map$cluster_id,
              private$..variable_map$stratum,
              private$..variable_map$weight,
              private$..variable_map$admin1,
              private$..variable_map$admin2,
              private$..variable_map$gps_lat,
              private$..variable_map$gps_lon
            )
          )

          private$..map_schema_vars(stage = "raw")

          phrutils::phr_message(phrutils::phr_txt(
            "{dataset_name} initialized as HouseholdData object."
          ))
        },
        on_error = "abort",
        origin = "HouseholdData$initialize"
      )
    },

    #' Generate Survey Weights from a Sampling Frame
    #'
    #' @description
    #' Computes survey weights for each record by matching stratum values in the
    #' dataset against population totals held in a \code{\link{SamplingFrame}}.
    #' The weight for every row in stratum \eqn{s} is:
    #' \deqn{w_s = \frac{N_s / N}{n_s / n}}
    #' where \eqn{N_s} is the total population of stratum \eqn{s} (sum of
    #' \code{population_size} in the \code{SamplingFrame}), \eqn{N} is the grand
    #' total population across all strata, \eqn{n_s} is the number of surveyed
    #' households in stratum \eqn{s}, and \eqn{n} is the total sample size.
    #' This is the standard design weight for stratified sampling (proportion of
    #' population in the stratum divided by the proportion of the sample in the
    #' stratum).
    #'
    #' @param sampling_frame Optional \code{\link{SamplingFrame}} object. When
    #'   \code{NULL} (default), the method falls back to \code{self$sampling_frame}.
    #'   If neither is available the method is skipped with an informational message.
    #' @param stage Character. Which data stage to add the weight column to.
    #'   One of \code{"standardized"} (default), \code{"clean"}, or \code{"raw"}.
    #'
    #' @return Invisible \code{self} (for method chaining). Modifies the dataset
    #'   in place by adding or overwriting the mapped weight column and, if no
    #'   weight column was previously mapped, registers \code{"survey_weight"} as
    #'   the \code{weight} role in \code{variable_map}.
    #'
    #' @details
    #' Column selection priority for writing weights:
    #' \enumerate{
    #'   \item If \code{variable_map$weight} is already set and that column exists
    #'     in the dataset, the existing column is overwritten.
    #'   \item Otherwise the weights are written to a new column called
    #'     \code{"survey_weight"} and \code{variable_map$weight} is updated
    #'     accordingly.
    #' }
    #'
    #' Rows whose stratum value is \code{NA}, empty, or absent from the
    #' \code{SamplingFrame} receive \code{NA} weights and a warning is issued.
    generate_weights = function(
    sampling_frame = NULL,
    stage = c("standardized", "clean", "raw")
    ) {
      stage <- match.arg(stage)
      origin <- paste0(private$..dataset_name, "$generate_weights")

      phrutils::phr_try(
        {
          # 1. Resolve SamplingFrame

          sf <- sampling_frame %||% private$..sampling_frame

          if (is.null(sf)) {
            phrutils::phr_message(phrutils::phr_txt(
              "No SamplingFrame available for {private$..dataset_name}. Skipping generate_weights."
            ))
            return(invisible(self))
          }

          if (!inherits(sf, "SamplingFrame")) {
            phrutils::phr_warning(
              origin,
              phrutils::phr_txt(
                "sampling_frame is not a SamplingFrame object. Skipping generate_weights."
              )
            )
            return(invisible(self))
          }

          sf_df <- sf$log_df
          if (is.null(sf_df) || nrow(sf_df) == 0) {
            phrutils::phr_message(phr_txt(
              "SamplingFrame is empty. Skipping generate_weights."
            ))
            return(invisible(self))
          }

          # 2. Resolve stratum column

          stratum_col <- private$..variable_map$stratum

          if (is.null(stratum_col) || stratum_col == "" || is.na(stratum_col)) {
            phrutils::phr_message(phr_txt(
              "No stratum role found in variable_map for {private$..dataset_name}. Skipping generate_weights."
            ))
            return(invisible(self))
          }

          # 3. Get dataset for the requested stage

          df <- self$get_data(stage)

          if (is.null(df) || nrow(df) == 0) {
            phrutils::phr_warning(
              origin,
              phrutils::phr_txt(
                "No {stage} data available in {private$..dataset_name}. Skipping generate_weights."
              )
            )
            return(invisible(self))
          }

          if (!stratum_col %in% names(df)) {
            phrutils::phr_message(
              phrutils::phr_txt(
                "Stratum column '{stratum_col}' not found in {stage} data for {private$..dataset_name}. Skipping generate_weights."
              )
            )
            return(invisible(self))
          }

          # 4. Compute population totals per stratum and grand total (N)

          sf_totals <- sf_df |>
            dplyr::mutate(stratum = as.character(.data$stratum)) |>
            dplyr::group_by(.data$stratum) |>
            dplyr::summarize(
              pop_total = sum(.data$population_size, na.rm = TRUE),
              .groups = "drop"
            )

          N <- sum(sf_totals$pop_total, na.rm = TRUE)

          # 5. Compute sample counts per stratum and total sample size (n)

          stratum_vals <- as.character(df[[stratum_col]])
          valid_mask <- !is.na(stratum_vals) & stratum_vals != ""

          n <- sum(valid_mask) # total sample size (excluding NA/empty strata)

          sample_counts_df <- data.frame(stratum = stratum_vals[valid_mask]) |>
            dplyr::group_by(.data$stratum) |>
            dplyr::summarize(n_s = dplyr::n(), .groups = "drop")

          # Warn if any strata in dataset had no match in the SamplingFrame
          unmatched <- setdiff(
            unique(stratum_vals[valid_mask]),
            sf_totals$stratum
          )
          if (length(unmatched) > 0) {
            phrutils::phr_warning(
              origin,
              phrutils::phr_txt(
                "The following strata in the dataset were not found in the SamplingFrame and received NA weights: {paste(unmatched, collapse=', ')}"
              )
            )
          }

          # 6. Build per-row weight vector via vectorized join
          #    Formula: w_s = (N_s / N) / (n_s / n)

          weight_lookup <- dplyr::left_join(
            sample_counts_df,
            sf_totals,
            by = "stratum"
          ) |>
            dplyr::mutate(weight = (.data$pop_total / N) / (.data$n_s / n))

          row_strata <- data.frame(stratum = stratum_vals)
          weight_vec <- dplyr::left_join(
            row_strata,
            weight_lookup,
            by = "stratum"
          )$weight

          # 7. Determine weight column name and update variable_map if needed

          existing_weight_col <- private$..variable_map$weight
          if (
            !is.null(existing_weight_col) &&
            !is.na(existing_weight_col) &&
            existing_weight_col != "" &&
            existing_weight_col %in% names(df)
          ) {
            weight_col <- existing_weight_col
            phrutils::phr_message(phrutils::phr_txt(
              "Using existing weight column '{weight_col}' from variable_map."
            ))
          } else {
            weight_col <- "survey_weight"
            private$..variable_map[["weight"]] <- weight_col
            phrutils::phr_message(phrutils::phr_txt(
              "No existing weight column mapped. Writing weights to '{weight_col}' and updating variable_map."
            ))
          }

          # 8. Add / overwrite weight column in the dataset

          df[[weight_col]] <- weight_vec

          if (stage == "standardized") {
            private$..standardized_data <- df
          } else if (stage == "clean") {
            private$..clean_data <- df
          } else {
            private$..raw_data <- df
          }

          phrutils::phr_message(
            phrutils::phr_txt(
              "Survey weights generated and added to '{weight_col}' column in {stage} data for {private$..dataset_name}."
            )
          )

          invisible(self)
        },
        on_error = "warn",
        origin = origin
      )
    }
  ),

  private = list(
    # @field ..default_variable_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_variable_schema_filename = c(
      "resources",
      "variable_schema_data_household_template.xlsx"
    ),

    # @field ..default_dependency_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_dependency_schema_filename = c(
      "resources",
      "dependency_schema_data_household_template.xlsx"
    ),

    # @field ..value_label Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_indicator_schema_filename = c(
      "resources",
      "indicator_schema_data_household_template.xlsx",
    ),

    ..sampling_frame = NULL, # optional SamplingFrame for generate_weights()

    #' Post-Validation Hook for Household Data
    #'
    #' @description
    #' Hook method called after core validation completes. Performs household-specific validation checks.
    #'
    #' @param df Data frame that was validated
    #'
    #' @return Logical indicating validation success (TRUE if no issues, FALSE if warnings occurred)
    #'
    #' @details
    #' Performs household-specific checks:
    #' * GPS coordinate range validation (lat: -90 to 90, lon: -180 to 180)
    #' * Weight distribution checks (negative or zero weights)
    #' * Interview date validity checks
    #' * Consent field validation
    ..post_validate = function(df) {
      nm <- private$..dataset_name

      # Track whether any issues occurred
      had_issues <- FALSE

      # GPS validity check

      lat_col <- private$..variable_map$gps_lat
      lon_col <- private$..variable_map$gps_lon

      if (
        !is.null(lat_col) &&
        lat_col %in% names(df) &&
        !is.null(lon_col) &&
        lon_col %in% names(df)
      ) {
        lat_invalid <- which(
          !is.na(df[[lat_col]]) &
            (df[[lat_col]] < -90 | df[[lat_col]] > 90)
        )
        lon_invalid <- which(
          !is.na(df[[lon_col]]) &
            (df[[lon_col]] < -180 | df[[lon_col]] > 180)
        )

        if (length(lat_invalid) > 0 || length(lon_invalid) > 0) {
          phrutils::phr_warning(
            nm,
            phrutils::phr_txt(
              "Some GPS coordinates appear invalid (latitudes outside [-90,90] or longitudes outside [-180,180])."
            )
          )
          had_issues <- TRUE
        }
      }

      # Weight checks

      if (
        !is.null(private$..variable_map$weight) &&
        private$..variable_map$weight %in% names(df)
      ) {
        wts <- suppressWarnings(as.numeric(df[[private$..variable_map$weight]]))

        if (any(is.na(wts))) {
          phrutils::phr_warning(
            nm,
            phrutils::phr_txt("Missing values found in weight variable.")
          )
          had_issues <- TRUE
        }

        if (any(wts < 0, na.rm = TRUE)) {
          phrutils::phr_warning(
            nm,
            phrutils::phr_txt("Negative weights detected.")
          )
          had_issues <- TRUE
        }
      }

      # Strata checks

      if (
        !is.null(private$..variable_map$strata) &&
        private$..variable_map$strata %in% names(df)
      ) {
        strata_vals <- df[[private$..variable_map$strata]]

        if (any(is.na(strata_vals))) {
          phrutils::phr_warning(
            nm,
            phrutils::phr_txt("Missing strata values detected.")
          )
          had_issues <- TRUE
        }
      }

      phrutils::phr_message(phrutils::phr_txt(
        "Post-validation for {nm} complete."
      ))

      # Return TRUE (good) or FALSE (warnings occurred)
      return(!had_issues)
    },

    #' Pre-Standardize Hook for Household Data
    #'
    #' @description
    #' Hook method called before standardization begins. Generates survey weights
    #' (if a SamplingFrame and stratum mapping are available), then checks linked
    #' datasets and aggregates data from them.
    #'
    #' @param stage Character string specifying source stage: "clean", "standardized", or "raw"
    #'
    #' @return NULL (modifies household data in place by adding aggregated columns)
    #'
    #' @details
    #' This hook performs four key operations:
    #' 1. Generates survey weights via \code{generate_weights()} when a
    #'    \code{SamplingFrame} is stored in \code{self$sampling_frame} and a
    #'    stratum role is mapped in \code{variable_map}; skipped silently otherwise.
    #' 2. Merges household-level variables to linked datasets using their canonical
    #'    role names (enum_id, device_id, date_survey, date_recall, weight, stratum,
    #'    cluster_id, site, admin1-admin4) and registers those roles in each linked
    #'    object's \code{variable_map}
    #' 3. Standardizes each linked dataset. Datasets that were already standardized
    #'    are standardized again when household variables were merged into them, so
    #'    that indicators depending on the merged columns (e.g. person-time, which
    #'    needs \code{date_survey} and \code{date_recall}) are recalculated
    #' 4. Aggregates data from linked datasets back to household level with prefix "linked_<name>_<column>"
    #'
    #' Supported aggregations vary by dataset type (see class documentation for details)
    ..pre_standardize = function(stage = c("clean", "standardized", "raw")) {
      stage <- match.arg(stage)

      phrutils::phr_try(
        {
          # Generate survey weights if a SamplingFrame and stratum mapping are available.
          # We write to raw_data here so the weight column is present when standardize()
          # copies raw_data -> standardized_data in subsequent steps.
          self$generate_weights(stage = "raw")

          # Refresh the household variable_map before merging so that any
          # household columns added after initialization (e.g. a computed
          # date_recall column) are mapped and therefore eligible for merging
          # into the linked datasets.
          private$..map_schema_vars(stage = "raw")

          # Check if any linked objects exist (using inherited linked_objects from Data class)
          if (length(private$..linked_objects) == 0) {
            phrutils::phr_message(phrutils::phr_txt(
              "No linked datasets found in {private$..dataset_name}."
            ))
            return(invisible(NULL))
          }

          phrutils::phr_message(phrutils::phr_txt(
            "Processing {length(private$..linked_objects)} linked dataset(s) for {private$..dataset_name}..."
          ))

          # Iterate through linked objects
          for (link_name in names(private$..linked_objects)) {
            link_info <- private$..linked_objects[[link_name]]
            linked_obj <- link_info$object

            # Validate that the linked object is a Data class or subclass
            if (!inherits(linked_obj, "Data")) {
              phrutils::phr_warning(
                private$..dataset_name,
                phrutils::phr_txt(
                  "Linked object '{link_name}' is not a Data class object. Skipping."
                )
              )
              next
            }

            # First merge household-level variables to linked dataset
            # This needs to happen before standardize so household vars are available
            # in variable and value maps during standardize operations
            merged <- private$..merge_household_vars_to_linked(
              linked_obj,
              link_name,
              hh_stage = stage,
              linked_stage = stage
            )

            # Then (re-)standardize the linked dataset. An already-standardized
            # dataset must be standardized again when household variables were
            # merged, otherwise the newly added columns are never picked up by
            # its variable_map or by indicator functions that depend on them
            # (e.g. add_persontime() needs date_survey and date_recall).
            phrutils::phr_message(phrutils::phr_txt(
              "Checking standardization status of linked dataset '{link_name}'..."
            ))
            if (!linked_obj$standardized) {
              phrutils::phr_message(phrutils::phr_txt(
                "Linked dataset '{link_name}' is not standardized. Running standardize()..."
              ))
              linked_obj$standardize(stage = stage)
            } else if (isTRUE(merged)) {
              phrutils::phr_message(phrutils::phr_txt(
                "Linked dataset '{link_name}' is already standardized but received merged household variables. Re-running standardize()..."
              ))
              linked_obj$standardize(stage = stage)
            } else {
              phrutils::phr_message(phr_txt(
                "Linked dataset '{link_name}' is already standardized."
              ))
            }
          }

          # Get household data using fallback logic
          hh_data_result <- self$get_data_with_fallback(self, stage)
          if (is.null(hh_data_result)) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "No household data available (tried stages: clean, standardized, raw). Cannot aggregate linked data."
              )
            )
            return(invisible(NULL))
          }

          hh_data <- hh_data_result$data
          hh_data_stage <- hh_data_result$stage
          phrutils::phr_message(phr_txt(
            "Using household data from stage: {hh_data_stage}"
          ))

          # Now aggregate data from linked datasets and add to household data
          hh_data_updated <- private$..aggregate_linked_data(
            hh_data,
            stage = stage
          )

          # Update the appropriate household data stage
          if (hh_data_stage == "clean") {
            private$..clean_data <- hh_data_updated
          } else if (hh_data_stage == "standardized") {
            private$..standardized_data <- hh_data_updated
          } else {
            private$..raw_data <- hh_data_updated
          }

          phrutils::phr_message(phrutils::phr_txt(
            "Pre-standardize processing complete for {private$..dataset_name}."
          ))
        },
        on_error = "warn",
        origin = paste0(private$..dataset_name, "$pre_standardize")
      )
    },

    #' Sub Class Specific Pre Generate Cleaning Log for Linked Datasets
    #'
    #' @description
    #' The household data class specific post generate cleaning log will iterate
    #' through  dataset by calling the parent
    #' implementation, then propagates the call to all linked data objects so
    #' that each linked dataset produces its own cleaning log entries.
    #'
    #' @param stage Character string specifying the data stage to use:
    #'   "standardized" (default), "clean", or "raw"
    #' @param overwrite Logical; if TRUE, clears the existing cleaning log before
    #'   generating new entries (default: FALSE)
    #'
    #' @return NULL (invisibly). Side effect: populates cleaning logs on the
    #'   household object and on every linked data object.
    ..pre_generate_cleaning_log = function(
    stage = "standardized",
    overwrite = FALSE
    ) {
      phrutils::phr_try(
        {
          if (length(self$linked_objects) == 0) {
            return(invisible(NULL))
          }

          phrutils::phr_message(phrutils::phr_txt(
            "Generating cleaning log for {length(private$..linked_objects)} linked dataset(s) in {private$..dataset_name}..."
          ))

          for (link_name in names(private$..linked_objects)) {
            link_info <- private$..linked_objects[[link_name]]
            linked_obj <- link_info$object

            if (!inherits(linked_obj, "Data")) {
              phrutils::phr_warning(
                self$dataset_name,
                phrutils::phr_txt(
                  "Linked object '{link_name}' is not a Data class object. Skipping."
                )
              )
              next
            }

            phrutils::phr_message(phrutils::phr_txt(
              "Generating cleaning log for linked dataset '{link_name}'..."
            ))
            linked_obj$generate_cleaning_log(
              stage = stage,
              overwrite = overwrite
            )
          }
        },
        on_error = "warn",
        origin = paste0(private$..dataset_name, "$generate_cleaning_log")
      )
    },

    #' Pre clean for HouseholdData object
    #'
    #' @description
    #' Runs clean on all linked Data Objects in the Household Data object.
    #'
    #' @return NULL (invisibly). Side effect: sets \code{clean_data}
    #'   on every linked data object.
    ..pre_clean = function() {
      # Run parent implementation for the household dataset itself

      phrutils::phr_try(
        {
          if (length(private$..linked_objects) == 0) {
            return(invisible(NULL))
          }

          phrutils::phr_message(phrutils::phr_txt(
            "Cleaning {length(private$..linked_objects)} linked dataset(s) in {private$..dataset_name}..."
          ))

          for (link_name in names(private$..linked_objects)) {
            link_info <- private$..linked_objects[[link_name]]
            linked_obj <- link_info$object

            if (!inherits(linked_obj, "Data")) {
              phrutils::phr_warning(
                private$..dataset_name,
                phrutils::phr_txt(
                  "Linked object '{link_name}' is not a Data class object. Skipping."
                )
              )
              next
            }

            phrutils::phr_message(phrutils::phr_txt(
              "Cleaning linked dataset '{link_name}'..."
            ))
            linked_obj$clean()
          }
        },
        on_error = "warn",
        origin = paste0(private$..dataset_name, "$clean")
      )
    },


    #' Aggregate Linked Dataset Data
    #'
    #' @description
    #' Internal helper that processes linked datasets and adds aggregated columns to household data
    #'
    #'
    #' @param hh_data Data frame of household data to augment with aggregated columns
    #' @param stage Character string specifying data stage: "clean", "standardized", or "raw"
    #'
    #' @return Modified household data frame with additional aggregated columns
    #'
    #' @details
    #' For each linked dataset, performs type-specific aggregations and adds columns with
    #' naming convention "linked_<name>_<column>". See class documentation for specific
    #' aggregation rules by dataset type.
    ..aggregate_linked_data = function(
      hh_data,
      stage = c("clean", "standardized", "raw")
    ) {
      stage <- match.arg(stage)

      # Store original hh_data to ensure we never return NULL
      original_hh_data <- hh_data

      result <- phrutils::phr_try(
        {
          # Use linked_objects (inherited from Data class) instead of linked_datasets
          if (length(private$..linked_objects) == 0) {
            return(hh_data)
          }

          # Get household UUID column
          hh_uuid_col <- private$..uuid

          if (!hh_uuid_col %in% names(hh_data)) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "Household UUID column '{hh_uuid_col}' not found. Cannot aggregate linked data."
              )
            )
            return(hh_data)
          }

          # Process each linked dataset
          for (link_name in names(private$..linked_objects)) {
            link_info <- private$..linked_objects[[link_name]]
            linked_obj <- link_info$object

            # Skip if not a Data object
            if (!inherits(linked_obj, "Data")) {
              next
            }

            # Get linked data using fallback logic
            linked_data_result <- self$get_data_with_fallback(
              linked_obj,
              stage
            )
            if (is.null(linked_data_result)) {
              phrutils::phr_warning(
                private$..dataset_name,
                phrutils::phr_txt(
                  "Linked dataset '{link_name}' has no data at any stage."
                )
              )
              next
            }

            linked_data <- linked_data_result$data
            linked_data_stage <- linked_data_result$stage
            phrutils::phr_message(phrutils::phr_txt(
              "Using linked dataset '{link_name}' from stage: {linked_data_stage}"
            ))

            # Determine the household linkage column in the linked dataset
            # Try to find hh_uuid in variable_map first
            linked_hh_col <- linked_obj$get(
              field = "..variable_map"
            )$hh_uuid %||%
              linked_obj$get(field = "..variable_map")$household_uuid %||%
              "hh_uuid"

            if (!linked_hh_col %in% names(linked_data)) {
              phrutils::phr_warning(
                private$..dataset_name,
                phrutils::phr_txt(
                  "Linked dataset '{link_name}' has no household linkage column '{linked_hh_col}'."
                )
              )
              next
            }

            phrutils::phr_message(phrutils::phr_txt(
              "Aggregating data from linked dataset '{link_name}'..."
            ))

            # Aggregate based on dataset class
            # Check for specific subclasses first, then fall back to base classes
            # Store result and check for NULL before assigning to hh_data
            agg_result <- NULL
            if (inherits(linked_obj, "DeathIndividualData")) {
              agg_result <- private$..aggregate_deaths_data(
                hh_data,
                linked_data,
                hh_uuid_col,
                linked_hh_col,
                link_name,
                linked_obj
              )
            } else if (inherits(linked_obj, "WaterContainerData")) {
              agg_result <- private$..aggregate_water_container_data(
                hh_data,
                linked_data,
                hh_uuid_col,
                linked_hh_col,
                link_name,
                linked_obj
              )
            } else if (inherits(linked_obj, "NutritionIndividualData")) {
              agg_result <- private$..aggregate_nutrition_data(
                hh_data,
                linked_data,
                hh_uuid_col,
                linked_hh_col,
                link_name,
                linked_obj
              )
            } else if (inherits(linked_obj, "HealthIndividualData")) {
              agg_result <- private$..aggregate_health_data(
                hh_data,
                linked_data,
                hh_uuid_col,
                linked_hh_col,
                link_name,
                linked_obj
              )
            } else if (inherits(linked_obj, "WomenIndividualData")) {
              agg_result <- private$..aggregate_women_data(
                hh_data,
                linked_data,
                hh_uuid_col,
                linked_hh_col,
                link_name,
                linked_obj
              )
            } else if (inherits(linked_obj, "IndividualData")) {
              # This is primarily for base IndividualData (roster data)
              # Any remaining IndividualData subclasses without specialized handlers will also use this
              # Note: roster-style aggregation may not be appropriate for all subclass types
              agg_result <- private$..aggregate_roster_data(
                hh_data,
                linked_data,
                hh_uuid_col,
                linked_hh_col,
                link_name,
                linked_obj
              )
            } else {
              phrutils::phr_message(phrutils::phr_txt(
                "No specific aggregation defined for linked dataset '{link_name}' of class {class(linked_obj)[1]}."
              ))
              agg_result <- hh_data # No aggregation, keep existing data
            }

            # Only update hh_data if aggregation succeeded (didn't return NULL)
            if (!is.null(agg_result)) {
              hh_data <- agg_result
            } else {
              phrutils::phr_warning(
                private$..dataset_name,
                phrutils::phr_txt(
                  "Aggregation of linked dataset '{link_name}' failed. Skipping this dataset."
                )
              )
            }
          }

          return(hh_data)
        },
        on_error = "warn",
        origin = paste0(private$..dataset_name, "$..aggregate_linked_data")
      )

      # Ensure we never return NULL - if phr_try returned NULL on error, return original data
      if (is.null(result)) {
        phrutils::phr_warning(
          private$..dataset_name,
          phrutils::phr_txt(
            "Aggregation encountered an error. Returning original household data without aggregated columns."
          )
        )
        return(original_hh_data)
      }

      return(result)
    },

    #' Aggregate Deaths Data from Linked Dataset
    #'
    #' @description
    #' Internal helper that aggregates death and person_time data from linked DeathIndividualData
    #'
    #' @param hh_data Data frame of household data to augment
    #' @param deaths_data Data frame of deaths data from linked dataset
    #' @param hh_uuid_col Column name for household UUID in household data
    #' @param linked_hh_col Column name for household foreign key in deaths data
    #' @param link_name Character name of the link
    #' @param linked_obj The linked Data object
    #'
    #'
    #' @return Modified household data frame with aggregated death columns
    #'
    #' @details
    #' Sums canonical death and person_time columns by household: death, death_under5, death_male,
    #' death_female, death_non_trauma, death_trauma, death_other, death_current_location,
    #' death_migration, death_last_location, death_birth, person_time, person_time_under5,
    #' person_time_male, person_time_female
    ..aggregate_deaths_data = function(
      hh_data,
      deaths_data,
      hh_uuid_col,
      linked_hh_col,
      link_name,
      linked_obj
    ) {
      phrutils::phr_try(
        {
          # Use variable_map to identify canonical death and person_time columns
          # These are the standardized output column names from death data processing
          vm <- linked_obj$get(field = "..variable_map")

          # Build list of canonical column names
          # These are standardized output columns created by add_standardized_deaths and add_persontime
          # They use fixed names as they are outputs, not inputs that need mapping
          canonical_cols <- c(
            "death",
            "death_under5",
            "death_male",
            "death_female",
            "death_non_trauma",
            "death_trauma",
            "death_other",
            "death_current_location",
            "death_migration",
            "death_last_location",
            "death_birth",
            "person_time",
            "person_time_under5",
            "person_time_male",
            "person_time_female"
          )

          # Filter to columns that actually exist in deaths_data
          available_cols <- intersect(canonical_cols, names(deaths_data))

          if (length(available_cols) == 0) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "No canonical death or person_time columns found in linked dataset '{link_name}'."
              )
            )
            return(hh_data)
          }

          # Aggregate by household
          agg_data <- deaths_data |>
            dplyr::group_by(!!rlang::sym(linked_hh_col)) |>
            dplyr::summarise(
              dplyr::across(
                dplyr::all_of(available_cols),
                ~ sum(as.numeric(.), na.rm = TRUE)
              ),
              .groups = "drop"
            )

          # Aggregate calc_month_death into comma-separated list per household
          if ("calc_month_death" %in% names(deaths_data)) {
            death_months_agg <- deaths_data |>
              dplyr::group_by(!!rlang::sym(linked_hh_col)) |>
              dplyr::summarise(
                death_month = paste(
                  stats::na.omit(calc_month_death),
                  collapse = ", "
                ),
                .groups = "drop"
              )
            names(death_months_agg) <- c(linked_hh_col, "death_month")
            agg_data <- dplyr::left_join(
              agg_data,
              death_months_agg,
              by = linked_hh_col
            )
          }

          # Rename columns with prefix to show they come from linked dataset
          prefix <- paste0("linked_", link_name, "_")
          col_names <- names(agg_data)
          col_names[-1] <- paste0(prefix, col_names[-1])
          names(agg_data) <- c(hh_uuid_col, col_names[-1])

          # Check for existing columns and warn if overwriting (excluding join column)
          agg_cols_only <- setdiff(names(agg_data), hh_uuid_col)
          existing_cols <- intersect(agg_cols_only, names(hh_data))
          if (length(existing_cols) > 0) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "The following columns already exist and will NOT be overwritten: {paste(existing_cols, collapse=', ')}"
              )
            )
            # Remove existing columns from agg_data but keep the join column
            cols_to_keep <- c(
              hh_uuid_col,
              setdiff(names(agg_data), c(hh_uuid_col, existing_cols))
            )
            agg_data <- agg_data[, cols_to_keep, drop = FALSE]
          }

          # Left join aggregated data to household data
          hh_data <- dplyr::left_join(hh_data, agg_data, by = hh_uuid_col)

          # Replace NA values with 0 for all linked columns
          # Households with no linked records will have NA after left_join.
          # For count/sum aggregations, 0 is semantically correct (measured as zero).
          # For text aggregations (comma-separated lists), use empty string.
          linked_cols <- setdiff(names(agg_data), hh_uuid_col)
          text_cols <- intersect(
            linked_cols,
            grep("death_month$", linked_cols, value = TRUE)
          )
          numeric_cols <- setdiff(linked_cols, text_cols)
          if (length(numeric_cols) > 0) {
            hh_data <- hh_data |>
              dplyr::mutate(dplyr::across(
                dplyr::all_of(numeric_cols),
                ~ ifelse(is.na(.), 0, .)
              ))
          }
          if (length(text_cols) > 0) {
            hh_data <- hh_data |>
              dplyr::mutate(dplyr::across(
                dplyr::all_of(text_cols),
                ~ ifelse(is.na(.), "", .)
              ))
          }

          phrutils::phr_message(
            phrutils::phr_txt(
              "Added {ncol(agg_data) - 1} aggregated column(s) from deaths data"
            )
          )

          return(hh_data)
        },
        on_error = "warn",
        origin = paste0(private$..dataset_name, "$..aggregate_deaths_data")
      )
    },

    #' Aggregate Water Container Data from Linked Dataset
    #'
    #' @description
    #' Internal helper that aggregates water container data from linked WaterContainerData
    #'
    #' @param hh_data Data frame of household data to augment
    #' @param water_data Data frame of water container data from linked dataset
    #' @param hh_uuid_col Column name for household UUID in household data
    #' @param linked_hh_col Column name for household foreign key in water data
    #' @param link_name Character name of the link
    #' @param linked_obj The linked Data object
    #'
    #'
    #' @return Modified household data frame with aggregated water container columns
    #'
    #' @details
    #' Sums total liters by household and counts number of containers
    ..aggregate_water_container_data = function(
      hh_data,
      water_data,
      hh_uuid_col,
      linked_hh_col,
      link_name,
      linked_obj
    ) {
      phrutils::phr_try(
        {
          # Look for total liters column - try variable_map first, then fallback to known names
          water_col <- linked_obj$get(
            field = ".variable_map"
          )$container_capacity_liters %||%
            linked_obj$get(
              field = ".variable_map"
            )$wash_container_total_liters %||%
            intersect(
              names(water_data),
              c(
                "wash_container_total_litres",
                "wash_container_total_liters",
                "container_capacity_liters",
                "total_liters"
              )
            )[1]

          if (
            is.null(water_col) ||
              is.na(water_col) ||
              !water_col %in% names(water_data)
          ) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "No water container total liters column found in linked dataset '{link_name}'."
              )
            )
            return(hh_data)
          }

          # Aggregate by household
          agg_data <- water_data |>
            dplyr::group_by(!!rlang::sym(linked_hh_col)) |>
            dplyr::summarise(
              wash_container_total_liters = sum(
                as.numeric(.data[[water_col]]),
                na.rm = TRUE
              ),
              num_containers = dplyr::n(),
              .groups = "drop"
            )

          # Rename with prefix
          prefix <- paste0("linked_", link_name, "_")
          col_names <- names(agg_data)
          col_names[-1] <- paste0(prefix, col_names[-1])
          names(agg_data) <- c(hh_uuid_col, col_names[-1])

          # Check for existing columns (excluding the join column)
          agg_cols_only <- setdiff(names(agg_data), hh_uuid_col)
          existing_cols <- intersect(agg_cols_only, names(hh_data))
          if (length(existing_cols) > 0) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "The following columns already exist and will NOT be overwritten: {paste(existing_cols, collapse=', ')}"
              )
            )
            # Remove existing columns from agg_data but keep the join column
            cols_to_keep <- c(
              hh_uuid_col,
              setdiff(names(agg_data), c(hh_uuid_col, existing_cols))
            )
            agg_data <- agg_data[, cols_to_keep, drop = FALSE]
          }

          # Left join
          hh_data <- dplyr::left_join(hh_data, agg_data, by = hh_uuid_col)

          # Replace NA values with 0 for all linked columns
          # Households with no linked records will have NA after left_join.
          # For count/sum aggregations, 0 is semantically correct (measured as zero).
          linked_cols <- setdiff(names(agg_data), hh_uuid_col)
          if (length(linked_cols) > 0) {
            hh_data <- hh_data |>
              dplyr::mutate(dplyr::across(
                dplyr::all_of(linked_cols),
                ~ ifelse(is.na(.), 0, .)
              ))
          }

          phrutils::phr_message(
            phrutils::phr_txt(
              "Added aggregated water container data: wash_container_total_liters, num_containers"
            )
          )

          return(hh_data)
        },
        on_error = "warn",
        origin = paste0(private$..dataset_name, "$..aggregate_water_container_data")
      )
    },

    #' Aggregate Roster Data from Linked Dataset
    #'
    #' @description
    #' Internal helper that aggregates roster (individual) data from linked IndividualData
    #'
    #' @param hh_data Data frame of household data to augment
    #' @param roster_data Data frame of roster data from linked dataset
    #' @param hh_uuid_col Column name for household UUID in household data
    #' @param linked_hh_col Column name for household foreign key in roster data
    #' @param link_name Character name of the link
    #' @param linked_obj The linked Data object
    #'
    #'
    #' @return Modified household data frame with aggregated roster columns
    #'
    #' @details
    #' Calculates household demographics: household_size, num_children, num_male, num_female,
    #' num_women_15to49, and person_time columns by household. If calc_date_birth_final is
    #' available and recall/survey dates exist, also counts births in recall period.
    ..aggregate_roster_data = function(
      hh_data,
      roster_data,
      hh_uuid_col,
      linked_hh_col,
      link_name,
      linked_obj
    ) {
      phrutils::phr_try(
        {
          # Use canonical column names created by add_standardized_roster_demographics
          # These are the standardized output column names from roster data processing
          # They use fixed names as they are outputs, not inputs that need mapping
          canonical_cols <- c(
            "roster_child_under2",
            "roster_child_under5",
            "roster_2to5",
            "roster_5plus",
            "roster_5_10",
            "roster_male",
            "roster_female",
            "roster_woman_15to49",
            "roster_birth",
            "person_time",
            "person_time_under5",
            "person_time_male",
            "person_time_female",
            "roster_age_6_29m",
            "roster_age_30_59m"
          )

          # Compute roster_birth from date columns as a fallback when the
          # indicator pipeline (add_standardized_age) did not produce it.
          if (!"roster_birth" %in% names(roster_data)) {
            dob_col_for_fallback <- if (
              "calc_date_birth_final" %in% names(roster_data)
            ) {
              "calc_date_birth_final"
            } else if ("dob_exact" %in% names(roster_data)) {
              "dob_exact"
            } else if ("dob_approx" %in% names(roster_data)) {
              "dob_approx"
            } else {
              NULL
            }

            recall_col_for_fallback <- if (
              "recall_date" %in% names(roster_data)
            ) {
              "recall_date"
            } else {
              NULL
            }

            if (
              !is.null(dob_col_for_fallback) &&
                !is.null(recall_col_for_fallback)
            ) {
              dob_final_vals <- tryCatch(
                phrutils::phr_convert_date(roster_data[[dob_col_for_fallback]]),
                error = function(e) {
                  as.Date(rep(NA_character_, nrow(roster_data)))
                }
              )
              recall_dt_vals <- tryCatch(
                phrutils::phr_convert_date(roster_data[[
                  recall_col_for_fallback
                ]]),
                error = function(e) {
                  as.Date(rep(NA_character_, nrow(roster_data)))
                }
              )
              roster_data[["roster_birth"]] <- as.integer(
                !is.na(dob_final_vals) &
                  !is.na(recall_dt_vals) &
                  dob_final_vals >= recall_dt_vals
              )
            }
          }

          # Filter to columns that actually exist in roster_data
          available_cols <- intersect(canonical_cols, names(roster_data))

          if (length(available_cols) == 0) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "No canonical roster columns found in linked dataset '{link_name}'. Falling back to basic household_size calculation."
              )
            )
            # At minimum, provide household size
            agg_data <- roster_data |>
              dplyr::group_by(!!rlang::sym(linked_hh_col)) |>
              dplyr::summarise(
                roster_household_size = dplyr::n(),
                .groups = "drop"
              )
          } else {
            # Aggregate canonical columns by summing
            agg_data <- roster_data |>
              dplyr::group_by(!!rlang::sym(linked_hh_col)) |>
              dplyr::summarise(
                roster_household_size = dplyr::n(),
                dplyr::across(
                  dplyr::all_of(available_cols),
                  ~ sum(as.numeric(.), na.rm = TRUE)
                ),
                .groups = "drop"
              )
          }

          # Aggregate calc_month_birth into comma-separated list per household
          if ("calc_month_birth" %in% names(roster_data)) {
            birth_months_agg <- roster_data |>
              dplyr::group_by(!!rlang::sym(linked_hh_col)) |>
              dplyr::summarise(
                roster_birth_months = paste(
                  stats::na.omit(calc_month_birth),
                  collapse = ", "
                ),
                .groups = "drop"
              )
            names(birth_months_agg) <- c(linked_hh_col, "roster_birth_months")
            agg_data <- dplyr::left_join(
              agg_data,
              birth_months_agg,
              by = linked_hh_col
            )
          }

          # Rename columns with prefix to show they come from linked dataset
          prefix <- paste0("linked_")
          col_names <- names(agg_data)
          col_names[-1] <- paste0(prefix, col_names[-1])
          names(agg_data) <- c(hh_uuid_col, col_names[-1])

          # Check for existing columns and warn if overwriting (excluding join column)
          agg_cols_only <- setdiff(names(agg_data), hh_uuid_col)
          existing_cols <- intersect(agg_cols_only, names(hh_data))
          if (length(existing_cols) > 0) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "The following columns already exist and will NOT be overwritten: {paste(existing_cols, collapse=', ')}"
              )
            )
            # Remove existing columns from agg_data but keep the join column
            cols_to_keep <- c(
              hh_uuid_col,
              setdiff(names(agg_data), c(hh_uuid_col, existing_cols))
            )
            agg_data <- agg_data[, cols_to_keep, drop = FALSE]
          }

          # Left join aggregated data to household data
          hh_data <- dplyr::left_join(hh_data, agg_data, by = hh_uuid_col)

          # Replace NA values with 0 for all linked columns
          # Households with no linked records will have NA after left_join.
          # For count/sum aggregations, 0 is semantically correct (measured as zero).
          # For text aggregations (comma-separated lists), use empty string.
          linked_cols <- setdiff(names(agg_data), hh_uuid_col)
          text_cols <- intersect(
            linked_cols,
            grep("birth_months$", linked_cols, value = TRUE)
          )
          numeric_cols <- setdiff(linked_cols, text_cols)
          if (length(numeric_cols) > 0) {
            hh_data <- hh_data |>
              dplyr::mutate(dplyr::across(
                dplyr::all_of(numeric_cols),
                ~ ifelse(is.na(.), 0, .)
              ))
          }
          if (length(text_cols) > 0) {
            hh_data <- hh_data |>
              dplyr::mutate(dplyr::across(
                dplyr::all_of(text_cols),
                ~ ifelse(is.na(.), "", .)
              ))
          }

          phrutils::phr_message(
            phrutils::phr_txt(
              "Added {ncol(agg_data) - 1} aggregated column(s) from roster data"
            )
          )

          return(hh_data)
        },
        on_error = "warn",
        origin = paste0(private$..dataset_name, "$..aggregate_roster_data")
      )
    },

    #' Aggregate Nutrition Data from Linked Dataset
    #'
    #' @description
    #' Internal helper that aggregates nutrition data from linked NutritionIndividualData
    #'
    #' @param hh_data Data frame of household data to augment
    #' @param nutrition_data Data frame of nutrition data from linked dataset
    #' @param hh_uuid_col Column name for household UUID in household data
    #' @param linked_hh_col Column name for household foreign key in nutrition data
    #' @param link_name Character name of the link
    #' @param linked_obj The linked Data object
    #'
    #'
    #' @return Modified household data frame with aggregated nutrition columns
    #'
    #' @details
    #' Aggregates by age groups: children_under2, children_2to5, children_under5 based on age in months
    ..aggregate_nutrition_data = function(
      hh_data,
      nutrition_data,
      hh_uuid_col,
      linked_hh_col,
      link_name,
      linked_obj
    ) {
      phrutils::phr_try(
        {
          # Use canonical column names created by add_standardized_nutrition_demographics
          # These are the standardized output column names from nutrition data processing
          # They use fixed names as they are outputs, not inputs that need mapping
          canonical_cols <- c(
            "nutrition_child_under2",
            "nutrition_child_2to5",
            "nutrition_child_under5"
          )

          # Filter to columns that actually exist in nutrition_data
          available_cols <- intersect(canonical_cols, names(nutrition_data))

          if (length(available_cols) > 0) {
            # New harmonized approach: sum canonical columns
            agg_data <- nutrition_data |>
              dplyr::group_by(!!rlang::sym(linked_hh_col)) |>
              dplyr::summarise(
                dplyr::across(
                  dplyr::all_of(available_cols),
                  ~ sum(as.numeric(.), na.rm = TRUE)
                ),
                .groups = "drop"
              )
          } else {
            # Fallback to old approach for backward compatibility
            # Calculate child age groups based on calc_age_months from add_standardized_age
            # Look for calc_age_months first (output of add_standardized_age), then fallback
            age_months_col <- if (
              "calc_age_months" %in% names(nutrition_data)
            ) {
              "calc_age_months"
            } else {
              linked_obj$get(field = ".variable_map")$age_months %||%
                linked_obj$get(field = ".variable_map")$nutr_age_months %||%
                intersect(
                  names(nutrition_data),
                  c("age_months", "nutr_age_months")
                )[1]
            }

            if (
              is.null(age_months_col) ||
                is.na(age_months_col) ||
                !age_months_col %in% names(nutrition_data)
            ) {
              phrutils::phr_warning(
                private$..dataset_name,
                phrutils::phr_txt(
                  "No canonical nutrition columns or age in months column found in linked dataset '{link_name}'."
                )
              )
              return(hh_data)
            }

            # Aggregate by household using old calculation method
            agg_data <- nutrition_data |>
              dplyr::group_by(!!rlang::sym(linked_hh_col)) |>
              dplyr::summarise(
                children_under2 = sum(
                  as.numeric(.data[[age_months_col]]) < 24,
                  na.rm = TRUE
                ),
                children_2to5 = sum(
                  as.numeric(.data[[age_months_col]]) >= 24 &
                    as.numeric(.data[[age_months_col]]) < 60,
                  na.rm = TRUE
                ),
                children_under5 = sum(
                  as.numeric(.data[[age_months_col]]) < 60,
                  na.rm = TRUE
                ),
                .groups = "drop"
              )
          }

          # Rename columns with prefix
          prefix <- paste0("linked_nutrition_")
          col_names <- names(agg_data)
          col_names[-1] <- paste0(prefix, col_names[-1])
          names(agg_data) <- c(hh_uuid_col, col_names[-1])

          # Check for existing columns (excluding the join column)
          agg_cols_only <- setdiff(names(agg_data), hh_uuid_col)
          existing_cols <- intersect(agg_cols_only, names(hh_data))
          if (length(existing_cols) > 0) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "The following columns already exist and will NOT be overwritten: {paste(existing_cols, collapse=', ')}"
              )
            )
            # Remove existing columns from agg_data but keep the join column
            cols_to_keep <- c(
              hh_uuid_col,
              setdiff(names(agg_data), c(hh_uuid_col, existing_cols))
            )
            agg_data <- agg_data[, cols_to_keep, drop = FALSE]
          }

          # Left join
          hh_data <- dplyr::left_join(hh_data, agg_data, by = hh_uuid_col)

          # Replace NA values with 0 for all linked columns
          linked_cols <- setdiff(names(agg_data), hh_uuid_col)
          if (length(linked_cols) > 0) {
            hh_data <- hh_data |>
              dplyr::mutate(dplyr::across(
                dplyr::all_of(linked_cols),
                ~ ifelse(is.na(.), 0, .)
              ))
          }

          phrutils::phr_message(
            phrutils::phr_txt(
              "Added {ncol(agg_data) - 1} aggregated column(s) from nutrition data"
            )
          )

          return(hh_data)
        },
        on_error = "warn",
        origin = paste0(private$..dataset_name, "$..aggregate_nutrition_data")
      )
    },

    #' Aggregate Health Data from Linked Dataset
    #'
    #' @description
    #' Internal helper that aggregates health data from linked HealthIndividualData
    #'
    #' @param hh_data Data frame of household data to augment
    #' @param health_data Data frame of health data from linked dataset
    #' @param hh_uuid_col Column name for household UUID in household data
    #' @param linked_hh_col Column name for household foreign key in health data
    #' @param link_name Character name of the link
    #' @param linked_obj The linked Data object
    #'
    #'
    #' @return Modified household data frame with aggregated health columns
    #'
    #' @details
    #' Counts total number of people recorded (rows) per household
    ..aggregate_health_data = function(
      hh_data,
      health_data,
      hh_uuid_col,
      linked_hh_col,
      link_name,
      linked_obj
    ) {
      phrutils::phr_try(
        {
          # Aggregate by household - count number of people recorded
          agg_data <- health_data |>
            dplyr::group_by(!!rlang::sym(linked_hh_col)) |>
            dplyr::summarise(
              num_people_recorded = dplyr::n(),
              .groups = "drop"
            )

          # Rename columns with prefix
          prefix <- paste0("linked_", link_name, "_")
          col_names <- names(agg_data)
          col_names[-1] <- paste0(prefix, col_names[-1])
          names(agg_data) <- c(hh_uuid_col, col_names[-1])

          # Check for existing columns (excluding the join column)
          agg_cols_only <- setdiff(names(agg_data), hh_uuid_col)
          existing_cols <- intersect(agg_cols_only, names(hh_data))
          if (length(existing_cols) > 0) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "The following columns already exist and will NOT be overwritten: {paste(existing_cols, collapse=', ')}"
              )
            )
            # Remove existing columns from agg_data but keep the join column
            cols_to_keep <- c(
              hh_uuid_col,
              setdiff(names(agg_data), c(hh_uuid_col, existing_cols))
            )
            agg_data <- agg_data[, cols_to_keep, drop = FALSE]
          }

          # Left join
          hh_data <- dplyr::left_join(hh_data, agg_data, by = hh_uuid_col)

          # Replace NA values with 0 for all linked columns
          linked_cols <- setdiff(names(agg_data), hh_uuid_col)
          if (length(linked_cols) > 0) {
            hh_data <- hh_data |>
              dplyr::mutate(dplyr::across(
                dplyr::all_of(linked_cols),
                ~ ifelse(is.na(.), 0, .)
              ))
          }

          phrutils::phr_message(
            phrutils::phr_txt(
              "Added {ncol(agg_data) - 1} aggregated column(s) from health data"
            )
          )

          return(hh_data)
        },
        on_error = "warn",
        origin = paste0(private$..dataset_name, "$..aggregate_health_data")
      )
    },

    #' Aggregate Women Data from Linked Dataset
    #'
    #' @description
    #' Internal helper that aggregates women data from linked WomenIndividualData
    #'
    #' @param hh_data Data frame of household data to augment
    #' @param women_data Data frame of women data from linked dataset
    #' @param hh_uuid_col Column name for household UUID in household data
    #' @param linked_hh_col Column name for household foreign key in women data
    #' @param link_name Character name of the link
    #' @param linked_obj The linked Data object
    #'
    #'
    #' @return Modified household data frame with aggregated women columns
    #'
    #' @details
    #' Counts women aged 15-49 per household (all rows in WomenIndividualData represent eligible women)
    ..aggregate_women_data = function(
      hh_data,
      women_data,
      hh_uuid_col,
      linked_hh_col,
      link_name,
      linked_obj
    ) {
      phrutils::phr_try(
        {
          # For women data, all rows represent women aged 15-49
          # Simply count the number of women per household
          agg_data <- women_data |>
            dplyr::group_by(!!rlang::sym(linked_hh_col)) |>
            dplyr::summarise(
              women_15to49 = dplyr::n(),
              .groups = "drop"
            )

          # Rename columns with prefix
          prefix <- paste0("linked_", link_name, "_")
          col_names <- names(agg_data)
          col_names[-1] <- paste0(prefix, col_names[-1])
          names(agg_data) <- c(hh_uuid_col, col_names[-1])

          # Check for existing columns (excluding the join column)
          agg_cols_only <- setdiff(names(agg_data), hh_uuid_col)
          existing_cols <- intersect(agg_cols_only, names(hh_data))
          if (length(existing_cols) > 0) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "The following columns already exist and will NOT be overwritten: {paste(existing_cols, collapse=', ')}"
              )
            )
            # Remove existing columns from agg_data but keep the join column
            cols_to_keep <- c(
              hh_uuid_col,
              setdiff(names(agg_data), c(hh_uuid_col, existing_cols))
            )
            agg_data <- agg_data[, cols_to_keep, drop = FALSE]
          }

          # Left join
          hh_data <- dplyr::left_join(hh_data, agg_data, by = hh_uuid_col)

          # Replace NA values with 0 for all linked columns
          linked_cols <- setdiff(names(agg_data), hh_uuid_col)
          if (length(linked_cols) > 0) {
            hh_data <- hh_data |>
              dplyr::mutate(dplyr::across(
                dplyr::all_of(linked_cols),
                ~ ifelse(is.na(.), 0, .)
              ))
          }

          phrutils::phr_message(
            phrutils::phr_txt(
              "Added {ncol(agg_data) - 1} aggregated column(s) from women data"
            )
          )

          return(hh_data)
        },
        on_error = "warn",
        origin = paste0(private$..dataset_name, "$..aggregate_women_data")
      )
    },

    #' Merge Household Variables to Linked Datasets
    #'
    #' @description
    #' Internal helper that merges key household-level variables to linked datasets
    #'
    #' @param linked_obj The linked Data object to receive merged variables
    #' @param link_name Character name of the link
    #' @param hh_stage Character string specifying household data stage: "clean", "standardized", or "raw"
    #' @param linked_stage Character string specifying linked data stage: "clean", "standardized", or "raw"
    #'
    #' @return TRUE invisibly on success, NULL on failure
    #'
    #' @details
    #' Merges household variables to linked datasets: enum_id, device_id, date_survey,
    #' date_recall, weight, stratum, cluster_id, site, admin1, admin2, admin3, admin4.
    #'
    #' Merged columns are written to the linked dataset using their *canonical role
    #' names* (for example the household column `today` is merged as `date_survey`),
    #' and the role is registered in the linked object's `variable_map` so that
    #' indicator functions such as `add_persontime()` can resolve
    #' `@variable_map$date_survey` / `@variable_map$date_recall` afterwards.
    #' Any existing column of the same name in the linked dataset is overwritten.
    #'
    #' The merge is applied to every populated stage of the linked dataset (raw,
    #' standardized and clean) rather than a single fallback stage, because
    #' `standardize()` always rebuilds its output from `raw_data`; merging into
    #' `clean_data` or `standardized_data` alone would be discarded on the next
    #' standardization pass.
    #'
    #' The household linkage column of the linked dataset is resolved from its
    #' `variable_map` (`hh_uuid` / `household_uuid`), falling back to the
    #' `col_names` aliases declared in its variable schema and finally to the
    #' role name used as a literal column name. Only candidates that actually
    #' exist in the data are accepted, so an unmapped or stale `variable_map`
    #' cannot make the merge target a non-existent column.
    #'
    #' @keywords internal
    ..merge_household_vars_to_linked = function(
      linked_obj,
      link_name,
      hh_stage = c("clean", "standardized", "raw"),
      linked_stage = c("clean", "standardized", "raw")
    ) {
      hh_stage <- match.arg(hh_stage)
      linked_stage <- match.arg(linked_stage)

      phrutils::phr_try(
        {
          if (!inherits(linked_obj, "Data")) {
            return(invisible(NULL))
          }

          # Get linked data using fallback logic
          linked_data_result <- self$get_data_with_fallback(
            linked_obj,
            linked_stage
          )
          if (is.null(linked_data_result)) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "Linked dataset '{link_name}' has no data at any stage to merge variables to."
              )
            )
            return(invisible(NULL))
          }

          linked_data <- linked_data_result$data
          linked_data_stage <- linked_data_result$stage
          phrutils::phr_message(phrutils::phr_txt(
            "Using linked dataset '{link_name}' data from stage: {linked_data_stage}"
          ))

          # Get household data using fallback logic
          hh_data_result <- self$get_data_with_fallback(self, hh_stage)
          if (is.null(hh_data_result)) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "Household data not available at any stage; cannot merge variables to linked dataset '{link_name}'."
              )
            )
            return(invisible(NULL))
          }

          hh_data <- hh_data_result$data
          hh_data_stage <- hh_data_result$stage
          phrutils::phr_message(phrutils::phr_txt(
            "Using household data from stage: {hh_data_stage}"
          ))

          # Get household UUID column
          hh_uuid_col <- private$..uuid

          if (!hh_uuid_col %in% names(hh_data)) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "Household UUID column '{hh_uuid_col}' not found. Cannot merge variables to linked dataset '{link_name}'."
              )
            )
            return(invisible(NULL))
          }

          # Determine the household linkage column in the linked dataset.
          # The linked object's variable_map may not have been populated yet
          # (its schema is loaded after Data$initialize() has already mapped),
          # so fall back to the schema's col_names aliases rather than assuming
          # a literal "hh_uuid" column.
          linkage <- .resolve_linked_hh_column(
            linked_obj = linked_obj,
            data_cols = names(linked_data)
          )
          linked_hh_col <- linkage$column

          if (is.null(linked_hh_col)) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "Linked dataset '{link_name}' has no household linkage column. Tried: {paste(linkage$candidates, collapse=', ')}."
              )
            )
            return(invisible(NULL))
          }

          # Keep the linked object's variable_map in sync with the column that
          # was actually used, so downstream lookups resolve the same column.
          linked_obj$set(
            field = "..variable_map",
            role = linkage$role,
            value = linked_hh_col
          )

          # Define the list of household variables to merge
          # These are the canonical variable names in the variable_map
          hh_vars_to_merge <- c(
            "enum_id",
            "device_id",
            "date_survey",
            "date_recall",
            "weight",
            "stratum",
            "cluster_id",
            "site",
            "admin1",
            "admin2",
            "admin3",
            "admin4"
          )

          # Identify which variables are available in household data
          vars_to_merge <- list()
          for (var_name in hh_vars_to_merge) {
            # Get the column name from variable_map
            col_name <- private$..variable_map[[var_name]]

            # Check if it's mapped and exists in household data
            if (
              !is.null(col_name) &&
                col_name != "" &&
                !is.na(col_name) &&
                col_name %in% names(hh_data)
            ) {
              vars_to_merge[[var_name]] <- col_name
            }
          }

          if (length(vars_to_merge) == 0) {
            phrutils::phr_message(phrutils::phr_txt(
              "No household variables found to merge to linked dataset '{link_name}'."
            ))
            return(invisible(NULL))
          }

          phrutils::phr_message(phrutils::phr_txt(
            "Merging {length(vars_to_merge)} household variable(s) to linked dataset '{link_name}'..."
          ))

          # Build the merge table keyed on the household UUID. Columns are named
          # after the canonical role (not the household column name) so that the
          # linked dataset receives canonical columns that its own schema and
          # variable_map can resolve.
          hh_merge_data <- hh_data[, hh_uuid_col, drop = FALSE]
          for (var_name in names(vars_to_merge)) {
            hh_merge_data[[var_name]] <- hh_data[[vars_to_merge[[var_name]]]]
          }

          # Merge into every populated stage of the linked dataset. standardize()
          # always rebuilds from raw_data, so writing to a single fallback stage
          # would be silently discarded on the next standardization pass.
          merge_stages <- c("raw", "standardized", "clean")
          merged_stages <- character(0)

          for (stage_name in merge_stages) {
            stage_field <- paste0("..", stage_name, "_data")
            stage_data <- linked_obj$get(field = stage_field)

            if (
              is.null(stage_data) ||
                !is.data.frame(stage_data) ||
                nrow(stage_data) == 0
            ) {
              next
            }

            # Stages can carry different column names (standardization may
            # rename the linkage column), so resolve it per stage.
            stage_hh_col <- linked_hh_col
            if (!stage_hh_col %in% names(stage_data)) {
              stage_hh_col <- .resolve_linked_hh_column(
                linked_obj = linked_obj,
                data_cols = names(stage_data)
              )$column
            }

            if (is.null(stage_hh_col)) {
              phrutils::phr_warning(
                private$..dataset_name,
                phrutils::phr_txt(
                  "Linked dataset '{link_name}' {stage_name} data has no household linkage column. Skipping that stage."
                )
              )
              next
            }

            # Drop any columns that would collide with the merged canonical
            # columns before joining (household values take precedence)
            existing_vars <- intersect(names(vars_to_merge), names(stage_data))
            if (length(existing_vars) > 0) {
              phrutils::phr_message(
                phrutils::phr_txt(
                  "The following variables already exist in linked dataset '{link_name}' ({stage_name}) and will be overwritten: {paste(existing_vars, collapse=', ')}"
                )
              )
              stage_data <- stage_data[,
                !(names(stage_data) %in% existing_vars),
                drop = FALSE
              ]
            }

            # Perform the merge (left join from the linked data perspective)
            stage_data_merged <- dplyr::left_join(
              stage_data,
              hh_merge_data,
              by = stats::setNames(hh_uuid_col, stage_hh_col)
            )

            linked_obj$set(field = stage_field, value = stage_data_merged)
            merged_stages <- c(merged_stages, stage_name)
          }

          if (length(merged_stages) == 0) {
            phrutils::phr_warning(
              private$..dataset_name,
              phrutils::phr_txt(
                "No stage of linked dataset '{link_name}' could receive the merged household variables."
              )
            )
            return(invisible(NULL))
          }

          # Register the merged roles in the linked object's variable_map so that
          # downstream indicators (e.g. add_persontime) can resolve
          # @variable_map$date_survey / @variable_map$date_recall. Without this,
          # roles absent from the linked dataset's own schema stay unmapped.
          for (var_name in names(vars_to_merge)) {
            linked_obj$set(
              field = "..variable_map",
              role = var_name,
              value = var_name
            )
          }

          phrutils::phr_message(
            phrutils::phr_txt(
              "Successfully merged {length(vars_to_merge)} household variable(s) to linked dataset '{link_name}' (stages: {paste(merged_stages, collapse=', ')})."
            )
          )

          invisible(TRUE)
        },
        on_error = "warn",
        origin = paste0(self$dataset_name, "$..merge_household_vars_to_linked")
      )
    },

    # @field ..data_registry List of data object types and associated types
    # that are allowable for add_linked_object
    # @keywords internal
    ..data_registry = function() {
      list(
        general = "Data",
        roster = "IndividualData",
        deaths = "DeathIndividualData",
        water_containers = "WaterContainerData",
        women = "WomenIndividualData",
        nutrition = "NutritionIndividualData",
        health = "HealthIndividualData"
      )
    },

    # @field ..analytics_registry List of analytics objects and types that can be
    # created with generate_data_analytics
    # @keywords internal
    ..analytics_registry = function() {

      registry <- super$..analytics_registry()

      c(
        registry,
        list(
          fsl = function(data,
                         stage,
                         analysis_config,
                         parent) {

            FSLDataAnalytics$new(
              data = data,
              dap = analysis_config,
              parent_data_object = parent,
              dataset_name = paste0(private$..dataset_name, "_FSLDataAnalytics"),
              data_stage_name = stage,
              data_hash = parent$get(field = "..metadata", role = "hash_id"),
              variable_map = parent$get(field = "..variable_map"),
              value_map = parent$get(field = "..value_map"),
              variable_label = parent$get(field = "..variable_label"),
              value_label = parent$get(field = "..value_label")
            )
          },


          wash = function(data,
                         stage,
                         analysis_config,
                         parent) {

            water_containers <- parent$linked_objects$water_containers$object
            water_containers_data <- water_containers$get_data_with_fallback(self, stage)
            varm <- water_containers$get(field = "..variable_map")
            valm <- water_containers$get(field = "..value_map")
            varlab <- water_containers$get(field = "..variable_label")
            vallab <- water_containers$get(field = "..value_label")

            WASHDataAnalytics$new(
                data = data,
                dap = analysis_config,
                parent_data_object = parent,
                dataset_name = paste0(private$..dataset_name, "_WASHDataAnalytics"),
                data_stage_name = stage,
                data_hash = parent$get(field = "..metadata", role = "hash_id"),
                variable_map = parent$get(field = "..variable_map"),
                value_map = parent$get(field = "..value_map"),
                variable_label = parent$get(field = "..variable_label"),
                value_label = parent$get(field = "..value_label"),
                linked_containers_data = if (!is.null(water_containers_data$data)) {
                  water_containers_data$data
                } else {
                  NULL
                },
                linked_containers_data_stage_name = if (
                  !is.null(water_containers_data$stage_name)
                ) {
                  water_containers_data$stage_name
                } else {
                  NULL
                },
                linked_containers_data_hash = if (
                  !is.null(water_containers$get(field = "..metadata", role = "hash_id"))
                ) {
                  water_containers$get(field = "..metadata", role = "hash_id")
                } else {
                  NULL
                },
                linked_containers_variable_map = if (
                  !is.null(varm)
                ) {
                  varm
                } else {
                  NULL
                },
                linked_containers_value_map = if (
                  !is.null(valm)
                ) {
                  valm
                } else {
                  NULL
                },
                linked_containers_variable_label = if (
                  !is.null(varlab)
                ) {
                  varlab
                } else {
                  NULL
                },
                linked_containers_value_label = if (
                  !is.null(vallab)
                ) {
                  vallab
                } else {
                  NULL
                }
              )

          },

          health = function(data,
                            stage,
                            analysis_config,
                            parent) {

            roster <- parent$linked_objects$roster$object
            roster_data <- roster$get_data_with_fallback(self, stage)

            health_ind <- parent$linked_objects$health$object
            health_ind_data <- water_containers$get_data_with_fallback(self, stage)

              HealthDataAnalytics$new(
                data = data,
                dap = analysis_config,
                parent_data_object = parent,
                dataset_name = paste0(private$..dataset_name, "_HealthDataAnalytics"),
                data_stage_name = stage,
                data_hash = parent$get(field = "..metadata", role = "hash_id"),
                variable_map = parent$get(field = "..variable_map"),
                value_map = parent$get(field = "..value_map"),
                variable_label = parent$get(field = "..variable_label"),
                value_label = parent$get(field = "..value_label"),
                linked_ind_roster_data = if (!is.null(roster_data$data)) {
                  roster_data$data
                } else {
                  NULL
                },
                linked_ind_roster_data_stage_name = if (
                  !is.null(roster_data$stage_name)
                ) {
                  roster_data$stage_name
                } else {
                  NULL
                },
                linked_ind_roster_data_hash = if (!is.null(roster$get(field = "..metadata", role = "hash_id"))) {
                  roster$get(field = "..metadata", role = "hash_id")
                } else {
                  NULL
                },
                linked_ind_roster_variable_map = if (
                  !is.null(roster$get(field = "..variable_map"))
                ) {
                  roster$get(field = "..variable_map")
                } else {
                  NULL
                },
                linked_ind_roster_value_map = if (!is.null(roster$get(field = "..value_map"))) {
                  roster$get(field = "..value_map")
                } else {
                  NULL
                },
                linked_ind_health_data = if (!is.null(health_ind_data$data)) {
                  health_ind_data$data
                } else {
                  NULL
                },
                linked_ind_health_data_stage_name = if (
                  !is.null(health_ind_data$stage_name)
                ) {
                  health_ind_data$stage_name
                } else {
                  NULL
                },
                linked_ind_health_data_hash = if (!is.null(health_ind$get(field = "..metadata", role = "hash_id"))) {
                  health_ind$get(field = "..metadata", role = "hash_id")
                } else {
                  NULL
                },
                linked_ind_health_variable_map = if (
                  !is.null(health_ind$get(field = "..variable_map"))
                ) {
                  health_ind$get(field = "..variable_map")
                } else {
                  NULL
                },
                linked_ind_health_value_map = if (!is.null(health_ind$get(field = "..value_map"))) {
                  health_ind$get(field = "..value_map")
                } else {
                  NULL
                }
              )
          },

          mortality = function(data,
                               stage,
                               analysis_config,
                               parent) {

            roster <- parent$linked_objects$roster$object
            roster_data <- roster$get_data_with_fallback(self, stage)

            deaths <- parent$linked_objects$deaths$object
            deaths_data <- deaths$get_data_with_fallback(self, stage)

              MortalityDataAnalytics$new(
                data = data,
                dap = analysis_config,
                parent_data_object = parent,
                dataset_name = paste0(
                  private$..dataset_name,
                  "_MortalityDataAnalytics"
                ),
                data_stage_name = stage,
                data_hash = parent$get(field = "..metadata", role = "hash_id"),
                variable_map = parent$get(field = "..variable_map"),
                value_map = parent$get(field = "..value_map"),
                variable_label = parent$get(field = "..variable_label"),
                value_label = parent$get(field = "..value_label"),
                linked_ind_roster_data = if (!is.null(roster_data$data)) {
                  roster_data$data
                } else {
                  NULL
                },
                linked_ind_roster_data_stage_name = if (
                  !is.null(roster_data$stage_name)
                ) {
                  roster_data$stage_name
                } else {
                  NULL
                },
                linked_ind_roster_data_hash = if (!is.null(roster$get(field = "..metadata", role = "hash_id"))) {
                  roster$get(field = "..metadata", role = "hash_id")
                } else {
                  NULL
                },
                linked_ind_roster_variable_map = if (
                  !is.null(roster$get(field = "..variable_map"))
                ) {
                  roster$get(field = "..variable_map")
                } else {
                  NULL
                },
                linked_ind_roster_value_map = if (!is.null(roster$get(field = "..value_map"))) {
                  roster$get(field = "..value_map")
                } else {
                  NULL
                },
                linked_ind_roster_variable_label = if (
                  !is.null(roster$get(field = "..variable_label"))
                ) {
                  roster$get(field = "..variable_label")
                } else {
                  NULL
                },
                linked_ind_roster_value_label = if (
                  !is.null(roster$get(field = "value_label"))
                ) {
                  roster$get(field = "..value_label")
                } else {
                  NULL
                },
                linked_ind_deaths_data = if (!is.null(deaths_data$data)) {
                  deaths_data$data
                } else {
                  NULL
                },
                linked_ind_deaths_data_stage_name = if (
                  !is.null(deaths_data$stage_name)
                ) {
                  deaths_data$stage_name
                } else {
                  NULL
                },
                linked_ind_deaths_data_hash = if (!is.null(deaths$get(field = "..metadata", role = "hash_id"))) {
                  deaths$get(field = "..metadata", role = "hash_id")
                } else {
                  NULL
                },
                linked_ind_deaths_variable_map = if (
                  !is.null(deaths$get(field = "..variable_map"))
                ) {
                  deaths$get(field = "..variable_map")
                } else {
                  NULL
                },
                linked_ind_deaths_value_map = if (!is.null(deaths$get(field = "..value_map"))) {
                  deaths$get(field = "..value_map")
                } else {
                  NULL
                },
                linked_ind_deaths_variable_label = if (
                  !is.null(deaths$get(field = "..variable_label"))
                ) {
                  deaths$get(field = "..variable_label")
                } else {
                  NULL
                },
                linked_ind_deaths_value_label = if (
                  !is.null(deaths$get(field = "..value_label"))
                ) {
                  deaths$get(field = "..value_label")
                } else {
                  NULL
                }
              )

          }
        )


      )
    }
  )
)
