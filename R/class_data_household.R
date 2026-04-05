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
#' 1. Check linked datasets and ensure they are standardized
#' 2. Merge household-level variables to all linked datasets (if available):
#'    `enum_id`, `device_id`, `date_survey`, `weight`, `stratum`, `cluster_id`,
#'    `site`, `admin1`, `admin2`, `admin3`, `admin4`. Existing variables in
#'    linked datasets are overwritten with household values.
#' 3. Aggregate data from linked datasets and add columns to household data with
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
#' @field survey_design Survey design object (srvyr) for weighted analyses (created at clean stage)
#' @field optional_columns Character vector of optional columns commonly present in household data
#'
#' @seealso [Data], [IndividualData], [DeathIndividualData], [WaterContainerData],
#'   [NutritionIndividualData], [HealthIndividualData]
#' @export
HouseholdData <- R6::R6Class(
  classname = "HouseholdData",
  inherit = Data,

  public = list(


    # Additional fields

    survey_design    = NULL,  # store srvyr design object (clean stage)
    # linked_datasets removed - now using inherited linked_objects from Data class
    optional_columns = NULL,  # non-required but commonly present columns


    #' Initialize HouseholdData
    #'
    #' @description
    #' Creates a new HouseholdData object with household-level survey data
    #'
    #' @param data Data frame containing household-level survey data
    #' @param dataset_name Character name for the dataset (default: "HouseholdData")
    #' @param metadata Optional list of metadata attributes
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
    initialize = function(data,
                          dataset_name = "HouseholdData",
                          metadata = NULL,
                          variable_map = NULL) {

      phr_try({


        # Default variable map for common household fields (roles -> columns)
        default_map <- list(
          uuid           = "uuid"           # inherited identifier
        )


        # Merge user-specified map over defaults
        variable_map <- modifyList(default_map, variable_map %||% list())

        # Call parent initializer first
        super$initialize(
          data        = data,
          dataset_name = dataset_name,
          metadata    = metadata,
          uuid        = variable_map$uuid,
          variable_map = variable_map
        )

        # 2) Build and merge household schema into any existing schema
        hh_schema     <- self$default_schema()
        parent_schema <- self$variable_schema %||% list()
        merged_schema <- utils::modifyList(parent_schema, hh_schema)
        self$set_variable_schema(merged_schema)

        # 3) Load default indicator schema
        default_ind_schema <- self$default_indicator_schema()
        if (length(default_ind_schema) > 0) {
          self$set_indicator_schema(default_ind_schema)
          phr_message(
            phr_txt(glue::glue("Loaded default indicator schema with {length(default_ind_schema)} indicator(s)."))
          )
        }


        # 4) Load default dependency schema
        default_dep_schema <- self$default_dependency_schema()
        if (length(default_dep_schema$dependencies) > 0) {
          self$set_dependency_schema(default_dep_schema)
          phr_message(
            phr_txt("Loaded default dependency schema with {length(default_dep_schema$dependencies)} dependency/ies.")
          )
        }

        # 5) Merge household-required columns into Data$required_columns
        #    (ensure we store *column names*, not roles)

        hh_required <- c(
          self$variable_map$uuid,
          self$variable_map$consent,
          self$variable_map$date_survey,
          self$variable_map$enum_id
        )
        hh_required <- unique(hh_required[!is.na(hh_required) & hh_required != ""])

        # Data$initialize already set self$required_columns <- uuid
        # We extend that with the household-specific requirements
        self$required_columns <- unique(c(self$required_columns, hh_required))

        # Optional (mapped) columns
        self$optional_columns <- c(
          self$variable_map$cluster_id,
          self$variable_map$stratum,
          self$variable_map$weight,
          self$variable_map$admin1,
          self$variable_map$admin2,
          self$variable_map$gps_lat,
          self$variable_map$gps_lon
        )

        phr_message(phr_txt("{dataset_name} initialized as HouseholdData object."))

      }, on_error = "abort", origin = "HouseholdData$initialize")
    },


    #' Load Default Variable Schema
    #'
    #' @description
    #' Loads the default variable schema from the household variable schema template Excel file
    #'
    #' @return Named list with variable schema definitions (types, allowed values, etc.)
    #'
    #' @details
    #' Reads variable_schema_data_household_template.xlsx from package resources and converts
    #' to canonical nested schema structure
    default_schema = function() {

      file <- system.file(
        "resources",
        "variable_schema_data_household_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file)) {
        phr_error(
          origin  = "HouseholdData$default_schema",
          message = "variable_schema_data_household_template.xlsx not found in package resources.",
          hint    = "Place the schema file under inst/resources/ before building the package."
        )
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_error(
            origin  = "HouseholdData$default_schema",
            message = "Failed to read variable_schema_data_household_template.xlsx",
            hint    = e$message
          )
        }
      )

      # Convert table → canonical nested schema list
      schema <- data_table_to_schema(df)

      return(schema)
    },

    #' Load Default Indicator Schema
    #'
    #' @description
    #' Loads the default indicator schema from the household indicator schema template Excel file
    #'
    #' @return Named list with indicator schema definitions, or empty list if template not found
    #'
    #' @details
    #' Reads indicator_schema_data_household_template.xlsx from package resources and converts
    #' to canonical nested indicator schema structure. Issues warning if file not found.
    default_indicator_schema = function() {

      file <- system.file(
        "resources",
        "indicator_schema_data_household_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file)) {
        phr_warning(
          origin  = "HouseholdData$default_indicator_schema",
          message = "indicator_schema_data_household_template.xlsx not found in package resources. Continuing without default indicator schema."
        )
        return(list())
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_warning(
            origin  = "HouseholdData$default_indicator_schema",
            message = phr_txt(glue::glue("Failed to read indicator_schema_data_household_template.xlsx: {e$message}"))
          )
          return(NULL)
        }
      )

      if (is.null(df)) return(list())

      # Convert table → canonical nested indicator schema list
      indicator_schema <- indicator_table_to_schema(df)

      return(indicator_schema)
    },

    #' Load Default Dependency Schema
    #'
    #' @description
    #' Loads the default dependency schema from the household dependency schema template Excel file
    #'
    #' @return List with 'dependencies' element containing dependency definitions, or empty dependencies list if template not found
    #'
    #' @details
    #' Reads dependency_schema_data_household_template.xlsx from package resources and converts
    #' to canonical nested dependency schema structure. Issues warning if file not found.
    default_dependency_schema = function() {

      file <- system.file(
        "resources",
        "dependency_schema_data_household_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file)) {
        phr_warning(
          origin  = "HouseholdData$default_dependency_schema",
          message = "dependency_schema_data_household_template.xlsx not found in package resources. Continuing without default dependency schema."
        )
        return(list(dependencies = list()))
      }

      # Read the Excel table (first sheet)
      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_warning(
            origin  = "HouseholdData$default_dependency_schema",
            message = phr_txt("Failed to read dependency_schema_data_household_template.xlsx: {e$message}")
          )
          return(NULL)
        }
      )

      if (is.null(df)) return(list(dependencies = list()))

      # Convert table → canonical nested dependency schema list
      dependency_schema <- dependency_table_to_schema(df)

      return(dependency_schema)
    },


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
    post_validate = function(df) {

      nm <- self$dataset_name

      # Track whether any issues occurred
      had_issues <- FALSE


      # GPS validity check

      lat_col <- self$variable_map$gps_lat
      lon_col <- self$variable_map$gps_lon

      if (!is.null(lat_col) && lat_col %in% names(df) &&
          !is.null(lon_col) && lon_col %in% names(df)) {

        lat_invalid <- which(!is.na(df[[lat_col]]) &
                               (df[[lat_col]] < -90 | df[[lat_col]] > 90))
        lon_invalid <- which(!is.na(df[[lon_col]]) &
                               (df[[lon_col]] < -180 | df[[lon_col]] > 180))


        if (length(lat_invalid) > 0 || length(lon_invalid) > 0) {
          phr_warning(
            nm,
            phr_txt("Some GPS coordinates appear invalid (latitudes outside [-90,90] or longitudes outside [-180,180]).")
          )
          had_issues <- TRUE
        }
      }


      # Weight checks

      if (!is.null(self$variable_map$weight) &&
          self$variable_map$weight %in% names(df)) {

        wts <- suppressWarnings(as.numeric(df[[self$variable_map$weight]]))

        if (any(is.na(wts))) {
          phr_warning(nm, phr_txt("Missing values found in weight variable."))
          had_issues <- TRUE
        }

        if (any(wts < 0, na.rm = TRUE)) {
          phr_warning(nm, phr_txt("Negative weights detected."))
          had_issues <- TRUE
        }
      }


      # Strata checks

      if (!is.null(self$variable_map$strata) &&
          self$variable_map$strata %in% names(df)) {

        strata_vals <- df[[self$variable_map$strata]]

        if (any(is.na(strata_vals))) {
          phr_warning(nm, phr_txt("Missing strata values detected."))
          had_issues <- TRUE
        }
      }


      # Link placeholders

      if (!is.null(self$roster_link)) {
        phr_message(phr_txt("Validating linked roster data (placeholder)."))
      }

      if (!is.null(self$deaths_link)) {
        phr_message(phr_txt("Validating linked deaths data (placeholder)."))
      }

      phr_message(phr_txt(glue::glue("Post-validation for {nm} complete.")))

      # Return TRUE (good) or FALSE (warnings occurred)
      return(!had_issues)
    },


    #' Add Linked Dataset
    #'
    #' @description
    #' Adds a Data object to the linked_objects list with validation of link name and object type.
    #' This method overrides the base Data class method to provide household-specific validation.
    #'
    #' @param name Character name for the link. Must be one of: roster, deaths, water_containers, women, nutrition, health
    #' @param data_object Data class or subclass object to link. Must match expected type for the link name.
    #' @param by_self_role Not used - automatically set to "uuid". Parameter included for compatibility with base class signature.
    #' @param by_other_role Not used - automatically set to "hh_uuid". Parameter included for compatibility with base class signature.
    #'
    #' @return TRUE invisibly on success
    #'
    #' @details
    #' Allowed link names and expected types:
    #' * roster: IndividualData
    #' * deaths: DeathIndividualData
    #' * water_containers: WaterContainerData
    #' * women: WomenIndividualData
    #' * nutrition: NutritionIndividualData
    #' * health: HealthIndividualData
    #'
    #' Links are stored with foreign key relationship information (by_self_role="uuid", by_other_role="hh_uuid").
    #' For HouseholdData, these roles are always fixed. If you need custom roles, use a base Data object instead.
    add_linked_dataset = function(name, data_object, by_self_role = NULL, by_other_role = NULL) {

      phr_try({

        # Warn if user provided role parameters (they will be ignored)
        if (!is.null(by_self_role) || !is.null(by_other_role)) {
          phr_warning(
            self$dataset_name,
            phr_txt("by_self_role and by_other_role are not used for HouseholdData. They are automatically set to 'uuid' and 'hh_uuid' respectively.")
          )
        }

        # Define allowed link names and their corresponding expected Data subclasses
        allowed_links <- list(
          roster = "IndividualData",
          deaths = "DeathIndividualData",
          water_containers = "WaterContainerData",
          women = "WomenIndividualData",
          nutrition = "NutritionIndividualData",
          health = "HealthIndividualData"
        )

        # Validate inputs
        if (missing(name) || !is.character(name) || length(name) != 1) {
          phr_error(
            self$dataset_name,
            phr_txt("Link name must be a single character string.")
          )
        }

        # Check if name is in the allowed list
        if (!name %in% names(allowed_links)) {
          phr_error(
            self$dataset_name,
            phr_txt("Link name '{name}' is not allowed. Allowed names are: {paste(names(allowed_links), collapse=', ')}.")
          )
        }

        if (!inherits(data_object, "Data")) {
          phr_error(
            self$dataset_name,
            phr_txt("Linked object must be a Data class or subclass object.")
          )
        }

        # Check if the data object is of the correct type for this link name
        expected_class <- allowed_links[[name]]
        if (!inherits(data_object, expected_class)) {
          phr_error(
            self$dataset_name,
            phr_txt("Link name '{name}' expects a {expected_class} object, but received {class(data_object)[1]}.")
          )
        }

        # Add to linked_objects (inherited from Data class)
        # Store with optional by_self_role and by_other_role for foreign key relationships
        # by_self_role: role name in household's variable_map (maps to household's UUID column)
        # by_other_role: role name in linked dataset's variable_map (maps to hh_uuid foreign key column)
        self$linked_objects[[name]] <- list(
          object = data_object,
          by_self_role = "uuid",  # household UUID role
          by_other_role = "hh_uuid"  # household UUID foreign key role in linked dataset
        )

        phr_message(
          phr_txt("Added linked dataset '{name}' ({class(data_object)[1]}) to {self$dataset_name}.")
        )

        invisible(TRUE)

      }, on_error = "abort", origin = paste0(self$dataset_name, "$add_linked_dataset"))
    },


    #' Get Data with Fallback Logic
    #'
    #' @description
    #' Internal helper to retrieve data from a Data object with automatic fallback to available stages
    #'
    #' @param data_obj Data class object to retrieve data from
    #' @param stage Character string specifying preferred stage: "clean", "standardized", or "raw"
    #'
    #' @return List with 'data' (data frame) and 'stage' (character) elements, or NULL if no data found
    #'
    #' @details
    #' Priority order depends on requested stage:
    #' * If "clean": clean -> standardized -> raw
    #' * If "standardized": standardized -> clean -> raw
    #' * If "raw": raw -> clean -> standardized
    #'
    #' @keywords internal
    ..get_data_with_fallback = function(data_obj, stage = c("clean", "standardized", "raw")) {
      stage <- match.arg(stage)

      phr_try({
        # Define priority order based on requested stage
        if (stage == "clean") {
          priority_order <- c("clean", "standardized", "raw")
        } else if (stage == "standardized") {
          priority_order <- c("standardized", "clean", "raw")
        } else {
          priority_order <- c("raw", "clean", "standardized")
        }

        # Try each stage in priority order
        for (stage_name in priority_order) {
          data <- switch(stage_name,
            "clean" = data_obj$clean_data,
            "standardized" = data_obj$standardized_data,
            "raw" = data_obj$raw_data,
            NULL
          )

          if (!is.null(data) && is.data.frame(data) && nrow(data) > 0) {
            return(list(data = data, stage = stage_name))
          }
        }

        # If no data found, return NULL
        return(NULL)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$..get_data_with_fallback"))
    },


    #' Pre-Standardize Hook for Household Data
    #'
    #' @description
    #' Hook method called before standardization begins. Checks linked datasets and aggregates data from them.
    #'
    #' @param stage Character string specifying source stage: "clean", "standardized", or "raw"
    #'
    #' @return NULL (modifies household data in place by adding aggregated columns)
    #'
    #' @details
    #' This hook performs three key operations:
    #' 1. Validates that linked datasets are standardized
    #' 2. Merges household-level variables to linked datasets (enum_id, device_id, date_survey, weight, stratum, cluster_id, etc.)
    #' 3. Aggregates data from linked datasets back to household level with prefix "linked_<name>_<column>"
    #'
    #' Supported aggregations vary by dataset type (see class documentation for details)
    pre_standardize = function(stage = c("clean", "standardized", "raw")) {
      stage <- match.arg(stage)

      phr_try({

        # Check if any linked objects exist (using inherited linked_objects from Data class)
        if (length(self$linked_objects) == 0) {
          phr_message(phr_txt("No linked datasets found in {self$dataset_name}."))
          return(invisible(NULL))
        }

        phr_message(phr_txt("Processing {length(self$linked_objects)} linked dataset(s) for {self$dataset_name}..."))

        # Iterate through linked objects
        for (link_name in names(self$linked_objects)) {
          link_info <- self$linked_objects[[link_name]]
          linked_obj <- link_info$object

          # Validate that the linked object is a Data class or subclass
          if (!inherits(linked_obj, "Data")) {
            phr_warning(
              self$dataset_name,
              phr_txt("Linked object '{link_name}' is not a Data class object. Skipping.")
            )
            next
          }

          # First merge household-level variables to linked dataset
          # This needs to happen before standardize so household vars are available
          # in variable and value maps during standardize operations
          self$..merge_household_vars_to_linked(linked_obj, link_name, hh_stage = stage, linked_stage = stage)

          # Then check if the linked dataset is standardized
          phr_message(phr_txt("Checking standardization status of linked dataset '{link_name}'..."))
          if (!linked_obj$standardized) {
            phr_message(phr_txt("Linked dataset '{link_name}' is not standardized. Running standardize()..."))
            linked_obj$standardize(stage = stage)
          } else {
            phr_message(phr_txt("Linked dataset '{link_name}' is already standardized."))
          }
        }

        # Get household data using fallback logic
        hh_data_result <- self$..get_data_with_fallback(self, stage)
        if (is.null(hh_data_result)) {
          phr_warning(
            self$dataset_name,
            phr_txt("No household data available (tried stages: clean, standardized, raw). Cannot aggregate linked data.")
          )
          return(invisible(NULL))
        }

        hh_data <- hh_data_result$data
        hh_data_stage <- hh_data_result$stage
        phr_message(phr_txt("Using household data from stage: {hh_data_stage}"))

        # Now aggregate data from linked datasets and add to household data
        hh_data_updated <- self$..aggregate_linked_data(hh_data, stage = stage)

        # Update the appropriate household data stage
        if (hh_data_stage == "clean") {
          self$clean_data <- hh_data_updated
        } else if (hh_data_stage == "standardized") {
          self$standardized_data <- hh_data_updated
        } else {
          self$raw_data <- hh_data_updated
        }

        phr_message(phr_txt("Pre-standardize processing complete for {self$dataset_name}."))

      }, on_error = "warn", origin = paste0(self$dataset_name, "$pre_standardize"))
    },


    #' Get Survey Design Object
    #'
    #' @description
    #' Creates and returns a srvyr survey design object for weighted analyses
    #'
    #' @param stage Character string specifying data stage: "clean", "standardized", or "raw"
    #'
    #' @return A srvyr survey design object, or NULL if required fields are missing
    #'
    #' @details
    #' Creates a survey design using:
    #' * ids: cluster_id (required)
    #' * weights: weight (required)
    #' * strata: strata (optional)
    #' * fpc: fpc (optional)
    #'
    #' Stores the design object in self$survey_design field for later use
    get_survey_design = function(stage = c("clean", "standardized", "raw")) {
      stage <- match.arg(stage)

      phr_try({
        if (!requireNamespace("srvyr", quietly = TRUE)) {
          phr_error(
            self$dataset_name,
            phr_txt("Package 'srvyr' must be installed to create survey design objects.")
          )
        }

        df <- self$get_data(stage)
        if (is.null(df)) {
          phr_warning(self$dataset_name, phr_txt(glue::glue("No {stage} data available to create survey design.")))
          return(NULL)
        }

        ids_col    <- self$variable_map$cluster_id
        strata_col <- self$variable_map$strata
        weight_col <- self$variable_map$weight
        fpc_col    <- self$variable_map$fpc %||% NULL

        missing_fields <- c()
        if (is.null(ids_col) || ids_col == "" || !ids_col %in% names(df))
          missing_fields <- c(missing_fields, "cluster_id")
        if (is.null(weight_col) || weight_col == "" || !weight_col %in% names(df))
          missing_fields <- c(missing_fields, "weight")

        if (length(missing_fields) > 0) {
          phr_warning(
            self$dataset_name,
            phr_txt(
              "Survey design creation incomplete: missing required fields ({paste(missing_fields, collapse=', ')})."
            )
          )
          return(NULL)
        }

        # ---- OPTIONAL COLUMN SAFETY
        # Prevent srvyr from choking on missing optional fields like `strata` or `fpc`
        optional_cols <- c(strata_col, fpc_col)
        for (oc in optional_cols) {
          if (!is.null(oc) && !(oc %in% names(df))) {
            # If the column does not exist, nullify it so srvyr ignores it
            if (identical(oc, strata_col)) strata_col <- NULL
            if (identical(oc, fpc_col))    fpc_col    <- NULL
          }
        }


        # Tidy-eval symbols
        ids_sym    <- if (!is.null(ids_col)) rlang::sym(ids_col) else NULL
        strata_sym <- if (!is.null(strata_col)) rlang::sym(strata_col) else NULL
        weight_sym <- if (!is.null(weight_col)) rlang::sym(weight_col) else NULL
        fpc_sym    <- if (!is.null(fpc_col)) rlang::sym(fpc_col) else NULL

        design <- srvyr::as_survey_design(
          .data   = df,
          ids     = !!ids_sym,
          strata  = !!strata_sym,
          weights = !!weight_sym,
          fpc     = !!fpc_sym,
          nest    = TRUE
        )

        self$survey_design <- design
        phr_message(phr_txt("Survey design object created and stored in HouseholdData."))

        invisible(design)

      }, on_error = "abort", origin = paste0(self$dataset_name, "$get_survey_design"))
    },


    #' @description
    #' Generate a thematic DataQuality object for household data.
    #' Overrides base Data class method to provide household-specific quality types.
    #'
    #' @param stage The data stage to use ("standardized" or "clean")
    #' @param type The type of DataQuality object to create ("fsl", "wash", "mortality", "health")
    #' @return A DataQuality object or NULL
    generate_data_quality = function(stage = c("standardized", "clean"),
                                     type = c("fsl", "wash", "mortality", "health")) {

      stage <- match.arg(stage)
      type <- match.arg(type)

      phr_try({

        df <- self$get_data(stage)

        if (is.null(df)) {
          phr_warning(
            self$dataset_name,
            phr_txt("No {stage} data available for DataQuality generation.")
          )
          return(NULL)
        }

        # Get hash, variable_map, and value_map from Data object
        data_hash <- self$get_hash(stage)
        variable_map <- self$variable_map
        value_map <- self$value_map

        # Helper function to extract linked data info
        get_linked_data_info <- function(linked_obj, stage) {
          if (is.null(linked_obj)) {
            return(list(
              data = NULL,
              stage_name = NULL,
              hash = NULL,
              variable_map = NULL,
              value_map = NULL
            ))
          }
          list(
            data = linked_obj$get_data(stage),
            stage_name = stage,
            hash = linked_obj$get_hash(stage),
            variable_map = linked_obj$variable_map,
            value_map = linked_obj$value_map
          )
        }

        # Get linked dataset info based on type
        linked_info_containers <- NULL
        linked_info_roster <- NULL
        linked_info_deaths <- NULL
        linked_info_health <- NULL
        
        if (type == "wash" && !is.null(self$linked_objects$water_containers)) {
          linked_info_containers <- get_linked_data_info(self$linked_objects$water_containers, stage)
        } else if (type == "mortality") {
          if (!is.null(self$linked_objects$roster)) {
            linked_info_roster <- get_linked_data_info(self$linked_objects$roster, stage)
          }
          if (!is.null(self$linked_objects$deaths)) {
            linked_info_deaths <- get_linked_data_info(self$linked_objects$deaths, stage)
          }
        } else if (type == "health") {
          if (!is.null(self$linked_objects$roster)) {
            linked_info_roster <- get_linked_data_info(self$linked_objects$roster, stage)
          }
          if (!is.null(self$linked_objects$health)) {
            linked_info_health <- get_linked_data_info(self$linked_objects$health, stage)
          }
        }

        # Create the appropriate DataQuality subclass based on type
        dq <- switch(
          type,
          "fsl" = FSLDataQuality$new(
            data = df,
            parent_data_object = self,
            dataset_name = paste0(self$dataset_name, "_FSLDataQuality"),
            data_stage_name = stage,
            data_hash = data_hash,
            variable_map = variable_map,
            value_map = value_map,
            variable_label = self$variable_label,
            value_label = self$value_label
          ),
          "wash" = WASHDataQuality$new(
            data = df,
            parent_data_object = self,
            dataset_name = paste0(self$dataset_name, "_WASHDataQuality"),
            data_stage_name = stage,
            data_hash = data_hash,
            variable_map = variable_map,
            value_map = value_map,
            variable_label = self$variable_label,
            value_label = self$value_label,
            # Linked water container data
            linked_containers_data = if (!is.null(linked_info_containers)) linked_info_containers$data else NULL,
            linked_containers_data_stage_name = if (!is.null(linked_info_containers)) linked_info_containers$stage_name else NULL,
            linked_containers_data_hash = if (!is.null(linked_info_containers)) linked_info_containers$hash else NULL,
            linked_containers_variable_map = if (!is.null(linked_info_containers)) linked_info_containers$variable_map else NULL,
            linked_containers_value_map = if (!is.null(linked_info_containers)) linked_info_containers$value_map else NULL
          ),
          "mortality" = MortalityDataQuality$new(
            data = df,
            parent_data_object = self,
            dataset_name = paste0(self$dataset_name, "_MortalityDataQuality"),
            data_stage_name = stage,
            data_hash = data_hash,
            variable_map = variable_map,
            value_map = value_map,
            variable_label = self$variable_label,
            value_label = self$value_label,
            # Linked roster data
            linked_ind_roster_data = if (!is.null(linked_info_roster)) linked_info_roster$data else NULL,
            linked_ind_roster_data_stage_name = if (!is.null(linked_info_roster)) linked_info_roster$stage_name else NULL,
            linked_ind_roster_data_hash = if (!is.null(linked_info_roster)) linked_info_roster$hash else NULL,
            linked_ind_roster_variable_map = if (!is.null(linked_info_roster)) linked_info_roster$variable_map else NULL,
            linked_ind_roster_value_map = if (!is.null(linked_info_roster)) linked_info_roster$value_map else NULL,
            # Linked deaths data
            linked_ind_deaths_data = if (!is.null(linked_info_deaths)) linked_info_deaths$data else NULL,
            linked_ind_deaths_data_stage_name = if (!is.null(linked_info_deaths)) linked_info_deaths$stage_name else NULL,
            linked_ind_deaths_data_hash = if (!is.null(linked_info_deaths)) linked_info_deaths$hash else NULL,
            linked_ind_deaths_variable_map = if (!is.null(linked_info_deaths)) linked_info_deaths$variable_map else NULL,
            linked_ind_deaths_value_map = if (!is.null(linked_info_deaths)) linked_info_deaths$value_map else NULL
          ),
          "health" = HealthDataQuality$new(
            data = df,
            parent_data_object = self,
            dataset_name = paste0(self$dataset_name, "_HealthDataQuality"),
            data_stage_name = stage,
            data_hash = data_hash,
            variable_map = variable_map,
            value_map = value_map,
            variable_label = self$variable_label,
            value_label = self$value_label,
            # Linked roster data
            linked_ind_roster_data = if (!is.null(linked_info_roster)) linked_info_roster$data else NULL,
            linked_ind_roster_data_stage_name = if (!is.null(linked_info_roster)) linked_info_roster$stage_name else NULL,
            linked_ind_roster_data_hash = if (!is.null(linked_info_roster)) linked_info_roster$hash else NULL,
            linked_ind_roster_variable_map = if (!is.null(linked_info_roster)) linked_info_roster$variable_map else NULL,
            linked_ind_roster_value_map = if (!is.null(linked_info_roster)) linked_info_roster$value_map else NULL,
            # Linked health individual data
            linked_ind_health_data = if (!is.null(linked_info_health)) linked_info_health$data else NULL,
            linked_ind_health_data_stage_name = if (!is.null(linked_info_health)) linked_info_health$stage_name else NULL,
            linked_ind_health_data_hash = if (!is.null(linked_info_health)) linked_info_health$hash else NULL,
            linked_ind_health_variable_map = if (!is.null(linked_info_health)) linked_info_health$variable_map else NULL,
            linked_ind_health_value_map = if (!is.null(linked_info_health)) linked_info_health$value_map else NULL
          ),
          # Default: error with phr_error
          phr_error(
            origin = paste0(self$dataset_name, "$generate_data_quality"),
            message = phr_txt("Unknown quality type '{type}' for HouseholdData. Valid types: fsl, wash, mortality, health")
          )
        )

        phr_message(
          phr_txt("Generated {type} DataQuality object for {self$dataset_name}.")
        )

        return(dq)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$generate_data_quality"))
    },


    #' @description
    #' Generate a thematic DataAnalysis object for household data.
    #' Overrides base Data class method to provide household-specific analysis types.
    #'
    #' @param stage The data stage to use ("standardized" or "clean")
    #' @param type The type of Analysis object to create ("fsl", "wash", "health", "mortality", "general")
    #' @param analysis_config Optional analysis configuration (data analysis plan)
    #' @return An Analysis object or NULL
    generate_data_analysis = function(stage = c("standardized", "clean"),
                                      type = c("fsl", "wash", "health", "mortality", "general"),
                                      analysis_config = NULL) {

      stage <- match.arg(stage)
      type <- match.arg(type)

      phr_try({

        df <- self$get_data(stage)

        if (is.null(df)) {
          phr_warning(
            self$dataset_name,
            phr_txt("No {stage} data available for Analysis generation.")
          )
          return(NULL)
        }

        # Get hash, variable_map, and value_map from Data object
        data_hash <- self$get_hash(stage)
        variable_map <- self$variable_map
        value_map <- self$value_map

        # Helper function to extract linked data info
        get_linked_data_info <- function(linked_obj, stage) {
          if (is.null(linked_obj)) {
            return(list(
              data = NULL,
              stage_name = NULL,
              hash = NULL,
              variable_map = NULL,
              value_map = NULL
            ))
          }
          list(
            data = linked_obj$get_data(stage),
            stage_name = stage,
            hash = linked_obj$get_hash(stage),
            variable_map = linked_obj$variable_map,
            value_map = linked_obj$value_map
          )
        }

        # Get linked dataset info based on type
        linked_info_containers <- NULL
        linked_info_health <- NULL
        linked_info_roster <- NULL
        linked_info_deaths <- NULL
        
        if (type == "wash" && !is.null(self$linked_objects$water_containers)) {
          linked_info_containers <- get_linked_data_info(self$linked_objects$water_containers, stage)
        } else if (type == "health" && !is.null(self$linked_objects$health)) {
          linked_info_health <- get_linked_data_info(self$linked_objects$health, stage)
        } else if (type == "mortality") {
          if (!is.null(self$linked_objects$roster)) {
            linked_info_roster <- get_linked_data_info(self$linked_objects$roster, stage)
          }
          if (!is.null(self$linked_objects$deaths)) {
            linked_info_deaths <- get_linked_data_info(self$linked_objects$deaths, stage)
          }
        }

        # Create the appropriate Analysis subclass based on type
        analysis <- switch(
          type,
          "fsl" = QuantDataAnalysisFSL$new(
            dap = analysis_config,
            parent_data_object = self,
            dataset_name = paste0(self$dataset_name, "_FSLAnalysis"),
            data = df,
            data_stage_name = stage,
            data_hash = data_hash,
            variable_map = variable_map,
            value_map = value_map,
            variable_label = self$variable_label,
            value_label = self$value_label
          ),
          "wash" = WASHAnalysis$new(
            dap = analysis_config,
            parent_data_object = self,
            dataset_name = paste0(self$dataset_name, "_WASHAnalysis"),
            data = df,
            data_stage_name = stage,
            data_hash = data_hash,
            variable_map = variable_map,
            value_map = value_map,
            variable_label = self$variable_label,
            value_label = self$value_label,
            # Linked water container data
            linked_containers_data = if (!is.null(linked_info_containers)) linked_info_containers$data else NULL,
            linked_containers_data_stage_name = if (!is.null(linked_info_containers)) linked_info_containers$stage_name else NULL,
            linked_containers_data_hash = if (!is.null(linked_info_containers)) linked_info_containers$hash else NULL,
            linked_containers_variable_map = if (!is.null(linked_info_containers)) linked_info_containers$variable_map else NULL,
            linked_containers_value_map = if (!is.null(linked_info_containers)) linked_info_containers$value_map else NULL
          ),
          "health" = HealthAnalysis$new(
            dap = analysis_config,
            parent_data_object = self,
            dataset_name = paste0(self$dataset_name, "_HealthAnalysis"),
            data = df,
            data_stage_name = stage,
            data_hash = data_hash,
            variable_map = variable_map,
            value_map = value_map,
            variable_label = self$variable_label,
            value_label = self$value_label,
            # Linked health individual data
            linked_ind_health_data = if (!is.null(linked_info_health)) linked_info_health$data else NULL,
            linked_ind_health_data_stage_name = if (!is.null(linked_info_health)) linked_info_health$stage_name else NULL,
            linked_ind_health_data_hash = if (!is.null(linked_info_health)) linked_info_health$hash else NULL,
            linked_ind_health_variable_map = if (!is.null(linked_info_health)) linked_info_health$variable_map else NULL,
            linked_ind_health_value_map = if (!is.null(linked_info_health)) linked_info_health$value_map else NULL
          ),
          "mortality" = MortalityAnalysis$new(
            dap = analysis_config,
            parent_data_object = self,
            dataset_name = paste0(self$dataset_name, "_MortalityAnalysis"),
            data = df,
            data_stage_name = stage,
            data_hash = data_hash,
            variable_map = variable_map,
            value_map = value_map,
            variable_label = self$variable_label,
            value_label = self$value_label,
            # Linked roster data
            linked_ind_roster_data = if (!is.null(linked_info_roster)) linked_info_roster$data else NULL,
            linked_ind_roster_data_stage_name = if (!is.null(linked_info_roster)) linked_info_roster$stage_name else NULL,
            linked_ind_roster_data_hash = if (!is.null(linked_info_roster)) linked_info_roster$hash else NULL,
            linked_ind_roster_variable_map = if (!is.null(linked_info_roster)) linked_info_roster$variable_map else NULL,
            linked_ind_roster_value_map = if (!is.null(linked_info_roster)) linked_info_roster$value_map else NULL,
            # Linked deaths data
            linked_ind_deaths_data = if (!is.null(linked_info_deaths)) linked_info_deaths$data else NULL,
            linked_ind_deaths_data_stage_name = if (!is.null(linked_info_deaths)) linked_info_deaths$stage_name else NULL,
            linked_ind_deaths_data_hash = if (!is.null(linked_info_deaths)) linked_info_deaths$hash else NULL,
            linked_ind_deaths_variable_map = if (!is.null(linked_info_deaths)) linked_info_deaths$variable_map else NULL,
            linked_ind_deaths_value_map = if (!is.null(linked_info_deaths)) linked_info_deaths$value_map else NULL
          ),
          "general" = QuantDataAnalysisGeneral$new(
            dap = analysis_config,
            parent_data_object = self,
            dataset_name = paste0(self$dataset_name, "_GeneralAnalysis"),
            data = df,
            data_stage_name = stage,
            data_hash = data_hash,
            variable_map = variable_map,
            value_map = value_map,
            variable_label = self$variable_label,
            value_label = self$value_label
          ),
          # Default: error with phr_error
          phr_error(
            origin = paste0(self$dataset_name, "$generate_data_analysis"),
            message = phr_txt("Unknown analysis type '{type}' for HouseholdData. Valid types: fsl, wash, health, mortality, general")
          )
        )

        phr_message(
          phr_txt("Generated {type} Analysis object for {self$dataset_name}.")
        )

        return(analysis)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$generate_data_analysis"))
    },


    #' @description
    #' Generate a thematic DataAnalytics object for household data.
    #'
    #' Creates a unified analytics object integrating both data quality checks and
    #' quantitative analysis for the specified sector. Merges the functionality of
    #' `generate_data_quality()` and `generate_data_analysis()` into one object.
    #'
    #' @param stage The data stage to use ("standardized" or "clean")
    #' @param type The type of analytics object to create ("fsl", "wash", "health", "mortality", "general")
    #' @param analysis_config Optional analysis configuration (data analysis plan tibble)
    #' @return A DataAnalytics subclass object or NULL
    generate_data_analytics = function(stage = c("standardized", "clean"),
                                       type  = c("fsl", "wash", "health", "mortality", "general"),
                                       analysis_config = NULL) {

      stage <- match.arg(stage)
      type  <- match.arg(type)

      phr_try({

        df <- self$get_data(stage)

        if (is.null(df)) {
          phr_warning(
            self$dataset_name,
            phr_txt("No {stage} data available for DataAnalytics generation.")
          )
          return(NULL)
        }

        data_hash    <- self$get_hash(stage)
        variable_map <- self$variable_map
        value_map    <- self$value_map

        # Helper to extract linked data info
        get_linked_data_info <- function(linked_obj, stage) {
          if (is.null(linked_obj)) {
            return(list(
              data = NULL, stage_name = NULL, hash = NULL,
              variable_map = NULL, value_map = NULL
            ))
          }
          list(
            data         = linked_obj$get_data(stage),
            stage_name   = stage,
            hash         = linked_obj$get_hash(stage),
            variable_map = linked_obj$variable_map,
            value_map    = linked_obj$value_map
          )
        }

        linked_info_containers <- NULL
        linked_info_roster     <- NULL
        linked_info_deaths     <- NULL
        linked_info_health     <- NULL

        if (type == "wash" && !is.null(self$linked_objects$water_containers)) {
          linked_info_containers <- get_linked_data_info(self$linked_objects$water_containers, stage)
        } else if (type == "mortality") {
          if (!is.null(self$linked_objects$roster)) {
            linked_info_roster <- get_linked_data_info(self$linked_objects$roster, stage)
          } else {
            phr_warning(
              origin  = paste0(self$dataset_name, "$generate_data_analytics"),
              message = phr_txt("No linked roster data found. Mortality analytics will use only household data.")
            )
          }
          if (!is.null(self$linked_objects$deaths)) {
            linked_info_deaths <- get_linked_data_info(self$linked_objects$deaths, stage)
          } else {
            phr_warning(
              origin  = paste0(self$dataset_name, "$generate_data_analytics"),
              message = phr_txt("No linked deaths data found. Mortality analytics will use only household data.")
            )
          }
        } else if (type == "health") {
          if (!is.null(self$linked_objects$roster)) {
            linked_info_roster <- get_linked_data_info(self$linked_objects$roster, stage)
          }
          if (!is.null(self$linked_objects$health)) {
            linked_info_health <- get_linked_data_info(self$linked_objects$health, stage)
          }
        }

        analytics <- switch(
          type,
          "fsl" = FSLDataAnalytics$new(
            data            = df,
            dap             = analysis_config,
            parent_data_object = self,
            dataset_name    = paste0(self$dataset_name, "_FSLDataAnalytics"),
            data_stage_name = stage,
            data_hash       = data_hash,
            variable_map    = variable_map,
            value_map       = value_map,
            variable_label  = self$variable_label,
            value_label     = self$value_label
          ),
          "wash" = WASHDataAnalytics$new(
            data            = df,
            dap             = analysis_config,
            parent_data_object = self,
            dataset_name    = paste0(self$dataset_name, "_WASHDataAnalytics"),
            data_stage_name = stage,
            data_hash       = data_hash,
            variable_map    = variable_map,
            value_map       = value_map,
            variable_label  = self$variable_label,
            value_label     = self$value_label,
            linked_containers_data              = if (!is.null(linked_info_containers)) linked_info_containers$data else NULL,
            linked_containers_data_stage_name   = if (!is.null(linked_info_containers)) linked_info_containers$stage_name else NULL,
            linked_containers_data_hash         = if (!is.null(linked_info_containers)) linked_info_containers$hash else NULL,
            linked_containers_variable_map      = if (!is.null(linked_info_containers)) linked_info_containers$variable_map else NULL,
            linked_containers_value_map         = if (!is.null(linked_info_containers)) linked_info_containers$value_map else NULL
          ),
          "mortality" = MortalityDataAnalytics$new(
            data            = df,
            dap             = analysis_config,
            parent_data_object = self,
            dataset_name    = paste0(self$dataset_name, "_MortalityDataAnalytics"),
            data_stage_name = stage,
            data_hash       = data_hash,
            variable_map    = variable_map,
            value_map       = value_map,
            variable_label  = self$variable_label,
            value_label     = self$value_label,
            linked_ind_roster_data              = if (!is.null(linked_info_roster)) linked_info_roster$data else NULL,
            linked_ind_roster_data_stage_name   = if (!is.null(linked_info_roster)) linked_info_roster$stage_name else NULL,
            linked_ind_roster_data_hash         = if (!is.null(linked_info_roster)) linked_info_roster$hash else NULL,
            linked_ind_roster_variable_map      = if (!is.null(linked_info_roster)) linked_info_roster$variable_map else NULL,
            linked_ind_roster_value_map         = if (!is.null(linked_info_roster)) linked_info_roster$value_map else NULL,
            linked_ind_deaths_data              = if (!is.null(linked_info_deaths)) linked_info_deaths$data else NULL,
            linked_ind_deaths_data_stage_name   = if (!is.null(linked_info_deaths)) linked_info_deaths$stage_name else NULL,
            linked_ind_deaths_data_hash         = if (!is.null(linked_info_deaths)) linked_info_deaths$hash else NULL,
            linked_ind_deaths_variable_map      = if (!is.null(linked_info_deaths)) linked_info_deaths$variable_map else NULL,
            linked_ind_deaths_value_map         = if (!is.null(linked_info_deaths)) linked_info_deaths$value_map else NULL
          ),
          "health" = HealthDataAnalytics$new(
            data            = df,
            dap             = analysis_config,
            parent_data_object = self,
            dataset_name    = paste0(self$dataset_name, "_HealthDataAnalytics"),
            data_stage_name = stage,
            data_hash       = data_hash,
            variable_map    = variable_map,
            value_map       = value_map,
            variable_label  = self$variable_label,
            value_label     = self$value_label,
            linked_ind_roster_data              = if (!is.null(linked_info_roster)) linked_info_roster$data else NULL,
            linked_ind_roster_data_stage_name   = if (!is.null(linked_info_roster)) linked_info_roster$stage_name else NULL,
            linked_ind_roster_data_hash         = if (!is.null(linked_info_roster)) linked_info_roster$hash else NULL,
            linked_ind_roster_variable_map      = if (!is.null(linked_info_roster)) linked_info_roster$variable_map else NULL,
            linked_ind_roster_value_map         = if (!is.null(linked_info_roster)) linked_info_roster$value_map else NULL,
            linked_ind_health_data              = if (!is.null(linked_info_health)) linked_info_health$data else NULL,
            linked_ind_health_data_stage_name   = if (!is.null(linked_info_health)) linked_info_health$stage_name else NULL,
            linked_ind_health_data_hash         = if (!is.null(linked_info_health)) linked_info_health$hash else NULL,
            linked_ind_health_variable_map      = if (!is.null(linked_info_health)) linked_info_health$variable_map else NULL,
            linked_ind_health_value_map         = if (!is.null(linked_info_health)) linked_info_health$value_map else NULL
          ),
          "general" = GeneralDataAnalytics$new(
            data            = df,
            dap             = analysis_config,
            parent_data_object = self,
            dataset_name    = paste0(self$dataset_name, "_GeneralDataAnalytics"),
            data_stage_name = stage,
            data_hash       = data_hash,
            variable_map    = variable_map,
            value_map       = value_map,
            variable_label  = self$variable_label,
            value_label     = self$value_label
          ),
          phr_error(
            origin  = paste0(self$dataset_name, "$generate_data_analytics"),
            message = phr_txt("Unknown analytics type '{type}' for HouseholdData. Valid types: fsl, wash, health, mortality, general")
          )
        )

        phr_message(
          phr_txt("Generated {type} DataAnalytics object for {self$dataset_name}.")
        )

        return(analytics)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$generate_data_analytics"))
    },



    #' Aggregate Linked Dataset Data
    #'
    #' @description
    #' Internal helper that processes linked datasets and adds aggregated columns to household data
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
    #'
    #' @keywords internal
    ..aggregate_linked_data = function(hh_data, stage = c("clean", "standardized", "raw")) {
      stage <- match.arg(stage)

      # Store original hh_data to ensure we never return NULL
      original_hh_data <- hh_data

      result <- phr_try({

        # Use linked_objects (inherited from Data class) instead of linked_datasets
        if (length(self$linked_objects) == 0) {
          return(hh_data)
        }

        # Get household UUID column
        hh_uuid_col <- self$uuid

        if (!hh_uuid_col %in% names(hh_data)) {
          phr_warning(
            self$dataset_name,
            phr_txt("Household UUID column '{hh_uuid_col}' not found. Cannot aggregate linked data.")
          )
          return(hh_data)
        }

        # Process each linked dataset
        for (link_name in names(self$linked_objects)) {
          link_info <- self$linked_objects[[link_name]]
          linked_obj <- link_info$object

          # Skip if not a Data object
          if (!inherits(linked_obj, "Data")) {
            next
          }

          # Get linked data using fallback logic
          linked_data_result <- self$..get_data_with_fallback(linked_obj, stage)
          if (is.null(linked_data_result)) {
            phr_warning(
              self$dataset_name,
              phr_txt("Linked dataset '{link_name}' has no data at any stage.")
            )
            next
          }

          linked_data <- linked_data_result$data
          linked_data_stage <- linked_data_result$stage
          phr_message(phr_txt("Using linked dataset '{link_name}' from stage: {linked_data_stage}"))

          # Determine the household linkage column in the linked dataset
          # Try to find hh_uuid in variable_map first
          linked_hh_col <- linked_obj$variable_map$hh_uuid %||% linked_obj$variable_map$household_uuid %||% "hh_uuid"

          if (!linked_hh_col %in% names(linked_data)) {
            phr_warning(
              self$dataset_name,
              phr_txt("Linked dataset '{link_name}' has no household linkage column '{linked_hh_col}'.")
            )
            next
          }

          phr_message(phr_txt("Aggregating data from linked dataset '{link_name}'..."))

          # Aggregate based on dataset class
          # Check for specific subclasses first, then fall back to base classes
          # Store result and check for NULL before assigning to hh_data
          agg_result <- NULL
          if (inherits(linked_obj, "DeathIndividualData")) {
            agg_result <- self$..aggregate_deaths_data(hh_data, linked_data, hh_uuid_col, linked_hh_col, link_name, linked_obj)
          } else if (inherits(linked_obj, "WaterContainerData")) {
            agg_result <- self$..aggregate_water_container_data(hh_data, linked_data, hh_uuid_col, linked_hh_col, link_name, linked_obj)
          } else if (inherits(linked_obj, "NutritionIndividualData")) {
            agg_result <- self$..aggregate_nutrition_data(hh_data, linked_data, hh_uuid_col, linked_hh_col, link_name, linked_obj)
          } else if (inherits(linked_obj, "HealthIndividualData")) {
            agg_result <- self$..aggregate_health_data(hh_data, linked_data, hh_uuid_col, linked_hh_col, link_name, linked_obj)
          } else if (inherits(linked_obj, "WomenIndividualData")) {
            agg_result <- self$..aggregate_women_data(hh_data, linked_data, hh_uuid_col, linked_hh_col, link_name, linked_obj)
          } else if (inherits(linked_obj, "IndividualData")) {
            # This is primarily for base IndividualData (roster data)
            # Any remaining IndividualData subclasses without specialized handlers will also use this
            # Note: roster-style aggregation may not be appropriate for all subclass types
            agg_result <- self$..aggregate_roster_data(hh_data, linked_data, hh_uuid_col, linked_hh_col, link_name, linked_obj)
          } else {
            phr_message(phr_txt("No specific aggregation defined for linked dataset '{link_name}' of class {class(linked_obj)[1]}."))
            agg_result <- hh_data  # No aggregation, keep existing data
          }

          # Only update hh_data if aggregation succeeded (didn't return NULL)
          if (!is.null(agg_result)) {
            hh_data <- agg_result
          } else {
            phr_warning(
              self$dataset_name,
              phr_txt("Aggregation of linked dataset '{link_name}' failed. Skipping this dataset.")
            )
          }
        }

        return(hh_data)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$..aggregate_linked_data"))

      # Ensure we never return NULL - if phr_try returned NULL on error, return original data
      if (is.null(result)) {
        phr_warning(
          self$dataset_name,
          phr_txt("Aggregation encountered an error. Returning original household data without aggregated columns.")
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
    #' @return Modified household data frame with aggregated death columns
    #'
    #' @details
    #' Sums canonical death and person_time columns by household: death, death_under5, death_male,
    #' death_female, death_non_trauma, death_trauma, death_other, death_current_location,
    #' death_migration, death_last_location, death_birth, person_time, person_time_under5,
    #' person_time_male, person_time_female
    #'
    #' @keywords internal
    ..aggregate_deaths_data = function(hh_data, deaths_data, hh_uuid_col, linked_hh_col, link_name, linked_obj) {

      phr_try({

        # Use variable_map to identify canonical death and person_time columns
        # These are the standardized output column names from death data processing
        vm <- linked_obj$variable_map

        # Build list of canonical column names
        # These are standardized output columns created by add_standardized_deaths and add_persontime
        # They use fixed names as they are outputs, not inputs that need mapping
        canonical_cols <- c(
          "death", "death_under5", "death_male", "death_female",
          "death_non_trauma", "death_trauma", "death_other",
          "death_current_location", "death_migration", "death_last_location",
          "death_birth",
          "person_time", "person_time_under5", "person_time_male", "person_time_female"
        )

        # Filter to columns that actually exist in deaths_data
        available_cols <- intersect(canonical_cols, names(deaths_data))

        if (length(available_cols) == 0) {
          phr_warning(
            self$dataset_name,
            phr_txt("No canonical death or person_time columns found in linked dataset '{link_name}'.")
          )
          return(hh_data)
        }

        # Aggregate by household
        agg_data <- deaths_data %>%
          dplyr::group_by(!!rlang::sym(linked_hh_col)) %>%
          dplyr::summarise(
            dplyr::across(
              dplyr::all_of(available_cols),
              ~ sum(as.numeric(.), na.rm = TRUE)
            ),
            .groups = "drop"
          )

        # Rename columns with prefix to show they come from linked dataset
        prefix <- paste0("linked_", link_name, "_")
        col_names <- names(agg_data)
        col_names[-1] <- paste0(prefix, col_names[-1])
        names(agg_data) <- c(hh_uuid_col, col_names[-1])

        # Check for existing columns and warn if overwriting (excluding join column)
        agg_cols_only <- setdiff(names(agg_data), hh_uuid_col)
        existing_cols <- intersect(agg_cols_only, names(hh_data))
        if (length(existing_cols) > 0) {
          phr_warning(
            self$dataset_name,
            phr_txt("The following columns already exist and will NOT be overwritten: {paste(existing_cols, collapse=', ')}")
          )
          # Remove existing columns from agg_data but keep the join column
          cols_to_keep <- c(hh_uuid_col, setdiff(names(agg_data), c(hh_uuid_col, existing_cols)))
          agg_data <- agg_data[, cols_to_keep, drop = FALSE]
        }

        # Left join aggregated data to household data
        hh_data <- dplyr::left_join(hh_data, agg_data, by = hh_uuid_col)

        # Replace NA values with 0 for all linked columns
        # Households with no linked records will have NA after left_join.
        # For count/sum aggregations, 0 is semantically correct (measured as zero).
        linked_cols <- setdiff(names(agg_data), hh_uuid_col)
        if (length(linked_cols) > 0) {
          hh_data <- hh_data %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(linked_cols), ~ ifelse(is.na(.), 0, .)))
        }

        phr_message(
          phr_txt("Added {ncol(agg_data) - 1} aggregated column(s) from deaths data")
        )

        return(hh_data)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$..aggregate_deaths_data"))
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
    #' @return Modified household data frame with aggregated water container columns
    #'
    #' @details
    #' Sums total liters by household and counts number of containers
    #'
    #' @keywords internal
    ..aggregate_water_container_data = function(hh_data, water_data, hh_uuid_col, linked_hh_col, link_name, linked_obj) {

      phr_try({

        # Look for total liters column - try variable_map first, then fallback to known names
        water_col <- linked_obj$variable_map$container_capacity_liters %||%
                     linked_obj$variable_map$wash_container_total_liters %||%
                     intersect(names(water_data), c("wash_container_total_litres", "wash_container_total_liters", "container_capacity_liters", "total_liters"))[1]

        if (is.null(water_col) || is.na(water_col) || !water_col %in% names(water_data)) {
          phr_warning(
            self$dataset_name,
            phr_txt("No water container total liters column found in linked dataset '{link_name}'.")
          )
          return(hh_data)
        }

        # Aggregate by household
        agg_data <- water_data %>%
          dplyr::group_by(!!rlang::sym(linked_hh_col)) %>%
          dplyr::summarise(
            wash_container_total_liters = sum(as.numeric(.data[[water_col]]), na.rm = TRUE),
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
          phr_warning(
            self$dataset_name,
            phr_txt("The following columns already exist and will NOT be overwritten: {paste(existing_cols, collapse=', ')}")
          )
          # Remove existing columns from agg_data but keep the join column
          cols_to_keep <- c(hh_uuid_col, setdiff(names(agg_data), c(hh_uuid_col, existing_cols)))
          agg_data <- agg_data[, cols_to_keep, drop = FALSE]
        }

        # Left join
        hh_data <- dplyr::left_join(hh_data, agg_data, by = hh_uuid_col)

        # Replace NA values with 0 for all linked columns
        # Households with no linked records will have NA after left_join.
        # For count/sum aggregations, 0 is semantically correct (measured as zero).
        linked_cols <- setdiff(names(agg_data), hh_uuid_col)
        if (length(linked_cols) > 0) {
          hh_data <- hh_data %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(linked_cols), ~ ifelse(is.na(.), 0, .)))
        }

        phr_message(
          phr_txt("Added aggregated water container data: wash_container_total_liters, num_containers")
        )

        return(hh_data)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$..aggregate_water_container_data"))
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
    #' @return Modified household data frame with aggregated roster columns
    #'
    #' @details
    #' Calculates household demographics: household_size, num_children, num_male, num_female,
    #' num_women_15to49, and person_time columns by household. If calc_date_birth_final is
    #' available and recall/survey dates exist, also counts births in recall period.
    #'
    #' @keywords internal
    ..aggregate_roster_data = function(hh_data, roster_data, hh_uuid_col, linked_hh_col, link_name, linked_obj) {

      phr_try({

        # Use canonical column names created by add_standardized_roster_demographics
        # These are the standardized output column names from roster data processing
        # They use fixed names as they are outputs, not inputs that need mapping
        canonical_cols <- c(
          "roster_child_under2", "roster_child_under5",
          "roster_male", "roster_female", "roster_woman_15to49",
          "roster_birth",
          "roster_person_time", "roster_person_time_under5",
          "roster_person_time_male", "roster_person_time_female",
          "roster_age_6_29m", "roster_age_30_59m"
        )

        # Filter to columns that actually exist in roster_data
        available_cols <- intersect(canonical_cols, names(roster_data))

        if (length(available_cols) == 0) {
          phr_warning(
            self$dataset_name,
            phr_txt("No canonical roster columns found in linked dataset '{link_name}'. Falling back to basic household_size calculation.")
          )
          # At minimum, provide household size
          agg_data <- roster_data %>%
            dplyr::group_by(!!rlang::sym(linked_hh_col)) %>%
            dplyr::summarise(roster_household_size = dplyr::n(), .groups = "drop")
        } else {
          # Aggregate canonical columns by summing
          agg_data <- roster_data %>%
            dplyr::group_by(!!rlang::sym(linked_hh_col)) %>%
            dplyr::summarise(
              roster_household_size = dplyr::n(),
              dplyr::across(
                dplyr::all_of(available_cols),
                ~ sum(as.numeric(.), na.rm = TRUE)
              ),
              .groups = "drop"
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
          phr_warning(
            self$dataset_name,
            phr_txt("The following columns already exist and will NOT be overwritten: {paste(existing_cols, collapse=', ')}")
          )
          # Remove existing columns from agg_data but keep the join column
          cols_to_keep <- c(hh_uuid_col, setdiff(names(agg_data), c(hh_uuid_col, existing_cols)))
          agg_data <- agg_data[, cols_to_keep, drop = FALSE]
        }

        # Left join aggregated data to household data
        hh_data <- dplyr::left_join(hh_data, agg_data, by = hh_uuid_col)

        # Replace NA values with 0 for all linked columns
        # Households with no linked records will have NA after left_join.
        # For count/sum aggregations, 0 is semantically correct (measured as zero).
        linked_cols <- setdiff(names(agg_data), hh_uuid_col)
        if (length(linked_cols) > 0) {
          hh_data <- hh_data %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(linked_cols), ~ ifelse(is.na(.), 0, .)))
        }

        phr_message(
          phr_txt("Added {ncol(agg_data) - 1} aggregated column(s) from roster data")
        )

        return(hh_data)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$..aggregate_roster_data"))
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
    #' @return Modified household data frame with aggregated nutrition columns
    #'
    #' @details
    #' Aggregates by age groups: children_under2, children_2to5, children_under5 based on age in months
    #'
    #' @keywords internal
    ..aggregate_nutrition_data = function(hh_data, nutrition_data, hh_uuid_col, linked_hh_col, link_name, linked_obj) {

      phr_try({

        # Use canonical column names created by add_standardized_nutrition_demographics
        # These are the standardized output column names from nutrition data processing
        # They use fixed names as they are outputs, not inputs that need mapping
        canonical_cols <- c(
          "nutrition_child_under2", "nutrition_child_2to5", "nutrition_child_under5"
        )

        # Filter to columns that actually exist in nutrition_data
        available_cols <- intersect(canonical_cols, names(nutrition_data))

        if (length(available_cols) > 0) {
          # New harmonized approach: sum canonical columns
          agg_data <- nutrition_data %>%
            dplyr::group_by(!!rlang::sym(linked_hh_col)) %>%
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
          age_months_col <- if ("calc_age_months" %in% names(nutrition_data)) {
            "calc_age_months"
          } else {
            linked_obj$variable_map$age_months %||%
              linked_obj$variable_map$nutr_age_months %||%
              intersect(names(nutrition_data), c("age_months", "nutr_age_months"))[1]
          }

          if (is.null(age_months_col) || is.na(age_months_col) || !age_months_col %in% names(nutrition_data)) {
            phr_warning(
              self$dataset_name,
              phr_txt("No canonical nutrition columns or age in months column found in linked dataset '{link_name}'.")
            )
            return(hh_data)
          }

          # Aggregate by household using old calculation method
          agg_data <- nutrition_data %>%
            dplyr::group_by(!!rlang::sym(linked_hh_col)) %>%
            dplyr::summarise(
              children_under2 = sum(as.numeric(.data[[age_months_col]]) < 24, na.rm = TRUE),
              children_2to5 = sum(as.numeric(.data[[age_months_col]]) >= 24 & as.numeric(.data[[age_months_col]]) < 60, na.rm = TRUE),
              children_under5 = sum(as.numeric(.data[[age_months_col]]) < 60, na.rm = TRUE),
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
          phr_warning(
            self$dataset_name,
            phr_txt("The following columns already exist and will NOT be overwritten: {paste(existing_cols, collapse=', ')}")
          )
          # Remove existing columns from agg_data but keep the join column
          cols_to_keep <- c(hh_uuid_col, setdiff(names(agg_data), c(hh_uuid_col, existing_cols)))
          agg_data <- agg_data[, cols_to_keep, drop = FALSE]
        }

        # Left join
        hh_data <- dplyr::left_join(hh_data, agg_data, by = hh_uuid_col)

        # Replace NA values with 0 for all linked columns
        linked_cols <- setdiff(names(agg_data), hh_uuid_col)
        if (length(linked_cols) > 0) {
          hh_data <- hh_data %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(linked_cols), ~ ifelse(is.na(.), 0, .)))
        }

        phr_message(
          phr_txt("Added {ncol(agg_data) - 1} aggregated column(s) from nutrition data")
        )

        return(hh_data)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$..aggregate_nutrition_data"))
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
    #' @return Modified household data frame with aggregated health columns
    #'
    #' @details
    #' Counts total number of people recorded (rows) per household
    #'
    #' @keywords internal
    ..aggregate_health_data = function(hh_data, health_data, hh_uuid_col, linked_hh_col, link_name, linked_obj) {

      phr_try({

        # Aggregate by household - count number of people recorded
        agg_data <- health_data %>%
          dplyr::group_by(!!rlang::sym(linked_hh_col)) %>%
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
          phr_warning(
            self$dataset_name,
            phr_txt("The following columns already exist and will NOT be overwritten: {paste(existing_cols, collapse=', ')}")
          )
          # Remove existing columns from agg_data but keep the join column
          cols_to_keep <- c(hh_uuid_col, setdiff(names(agg_data), c(hh_uuid_col, existing_cols)))
          agg_data <- agg_data[, cols_to_keep, drop = FALSE]
        }

        # Left join
        hh_data <- dplyr::left_join(hh_data, agg_data, by = hh_uuid_col)

        # Replace NA values with 0 for all linked columns
        linked_cols <- setdiff(names(agg_data), hh_uuid_col)
        if (length(linked_cols) > 0) {
          hh_data <- hh_data %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(linked_cols), ~ ifelse(is.na(.), 0, .)))
        }

        phr_message(
          phr_txt("Added {ncol(agg_data) - 1} aggregated column(s) from health data")
        )

        return(hh_data)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$..aggregate_health_data"))
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
    #' @return Modified household data frame with aggregated women columns
    #'
    #' @details
    #' Counts women aged 15-49 per household (all rows in WomenIndividualData represent eligible women)
    #'
    #' @keywords internal
    ..aggregate_women_data = function(hh_data, women_data, hh_uuid_col, linked_hh_col, link_name, linked_obj) {

      phr_try({

        # For women data, all rows represent women aged 15-49
        # Simply count the number of women per household
        agg_data <- women_data %>%
          dplyr::group_by(!!rlang::sym(linked_hh_col)) %>%
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
          phr_warning(
            self$dataset_name,
            phr_txt("The following columns already exist and will NOT be overwritten: {paste(existing_cols, collapse=', ')}")
          )
          # Remove existing columns from agg_data but keep the join column
          cols_to_keep <- c(hh_uuid_col, setdiff(names(agg_data), c(hh_uuid_col, existing_cols)))
          agg_data <- agg_data[, cols_to_keep, drop = FALSE]
        }

        # Left join
        hh_data <- dplyr::left_join(hh_data, agg_data, by = hh_uuid_col)

        # Replace NA values with 0 for all linked columns
        linked_cols <- setdiff(names(agg_data), hh_uuid_col)
        if (length(linked_cols) > 0) {
          hh_data <- hh_data %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(linked_cols), ~ ifelse(is.na(.), 0, .)))
        }

        phr_message(
          phr_txt("Added {ncol(agg_data) - 1} aggregated column(s) from women data")
        )

        return(hh_data)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$..aggregate_women_data"))
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
    #' Merges household variables to linked datasets: enum_id, device_id, date_survey, weight,
    #' stratum, cluster_id, site, admin1, admin2, admin3, admin4. Existing variables in linked
    #' datasets are overwritten with household values. Uses fallback logic to find available data stages.
    #'
    #' @keywords internal
    ..merge_household_vars_to_linked = function(linked_obj, link_name,
                                                hh_stage = c("clean", "standardized", "raw"),
                                                linked_stage = c("clean", "standardized", "raw")) {
      hh_stage <- match.arg(hh_stage)
      linked_stage <- match.arg(linked_stage)

      phr_try({

        if (!inherits(linked_obj, "Data")) {
          return(invisible(NULL))
        }

        # Get linked data using fallback logic
        linked_data_result <- self$..get_data_with_fallback(linked_obj, linked_stage)
        if (is.null(linked_data_result)) {
          phr_warning(
            self$dataset_name,
            phr_txt("Linked dataset '{link_name}' has no data at any stage to merge variables to.")
          )
          return(invisible(NULL))
        }

        linked_data <- linked_data_result$data
        linked_data_stage <- linked_data_result$stage
        phr_message(phr_txt("Using linked dataset '{link_name}' data from stage: {linked_data_stage}"))

        # Get household data using fallback logic
        hh_data_result <- self$..get_data_with_fallback(self, hh_stage)
        if (is.null(hh_data_result)) {
          phr_warning(
            self$dataset_name,
            phr_txt("Household data not available at any stage; cannot merge variables to linked dataset '{link_name}'.")
          )
          return(invisible(NULL))
        }

        hh_data <- hh_data_result$data
        hh_data_stage <- hh_data_result$stage
        phr_message(phr_txt("Using household data from stage: {hh_data_stage}"))

        # Get household UUID column
        hh_uuid_col <- self$uuid

        if (!hh_uuid_col %in% names(hh_data)) {
          phr_warning(
            self$dataset_name,
            phr_txt("Household UUID column '{hh_uuid_col}' not found. Cannot merge variables to linked dataset '{link_name}'.")
          )
          return(invisible(NULL))
        }

        # Determine the household linkage column in the linked dataset
        linked_hh_col <- linked_obj$variable_map$hh_uuid %||% linked_obj$variable_map$household_uuid %||% "hh_uuid"

        if (!linked_hh_col %in% names(linked_data)) {
          phr_warning(
            self$dataset_name,
            phr_txt("Linked dataset '{link_name}' has no household linkage column '{linked_hh_col}'.")
          )
          return(invisible(NULL))
        }

        # Define the list of household variables to merge
        # These are the canonical variable names in the variable_map
        hh_vars_to_merge <- c(
          "enum_id", "device_id", "date_survey", "weight", "stratum",
          "cluster_id", "site", "admin1", "admin2", "admin3", "admin4"
        )

        # Identify which variables are available in household data
        vars_to_merge <- list()
        for (var_name in hh_vars_to_merge) {
          # Get the column name from variable_map
          col_name <- self$variable_map[[var_name]]

          # Check if it's mapped and exists in household data
          if (!is.null(col_name) && col_name != "" && !is.na(col_name) && col_name %in% names(hh_data)) {
            vars_to_merge[[var_name]] <- col_name
          }
        }

        if (length(vars_to_merge) == 0) {
          phr_message(phr_txt("No household variables found to merge to linked dataset '{link_name}'."))
          return(invisible(NULL))
        }

        phr_message(phr_txt("Merging {length(vars_to_merge)} household variable(s) to linked dataset '{link_name}'..."))

        # Create a subset of household data with only the variables to merge plus the UUID
        merge_cols <- c(hh_uuid_col, unlist(vars_to_merge))
        hh_merge_data <- hh_data[, merge_cols, drop = FALSE]

        # Check which variables already exist in linked data and will be overwritten
        existing_vars <- intersect(unlist(vars_to_merge), names(linked_data))
        if (length(existing_vars) > 0) {
          phr_message(
            phr_txt("The following variables already exist in linked dataset '{link_name}' and will be overwritten: {paste(existing_vars, collapse=', ')}")
          )
          # Remove existing columns from linked data before merge
          linked_data <- linked_data[, !(names(linked_data) %in% existing_vars), drop = FALSE]
        }

        # Perform the merge (left join from linked_data perspective)
        linked_data_merged <- dplyr::left_join(
          linked_data,
          hh_merge_data,
          by = stats::setNames(hh_uuid_col, linked_hh_col)
        )

        # Update the appropriate linked data stage
        if (linked_data_stage == "clean") {
          linked_obj$clean_data <- linked_data_merged
        } else if (linked_data_stage == "standardized") {
          linked_obj$standardized_data <- linked_data_merged
        } else {
          linked_obj$raw_data <- linked_data_merged
        }

        phr_message(
          phr_txt("Successfully merged {length(vars_to_merge)} household variable(s) to linked dataset '{link_name}'.")
        )

        invisible(TRUE)

      }, on_error = "warn", origin = paste0(self$dataset_name, "$..merge_household_vars_to_linked"))
    }
  )
)
