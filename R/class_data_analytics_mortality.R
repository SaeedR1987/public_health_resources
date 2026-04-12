#' IPHRA Mortality Data Analytics Class
#'
#' The `MortalityDataAnalytics` R6 class extends `DataAnalytics` to provide
#' Mortality-specific quality checks and quantitative analysis in a single
#' integrated object.
#'
#' @description
#' This class provides Mortality-specific:
#' * Quality checks (death recording, mortality rates, age/sex distribution) via quality_schema
#' * Quantitative analysis indicators via analysis_schema (household), analysis_schema_roster,
#'   and analysis_schema_deaths
#' * All visualizations/tables stored in named sub-lists: \code{$household}, \code{$roster},
#'   \code{$deaths} within \code{visualizations} and \code{tables}
#'
#' @details
#' A household dataframe is required at initialization. Linked roster and deaths data are
#' optional and are populated when generated from a \code{HouseholdData} object via
#' \code{generate_data_analytics(type = "mortality")}. When \code{run_analysis()} or
#' \code{run_outputs()} are called, each available dataset (household, roster, deaths) is
#' processed using its dedicated schema and results are stored under the corresponding
#' named sub-list.
#'
#' @field linked_ind_roster_data Optional linked roster dataframe
#' @field linked_ind_roster_data_stage_name Name of linked roster data stage
#' @field linked_ind_roster_data_hash Hash of linked roster data
#' @field linked_ind_roster_variable_map Variable mappings for linked roster data
#' @field linked_ind_roster_value_map Value mappings for linked roster data
#' @field linked_ind_roster_variable_label Variable labels for linked roster data
#' @field linked_ind_roster_value_label Value labels for linked roster data
#' @field linked_ind_deaths_data Optional linked deaths dataframe
#' @field linked_ind_deaths_data_stage_name Name of linked deaths data stage
#' @field linked_ind_deaths_data_hash Hash of linked deaths data
#' @field linked_ind_deaths_variable_map Variable mappings for linked deaths data
#' @field linked_ind_deaths_value_map Value mappings for linked deaths data
#' @field linked_ind_deaths_variable_label Variable labels for linked deaths data
#' @field linked_ind_deaths_value_label Value labels for linked deaths data
#' @field analysis_schema_roster Analysis schema for linked roster dataset
#' @field analysis_schema_deaths Analysis schema for linked deaths dataset
#' @field outputs_schema_roster Outputs schema for linked roster dataset
#' @field outputs_schema_deaths Outputs schema for linked deaths dataset
#' @field data_analysis_plan_roster Data analysis plan for linked roster dataset
#' @field data_analysis_plan_deaths Data analysis plan for linked deaths dataset
#' @field survey_design_roster srvyr survey design object for the linked roster dataset
#' @field survey_design_deaths srvyr survey design object for the linked deaths dataset
#'
#' @seealso [DataAnalytics]
#' @export
MortalityDataAnalytics <- R6::R6Class(
  classname = "MortalityDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    # Fields for linked roster data
    linked_ind_roster_data = NULL,
    linked_ind_roster_data_stage_name = NULL,
    linked_ind_roster_data_hash = NULL,
    linked_ind_roster_variable_map = NULL,
    linked_ind_roster_value_map = NULL,
    linked_ind_roster_variable_label = NULL,
    linked_ind_roster_value_label = NULL,

    # Fields for linked deaths data
    linked_ind_deaths_data = NULL,
    linked_ind_deaths_data_stage_name = NULL,
    linked_ind_deaths_data_hash = NULL,
    linked_ind_deaths_variable_map = NULL,
    linked_ind_deaths_value_map = NULL,
    linked_ind_deaths_variable_label = NULL,
    linked_ind_deaths_value_label = NULL,

    # Per-dataset schemas for roster and deaths
    analysis_schema_roster = NULL,
    analysis_schema_deaths = NULL,
    outputs_schema_roster  = NULL,
    outputs_schema_deaths  = NULL,

    # Per-dataset data analysis plans
    data_analysis_plan_roster = NULL,
    data_analysis_plan_deaths = NULL,

    # Per-dataset survey design objects
    survey_design_roster = NULL,
    survey_design_deaths = NULL,
    base_survey_design_roster = NULL,
    base_survey_design_deaths = NULL,

    #' @description
    #' Initialize a new MortalityDataAnalytics object
    #'
    #' @param data A required data frame (standardized or clean household/mortality data)
    #' @param dap Optional data analysis plan (tibble)
    #' @param parent_data_object The Data object that generated this
    #' @param dataset_name A name for this analytics assessment
    #' @param data_stage_name Name of the data stage (e.g., "standardized", "clean")
    #' @param data_hash Hash of the data from parent Data object
    #' @param variable_map Variable mappings from Data object
    #' @param value_map Value mappings from Data object
    #' @param variable_label Variable labels from Data object
    #' @param value_label Value labels from Data object
    #' @param quality_schema Optional quality check schema
    #' @param linked_ind_roster_data Optional linked roster dataframe
    #' @param linked_ind_roster_data_stage_name Name of linked roster data stage
    #' @param linked_ind_roster_data_hash Hash of linked roster data
    #' @param linked_ind_roster_variable_map Variable mappings for linked roster data
    #' @param linked_ind_roster_value_map Value mappings for linked roster data
    #' @param linked_ind_roster_variable_label Variable labels for linked roster data
    #' @param linked_ind_roster_value_label Value labels for linked roster data
    #' @param linked_ind_deaths_data Optional linked deaths dataframe
    #' @param linked_ind_deaths_data_stage_name Name of linked deaths data stage
    #' @param linked_ind_deaths_data_hash Hash of linked deaths data
    #' @param linked_ind_deaths_variable_map Variable mappings for linked deaths data
    #' @param linked_ind_deaths_value_map Value mappings for linked deaths data
    #' @param linked_ind_deaths_variable_label Variable labels for linked deaths data
    #' @param linked_ind_deaths_value_label Value labels for linked deaths data
    #' @return A new MortalityDataAnalytics object
    initialize = function(data,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "MortalityDataAnalytics",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          quality_schema = NULL,
                          linked_ind_roster_data = NULL,
                          linked_ind_roster_data_stage_name = NULL,
                          linked_ind_roster_data_hash = NULL,
                          linked_ind_roster_variable_map = NULL,
                          linked_ind_roster_value_map = NULL,
                          linked_ind_roster_variable_label = NULL,
                          linked_ind_roster_value_label = NULL,
                          linked_ind_deaths_data = NULL,
                          linked_ind_deaths_data_stage_name = NULL,
                          linked_ind_deaths_data_hash = NULL,
                          linked_ind_deaths_variable_map = NULL,
                          linked_ind_deaths_value_map = NULL,
                          linked_ind_deaths_variable_label = NULL,
                          linked_ind_deaths_value_label = NULL) {

      origin <- paste0(dataset_name, "$initialize")

      if (missing(data) || is.null(data)) {
        phr_error(
          origin  = origin,
          message = "The data parameter (household dataframe) is required to initialize MortalityDataAnalytics."
        )
      }

      # Pre-merge linked roster and deaths variable/value maps and labels into the
      # household maps BEFORE calling super$initialize. This ensures that when
      # super$initialize() calls generate_dap_from_schema(), self$variable_map
      # already contains all canonical-name mappings, including those for mortality
      # analysis indicators that may be defined in a linked dataset's variable_map.
      # Household data takes precedence: keys already present are never overwritten.
      merged_variable_map   <- variable_map   %||% list()
      merged_value_map      <- value_map      %||% list()
      merged_variable_label <- variable_label %||% list()
      merged_value_label    <- value_label    %||% list()

      for (linked_maps in list(
        list(
          variable_map   = linked_ind_roster_variable_map,
          value_map      = linked_ind_roster_value_map,
          variable_label = linked_ind_roster_variable_label,
          value_label    = linked_ind_roster_value_label
        ),
        list(
          variable_map   = linked_ind_deaths_variable_map,
          value_map      = linked_ind_deaths_value_map,
          variable_label = linked_ind_deaths_variable_label,
          value_label    = linked_ind_deaths_value_label
        )
      )) {
        if (!is.null(linked_maps$variable_map)) {
          for (key in names(linked_maps$variable_map)) {
            if (is.null(merged_variable_map[[key]])) {
              merged_variable_map[[key]] <- linked_maps$variable_map[[key]]
            }
          }
        }
        if (!is.null(linked_maps$value_map)) {
          for (key in names(linked_maps$value_map)) {
            if (is.null(merged_value_map[[key]])) {
              merged_value_map[[key]] <- linked_maps$value_map[[key]]
            }
          }
        }
        if (!is.null(linked_maps$variable_label)) {
          for (key in names(linked_maps$variable_label)) {
            if (is.null(merged_variable_label[[key]])) {
              merged_variable_label[[key]] <- linked_maps$variable_label[[key]]
            }
          }
        }
        if (!is.null(linked_maps$value_label)) {
          for (key in names(linked_maps$value_label)) {
            if (is.null(merged_value_label[[key]])) {
              merged_value_label[[key]] <- linked_maps$value_label[[key]]
            }
          }
        }
      }

      super$initialize(
        data = data,
        dap = dap,
        parent_data_object = parent_data_object,
        dataset_name = dataset_name,
        data_stage_name = data_stage_name,
        data_hash = data_hash,
        variable_map = merged_variable_map,
        value_map = merged_value_map,
        variable_label = merged_variable_label,
        value_label = merged_value_label,
        quality_schema = quality_schema
      )

      self$linked_ind_roster_data              <- linked_ind_roster_data
      self$linked_ind_roster_data_stage_name   <- linked_ind_roster_data_stage_name
      self$linked_ind_roster_data_hash         <- linked_ind_roster_data_hash
      self$linked_ind_roster_variable_map      <- linked_ind_roster_variable_map
      self$linked_ind_roster_value_map         <- linked_ind_roster_value_map
      self$linked_ind_roster_variable_label    <- linked_ind_roster_variable_label
      self$linked_ind_roster_value_label       <- linked_ind_roster_value_label

      self$linked_ind_deaths_data              <- linked_ind_deaths_data
      self$linked_ind_deaths_data_stage_name   <- linked_ind_deaths_data_stage_name
      self$linked_ind_deaths_data_hash         <- linked_ind_deaths_data_hash
      self$linked_ind_deaths_variable_map      <- linked_ind_deaths_variable_map
      self$linked_ind_deaths_value_map         <- linked_ind_deaths_value_map
      self$linked_ind_deaths_variable_label    <- linked_ind_deaths_variable_label
      self$linked_ind_deaths_value_label       <- linked_ind_deaths_value_label

      # Load per-dataset schemas for roster and deaths
      self$analysis_schema_roster <- self$default_analysis_schema_roster()
      self$analysis_schema_deaths <- self$default_analysis_schema_deaths()
      self$outputs_schema_roster  <- self$default_outputs_schema_roster()
      self$outputs_schema_deaths  <- self$default_outputs_schema_deaths()

      # Generate per-dataset analysis plans for roster and deaths
      self$data_analysis_plan_roster <- QuantDataAnalysisPlanLog$new(
        log_df   = NULL,
        log_name = "Quant Data Analysis Plan (Roster)"
      )
      self$data_analysis_plan_deaths <- QuantDataAnalysisPlanLog$new(
        log_df   = NULL,
        log_name = "Quant Data Analysis Plan (Deaths)"
      )

      if (!is.null(linked_ind_roster_data)) {
        self$generate_dap_from_schema_roster()
      }

      if (!is.null(linked_ind_deaths_data)) {
        self$generate_dap_from_schema_deaths()
      }

      # Create and store per-dataset survey designs for roster and deaths
      if (!is.null(linked_ind_roster_data)) {
        self$base_survey_design_roster <- phr_try(
          srvyr::as_survey_design(.data = linked_ind_roster_data, ids = 1),
          on_error = "warn",
          origin   = paste0(dataset_name, "$initialize"),
          hint     = "Could not create base (unweighted) survey design for roster data."
        )
        self$survey_design_roster <- private$.create_survey_design_for_dataset(
          data         = linked_ind_roster_data,
          variable_map = linked_ind_roster_variable_map %||% merged_variable_map,
          origin       = paste0(dataset_name, "$initialize")
        )
      }

      if (!is.null(linked_ind_deaths_data)) {
        self$base_survey_design_deaths <- phr_try(
          srvyr::as_survey_design(.data = linked_ind_deaths_data, ids = 1),
          on_error = "warn",
          origin   = paste0(dataset_name, "$initialize"),
          hint     = "Could not create base (unweighted) survey design for deaths data."
        )
        self$survey_design_deaths <- private$.create_survey_design_for_dataset(
          data         = linked_ind_deaths_data,
          variable_map = linked_ind_deaths_variable_map %||% merged_variable_map,
          origin       = paste0(dataset_name, "$initialize")
        )
      }

      msg_parts <- c()
      if (!is.null(linked_ind_roster_data)) msg_parts <- c(msg_parts, "roster")
      if (!is.null(linked_ind_deaths_data)) msg_parts <- c(msg_parts, "deaths")
      if (length(msg_parts) > 0) {
        phr_message(
          phr_txt(glue::glue(
            "MortalityDataAnalytics initialized with linked {paste(msg_parts, collapse=' and ')} data."
          ))
        )
      } else {
        phr_message(
          phr_txt(glue::glue("{dataset_name} initialized as MortalityDataAnalytics object."))
        )
      }
    },

    #' @description Load the default Mortality quality schema from template file
    #' @return A list of Mortality-specific quality checks
    default_quality_schema = function() {

      file <- system.file(
        "resources",
        "quality_schema_data_quality_mortality_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "quality_schema_data_quality_mortality_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- tryCatch(
        readxl::read_xlsx(file),
        error = function(e) {
          phr_warning(
            origin  = "MortalityDataAnalytics$default_quality_schema",
            message = phr_txt(glue::glue("Failed to read quality_schema_data_quality_mortality_template.xlsx: {e$message}"))
          )
          return(NULL)
        }
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      schema_with_metadata <- quality_table_to_schema(df)

      if (!is.null(schema_with_metadata) && !is.null(schema_with_metadata$checks)) {
        return(schema_with_metadata$checks)
      }

      return(list())
    },

    #' @description Load the default Mortality household outputs schema from template file
    #' @return A list of Mortality-specific outputs definitions for the household dataset
    default_outputs_schema = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_mortality_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_mortality_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "MortalityDataAnalytics$default_outputs_schema",
        hint     = "Check that outputs_schema_data_analytics_mortality_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    },

    #' @description Load the default Mortality household analysis schema from template file
    #' @return A tibble containing the Mortality household analysis schema
    default_analysis_schema = function() {

      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_mortality_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_mortality_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "MortalityDataAnalytics$default_analysis_schema",
        hint     = "Check that analysis_schema_quant_data_analysis_mortality_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description Load the default Mortality roster outputs schema from template file
    #' @return A list of outputs definitions for the linked roster dataset
    default_outputs_schema_roster = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_mortality_roster_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_mortality_roster_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "MortalityDataAnalytics$default_outputs_schema_roster",
        hint     = "Check that outputs_schema_data_analytics_mortality_roster_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    },

    #' @description Load the default Mortality deaths outputs schema from template file
    #' @return A list of outputs definitions for the linked deaths dataset
    default_outputs_schema_deaths = function() {

      file <- system.file(
        "resources",
        "outputs_schema_data_analytics_mortality_deaths_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "outputs_schema_data_analytics_mortality_deaths_template.xlsx")
        if (!file.exists(file)) {
          return(list())
        }
      }

      df <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "MortalityDataAnalytics$default_outputs_schema_deaths",
        hint     = "Check that outputs_schema_data_analytics_mortality_deaths_template.xlsx is a valid Excel file."
      )

      if (is.null(df) || nrow(df) == 0) {
        return(list())
      }

      outputs_table_to_schema(df)
    },

    #' @description Load the default Mortality roster analysis schema from template file
    #' @return A tibble containing the analysis schema for the linked roster dataset
    default_analysis_schema_roster = function() {

      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_mortality_roster_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_mortality_roster_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "MortalityDataAnalytics$default_analysis_schema_roster",
        hint     = "Check that analysis_schema_quant_data_analysis_mortality_roster_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description Load the default Mortality deaths analysis schema from template file
    #' @return A tibble containing the analysis schema for the linked deaths dataset
    default_analysis_schema_deaths = function() {

      file <- system.file(
        "resources",
        "analysis_schema_quant_data_analysis_mortality_deaths_template.xlsx",
        package = "iphRa"
      )

      if (!file.exists(file) || file == "") {
        file <- file.path("resources", "analysis_schema_quant_data_analysis_mortality_deaths_template.xlsx")
        if (!file.exists(file)) {
          return(tibble::tibble())
        }
      }

      schema_tbl <- phr_try(
        readxl::read_xlsx(file),
        on_error = "warn",
        origin   = "MortalityDataAnalytics$default_analysis_schema_deaths",
        hint     = "Check that analysis_schema_quant_data_analysis_mortality_deaths_template.xlsx is a valid Excel file."
      )

      if (is.null(schema_tbl) || nrow(schema_tbl) == 0) {
        return(tibble::tibble())
      }

      return(schema_tbl)
    },

    #' @description
    #' Generate the data analysis plan for the linked roster dataset.
    #'
    #' Uses \code{analysis_schema_roster} and \code{linked_ind_roster_variable_map}
    #' (falling back to the merged \code{variable_map}) to translate canonical variable
    #' names to actual column names present in \code{linked_ind_roster_data}, then
    #' stores the resulting plan in \code{data_analysis_plan_roster}.
    #'
    #' @return Invisibly returns self.
    generate_dap_from_schema_roster = function() {
      origin <- paste0(self$dataset_name, "$generate_dap_from_schema_roster")
      phr_message(origin, "Generating data_analysis_plan_roster from analysis_schema_roster...")

      phr_try({

        if (is.null(self$linked_ind_roster_data) || nrow(self$linked_ind_roster_data) == 0) {
          phr_warning(origin, "linked_ind_roster_data is not available; skipping roster DAP generation.")
          return(invisible(self))
        }

        if (is.null(self$analysis_schema_roster) || nrow(self$analysis_schema_roster) == 0) {
          phr_warning(origin, "analysis_schema_roster is empty; data_analysis_plan_roster will remain empty.")
          return(invisible(self))
        }

        result <- private$.build_dap_from_schema(
          analysis_schema = self$analysis_schema_roster,
          variable_map    = self$linked_ind_roster_variable_map %||% self$variable_map %||% list(),
          available_vars  = names(self$linked_ind_roster_data),
          dataset_label   = "roster"
        )

        self$data_analysis_plan_roster$log_df <- result$dap_df

        if (nrow(result$issues) > 0) {
          self$analysis_plan_issue_log <- dplyr::bind_rows(
            self$analysis_plan_issue_log,
            result$issues
          )
          phr_warning(origin, paste0(nrow(result$issues), " roster indicators skipped due to missing variables."))
        } else {
          phr_message(origin, "All roster schema indicators found and added to data_analysis_plan_roster.")
        }

      }, on_error = "warn", origin = origin,
         hint = "Ensure linked_ind_roster_data has the expected columns and analysis_schema_roster is valid.")

      invisible(self)
    },

    #' @description
    #' Generate the data analysis plan for the linked deaths dataset.
    #'
    #' Uses \code{analysis_schema_deaths} and \code{linked_ind_deaths_variable_map}
    #' (falling back to the merged \code{variable_map}) to translate canonical variable
    #' names to actual column names present in \code{linked_ind_deaths_data}, then
    #' stores the resulting plan in \code{data_analysis_plan_deaths}.
    #'
    #' @return Invisibly returns self.
    generate_dap_from_schema_deaths = function() {
      origin <- paste0(self$dataset_name, "$generate_dap_from_schema_deaths")
      phr_message(origin, "Generating data_analysis_plan_deaths from analysis_schema_deaths...")

      phr_try({

        if (is.null(self$linked_ind_deaths_data) || nrow(self$linked_ind_deaths_data) == 0) {
          phr_warning(origin, "linked_ind_deaths_data is not available; skipping deaths DAP generation.")
          return(invisible(self))
        }

        if (is.null(self$analysis_schema_deaths) || nrow(self$analysis_schema_deaths) == 0) {
          phr_warning(origin, "analysis_schema_deaths is empty; data_analysis_plan_deaths will remain empty.")
          return(invisible(self))
        }

        result <- private$.build_dap_from_schema(
          analysis_schema = self$analysis_schema_deaths,
          variable_map    = self$linked_ind_deaths_variable_map %||% self$variable_map %||% list(),
          available_vars  = names(self$linked_ind_deaths_data),
          dataset_label   = "deaths"
        )

        self$data_analysis_plan_deaths$log_df <- result$dap_df

        if (nrow(result$issues) > 0) {
          self$analysis_plan_issue_log <- dplyr::bind_rows(
            self$analysis_plan_issue_log,
            result$issues
          )
          phr_warning(origin, paste0(nrow(result$issues), " deaths indicators skipped due to missing variables."))
        } else {
          phr_message(origin, "All deaths schema indicators found and added to data_analysis_plan_deaths.")
        }

      }, on_error = "warn", origin = origin,
         hint = "Ensure linked_ind_deaths_data has the expected columns and analysis_schema_deaths is valid.")

      invisible(self)
    },

    #' @description
    #' Run quantitative analysis for all available mortality datasets.
    #'
    #' Analysis is run independently for each available dataset:
    #' \describe{
    #'   \item{household}{Always run using the inherited \code{analysis_schema} and
    #'     survey design from the household data. Results stored in
    #'     \code{analysis_results$household}.}
    #'   \item{roster}{Run when \code{linked_ind_roster_data} is available and
    #'     \code{data_analysis_plan_roster} contains at least one indicator. Results
    #'     stored in \code{analysis_results$roster}.}
    #'   \item{deaths}{Run when \code{linked_ind_deaths_data} is available and
    #'     \code{data_analysis_plan_deaths} contains at least one indicator. Results
    #'     stored in \code{analysis_results$deaths}.}
    #' }
    #'
    #' @return Invisibly returns self.
    run_analysis = function() {
      origin <- paste0(self$dataset_name, "$run_analysis")
      phr_message(origin, "Running mortality analysis for all available datasets...")

      phr_try({

        # --- Household analysis (base class logic)
        super$run_analysis()
        household_results        <- self$analysis_results
        self$analysis_results    <- list(household = household_results)

        # --- Roster analysis
        roster_dap_rows <- private$.dap_row_count(self$data_analysis_plan_roster)

        if (!is.null(self$linked_ind_roster_data) && roster_dap_rows > 0L) {

          phr_message(origin, "Running analysis for linked roster data using data_analysis_plan_roster...")

          roster_survey <- private$.create_survey_design_for_dataset(
            data         = self$linked_ind_roster_data,
            variable_map = self$variable_map,
            origin       = origin
          )

          if (!is.null(roster_survey)) {
            roster_sd_results <- phr_try(
              phr_calc_survey_from_plan(
                design        = roster_survey,
                analysis_plan = self$data_analysis_plan_roster$log_df
              ),
              on_error = "warn",
              origin   = origin,
              hint     = "Verify all variables exist in linked_ind_roster_data."
            )
            # survey_design carries the weighted/clustered design result; base carries
            # the same result set. phr_calc_survey_from_plan returns a plain list so
            # R's copy-on-modify semantics keep the two fields independent if mutated.
            self$analysis_results[["roster"]] <- list(
              survey_design = roster_sd_results,
              base          = roster_sd_results
            )
          }

        } else if (!is.null(self$linked_ind_roster_data)) {
          phr_message(origin, "Linked roster data present but data_analysis_plan_roster is empty. Skipping roster analysis.")
        }

        # --- Deaths analysis
        deaths_dap_rows <- private$.dap_row_count(self$data_analysis_plan_deaths)

        if (!is.null(self$linked_ind_deaths_data) && deaths_dap_rows > 0L) {

          phr_message(origin, "Running analysis for linked deaths data using data_analysis_plan_deaths...")

          deaths_survey <- private$.create_survey_design_for_dataset(
            data         = self$linked_ind_deaths_data,
            variable_map = self$variable_map,
            origin       = origin
          )

          if (!is.null(deaths_survey)) {
            deaths_sd_results <- phr_try(
              phr_calc_survey_from_plan(
                design        = deaths_survey,
                analysis_plan = self$data_analysis_plan_deaths$log_df
              ),
              on_error = "warn",
              origin   = origin,
              hint     = "Verify all variables exist in linked_ind_deaths_data."
            )
            # survey_design carries the weighted/clustered design result; base carries
            # the same result set. phr_calc_survey_from_plan returns a plain list so
            # R's copy-on-modify semantics keep the two fields independent if mutated.
            self$analysis_results[["deaths"]] <- list(
              survey_design = deaths_sd_results,
              base          = deaths_sd_results
            )
          }

        } else if (!is.null(self$linked_ind_deaths_data)) {
          phr_message(origin, "Linked deaths data present but data_analysis_plan_deaths is empty. Skipping deaths analysis.")
        }

        phr_message(
          origin,
          phr_txt(glue::glue(
            "Mortality analysis complete. Datasets analysed: {paste(names(self$analysis_results), collapse = ', ')}."
          ))
        )

      }, on_error = "warn", origin = origin)

      invisible(self)
    },

    #' @description
    #' Run all outputs for all available mortality datasets.
    #'
    #' Outputs are generated independently for each available dataset and stored
    #' in named sub-lists:
    #' \describe{
    #'   \item{\code{visualizations$household} / \code{tables$household}}{Household outputs
    #'     using \code{outputs_schema}.}
    #'   \item{\code{visualizations$roster} / \code{tables$roster}}{Roster outputs using
    #'     \code{outputs_schema_roster} (only when \code{linked_ind_roster_data} is set and
    #'     \code{outputs_schema_roster} is non-empty).}
    #'   \item{\code{visualizations$deaths} / \code{tables$deaths}}{Deaths outputs using
    #'     \code{outputs_schema_deaths} (only when \code{linked_ind_deaths_data} is set and
    #'     \code{outputs_schema_deaths} is non-empty).}
    #' }
    #'
    #' @param language Character string specifying the language for auto-generated
    #'   titles. One of \code{"english"} (default), \code{"french"}, or
    #'   \code{"arabic"}.
    #' @return Invisibly returns a list with \code{visualizations} and \code{tables}.
    run_outputs = function(language = "english") {
      origin <- paste0(self$dataset_name, "$run_outputs")

      phr_try({

        # --- Household outputs: call parent, then wrap in $household
        saved_viz        <- self$visualizations
        saved_tbl        <- self$tables
        self$visualizations <- list()
        self$tables         <- list()

        super$run_outputs(language = language)

        saved_viz[["household"]] <- self$visualizations
        saved_tbl[["household"]] <- self$tables

        self$visualizations <- saved_viz
        self$tables         <- saved_tbl

        # --- Roster outputs
        if (!is.null(self$linked_ind_roster_data) &&
            !is.null(self$outputs_schema_roster) &&
            length(self$outputs_schema_roster) > 0) {

          private$.run_outputs_for_namespace(
            data           = self$linked_ind_roster_data,
            outputs_schema = self$outputs_schema_roster,
            namespace      = "roster",
            survey_design  = self$survey_design_roster,
            language       = language
          )

        } else if (!is.null(self$linked_ind_roster_data)) {
          phr_message(phr_txt(glue::glue("{origin}: Linked roster data present but outputs_schema_roster is empty. Skipping roster outputs.")))
        }

        # --- Deaths outputs
        if (!is.null(self$linked_ind_deaths_data) &&
            !is.null(self$outputs_schema_deaths) &&
            length(self$outputs_schema_deaths) > 0) {

          private$.run_outputs_for_namespace(
            data           = self$linked_ind_deaths_data,
            outputs_schema = self$outputs_schema_deaths,
            namespace      = "deaths",
            survey_design  = self$survey_design_deaths,
            language       = language
          )

        } else if (!is.null(self$linked_ind_deaths_data)) {
          phr_message(phr_txt(glue::glue("{origin}: Linked deaths data present but outputs_schema_deaths is empty. Skipping deaths outputs.")))
        }

        phr_message(phr_txt(glue::glue(
          "{origin}: run_outputs complete. Namespaces: {paste(names(self$visualizations), collapse = ', ')}."
        )))

        invisible(list(visualizations = self$visualizations, tables = self$tables))

      }, on_error = "warn", origin = origin)
    }

  ),

  private = list(

    #' Return the number of rows in a QuantDataAnalysisPlanLog's log_df, or 0L.
    #'
    #' @param dap A QuantDataAnalysisPlanLog object (or NULL).
    #' @return Integer row count, or 0L when dap or its log_df is NULL.
    .dap_row_count = function(dap) {
      if (!is.null(dap) && !is.null(dap$log_df)) nrow(dap$log_df) else 0L
    },

    #' Create a proper survey design for an arbitrary data frame using variable_map.
    #'
    #' Applies the same logic as \code{create_survey_design()} (which operates on
    #' \code{self$data} and \code{self$variable_map}) but accepts any data frame and
    #' variable map. Reads \code{cluster_id_numeric} / \code{cluster_id},
    #' \code{weight}, \code{stratum}, and \code{fpc} from \code{variable_map} and
    #' builds an \code{srvyr} survey design accordingly. Falls back to a simple
    #' random sample (\code{ids = 1}) when none of those columns are present.
    #'
    #' @param data A data frame for the linked dataset.
    #' @param variable_map Named list mapping canonical names to actual column names.
    #' @param origin Character; caller label used in log/warning messages.
    #' @return An \code{srvyr} survey design object, or NULL on failure.
    .create_survey_design_for_dataset = function(data, variable_map, origin = NULL) {

      origin <- origin %||% paste0(self$dataset_name, "$.create_survey_design_for_dataset")

      if (is.null(data) || nrow(data) == 0) {
        phr_warning(origin, "No data available to create survey design for linked dataset.")
        return(NULL)
      }

      vm        <- variable_map %||% list()
      data_cols <- names(data)

      cluster_col <- vm[["cluster_id_numeric"]]
      if (is.null(cluster_col) || !cluster_col %in% data_cols) {
        cluster_col <- vm[["cluster_id"]]
      }
      if (is.null(cluster_col) || !cluster_col %in% data_cols) {
        cluster_col <- NULL
      }

      weight_col <- vm[["weight"]]
      if (is.null(weight_col) || !weight_col %in% data_cols) weight_col <- NULL

      strata_col <- vm[["stratum"]]
      if (is.null(strata_col) || !strata_col %in% data_cols) strata_col <- NULL

      fpc_col    <- vm[["fpc"]]
      if (is.null(fpc_col) || !fpc_col %in% data_cols) fpc_col <- NULL

      if (is.null(cluster_col)) {
        phr_message(origin, "No cluster column found for linked dataset; using ids = 1 (simple random sample design).")
      }

      ids_sym    <- if (!is.null(cluster_col)) rlang::sym(cluster_col) else 1
      strata_sym <- if (!is.null(strata_col))  rlang::sym(strata_col)  else NULL
      weight_sym <- if (!is.null(weight_col))  rlang::sym(weight_col)  else NULL
      fpc_sym    <- if (!is.null(fpc_col))     rlang::sym(fpc_col)     else NULL

      design <- phr_try(
        srvyr::as_survey_design(
          .data   = data,
          ids     = !!ids_sym,
          strata  = !!strata_sym,
          weights = !!weight_sym,
          fpc     = !!fpc_sym,
          nest    = TRUE
        ),
        on_error = "warn",
        origin   = origin,
        hint     = "Check that cluster_id_numeric (or cluster_id) and weight columns contain valid data in the linked dataset."
      )

      if (!is.null(design)) {
        phr_message(origin, "Survey design created successfully for linked dataset.")
      }

      design
    },

    #' Build a DAP tibble from an analysis schema, resolving canonical variable names.
    #'
    #' Translates each \code{var_name} and \code{denom_var} in \code{analysis_schema}
    #' through \code{variable_map}, then keeps only indicators whose resolved columns
    #' are present in \code{available_vars}.
    #'
    #' @param analysis_schema A tibble with analysis schema columns.
    #' @param variable_map Named list mapping canonical names to actual column names.
    #' @param available_vars Character vector of column names available in the target dataset.
    #' @param dataset_label Character label used in the returned issues tibble (e.g. "roster").
    #' @return A named list with elements \code{dap_df} (tibble) and \code{issues} (tibble).
    .build_dap_from_schema = function(analysis_schema, variable_map, available_vars,
                                       dataset_label = "dataset") {

      vm <- variable_map %||% list()

      translate_var <- function(canonical_name) {
        if (is.null(canonical_name) || is.na(canonical_name)) return(NA_character_)
        if (canonical_name %in% names(vm)) {
          actual <- vm[[canonical_name]]
          if (!is.null(actual) && nzchar(actual)) return(actual)
        }
        return(canonical_name)
      }

      schema_valid <- analysis_schema %>%
        dplyr::mutate(
          var_name_actual  = purrr::map_chr(.data$var_name,  translate_var),
          denom_var_actual = purrr::map_chr(.data$denom_var, translate_var),
          var_exists       = .data$var_name_actual %in% available_vars,
          denom_exists     = ifelse(
            !is.na(.data$denom_var_actual),
            .data$denom_var_actual %in% available_vars,
            TRUE
          )
        ) %>%
        dplyr::mutate(include = .data$var_exists & .data$denom_exists)

      issues <- schema_valid %>%
        dplyr::filter(!.data$include) %>%
        dplyr::transmute(
          dataset        = dataset_label,
          indicator_name = .data$indicator_name,
          issue = paste0(
            "Missing variable(s): ",
            ifelse(!.data$var_exists,
                   paste0(.data$var_name, " (maps to: ", .data$var_name_actual, ")"),
                   ""),
            ifelse(!.data$denom_exists & !is.na(.data$denom_var),
                   paste0(", ", .data$denom_var, " (maps to: ", .data$denom_var_actual, ")"),
                   "")
          )
        )

      dap_df <- schema_valid %>%
        dplyr::filter(.data$include) %>%
        dplyr::transmute(
          indicator_name = .data$indicator_name,
          calculation    = .data$calculation,
          var_name       = .data$var_name_actual,
          denom_var      = .data$denom_var_actual,
          disaggregation = .data$disaggregation,
          multiplier     = .data$multiplier,
          indicator_unit = .data$indicator_unit
        )

      list(dap_df = dap_df, issues = issues)
    },

    #' Run analysis for an arbitrary dataset using a given analysis schema.
    #'
    #' Creates a simple SRS survey design from \code{data}, builds a DAP from
    #' \code{analysis_schema} (resolving canonical names via \code{variable_map}),
    #' and runs \code{phr_calc_survey_from_plan}.
    #'
    #' @param data A data frame.
    #' @param analysis_schema A tibble with columns: indicator_name, calculation,
    #'   var_name, denom_var, disaggregation, multiplier, indicator_unit.
    #' @param variable_map Optional named list mapping canonical names to actual column names.
    #' @return A named list with elements \code{survey_design} and \code{base}, or NULLs.
    .run_analysis_for_dataset = function(data, analysis_schema, variable_map = NULL) {
      origin <- paste0(self$dataset_name, "$.run_analysis_for_dataset")

      if (is.null(data) || nrow(data) == 0) {
        phr_warning(origin, "No data available for linked-dataset analysis.")
        return(list(survey_design = NULL, base = NULL))
      }

      if (is.null(analysis_schema) || nrow(analysis_schema) == 0) {
        phr_warning(origin, "No analysis schema rows available for linked-dataset analysis.")
        return(list(survey_design = NULL, base = NULL))
      }

      survey_design <- phr_try(
        srvyr::as_survey_design(.data = data, ids = 1),
        on_error = "warn",
        origin   = origin,
        hint     = "Could not create simple survey design from linked dataset."
      )

      if (is.null(survey_design)) {
        return(list(survey_design = NULL, base = NULL))
      }

      available_vars <- names(survey_design$variables)
      vm             <- variable_map %||% list()

      translate_var <- function(canonical_name) {
        if (is.null(canonical_name) || is.na(canonical_name)) return(NA_character_)
        if (canonical_name %in% names(vm)) {
          actual <- vm[[canonical_name]]
          if (!is.null(actual) && nzchar(actual)) return(actual)
        }
        return(canonical_name)
      }

      schema_valid <- analysis_schema %>%
        dplyr::mutate(
          var_name_actual  = purrr::map_chr(.data$var_name,  translate_var),
          denom_var_actual = purrr::map_chr(.data$denom_var, translate_var),
          var_exists       = .data$var_name_actual %in% available_vars,
          denom_exists     = ifelse(
            !is.na(.data$denom_var_actual),
            .data$denom_var_actual %in% available_vars,
            TRUE
          )
        ) %>%
        dplyr::filter(.data$var_exists & .data$denom_exists)

      if (nrow(schema_valid) == 0) {
        phr_warning(origin, "No valid indicators found in analysis schema for linked dataset after variable matching.")
        return(list(survey_design = NULL, base = NULL))
      }

      dap_df <- schema_valid %>%
        dplyr::transmute(
          indicator_name = .data$indicator_name,
          calculation    = .data$calculation,
          var_name       = .data$var_name_actual,
          denom_var      = .data$denom_var_actual,
          disaggregation = .data$disaggregation,
          multiplier     = .data$multiplier,
          indicator_unit = .data$indicator_unit
        )

      survey_results <- phr_try(
        phr_calc_survey_from_plan(
          design        = survey_design,
          analysis_plan = dap_df
        ),
        on_error = "warn",
        origin   = origin,
        hint     = "Verify all variables exist in the linked dataset and that the analysis plan is valid."
      )

      list(survey_design = survey_results, base = survey_results)
    },

    #' Run outputs for an arbitrary dataset, storing results under a namespace sub-list.
    #'
    #' For each output defined in \code{outputs_schema}, the specified output function
    #' is called with the appropriate first positional argument (determined by
    #' \code{dataset_type} in the output entry) and any additional parameters
    #' resolved from \code{test_params}. Results are stored in
    #' \code{self$visualizations[[namespace]]} or \code{self$tables[[namespace]]}
    #' depending on \code{output_type}.
    #'
    #' Supported \code{dataset_type} values per output entry:
    #' \describe{
    #'   \item{"data"}{uses \code{data} as the first argument (default)}
    #'   \item{"survey_design"}{uses the pre-built \code{survey_design} object as the
    #'     first argument. If \code{survey_design} is \code{NULL}, outputs requesting
    #'     this type are skipped with a warning.}
    #' }
    #'
    #' @param data A data frame for the namespace dataset.
    #' @param outputs_schema A named list of output definitions (as produced by
    #'   \code{outputs_table_to_schema}).
    #' @param namespace Character; key used for the sub-list in \code{self$visualizations}
    #'   and \code{self$tables} (e.g., "roster" or "deaths").
    #' @param survey_design An srvyr survey design object for the namespace dataset,
    #'   used when an output entry has \code{dataset_type = "survey_design"}.
    #'   Should be pre-built and stored (e.g. \code{self$survey_design_roster}).
    #' @return Invisibly returns NULL.
    .run_outputs_for_namespace = function(data, outputs_schema, namespace,
                                          survey_design = NULL, language = "english") {
      origin <- paste0(self$dataset_name, "$.run_outputs_for_namespace[", namespace, "]")

      if (is.null(outputs_schema) || length(outputs_schema) == 0) {
        return(invisible(NULL))
      }

      phr_message(phr_txt(glue::glue(
        "Running {length(outputs_schema)} output(s) for '{namespace}' dataset..."
      )))

      self$visualizations[[namespace]] <- self$visualizations[[namespace]] %||% list()
      self$tables[[namespace]]         <- self$tables[[namespace]]         %||% list()

      for (out_name in names(outputs_schema)) {
        out <- outputs_schema[[out_name]]

        phr_try({

          func_name <- out$output_func_name
          if (is.null(func_name) || is.na(func_name) || !nzchar(func_name)) {
            phr_warning(
              message = phr_txt(glue::glue(
                "Output '{out_name}' in '{namespace}' schema has no output_func_name. Skipping."
              )),
              origin = origin
            )
            next
          }

          output_function <- NULL
          if (requireNamespace("iphRa", quietly = TRUE)) {
            tryCatch({
              ns_env <- asNamespace("iphRa")
              if (exists(func_name, envir = ns_env, mode = "function", inherits = FALSE)) {
                output_function <- get(func_name, envir = ns_env, mode = "function", inherits = FALSE)
              }
            }, error = function(e) NULL)
          }
          if (is.null(output_function)) {
            tryCatch({
              if (exists(func_name, mode = "function", inherits = TRUE)) {
                output_function <- get(func_name, mode = "function", inherits = TRUE)
              }
            }, error = function(e) NULL)
          }

          if (is.null(output_function)) {
            phr_warning(
              message = phr_txt(glue::glue(
                "Function '{func_name}' for '{namespace}' output '{out_name}' not found. Skipping."
              )),
              origin = origin
            )
            next
          }

          # Determine first positional argument from dataset_type.
          # "survey_design" – the pre-built survey design for this namespace dataset.
          # "data" (default) – the raw data frame.
          dataset_type <- if (!is.null(out$dataset_type) &&
                               !is.na(out$dataset_type) &&
                               nzchar(out$dataset_type)) {
            out$dataset_type
          } else "data"

          if (dataset_type == "survey_design") {
            if (is.null(survey_design)) {
              phr_warning(
                message = phr_txt(glue::glue(
                  "Output '{out_name}' in '{namespace}' requires dataset_type='survey_design' but ",
                  "no survey design is available for this dataset. Skipping."
                )),
                origin = origin
              )
              next
            }
            func_args <- list(survey_design)
          } else {
            func_args <- list(data)
          }

          if (!is.null(out$test_params) && length(out$test_params) > 0) {
            func_args <- private$.resolve_output_params(
              func_args          = func_args,
              test_params        = out$test_params,
              out_name           = out_name,
              skip_results_table = TRUE
            )
          }

          # Storage key: use output_name field; fall back to out_name (schema list key)
          label <- if (!is.null(out$output_name) && !is.na(out$output_name) && nzchar(out$output_name)) {
            out$output_name
          } else {
            out_name
          }

          # Auto-inject title_name unless already supplied in test_params
          if (is.null(func_args[["title_name"]])) {
            title_field <- switch(language,
              "french"  = "output_title_french",
              "arabic"  = "output_title_arabic",
              "output_title_english"
            )
            auto_title <- out[[title_field]]
            if (is.null(auto_title) || is.na(auto_title) || !nzchar(auto_title)) {
              auto_title <- out$output_title
            }
            if (!is.null(auto_title) && !is.na(auto_title) && nzchar(auto_title)) {
              func_args[["title_name"]] <- auto_title
            }
          }

          phr_message(phr_txt(glue::glue(
            "Calling {func_name} for '{namespace}' output '{out_name}'..."
          )))

          output_result <- do.call(output_function, func_args)

          if (!is.null(out$output_type) && out$output_type == "table") {
            self$tables[[namespace]][[label]] <- output_result
            phr_message(phr_txt(glue::glue("Table '{label}' stored in tables${namespace}.")))
          } else if (!is.null(out$output_type) && out$output_type == "visualization") {
            self$visualizations[[namespace]][[label]] <- output_result
            phr_message(phr_txt(glue::glue("Visualization '{label}' stored in visualizations${namespace}.")))
          } else {
            phr_warning(
              message = phr_txt(glue::glue(
                "Output '{out_name}' in '{namespace}' schema has unrecognized output_type ",
                "'{out$output_type}'. Expected 'visualization' or 'table'. Result not stored."
              )),
              origin = origin
            )
          }

        }, on_error = "warn",
           origin   = paste0(origin, "$", out_name))
      }

      invisible(NULL)
    }

  )
)
