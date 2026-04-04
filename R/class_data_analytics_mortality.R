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
#' @field linked_ind_deaths_data Optional linked deaths dataframe
#' @field linked_ind_deaths_data_stage_name Name of linked deaths data stage
#' @field linked_ind_deaths_data_hash Hash of linked deaths data
#' @field linked_ind_deaths_variable_map Variable mappings for linked deaths data
#' @field linked_ind_deaths_value_map Value mappings for linked deaths data
#' @field analysis_schema_roster Analysis schema for linked roster dataset
#' @field analysis_schema_deaths Analysis schema for linked deaths dataset
#' @field outputs_schema_roster Outputs schema for linked roster dataset
#' @field outputs_schema_deaths Outputs schema for linked deaths dataset
#'
#' @seealso [DataAnalytics], [MortalityDataQuality], [MortalityAnalysis]
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

    # Fields for linked deaths data
    linked_ind_deaths_data = NULL,
    linked_ind_deaths_data_stage_name = NULL,
    linked_ind_deaths_data_hash = NULL,
    linked_ind_deaths_variable_map = NULL,
    linked_ind_deaths_value_map = NULL,

    # Per-dataset schemas for roster and deaths
    analysis_schema_roster = NULL,
    analysis_schema_deaths = NULL,
    outputs_schema_roster  = NULL,
    outputs_schema_deaths  = NULL,

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
    #' @param linked_ind_deaths_data Optional linked deaths dataframe
    #' @param linked_ind_deaths_data_stage_name Name of linked deaths data stage
    #' @param linked_ind_deaths_data_hash Hash of linked deaths data
    #' @param linked_ind_deaths_variable_map Variable mappings for linked deaths data
    #' @param linked_ind_deaths_value_map Value mappings for linked deaths data
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
                          linked_ind_deaths_data = NULL,
                          linked_ind_deaths_data_stage_name = NULL,
                          linked_ind_deaths_data_hash = NULL,
                          linked_ind_deaths_variable_map = NULL,
                          linked_ind_deaths_value_map = NULL) {

      origin <- paste0(dataset_name, "$initialize")

      if (missing(data) || is.null(data)) {
        phr_error(
          origin  = origin,
          message = "A household dataframe is required to initialize MortalityDataAnalytics."
        )
      }

      super$initialize(
        data = data,
        dap = dap,
        parent_data_object = parent_data_object,
        dataset_name = dataset_name,
        data_stage_name = data_stage_name,
        data_hash = data_hash,
        variable_map = variable_map,
        value_map = value_map,
        variable_label = variable_label,
        value_label = value_label,
        quality_schema = quality_schema
      )

      self$linked_ind_roster_data              <- linked_ind_roster_data
      self$linked_ind_roster_data_stage_name   <- linked_ind_roster_data_stage_name
      self$linked_ind_roster_data_hash         <- linked_ind_roster_data_hash
      self$linked_ind_roster_variable_map      <- linked_ind_roster_variable_map
      self$linked_ind_roster_value_map         <- linked_ind_roster_value_map

      self$linked_ind_deaths_data              <- linked_ind_deaths_data
      self$linked_ind_deaths_data_stage_name   <- linked_ind_deaths_data_stage_name
      self$linked_ind_deaths_data_hash         <- linked_ind_deaths_data_hash
      self$linked_ind_deaths_variable_map      <- linked_ind_deaths_variable_map
      self$linked_ind_deaths_value_map         <- linked_ind_deaths_value_map

      # Load per-dataset schemas for roster and deaths
      self$analysis_schema_roster <- self$default_analysis_schema_roster()
      self$analysis_schema_deaths <- self$default_analysis_schema_deaths()
      self$outputs_schema_roster  <- self$default_outputs_schema_roster()
      self$outputs_schema_deaths  <- self$default_outputs_schema_deaths()

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
    #' Run quantitative analysis for all available mortality datasets.
    #'
    #' Analysis is run independently for each available dataset:
    #' \describe{
    #'   \item{household}{Always run using the inherited \code{analysis_schema} and
    #'     survey design from the household data. Results stored in
    #'     \code{analysis_results$household}.}
    #'   \item{roster}{Run when \code{linked_ind_roster_data} is available and
    #'     \code{analysis_schema_roster} contains at least one indicator. Results
    #'     stored in \code{analysis_results$roster}.}
    #'   \item{deaths}{Run when \code{linked_ind_deaths_data} is available and
    #'     \code{analysis_schema_deaths} contains at least one indicator. Results
    #'     stored in \code{analysis_results$deaths}.}
    #' }
    #'
    #' @return Invisibly returns self.
    run_analysis = function() {
      origin <- paste0(self$dataset_name, "$run_analysis")
      phr_message(origin, "Running mortality analysis for all available datasets...")

      phr_try({

        # --- Household analysis (base class logic) ----------------------------
        super$run_analysis()
        household_results        <- self$analysis_results
        self$analysis_results    <- list(household = household_results)

        # --- Roster analysis --------------------------------------------------
        if (!is.null(self$linked_ind_roster_data) &&
            !is.null(self$analysis_schema_roster) &&
            nrow(self$analysis_schema_roster) > 0) {

          phr_message(origin, "Running analysis for linked roster data...")
          roster_results <- private$.run_analysis_for_dataset(
            data            = self$linked_ind_roster_data,
            analysis_schema = self$analysis_schema_roster,
            variable_map    = self$linked_ind_roster_variable_map %||% self$variable_map
          )
          self$analysis_results[["roster"]] <- roster_results

        } else if (!is.null(self$linked_ind_roster_data)) {
          phr_message(origin, "Linked roster data present but analysis_schema_roster is empty. Skipping roster analysis.")
        }

        # --- Deaths analysis --------------------------------------------------
        if (!is.null(self$linked_ind_deaths_data) &&
            !is.null(self$analysis_schema_deaths) &&
            nrow(self$analysis_schema_deaths) > 0) {

          phr_message(origin, "Running analysis for linked deaths data...")
          deaths_results <- private$.run_analysis_for_dataset(
            data            = self$linked_ind_deaths_data,
            analysis_schema = self$analysis_schema_deaths,
            variable_map    = self$linked_ind_deaths_variable_map %||% self$variable_map
          )
          self$analysis_results[["deaths"]] <- deaths_results

        } else if (!is.null(self$linked_ind_deaths_data)) {
          phr_message(origin, "Linked deaths data present but analysis_schema_deaths is empty. Skipping deaths analysis.")
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
    #' @return Invisibly returns a list with \code{visualizations} and \code{tables}.
    run_outputs = function() {
      origin <- paste0(self$dataset_name, "$run_outputs")

      phr_try({

        # --- Household outputs: call parent, then wrap in $household ----------
        saved_viz        <- self$visualizations
        saved_tbl        <- self$tables
        self$visualizations <- list()
        self$tables         <- list()

        super$run_outputs()

        saved_viz[["household"]] <- self$visualizations
        saved_tbl[["household"]] <- self$tables

        self$visualizations <- saved_viz
        self$tables         <- saved_tbl

        # --- Roster outputs ---------------------------------------------------
        if (!is.null(self$linked_ind_roster_data) &&
            !is.null(self$outputs_schema_roster) &&
            length(self$outputs_schema_roster) > 0) {

          private$.run_outputs_for_namespace(
            data           = self$linked_ind_roster_data,
            outputs_schema = self$outputs_schema_roster,
            namespace      = "roster"
          )

        } else if (!is.null(self$linked_ind_roster_data)) {
          phr_message(phr_txt(glue::glue("{origin}: Linked roster data present but outputs_schema_roster is empty. Skipping roster outputs.")))
        }

        # --- Deaths outputs ---------------------------------------------------
        if (!is.null(self$linked_ind_deaths_data) &&
            !is.null(self$outputs_schema_deaths) &&
            length(self$outputs_schema_deaths) > 0) {

          private$.run_outputs_for_namespace(
            data           = self$linked_ind_deaths_data,
            outputs_schema = self$outputs_schema_deaths,
            namespace      = "deaths"
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
    #' is called with \code{data} as the first positional argument and any additional
    #' parameters resolved from \code{test_params}. Results are stored in
    #' \code{self$visualizations[[namespace]]} or \code{self$tables[[namespace]]}
    #' depending on \code{output_type}.
    #'
    #' @param data A data frame to pass as the first argument to each output function.
    #' @param outputs_schema A named list of output definitions (as produced by
    #'   \code{outputs_table_to_schema}).
    #' @param namespace Character; key used for the sub-list in \code{self$visualizations}
    #'   and \code{self$tables} (e.g., "roster" or "deaths").
    #' @return Invisibly returns NULL.
    .run_outputs_for_namespace = function(data, outputs_schema, namespace) {
      origin <- paste0(self$dataset_name, "$.run_outputs_for_namespace[", namespace, "]")

      if (is.null(outputs_schema) || length(outputs_schema) == 0) {
        return(invisible(NULL))
      }

      phr_message(phr_txt(glue::glue(
        "Running {length(outputs_schema)} output(s) for '{namespace}' dataset..."
      )))

      if (is.null(self$visualizations[[namespace]])) self$visualizations[[namespace]] <- list()
      if (is.null(self$tables[[namespace]]))         self$tables[[namespace]]         <- list()

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

          func_args <- list(data)

          if (!is.null(out$test_params) && length(out$test_params) > 0) {
            func_args <- private$.resolve_output_params(
              func_args          = func_args,
              test_params        = out$test_params,
              out_name           = out_name,
              skip_results_table = TRUE
            )
          }

          label <- if (!is.null(out$output_title) && !is.na(out$output_title) && nzchar(out$output_title)) {
            out$output_title
          } else {
            out_name
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
