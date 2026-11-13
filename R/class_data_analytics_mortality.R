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
#' named sub-list. This is driven by the \code{pre_run_analysis()} and
#' \code{pre_run_outputs()} hooks, which return one input set (data, schema, survey
#' designs, analysis plan) per available dataset; the inherited \code{run_analysis()}
#' and \code{run_outputs()} iterate over these sets.
#'
#' Linked datasets are read from their own Data objects, using
#' \code{get_data_with_fallback()} to select an available stage. Their actual
#' stages, hashes, mappings, and labels are retained independently of the
#' household data. Inspect private fields through \code{get(field = "..data")}
#' or \code{get(field = "..linked_ind_roster_data")}.
#'
#' @section Accessible fields:
#' * `linked_ind_roster_data`: Optional linked roster dataframe
#' * `linked_ind_roster_data_stage_name`: Name of linked roster data stage
#' * `linked_ind_roster_data_hash`: Hash of linked roster data
#' * `linked_ind_roster_variable_map`: Variable mappings for linked roster data
#' * `linked_ind_roster_value_map`: Value mappings for linked roster data
#' * `linked_ind_roster_variable_label`: Variable labels for linked roster data
#' * `linked_ind_roster_value_label`: Value labels for linked roster data
#' * `linked_ind_deaths_data`: Optional linked deaths dataframe
#' * `linked_ind_deaths_data_stage_name`: Name of linked deaths data stage
#' * `linked_ind_deaths_data_hash`: Hash of linked deaths data
#' * `linked_ind_deaths_variable_map`: Variable mappings for linked deaths data
#' * `linked_ind_deaths_value_map`: Value mappings for linked deaths data
#' * `linked_ind_deaths_variable_label`: Variable labels for linked deaths data
#' * `linked_ind_deaths_value_label`: Value labels for linked deaths data
#' * `analysis_schema_roster`: Analysis schema for linked roster dataset
#' * `analysis_schema_deaths`: Analysis schema for linked deaths dataset
#' * `outputs_schema_roster`: Outputs schema (data frame) for linked roster dataset
#' * `outputs_schema_deaths`: Outputs schema (data frame) for linked deaths dataset
#' * `data_analysis_plan_roster`: Data analysis plan for linked roster dataset
#' * `data_analysis_plan_deaths`: Data analysis plan for linked deaths dataset
#' * `survey_design_roster`: srvyr survey design object for the linked roster dataset
#' * `survey_design_deaths`: srvyr survey design object for the linked deaths dataset
#' * `base_survey_design_roster`: Unfiltered base srvyr survey design object for the linked roster dataset
#' * `base_survey_design_deaths`: Unfiltered base srvyr survey design object for the linked deaths dataset
#'
#' @seealso [DataAnalytics]
#' @export
MortalityDataAnalytics <- R6::R6Class(
  classname = "MortalityDataAnalytics",
  inherit = DataAnalytics,

  public = list(

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

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = dataset_name,
        message = private$..phr_txt("Started initialization for {dataset_name}.")
      )

      if (missing(data) || is.null(data)) {
        phrutils::phr_error(
          origin  = origin,
          message = private$..phr_txt("The data parameter (household dataframe) is required to initialize MortalityDataAnalytics.")
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

      # Step 2: Set fields
      result <- phrutils::phr_try_step(
        {
          self$set(field = "..linked_ind_roster_data", value = linked_ind_roster_data)
          self$set(field = "..linked_ind_roster_data_stage_name", value = linked_ind_roster_data_stage_name)
          self$set(field = "..linked_ind_roster_data_hash", value = linked_ind_roster_data_hash)
          self$set(field = "..linked_ind_roster_variable_map", value = linked_ind_roster_variable_map)
          self$set(field = "..linked_ind_roster_value_map", value = linked_ind_roster_value_map)
          self$set(field = "..linked_ind_roster_variable_label", value = linked_ind_roster_variable_label)
          self$set(field = "..linked_ind_roster_value_label", value = linked_ind_roster_value_label)

          self$set(field = "..linked_ind_deaths_data", value = linked_ind_deaths_data)
          self$set(field = "..linked_ind_deaths_data_stage_name", value = linked_ind_deaths_data_stage_name)
          self$set(field = "..linked_ind_deaths_data_hash", value = linked_ind_deaths_data_hash)
          self$set(field = "..linked_ind_deaths_variable_map", value = linked_ind_deaths_variable_map)
          self$set(field = "..linked_ind_deaths_value_map", value = linked_ind_deaths_value_map)
          self$set(field = "..linked_ind_deaths_variable_label", value = linked_ind_deaths_variable_label)
          self$set(field = "..linked_ind_deaths_value_label", value = linked_ind_deaths_value_label)

        },
                  step = private$..phr_txt("Set initial fields for DataAnalytics class"),
                  hint = private$..phr_txt("Check initial arguments passed for DataAnalytics")
      )
      if (phrutils::phr_failed(result)) {
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }

      # Step 3: Load quality, analysis, and outputs schema for mortality
      result <- phrutils::phr_try_step(
        {
          private$..initialize_load_schemas_mortality()
        },
                  step = private$..phr_txt("Set initial fields for DataAnalytics class"),
                  hint = private$..phr_txt("Check initial arguments passed for DataAnalytics")
      )
      if (phrutils::phr_failed(result)) {
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }

      # Step 4: Create initial data analysis plans for roster and deaths
      result <- phrutils::phr_try_step(
        {
          # Generate per-dataset analysis plans for roster and deaths
          private$..data_analysis_plan_roster <- QuantDataAnalysisPlanLog$new(
            log_df   = NULL,
            log_name = private$..phr_txt("Quant Data Analysis Plan (Roster)")
          )
          private$..data_analysis_plan_deaths <- QuantDataAnalysisPlanLog$new(
            log_df   = NULL,
            log_name = private$..phr_txt("Quant Data Analysis Plan (Deaths)")
          )
          # Build the DAP for roster dataset from available columns and analysis_schema_roster
          if (!is.null(linked_ind_roster_data)) {

            result <- private$..build_dap_from_schema(
              analysis_schema = private$..analysis_schema_roster,
              variable_map    = private$..linked_ind_roster_variable_map %||% list(),
              available_vars  = names(private$..linked_ind_roster_data),
              dataset_label   = "roster"
            )

            private$..data_analysis_plan_roster$set(field = "..log_df", value = result$dap_df)

          }

          # Build the DAP for deaths dataset from available columns and analysis_schema_deaths
          if (!is.null(linked_ind_deaths_data)) {

            result <- private$..build_dap_from_schema(
              analysis_schema = private$..analysis_schema_deaths,
              variable_map    = private$..linked_ind_deaths_variable_map %||% list(),
              available_vars  = names(private$..linked_ind_deaths_data),
              dataset_label   = "deaths"
            )

            private$..data_analysis_plan_deaths$set(field = "..log_df", value = result$dap_df)

          }
        },
                    step = private$..phr_txt("Create initial roster and death specific data analysis plans"),
                    hint = private$..phr_txt("Check analysis schemas for roster and deaths.")
      )
      if (phrutils::phr_failed(result)) {
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }

      # Step 5: Create survey design objects for roster and deaths
      result <- phrutils::phr_try_step(
        {
          # Create and store per-dataset survey designs for roster and deaths
          if (!is.null(linked_ind_roster_data)) {

            self$set(field = "..base_survey_design_roster", value = srvyr::as_survey_design(.data = linked_ind_roster_data, ids = 1))
            self$set(field = "..survey_design_roster",
                    value = private$..create_survey_design_for_dataset(
                      data         = linked_ind_roster_data,
                      variable_map = linked_ind_roster_variable_map %||% list(),
                      origin       = paste0(dataset_name, "$initialize")
            ))
          }

          if (!is.null(linked_ind_deaths_data)) {

            self$set(field = "..base_survey_design_deaths", value = srvyr::as_survey_design(.data = linked_ind_deaths_data, ids = 1))

            self$set(field = "..survey_design_deaths",
                     value = private$..create_survey_design_for_dataset(
                       data         = linked_ind_deaths_data,
                       variable_map = linked_ind_deaths_variable_map %||% list(),
                       origin       = paste0(dataset_name, "$initialize")
                     ))
            }
        },
        step = private$..phr_txt("Create initial roster and death specific data analysis plans"),
        hint = private$..phr_txt("Check analysis schemas for roster and deaths.")
      )
      if (phrutils::phr_failed(result)) {
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }

      msg_parts <- c()
      if (!is.null(linked_ind_roster_data)) msg_parts <- c(msg_parts, "roster")
      if (!is.null(linked_ind_deaths_data)) msg_parts <- c(msg_parts, "deaths")
      if (length(msg_parts) > 0) {
        msg <- private$..phr_txt("MortalityDataAnalytics initialized with linked {paste(msg_parts, collapse=' and ')} data.")
      } else {
        msg <- private$..phr_txt("{dataset_name} initialized as MortalityDataAnalytics object.")
      }

      private$..touch(
        log = TRUE,
        touch = TRUE,
        name = dataset_name,
        message = paste0(origin, msg)
      )

    }
  ),

  private = list(

    # Fields for linked roster data
    ..linked_ind_roster_data = NULL,
    ..linked_ind_roster_data_stage_name = NULL,
    ..linked_ind_roster_data_hash = NULL,
    ..linked_ind_roster_variable_map = NULL,
    ..linked_ind_roster_value_map = NULL,
    ..linked_ind_roster_variable_label = NULL,
    ..linked_ind_roster_value_label = NULL,

    # Fields for linked deaths data
    ..linked_ind_deaths_data = NULL,
    ..linked_ind_deaths_data_stage_name = NULL,
    ..linked_ind_deaths_data_hash = NULL,
    ..linked_ind_deaths_variable_map = NULL,
    ..linked_ind_deaths_value_map = NULL,
    ..linked_ind_deaths_variable_label = NULL,
    ..linked_ind_deaths_value_label = NULL,

    # Per-dataset schemas for roster and deaths
    ..analysis_schema_roster = NULL,
    ..analysis_schema_deaths = NULL,
    ..outputs_schema_roster  = NULL,
    ..outputs_schema_deaths  = NULL,

    # Per-dataset data analysis plans
    ..data_analysis_plan_roster = NULL,
    ..data_analysis_plan_deaths = NULL,

    # Per-dataset survey design objects
    ..survey_design_roster = NULL,
    ..survey_design_deaths = NULL,
    ..base_survey_design_roster = NULL,
    ..base_survey_design_deaths = NULL,

    # @field ..default_quality_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_quality_schema_filename = c(
      "resources",
      "quality_schema_data_quality_mortality_template.xlsx"
    ),

    # @field ..default_outputs_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_outputs_schema_filename = c(
      "resources",
      "outputs_schema_data_analytics_mortality_template.xlsx"
    ),

    # @field ..default_analysis_schema_filename Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_analysis_schema_filename = c(
      "resources",
      "analysis_schema_quant_data_analysis_mortality_template.xlsx"
    ),

    # @field ..default_outputs_schema_roster_filename Default file name for outputs schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_outputs_schema_roster_filename = c(
      "resources",
      "outputs_schema_data_analytics_mortality_roster_template.xlsx"
    ),

    # @field ..default_analysis_schema_roster_filename Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_analysis_schema_roster_filename = c(
      "resources",
      "analysis_schema_quant_data_analysis_mortality_roster_template.xlsx"
    ),

    # @field ..default_outputs_schema_deaths_filename Default file name for outputs schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_outputs_schema_deaths_filename = c(
      "resources",
      "outputs_schema_data_analytics_mortality_deaths_template.xlsx"
    ),

    # @field ..default_analysis_schema_deaths_filename Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_analysis_schema_deaths_filename = c(
      "resources",
      "analysis_schema_quant_data_analysis_mortality_deaths_template.xlsx"
    ),

    # Additional Accessible Fields Hook
    #
    # @description
    # Returns subclass-specific fields accessible via `get()`.
    #
    # @return Character vector of field names.
    ..additional_get_fields = function() {
      c(
        super$..additional_get_fields(),
        # Fields for linked roster data
        "..linked_ind_roster_data",
        "..linked_ind_roster_data_stage_name",
        "..linked_ind_roster_data_hash",
        "..linked_ind_roster_variable_map",
        "..linked_ind_roster_value_map",
        "..linked_ind_roster_variable_label",
        "..linked_ind_roster_value_label",

        # Fields for linked deaths data
        "..linked_ind_deaths_data",
        "..linked_ind_deaths_data_stage_name",
        "..linked_ind_deaths_data_hash",
        "..linked_ind_deaths_variable_map",
        "..linked_ind_deaths_value_map",
        "..linked_ind_deaths_variable_label",
        "..linked_ind_deaths_value_label",

        # Per-dataset schemas for roster and deaths
        "..analysis_schema_roster",
        "..analysis_schema_deaths",
        "..outputs_schema_roster",
        "..outputs_schema_deaths",

        # Per-dataset data analysis plans
        "..data_analysis_plan_roster",
        "..data_analysis_plan_deaths",

        # Per-dataset survey design objects
        "..survey_design_roster",
        "..survey_design_deaths",
        "..base_survey_design_roster",
        "..base_survey_design_deaths"
      )
    },

    # Additional Settable Fields Hook
    #
    # @description
    # Returns subclass-specific fields that may be modified via `set()`.
    #
    # @return Character vector of field names.
    ..additional_set_fields = function() {
      c(
        super$..additional_set_fields(),
        # Fields for linked roster data
        "..linked_ind_roster_data",
        "..linked_ind_roster_data_stage_name",
        "..linked_ind_roster_data_hash",
        "..linked_ind_roster_variable_map",
        "..linked_ind_roster_value_map",
        "..linked_ind_roster_variable_label",
        "..linked_ind_roster_value_label",

        # Fields for linked deaths data
        "..linked_ind_deaths_data",
        "..linked_ind_deaths_data_stage_name",
        "..linked_ind_deaths_data_hash",
        "..linked_ind_deaths_variable_map",
        "..linked_ind_deaths_value_map",
        "..linked_ind_deaths_variable_label",
        "..linked_ind_deaths_value_label",

        # Per-dataset schemas for roster and deaths
        "..analysis_schema_roster",
        "..analysis_schema_deaths",
        "..outputs_schema_roster",
        "..outputs_schema_deaths",

        # Per-dataset data analysis plans
        "..data_analysis_plan_roster",
        "..data_analysis_plan_deaths",

        # Per-dataset survey design objects
        "..survey_design_roster",
        "..survey_design_deaths",
        "..base_survey_design_roster",
        "..base_survey_design_deaths"
      )
    },

    # Initialize Default Schemas
    #
    # @description
    # Loads the default quality, analysis, and outputs schemas into
    # their corresponding private fields.
    #
    # @return Invisibly returns TRUE.
    ..initialize_load_schemas_mortality = function() {

      private$..load_default_schema(
        filename = private$..default_analysis_schema_roster_filename,
        target_field = "..analysis_schema_roster",
        required_cols = c(
          "indicator_name",
          "calculation",
          "var_name",
          "denom_var",
          "disaggregation",
          "multiplier",
          "indicator_unit"
        ),
        schema_name = "analysis_roster",
        dataset_name = private$..dataset_name
      )

      private$..load_default_schema(
        filename = private$..default_outputs_schema_roster_filename,
        target_field = "..outputs_schema_roster",
        required_cols = c(
          "output_title",
          "output_name",
          "output_subtitle",
          "variables",
          "disaggregation",
          "output_func_name",
          "test_params",
          "output_type",
          "outputs_group",
          "outputs_per_group",
          "dataset_type",
          "output_title_english",
          "output_title_french",
          "output_title_arabic"
        ),
        schema_name = "outputs_roster",
        dataset_name = private$..dataset_name
      )

      private$..load_default_schema(
        filename = private$..default_analysis_schema_deaths_filename,
        target_field = "..analysis_schema_deaths",
        required_cols = c(
          "indicator_name",
          "calculation",
          "var_name",
          "denom_var",
          "disaggregation",
          "multiplier",
          "indicator_unit"
        ),
        schema_name = "analysis_deaths",
        dataset_name = private$..dataset_name
      )

      private$..load_default_schema(
        filename = private$..default_outputs_schema_deaths_filename,
        target_field = "..outputs_schema_deaths",
        required_cols = c(
          "output_title",
          "output_name",
          "output_subtitle",
          "variables",
          "disaggregation",
          "output_func_name",
          "test_params",
          "output_type",
          "outputs_group",
          "outputs_per_group",
          "dataset_type",
          "output_title_english",
          "output_title_french",
          "output_title_arabic"
        ),
        schema_name = "outputs_deaths",
        dataset_name = private$..dataset_name
      )

      invisible(TRUE)
    },

    # @description
    # Pre-hook providing the input sets used by the inherited \code{run_analysis()}.
    #
    # Returns one input set per available mortality dataset:
    # \describe{
    #   \item{household}{Always included, using the core \code{data},
    #     \code{survey_design} and \code{data_analysis_plan} fields. Results
    #     stored in \code{analysis_results$household}.}
    #   \item{roster}{Included when \code{linked_ind_roster_data} is available and
    #     \code{data_analysis_plan_roster} contains at least one indicator. Results
    #     stored in \code{analysis_results$roster}.}
    #   \item{deaths}{Included when \code{linked_ind_deaths_data} is available and
    #     \code{data_analysis_plan_deaths} contains at least one indicator. Results
    #     stored in \code{analysis_results$deaths}.}
    # }
    #
    # @return A named list of field sets.
    ..pre_run_analysis = function() {
      origin <- paste0(private$..dataset_name, "$pre_run_analysis")

      sets <- super$..pre_run_analysis()

      # sets <- list(
      #   household = list(
      #     data = "data",
      #     survey_design = "survey_design",
      #     data_analysis_plan = "data_analysis_plan"
      #   )
      # )

      if (!is.null(private$..linked_ind_roster_data)) {
        if (!is.null(private$..data_analysis_plan_roster$get(field = "..log_df"))) {
          sets$roster <- list(
            data = "..linked_ind_roster_data",
            survey_design = "..survey_design_roster",
            data_analysis_plan = "..data_analysis_plan_roster"
          )
        } else {
          phrutils::phr_message(origin, private$..phr_txt("Linked roster data present but data_analysis_plan_roster is empty. Skipping roster analysis."))
        }
      }

      if (!is.null(private$..linked_ind_deaths_data)) {
        if (!is.null(private$..data_analysis_plan_deaths$get(field = "..log_df"))) {
          sets$deaths <- list(
            data = "..linked_ind_deaths_data",
            survey_design = "..survey_design_deaths",
            data_analysis_plan = "..data_analysis_plan_deaths"
          )
        } else {
          phrutils::phr_message(origin, private$..phr_txt("Linked deaths data present but data_analysis_plan_deaths is empty. Skipping deaths analysis."))
        }
      }

      sets
    },

    # @description
    # Pre-hook providing the input sets used by the inherited \code{run_outputs()}.
    #
    # Returns one input set per available mortality dataset so that outputs are
    # generated independently for each dataset and stored in named sub-lists
    # (\code{visualizations$household} / \code{tables$household},
    # \code{visualizations$roster} / \code{tables$roster},
    # \code{visualizations$deaths} / \code{tables$deaths}). Each set passes
    # the appropriate survey design objects for the dataset, so output entries
    # with \code{dataset_type = "survey_design"} receive the weighted design and
    # \code{"base"} entries receive the unweighted base design. Each set also
    # names the dataset's own variable map, so \code{@variable_map$} references
    # in \code{test_params} resolve against the correct dataset.
    #
    # @return A named list of field sets.
    ..pre_run_outputs = function() {
      origin <- paste0(private$..dataset_name, "$pre_run_outputs")

      sets <- super$..pre_run_outputs()

      # sets <- list(
      #   household = list(
      #     data = "data",
      #     outputs_schema = "..outputs_schema",
      #     base_survey_design = "..base_survey_design",
      #     survey_design = "..survey_design",
      #     variable_map = "..variable_map"
      #   )
      # )

      if (!is.null(private$..linked_ind_roster_data)) {
        if (!is.null(private$..outputs_schema_roster) && nrow(private$..outputs_schema_roster) > 0) {
          sets$roster <- list(
            data = "..linked_ind_roster_data",
            outputs_schema = "..outputs_schema_roster",
            base_survey_design = "..base_survey_design_roster",
            survey_design = "..survey_design_roster",
            variable_map = "..linked_ind_roster_variable_map",
            value_map = "..linked_ind_roster_value_map",
            variable_label = "..linked_ind_roster_variable_label",
            value_label = "..linked_ind_roster_value_label"
          )
        } else {
          phrutils::phr_message(origin, "Linked roster data present but outputs_schema_roster is empty. Skipping roster outputs.")
        }
      }

      if (!is.null(private$..linked_ind_deaths_data)) {
        if (!is.null(private$..outputs_schema_deaths) && nrow(private$..outputs_schema_deaths) > 0) {
          sets$deaths <- list(
            data = "..linked_ind_deaths_data",
            outputs_schema = "..outputs_schema_deaths",
            base_survey_design = "..base_survey_design_deaths",
            survey_design = "..survey_design_deaths",
            variable_map = "..linked_ind_deaths_variable_map",
            value_map = "..linked_ind_deaths_value_map",
            variable_label = "..linked_ind_deaths_variable_label",
            value_label = "..linked_ind_deaths_value_label"
          )
        } else {
          phrutils::phr_message(origin, "Linked deaths data present but outputs_schema_deaths is empty. Skipping deaths outputs.")
        }
      }

      sets
    }

  )
)
