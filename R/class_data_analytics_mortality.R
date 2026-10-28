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
#' @field outputs_schema_roster Outputs schema (data frame) for linked roster dataset
#' @field outputs_schema_deaths Outputs schema (data frame) for linked deaths dataset
#' @field data_analysis_plan_roster Data analysis plan for linked roster dataset
#' @field data_analysis_plan_deaths Data analysis plan for linked deaths dataset
#' @field survey_design_roster srvyr survey design object for the linked roster dataset
#' @field survey_design_deaths srvyr survey design object for the linked deaths dataset
#' @field base_survey_design_roster Unfiltered base srvyr survey design object for the linked roster dataset
#' @field base_survey_design_deaths Unfiltered base srvyr survey design object for the linked deaths dataset
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

      if (missing(data) || is.null(data)) {
        phrutils::phr_error(
          origin  = origin,
          message = phrutils::phr_txt("The data parameter (household dataframe) is required to initialize MortalityDataAnalytics.")
        )
      }

      # Pre-merge linked roster and deaths variable/value maps and labels into the
      # household maps BEFORE calling super$initialize. This ensures that when
      # super$initialize() calls generate_dap_from_schema(), private$..variable_map
      # already contains all canonical-name mappings, including those for mortality
      # analysis indicators that may be defined in a linked dataset's variable_map.
      # Household data takes precedence: keys already present are never overwritten.
      # merged_variable_map   <- variable_map   %||% list()
      # merged_value_map      <- value_map      %||% list()
      # merged_variable_label <- variable_label %||% list()
      # merged_value_label    <- value_label    %||% list()
      #
      # for (linked_maps in list(
      #   list(
      #     variable_map   = linked_ind_roster_variable_map,
      #     value_map      = linked_ind_roster_value_map,
      #     variable_label = linked_ind_roster_variable_label,
      #     value_label    = linked_ind_roster_value_label
      #   ),
      #   list(
      #     variable_map   = linked_ind_deaths_variable_map,
      #     value_map      = linked_ind_deaths_value_map,
      #     variable_label = linked_ind_deaths_variable_label,
      #     value_label    = linked_ind_deaths_value_label
      #   )
      # )) {
      #   if (!is.null(linked_maps$variable_map)) {
      #     for (key in names(linked_maps$variable_map)) {
      #       if (is.null(merged_variable_map[[key]])) {
      #         merged_variable_map[[key]] <- linked_maps$variable_map[[key]]
      #       }
      #     }
      #   }
      #   if (!is.null(linked_maps$value_map)) {
      #     for (key in names(linked_maps$value_map)) {
      #       if (is.null(merged_value_map[[key]])) {
      #         merged_value_map[[key]] <- linked_maps$value_map[[key]]
      #       }
      #     }
      #   }
      #   if (!is.null(linked_maps$variable_label)) {
      #     for (key in names(linked_maps$variable_label)) {
      #       if (is.null(merged_variable_label[[key]])) {
      #         merged_variable_label[[key]] <- linked_maps$variable_label[[key]]
      #       }
      #     }
      #   }
      #   if (!is.null(linked_maps$value_label)) {
      #     for (key in names(linked_maps$value_label)) {
      #       if (is.null(merged_value_label[[key]])) {
      #         merged_value_label[[key]] <- linked_maps$value_label[[key]]
      #       }
      #     }
      #   }
      # }

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

      private$..linked_ind_roster_data              <- linked_ind_roster_data
      private$..linked_ind_roster_data_stage_name   <- linked_ind_roster_data_stage_name
      private$..linked_ind_roster_data_hash         <- linked_ind_roster_data_hash
      private$..linked_ind_roster_variable_map      <- linked_ind_roster_variable_map
      private$..linked_ind_roster_value_map         <- linked_ind_roster_value_map
      private$..linked_ind_roster_variable_label    <- linked_ind_roster_variable_label
      private$..linked_ind_roster_value_label       <- linked_ind_roster_value_label

      private$..linked_ind_deaths_data              <- linked_ind_deaths_data
      private$..linked_ind_deaths_data_stage_name   <- linked_ind_deaths_data_stage_name
      private$..linked_ind_deaths_data_hash         <- linked_ind_deaths_data_hash
      private$..linked_ind_deaths_variable_map      <- linked_ind_deaths_variable_map
      private$..linked_ind_deaths_value_map         <- linked_ind_deaths_value_map
      private$..linked_ind_deaths_variable_label    <- linked_ind_deaths_variable_label
      private$..linked_ind_deaths_value_label       <- linked_ind_deaths_value_label

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

      # Generate per-dataset analysis plans for roster and deaths
      private$..data_analysis_plan_roster <- QuantDataAnalysisPlanLog$new(
        log_df   = NULL,
        log_name = phrutils::phr_txt("Quant Data Analysis Plan (Roster)")
      )
      private$..data_analysis_plan_deaths <- QuantDataAnalysisPlanLog$new(
        log_df   = NULL,
        log_name = phrutils::phr_txt("Quant Data Analysis Plan (Deaths)")
      )

      # Build the DAP for roster dataset from available columns and analysis_schema_roster
      if (!is.null(linked_ind_roster_data)) {

        result <- private$..build_dap_from_schema(
          analysis_schema = private$..analysis_schema_roster,
          variable_map    = private$..variable_map_roster %||% list(),
          available_vars  = names(private$..linked_ind_roster_data),
          dataset_label   = "roster"
        )

        private$..data_analysis_plan_roster$set(field = "log_df", value = result$dap_df)

      }

      # Build the DAP for deaths dataset from available columns and analysis_schema_deaths
      if (!is.null(linked_ind_deaths_data)) {

        result <- private$..build_dap_from_schema(
          analysis_schema = private$..analysis_schema_deaths,
          variable_map    = private$..variable_map_deaths %||% list(),
          available_vars  = names(private$..linked_ind_deaths_data),
          dataset_label   = "deaths"
        )

        private$..data_analysis_plan_deaths$set(field = "log_df", value = result$dap_df)

      }

      # Create and store per-dataset survey designs for roster and deaths
      if (!is.null(linked_ind_roster_data)) {
        private$..base_survey_design_roster <- phrutils::phr_try(
          srvyr::as_survey_design(.data = linked_ind_roster_data, ids = 1),
          on_error = "warn",
          origin   = paste0(dataset_name, "$initialize"),
          hint     = "Could not create base (unweighted) survey design for roster data."
        )
        private$..survey_design_roster <- private$..create_survey_design_for_dataset(
          data         = linked_ind_roster_data,
          variable_map = linked_ind_roster_variable_map %||% merged_variable_map,
          origin       = paste0(dataset_name, "$initialize")
        )
      }

      if (!is.null(linked_ind_deaths_data)) {
        private$..base_survey_design_deaths <- phrutils::phr_try(
          srvyr::as_survey_design(.data = linked_ind_deaths_data, ids = 1),
          on_error = "warn",
          origin   = paste0(dataset_name, "$initialize"),
          hint     = "Could not create base (unweighted) survey design for deaths data."
        )
        private$..survey_design_deaths <- private$..create_survey_design_for_dataset(
          data         = linked_ind_deaths_data,
          variable_map = linked_ind_deaths_variable_map %||% merged_variable_map,
          origin       = paste0(dataset_name, "$initialize")
        )
      }

      msg_parts <- c()
      if (!is.null(linked_ind_roster_data)) msg_parts <- c(msg_parts, "roster")
      if (!is.null(linked_ind_deaths_data)) msg_parts <- c(msg_parts, "deaths")
      if (length(msg_parts) > 0) {
        phrutils::phr_message(
          phr_txt(glue::glue(
            "MortalityDataAnalytics initialized with linked {paste(msg_parts, collapse=' and ')} data."
          ))
        )
      } else {
        phrutils::phr_message(
          phr_txt(glue::glue("{dataset_name} initialized as MortalityDataAnalytics object."))
        )
      }
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

    #' @description
    #' Pre-hook providing the input sets used by the inherited \code{run_analysis()}.
    #'
    #' Returns one input set per available mortality dataset:
    #' \describe{
    #'   \item{household}{Always included, using the core \code{data},
    #'     \code{survey_design} and \code{data_analysis_plan} fields. Results
    #'     stored in \code{analysis_results$household}.}
    #'   \item{roster}{Included when \code{linked_ind_roster_data} is available and
    #'     \code{data_analysis_plan_roster} contains at least one indicator. Results
    #'     stored in \code{analysis_results$roster}.}
    #'   \item{deaths}{Included when \code{linked_ind_deaths_data} is available and
    #'     \code{data_analysis_plan_deaths} contains at least one indicator. Results
    #'     stored in \code{analysis_results$deaths}.}
    #' }
    #'
    #' @return A named list of field sets.
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
        if (is.null(private$..data_analysis_plan_rosterget(field = "log_df"))) {
          sets$roster <- list(
            data = "..linked_ind_roster_data",
            survey_design = "..survey_design_roster",
            data_analysis_plan = "..data_analysis_plan_roster"
          )
        } else {
          phrutils::phr_message(origin, phrutils::phr_txt("Linked roster data present but data_analysis_plan_roster is empty. Skipping roster analysis."))
        }
      }

      if (!is.null(private$..linked_ind_deaths_data)) {
        if (is.null(private$..data_analysis_plan_deathsget(field = "log_df"))) {
          sets$deaths <- list(
            data = "..linked_ind_deaths_data",
            survey_design = "..survey_design_deaths",
            data_analysis_plan = "..data_analysis_plan_deaths"
          )
        } else {
          phrutils::phr_message(origin, phrutils::phr_txt("Linked deaths data present but data_analysis_plan_deaths is empty. Skipping deaths analysis."))
        }
      }

      sets
    },

    #' @description
    #' Pre-hook providing the input sets used by the inherited \code{run_outputs()}.
    #'
    #' Returns one input set per available mortality dataset so that outputs are
    #' generated independently for each dataset and stored in named sub-lists
    #' (\code{visualizations$household} / \code{tables$household},
    #' \code{visualizations$roster} / \code{tables$roster},
    #' \code{visualizations$deaths} / \code{tables$deaths}). Each set passes
    #' the appropriate survey design objects for the dataset, so output entries
    #' with \code{dataset_type = "survey_design"} receive the weighted design and
    #' \code{"base"} entries receive the unweighted base design. Each set also
    #' names the dataset's own variable map, so \code{@variable_map$} references
    #' in \code{test_params} resolve against the correct dataset.
    #'
    #' @return A named list of field sets.
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
            variable_map = "..linked_ind_roster_variable_map"
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
            variable_map = "..linked_ind_deaths_variable_map"
          )
        } else {
          phrutils::phr_message(origin, "Linked deaths data present but outputs_schema_deaths is empty. Skipping deaths outputs.")
        }
      }

      sets
    }

  )
)
