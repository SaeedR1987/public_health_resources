#' IPHRA WASH Data Analytics Class
#'
#' The `WASHDataAnalytics` R6 class extends `DataAnalytics` to provide
#' Water, Sanitation and Hygiene specific quality checks and quantitative
#' analysis in a single integrated object.
#'
#' @description
#' This class provides WASH-specific:
#' * Quality checks (water source, JMP ladder, sanitation, handwashing) via quality_schema
#' * Quantitative analysis indicators via analysis_schema
#' * All visualizations/tables (quality and analysis) via outputs_schema
#'
#' @field linked_containers_data Optional linked WaterContainerData dataframe
#' @field linked_containers_data_stage_name Name of linked containers data stage
#' @field linked_containers_data_hash Hash of linked containers data
#' @field linked_containers_variable_map Variable mappings for linked containers data
#' @field linked_containers_value_map Value mappings for linked containers data
#'
#' @seealso [DataAnalytics]
#' @export
WASHDataAnalytics <- R6::R6Class(
  classname = "WASHDataAnalytics",
  inherit = DataAnalytics,

  public = list(

    #' @description
    #' Initialize a new WASHDataAnalytics object
    #'
    #' @param data A data frame (standardized or clean WASH data)
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
    #' @param linked_containers_data Optional linked WaterContainerData dataframe
    #' @param linked_containers_data_stage_name Name of linked containers data stage
    #' @param linked_containers_data_hash Hash of linked containers data
    #' @param linked_containers_variable_map Variable mappings for linked containers data
    #' @param linked_containers_value_map Value mappings for linked containers data
    #' @param linked_containers_variable_label Variable labels for linked containers data
    #' @param linked_containers_value_label Value labels for linked containers data
    #' @return A new WASHDataAnalytics object
    initialize = function(data = NULL,
                          dap = NULL,
                          parent_data_object = NULL,
                          dataset_name = "WASHDataAnalytics",
                          data_stage_name = NULL,
                          data_hash = NULL,
                          variable_map = NULL,
                          value_map = NULL,
                          variable_label = NULL,
                          value_label = NULL,
                          quality_schema = NULL,
                          linked_containers_data = NULL,
                          linked_containers_data_stage_name = NULL,
                          linked_containers_data_hash = NULL,
                          linked_containers_variable_map = NULL,
                          linked_containers_value_map = NULL,
                          linked_containers_variable_label = NULL,
                          linked_containers_value_label = NULL) {

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = dataset_name,
        message = phrutils::phr_txt("Started initialization for {dataset_name}.")
      )

      # Step 1: Set super initialize
      result <- phrutils::phr_try_step(
        {

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

        },
      step = phrutils::phr_txt("Super initialize in {dataset_name}"),
      hint = phrutils::phr_txt("Check the parent DataAnalytics class")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      # Step 2: Set fields
      result <- phrutils::phr_try_step(
        {
          if(!is.null(linked_containers_data)) {
            self$set(field = "..linked_containers_data", value = linked_containers_data)
            self$set(field = "..linked_containers_data_stage_name", value = linked_containers_data_stage_name)
            self$set(field = "..linked_containers_data_hash", value = linked_containers_data_hash)
            self$set(field = "..linked_containers_variable_map", value = linked_containers_variable_map)
            self$set(field = "..linked_containers_value_map", value = linked_containers_value_map)
            self$set(field = "..linked_containers_variable_label", value = linked_containers_variable_label)
            self$set(field = "..linked_containers_value_label", value = linked_containers_value_label)
          }
        },
      step = phrutils::phr_txt("Loading WASH specific fields in initialize"),
      hint = phrutils::phr_txt("Check water container data and inputs")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      # Step 3: Load schemas for water containers
      result <- phrutils::phr_try_step(
        {
          if(!is.null(linked_containers_data)) {
            private$..initialize_load_schemas_wash()
          }
        },
        step = phrutils::phr_txt("Loading WASH specific fields in initialize"),
        hint = phrutils::phr_txt("Check water container data and inputs")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      # Step 4: Create WASH specific data analysis plans
      result <- phrutils::phr_try_step(
        {
          # Generate per-dataset analysis plans for roster and deaths

          self$set(field = "..data_analysis_plan_water_container",
                   value = QuantDataAnalysisPlanLog$new(
                     log_df   = NULL,
                     log_name = phrutils::phr_txt("Quant Data Analysis Plan (Water Container)")
                   ))

          # Build the DAP for roster dataset from available columns and analysis_schema_roster
          if (!is.null(linked_containers_data)) {

            result <- private$..build_dap_from_schema(
              analysis_schema = private$..analysis_schema_water_container,
              variable_map    = private$..linked_containers_variable_map %||% list(),
              available_vars  = names(private$..linked_containers_data),
              dataset_label   = "water_containers"
            )

            private$..data_analysis_plan_water_container$set(field = "..log_df", value = result$dap_df)

          }
        },
        step = phrutils::phr_txt("Create water container specific data analysis plan"),
        hint = phrutils::phr_txt("Check water container data and inputs")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      # Step 5: Create WASH survey design objects
      result <- phrutils::phr_try_step(
        {
          # Create and store per-dataset survey designs for roster and deaths
          if (!is.null(linked_containers_data)) {

            self$set(field = "..base_survey_design_water_container",
                     value = srvyr::as_survey_design(.data = linked_containers_data, ids = 1))

            self$set(field = "..survey_design_water_container",
                     value = private$..create_survey_design_for_dataset(
                       data         = linked_containers_data,
                       variable_map = linked_containers_variable_map %||% private$..linked_containers_variable_map,
                       origin       = paste0(dataset_name, "$initialize")
                     ))
          }
        },
        step = phrutils::phr_txt("Create survey design objects for water containers."),
        hint = phrutils::phr_txt("Check water container data and inputs")
      )
      if (phrutils::phr_failed(result)) {
        return(result)
      }

      if (!is.null(linked_containers_data)) {
        msg <- "{dataset_name} initialized successfully with linked water container data."
      } else {
        msg <- "{dataset_name} initialized successfully."
      }

      private$..touch(
        log = TRUE,
        touch = TRUE,
        name = dataset_name,
        message = phrutils::phr_txt(msg)
      )

    }
  ),

  private = list(

    # Fields for linked water container data
    ..linked_containers_data = NULL,
    ..linked_containers_data_stage_name = NULL,
    ..linked_containers_data_hash = NULL,
    ..linked_containers_variable_map = NULL,
    ..linked_containers_value_map = NULL,
    ..linked_containers_variable_label = NULL,
    ..linked_containers_value_label = NULL,

    ..data_analysis_plan_water_container = NULL,
    ..analysis_schema_water_container = NULL,
    ..outputs_schema_water_container = NULL,

    ..base_survey_design_water_container = NULL,
    ..survey_design_water_container = NULL,


    # @field ..default_quality_schema_filename Default file name for variable schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_quality_schema_filename = c(
      "resources",
      "quality_schema_data_quality_wash_template.xlsx"
    ),

    # @field ..default_outputs_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_outputs_schema_filename = c(
      "resources",
      "outputs_schema_data_analytics_wash_template.xlsx"
    ),

    # @field ..default_analysis_schema_filename Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_analysis_schema_filename = c(
      "resources",
      "analysis_schema_quant_data_analysis_wash_template.xlsx"
    ),

    # @field ..default_outputs_schema_filename Default file name for dependency schema xlsx file
    # Should be a two element character vector with the folder and filename.
    # @keywords internal
    ..default_outputs_schema_water_container_filename = c(
      "resources",
      "outputs_schema_data_analytics_water_container_template.xlsx"
    ),

    # @field ..default_analysis_schema_filename Named list of presentation-ready value labels
    #  for categorical variables (populated by ..map_schema_labels()).
    # @keywords internal
    ..default_analysis_schema_water_container_filename = c(
      "resources",
      "analysis_schema_quant_data_analysis_water_container_template.xlsx"
    ),

    #' Additional Accessible Fields Hook
    #'
    #' @description
    #' Returns subclass-specific fields accessible via `get()`.
    #'
    #' @return Character vector of field names.
    ..additional_get_fields = function() {
      c(
        super$..additional_get_fields(),
        "..linked_containers_data",
        "..linked_containers_data_stage_name",
        "..linked_containers_data_hash",
        "..linked_containers_variable_map",
        "..linked_containers_value_map",
        "..linked_containers_variable_label",
        "..linked_containers_value_label",
        "..data_analysis_plan_water_container",
        "..analysis_schema_water_container",
        "..outputs_schema_water_container",
        "..base_survey_design_water_container",
        "..survey_design_water_container"
      )
    },

    #' Additional Settable Fields Hook
    #'
    #' @description
    #' Returns subclass-specific fields that may be modified via `set()`.
    #'
    #' @return Character vector of field names.
    ..additional_set_fields = function() {
      c(
        super$..additional_set_fields(),
        "..linked_containers_data",
        "..linked_containers_data_stage_name",
        "..linked_containers_data_hash",
        "..linked_containers_variable_map",
        "..linked_containers_value_map",
        "..linked_containers_variable_label",
        "..linked_containers_value_label",
        "..data_analysis_plan_water_container",
        "..analysis_schema_water_container",
        "..outputs_schema_water_container",
        "..base_survey_design_water_container",
        "..survey_design_water_container"
      )
    },

    #' Initialize Default Schemas
    #'
    #' @description
    #' Loads the default quality, analysis, and outputs schemas into
    #' their corresponding private fields.
    #'
    #' @return Invisibly returns TRUE.
    ..initialize_load_schemas_wash = function() {

      private$..load_default_schema(
        filename = private$..default_analysis_schema_water_container_filename,
        target_field = "..analysis_schema_water_container",
        required_cols = c(
          "indicator_name",
          "calculation",
          "var_name",
          "denom_var",
          "disaggregation",
          "multiplier",
          "indicator_unit"
        ),
        schema_name = "analysis_water_container",
        dataset_name = private$..dataset_name
      )

      private$..load_default_schema(
        filename = private$..default_outputs_schema_water_container_filename,
        target_field = "..outputs_schema_water_container",
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
        schema_name = "outputs_water_container",
        dataset_name = private$..dataset_name
      )

      invisible(TRUE)
    },

    #' @description
    #' Pre-hook providing the input sets used by the inherited \code{run_analysis()}.
    #'
    #' Returns one input set per available mortality dataset:
    #' \describe{
    #'   \item{household}{Always included, using the core \code{data},
    #'     \code{survey_design} and \code{data_analysis_plan} fields. Results
    #'     stored in \code{analysis_results$household}.}
    #'   \item{water_container}{Included when \code{linked_ind_container_data} is available and
    #'     \code{data_analysis_plan_water_container} contains at least one indicator. Results
    #'     stored in \code{analysis_results$water_container}.}
    #' }
    #'
    #' @return A named list of field sets.
    ..pre_run_analysis = function() {
      origin <- paste0(private$..dataset_name, "$pre_run_analysis")

      sets <- super$..pre_run_analysis()

      if (!is.null(private$..linked_containers_data)) {
        if (!is.null(private$..data_analysis_plan_water_container$get(field = "..log_df"))) {
          sets$water_container <- list(
            data = "..linked_containers_data",
            survey_design = "..survey_design_water_container",
            data_analysis_plan = "..data_analysis_plan_water_container"
          )
        } else {
          phrutils::phr_message(origin, phrutils::phr_txt("Linked water container data present but data_analysis_plan_water_container is empty. Skipping water container analysis."))
        }
      }

      sets
    },

    #' @description
    #' Pre-hook providing the input sets used by the inherited \code{run_outputs()}.
    #'
    #' Returns one input set per available WASH dataset so that outputs are
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
      origin <- paste0(private$..dataset_name, "$..pre_run_outputs")

      sets <- super$..pre_run_outputs()

      if (!is.null(private$..linked_containers_data)) {
        if (!is.null(private$..outputs_schema_water_container) && nrow(private$..outputs_schema_water_container) > 0) {
          sets$roster <- list(
            data = "..linked_containers_data",
            outputs_schema = "..outputs_schema_water_container",
            base_survey_design = "..base_survey_design_water_container",
            survey_design = "..survey_design_water_container",
            variable_map = "..linked_containers_variable_map"
          )
        } else {
          phrutils::phr_message(origin, "Linked water container data present but outputs_schema_water_container is empty. Skipping water container outputs.")
        }
      }
      sets
    }
  )
)
