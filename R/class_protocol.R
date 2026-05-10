#' Protocol R6 Class
#'
#' @description
#' Base class for managing protocol pipeline workflows.
#' Handles core components shared across all protocol types:
#' 1. Objective Selection (managed through the associated \code{\link{Framework}} object)
#' 2. Tool and Indicator Selection
#'
#' For survey protocols that require strata definition, sample size
#' calculations, sampling frame management, and sample drawing, use
#' the \code{\link{SurveyProtocol}} subclass instead.
#'
#' @importFrom R6 R6Class
#' @export
Protocol <- R6::R6Class(
  "Protocol",
  public = list(
    #' @field objectives Nested list of research objectives (legacy field).
    #'   Objectives are now primarily managed through the associated
    #'   \code{\link{Framework}} object via its \code{adjusted_schema}.
    #'   Use \code{objectives_to_df()} or \code{flatten_objectives()} to inspect.
    objectives = NULL,

    #' @field framework A \code{\link{Framework}} object (e.g.
    #'   \code{\link{ANAFramework}}) that holds the master and adjusted
    #'   reference schemas and SVG diagrams for this protocol.  Assign a
    #'   Framework instance to this field to associate a conceptual framework
    #'   with the protocol.
    framework = NULL,

    #' @field objective_schema Data frame containing the objective schema.
    #'   This field is no longer auto-populated on initialisation.  When a
    #'   \code{\link{Framework}} is associated with this protocol, access the
    #'   schema via \code{self$framework$master_schema} or
    #'   \code{self$framework$adjusted_schema}.  \code{objective_schema} may
    #'   still be set manually for custom schemas that do not require a full
    #'   Framework object.
    objective_schema = NULL,

    #' @field protocol_schema Data frame describing TOR placeholder handling
    #'   rules.  Expected columns are \code{tag_name}, \code{handling},
    #'   \code{condition}, and \code{default_value}.
    protocol_schema = NULL,

    #' @field tools List of Tool objects (placeholder for Tool class instances)
    tools = NULL,

    #' @field selected_indicators List of selected indicators
    selected_indicators = NULL,

    #' @field objective_catalog_master Named list keyed by objective code from
    #'   \code{framework$master_schema}; each value stores objective metadata.
    objective_catalog_master = list(),

    #' @field objective_catalog_adjusted Named list keyed by objective code from
    #'   \code{framework$adjusted_schema}; each value stores objective metadata.
    objective_catalog_adjusted = list(),

    #' @field indicator_catalog_master Named list keyed by indicator code from
    #'   \code{framework$master_schema}; each value stores indicator metadata.
    indicator_catalog_master = list(),

    #' @field indicator_catalog_adjusted Named list keyed by indicator code from
    #'   \code{framework$adjusted_schema}; each value stores indicator metadata.
    indicator_catalog_adjusted = list(),

    #' @field tool_indicator_catalog_master Named list keyed by tool name with
    #'   vectors of indicator codes available in master tool surveys.
    tool_indicator_catalog_master = list(),

    #' @field tool_indicator_catalog_revised Named list keyed by tool name with
    #'   vectors of indicator codes available in revised tool surveys.
    tool_indicator_catalog_revised = list(),

    #' @field sampling_frame_strata_names Character vector of unique strata names
    #'   currently available in the held \code{SamplingFrame}, when present.
    sampling_frame_strata_names = character(0),

    #' @field issues List of validation issues and discrepancies
    issues = list(),

    #' @field issues_coherence List of coherence issues found between the
    #'   \code{adjusted_schema} indicator codes and the tool indicator codes.
    #'   Populated by \code{diagnose_coherence()}.
    issues_coherence = list(),

    #' @field metadata List containing protocol metadata
    metadata = list(
      created_date = NULL,
      modified_datetime = NULL,
      month_year = NULL,
      country_name = NULL,
      assessment_title = NULL,
      target_strata = list(),
      protocol_version = "1.0",
      version = 1L,
      # Text metadata fields
      mandating_body = NULL,
      project_code = NULL,
      overall_timeframe = NULL,
      geographic_coverage = NULL,
      general_objective = NULL,
      sampling_strata_names = character(0),
      sampling_method_flags = list(),
      # Audience type boolean fields
      `audience_type.strategic` = FALSE,
      `audience_type.operational` = FALSE,
      `audience_type.programmatic` = FALSE,
      `audience_type.other` = FALSE,
      # Population boolean fields
      `pop_idpcamp` = FALSE,
      `pop_idphost` = FALSE,
      `pop_idpinformal` = FALSE,
      `pop_idpother` = FALSE,
      `pop_refugee` = FALSE,
      `pop_refugeeinformal` = FALSE,
      `pop_refugeehost` = FALSE,
      `pop_refugeeother` = FALSE,
      `pop_host` = FALSE,
      `pop_other` = FALSE
    ),

    #' @field conditional_metadata Named list of boolean flags used for
    #'   conditional schema replacements.
    conditional_metadata = list(),

    #' @field secondary_data Named list of secondary data sources keyed by
    #'   objective code.  Each element is a character string naming the source
    #'   or a URL.  Objective codes must match codes available in the
    #'   \code{master_schema} of the associated \code{\link{Framework}}.
    secondary_data = NULL,

    #' @description
    #' Creates a new Protocol object
    #' @param assessment_title Character. Title of the assessment
    #' @param country_name Character. Country where assessment takes place
    #' @param month_year Character. Month and year of data collection (e.g., "January 2024")
    #' @param framework_type Character. Type of framework to initialise.  Must be
    #'   one of \code{"none"} (creates a generic \code{\link{Framework}} object) or
    #'   \code{"ana"} (creates an \code{\link{ANAFramework}} object).
    #' @return A new Protocol object
    initialize = function(assessment_title = NULL, country_name = NULL, month_year = NULL,
                          framework_type = "none") {
      phr_try({
        valid_fw_types <- c("none", "ana")
        phr_assert(
          is.character(framework_type) && length(framework_type) == 1 &&
            framework_type %in% valid_fw_types,
          message = phr_txt(
            "framework_type must be one of: {paste(valid_fw_types, collapse=', ')}."
          ),
          origin = "Protocol$initialize"
        )
        self$metadata$created_date <- Sys.time()
        self$metadata$modified_datetime <- Sys.time()
        self$metadata$assessment_title <- assessment_title
        self$metadata$country_name <- country_name
        self$metadata$month_year <- month_year
        self$objectives <- list()
        self$tools <- list()
        self$issues <- list()
        self$issues_coherence <- list()
        self$conditional_metadata <- list()
        self$objective_catalog_master <- list()
        self$objective_catalog_adjusted <- list()
        self$indicator_catalog_master <- list()
        self$indicator_catalog_adjusted <- list()
        self$tool_indicator_catalog_master <- list()
        self$tool_indicator_catalog_revised <- list()
        self$sampling_frame_strata_names <- character(0)
        self$protocol_schema <- private$.load_protocol_schema()

        self$framework <- if (framework_type == "ana") {
          ANAFramework$new()
        } else {
          Framework$new()
        }
        self$sync_framework_catalog_fields
        self$sync_tool_indicator_catalog_fields
        self$sync_sample_metadata_fields
        self$sync_sampling_frame_fields

        phr_message(phr_txt("Protocol initialized."), origin = "Protocol$initialize")
      }, on_error = "abort", origin = "Protocol$initialize")
      invisible(self)
    },

    #' @description Set or replace the associated Framework object.
    #' @param framework A \code{\link{Framework}} object.
    #' @return Invisibly returns \code{self}.
    set_framework = function(framework) {
      phr_assert(
        !is.null(framework) && inherits(framework, "Framework"),
        message = phr_txt("framework must be a Framework object."),
        origin  = "Protocol$set_framework"
      )
      self$framework <- framework
      self$sync_framework_catalog_fields
      private$touch()
      private$check_issues()
      invisible(self)
    },

    #' @description Synchronize orchestrator-level cached fields from held
    #'   framework/tool/sample/sampling-frame objects.
    #' @return Invisibly returns \code{self}.
    synchronize_state = function() {
      self$sync_framework_catalog_fields
      self$sync_tool_indicator_catalog_fields
      self$sync_sample_metadata_fields
      self$sync_sampling_frame_fields
      invisible(self)
    },
    
    #' @description Add a single Tool object to the protocol by specifying its type.
    #' A new tool of the requested type is instantiated (loading its bundled
    #' default XLSForm template) and stored in the \code{tools} named list under
    #' \code{tool_name} so it is accessible via \code{protocol$tools$<name>}.
    #' Call this method once per tool you wish to add.
    #' @param tool_type Character. Type of tool to create.  One of
    #'   \code{"household"}, \code{"key_informant"}, \code{"observation"}, or
    #'   \code{"generic"}.  Defaults to \code{"household"}.
    #' @param tool_name Optional character. Name/key for the tool in the
    #'   \code{tools} list.  When \code{NULL} a key is generated automatically
    #'   from \code{tool_type} and the current count (e.g. \code{"household_1"}).
    #' @return Invisibly returns self for method chaining.
    add_tools = function(tool_type = "household", tool_name = NULL) {
      phr_try({
        valid_types <- c("household", "key_informant", "observation", "generic")
        phr_assert(
          tool_type %in% valid_types,
          message = phr_txt("tool_type must be one of: {paste(valid_types, collapse=', ')}."),
          origin  = "Protocol$add_tools"
        )

        # Determine the key to use in the named tools list.
        if (is.null(tool_name) || !nzchar(tool_name)) {
          existing_keys <- names(self$tools)
          n_same_type   <- sum(grepl(paste0("^", tool_type, "(_[0-9]+)?$"), existing_keys))
          tool_name     <- if (n_same_type == 0) tool_type else paste0(tool_type, "_", n_same_type + 1L)
        }

        tool <- switch(
          tool_type,
          "household"     = HouseholdTool$new(name = tool_name),
          "key_informant" = KeyInformantTool$new(name = tool_name),
          "observation"   = ObservationTool$new(name = tool_name),
          Tool$new(name = tool_name)
        )

        if (is.null(self$tools)) {
          self$tools <- list()
        }

        self$tools[[tool_name]] <- tool
        self$sync_tool_indicator_catalog_fields
        private$touch()
        private$check_issues()
        phr_message(
          phr_txt("Tool of type '{tool_type}' added as '{tool_name}'."),
          origin = "Protocol$add_tools"
        )
      }, on_error = "abort", origin = "Protocol$add_tools")
      invisible(self)
    },
    
    #' @description Select indicators for data collection
    #' @param indicator_list List of indicators
    select_indicators = function(indicator_list) {
      self$selected_indicators <- indicator_list
      self$sync_tool_indicator_catalog_fields
      private$touch()
      private$check_issues()
      invisible(self)
    },
    
    #' @description Get all issues
    #' @return List of validation issues
    get_issues = function() {
      return(self$issues)
    },
    
    #' @description Get protocol summary
    #' @return List with protocol summary information
    get_protocol_summary = function() {
      list(
        assessment_title = self$metadata$assessment_title,
        country_name = self$metadata$country_name,
        month_year = self$metadata$month_year,
        created = self$metadata$created_date,
        modified = self$metadata$modified_datetime,
        num_objectives = count_objectives(self$objectives),
        num_tools = length(self$tools),
        num_issues = length(self$issues)
      )
    },

    #' @description Update one or more metadata fields by name.
    #'
    #' Assigns each named value to the corresponding key in
    #' \code{self$metadata} and calls \code{touch()} to update the
    #' \code{modified_datetime}.  Unknown keys are silently added as new fields;
    #' existing keys are overwritten.
    #'
    #' @param ... Named arguments where each name is a metadata field key and
    #'   each value is the new value for that field.  At least one named
    #'   argument must be supplied.
    #' @return Invisibly returns \code{self} for method chaining.
    #'
    #' @examples
    #' \dontrun{
    #' p$update_metadata(country_name = "Somalia", version = 2L)
    #' p$update_metadata(recall_period = "Past 3 months")
    #' }
    update_metadata = function(...) {
      phr_try({
        args <- list(...)
        phr_assert(
          length(args) > 0 && !is.null(names(args)) && all(nzchar(names(args))),
          message = phr_txt("update_metadata requires at least one named argument."),
          origin  = "Protocol$update_metadata"
        )
        for (key in names(args)) {
          self$metadata[[key]] <- args[[key]]
        }
        private$touch()
        phr_message(
          phr_txt("Metadata updated: {paste(names(args), collapse=', ')}."),
          origin = "Protocol$update_metadata"
        )
      }, on_error = "abort", origin = "Protocol$update_metadata")
      invisible(self)
    },

    # ── Schema / Framework helpers ─────────────────────────────────────────

    #' @description Retrieve a schema data frame from the associated
    #'   \code{\link{Framework}}.
    #'
    #' Returns \code{adjusted_schema} when \code{type = "adjusted"} and
    #' \code{master_schema} when \code{type = "master"}.  Falls back to
    #' \code{self$objective_schema} when no framework is attached, and returns
    #' an empty \code{data.frame()} when neither is available.
    #'
    #' @param type Character. One of \code{"master"} (default) or
    #'   \code{"adjusted"}.
    #' @return A data frame.
    get_schema = function(type = c("master", "adjusted")) {
      type <- match.arg(type)
      if (!is.null(self$framework) && inherits(self$framework, "Framework")) {
        schema <- if (type == "adjusted") {
          self$framework$adjusted_schema
        } else {
          self$framework$master_schema
        }
        if (!is.null(schema) && is.data.frame(schema) && nrow(schema) > 0) {
          return(as.data.frame(schema, stringsAsFactors = FALSE))
        }
      }
      # Fallback: self$objective_schema
      if (!is.null(self$objective_schema) && is.data.frame(self$objective_schema) &&
          nrow(self$objective_schema) > 0) {
        return(as.data.frame(self$objective_schema, stringsAsFactors = FALSE))
      }
      data.frame()
    },

    #' @description Return all unique, non-NA indicator codes present in a
    #'   schema.
    #'
    #' @param type Character. One of \code{"master"} (default) or
    #'   \code{"adjusted"}.  Passed to \code{get_schema()}.
    #' @return Character vector of unique indicator codes.  Empty character
    #'   vector when none are found.
    get_indicator_codes_from_schema = function(type = c("master", "adjusted")) {
      schema <- self$get_schema(type = match.arg(type))
      if (is.data.frame(schema) && "indicator_code" %in% names(schema)) {
        codes <- as.character(schema$indicator_code)
        return(unique(codes[!is.na(codes) & nzchar(codes)]))
      }
      character(0)
    },

    #' @description Return the subset of a schema whose \code{indicator_code}
    #'   matches the supplied codes.
    #'
    #' @param indicator_codes Character vector of indicator codes to keep.
    #' @param type Character. \code{"master"} (default) or \code{"adjusted"}.
    #' @return Filtered data frame (zero rows when none match).
    get_schema_for_indicators = function(indicator_codes,
                                         type = c("master", "adjusted")) {
      schema <- self$get_schema(type = match.arg(type))
      if (!is.data.frame(schema) || nrow(schema) == 0 ||
          !"indicator_code" %in% names(schema)) {
        return(data.frame())
      }
      ic <- as.character(indicator_codes)
      schema[as.character(schema$indicator_code) %in% ic, , drop = FALSE]
    },

    #' @description Extract one column from the schema, optionally filtered to
    #'   rows whose \code{indicator_code} matches \code{indicator_codes}.
    #'
    #' @param col_name Character. Name of the column to extract.
    #' @param indicator_codes Optional character vector. When supplied, only
    #'   rows whose \code{indicator_code} matches are used.
    #' @param type Character. \code{"master"} (default) or \code{"adjusted"}.
    #' @param unique_only Logical. When \code{TRUE} (default) return only
    #'   unique values.
    #' @param drop_na Logical. When \code{TRUE} (default) remove \code{NA}
    #'   and empty-string values.
    #' @return Character vector of values.
    get_schema_column = function(col_name,
                                  indicator_codes = NULL,
                                  type            = c("master", "adjusted"),
                                  unique_only     = TRUE,
                                  drop_na         = TRUE) {
      schema <- if (!is.null(indicator_codes)) {
        self$get_schema_for_indicators(indicator_codes, type = match.arg(type))
      } else {
        self$get_schema(type = match.arg(type))
      }
      if (!is.data.frame(schema) || nrow(schema) == 0 ||
          !col_name %in% names(schema)) {
        return(character(0))
      }
      vals <- as.character(schema[[col_name]])
      if (drop_na)    vals <- vals[!is.na(vals) & nzchar(vals)]
      if (unique_only) vals <- unique(vals)
      vals
    },

    # ── Tool helpers ────────────────────────────────────────────────────────

    #' @description Return the names of all currently registered tools.
    #' @return Character vector of tool names (keys of \code{self$tools}).
    #'   Empty character vector when no tools are registered.
    get_tool_names = function() {
      if (is.null(self$tools) || length(self$tools) == 0) return(character(0))
      names(self$tools)
    },

    #' @description Check whether a specific tool is registered.
    #'
    #' @param tool_name Character. Tool name to look up.
    #' @return \code{TRUE} if the tool is present in \code{self$tools},
    #'   \code{FALSE} otherwise.
    is_tool_included = function(tool_name) {
      if (is.null(self$tools) || length(self$tools) == 0) return(FALSE)
      isTRUE(tool_name %in% names(self$tools))
    },

    #' @description Return the survey data frame for a named tool.
    #'
    #' Returns \code{revised_survey} when it is non-NULL and non-empty, and
    #' falls back to \code{survey}.  Returns \code{NULL} when the tool is not
    #' found or has no survey data.
    #'
    #' @param tool_name Character. Tool name (key of \code{self$tools}).
    #' @param prefer_revised Logical. When \code{TRUE} (default), prefer
    #'   \code{revised_survey} over \code{survey}.
    #' @return Data frame or \code{NULL}.
    get_tool_survey = function(tool_name, prefer_revised = TRUE) {
      if (!self$is_tool_included(tool_name)) return(NULL)
      tool <- self$tools[[tool_name]]
      if (!methods::is(tool, "R6")) return(NULL)
      sv <- if (isTRUE(prefer_revised)) {
        rs <- tryCatch(tool$revised_survey, error = function(e) NULL)
        if (!is.null(rs) && is.data.frame(rs) && nrow(rs) > 0) rs
        else tryCatch(tool$survey, error = function(e) NULL)
      } else {
        tryCatch(tool$survey, error = function(e) NULL)
      }
      sv
    },

    #' @description Return all unique indicator codes present across the
    #'   survey data of the specified tools (or all tools when
    #'   \code{tool_names} is \code{NULL}).
    #'
    #' Uses each tool's \code{get_indicator_codes()} helper, which correctly
    #' handles comma-separated multi-code cells (e.g. \code{"10501, 10502"}).
    #'
    #' @param tool_names Optional character vector of tool names to restrict
    #'   the search.  Defaults to \code{NULL} (all registered tools).
    #' @param prefer_revised Logical. When \code{TRUE} (default), prefer
    #'   \code{revised_survey} over \code{survey}.
    #' @return Character vector of unique, non-NA indicator codes.
    get_indicator_codes_from_tools = function(tool_names = NULL,
                                               prefer_revised = TRUE) {
      all_names <- self$get_tool_names()
      if (length(all_names) == 0) return(character(0))
      if (!is.null(tool_names)) {
        all_names <- intersect(all_names, as.character(tool_names))
      }
      codes <- character(0)
      for (tn in all_names) {
        tool <- self$tools[[tn]]
        if (!is.null(tool) && inherits(tool, "Tool")) {
          # get_indicator_codes() splits comma-separated cells and returns integers
          tool_codes <- as.character(tool$get_indicator_codes(prefer_revised = prefer_revised))
          codes <- c(codes, tool_codes[nzchar(tool_codes)])
        }
      }
      unique(codes)
    },
    
    #' @description Export protocol to a list
    #' @return List containing all protocol data
    export_protocol = function() {
      fw_data <- if (!is.null(self$framework) && inherits(self$framework, "Framework")) {
        fw <- self$framework
        list(
          class                = class(fw)[1],
          master_schema        = fw$master_schema,
          adjusted_schema      = fw$adjusted_schema,
          master_svg           = fw$master_svg,
          adjusted_svg         = fw$adjusted_svg,
          primary_objectives   = fw$primary_objectives,
          secondary_objectives = fw$secondary_objectives
        )
      } else {
        NULL
      }
      list(
        metadata = self$metadata,
        conditional_metadata = self$conditional_metadata,
        objectives = self$objectives,
        objective_schema = self$objective_schema,
        framework = fw_data,
        tools = self$tools,
        selected_indicators = self$selected_indicators,
        objective_catalog_master = self$objective_catalog_master,
        objective_catalog_adjusted = self$objective_catalog_adjusted,
        indicator_catalog_master = self$indicator_catalog_master,
        indicator_catalog_adjusted = self$indicator_catalog_adjusted,
        tool_indicator_catalog_master = self$tool_indicator_catalog_master,
        tool_indicator_catalog_revised = self$tool_indicator_catalog_revised,
        sampling_frame_strata_names = self$sampling_frame_strata_names,
        issues = self$issues,
        summary = self$get_protocol_summary()
      )
    },

    #' @description Validate an objective schema data frame.
    #'
    #' Convenience method that delegates to the standalone
    #' \code{\link{validate_objective_schema}} function.  Useful for validating
    #' custom schemas before associating them with this protocol.
    #'
    #' @param schema Data frame to validate as an objective schema.
    #' @param soft Logical. When \code{TRUE} issues warnings rather than errors
    #'   for recoverable problems.  Defaults to \code{FALSE}.
    #' @return Invisibly returns \code{TRUE} if valid.
    validate_objective_schema = function(schema, soft = FALSE) {
      validate_objective_schema(schema, soft = soft)
    },

    #' @description Diagnose coherence between the \code{adjusted_schema} indicator
    #' codes and the \code{indicator_code} values across all tools in
    #' \code{self$tools} (using each tool's \code{revised_survey}).
    #'
    #' Checks performed:
    #' \enumerate{
    #'   \item Objectives in \code{adjusted_schema} that have no matching
    #'     \code{indicator_code} in any tool's \code{revised_survey}.
    #'   \item \code{indicator_code} values present in tools but absent from
    #'     \code{adjusted_schema}.
    #' }
    #'
    #' Results are stored in \code{self$issues_coherence} as a named list.
    #' An empty list means no coherence issues were found.
    #'
    #' @return Invisibly returns \code{self} for method chaining.
    diagnose_coherence = function() {
      self$issues_coherence <- list()

      if (is.null(self$framework) || !inherits(self$framework, "Framework")) {
        self$issues_coherence$no_framework <-
          "No framework is associated with this protocol."
        return(invisible(self))
      }

      schema <- self$framework$adjusted_schema
      if (is.null(schema) || !is.data.frame(schema) || nrow(schema) == 0) {
        self$issues_coherence$no_schema <-
          "adjusted_schema is empty or not set in the framework."
        return(invisible(self))
      }

      has_obj_col <- "objective_code" %in% names(schema)
      has_ind_col <- "indicator_code"  %in% names(schema)

      if (!has_obj_col || !has_ind_col) {
        self$issues_coherence$schema_columns <- paste0(
          "adjusted_schema must contain 'objective_code' and 'indicator_code' columns. ",
          "Found: ", paste(names(schema), collapse = ", ")
        )
        return(invisible(self))
      }

      schema_ind_codes <- as.character(schema$indicator_code)
      schema_ind_codes <- schema_ind_codes[!is.na(schema_ind_codes) & nzchar(schema_ind_codes)]

      # Unique objective rows (objective_code + label for reporting)
      obj_col  <- as.character(schema$objective_code)
      name_col <- if ("text_objective" %in% names(schema)) {
        as.character(schema$text_objective)
      } else {
        obj_col
      }
      obj_df <- unique(data.frame(
        objective_code = obj_col,
        objective_name = name_col,
        stringsAsFactors = FALSE
      ))
      obj_df <- obj_df[!is.na(obj_df$objective_code), , drop = FALSE]

      # Collect all indicator_codes from all tools' revised_survey
      tool_ind_codes <- character(0)
      if (!is.null(self$tools) && length(self$tools) > 0) {
        for (tool in self$tools) {
          sv <- tryCatch({
            if (!is.null(tool$revised_survey) && nrow(tool$revised_survey) > 0) {
              tool$revised_survey
            } else {
              tool$survey
            }
          }, error = function(e) NULL)
          if (!is.null(sv) && is.data.frame(sv) && "indicator_code" %in% names(sv)) {
            codes <- as.character(sv$indicator_code)
            codes <- codes[!is.na(codes) & nzchar(codes)]
            tool_ind_codes <- c(tool_ind_codes, codes)
          }
        }
        tool_ind_codes <- unique(tool_ind_codes)
      }

      # Check 1: objectives with no indicator coverage in tools
      obj_no_coverage <- list()
      for (i in seq_len(nrow(obj_df))) {
        obj_code <- obj_df$objective_code[i]
        obj_name <- obj_df$objective_name[i]
        obj_inds <- schema_ind_codes[schema$objective_code == obj_code]
        obj_inds <- obj_inds[!is.na(obj_inds) & nzchar(obj_inds)]
        if (length(obj_inds) == 0 || !any(obj_inds %in% tool_ind_codes)) {
          obj_no_coverage[[obj_code]] <- paste0(
            "Objective '", obj_code, "' (", obj_name, ") has no indicators ",
            "in any tool's revised_survey."
          )
        }
      }
      if (length(obj_no_coverage) > 0) {
        self$issues_coherence$objectives_without_indicators <- obj_no_coverage
      }

      # Check 2: tool indicators not matched to any objective in schema
      if (length(tool_ind_codes) > 0) {
        unmatched <- tool_ind_codes[!tool_ind_codes %in% schema_ind_codes]
        if (length(unmatched) > 0) {
          self$issues_coherence$tool_indicators_without_objectives <- paste0(
            "The following indicator_code(s) in tools have no match in ",
            "adjusted_schema: ", paste(unmatched, collapse = ", ")
          )
        }
      }

      if (length(self$issues_coherence) == 0) {
        phr_message(
          phr_txt("Coherence validation passed: all objectives have tool coverage and all tool indicators match the schema."),
          origin = "Protocol$diagnose_coherence"
        )
      } else {
        phr_message(
          phr_txt("Coherence validation found {length(self$issues_coherence)} issue(s). Check self$issues_coherence for details."),
          origin = "Protocol$diagnose_coherence"
        )
      }
      invisible(self)
    },

    # ── Framework delegation methods ───────────────────────────────────────

    #' @description Set primary objectives on the attached Framework.
    #'
    #' Delegates to \code{self$framework$set_primary_objectives()} and then
    #' syncs the Protocol catalog fields and updates the last-modified timestamp.
    #'
    #' @param objective_codes Numeric vector of primary objective codes.
    #' @return Invisibly returns \code{self} for method chaining.
    framework_set_primary_objectives = function(objective_codes) {
      phr_assert(
        !is.null(self$framework) && inherits(self$framework, "Framework"),
        message = phr_txt("No Framework is attached to this Protocol."),
        origin  = "Protocol$framework_set_primary_objectives"
      )
      self$framework$set_primary_objectives(objective_codes)
      self$sync_framework_catalog_fields
      private$touch()
      invisible(self)
    },

    #' @description Set secondary objectives on the attached Framework.
    #'
    #' Delegates to \code{self$framework$set_secondary_objectives()} and then
    #' syncs the Protocol catalog fields and updates the last-modified timestamp.
    #'
    #' @param objective_codes Numeric vector of secondary objective codes.
    #' @return Invisibly returns \code{self} for method chaining.
    framework_set_secondary_objectives = function(objective_codes) {
      phr_assert(
        !is.null(self$framework) && inherits(self$framework, "Framework"),
        message = phr_txt("No Framework is attached to this Protocol."),
        origin  = "Protocol$framework_set_secondary_objectives"
      )
      self$framework$set_secondary_objectives(objective_codes)
      self$sync_framework_catalog_fields
      private$touch()
      invisible(self)
    },

    #' @description Modify the adjusted SVG on the attached Framework.
    #'
    #' Delegates to \code{self$framework$modify_adjusted_svg()} and then
    #' syncs the Protocol catalog fields and updates the last-modified timestamp.
    #'
    #' @param primary_objective_codes Numeric vector of primary objective codes.
    #'   When \code{NULL} (default) the Framework's \code{primary_objectives}
    #'   field is used as a fallback.
    #' @param secondary_objective_codes Numeric vector of secondary objective
    #'   codes.  When \code{NULL} (default) the Framework's
    #'   \code{secondary_objectives} field is used as a fallback.
    #' @return Invisibly returns \code{self} for method chaining.
    framework_modify_svg = function(primary_objective_codes = NULL,
                                    secondary_objective_codes = NULL) {
      phr_assert(
        !is.null(self$framework) && inherits(self$framework, "Framework"),
        message = phr_txt("No Framework is attached to this Protocol."),
        origin  = "Protocol$framework_modify_svg"
      )
      self$framework$modify_adjusted_svg(
        primary_objective_codes   = primary_objective_codes,
        secondary_objective_codes = secondary_objective_codes
      )
      self$sync_framework_catalog_fields
      private$touch()
      invisible(self)
    },

    #' @description Modify the adjusted schema on the attached Framework.
    #'
    #' Delegates to \code{self$framework$modify_adjusted_schema()} and then
    #' syncs the Protocol catalog fields and updates the last-modified timestamp.
    #'
    #' @param objective_codes Character or numeric vector of objective codes to
    #'   retain.  Pass \code{NULL} (the default) to include all rows.
    #' @return Invisibly returns \code{self} for method chaining.
    framework_modify_schema = function(objective_codes = NULL) {
      phr_assert(
        !is.null(self$framework) && inherits(self$framework, "Framework"),
        message = phr_txt("No Framework is attached to this Protocol."),
        origin  = "Protocol$framework_modify_schema"
      )
      self$framework$modify_adjusted_schema(objective_codes)
      self$sync_framework_catalog_fields
      private$touch()
      invisible(self)
    },

    #' @description Retrieve a schema data frame directly from the attached
    #'   Framework.
    #'
    #' @param type Character. \code{"master"} (default) or \code{"adjusted"}.
    #' @return Data frame of the requested schema, or an empty \code{data.frame()}
    #'   when the Framework or the requested schema is absent.
    framework_get_schema = function(type = c("master", "adjusted")) {
      type <- match.arg(type)
      phr_assert(
        !is.null(self$framework) && inherits(self$framework, "Framework"),
        message = phr_txt("No Framework is attached to this Protocol."),
        origin  = "Protocol$framework_get_schema"
      )
      schema <- if (type == "adjusted") {
        self$framework$adjusted_schema
      } else {
        self$framework$master_schema
      }
      if (!is.null(schema) && is.data.frame(schema)) {
        return(as.data.frame(schema, stringsAsFactors = FALSE))
      }
      data.frame()
    },

    #' @description Retrieve the SVG string directly from the attached Framework.
    #'
    #' @param type Character. \code{"master"} or \code{"adjusted"} (default).
    #' @return Character string containing the SVG markup, or \code{NULL} when
    #'   unavailable.
    framework_get_svg = function(type = c("adjusted", "master")) {
      type <- match.arg(type)
      phr_assert(
        !is.null(self$framework) && inherits(self$framework, "Framework"),
        message = phr_txt("No Framework is attached to this Protocol."),
        origin  = "Protocol$framework_get_svg"
      )
      if (type == "master") {
        self$framework$master_svg
      } else {
        self$framework$adjusted_svg %||% self$framework$master_svg
      }
    },

    #' @description Empty hook for generating a Word document report.
    #'
    #' This base implementation is a no-op stub.  Subclasses (\emph{e.g.}
    #' \code{\link{IPHRAProtocol}}) override this method to produce a
    #' protocol report document.
    #'
    #' @param output_file Character. Output \code{.docx} file path.
    #'   Defaults to \code{"protocol_report.docx"}.
    #' @param reference_docx Character or \code{NULL}. Path to a custom
    #'   \code{.docx} template.
    #' @param open Logical. Open the file after writing.  Defaults to \code{FALSE}.
    #' @return Invisibly returns \code{self} for method chaining.
    generate_reach_tor = function(output_file = "protocol_report.docx",
                                  reference_docx = NULL,
                                  open = FALSE) {
      phr_message(
        phr_txt("generate_reach_tor is not implemented for this protocol type. Override in a subclass."),
        origin = "Protocol$generate_reach_tor"
      )
      invisible(self)
    }
  ),

  active = list(
    sync_framework_catalog_fields = function(value) {
      if (!missing(value)) {
        stop("sync_framework_catalog_fields is a read-only active binding.")
      }
      master_schema <- if (!is.null(self$framework) && inherits(self$framework, "Framework")) {
        self$framework$master_schema
      } else {
        NULL
      }
      adjusted_schema <- if (!is.null(self$framework) && inherits(self$framework, "Framework")) {
        self$framework$adjusted_schema
      } else {
        NULL
      }

      self$objective_catalog_master <- private$build_objective_catalog(master_schema)
      self$objective_catalog_adjusted <- private$build_objective_catalog(adjusted_schema)
      self$indicator_catalog_master <- private$build_indicator_catalog(master_schema)
      self$indicator_catalog_adjusted <- private$build_indicator_catalog(adjusted_schema)
      invisible(NULL)
    },

    sync_tool_indicator_catalog_fields = function(value) {
      if (!missing(value)) {
        stop("sync_tool_indicator_catalog_fields is a read-only active binding.")
      }
      self$tool_indicator_catalog_master <- list()
      self$tool_indicator_catalog_revised <- list()
      if (is.null(self$tools) || length(self$tools) == 0L) return(invisible(NULL))

      for (tn in names(self$tools)) {
        tool <- self$tools[[tn]]
        if (is.null(tool) || !inherits(tool, "Tool")) next
        self$tool_indicator_catalog_master[[tn]] <- as.character(tool$get_indicator_codes(prefer_revised = FALSE))
        self$tool_indicator_catalog_revised[[tn]] <- as.character(tool$get_indicator_codes(prefer_revised = TRUE))
      }
      invisible(NULL)
    },

    sync_sample_metadata_fields = function(value) {
      if (!missing(value)) {
        stop("sync_sample_metadata_fields is a read-only active binding.")
      }
      st <- NULL
      if (!is.null(self$sample_table) && inherits(self$sample_table, "Sample")) {
        st <- self$sample_table$get_sample_table()
      } else if (!is.null(self$sample_table) && is.data.frame(self$sample_table)) {
        st <- self$sample_table
      }
      if (is.null(st) || !is.data.frame(st) || nrow(st) == 0L) {
        self$metadata$sampling_strata_names <- character(0)
        self$metadata$sampling_method_flags <- list()
        return(invisible(NULL))
      }

      strata_names <- if ("stratum_name" %in% names(st)) {
        as.character(st$stratum_name)
      } else if ("Population_Name" %in% names(st)) {
        as.character(st$Population_Name)
      } else {
        as.character(st$stratum_id %||% character(0))
      }
      strata_names <- unique(strata_names[!is.na(strata_names) & nzchar(strata_names)])

      methods_used <- if ("sampling_method" %in% names(st)) {
        unique(trimws(tolower(as.character(st$sampling_method))))
      } else {
        character(0)
      }
      methods_used <- methods_used[!is.na(methods_used) & nzchar(methods_used)]
      known_methods <- c("simple_random", "proportional", "pps_cluster", "pps_rlc",
                         "systematic", "simple_random_rlc", "systematic_rlc",
                         "proportional_rlc", "purposive")
      flags <- setNames(as.list(known_methods %in% methods_used), known_methods)

      self$metadata$sampling_strata_names <- strata_names
      self$metadata$sampling_method_flags <- flags
      invisible(NULL)
    },

    sync_sampling_frame_fields = function(value) {
      if (!missing(value)) {
        stop("sync_sampling_frame_fields is a read-only active binding.")
      }
      sf <- if (!is.null(self$sampling_frame) && inherits(self$sampling_frame, "SamplingFrame")) {
        self$sampling_frame$log_df
      } else {
        NULL
      }
      if (is.null(sf) || !is.data.frame(sf) || nrow(sf) == 0L || !"stratum" %in% names(sf)) {
        self$sampling_frame_strata_names <- character(0)
      } else {
        vals <- as.character(sf$stratum)
        self$sampling_frame_strata_names <- unique(vals[!is.na(vals) & nzchar(vals)])
      }
      invisible(NULL)
    }
  ),

  private = list(
    # Update the modified_datetime timestamp.
    touch = function() {
      self$metadata$modified_datetime <- Sys.time()
      invisible(NULL)
    },

    build_objective_catalog = function(schema) {
      if (is.null(schema) || !is.data.frame(schema) || nrow(schema) == 0) return(list())
      code_col <- if ("objective_code" %in% names(schema)) "objective_code" else
        if ("short_objective" %in% names(schema)) "short_objective" else NULL
      if (is.null(code_col)) return(list())

      codes <- as.character(schema[[code_col]])
      codes <- codes[!is.na(codes) & nzchar(codes)]
      if (length(codes) == 0L) return(list())

      out <- list()
      uniq_codes <- unique(codes)
      for (code in uniq_codes) {
        idx <- which(as.character(schema[[code_col]]) == code)
        if (length(idx) == 0L) next
        s <- schema[idx, , drop = FALSE]
        first_non_empty <- function(col, default = "") {
          if (!col %in% names(s)) return(default)
          vals <- as.character(s[[col]])
          vals <- vals[!is.na(vals) & nzchar(vals)]
          if (length(vals) == 0L) default else vals[[1L]]
        }
        out[[code]] <- list(
          short_objective = first_non_empty("short_objective"),
          text_objective = first_non_empty("text_objective"),
          objective_research_question = first_non_empty("objective_research_question")
        )
      }
      out
    },

    build_indicator_catalog = function(schema) {
      if (is.null(schema) || !is.data.frame(schema) || nrow(schema) == 0 ||
          !"indicator_code" %in% names(schema)) return(list())
      codes <- as.character(schema$indicator_code)
      codes <- codes[!is.na(codes) & nzchar(codes)]
      if (length(codes) == 0L) return(list())

      out <- list()
      uniq_codes <- unique(codes)
      for (code in uniq_codes) {
        idx <- which(as.character(schema$indicator_code) == code)
        if (length(idx) == 0L) next
        s <- schema[idx, , drop = FALSE]
        first_non_empty <- function(cols, default = "") {
          for (col in cols) {
            if (!col %in% names(s)) next
            vals <- as.character(s[[col]])
            vals <- vals[!is.na(vals) & nzchar(vals)]
            if (length(vals) > 0L) return(vals[[1L]])
          }
          default
        }
        out[[code]] <- list(
          indicator_definition = first_non_empty(c("indicator_definition", "text_indicator", "indicator_name")),
          research_question = first_non_empty(c("research_question", "objective_research_question"))
        )
      }
      out
    },

    # Check for issues and discrepancies in the protocol
    check_issues = function() {
      self$issues <- list()
      
      # Check if objectives have matching indicators in tools
      all_objectives <- flatten_objectives(self$objectives)
      if (length(all_objectives) > 0 && length(self$tools) > 0) {
        obj_sectors <- unique(sapply(all_objectives, function(x) x$sector))
        
        # Placeholder: Check tool coverage (actual Tool class will define how to extract sectors)
        tool_sectors <- character(0)
        tryCatch({
          tool_sectors <- unique(sapply(self$tools, function(x) {
            if (is.list(x) && "sector" %in% names(x)) {
              return(x$sector)
            } else if (methods::is(x, "R6") && "sector" %in% names(x)) {
              return(x$sector)
            }
            return(NA_character_)
          }))
          tool_sectors <- tool_sectors[!is.na(tool_sectors)]
        }, error = function(e) {
          # Ignore extraction errors
        })
        
        missing_sectors <- setdiff(obj_sectors, tool_sectors)
        if (length(missing_sectors) > 0) {
          self$issues$tool_coverage <- paste(
            "Objectives require sectors not covered by tools:",
            paste(missing_sectors, collapse = ", ")
          )
        }
      }
      
      invisible(self)
    },

    # Create an officer doc using the REACH TOR template when available, or blank.
    create_base_doc = function(reference_docx = NULL) {
      # Caller-supplied path takes highest priority
      if (!is.null(reference_docx) && file.exists(reference_docx)) {
        return(officer::read_docx(reference_docx))
      }
      # Try the REACH TOR-based template first
      reach_path <- system.file("resources", "reach_tor_template.docx", package = "phr")
      if (nzchar(reach_path)) {
        return(officer::read_docx(reach_path))
      }
      # Fall back to the older protocol report template
      sys_path <- system.file("resources", "protocol_report_template.docx", package = "phr")
      if (nzchar(sys_path)) {
        return(officer::read_docx(sys_path))
      }
      # No template found — use a blank Word document with default styles
      officer::read_docx()
    },

    # Apply protocol schema handling in a predictable order.
    apply_protocol_schema_sections = function(doc) {
      schema <- self$protocol_schema
      if (is.null(schema) || !is.data.frame(schema) || nrow(schema) == 0) {
        return(doc)
      }
      required_cols <- c("tag_name", "handling", "condition", "default_value")
      if (!all(required_cols %in% names(schema))) {
        return(doc)
      }

      handling_order <- c(
        "row_delete", "replace", "input", "checkbox_replace",
        "conditional_replace", "calculate", "table", "image"
      )

      for (handling in handling_order) {
        schema_handling <- if ("handling" %in% names(schema) && !is.null(schema$handling)) {
          as.character(schema$handling)
        } else {
          rep("", nrow(schema))
        }
        idx <- which(schema_handling == handling)
        if (length(idx) == 0L) next
        for (i in idx) {
          row <- schema[i, required_cols, drop = FALSE]
          doc <- switch(
            handling,
            replace            = private$add_replace_section(doc, row),
            input              = private$add_input_section(doc, row),
            calculate          = private$add_calculate_section(doc, row),
            checkbox_replace   = private$add_checkbox_replace_section(doc, row),
            row_delete         = private$add_row_delete_section(doc, row),
            table              = private$add_table_section(doc, row),
            image              = private$add_image_section(doc, row),
            conditional_replace = private$add_conditional_replace_section(doc, row),
            doc
          )
        }
      }

      doc
    },

    add_replace_section = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      default_value <- as.character(row$default_value[[1L]] %||% "")
      private$.replace_schema_tag(doc, tag, default_value)
    },

    add_input_section = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      key <- sub("^@", "", tag)
      value <- if (nzchar(key) && key %in% names(self$metadata)) self$metadata[[key]] else ""
      private$.replace_schema_tag(doc, tag, as.character(value %||% ""))
    },

    add_calculate_section = function(doc, row) {
      doc
    },

    add_checkbox_replace_section = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      key <- sub("^@", "", tag)
      value <- if (nzchar(key) && key %in% names(self$metadata)) self$metadata[[key]] else FALSE
      private$.replace_schema_tag(doc, tag, if (isTRUE(value)) "X" else "\u25a1")
    },

    add_row_delete_section = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      private$.replace_schema_tag(doc, tag, "")
    },

    add_table_section = function(doc, row) {
      doc
    },

    add_image_section = function(doc, row) {
      doc
    },

    add_conditional_replace_section = function(doc, row) {
      tag <- as.character(row$tag_name[[1L]] %||% "")
      default_value <- as.character(row$default_value[[1L]] %||% "")
      key <- sub("^@", "", tag)
      value <- if (nzchar(key) && key %in% names(self$metadata)) self$metadata[[key]] else FALSE
      private$.replace_schema_tag(doc, tag, if (isTRUE(value)) default_value else "")
    },

    # Extract unique non-NA indicator names from an XLSForm survey data frame.
    extract_indicators_from_survey = function(survey) {
      if (!is.null(survey) && is.data.frame(survey) &&
          nrow(survey) > 0 && "name" %in% names(survey)) {
        unique(stats::na.omit(survey$name))
      } else {
        character(0)
      }
    },

    # Load protocol schema metadata with a blank fallback.
    .load_protocol_schema = function() {
      required_cols <- c("tag_name", "handling", "condition", "default_value")
      empty_schema <- as.data.frame(
        setNames(replicate(length(required_cols), character(0), simplify = FALSE),
                 required_cols),
        stringsAsFactors = FALSE
      )

      schema_path <- tryCatch(
        system.file("resources", "protocol_schema_blank.csv", package = "phr"),
        error = function(e) ""
      )
      if (!nzchar(schema_path) || !file.exists(schema_path)) {
        schema_path <- file.path("inst", "resources", "protocol_schema_blank.csv")
      }
      if (!file.exists(schema_path)) {
        return(empty_schema)
      }

      schema <- tryCatch(
        utils::read.csv(schema_path, stringsAsFactors = FALSE, na.strings = character(0)),
        error = function(e) NULL
      )
      if (!is.data.frame(schema)) return(empty_schema)
      for (nm in required_cols) {
        if (!nm %in% names(schema)) schema[[nm]] <- character(nrow(schema))
      }
      schema[required_cols]
    },

    .replace_schema_tag = function(doc, tag, value) {
      if (!is.character(tag) || length(tag) != 1L || !nzchar(tag)) {
        return(doc)
      }
      tryCatch(
        officer::body_replace_all_text(
          doc,
          old_value = tag,
          new_value = as.character(value %||% ""),
          fixed = TRUE
        ),
        error = function(e) doc
      )
    }
  )
)
