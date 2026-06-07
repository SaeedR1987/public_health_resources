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
  inherit = Document,
  public = list(
    #' @field framework A \code{\link{Framework}} object (e.g.
    #'   \code{\link{ANAFramework}}) that holds the master and adjusted
    #'   reference schemas and SVG diagrams for this protocol.  Assign a
    #'   Framework instance to this field to associate a conceptual framework
    #'   with the protocol.
    framework = NULL,

    #' @field protocol_schema Data frame describing TOR placeholder handling
    #'   rules.  Expected columns are \code{tag_name}, \code{handling},
    #'   \code{condition}, and \code{default_value}.
    protocol_schema = NULL,

    #' @field tools List of Tool objects (placeholder for Tool class instances)
    tools = NULL,

    #' @field valid_tool_types Character vector of allowed tool types for
    #'   \code{add_tools()}.
    valid_tool_types = c("household", "key_informant", "observation", "generic"),

    #' @field framework_objective_catalog_master Named list keyed by objective
    #'   code from \code{framework$master_objectives_schema}; each value stores
    #'   objective metadata including \code{short_objective},
    #'   \code{text_objective}, \code{objective_research_question}, and
    #'   \code{pillar}.
    framework_objective_catalog_master = list(),

    #' @field framework_objective_catalog_adjusted Named list keyed by objective
    #'   code from \code{framework$modified_objectives_schema}; each value stores
    #'   objective metadata including \code{short_objective},
    #'   \code{text_objective}, \code{objective_research_question}, and
    #'   \code{pillar}.
    framework_objective_catalog_adjusted = list(),

    #' @field framework_indicator_catalog_master Named list keyed by indicator
    #'   code from \code{framework$master_objectives_schema}; each value stores
    #'   indicator metadata.
    framework_indicator_catalog_master = list(),

    #' @field framework_indicator_catalog_adjusted Named list keyed by indicator
    #'   code from \code{framework$modified_objectives_schema}; each value stores
    #'   indicator metadata.
    framework_indicator_catalog_adjusted = list(),

    #' @field tool_indicator_catalog_master Named list keyed by tool name with
    #'   vectors of indicator codes available in master tool surveys.
    tool_indicator_catalog_master = list(),

    #' @field tool_indicator_catalog_revised Named list keyed by tool name with
    #'   vectors of indicator codes available in revised tool surveys.
    tool_indicator_catalog_revised = list(),

    #' @field tool_objective_catalog_master Nested list keyed first by tool name
    #'   and then by objective code.  Each objective entry contains the same
    #'   metadata fields as \code{framework_objective_catalog_master}
    #'   (\code{short_objective}, \code{text_objective},
    #'   \code{objective_research_question}, \code{pillar}).  Objectives are
    #'   those whose indicator codes appear in the tool's master survey.
    #'   Updated automatically by \code{sync_tool_indicator_catalog_fields}.
    tool_objective_catalog_master = list(),

    #' @field tool_objective_catalog_revised Nested list keyed first by tool
    #'   name and then by objective code.  Each objective entry contains the
    #'   same metadata fields as \code{framework_objective_catalog_master}
    #'   (\code{short_objective}, \code{text_objective},
    #'   \code{objective_research_question}, \code{pillar}).  Objectives are
    #'   those whose indicator codes appear in the tool's revised survey.
    #'   Updated automatically by \code{sync_tool_indicator_catalog_fields}.
    tool_objective_catalog_revised = list(),

    #' @field issues List of validation issues and discrepancies
    issues = list(),

    #' @field issues_coherence List of coherence issues found between the
    #'   \code{modified_objectives_schema} indicator codes and the tool indicator
    #'   codes.  Populated by \code{diagnose_coherence()}.
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
    #'   \code{master_objectives_schema} of the associated \code{\link{Framework}}.
    secondary_data = NULL,

    #' @description
    #' Creates a new Protocol object
    #' @param assessment_title Character. Title of the assessment
    #' @param country_name Character. Country where assessment takes place
    #' @param month_year Character. Month and year of data collection (e.g., "January 2024")
    #' @param framework_type Character. Type of framework to initialise.  Must be
    #'   one of \code{"none"} (creates a generic \code{\link{Framework}} object) or
    #'   \code{"ana"} (creates an \code{\link{ANAFramework}} object).
    #' @param reference_doc_filename Optional document template filename/path
    #'   passed to \code{Document$initialize()}.
    #' @return A new Protocol object
    initialize = function(assessment_title = NULL, country_name = NULL, month_year = NULL,
                          framework_type = "none", reference_doc_filename = NULL) {
      phr_try({
        super$initialize(reference_doc_filename = reference_doc_filename)
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
        self$tools <- list()
        self$valid_tool_types <- as.character(self$valid_tool_types %||% character(0))
        self$issues <- list()
        self$issues_coherence <- list()
        self$conditional_metadata <- list()
        self$framework_objective_catalog_master <- list()
        self$framework_objective_catalog_adjusted <- list()
        self$framework_indicator_catalog_master <- list()
        self$framework_indicator_catalog_adjusted <- list()
        self$tool_indicator_catalog_master <- list()
        self$tool_indicator_catalog_revised <- list()
        self$tool_objective_catalog_master <- list()
        self$tool_objective_catalog_revised <- list()
        self$protocol_schema <- private$.load_protocol_schema()

        self$framework <- if (framework_type == "ana") {
          ANAFramework$new()
        } else {
          Framework$new()
        }
        self$sync_framework_catalog_fields
        self$sync_tool_indicator_catalog_fields

        phr_message(phr_txt("Protocol initialized."), origin = "Protocol$initialize")
      }, on_error = "abort", origin = "Protocol$initialize")
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
        valid_types <- self$valid_tool_types %||% character(0)
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
        private$.touch()
        self$diagnose_coherence()
        phr_message(
          phr_txt("Tool of type '{tool_type}' added as '{tool_name}'."),
          origin = "Protocol$add_tools"
        )
      }, on_error = "abort", origin = "Protocol$add_tools")
      invisible(self)
    },

    #' @description Get all issues
    #' @return List of validation issues
    get_issues = function() {
      return(self$issues)
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
        private$.touch()
        phr_message(
          phr_txt("Metadata updated: {paste(names(args), collapse=', ')}."),
          origin = "Protocol$update_metadata"
        )
      }, on_error = "abort", origin = "Protocol$update_metadata")
      invisible(self)
    },

    # ── Schema / Framework helpers ─────────────────────────────────────────

    #' @description Return all unique, non-NA indicator codes present in a
    #'   schema.
    #'
    #' @param type Character. One of \code{"master"} (default) or
    #'   \code{"adjusted"} indicating which framework schema to read.
    #' @return Character vector of unique indicator codes.  Empty character
    #'   vector when none are found.
    get_indicator_codes_from_schema = function(type = c("master", "adjusted")) {
      schema <- self$get_schema(type = match.arg(type))
      private$.touch()
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
    get_schema_for_indicator_codes = function(indicator_codes,
                                              type = c("master", "adjusted")) {
      schema <- self$get_schema(type = match.arg(type))
      if (!is.data.frame(schema) || nrow(schema) == 0 ||
          !"indicator_code" %in% names(schema)) {
        private$.touch()
        return(data.frame())
      }
      ic <- as.character(indicator_codes)
      out <- schema[as.character(schema$indicator_code) %in% ic, , drop = FALSE]
      private$.touch()
      out
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

    #' @description Validate an objective schema data frame.
    #'
    #' Validates that \code{schema} is a non-NULL data frame with at least one
    #' row, contains all required columns, and passes completeness and type
    #' checks.
    #'
    #' @param schema Data frame to validate as an objective schema.
    #' @param soft Logical. When \code{TRUE} issues warnings rather than errors
    #'   for recoverable problems.  Defaults to \code{FALSE}.
    #' @return Invisibly returns \code{TRUE} if valid.
    validate_objective_schema = function(schema, soft = FALSE) {
      phr_try({
        origin <- "Protocol$validate_objective_schema"

        if (is.null(schema) || !is.data.frame(schema)) {
          phr_error(
            origin  = origin,
            message = phr_txt("Objective schema must be a data frame."),
            hint    = phr_txt("Use load_objective_schema() to obtain the default schema.")
          )
        }

        if (nrow(schema) == 0) {
          msg <- phr_txt("Objective schema is empty (zero rows).")
          if (soft) {
            phr_warning(origin = origin, message = msg)
            return(invisible(FALSE))
          }
          phr_error(origin = origin, message = msg)
        }

        missing_cols <- setdiff(.objective_schema_required_cols, names(schema))
        if (length(missing_cols) > 0) {
          phr_error(
            origin  = origin,
            message = phr_txt(
              glue::glue(
                "Objective schema is missing required column(s): {paste(missing_cols, collapse = ', ')}"
              )
            ),
            hint = phr_txt(
              glue::glue(
                "Required columns are: {paste(.objective_schema_required_cols, collapse = ', ')}"
              )
            )
          )
        }

        if (all(is.na(schema$sector))) {
          phr_error(
            origin  = origin,
            message = phr_txt("All 'sector' values in the objective schema are NA.")
          )
        }

        if (all(is.na(schema$short_objective))) {
          msg <- phr_txt("All 'short_objective' values in the objective schema are NA.")
          if (soft) {
            phr_warning(origin = origin, message = msg)
          } else {
            phr_error(origin = origin, message = msg)
          }
        }

        char_cols <- c("sector", "pillar", "sub_pillar", "text_objective")
        bad_types <- char_cols[sapply(char_cols, function(col) {
          col %in% names(schema) && !is.character(schema[[col]]) &&
            !is.factor(schema[[col]])
        })]
        if (length(bad_types) > 0) {
          msg <- phr_txt(
            glue::glue(
              "The following column(s) should be character (or factor): {paste(bad_types, collapse = ', ')}"
            )
          )
          if (soft) {
            phr_warning(origin = origin, message = msg)
          } else {
            phr_error(origin = origin, message = msg)
          }
        }

        invisible(TRUE)
      }, on_error = "abort", origin = "Protocol$validate_objective_schema")
    },

    #' @description Diagnose coherence between the \code{modified_objectives_schema}
    #' indicator codes and the \code{indicator_code} values across all tools in
    #' \code{self$tools} (using each tool's \code{revised_survey}).
    #'
    #' Checks performed:
    #' \enumerate{
    #'   \item Objectives in the legacy \code{objectives} list whose sectors are
    #'     not represented by any registered tool.
    #'   \item Objectives in \code{modified_objectives_schema} that have no
    #'     matching \code{indicator_code} in any tool's \code{revised_survey}.
    #'   \item \code{indicator_code} values present in tools but absent from
    #'     \code{modified_objectives_schema}.
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

      schema <- self$framework$modified_objectives_schema
      if (is.null(schema) || !is.data.frame(schema) || nrow(schema) == 0) {
        self$issues_coherence$no_schema <-
          "modified_objectives_schema is empty or not set in the framework."
        return(invisible(self))
      }

      has_obj_col <- "objective_code" %in% names(schema)
      has_ind_col <- "indicator_code"  %in% names(schema)

      if (!has_obj_col || !has_ind_col) {
        self$issues_coherence$schema_columns <- paste0(
          "modified_objectives_schema must contain 'objective_code' and 'indicator_code' columns. ",
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
            "modified_objectives_schema: ", paste(unmatched, collapse = ", ")
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

    # ── Framework convenience methods ──────────────────────────────────────

    #' @description Set primary objectives on the attached Framework.
    #' @param objective_codes Numeric vector of primary objective codes.
    #' @return Invisibly returns \code{self} for method chaining.
    set_primary_objectives = function(objective_codes) {
      self$access_nested("framework", member = "set_primary_objectives", objective_codes)
      invisible(self)
    },

    #' @description Set secondary objectives on the attached Framework.
    #' @param objective_codes Numeric vector of secondary objective codes.
    #' @return Invisibly returns \code{self} for method chaining.
    set_secondary_objectives = function(objective_codes) {
      self$access_nested("framework", member = "set_secondary_objectives", objective_codes)
      invisible(self)
    },

    #' @description Set primary indicator codes on the attached Framework.
    #' @param indicator_codes Character/numeric vector (or list) of indicator
    #'   codes.
    #' @return Invisibly returns \code{self}.
    set_primary_indicators = function(indicator_codes) {
      self$access_nested("framework", member = "set_primary_indicators", indicator_codes)
      invisible(self)
    },

    #' @description Set secondary indicator codes on the attached Framework.
    #' @param indicator_codes Character/numeric vector (or list) of indicator
    #'   codes.
    #' @return Invisibly returns \code{self}.
    set_secondary_indicators = function(indicator_codes) {
      self$access_nested("framework", member = "set_secondary_indicators", indicator_codes)
      invisible(self)
    },

    #' @description Modify the adjusted schema on the attached Framework.
    #' @param objective_codes Character or numeric vector of objective codes.
    #' @return Invisibly returns \code{self}.
    modify_schema = function(objective_codes = NULL) {
      self$access_nested("framework", member = "modify_adjusted_schema", objective_codes)
      invisible(self)
    },

    #' @description Modify the adjusted SVG on the attached Framework.
    #' @param primary_objective_codes Numeric vector of primary objective codes.
    #' @param secondary_objective_codes Numeric vector of secondary objective
    #'   codes.
    #' @return Invisibly returns \code{self}.
    modify_svg = function(primary_objective_codes = NULL,
                          secondary_objective_codes = NULL) {
      self$access_nested(
        "framework",
        member = "modify_adjusted_svg",
        primary_objective_codes = primary_objective_codes,
        secondary_objective_codes = secondary_objective_codes
      )
      invisible(self)
    },

    #' @description Retrieve a schema data frame from the attached Framework.
    #' @param type Character. \code{"master"} (default) or \code{"adjusted"}.
    #' @return Data frame of the requested schema.
    get_schema = function(type = c("master", "adjusted")) {
      type <- match.arg(type)
      schema <- self$access_nested(
        "framework",
        member = if (type == "adjusted") "modified_objectives_schema" else "master_objectives_schema"
      )
      if (!is.null(schema) && is.data.frame(schema)) {
        return(as.data.frame(schema, stringsAsFactors = FALSE))
      }
      data.frame()
    },

    #' @description Render framework SVG.
    #' @param type Character. \code{"master"} or \code{"adjusted"} (default).
    #' @return Result of \code{Framework$render_framework_svg()}.
    get_framework_svg = function(type = c("adjusted", "master")) {
      type <- match.arg(type)
      self$access_nested("framework", member = "render_framework_svg", version = type)
    },

    #' @description Generate a document report from the current schema/template.
    #' @param output_file Character output \code{.docx} path.
    #' @param open Logical indicating whether to open the output path.
    #' @return Invisibly returns \code{self}.
    generate_doc = function(output_file = "protocol_report.docx", open = FALSE) {
      super$generate_doc(output_file = output_file, open = open)
    }
  ),

  active = list(
    sync_framework_catalog_fields = function(value) {
      if (!missing(value)) {
        stop("sync_framework_catalog_fields is a read-only active binding.")
      }
      master_schema <- if (!is.null(self$framework) && inherits(self$framework, "Framework")) {
        self$framework$master_objectives_schema
      } else {
        NULL
      }
      adjusted_schema <- if (!is.null(self$framework) && inherits(self$framework, "Framework")) {
        self$framework$modified_objectives_schema
      } else {
        NULL
      }

      self$framework_objective_catalog_master <- private$build_objective_catalog(master_schema)
      self$framework_objective_catalog_adjusted <- private$build_objective_catalog(adjusted_schema)
      self$framework_indicator_catalog_master <- private$build_indicator_catalog(master_schema)
      self$framework_indicator_catalog_adjusted <- private$build_indicator_catalog(adjusted_schema)
      invisible(NULL)
    },

    sync_tool_indicator_catalog_fields = function(value) {
      if (!missing(value)) {
        stop("sync_tool_indicator_catalog_fields is a read-only active binding.")
      }
      self$tool_indicator_catalog_master <- list()
      self$tool_indicator_catalog_revised <- list()
      self$tool_objective_catalog_master <- list()
      self$tool_objective_catalog_revised <- list()
      if (is.null(self$tools) || length(self$tools) == 0L) return(invisible(NULL))

      # Get the Framework master objectives schema for objective lookups
      fw_schema <- if (!is.null(self$framework) && inherits(self$framework, "Framework")) {
        self$framework$master_objectives_schema
      } else {
        NULL
      }

      for (tn in names(self$tools)) {
        tool <- self$tools[[tn]]
        if (is.null(tool) || !inherits(tool, "Tool")) next
        master_codes  <- as.character(tool$get_indicator_codes(prefer_revised = FALSE))
        revised_codes <- as.character(tool$get_indicator_codes(prefer_revised = TRUE))
        self$tool_indicator_catalog_master[[tn]] <- master_codes
        self$tool_indicator_catalog_revised[[tn]] <- revised_codes
        # Build per-tool objective catalogs from Framework schema rows
        # matching the tool's indicator codes
        self$tool_objective_catalog_master[[tn]] <-
          private$build_tool_objective_catalog(fw_schema, master_codes)
        self$tool_objective_catalog_revised[[tn]] <-
          private$build_tool_objective_catalog(fw_schema, revised_codes)
      }
      invisible(NULL)
    }

  ),

  private = list(
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
          objective_research_question = first_non_empty("objective_research_question"),
          pillar = first_non_empty("pillar")
        )
      }
      out
    },

    # Build an objective catalog for a single tool by finding the objectives in
    # fw_schema whose indicator_code rows match the supplied indicator_codes.
    build_tool_objective_catalog = function(fw_schema, indicator_codes) {
      if (is.null(fw_schema) || !is.data.frame(fw_schema) || nrow(fw_schema) == 0 ||
          length(indicator_codes) == 0L) {
        return(list())
      }
      if (!"indicator_code" %in% names(fw_schema)) return(list())
      # Filter schema rows to those matching the tool's indicator codes
      matching_rows <- fw_schema[
        as.character(fw_schema$indicator_code) %in% as.character(indicator_codes),
        , drop = FALSE
      ]
      if (nrow(matching_rows) == 0L) return(list())
      private$build_objective_catalog(matching_rows)
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
    }
  )
)
