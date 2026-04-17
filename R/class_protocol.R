#' Protocol R6 Class
#'
#' @description
#' Base class for managing protocol pipeline workflows.
#' Handles core components shared across all protocol types:
#' 1. Objective Selection (nested by sector, pillar, sub-pillar, and data source)
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
    #' @field objectives Nested list of research objectives structured as
    #'   \code{sector → pillar → sub_pillar → data_source → [list of objectives]}.
    #'   Use \code{add_objective()}, \code{remove_objective()}, and
    #'   \code{set_objectives()} to manage; use \code{flatten_objectives()} or
    #'   \code{objectives_to_df()} to inspect.
    objectives = NULL,

    #' @field objective_schema Data frame containing the loaded objective schema
    objective_schema = NULL,

    #' @field tools List of Tool objects (placeholder for Tool class instances)
    tools = NULL,

    #' @field selected_indicators List of selected indicators
    selected_indicators = NULL,

    #' @field issues List of validation issues and discrepancies
    issues = list(),

    #' @field metadata List containing protocol metadata
    metadata = list(
      created_date = NULL,
      modified_date = NULL,
      month_year = NULL,
      country_name = NULL,
      assessment_title = NULL,
      target_strata = list(),
      protocol_version = "1.0"
    ),

    #' @description
    #' Creates a new Protocol object
    #' @param assessment_title Character. Title of the assessment
    #' @param country_name Character. Country where assessment takes place
    #' @param month_year Character. Month and year of data collection (e.g., "January 2024")
    #' @return A new Protocol object
    initialize = function(assessment_title = NULL, country_name = NULL, month_year = NULL) {
      phr_try({
        self$metadata$created_date <- Sys.time()
        self$metadata$modified_date <- Sys.time()
        self$metadata$assessment_title <- assessment_title
        self$metadata$country_name <- country_name
        self$metadata$month_year <- month_year
        self$objectives <- list()
        self$tools <- list()
        self$issues <- list()
        self$objective_schema <- private$default_objective_schema()
        phr_message(phr_txt("Protocol initialized."), origin = "Protocol$initialize")
      }, on_error = "abort", origin = "Protocol$initialize")
      invisible(self)
    },
    
    #' @description Set all objectives at once, replacing the current objectives
    #'
    #' Accepts either a flat list of objectives (as produced by
    #' \code{create_objective()} or \code{create_objectives_from_df()}) or an
    #' already-nested structure (sector → pillar → sub_pillar → data_source).
    #' Flat lists are automatically converted with \code{nest_objectives()}.
    #'
    #' @param objectives List.  Flat or nested objectives.
    #' @return Invisibly returns \code{self} for method chaining.
    set_objectives = function(objectives) {
      phr_try({
        phr_assert(is.list(objectives),
                   message = phr_txt("objectives must be a list."),
                   origin  = "Protocol$set_objectives")

        # Detect flat vs nested using robust check (all elements have $short_objective).
        # Empty lists are assigned directly (treated as empty nested structure).
        if (.is_flat_objectives(objectives)) {
          self$objectives <- nest_objectives(objectives)
        } else {
          self$objectives <- objectives
        }

        self$metadata$modified_date <- Sys.time()
        private$check_issues()
        phr_message(
          phr_txt("{count_objectives(self$objectives)} objective(s) set."),
          origin = "Protocol$set_objectives"
        )
      }, on_error = "abort", origin = "Protocol$set_objectives")
      invisible(self)
    },

    #' @description Add a single objective to the protocol
    #'
    #' The objective is placed into the nested structure under its
    #' \code{sector}, \code{pillar}, \code{sub_pillar}, and \code{data_source}
    #' keys.  Use \code{create_objective()} to build a conforming objective
    #' list.
    #'
    #' @param objective Named list. Objective to add (see \code{create_objective()}).
    #' @return Invisibly returns \code{self} for method chaining.
    add_objective = function(objective) {
      phr_try({
        phr_assert(
          is.list(objective),
          message = phr_txt("objective must be a named list."),
          origin  = "Protocol$add_objective",
          hint    = phr_txt("Use create_objective() to build a conforming objective.")
        )

        required_fields <- c("sector", "pillar", "sub_pillar", "short_objective", "text_objective")
        missing <- setdiff(required_fields, names(objective))
        if (length(missing) > 0) {
          phr_error(
            message = phr_txt("Objective missing required fields: {paste(missing, collapse=', ')}"),
            origin  = "Protocol$add_objective",
            hint    = phr_txt("Use create_objective() to build a conforming objective list.")
          )
        }

        # Default data_source to "primary" when absent or NA
        ds <- .normalize_data_source(objective$data_source)

        s  <- objective$sector
        p  <- objective$pillar
        sp <- objective$sub_pillar

        if (is.null(self$objectives[[s]]))            self$objectives[[s]]            <- list()
        if (is.null(self$objectives[[s]][[p]]))       self$objectives[[s]][[p]]       <- list()
        if (is.null(self$objectives[[s]][[p]][[sp]])) self$objectives[[s]][[p]][[sp]] <- list()
        if (is.null(self$objectives[[s]][[p]][[sp]][[ds]])) self$objectives[[s]][[p]][[sp]][[ds]] <- list()

        self$objectives[[s]][[p]][[sp]][[ds]] <- c(
          self$objectives[[s]][[p]][[sp]][[ds]],
          list(objective)
        )

        self$metadata$modified_date <- Sys.time()
        private$check_issues()
        phr_message(
          phr_txt("Objective '{objective$short_objective}' added [{ds}]."),
          origin = "Protocol$add_objective"
        )
      }, on_error = "abort", origin = "Protocol$add_objective")
      invisible(self)
    },

    #' @description Remove an objective from the protocol by its short_objective label
    #'
    #' Searches the entire nested objectives structure and removes the first
    #' objective whose \code{short_objective} matches the supplied value.
    #'
    #' @param short_objective Character. The \code{short_objective} value of the
    #'   objective to remove.
    #' @return Invisibly returns \code{self} for method chaining.
    remove_objective = function(short_objective) {
      phr_try({
        found <- FALSE

        for (s in names(self$objectives)) {
          for (p in names(self$objectives[[s]])) {
            for (sp in names(self$objectives[[s]][[p]])) {
              for (ds in names(self$objectives[[s]][[p]][[sp]])) {
                before <- length(self$objectives[[s]][[p]][[sp]][[ds]])
                self$objectives[[s]][[p]][[sp]][[ds]] <- Filter(
                  function(x) !identical(x$short_objective, short_objective),
                  self$objectives[[s]][[p]][[sp]][[ds]]
                )
                if (length(self$objectives[[s]][[p]][[sp]][[ds]]) < before) {
                  found <- TRUE
                }
              }
            }
          }
        }

        if (!found) {
          phr_warning(
            message = phr_txt("No objective with short_objective '{short_objective}' was found."),
            origin  = "Protocol$remove_objective"
          )
        }

        self$metadata$modified_date <- Sys.time()
        private$check_issues()
      }, on_error = "abort", origin = "Protocol$remove_objective")
      invisible(self)
    },
    
    #' @description Add a target stratum to metadata
    #' @param stratum_id Character. Unique identifier
    #' @param stratum_name Character. Human-readable name
    add_target_stratum = function(stratum_id, stratum_name) {
      self$metadata$target_strata[[stratum_id]] <- stratum_name
      self$metadata$modified_date <- Sys.time()
      invisible(self)
    },
    
    #' @description Add a single Tool object to the protocol by specifying its type.
    #' A new tool of the requested type is instantiated (loading its bundled
    #' default XLSForm template) and appended to the \code{tools} list.
    #' Call this method once per tool you wish to add.
    #' @param tool_type Character. Type of tool to create.  One of
    #'   \code{"household"}, \code{"key_informant"}, \code{"observation"}, or
    #'   \code{"generic"}.  Defaults to \code{"household"}.
    #' @param tool_name Optional character. Name for the new tool.
    #' @return Invisibly returns self for method chaining.
    add_tools = function(tool_type = "household", tool_name = NULL) {
      phr_try({
        valid_types <- c("household", "key_informant", "observation", "generic")
        phr_assert(
          tool_type %in% valid_types,
          message = phr_txt("tool_type must be one of: {paste(valid_types, collapse=', ')}."),
          origin  = "Protocol$add_tools"
        )

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

        self$tools <- c(self$tools, list(tool))
        self$metadata$modified_date <- Sys.time()
        private$check_issues()
        phr_message(phr_txt("Tool of type '{tool_type}' added."), origin = "Protocol$add_tools")
      }, on_error = "abort", origin = "Protocol$add_tools")
      invisible(self)
    },
    
    #' @description Select indicators for data collection
    #' @param indicator_list List of indicators
    select_indicators = function(indicator_list) {
      self$selected_indicators <- indicator_list
      self$metadata$modified_date <- Sys.time()
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
        modified = self$metadata$modified_date,
        num_objectives = count_objectives(self$objectives),
        num_tools = length(self$tools),
        num_issues = length(self$issues)
      )
    },
    
    #' @description Export protocol to a list
    #' @return List containing all protocol data
    export_protocol = function() {
      list(
        metadata = self$metadata,
        objectives = self$objectives,
        objective_schema = self$objective_schema,
        tools = self$tools,
        selected_indicators = self$selected_indicators,
        issues = self$issues,
        summary = self$get_protocol_summary()
      )
    }
  ),

  private = list(
    # Load the default objective schema from the bundled reference.xlsx file
    default_objective_schema = function() {
      load_objective_schema()
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
    }
  )
)
