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
    },

    #' @description Generate a Word document report based on the REACH TOR template
    #'
    #' Creates a \code{.docx} file based on the bundled REACH Terms of Reference
    #' template (\code{inst/resources/reach_tor_template.docx}).  The document
    #' inherits the template's formatting, branding, section structure (Executive
    #' Summary, Rationale, Methodology, DAP, etc.) and key placeholder strings are
    #' replaced with protocol metadata.  A new \strong{Protocol Data} section is
    #' inserted directly after the Executive Summary table and contains:
    #' \itemize{
    #'   \item Research Objectives table
    #'   \item Data Collection Tools (one sub-section per tool with indicator list)
    #' }
    #'
    #' @param output_file Character. Output file path including the \code{.docx}
    #'   extension.  Defaults to \code{"protocol_report.docx"} in the current
    #'   working directory.
    #' @param reference_docx Character or \code{NULL}.  Path to a custom
    #'   \code{.docx} template.  When \code{NULL} (default) the package-bundled
    #'   REACH TOR template is used.
    #' @param open Logical.  Whether to open the generated file after writing.
    #'   Defaults to \code{FALSE}.
    #' @return Invisibly returns \code{self} for method chaining.
    generate_report = function(output_file = "protocol_report.docx",
                               reference_docx = NULL,
                               open = FALSE) {
      phr_try({
        doc <- private$create_base_doc(reference_docx)
        doc <- private$add_metadata_section(doc)
        doc <- private$add_objectives_section(doc)
        doc <- private$add_tools_section(doc)

        print(doc, target = output_file)
        phr_message(
          phr_txt("Protocol report saved to: {output_file}"),
          origin = "Protocol$generate_report"
        )
        if (isTRUE(open)) utils::browseURL(output_file)
      }, on_error = "abort", origin = "Protocol$generate_report")
      invisible(self)
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

    # Replace REACH TOR template placeholders with actual metadata, then
    # insert a "Protocol Data" section heading before the "Rationale" heading
    # so that all protocol-specific content sits directly after the Executive
    # Summary table in the finished document.
    add_metadata_section = function(doc) {
      title   <- self$metadata$assessment_title %||% "Protocol Report"
      country <- self$metadata$country_name     %||% ""
      period  <- self$metadata$month_year       %||% ""
      version <- self$metadata$protocol_version %||% "1.0"
      date_str <- format(Sys.Date(), "%d/%m/%Y")

      # Build objective summary text for the General Objective / Specific
      # Objectives / Research Questions cells in the Executive Summary table.
      all_objectives <- flatten_objectives(self$objectives)

      gen_obj_text <- if (length(all_objectives) > 0) {
        all_objectives[[1]]$text_objective %||% title
      } else {
        title
      }

      spec_obj_text <- if (length(all_objectives) > 0) {
        paste(vapply(seq_along(all_objectives), function(i) {
          obj <- all_objectives[[i]]
          paste0(i, ". ", obj$text_objective %||% "")
        }, character(1)), collapse = "; ")
      } else {
        ""
      }

      rq_text <- if (length(all_objectives) > 0) {
        paste(vapply(seq_along(all_objectives), function(i) {
          obj <- all_objectives[[i]]
          paste0(i, ". ", obj$short_objective %||% "")
        }, character(1)), collapse = "; ")
      } else {
        ""
      }

      # Placeholder → replacement pairs (REACH TOR template placeholders).
      # Single-run placeholders (reliable cross-run replacements marked below).
      replace_pairs <- list(
        c("[Research Cycle title]", title),
        c("[Country]",              country),
        c("[Release date]",         date_str),
        c("[Version number]",       paste0("v", version))
      )
      if (nzchar(country)) {
        replace_pairs <- c(replace_pairs, list(
          c("[Specify name(s) here]", country),
          c("[Describe here the geographic area that the research aims to provide findings about]",
            country)
        ))
      }
      if (nzchar(gen_obj_text)) {
        replace_pairs <- c(replace_pairs, list(
          c(paste0("[Describe here what this research aims to inform \u2013",
                   " for specific guidance on this, see Step 1.2 of the",
                   " IMPACT Research Design Guidelines]"),
            gen_obj_text)
        ))
      }
      if (nzchar(spec_obj_text)) {
        replace_pairs <- c(replace_pairs, list(
          c(paste0("[List here what the research aims to identify to facilitate",
                   " the general objective \u2013 for specific guidance on this,",
                   " see Step 1.2 of the IMPACT Research Design Guidelines]"),
            spec_obj_text)
        ))
      }
      if (nzchar(rq_text)) {
        replace_pairs <- c(replace_pairs, list(
          c(paste0("[List here the research questions that will need to be answered",
                   " to meet the objectives\u2013 for specific guidance on this,",
                   " see Step 2.1 of the IMPACT Research Design Guidelines]"),
            rq_text)
        ))
      }

      for (rp in replace_pairs) {
        tryCatch(
          doc <- officer::body_replace_all_text(doc,
                                                old_value = rp[1],
                                                new_value = rp[2],
                                                fixed     = TRUE),
          error = function(e) NULL
        )
      }

      # Navigate to "Rationale" heading (REACH TOR section) and insert the
      # "Protocol Data" section heading immediately before it, so our
      # structured content sits right after the Executive Summary table.
      meta_line <- paste(
        Filter(nzchar, c(
          if (nzchar(country)) paste0("Country: ", country),
          if (nzchar(period))  paste0("Period: ",  period),
          paste0("Generated: ", format(Sys.Date(), "%d %B %Y")),
          paste0("Version: ", version)
        )),
        collapse = "  |  "
      )

      doc <- tryCatch({
        doc <- officer::cursor_begin(doc)
        doc <- officer::cursor_reach(doc, keyword = "Rationale")
        doc <- officer::body_add_par(doc, "Protocol Data", style = "heading 1", pos = "before")
        doc <- officer::body_add_par(doc, meta_line, style = "Normal", pos = "after")
        doc
      }, error = function(e) {
        # Fallback when "Rationale" section is absent (e.g. custom template).
        doc <- officer::cursor_end(doc)
        doc <- officer::body_add_par(doc, "Protocol Data", style = "heading 1")
        doc <- officer::body_add_par(doc, meta_line, style = "Normal")
        doc
      })

      doc
    },

    # Add the research objectives section after the current cursor position.
    add_objectives_section = function(doc) {
      doc <- officer::body_add_par(doc, "Research Objectives", style = "heading 2", pos = "after")

      all_objectives <- flatten_objectives(self$objectives)
      if (length(all_objectives) == 0) {
        return(officer::body_add_par(
          doc, "No objectives have been defined.", style = "Normal", pos = "after"
        ))
      }

      obj_df <- do.call(rbind, lapply(all_objectives, function(x) {
        data.frame(
          Sector        = as.character(x$sector          %||% ""),
          Pillar        = as.character(x$pillar          %||% ""),
          "Sub-Pillar"  = as.character(x$sub_pillar      %||% ""),
          ID            = as.character(x$short_objective %||% ""),
          Objective     = as.character(x$text_objective  %||% ""),
          "Data Source" = as.character(x$data_source     %||% "primary"),
          check.names   = FALSE,
          stringsAsFactors = FALSE
        )
      }))

      ft <- flextable::flextable(obj_df)
      ft <- flextable::theme_zebra(ft)
      ft <- flextable::autofit(ft)
      flextable::body_add_flextable(doc, ft, pos = "after")
    },

    # Add the data collection tools section after the current cursor position.
    # Skipped (with a placeholder note) when self$tools is empty.
    add_tools_section = function(doc) {
      doc <- officer::body_add_par(doc, "Data Collection Tools", style = "heading 2", pos = "after")

      if (length(self$tools) == 0) {
        return(officer::body_add_par(
          doc, "No data collection tools have been defined.", style = "Normal", pos = "after"
        ))
      }

      for (i in seq_along(self$tools)) {
        tool <- self$tools[[i]]

        if (methods::is(tool, "R6")) {
          tool_name  <- tryCatch(tool$get_name(),      error = function(e) {
            phr_warning(phr_txt("Could not read name for tool {i}: {conditionMessage(e)}"),
                        origin = "Protocol$add_tools_section")
            paste("Tool", i)
          })
          tool_type  <- tryCatch(tool$get_tool_type(), error = function(e) {
            phr_warning(phr_txt("Could not read type for tool {i}: {conditionMessage(e)}"),
                        origin = "Protocol$add_tools_section")
            "unknown"
          })
          survey     <- tryCatch(tool$get_survey(),    error = function(e) {
            phr_warning(phr_txt("Could not read survey sheet for tool {i}: {conditionMessage(e)}"),
                        origin = "Protocol$add_tools_section")
            NULL
          })
          indicators <- private$extract_indicators_from_survey(survey)
        } else {
          tool_name  <- tool$tool_name  %||% paste("Tool", i)
          tool_type  <- tool$tool_type  %||% "unknown"
          indicators <- tool$indicators %||% character(0)
        }

        doc <- officer::body_add_par(
          doc,
          paste0(i, ". ", tool_name, " (", tool_type, ")"),
          style = "heading 3",
          pos   = "after"
        )

        if (length(indicators) > 0) {
          ind_df <- data.frame(Indicator = as.character(indicators), stringsAsFactors = FALSE)
          ft <- flextable::flextable(ind_df)
          ft <- flextable::autofit(ft)
          doc <- flextable::body_add_flextable(doc, ft, pos = "after")
        } else {
          doc <- officer::body_add_par(
            doc, "No indicators defined for this tool.", style = "Normal", pos = "after"
          )
        }
      }

      doc
    },

    # Extract unique non-NA indicator names from an XLSForm survey data frame.
    # Returns an empty character vector when the survey is absent or has no
    # 'name' column.
    extract_indicators_from_survey = function(survey) {
      if (!is.null(survey) && is.data.frame(survey) &&
          nrow(survey) > 0 && "name" %in% names(survey)) {
        unique(stats::na.omit(survey$name))
      } else {
        character(0)
      }
    }
  )
)
