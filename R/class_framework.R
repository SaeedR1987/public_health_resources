# Internal helper: parse an SVG string and return a named list that maps each
# objective_code (as a character string) to a character vector of SVG <g> group
# ids that carry that code as a text label.
#
# Three label formats are recognised:
#   1. Leaf sub-pillar blocks whose <text> element contains "(CODE)" in
#      parentheses, e.g. "HH Consump. (112)".
#   2. Parent blocks whose second <text> element is exactly "OC: CODE",
#      e.g. "OC: 105".
#   3. Parent blocks with a code range "OC: CODE1-CODE2",
#      e.g. "OC: 152-154" — all codes in the range are mapped.
#
# The function works by splitting the SVG markup on </g> so that each
# chunk contains at most one <g id="..."> opening tag. It never reads
# reference.xlsx and never uses the sub_pillar column.
.build_code_svg_map <- function(svg) {
  code_map <- list()   # named list: code_str -> character vector of group ids

  chunks <- strsplit(svg, "</g>", fixed = TRUE)[[1]]

  for (chunk in chunks) {
    id_match <- regmatches(chunk, regexpr('<g id="([^"]+)"', chunk, perl = TRUE))
    if (length(id_match) == 0L || !nzchar(id_match)) next
    id_val <- sub('<g id="([^"]+)"', "\\1", id_match, perl = TRUE)

    # Pattern 1: leaf node – text contains "(CODE)"
    leaf_match <- regmatches(chunk, regexpr("\\((\\d+)\\)", chunk, perl = TRUE))
    if (length(leaf_match) > 0L && nzchar(leaf_match)) {
      code_str <- sub("\\((\\d+)\\)", "\\1", leaf_match, perl = TRUE)
      code_map[[code_str]] <- c(code_map[[code_str]], id_val)
      next
    }

    # Pattern 2: range "OC: N-M"
    range_match <- regmatches(chunk, regexpr("OC:\\s*(\\d+)-(\\d+)", chunk, perl = TRUE))
    if (length(range_match) > 0L && nzchar(range_match)) {
      from_c <- as.integer(sub("OC:\\s*(\\d+)-(\\d+)", "\\1", range_match, perl = TRUE))
      to_c   <- as.integer(sub("OC:\\s*(\\d+)-(\\d+)", "\\2", range_match, perl = TRUE))
      for (c in seq(from_c, to_c)) {
        code_str <- as.character(c)
        code_map[[code_str]] <- c(code_map[[code_str]], id_val)
      }
      next
    }

    # Pattern 3: single "OC: N"
    oc_match <- regmatches(chunk, regexpr("OC:\\s*(\\d+)", chunk, perl = TRUE))
    if (length(oc_match) > 0L && nzchar(oc_match)) {
      code_str <- sub("OC:\\s*(\\d+)", "\\1", oc_match, perl = TRUE)
      code_map[[code_str]] <- c(code_map[[code_str]], id_val)
    }
  }

  code_map
}


#' Framework R6 Class
#'
#' @description
#' Base class for conceptual frameworks used in protocol planning.
#' A Framework holds a master reference schema (all objectives and indicators),
#' an adjusted schema filtered to the currently selected objectives, a
#' master SVG diagram representing the full conceptual framework, and an
#' adjusted SVG diagram derived from the master and trimmed to the selected
#' objectives.
#'
#' Subclasses are responsible for loading domain-specific master schemas and
#' master SVG diagrams.  Use \code{\link{ANAFramework}} for the ANA
#' (Area-based Needs Assessment) conceptual framework.
#'
#' @importFrom R6 R6Class
#' @export
Framework <- R6::R6Class(
  "Framework",
  public = list(
    #' @field master_schema Data frame containing the full reference schema with
    #'   all available objectives and indicators.
    master_schema = NULL,

    #' @field adjusted_schema Data frame derived from \code{master_schema} and
    #'   filtered to the currently selected objectives.
    adjusted_schema = NULL,

    #' @field master_svg Character string with the full SVG diagram for the
    #'   conceptual framework.  Can be set with \code{set_master_svg()}.
    master_svg = NULL,

    #' @field adjusted_svg Character string with an SVG diagram derived from
    #'   \code{master_svg} and modified to reflect the selected objectives.
    adjusted_svg = NULL,

    #' @field primary_objectives Numeric vector of objective codes selected as
    #'   primary data collection objectives.  Used by
    #'   \code{modify_adjusted_svg()} to colour the corresponding sub-pillar
    #'   blocks light green in the adjusted SVG.
    primary_objectives = NULL,

    #' @field secondary_objectives Numeric vector of objective codes selected as
    #'   secondary data objectives.  Used by \code{modify_adjusted_svg()} to
    #'   colour the corresponding sub-pillar blocks light blue in the adjusted
    #'   SVG.
    secondary_objectives = NULL,

    #' @field available_indicator_codes Named list of tool type and
    #'   \code{indicator_code} values derived from \code{adjusted_schema}.
    #'   Updated automatically by \code{modify_adjusted_schema()}.
    available_indicator_codes = NULL,

    #' @field selected_indicator_codes Character vector of indicator codes
    #'   currently selected by the user.  Does not need to align with
    #'   \code{available_indicator_codes}; mismatches are surfaced by
    #'   \code{diagnose_coherence()}.
    selected_indicator_codes = NULL,

    #' @field issues_coherence List of coherence issues found between
    #'   \code{available_indicator_codes} and \code{selected_indicator_codes}.
    #'   Populated by \code{diagnose_coherence()}.
    issues_coherence = list(),

    #' @description
    #' Creates a new Framework object.
    #' @return A new Framework object.
    initialize = function() {
      phr_try({
        self$master_schema            <- NULL
        self$adjusted_schema          <- NULL
        self$master_svg               <- NULL
        self$adjusted_svg             <- NULL
        self$primary_objectives       <- NULL
        self$secondary_objectives     <- NULL
        self$available_indicator_codes <- NULL
        self$selected_indicator_codes  <- NULL
        self$issues_coherence          <- list()
        phr_message(phr_txt("Framework initialized."), origin = "Framework$initialize")
      }, on_error = "abort", origin = "Framework$initialize")
      invisible(self)
    },

    #' @description Set the master reference schema.
    #'
    #' Validates and stores a data frame as the master schema.  The data frame
    #' must contain at minimum the columns required by
    #' \code{\link{validate_objective_schema}}: \code{sector},
    #' \code{pillar}, \code{sub_pillar}, \code{short_objective}, and
    #' \code{text_objective}.
    #'
    #' @param schema Data frame. The master reference schema.
    #' @return Invisibly returns \code{self} for method chaining.
    set_master_schema = function(schema) {
      phr_try({
        validate_objective_schema(schema, soft = FALSE)
        self$master_schema <- as.data.frame(schema, stringsAsFactors = FALSE)
        phr_message(
          phr_txt("Master schema set ({nrow(self$master_schema)} rows)."),
          origin = "Framework$set_master_schema"
        )
      }, on_error = "abort", origin = "Framework$set_master_schema")
      invisible(self)
    },

    #' @description Set the master SVG diagram.
    #'
    #' Stores a character string as the master SVG diagram for the conceptual
    #' framework.  The string should be valid SVG markup.
    #'
    #' @param svg_content Character. SVG content as a single string.
    #' @return Invisibly returns \code{self} for method chaining.
    set_master_svg = function(svg_content) {
      phr_try({
        phr_assert(
          is.character(svg_content) && length(svg_content) >= 1,
          message = phr_txt("svg_content must be a non-empty character string."),
          origin  = "Framework$set_master_svg"
        )
        self$master_svg <- svg_content
        phr_message(phr_txt("Master SVG set."), origin = "Framework$set_master_svg")
      }, on_error = "abort", origin = "Framework$set_master_svg")
      invisible(self)
    },

    #' @description Colour sub-pillar blocks in the adjusted SVG according to
    #'   the \code{primary_objectives} and \code{secondary_objectives} fields.
    #'
    #' Derives \code{adjusted_svg} from \code{master_svg} by applying fill
    #' colours to each sub-pillar block whose \code{objective_code} is present
    #' in \code{primary_objectives}, \code{secondary_objectives}, or both.
    #' The mapping from objective codes to SVG group ids uses the
    #' \code{objective_code} and \code{sub_pillar} columns of
    #' \code{master_schema}.
    #'
    #' Colour rules (applied per sub-pillar, using all objective codes that
    #' belong to that sub-pillar):
    #' \itemize{
    #'   \item \strong{white} – no codes present in either vector (default).
    #'   \item \strong{\code{#90EE90}} (light green) – one or more codes are
    #'     present in \code{primary_objectives} only.
    #'   \item \strong{\code{#ADD8E6}} (light blue) – one or more codes are
    #'     present in \code{secondary_objectives} only.
    #'   \item \strong{\code{#DDA0DD}} (light purple) – one or more codes are
    #'     present in both \code{primary_objectives} and
    #'     \code{secondary_objectives}.
    #' }
    #'
    #' When \code{master_svg} or \code{master_schema} is \code{NULL} the
    #' method issues a warning and returns without modifying \code{adjusted_svg}.
    #'
    #' @return Invisibly returns \code{self} for method chaining.
    modify_adjusted_svg = function() {
      phr_try({
        if (is.null(self$master_svg)) {
          phr_warning(
            message = phr_txt("master_svg is not set; skipping modify_adjusted_svg()."),
            origin  = "Framework$modify_adjusted_svg"
          )
          return(invisible(self))
        }

        if (is.null(self$master_schema) || !is.data.frame(self$master_schema) ||
            nrow(self$master_schema) == 0) {
          phr_warning(
            message = phr_txt("master_schema is not set; skipping modify_adjusted_svg()."),
            origin  = "Framework$modify_adjusted_svg"
          )
          return(invisible(self))
        }

        phr_assert(
          "objective_code" %in% names(self$master_schema),
          message = phr_txt(
            "master_schema must contain an 'objective_code' column for modify_adjusted_svg()."
          ),
          origin = "Framework$modify_adjusted_svg"
        )

        primary   <- as.numeric(self$primary_objectives)
        secondary <- as.numeric(self$secondary_objectives)

        svg <- self$master_svg

        # Primary lookup: build objective_code -> [svg_group_id, ...] by parsing
        # the numeric codes embedded in the SVG diagram text labels.  This is the
        # authoritative source and does not rely on sub_pillar column values.
        svg_code_map <- .build_code_svg_map(svg)

        # Fallback lookup: derive objective_code -> sub_pillar from master_schema.
        # Used only for codes whose SVG groups do not carry embedded code labels
        # (e.g. minimal test-fixture SVGs where group ids equal sub_pillar values).
        schema_code_map <- list()
        if ("sub_pillar" %in% names(self$master_schema)) {
          for (i in seq_len(nrow(self$master_schema))) {
            code <- self$master_schema$objective_code[[i]]
            sp   <- self$master_schema$sub_pillar[[i]]
            if (!is.na(code) && !is.na(sp) && nzchar(as.character(sp))) {
              code_str <- as.character(as.integer(code))
              if (is.null(schema_code_map[[code_str]])) {
                schema_code_map[[code_str]] <- as.character(sp)
              }
            }
          }
        }

        # Merge: SVG-parsed entries take priority over schema-derived fallbacks.
        code_map <- schema_code_map
        for (code_str in names(svg_code_map)) {
          code_map[[code_str]] <- svg_code_map[[code_str]]
        }

        # Retain only entries whose key is a valid integer code string.
        valid_keys <- names(code_map)[grepl("^\\d+$", names(code_map))]

        # Colour each SVG group whose objective_code appears in the selected sets.
        for (code_str in valid_keys) {
          code    <- as.numeric(code_str)
          svg_ids <- code_map[[code_str]]

          in_primary   <- length(primary)   > 0L && code %in% primary
          in_secondary <- length(secondary) > 0L && code %in% secondary

          if (!in_primary && !in_secondary) next

          colour <- if (in_primary && in_secondary) {
            "#DDA0DD"
          } else if (in_primary) {
            "#90EE90"
          } else {
            "#ADD8E6"
          }

          for (svg_id in svg_ids) {
            pattern     <- paste0(
              '(<g id="', svg_id, '">[^<]*<rect(?:[^>]*?) )fill="[^"]*"([^>]*>)'
            )
            replacement <- paste0('\\1fill="', colour, '"\\2')
            svg         <- gsub(pattern, replacement, svg, perl = TRUE)
          }
        }

        self$adjusted_svg <- svg
        phr_message(
          phr_txt("Adjusted SVG updated via modify_adjusted_svg()."),
          origin = "Framework$modify_adjusted_svg"
        )
      }, on_error = "abort", origin = "Framework$modify_adjusted_svg")
      invisible(self)
    },

    #' @description Render the framework SVG and display it in the active
    #'   graphics device (e.g. the RStudio Plots pane).
    #'
    #' The \code{version} argument controls which SVG is rendered.  Pass
    #' \code{"adjusted"} (the default) to render \code{adjusted_svg}, falling
    #' back to \code{master_svg} when \code{adjusted_svg} is \code{NULL}.
    #' Pass \code{"master"} to render \code{master_svg} directly, regardless of
    #' whether an adjusted version exists.
    #'
    #' Requires the \pkg{rsvg} and \pkg{grid} packages.
    #' When neither is installed the SVG is written to a temporary file and
    #' the file path is returned visibly so the caller can open it manually.
    #'
    #' @param version Character. Which SVG to render: \code{"adjusted"}
    #'   (default) or \code{"master"}.
    #' @return Invisibly returns \code{self} for method chaining.  When
    #'   \pkg{rsvg}/\pkg{grid} are unavailable the path to the written SVG
    #'   file is returned visibly instead.
    render_framework_svg = function(version = "adjusted") {
      phr_try({
        phr_assert(
          is.character(version) && length(version) == 1 &&
            version %in% c("adjusted", "master"),
          message = phr_txt("version must be 'adjusted' or 'master'."),
          origin  = "Framework$render_framework_svg"
        )
        svg_content <- if (version == "master") {
          self$master_svg
        } else {
          self$adjusted_svg %||% self$master_svg
        }
        phr_assert(
          !is.null(svg_content) && nzchar(svg_content),
          message = phr_txt(
            "No SVG content available. Call modify_adjusted_schema() or set_master_svg() first."
          ),
          origin = "Framework$render_framework_svg"
        )

        tmp_svg <- tempfile(fileext = ".svg")
        writeLines(svg_content, con = tmp_svg)

        if (requireNamespace("rsvg", quietly = TRUE) &&
            requireNamespace("grid", quietly = TRUE)) {
          native_raster <- rsvg::rsvg_nativeraster(tmp_svg)
          grid::grid.newpage()
          grid::grid.raster(native_raster)
          phr_message(
            phr_txt("Framework SVG rendered to the active graphics device."),
            origin = "Framework$render_framework_svg"
          )
        } else {
          phr_warning(
            message = phr_txt(
              "Packages 'rsvg' and 'grid' are required to display the SVG in the plots window. SVG written to: {tmp_svg}"
            ),
            origin = "Framework$render_framework_svg"
          )
          return(tmp_svg)
        }
      }, on_error = "abort", origin = "Framework$render_framework_svg")
      invisible(self)
    },

    #' @description Filter the master schema to the specified objective codes and
    #'   store the result in \code{adjusted_schema}.
    #'
    #' Filters \code{master_schema} to retain only rows whose
    #' \code{short_objective} value is present in \code{objective_codes} and
    #' stores the result as \code{adjusted_schema}.  When \code{objective_codes}
    #' is \code{NULL} or an empty vector all objective codes found in
    #' \code{master_schema} are used, making \code{adjusted_schema} identical to
    #' \code{master_schema}.  Also updates \code{available_indicator_codes} from
    #' the resulting \code{adjusted_schema}.
    #'
    #' @param objective_codes Character vector or list of \code{short_objective}
    #'   values to retain.  Pass \code{NULL} (the default) to include all
    #'   objectives from the master schema.
    #' @return Invisibly returns \code{self} for method chaining.
    modify_adjusted_schema = function(objective_codes = NULL) {
      phr_try({
        phr_assert(
          !is.null(self$master_schema) && is.data.frame(self$master_schema) &&
            nrow(self$master_schema) > 0,
          message = phr_txt("master_schema must be set before calling modify_adjusted_schema()."),
          origin  = "Framework$modify_adjusted_schema"
        )

        if (is.null(objective_codes) || length(objective_codes) == 0) {
          objective_codes <- unique(self$master_schema$short_objective)
        } else {
          objective_codes <- as.character(unlist(objective_codes))
        }

        self$adjusted_schema <- self$master_schema[
          self$master_schema$short_objective %in% objective_codes, ,
          drop = FALSE
        ]

        # Update available_indicator_codes from the adjusted_schema
        private$.refresh_available_indicator_codes()

        phr_message(
          phr_txt(
            "Adjusted schema modified: {nrow(self$adjusted_schema)} of {nrow(self$master_schema)} rows selected."
          ),
          origin = "Framework$modify_adjusted_schema"
        )
      }, on_error = "abort", origin = "Framework$modify_adjusted_schema")
      invisible(self)
    },

    #' @description Diagnose coherence between \code{available_indicator_codes}
    #'   and \code{selected_indicator_codes}.
    #'
    #' Compares the indicator codes present in \code{available_indicator_codes}
    #' (derived from \code{adjusted_schema}) with those in
    #' \code{selected_indicator_codes} (chosen by the user).  Any mismatches are
    #' collected and stored in \code{issues_coherence}.  An empty
    #' \code{issues_coherence} list indicates full coherence.
    #'
    #' @return Invisibly returns \code{self} for method chaining.
    diagnose_coherence = function() {
      phr_try({
        self$issues_coherence <- list()

        avail_codes <- if (!is.null(self$available_indicator_codes) &&
                           is.data.frame(self$available_indicator_codes) &&
                           "indicator_code" %in% names(self$available_indicator_codes)) {
          unique(as.character(self$available_indicator_codes[["indicator_code"]]))
        } else {
          character(0)
        }

        sel_codes <- if (!is.null(self$selected_indicator_codes)) {
          unique(as.character(self$selected_indicator_codes))
        } else {
          character(0)
        }

        in_selected_not_available <- setdiff(sel_codes, avail_codes)
        in_available_not_selected <- setdiff(avail_codes, sel_codes)

        if (length(in_selected_not_available) > 0) {
          self$issues_coherence$selected_not_in_available <- paste0(
            "The following selected_indicator_codes are not in available_indicator_codes: ",
            paste(in_selected_not_available, collapse = ", ")
          )
        }

        if (length(in_available_not_selected) > 0) {
          self$issues_coherence$available_not_in_selected <- paste0(
            "The following available_indicator_codes have not been selected: ",
            paste(in_available_not_selected, collapse = ", ")
          )
        }

        if (length(self$issues_coherence) == 0) {
          phr_message(
            phr_txt("Coherence check passed: available and selected indicator codes align."),
            origin = "Framework$diagnose_coherence"
          )
        } else {
          phr_message(
            phr_txt(
              "Coherence check found {length(self$issues_coherence)} issue(s). Check self$issues_coherence."
            ),
            origin = "Framework$diagnose_coherence"
          )
        }
      }, on_error = "abort", origin = "Framework$diagnose_coherence")
      invisible(self)
    }
  ),

  private = list(
    # Rebuild available_indicator_codes from the current adjusted_schema.
    .refresh_available_indicator_codes = function() {
      if (!is.null(self$adjusted_schema) &&
          is.data.frame(self$adjusted_schema) &&
          "indicator_code" %in% names(self$adjusted_schema)) {
        tool_type_col <- if ("tool_type" %in% names(self$adjusted_schema)) {
          as.character(self$adjusted_schema[["tool_type"]])
        } else {
          rep(NA_character_, nrow(self$adjusted_schema))
        }
        ind_codes <- as.character(self$adjusted_schema[["indicator_code"]])
        keep <- !is.na(ind_codes) & nzchar(ind_codes)
        self$available_indicator_codes <- data.frame(
          tool_type      = tool_type_col[keep],
          indicator_code = ind_codes[keep],
          stringsAsFactors = FALSE
        )
      } else {
        self$available_indicator_codes <- NULL
      }
    }
  )
)
