#' ANAFramework R6 Class
#'
#' @description
#' Subclass of \code{\link{Framework}} for the ANA (Area-based Needs
#' Assessment) conceptual framework.  On initialisation the class automatically
#' loads:
#' \itemize{
#'   \item the bundled \code{reference.xlsx} as \code{master_schema};
#'   \item the bundled \code{ana_framework.svg} as \code{master_svg}.
#' }
#'
#' Call \code{modify_adjusted_schema()} with
#' a vector of objective codes to filter the schema, and then
#' \code{modify_adjusted_svg()} to produce a highlighted SVG.
#'
#' @importFrom R6 R6Class
#' @export
ANAFramework <- R6::R6Class(
  "ANAFramework",
  inherit = Framework,
  public = list(
    #' @description
    #' Creates a new ANAFramework object.
    #'
    #' The bundled \code{reference.xlsx} is loaded as \code{master_schema} and
    #' \code{ana_framework.svg} is loaded as \code{master_svg}.  Warnings are
    #' issued if either file cannot be found or read.
    #'
    #' @return A new ANAFramework object.
    initialize = function() {
      super$initialize()

      # ---- Load master schema from reference.xlsx ----
      phr_try({
        schema <- load_objective_schema()
        if (!is.null(schema) && is.data.frame(schema) && nrow(schema) > 0) {
          self$master_schema <- schema
          phr_message(
            phr_txt(
              "ANAFramework: master schema loaded ({nrow(schema)} rows)."
            ),
            origin = "ANAFramework$initialize"
          )
        } else {
          phr_warning(
            message = phr_txt(
              "ANAFramework: reference.xlsx could not be loaded; master_schema is NULL."
            ),
            origin = "ANAFramework$initialize",
            hint   = phr_txt(
              "Ensure reference.xlsx is present in the package resources folder."
            )
          )
        }
      }, on_error = "warn", origin = "ANAFramework$initialize")

      # ---- Load master SVG from ana_framework.svg ----
      phr_try({
        svg_file <- system.file("resources", "ana_framework.svg", package = "phr")
        if (!nzchar(svg_file) || !file.exists(svg_file)) {
          svg_file <- file.path("resources", "ana_framework.svg")
        }
        if (file.exists(svg_file)) {
          svg_content <- paste(readLines(svg_file, warn = FALSE), collapse = "\n")
          self$master_svg  <- svg_content
          self$adjusted_svg <- svg_content
          phr_message(
            phr_txt("ANAFramework: master SVG loaded from ana_framework.svg."),
            origin = "ANAFramework$initialize"
          )
        } else {
          phr_warning(
            message = phr_txt(
              "ANAFramework: ana_framework.svg could not be found; master_svg is NULL."
            ),
            origin = "ANAFramework$initialize",
            hint   = phr_txt(
              "Ensure ana_framework.svg is present in the package resources folder."
            )
          )
        }
      }, on_error = "warn", origin = "ANAFramework$initialize")

      invisible(self)
    },

    #' @description Filter the master schema to objectives whose \code{objective_code}
    #'   matches one or more of the supplied objective code values and store the
    #'   result in \code{adjusted_schema}.
    #'
    #' This corresponds to the objective-code-dimension filter used in the Shiny module.
    #' Also updates \code{available_indicator_codes} from the resulting
    #' \code{adjusted_schema}.
    #'
    #' @param objective_codes Numeric or character vector of objective codes to retain.
    #' @return Invisibly returns \code{self} for method chaining.
    modify_adjusted_schema = function(objective_codes) {
      phr_try({
        phr_assert(
          !is.null(self$master_schema) && is.data.frame(self$master_schema),
          message = phr_txt("master_schema must be set before calling modify_adjusted_schema()."),
          origin  = "ANAFramework$modify_adjusted_schema"
        )
        phr_assert(
          (is.numeric(objective_codes) || is.character(objective_codes)) &&
            length(objective_codes) > 0,
          message = phr_txt("objective_codes must be a non-empty numeric or character vector."),
          origin  = "ANAFramework$modify_adjusted_schema"
        )
        phr_assert(
          "objective_code" %in% names(self$master_schema),
          message = phr_txt("master_schema must contain an 'objective_code' column."),
          origin  = "ANAFramework$modify_adjusted_schema"
        )
        self$adjusted_schema <- self$master_schema[
          self$master_schema$objective_code %in% objective_codes, , drop = FALSE
        ]

        # Update available_indicator_codes from the adjusted_schema
        if ("indicator_code" %in% names(self$adjusted_schema)) {
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

        phr_message(
          phr_txt(
            "ANAFramework adjusted_schema updated: {nrow(self$adjusted_schema)} row(s) selected."
          ),
          origin = "ANAFramework$modify_adjusted_schema"
        )
      }, on_error = "abort", origin = "ANAFramework$modify_adjusted_schema")
      invisible(self)
    }
  ),

  private = list(
    # Replace fill="..." on the primary <rect stroke="black"> inside each
    # named <g id="BLOCK_ID"> group.
    #
    # @param svg Character. SVG markup to modify.
    # @param all_blocks Character vector of all known sub_pillar block IDs.
    # @param selected_blocks Character vector of the currently selected block IDs.
    # @param highlight_colour Character. Fill colour for selected blocks.
    # @param default_colour Character. Fill colour for unselected blocks.
    # @return Modified SVG character string.
    set_block_fills = function(svg, all_blocks, selected_blocks,
                               highlight_colour, default_colour) {
      for (bid in all_blocks) {
        colour <- if (bid %in% selected_blocks) highlight_colour else default_colour
        # Match:  <g id="BID">  (optional whitespace/newline)
        #         <rect  (any attrs)  fill="ANYTHING"  stroke="black"
        # and replace the fill value.
        pattern <- paste0(
          '(<g id="', bid, '">[^<]*<rect(?:[^>]*?) )fill="[^"]*"',
          '([^>]*?stroke="black")'
        )
        replacement <- paste0('\\1fill="', colour, '"\\2')
        svg <- gsub(pattern, replacement, svg, perl = TRUE)
      }
      svg
    }
  )
)

