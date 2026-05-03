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

