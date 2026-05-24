#' ANAFramework R6 Class
#'
#' @description
#' Subclass of \code{\link{Framework}} for the ANA (Area-based Needs
#' Assessment) conceptual framework.  On initialisation the class automatically
#' loads:
#' \itemize{
#'   \item the bundled \code{reference_objectives.xlsx} and
#'     \code{reference_indicator_bank.xlsx} (merged on \code{objective_code})
#'     as \code{master_schema};
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
    #' The bundled \code{reference_objectives.xlsx} and
    #' \code{reference_indicator_bank.xlsx} are loaded and merged on
    #' \code{objective_code} to form \code{master_schema}.
    #' \code{ana_framework.svg} is loaded as \code{master_svg}.  Warnings are
    #' issued if any file cannot be found or read.
    #'
    #' @return A new ANAFramework object.
    initialize = function() {
      super$initialize()

      # ---- Load master schema from reference_objectives.xlsx and
      #      reference_indicator_bank.xlsx, then merge on objective_code ----
      phr_try({
        objectives <- load_objective_schema()
        indicators <- load_indicator_bank()

        if (!is.null(objectives) && is.data.frame(objectives) && nrow(objectives) > 0) {
          if (!is.null(indicators) && is.data.frame(indicators) && nrow(indicators) > 0) {
            # Left-join objectives with indicators so every objective is retained
            # even if it has no indicators yet, and each indicator row carries
            # the full objective-level metadata.  Suffixes are applied to any
            # column names that appear in both data frames (other than the join
            # key) to prevent silent data loss or ambiguous column names.
            schema <- merge(
              objectives, indicators,
              by        = "objective_code",
              all.x     = TRUE,
              sort      = FALSE,
              suffixes  = c("_obj", "_ind")
            )
          } else {
            schema <- objectives
          }
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
              "ANAFramework: reference_objectives.xlsx could not be loaded; master_schema is NULL."
            ),
            origin = "ANAFramework$initialize",
            hint   = phr_txt(
              "Ensure reference_objectives.xlsx is present in the package resources folder."
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

