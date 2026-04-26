#' ANAFramework R6 Class
#'
#' @description
#' Subclass of \code{\link{Framework}} for the ANA (Area-based Needs
#' Assessment) conceptual framework.  On initialisation the class
#' automatically loads the bundled \code{reference.xlsx} file as the master
#' reference schema, making all standard ANA objectives immediately available.
#'
#' The master SVG diagram can be set at any time via
#' \code{set_master_svg()}.  Once both the master schema and SVG are in place,
#' call \code{update_adjusted_schema()} and \code{update_adjusted_svg()} to
#' synchronise the adjusted versions with a current objective selection.
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
    #' The bundled \code{reference.xlsx} is loaded automatically and stored as
    #' \code{master_schema}.  If the file cannot be found or read, a warning is
    #' issued and \code{master_schema} remains \code{NULL}.
    #'
    #' @return A new ANAFramework object.
    initialize = function() {
      super$initialize()
      phr_try({
        schema <- load_objective_schema()
        if (!is.null(schema) && is.data.frame(schema) && nrow(schema) > 0) {
          self$master_schema <- schema
          phr_message(
            phr_txt(
              "ANAFramework: master schema loaded from reference.xlsx ({nrow(schema)} rows)."
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
      invisible(self)
    }
  )
)
