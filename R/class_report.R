#' Report R6 Class
#'
#' @description
#' Subclass of \code{\link{Document}} for holding a protocol object, multiple
#' data analytics objects, and an integrated analysis object.
#'
#' @importFrom R6 R6Class
Report <- R6::R6Class(
  "Report",
  inherit = Document,
  public = list(
    #' @field protocol A \code{\link{Protocol}} object.
    protocol = NULL,

    #' @field data_analytics Named list of \code{\link{DataAnalytics}} objects.
    data_analytics = list(),

    #' @field integrated_analysis An \code{\link{IntegratedAnalysis}} object.
    integrated_analysis = NULL,

    #' @description
    #' Creates a new Report object.
    #' @param protocol Optional \code{\link{Protocol}} object.
    #' @param data_analytics Optional named list of DataAnalytics objects.
    #' @param integrated_analysis Optional \code{\link{IntegratedAnalysis}}
    #'   object.
    #' @return A new Report object.
    initialize = function(protocol = NULL,
                          data_analytics = list(),
                          integrated_analysis = NULL) {
      origin <- "Report$initialize"
      super$initialize()
      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = origin,
        message = "Starting initialization of Report."
      )

      result <- phrutils::phr_try_step(
        {
          self$protocol <- protocol
          self$data_analytics <- data_analytics
          self$integrated_analysis <- integrated_analysis
        },
        step = "Assigning Report components",
        hint = "Check protocol, data_analytics and integrated_analysis."
      )
      if (phrutils::phr_failed(result)) {
        private$..touch(log = TRUE, touch = FALSE, name = origin, display = TRUE, message = result$error)
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = origin,
        message = paste0(
          "Report initialized with ", length(data_analytics),
          " data analytics object(s)."
        )
      )
      invisible(self)
    }
  )
)
