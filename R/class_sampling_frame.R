#' SamplingFrame: Sampling Frame Log Class
#'
#' @description
#' A \code{\link{Log}} subclass for holding and validating the sampling frame
#' data used in survey sample drawing workflows.
#'
#' @details
#' The default structure is the full extended sampling frame with the following
#' columns:
#' \itemize{
#'   \item \code{stratum} — stratum identifier.
#'   \item \code{psu} — primary sampling unit identifier.
#'   \item \code{population_size} — population count for the PSU.
#'   \item \code{inclusion} — logical flag marking PSUs eligible for sampling.
#'   \item \code{sampled_psu} — cluster number(s) assigned by \code{draw_sample()};
#'     \code{NA} for unselected PSUs.  When a PSU is drawn more than once (as
#'     can happen with PPS cluster or RLC sampling), this field contains the
#'     comma-separated consecutive cluster numbers assigned to that PSU
#'     (e.g. \code{"9, 10, 11"} for a PSU drawn three times).
#'   \item \code{allocated_sample} — number of households allocated to the PSU
#'     by \code{draw_sample()}; \code{NA} for unselected PSUs.
#' }
#'
#' A \code{SamplingFrame} can be initialised with an existing data frame
#' (e.g. a pre-built frame loaded from a file) or left empty for later
#' population via \code{\link[=SurveyProtocol]{SurveyProtocol$set_sampling_frame()}}.
#'
#' @importFrom R6 R6Class
#' @export
SamplingFrame <- R6::R6Class(
  classname = "SamplingFrame",
  inherit = Log,

  public = list(

    #' @description
    #' Creates a new SamplingFrame object.
    #'
    #' @param log_df Optional data frame of sampling frame entries.  When
    #'   \code{NULL} (default), an empty frame with the standard columns is
    #'   created automatically.
    #' @param log_name Character. Display name for this log (default:
    #'   \code{"Sampling Frame"}).
    #' @param required_columns Character vector of required column names.
    #'   Defaults to the standard set: \code{stratum}, \code{psu},
    #'   \code{population_size}, \code{inclusion}, \code{sampled_psu},
    #'   \code{allocated_sample}.
    #' @param schema List with \code{types} and/or \code{allowed_values} for
    #'   validation.  Defaults to the standard sampling frame schema.
    #' @return A new SamplingFrame R6 object.
    initialize = function(log_df = NULL,
                          log_name = "Sampling Frame",
                          required_columns = NULL,
                          schema = NULL) {

      required_columns <- required_columns %||% c(
        "stratum",
        "psu",
        "population_size",
        "inclusion",
        "sampled_psu",
        "allocated_sample"
      )

      schema <- schema %||% list(
        types = list(
          stratum          = "character",
          psu              = "character",
          population_size  = "numeric",
          inclusion        = "logical",
          sampled_psu      = "character",
          allocated_sample = "numeric"
        )
      )

      super$initialize(
        log_df           = log_df,
        log_name         = log_name,
        required_columns = required_columns,
        schema           = schema
      )
    }
  )
)
