#' SurveyProtocol R6 Class
#'
#' @description
#' Subclass of \code{\link{Protocol}} for managing survey-based protocol
#' workflows.  Extends the base \code{Protocol} class with strata definition,
#' sample size calculations, sampling frame management, and sample drawing.
#'
#' In addition to all capabilities inherited from \code{Protocol}, this class
#' provides:
#' 1. Strata Definition and Sample Size Calculations (\code{add_stratum()})
#' 2. Sampling Frame Validation (\code{set_sampling_frame()})
#' 3. Sample Drawing (\code{draw_sample()})
#'
#' @importFrom R6 R6Class
#' @export
SurveyProtocol <- R6::R6Class(
  "SurveyProtocol",
  inherit = Protocol,
  public = list(
    #' @field sample_object A \code{\link{Sample}} object that stores the
    #'   strata/sample table and sample-size workflows.
    sample_object = NULL,

    #' @field sample_table Data frame mirror of \code{sample_object$sample_table}.
    sample_table = NULL,

    #' @field strata_names Character vector of strata names synced from
    #'   \code{sample_object}.
    strata_names = character(0),

    #' @field sampling_methods Character vector of unique sampling methods synced
    #'   from \code{sample_object}.
    sampling_methods = character(0),

    #' @field sampling_frame_strata_population Data frame of strata-level
    #'   population totals aggregated from \code{sampling_frame$log_df}.
    sampling_frame_strata_population = NULL,

    #' @field drawn_sample_from_frame Data-table-style data frame of sampled rows
    #'   synced from the nested \code{Sample} object.
    drawn_sample_from_frame = NULL,

    #' @field sampling_frame A \code{\link{SamplingFrame}} object holding and
    #'   validating the sampling frame data.  Initialised to an empty
    #'   \code{SamplingFrame} on construction; populated via
    #'   \code{set_sampling_frame()} or by passing a data frame on
    #'   initialisation.
    sampling_frame = NULL,

    #' @field sampling_frame_strata_names Character vector of unique strata names
    #'   currently available in the held \code{SamplingFrame}, when present.
    sampling_frame_strata_names = character(0),

    #' @field drawn_sample Data frame with selected PSUs (filtered from drawn_sample_full)
    drawn_sample = NULL,

    #' @field drawn_sample_full Full sampling frame with sampled_psu and allocated_sample columns
    drawn_sample_full = NULL,

    #' @description
    #' Creates a new SurveyProtocol object
    #' @param assessment_title Character. Title of the assessment
    #' @param country_name Character. Country where assessment takes place
    #' @param month_year Character. Month and year of data collection (e.g., "January 2024")
    #' @param framework_type Character. Type of framework to initialise.  One of
    #'   \code{"none"} or \code{"ana"}.  Defaults to \code{"none"}.
    #' @param sampling_frame Optional data frame to initialise the
    #'   \code{\link{SamplingFrame}} with.  When \code{NULL} (default), an empty
    #'   \code{SamplingFrame} is created.
    #' @param reference_doc_filename Optional document template filename/path.
    #' @param reference_ppt_filename Optional PowerPoint template filename/path.
    #' @return A new SurveyProtocol object
    initialize = function(assessment_title = NULL, country_name = NULL, month_year = NULL,
                          framework_type = "none", sampling_frame = NULL,
                          reference_doc_filename = NULL,
                          reference_ppt_filename = NULL) {
      super$initialize(
        assessment_title = assessment_title,
        country_name     = country_name,
        month_year       = month_year,
        framework_type   = framework_type,
        reference_doc_filename = reference_doc_filename,
        reference_ppt_filename = reference_ppt_filename
      )
      self$sample_object <- Sample$new()
      self$sampling_frame <- SamplingFrame$new(log_df = sampling_frame)
      private$.sync_state()
      invisible(self)
    },

    #' @description Set the sampling frame
    #'
    #' Validates the frame using existing \pkg{phr} validators and
    #' \code{validate_sampling_frame()}, then stores it.  If the frame does
    #' not have an \code{inclusion} column, one is added with all \code{TRUE}
    #' values.
    #'
    #' @param frame Data frame. Must contain at minimum a \code{psu} column
    #'   (primary sampling unit identifier).  A \code{population_size} column
    #'   is required for proportional, pps_cluster, and rlc sampling methods.
    #'   A \code{stratum} column enables stratified sampling.
    #' @return Invisibly returns \code{self} for method chaining.
    set_sampling_frame = function(frame) {
      phr_try({

        # 1. Confirm it is a data frame and not empty
        phr_validate_dataframe(frame, origin = "SurveyProtocol$set_sampling_frame", soft = FALSE)
        phr_assert(
          nrow(frame) > 0,
          message = phr_txt("Sampling frame is empty."),
          origin  = "SurveyProtocol$set_sampling_frame",
          hint    = phr_txt("Provide a data frame with at least one PSU row.")
        )

        # 2. Run validate_sampling_frame — stops on hard issues
        val_result <- validate_sampling_frame(frame)
        if (!val_result$valid) {
          hard_issues <- val_result$issues[setdiff(names(val_result$issues), "missing_inclusion")]
          if (length(hard_issues) > 0) {
            phr_error(
              message = phr_txt("Sampling frame validation failed: {paste(names(hard_issues), unlist(hard_issues), sep=': ', collapse='; ')}"),
              origin  = "SurveyProtocol$set_sampling_frame",
              hint    = phr_txt("Ensure the frame has 'stratum', 'psu', and 'population_size' columns, and that any 'inclusion' column contains only TRUE/FALSE values.")
            )
          }
        }

        # 3. Add inclusion column (all TRUE) if absent
        if (!"inclusion" %in% names(frame)) {
          frame$inclusion <- TRUE
          phr_message(
            phr_txt("'inclusion' column not found — defaulting all PSUs to TRUE."),
            origin = "SurveyProtocol$set_sampling_frame"
          )
        }

        self$set_nested(
          field = "sampling_frame",
          member = "log_df",
          value = tibble::as_tibble(frame)
        )
        private$.sync_state()
        private$.touch()
        self$diagnose_coherence()
        phr_message(
          phr_txt("Sampling frame set with {nrow(frame)} PSUs."),
          origin = "SurveyProtocol$set_sampling_frame"
        )

      }, on_error = "abort", origin = "SurveyProtocol$set_sampling_frame")
      invisible(self)
    },


    #' @description Validate the structure of the master sample table
    #'
    #' Checks that \code{sample_table} exists and contains all required master
    #' table columns.
    #'
    #' @return \code{TRUE} if valid, \code{FALSE} otherwise.
    validate_strata_table = function() {
    if (is.null(self$sample_object) || !inherits(self$sample_object, "Sample")) return(FALSE)
    isTRUE(self$sample_object$validate_strata_table())
    },

    #' @description Get the sample table
    #' @return Data frame containing the sample table
    get_sample_table = function() {
      if (is.null(self$sample_object) || !inherits(self$sample_object, "Sample")) return(NULL)
      self$sample_object$get_sample_table()
    },

    # ── Sampling helpers ────────────────────────────────────────────────────

    #' @description Return the unique sampling methods used across all strata.
    #'
    #' Reads the \code{sampling_method} column of \code{self$get_sample_table()}.
    #'
    #' @return Character vector of unique, non-NA sampling method values.
    #'   Empty character vector when no sample table is set.
    get_sampling_methods = function() {
      if (is.null(self$sample_object) || !inherits(self$sample_object, "Sample")) return(character(0))
      self$sample_object$get_sampling_methods()
    },

    #' @description Return the stratum names from the sample table.
    #'
    #' Uses \code{Population_Name} when available, falling back to
    #' \code{stratum_id}.
    #'
    #' @return Character vector of stratum names.  Empty character vector
    #'   when no sample table is set.
    get_strata_names = function() {
      if (is.null(self$sample_object) || !inherits(self$sample_object, "Sample")) return(character(0))
      self$sample_object$get_strata_names()
    },

    #' @description Extract a column vector from the sampling frame, optionally
    #'   filtered to a specific stratum and/or to included PSUs only.
    #'
    #' @param col_name Character. Column to extract.
    #' @param strata Optional character vector of stratum names to filter to.
    #'   \code{NULL} (default) returns all strata.
    #' @param included_only Logical. When \code{TRUE} (default) only return
    #'   rows where the \code{inclusion} column is \code{TRUE}.
    #' @return Vector of values from the requested column.  \code{NULL} when
    #'   the sampling frame is not set or the column does not exist.
    get_frame_column = function(col_name, strata = NULL, included_only = TRUE) {
      sf <- if (!is.null(self$sampling_frame)) self$sampling_frame$log_df else NULL
      if (is.null(sf) || !is.data.frame(sf) || !col_name %in% names(sf)) {
        return(NULL)
      }
      df <- as.data.frame(sf, stringsAsFactors = FALSE)
      if (isTRUE(included_only) && "inclusion" %in% names(df)) {
        df <- df[!is.na(df$inclusion) & df$inclusion, , drop = FALSE]
      }
      if (!is.null(strata) && "stratum" %in% names(df)) {
        df <- df[df$stratum %in% as.character(strata), , drop = FALSE]
      }
      df[[col_name]]
    },

    #' @description Override \code{diagnose_coherence} to additionally check
    #'   strata consistency between the sampling frame and the sample table.
    #'
    #' Calls the base \code{Protocol$diagnose_coherence()} and then appends
    #' strata consistency checks to \code{self$issues_coherence}.
    #'
    #' @return Invisibly returns \code{self} for method chaining.
    diagnose_coherence = function() {
      super$diagnose_coherence()

      # Check strata consistency between frame and sample table
      st <- self$get_sample_table()
      if (!is.null(st) && !is.null(self$sampling_frame) &&
          nrow(self$sampling_frame$log_df) > 0) {
        table_strata <- st$stratum_id
        frame_strata <- unique(self$sampling_frame$log_df$stratum)

        if (!setequal(table_strata, frame_strata)) {
          missing_in_frame <- setdiff(table_strata, frame_strata)
          missing_in_table <- setdiff(frame_strata, table_strata)

          if (length(missing_in_frame) > 0) {
            self$issues_coherence$strata_missing_in_frame <- paste(
              "Strata in sample table but not in frame:",
              paste(missing_in_frame, collapse = ", ")
            )
          }
          if (length(missing_in_table) > 0) {
            self$issues_coherence$strata_missing_in_table <- paste(
              "Strata in frame but not in sample table:",
              paste(missing_in_table, collapse = ", ")
            )
          }
        }
      }

      invisible(self)
    },

    #' @description Post-sync hook for sampling-related state.
    #' @param field Optional top-level field name.
    #' @param member Optional nested member name.
    #' @param target_field Optional destination field path.
    #' @param name Optional named list entry inside \code{field}.
    #' @param role Optional role-based list resolution key.
    #' @return Invisibly returns \code{NULL}.
    post_sync_state = function(field = NULL, member = NULL, target_field = NULL,
                               name = NULL, role = NULL) {
      super$post_sync_state(
        field = field, member = member, target_field = target_field,
        name = name, role = role
      )
      if (isTRUE(private$.post_sync_guard)) return(invisible(NULL))
      private$.post_sync_guard <- TRUE
      on.exit({ private$.post_sync_guard <- FALSE }, add = TRUE)
      private$.sync_sampling_state()
      private$.sync_sample_frame_state()
      invisible(NULL)
    }

  ),

  private = list(
    .post_sync_guard = FALSE,

    .sync_sampling_state = function() {
      st <- tryCatch(
        self$access_nested(field = "sample_object", member = "get_sample_table"),
        error = function(e) NULL
      )
      strata_names <- tryCatch(
        self$access_nested(field = "sample_object", member = "get_strata_names"),
        error = function(e) character(0)
      )
      methods_used <- tryCatch(
        self$access_nested(field = "sample_object", member = "get_sampling_methods"),
        error = function(e) character(0)
      )
      drawn_sample <- tryCatch(
        self$access_nested(field = "sample_object", member = "drawn_sample"),
        error = function(e) NULL
      )
      drawn_sample_full <- tryCatch(
        self$access_nested(field = "sample_object", member = "drawn_sample_full"),
        error = function(e) NULL
      )

      self$sample_table <- if (is.data.frame(st)) st else NULL
      self$strata_names <- unique(as.character(strata_names %||% character(0)))

      if (is.null(methods_used)) methods_used <- character(0)
      methods_used <- unique(trimws(tolower(as.character(methods_used))))
      methods_used <- methods_used[!is.na(methods_used) & nzchar(methods_used)]
      self$sampling_methods <- methods_used

      known_methods <- c("simple_random", "proportional", "pps_cluster", "pps_rlc",
                         "systematic", "simple_random_rlc", "systematic_rlc",
                         "proportional_rlc", "purposive")
      self$metadata$sampling_strata_names <- as.character(self$strata_names %||% character(0))
      self$metadata$sampling_method_flags <- setNames(as.list(known_methods %in% methods_used), known_methods)

      if (!is.null(st) && nrow(st) > 0) {
        strata_ids <- as.character(st$stratum_id)
        strata_names <- as.character(st$stratum_name)
        self$metadata$target_strata <- setNames(as.list(strata_names), strata_ids)
      } else {
        self$metadata$target_strata <- list()
      }
      self$drawn_sample <- drawn_sample
      self$drawn_sample_from_frame <- if (is.data.frame(drawn_sample)) {
        data.table::as.data.table(drawn_sample)
      } else {
        NULL
      }
      self$drawn_sample_full <- drawn_sample_full
      invisible(NULL)
    },

    .sync_sample_frame_state = function() {
      sf <- tryCatch(
        self$access_nested(field = "sampling_frame", member = "log_df"),
        error = function(e) NULL
      )
      if (is.null(sf) || !is.data.frame(sf) || nrow(sf) == 0L || !"stratum" %in% names(sf)) {
        self$sampling_frame_strata_names <- character(0)
        self$sampling_frame_strata_population <- NULL
        return(invisible(NULL))
      }

      vals <- as.character(sf$stratum)
      self$sampling_frame_strata_names <- unique(vals[!is.na(vals) & nzchar(vals)])

      sf2 <- sf[!is.na(sf$stratum) & nzchar(as.character(sf$stratum)), , drop = FALSE]
      if (nrow(sf2) == 0L) {
        self$sampling_frame_strata_population <- NULL
        return(invisible(NULL))
      }

      if ("population_size" %in% names(sf2)) {
        agg <- stats::aggregate(
          sf2$population_size,
          by = list(stratum = sf2$stratum),
          FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
        )
        names(agg)[2] <- "total_population"
      } else {
        agg <- stats::aggregate(
          rep(1, nrow(sf2)),
          by = list(stratum = sf2$stratum),
          FUN = sum
        )
        names(agg)[2] <- "total_population"
      }
      self$sampling_frame_strata_population <- as.data.frame(agg, stringsAsFactors = FALSE)
      invisible(NULL)
    }
  )
)
