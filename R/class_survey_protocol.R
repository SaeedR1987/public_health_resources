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
    #' @field sample_table A \code{\link{Sample}} object that stores the
    #'   strata/sample table and sample-size workflows.
    sample_table = NULL,

    #' @field sampling_frame A \code{\link{SamplingFrame}} object holding and
    #'   validating the sampling frame data.  Initialised to an empty
    #'   \code{SamplingFrame} on construction; populated via
    #'   \code{set_sampling_frame()} or by passing a data frame on
    #'   initialisation.
    sampling_frame = NULL,

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
    #' @return A new SurveyProtocol object
    initialize = function(assessment_title = NULL, country_name = NULL, month_year = NULL,
                          framework_type = "none", sampling_frame = NULL) {
      super$initialize(
        assessment_title = assessment_title,
        country_name     = country_name,
        month_year       = month_year,
        framework_type   = framework_type
      )
      self$sample_table <- Sample$new()
      self$sampling_frame <- SamplingFrame$new(log_df = sampling_frame)
      self$sync_sample_metadata_fields
      self$sync_sampling_frame_fields
      invisible(self)
    },

    #' @description Add a stratum row to the master sample table
    #'
    #' Each call appends one row to \code{sample_table}.  Every column in the
    #' master table is included; columns that are not supplied default to
    #' \code{NA}.  Parameters that have clear equivalents in the master table
    #' schema are mapped automatically (e.g. \code{population_size} maps to
    #' \code{Total_Population}).
    #'
    #' @param stratum_id Character. Unique identifier for the stratum (used as
    #'   the row key and for cross-referencing the sampling frame).
    #' @param stratum_name Character. Human-readable name (stored as
    #'   \code{Population_Name}).
    #' @param population_size Numeric. Total population for this stratum
    #'   (stored as \code{Total_Population}).
    #' @param total_households Numeric. Total number of households (stored as
    #'   \code{Total_Households}).  Defaults to \code{NA}.
    #' @param sampling_method Character. Primary sampling method for the
    #'   stratum (stored as \code{sampling_method}).  Also accepted via the
    #'   legacy alias \code{allocation_method}.  Allowable values are
    #'   \code{"simple_random"}, \code{"proportional"}, \code{"pps_cluster"},
    #'   \code{"pps_rlc"}, \code{"systematic"}, \code{"simple_random_rlc"},
    #'   \code{"systematic_rlc"}, \code{"proportional_rlc"}, and \code{"purposive"}.
    #'   Defaults to \code{"simple_random"}.
    #' @param allocation_method Deprecated alias for \code{sampling_method}.
    #' @param pop_indicator Character. Indicator label for population-level
    #'   sample size calculation (default \code{"General"}).
    #' @param pop_expected_prevalence Numeric. Expected prevalence used for
    #'   population-level sample size (\%).
    #' @param pop_precision Numeric. Desired precision for population-level
    #'   estimate (\%).
    #' @param pop_nonresponse Numeric. Expected non-response rate for
    #'   population-level calculation (\%).
    #' @param pop_design_effect Numeric. Design effect for population-level
    #'   calculation.  Also accepted via the legacy alias \code{design_effect}.
    #' @param pop_fpc Logical. Apply finite population correction at population
    #'   level?  Defaults to \code{FALSE}.
    #' @param General_HH_Sample_Size Numeric. Calculated (or placeholder) general
    #'   household-level sample size (from population-level calculation).
    #' @param ind_indicator Character. Indicator label for individual-level
    #'   sample size calculation.
    #' @param ind_expected_prevalence Numeric. Expected prevalence for
    #'   individual-level calculation (\%).
    #' @param ind_precision Numeric. Desired precision for individual-level
    #'   estimate (\%).
    #' @param ind_nonresponse Numeric. Expected non-response rate for
    #'   individual-level calculation (\%).
    #' @param ind_design_effect Numeric. Design effect for individual-level
    #'   calculation.
    #' @param ind_avg_hh_size Numeric. Average household size (individual
    #'   level).
    #' @param ind_subpop_prop Numeric. Sub-population proportion (\%) for
    #'   individual-level calculation.
    #' @param ind_fpc Logical. Apply finite population correction at individual
    #'   level?  Defaults to \code{FALSE}.
    #' @param Ind_Sample_Size Numeric. Calculated individual-level sample size
    #'   (number of individuals).
    #' @param Ind_HH_Sample_Size Numeric. Calculated individual-level sample
    #'   size expressed in households.
    #' @param mort_indicator Character. Indicator label for mortality-level
    #'   sample size calculation.
    #' @param mort_expected_death_rate Numeric. Expected crude death rate used
    #'   for mortality sample size calculation.
    #' @param mort_precision Numeric. Desired precision for mortality estimate.
    #' @param mort_nonresponse Numeric. Expected non-response rate for
    #'   mortality calculation (\%).
    #' @param mort_design_effect Numeric. Design effect for mortality
    #'   calculation.
    #' @param mort_recall_days Integer. Recall period in days for mortality
    #'   estimate.
    #' @param mort_avg_hh_size Numeric. Average household size (mortality
    #'   level).
    #' @param mort_fpc Logical. Apply finite population correction at mortality
    #'   level?  Defaults to \code{FALSE}.
    #' @param Mort_Ind_Sample_Size Numeric. Calculated mortality-level
    #'   individual sample size.
    #' @param Mort_PT_Sample_Size Numeric. Calculated mortality-level
    #'   person-time sample size.
    #' @param Mort_HH_Sample_Size Numeric. Calculated mortality-level
    #'   household sample size.
    #' @param teams Numeric. Number of field teams.
    #' @param avg_interview_time Numeric. Average interview time in minutes.
    #' @param clusters_per_day Numeric. Number of clusters visited per day per
    #'   team.
    #' @param enumerators_per_team Numeric. Number of enumerators per team.
    #' @param avg_rest_time Numeric. Average rest/break time in minutes per
    #'   day.
    #' @param avg_travel_time Numeric. Average travel time to cluster in
    #'   minutes.
    #' @param start_time Character. Planned work start time (e.g.
    #'   \code{"08:00"}).
    #' @param end_time Character. Planned work end time (e.g.
    #'   \code{"17:00"}).
    #' @param design_effect Deprecated alias for \code{pop_design_effect}.
    #' @param precision Deprecated alias for \code{pop_precision}.
    #' @param confidence_level Deprecated / ignored in the new schema.
    #' @param n_psu Integer.  Number of PSUs to select.  Required by
    #'   \code{draw_sample()} when \code{sampling_method} is \code{"pps_cluster"};
    #'   may be omitted at \code{add_stratum()} time and set later (e.g. by
    #'   \code{calculate_sample_sizes()}).
    #' @param cluster_size Integer.  Households per cluster.  Used by
    #'   \code{draw_sample()} when \code{sampling_method} is \code{"pps_cluster"},
    #'   \code{"pps_rlc"}, \code{"simple_random_rlc"}, \code{"systematic_rlc"}, or
    #'   \code{"proportional_rlc"} (automatically set to \code{3} for RLC methods if
    #'   not supplied).
    #' @param n_sites Integer.  Number of sites / PSUs to select.  Required for
    #'   \code{"simple_random"}, \code{"pps_rlc"}, \code{"systematic"},
    #'   \code{"simple_random_rlc"}, and \code{"systematic_rlc"} sampling methods.
    #'   Not used by \code{"proportional"}, \code{"proportional_rlc"}, or
    #'   \code{"purposive"} (which apply to all eligible PSUs).
    #' @return Invisibly returns \code{self} for method chaining.
    add_stratum = function(
      stratum_id,
      stratum_name,
      population_size        = NA_real_,
      total_households       = NA_real_,
      sampling_method        = NULL,
      allocation_method      = NULL,   # legacy alias for sampling_method
      n_psu                  = NA_real_,
      cluster_size           = NA_real_,
      n_sites                = NA_real_,
      pop_indicator          = "General",
      pop_expected_prevalence = NA_real_,
      pop_precision          = NA_real_,
      pop_nonresponse        = NA_real_,
      pop_design_effect      = NA_real_,
      pop_fpc                = FALSE,
      General_HH_Sample_Size = NA_real_,
      ind_indicator          = NA_character_,
      ind_expected_prevalence = NA_real_,
      ind_precision          = NA_real_,
      ind_nonresponse        = NA_real_,
      ind_design_effect      = NA_real_,
      ind_avg_hh_size        = NA_real_,
      ind_subpop_prop        = NA_real_,
      ind_fpc                = FALSE,
      Ind_Sample_Size        = NA_real_,
      Ind_HH_Sample_Size     = NA_real_,
      mort_indicator         = NA_character_,
      mort_expected_death_rate = NA_real_,
      mort_precision         = NA_real_,
      mort_nonresponse       = NA_real_,
      mort_design_effect     = NA_real_,
      mort_recall_days       = NA_real_,
      mort_avg_hh_size       = NA_real_,
      mort_fpc               = FALSE,
      Mort_Ind_Sample_Size   = NA_real_,
      Mort_PT_Sample_Size    = NA_real_,
      Mort_HH_Sample_Size    = NA_real_,
      teams                  = NA_real_,
      avg_interview_time     = NA_real_,
      clusters_per_day       = NA_real_,
      enumerators_per_team   = NA_real_,
      avg_rest_time          = NA_real_,
      avg_travel_time        = NA_real_,
      start_time             = NA_character_,
      end_time               = NA_character_,
      # Legacy / deprecated params kept for backward compatibility
      design_effect          = NULL,
      precision              = NULL,
      confidence_level       = NULL
    ) {
      super$sample_add_stratum(
        stratum_id = stratum_id,
        stratum_name = stratum_name,
        population_size = population_size,
        total_households = total_households,
        sampling_method = sampling_method,
        allocation_method = allocation_method,
        n_psu = n_psu,
        cluster_size = cluster_size,
        n_sites = n_sites,
        pop_indicator = pop_indicator,
        pop_expected_prevalence = pop_expected_prevalence,
        pop_precision = pop_precision,
        pop_nonresponse = pop_nonresponse,
        pop_design_effect = pop_design_effect,
        pop_fpc = pop_fpc,
        General_HH_Sample_Size = General_HH_Sample_Size,
        ind_indicator = ind_indicator,
        ind_expected_prevalence = ind_expected_prevalence,
        ind_precision = ind_precision,
        ind_nonresponse = ind_nonresponse,
        ind_design_effect = ind_design_effect,
        ind_avg_hh_size = ind_avg_hh_size,
        ind_subpop_prop = ind_subpop_prop,
        ind_fpc = ind_fpc,
        Ind_Sample_Size = Ind_Sample_Size,
        Ind_HH_Sample_Size = Ind_HH_Sample_Size,
        mort_indicator = mort_indicator,
        mort_expected_death_rate = mort_expected_death_rate,
        mort_precision = mort_precision,
        mort_nonresponse = mort_nonresponse,
        mort_design_effect = mort_design_effect,
        mort_recall_days = mort_recall_days,
        mort_avg_hh_size = mort_avg_hh_size,
        mort_fpc = mort_fpc,
        Mort_Ind_Sample_Size = Mort_Ind_Sample_Size,
        Mort_PT_Sample_Size = Mort_PT_Sample_Size,
        Mort_HH_Sample_Size = Mort_HH_Sample_Size,
        teams = teams,
        avg_interview_time = avg_interview_time,
        clusters_per_day = clusters_per_day,
        enumerators_per_team = enumerators_per_team,
        avg_rest_time = avg_rest_time,
        avg_travel_time = avg_travel_time,
        start_time = start_time,
        end_time = end_time,
        design_effect = design_effect,
        precision = precision,
        confidence_level = confidence_level
      )

      private$touch()
      private$add_target_stratum()
      private$check_issues()
      phr_message(phr_txt("Stratum '{stratum_id}' added."), origin = "SurveyProtocol$add_stratum")
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

        self$sampling_frame$log_df <- tibble::as_tibble(frame)
        self$sync_sampling_frame_fields
        private$touch()
        private$check_issues()
        phr_message(
          phr_txt("Sampling frame set with {nrow(frame)} PSUs."),
          origin = "SurveyProtocol$set_sampling_frame"
        )

      }, on_error = "abort", origin = "SurveyProtocol$set_sampling_frame")
      invisible(self)
    },

    #' @description Draw sample from the sampling frame
    #'
    #' Applies PSU-level sampling methods to the eligible PSUs in the sampling
    #' frame.  Results are stored in \code{drawn_sample_full} (the full frame
    #' annotated with \code{sampled_psu} and \code{allocated_sample} columns)
    #' and \code{drawn_sample} (only the selected rows).
    #'
    #' Sampling method and all method-specific parameters (\code{n_psu},
    #' \code{cluster_size}, \code{n_sites}) are read from the strata table
    #' row(s).  When the frame contains a \code{stratum} column and the strata
    #' table has multiple rows, sampling is applied independently per stratum
    #' using that stratum's own method and parameters.  If a stratum fails
    #' (e.g. a required parameter is missing), a \code{phr_warning} is issued
    #' and that stratum is skipped.
    #'
    #' Supported \code{sampling_method} values in the strata table:
    #' \itemize{
    #'   \item \code{"simple_random"} — simple random sampling; requires \code{n_sites}.
    #'   \item \code{"proportional"} — proportional household allocation across
    #'     \strong{all} eligible PSUs; requires \code{population_size} in the frame.
    #'     \code{n_sites} does not apply.
    #'   \item \code{"pps_cluster"} — PPS cluster sampling; requires
    #'     \code{n_psu} and \code{cluster_size}.
    #'   \item \code{"pps_rlc"} — random location cluster; sites pre-selected by
    #'     PPS (\code{pps::ppswor}), clusters evenly distributed across sites.
    #'     Requires \code{n_sites} and \code{population_size}; defaults
    #'     \code{cluster_size} to \code{3}.
    #'   \item \code{"simple_random_rlc"} — random location cluster with SRS
    #'     site selection; clusters allocated proportional to population size
    #'     when \code{population_size} is available, otherwise evenly.
    #'     Requires \code{n_sites}; defaults \code{cluster_size} to \code{3}.
    #'   \item \code{"systematic_rlc"} — random location cluster with systematic
    #'     site selection; clusters allocated proportional to population size
    #'     when \code{population_size} is available, otherwise evenly.
    #'     Requires \code{n_sites}; defaults \code{cluster_size} to \code{3}.
    #'   \item \code{"proportional_rlc"} — cluster allocation proportional to
    #'     population size across \strong{all} eligible PSUs; requires
    #'     \code{population_size} in the frame.  \code{n_sites} does not apply.
    #'     Defaults \code{cluster_size} to \code{3}.
    #'   \item \code{"systematic"} — systematic sampling; requires
    #'     \code{n_sites}.
    #'   \item \code{"purposive"} — purposive / convenience sampling; returns
    #'     \code{NA} for \code{sampled_psu} and \code{allocated_sample} —
    #'     the user manually designates selected PSUs.  \code{n_sites} does not apply.
    #' }
    #'
    #' @param frame Data frame.  The sampling frame to draw from.  If
    #'   \code{NULL} (default), uses \code{self$sampling_frame$log_df}.
    #' @param strata_table Data frame.  The strata table supplying
    #'   \code{sampling_method}, \code{Final_HH_Sample_Size}, and sampling
    #'   parameters (\code{n_psu}, \code{cluster_size}, \code{n_sites}).  If
    #'   \code{NULL} (default), uses \code{self$get_sample_table()}.
    #' @param seed Integer. Random seed for reproducibility (default \code{42}).
    #' @return Invisibly returns \code{self} for method chaining.
    draw_sample = function(frame = NULL, strata_table = NULL, seed = 42) {
      phr_try({

        # -- Resolve frame and strata_table --
        if (is.null(frame)) {
          phr_assert(
            !is.null(self$sampling_frame) && nrow(self$sampling_frame$log_df) > 0,
            message = phr_txt("Must set sampling frame before drawing sample."),
            origin  = "SurveyProtocol$draw_sample",
            hint    = phr_txt("Call set_sampling_frame() first, or pass a frame argument.")
          )
          frame <- as.data.frame(self$sampling_frame$log_df)
        }
        phr_validate_dataframe(frame, origin = "SurveyProtocol$draw_sample", soft = FALSE)

        if (is.null(strata_table)) {
          st <- self$get_sample_table()
          phr_assert(
            !is.null(st) && nrow(st) > 0,
            message = phr_txt("No strata table available. Call add_stratum() first or pass a strata_table argument."),
            origin  = "SurveyProtocol$draw_sample"
          )
          strata_table <- st
        }
        phr_validate_dataframe(strata_table, origin = "SurveyProtocol$draw_sample", soft = FALSE)
        phr_assert(
          "sampling_method" %in% names(strata_table),
          message = phr_txt("strata_table must contain a 'sampling_method' column."),
          origin  = "SurveyProtocol$draw_sample"
        )

        # Ensure method-specific parameter columns exist (add as NA if absent)
        for (col in c("n_psu", "cluster_size", "n_sites", "Final_HH_Sample_Size")) {
          if (!col %in% names(strata_table)) strata_table[[col]] <- NA_real_
        }

        # Eligible PSUs
        if ("inclusion" %in% names(frame)) {
          eligible_rows <- which(!is.na(frame$inclusion) & frame$inclusion)
        } else {
          eligible_rows <- seq_len(nrow(frame))
        }
        eligible_frame <- frame[eligible_rows, , drop = FALSE]

        # Initialise output columns on full frame
        frame$sampled_psu      <- NA_character_
        frame$allocated_sample <- NA_real_

        is_stratified <- ("stratum" %in% names(eligible_frame)) &&
          ("stratum_id" %in% names(strata_table)) &&
          (nrow(strata_table) > 1L || !is.null(strata_table$stratum_id))

        if (is_stratified) {
          strata_ids     <- unique(strata_table$stratum_id)
          cluster_offset <- 0L

          for (st_id in strata_ids) {
            st_row <- strata_table[strata_table$stratum_id == st_id, ]
            if (nrow(st_row) == 0L) next

            st_eligible_rows <- which(eligible_frame$stratum == st_id)
            if (length(st_eligible_rows) == 0L) {
              phr_warning(
                message = phr_txt("Stratum '{st_id}' not found in sampling frame — skipping."),
                origin  = "SurveyProtocol$draw_sample"
              )
              next
            }
            st_frame <- eligible_frame[st_eligible_rows, , drop = FALSE]

            st_params <- private$params_from_strata_row(
              st_row, nrow(st_frame), nrow(eligible_frame)
            )

            st_result <- tryCatch(
              private$apply_sampling_method(
                frame        = st_frame,
                method       = st_params$method,
                sample_size  = st_params$sample_size,
                n_psu        = st_params$n_psu,
                n_sites      = st_params$n_sites,
                cluster_size = st_params$cluster_size,
                seed         = seed
              ),
              error = function(e) {
                phr_warning(
                  message = phr_txt("Sampling for stratum '{st_id}' failed and will be skipped: {conditionMessage(e)}"),
                  origin  = "SurveyProtocol$draw_sample"
                )
                NULL
              }
            )
            if (is.null(st_result)) next

            sel_mask <- !is.na(st_result$sampled_psu)
            if (any(sel_mask)) {
              # parse_cluster_labels: extract the numeric cluster numbers from a
              # comma-separated sampled_psu string, ignoring "RC" tokens.
              parse_cluster_labels <- function(s) {
                parts <- trimws(strsplit(as.character(s), ",\\s*")[[1]])
                nums  <- suppressWarnings(as.integer(parts))
                nums[!is.na(nums)]
              }
              # Apply cross-stratum offset to numeric labels; "RC" tokens are kept unchanged.
              apply_cluster_offset <- function(s, offset) {
                parts <- trimws(strsplit(as.character(s), ",\\s*")[[1]])
                vapply(parts, function(p) {
                  n <- suppressWarnings(as.integer(p))
                  if (is.na(n)) p else as.character(n + offset)
                }, character(1), USE.NAMES = FALSE)
              }
              st_result$sampled_psu[sel_mask] <- vapply(
                st_result$sampled_psu[sel_mask],
                function(s) paste(apply_cluster_offset(s, cluster_offset), collapse = ", "),
                character(1)
              )
              all_nums <- unlist(lapply(st_result$sampled_psu[sel_mask], parse_cluster_labels))
              if (length(all_nums) > 0L) {
                # Guard: purposive strata have sel_mask=FALSE and skip this block; other
                # methods always produce at least one numeric label (n_psu > 0), so
                # all_nums should be non-empty in practice.
                cluster_offset <- cluster_offset + max(all_nums)
              }
            }

            full_frame_rows <- eligible_rows[st_eligible_rows]
            frame$sampled_psu[full_frame_rows]      <- st_result$sampled_psu
            frame$allocated_sample[full_frame_rows] <- st_result$allocated_sample
          }

        } else {
          # Non-stratified: use first strata table row for the whole frame
          st_row    <- strata_table[1L, ]
          st_params <- private$params_from_strata_row(
            st_row, nrow(eligible_frame), nrow(eligible_frame)
          )

          result <- tryCatch(
            private$apply_sampling_method(
              frame        = eligible_frame,
              method       = st_params$method,
              sample_size  = st_params$sample_size,
              n_psu        = st_params$n_psu,
              n_sites      = st_params$n_sites,
              cluster_size = st_params$cluster_size,
              seed         = seed
            ),
            error = function(e) {
              phr_warning(
                message = phr_txt("Sampling failed: {conditionMessage(e)}"),
                origin  = "SurveyProtocol$draw_sample"
              )
              NULL
            }
          )
          if (!is.null(result)) {
            frame$sampled_psu[eligible_rows]      <- result$sampled_psu
            frame$allocated_sample[eligible_rows] <- result$allocated_sample
          }
        }

        self$drawn_sample_full <- frame
        # For purposive, drawn_sample is empty (user fills manually)
        self$drawn_sample <- frame[!is.na(frame$sampled_psu), , drop = FALSE]

        # Update the SamplingFrame object with the annotated frame
        self$sampling_frame$log_df <- tibble::as_tibble(frame)

        self$sync_sampling_frame_fields
        private$touch()
        private$check_issues()
        phr_message(
          phr_txt("Sample drawn: {nrow(self$drawn_sample)} PSU(s) selected."),
          origin = "SurveyProtocol$draw_sample"
        )

      }, on_error = "abort", origin = "SurveyProtocol$draw_sample")
      invisible(self)
    },

    #' @description Clear sample selection from the sampling frame
    #'
    #' Resets the \code{sampled_psu} and \code{allocated_sample} columns of the
    #' \code{\link{SamplingFrame}} to \code{NA}, leaving all other columns
    #' (e.g. \code{stratum}, \code{psu}, \code{population_size},
    #' \code{inclusion}) intact.  Also clears the \code{drawn_sample} and
    #' \code{drawn_sample_full} fields.
    #'
    #' This is useful when you want to re-draw the sample with different
    #' parameters without discarding the sampling frame itself.
    #'
    #' @return Invisibly returns \code{self} for method chaining.
    clear_sample = function() {
      phr_try({
        if (!is.null(self$sampling_frame) &&
            nrow(self$sampling_frame$log_df) > 0) {
          if ("sampled_psu" %in% names(self$sampling_frame$log_df)) {
            self$sampling_frame$log_df$sampled_psu <- NA_character_
          }
          if ("allocated_sample" %in% names(self$sampling_frame$log_df)) {
            self$sampling_frame$log_df$allocated_sample <- NA_real_
          }
        }
        self$drawn_sample      <- NULL
        self$drawn_sample_full <- NULL
        self$sync_sampling_frame_fields
        private$touch()
        private$check_issues()
        phr_message(
          phr_txt("Sample cleared from sampling frame."),
          origin = "SurveyProtocol$clear_sample"
        )
      }, on_error = "abort", origin = "SurveyProtocol$clear_sample")
      invisible(self)
    },

    #' @description Validate the structure of the master sample table
    #'
    #' Checks that \code{sample_table} exists and contains all required master
    #' table columns.
    #'
    #' @return \code{TRUE} if valid, \code{FALSE} otherwise.
    validate_strata_table = function() {
      if (is.null(self$sample_table) || !inherits(self$sample_table, "Sample")) return(FALSE)
      isTRUE(self$sample_table$validate_strata_table())
    },

    #' @description Get the sample table
    #' @return Data frame containing the sample table
    get_sample_table = function() {
      if (is.null(self$sample_table) || !inherits(self$sample_table, "Sample")) return(NULL)
      self$sample_table$get_sample_table()
    },

    #' @description Replace the current sample table through the inherited
    #'   \code{Protocol} sample accessor and keep survey metadata in sync.
    #' @param sample_table Data frame.
    #' @return Invisibly returns \code{self}.
    sample_set_sample_table = function(sample_table) {
      super$sample_set_sample_table(sample_table)
      private$add_target_stratum()
      private$check_issues()
      invisible(self)
    },

    #' @description Clear the current sample table through the inherited
    #'   \code{Protocol} sample accessor and keep survey metadata in sync.
    #' @return Invisibly returns \code{self}.
    sample_clear_sample_table = function() {
      super$sample_clear_sample_table()
      private$add_target_stratum()
      private$check_issues()
      invisible(self)
    },

    #' @description Add a stratum row through the inherited \code{Protocol}
    #'   sample accessor and keep survey metadata in sync.
    #' @param ... Arguments forwarded to \code{Sample$add_stratum()}.
    #' @return Invisibly returns \code{self}.
    sample_add_stratum = function(...) {
      super$sample_add_stratum(...)
      private$add_target_stratum()
      private$check_issues()
      invisible(self)
    },

    #' @description Remove a stratum row through the inherited \code{Protocol}
    #'   sample accessor and keep survey metadata in sync.
    #' @param strata_name Character scalar naming the stratum to remove.
    #' @return Invisibly returns \code{self}.
    sample_remove_stratum = function(strata_name) {
      super$sample_remove_stratum(strata_name = strata_name)
      private$add_target_stratum()
      private$check_issues()
      invisible(self)
    },

    # ── Sampling helpers ────────────────────────────────────────────────────

    #' @description Return the unique sampling methods used across all strata.
    #'
    #' Reads the \code{sampling_method} column of \code{self$get_sample_table()}.
    #'
    #' @return Character vector of unique, non-NA sampling method values.
    #'   Empty character vector when no sample table is set.
    get_sampling_methods = function() {
      if (is.null(self$sample_table) || !inherits(self$sample_table, "Sample")) return(character(0))
      self$sample_table$get_sampling_methods()
    },

    #' @description Return the stratum names from the sample table.
    #'
    #' Uses \code{Population_Name} when available, falling back to
    #' \code{stratum_id}.
    #'
    #' @return Character vector of stratum names.  Empty character vector
    #'   when no sample table is set.
    get_strata_names = function() {
      if (is.null(self$sample_table) || !inherits(self$sample_table, "Sample")) return(character(0))
      self$sample_table$get_strata_names()
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

    #' @description Get protocol summary including sampling information
    #' @return List with protocol summary information
    get_protocol_summary = function() {
      base_summary <- super$get_protocol_summary()
      st <- self$get_sample_table()
      base_summary$num_strata       <- if (is.null(st)) 0L else nrow(st)
      base_summary$total_sample_size <- if (is.null(st)) 0 else sum(st$General_HH_Sample_Size, na.rm = TRUE)
      base_summary
    },

    #' @description Calculate sample sizes for all strata in the sample table
    #'
    #' Delegates to \code{\link{calculate_sample_size_strata_table}}, which
    #' reads each stratum row from \code{sample_table}, calls the appropriate
    #' \code{calculate_sample_size_*} function for each applicable calculation
    #' type (general, individual, mortality), sets \code{Final_HH_Sample_Size}
    #' to the maximum household sample size across the three types, and also
    #' estimates the field plan for each stratum where the necessary logistics
    #' parameters are present.
    #'
    #' @return Invisibly returns \code{self} for method chaining.
    calculate_sample_sizes = function() {
      phr_try({
        phr_assert(
          !is.null(self$sample_table) && inherits(self$sample_table, "Sample"),
          message = phr_txt("sample_table must be a Sample object."),
          origin  = "SurveyProtocol$calculate_sample_sizes"
        )
        super$sample_calculate_sample_sizes()

        private$add_target_stratum()
        private$check_issues()
        phr_message(
          phr_txt("Sample sizes calculated for {nrow(self$get_sample_table())} stratum/strata."),
          origin = "SurveyProtocol$calculate_sample_sizes"
        )
      }, on_error = "abort", origin = "SurveyProtocol$calculate_sample_sizes")
      invisible(self)
    },

    #' @description Calculate sample sizes through the inherited \code{Protocol}
    #'   sample accessor and keep survey metadata in sync.
    #' @return Invisibly returns \code{self}.
    sample_calculate_sample_sizes = function() {
      super$sample_calculate_sample_sizes()
      private$add_target_stratum()
      private$check_issues()
      invisible(self)
    },

    #' @description Add strata names to a tool's choices list and insert a
    #'   \emph{Please select strata} question into the survey form.
    #'
    #' Creates (or replaces) a \code{strata} choice list in the tool's
    #' \code{revised_choices}, then inserts a \code{select_one strata} question
    #' immediately after the last row whose \code{indicator_code} is
    #' \code{10000} (the core header block) in \code{revised_survey}.  If
    #' \code{strata_names} is \code{NULL} the names are taken from the
    #' stratum names from \code{self$sample_table} (\code{Sample} object).
    #'
    #' @param strata_names Character vector of stratum names.  Defaults to
    #'   \code{NULL}, in which case \code{self$get_strata_names()} is used.
    #' @param tool_name Character. Name of the tool to modify (key in
    #'   \code{self$tools}).  When \code{NULL} (default) and exactly one tool
    #'   is registered, that tool is used.
    #' @return Invisibly returns \code{self} for method chaining.
    add_strata_to_survey = function(strata_names = NULL, tool_name = NULL) {
      phr_try({

        # Resolve strata names
        if (is.null(strata_names)) {
          st <- self$get_sample_table()
          phr_assert(
            !is.null(st) && nrow(st) > 0,
            message = phr_txt("strata_names is NULL and sample_table is empty. Either call add_stratum() first, or pass strata_names explicitly."),
            origin  = "SurveyProtocol$add_strata_to_survey"
          )
          strata_names <- self$get_strata_names()
        }

        strata_names <- as.character(strata_names)
        strata_names <- strata_names[!is.na(strata_names) & nzchar(strata_names)]
        phr_assert(
          length(strata_names) > 0,
          message = phr_txt("strata_names must contain at least one non-empty name."),
          origin  = "SurveyProtocol$add_strata_to_survey"
        )

        # Resolve tool
        tool <- private$resolve_tool(tool_name, "SurveyProtocol$add_strata_to_survey")

        # ----- Build new choices rows -----
        # Match column structure of existing revised_choices
        ch <- tool$revised_choices
        if (is.null(ch)) ch <- tool$choices
        if (is.null(ch)) ch <- data.frame(list_name = character(0), name = character(0),
                                           label = character(0), stringsAsFactors = FALSE)

        # Remove any existing 'strata' list
        if ("list_name" %in% names(ch)) {
          ch <- ch[ch$list_name != "strata", , drop = FALSE]
        }

        # Build new strata rows aligned to ch columns
        strata_rows <- lapply(seq_along(strata_names), function(i) {
          nm  <- gsub("[^A-Za-z0-9_]", "_", tolower(strata_names[i]))
          row <- data.frame(list_name = "strata", name = nm, label = strata_names[i],
                            stringsAsFactors = FALSE)
          # Add extra columns present in ch but not in row
          for (col in setdiff(names(ch), names(row))) row[[col]] <- NA
          row[, union(names(row), names(ch)), drop = FALSE]
        })
        strata_df <- do.call(rbind, strata_rows)

        # Align columns to ch
        all_cols <- union(names(ch), names(strata_df))
        for (col in setdiff(all_cols, names(ch)))     ch[[col]]        <- NA
        for (col in setdiff(all_cols, names(strata_df))) strata_df[[col]] <- NA
        tool$revised_choices <- rbind(ch[, all_cols, drop = FALSE],
                                      strata_df[, all_cols, drop = FALSE])

        # ----- Insert strata question into revised_survey -----
        sv <- tool$revised_survey
        if (is.null(sv)) sv <- tool$survey

        if (!is.null(sv) && nrow(sv) > 0 && "indicator_code" %in% names(sv)) {
          last_10000 <- max(which(sv$indicator_code == 10000), 0L)
          insert_after <- if (last_10000 > 0L) last_10000 else 0L

          # Build new survey row
          new_row <- data.frame(
            type  = "select_one strata",
            name  = "strata",
            label = "Please select strata",
            stringsAsFactors = FALSE
          )
          # Add extra columns
          for (col in setdiff(names(sv), names(new_row))) new_row[[col]] <- NA
          new_row <- new_row[, names(sv), drop = FALSE]

          if (insert_after >= 1L && insert_after < nrow(sv)) {
            tool$revised_survey <- rbind(
              sv[seq_len(insert_after), , drop = FALSE],
              new_row,
              sv[seq(insert_after + 1L, nrow(sv)), , drop = FALSE]
            )
          } else if (insert_after == 0L) {
            tool$revised_survey <- rbind(new_row, sv)
          } else {
            tool$revised_survey <- rbind(sv, new_row)
          }
        }

        self$sync_tool_indicator_catalog_fields
        private$touch()
        phr_message(
          phr_txt("Strata choices ({length(strata_names)}) added and strata question inserted."),
          origin = "SurveyProtocol$add_strata_to_survey"
        )
      }, on_error = "abort", origin = "SurveyProtocol$add_strata_to_survey")
      invisible(self)
    },

    #' @description Add a \code{teams} choice list to a tool's \code{revised_choices}.
    #'
    #' Creates (or replaces) a \code{teams} choice list with entries
    #' \emph{Team 1} through \emph{Team n}.
    #'
    #' @param n_teams Positive integer. Number of teams.
    #' @param tool_name Character. Name of the tool to modify.  When
    #'   \code{NULL} (default) and exactly one tool is registered, that tool
    #'   is used.
    #' @return Invisibly returns \code{self} for method chaining.
    add_teams_to_choices = function(n_teams, tool_name = NULL) {
      phr_try({
        phr_assert(
          is.numeric(n_teams) && length(n_teams) == 1L && n_teams >= 1L,
          message = phr_txt("n_teams must be a single positive integer."),
          origin  = "SurveyProtocol$add_teams_to_choices"
        )
        n_teams <- as.integer(n_teams)
        tool    <- private$resolve_tool(tool_name, "SurveyProtocol$add_teams_to_choices")
        tool$revised_choices <- private$add_numbered_list(
          tool$revised_choices %||% tool$choices,
          list_name  = "teams",
          prefix_val = "team_",
          prefix_lbl = "Team ",
          n          = n_teams
        )
        self$sync_tool_indicator_catalog_fields
        private$touch()
        phr_message(
          phr_txt("{n_teams} team choice(s) added."),
          origin = "SurveyProtocol$add_teams_to_choices"
        )
      }, on_error = "abort", origin = "SurveyProtocol$add_teams_to_choices")
      invisible(self)
    },

    #' @description Add an \code{enumerators} choice list to a tool's
    #'   \code{revised_choices}.
    #'
    #' Creates (or replaces) an \code{enumerators} choice list with entries
    #' \emph{Enumerator 1} through \emph{Enumerator n}.
    #'
    #' @param n_enumerators Positive integer. Number of enumerators.
    #' @param tool_name Character. Name of the tool to modify.  When
    #'   \code{NULL} (default) and exactly one tool is registered, that tool
    #'   is used.
    #' @return Invisibly returns \code{self} for method chaining.
    add_enumerators_to_choices = function(n_enumerators, tool_name = NULL) {
      phr_try({
        phr_assert(
          is.numeric(n_enumerators) && length(n_enumerators) == 1L && n_enumerators >= 1L,
          message = phr_txt("n_enumerators must be a single positive integer."),
          origin  = "SurveyProtocol$add_enumerators_to_choices"
        )
        n_enumerators <- as.integer(n_enumerators)
        tool <- private$resolve_tool(tool_name, "SurveyProtocol$add_enumerators_to_choices")
        tool$revised_choices <- private$add_numbered_list(
          tool$revised_choices %||% tool$choices,
          list_name  = "enumerators",
          prefix_val = "enumerator_",
          prefix_lbl = "Enumerator ",
          n          = n_enumerators
        )
        self$sync_tool_indicator_catalog_fields
        private$touch()
        phr_message(
          phr_txt("{n_enumerators} enumerator choice(s) added."),
          origin = "SurveyProtocol$add_enumerators_to_choices"
        )
      }, on_error = "abort", origin = "SurveyProtocol$add_enumerators_to_choices")
      invisible(self)
    },

    #' @description Export protocol to a list including sampling data
    #' @return List containing all protocol data
    export_protocol = function() {
      base_export <- super$export_protocol()
      base_export$sample_table      <- self$get_sample_table()
      base_export$sample_object     <- self$sample_table
      base_export$sampling_frame    <- if (!is.null(self$sampling_frame)) self$sampling_frame$log_df else NULL
      base_export$drawn_sample      <- self$drawn_sample
      base_export$drawn_sample_full <- self$drawn_sample_full
      base_export$summary           <- self$get_protocol_summary()
      base_export
    }
  ),

  private = list(
    # Rebuild metadata$target_strata from the current sample_table.
    # Called automatically after any method that creates or modifies sample_table
    # so that metadata strata are always aligned with the sample_table.
    add_target_stratum = function() {
      st <- self$get_sample_table()
      if (!is.null(st) && nrow(st) > 0) {
        strata_ids   <- as.character(st$stratum_id)
        strata_names <- as.character(st$stratum_name)
        self$metadata$target_strata <- setNames(as.list(strata_names), strata_ids)
      } else {
        self$metadata$target_strata <- list()
      }
      invisible(NULL)
    },

    # Resolve a tool from self$tools.  If tool_name is NULL and there is
    # exactly one tool registered, return that tool.  Otherwise raise an error.
    resolve_tool = function(tool_name, origin) {
      if (is.null(tool_name)) {
        phr_assert(
          !is.null(self$tools) && length(self$tools) > 0,
          message = phr_txt("No tools registered. Add a tool with add_tools() first."),
          origin  = origin
        )
        if (length(self$tools) == 1L) {
          return(self$tools[[1L]])
        }
        phr_assert(
          FALSE,
          message = phr_txt("Multiple tools registered; supply tool_name to specify which tool to update. Available: {paste(names(self$tools), collapse=', ')}."),
          origin  = origin
        )
      }
      phr_assert(
        tool_name %in% names(self$tools),
        message = phr_txt("Tool '{tool_name}' not found. Available: {paste(names(self$tools), collapse=', ')}."),
        origin  = origin
      )
      self$tools[[tool_name]]
    },

    # Build (or replace) a numbered choice list within an existing choices df.
    # list_name  : name of the choice list (e.g. "teams")
    # prefix_val : value prefix (e.g. "team_")
    # prefix_lbl : label prefix (e.g. "Team ")
    # n          : integer count
    add_numbered_list = function(choices, list_name, prefix_val, prefix_lbl, n) {
      if (is.null(choices)) {
        choices <- data.frame(list_name = character(0), name = character(0),
                              label = character(0), stringsAsFactors = FALSE)
      }
      # Remove any existing entries for this list
      if ("list_name" %in% names(choices)) {
        choices <- choices[choices$list_name != list_name, , drop = FALSE]
      }
      new_rows <- lapply(seq_len(n), function(i) {
        row <- data.frame(list_name = list_name,
                          name  = paste0(prefix_val, i),
                          label = paste0(prefix_lbl, i),
                          stringsAsFactors = FALSE)
        for (col in setdiff(names(choices), names(row))) row[[col]] <- NA
        row
      })
      new_df   <- do.call(rbind, new_rows)
      all_cols <- union(names(choices), names(new_df))
      for (col in setdiff(all_cols, names(choices)))  choices[[col]]  <- NA
      for (col in setdiff(all_cols, names(new_df)))   new_df[[col]]   <- NA
      rbind(choices[, all_cols, drop = FALSE], new_df[, all_cols, drop = FALSE])
    },

    # Generic hooks for TOR sample-size table tags.
    # Subclasses can override with protocol-specific table builders.
    add_sample_size_gen_table = function(doc) {
      private$.replace_schema_tag(doc, "@sample_size_hh_gen_table", "")
    },

    add_sample_size_ind_table = function(doc) {
      private$.replace_schema_tag(doc, "@sample_size_hh_ind_table", "")
    },

    add_sample_size_mort_table = function(doc) {
      private$.replace_schema_tag(doc, "@sample_size_hh_mort_table", "")
    },

    # Check for issues and discrepancies in the survey protocol,
    # including strata consistency between frame and sample table.
    check_issues = function() {
      self$issues <- list()

      # Check if objectives have matching indicators in tools
      all_objectives <- flatten_objectives(self$objectives)
      if (length(all_objectives) > 0 && length(self$tools) > 0) {
        obj_sectors <- unique(sapply(all_objectives, function(x) x$sector))

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
            self$issues$strata_missing_in_frame <- paste(
              "Strata in sample table but not in frame:",
              paste(missing_in_frame, collapse = ", ")
            )
          }
          if (length(missing_in_table) > 0) {
            self$issues$strata_missing_in_table <- paste(
              "Strata in frame but not in sample table:",
              paste(missing_in_table, collapse = ", ")
            )
          }
        }
      }

      invisible(self)
    },

    # Apply a PSU-level sampling method — dispatches to draw_sample_psu_* utilities
    apply_sampling_method = function(frame, method, sample_size, n_psu,
                                     n_sites, cluster_size, seed) {
      origin <- "SurveyProtocol$apply_sampling_method"
      valid_methods <- c("simple_random", "proportional", "pps_cluster", "pps_rlc",
                         "systematic", "simple_random_rlc", "systematic_rlc",
                         "proportional_rlc", "purposive")
      phr_assert(
        method %in% valid_methods,
        message = phr_txt("Unknown sampling method '{method}' — must be one of: {paste(valid_methods, collapse=', ')}."),
        origin  = origin
      )

      if (method == "simple_random") {
        phr_assert(!is.null(n_sites) && !is.na(n_sites),
                   message = phr_txt("n_sites is required for the 'simple_random' method — set the 'n_sites' column in the strata table."),
                   origin = origin)
        draw_sample_psu_srs(frame, n_sites, sample_size, seed)
      } else if (method == "proportional") {
        draw_sample_psu_proportional(frame, sample_size, seed)
      } else if (method == "pps_cluster") {
        phr_assert(!is.null(n_psu) && !is.na(n_psu),
                   message = phr_txt("n_psu is required for the 'pps_cluster' method — set the 'n_psu' column in the strata table."),
                   origin = origin)
        phr_assert(!is.null(cluster_size) && !is.na(cluster_size),
                   message = phr_txt("cluster_size is required for the 'pps_cluster' method — set the 'cluster_size' column in the strata table."),
                   origin = origin)
        draw_sample_psu_pps_cluster(frame, n_psu, cluster_size, seed)
      } else if (method == "pps_rlc") {
        phr_assert(!is.null(n_sites) && !is.na(n_sites),
                   message = phr_txt("n_sites is required for the 'pps_rlc' method — set the 'n_sites' column in the strata table."),
                   origin = origin)
        cs <- if (!is.null(cluster_size) && !is.na(cluster_size)) cluster_size else 3L
        draw_sample_psu_rlc(frame, sample_size, n_sites, cs, seed)
      } else if (method == "simple_random_rlc") {
        phr_assert(!is.null(n_sites) && !is.na(n_sites),
                   message = phr_txt("n_sites is required for the 'simple_random_rlc' method — set the 'n_sites' column in the strata table."),
                   origin = origin)
        cs <- if (!is.null(cluster_size) && !is.na(cluster_size)) cluster_size else 3L
        draw_sample_psu_srs_rlc(frame, sample_size, n_sites, cs, seed)
      } else if (method == "systematic_rlc") {
        phr_assert(!is.null(n_sites) && !is.na(n_sites),
                   message = phr_txt("n_sites is required for the 'systematic_rlc' method — set the 'n_sites' column in the strata table."),
                   origin = origin)
        cs <- if (!is.null(cluster_size) && !is.na(cluster_size)) cluster_size else 3L
        draw_sample_psu_systematic_rlc(frame, sample_size, n_sites, cs, seed)
      } else if (method == "proportional_rlc") {
        cs <- if (!is.null(cluster_size) && !is.na(cluster_size)) cluster_size else 3L
        draw_sample_psu_proportional_rlc(frame, sample_size, cs, seed)
      } else if (method == "systematic") {
        phr_assert(!is.null(n_sites) && !is.na(n_sites),
                   message = phr_txt("n_sites is required for the 'systematic' method — set the 'n_sites' column in the strata table."),
                   origin = origin)
        draw_sample_psu_systematic(frame, n_sites, sample_size, seed)
      } else {  # purposive
        draw_sample_psu_purposive(frame, seed)
      }
    },

    # Extract sampling parameters from a single strata table row.
    # Returns a named list: method, sample_size, n_psu, cluster_size, n_sites.
    # sample_size falls back to General_HH_Sample_Size, then a proportional estimate when
    # Final_HH_Sample_Size is NA.
    params_from_strata_row = function(st_row, stratum_n_eligible, total_n_eligible) {
      method <- as.character(st_row$sampling_method[1])

      ss <- if ("Final_HH_Sample_Size" %in% names(st_row) &&
                !is.na(st_row$Final_HH_Sample_Size[1])) {
        as.integer(st_row$Final_HH_Sample_Size[1])
      } else {
        # Fallback: use the calculated general population-level sample size if available
        if ("General_HH_Sample_Size" %in% names(st_row) && !is.na(st_row$General_HH_Sample_Size[1])) {
          as.integer(st_row$General_HH_Sample_Size[1])
        } else {
          as.integer(round(100 * stratum_n_eligible / max(total_n_eligible, 1L)))
        }
      }

      # Convert NA to NULL so callers can use is.null() checks
      .na_as_null <- function(x) if (length(x) == 0L || is.na(x)) NULL else x

      list(
        method       = method,
        sample_size  = ss,
        n_psu        = .na_as_null(if ("n_psu"        %in% names(st_row)) st_row$n_psu[1]        else NA),
        cluster_size = .na_as_null(if ("cluster_size" %in% names(st_row)) st_row$cluster_size[1] else NA),
        n_sites      = .na_as_null(if ("n_sites"      %in% names(st_row)) st_row$n_sites[1]      else NA)
      )
    }
  )
)
