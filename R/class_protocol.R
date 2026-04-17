#' Protocol R6 Class
#'
#' @description
#' Main class for managing the complete protocols pipeline workflow.
#' Allows flexible interaction with different workflow components:
#' 1. Objective Selection (nested by sector, pillar, sub-pillar, and data source)
#' 2. Strata Definition and Sample Size Calculations
#' 3. Sampling Frame Validation and Sample Drawing
#' 4. Tool and Indicator Selection
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

    #' @field sample_table Master data frame with one row per stratum and all
    #'   relevant population, sample-size, and logistics parameters
    sample_table = NULL,

    #' @field sampling_frame Data frame with sampling units and strata
    sampling_frame = NULL,

    #' @field drawn_sample Data frame with selected PSUs (filtered from drawn_sample_full)
    drawn_sample = NULL,

    #' @field drawn_sample_full Full sampling frame with sampled_psu and allocated_sample columns
    drawn_sample_full = NULL,

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
    #'   stratum (stored as \code{Sampling_Method}).  Also accepted via the
    #'   legacy alias \code{allocation_method}.  Defaults to \code{"srs"}.
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
    #' @param pop_result_dummy Numeric. Calculated (or placeholder) sample size
    #'   at population level.
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
    #' @param ind_result_dummy Numeric. Calculated (or placeholder) sample size
    #'   at individual level.
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
    #' @param mort_result_dummy Numeric. Calculated (or placeholder) sample
    #'   size at mortality level.
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
    #' @return Invisibly returns \code{self} for method chaining.
    add_stratum = function(
      stratum_id,
      stratum_name,
      population_size        = NA_real_,
      total_households       = NA_real_,
      sampling_method        = "srs",
      allocation_method      = NULL,   # legacy alias for sampling_method
      pop_indicator          = "General",
      pop_expected_prevalence = NA_real_,
      pop_precision          = NA_real_,
      pop_nonresponse        = NA_real_,
      pop_design_effect      = NA_real_,
      pop_fpc                = FALSE,
      pop_result_dummy       = NA_real_,
      ind_indicator          = NA_character_,
      ind_expected_prevalence = NA_real_,
      ind_precision          = NA_real_,
      ind_nonresponse        = NA_real_,
      ind_design_effect      = NA_real_,
      ind_avg_hh_size        = NA_real_,
      ind_subpop_prop        = NA_real_,
      ind_fpc                = FALSE,
      ind_result_dummy       = NA_real_,
      mort_indicator         = NA_character_,
      mort_expected_death_rate = NA_real_,
      mort_precision         = NA_real_,
      mort_nonresponse       = NA_real_,
      mort_design_effect     = NA_real_,
      mort_recall_days       = NA_real_,
      mort_avg_hh_size       = NA_real_,
      mort_fpc               = FALSE,
      mort_result_dummy      = NA_real_,
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

      # Resolve legacy aliases
      if (!is.null(allocation_method) && sampling_method == "srs") {
        sampling_method <- allocation_method
      }
      if (!is.null(design_effect) && is.na(pop_design_effect)) {
        pop_design_effect <- design_effect
      }
      if (!is.null(precision) && is.na(pop_precision)) {
        pop_precision <- precision
      }

      new_row <- data.frame(
        stratum_id               = stratum_id,
        Population_Name          = stratum_name,
        Total_Households         = as.numeric(total_households),
        Total_Population         = as.numeric(population_size),
        Sampling_Method          = sampling_method,
        pop_indicator            = pop_indicator,
        pop_expected_prevalence  = as.numeric(pop_expected_prevalence),
        pop_precision            = as.numeric(pop_precision),
        pop_nonresponse          = as.numeric(pop_nonresponse),
        pop_design_effect        = as.numeric(pop_design_effect),
        pop_fpc                  = as.logical(pop_fpc),
        pop_result_dummy         = as.numeric(pop_result_dummy),
        ind_indicator            = as.character(ind_indicator),
        ind_expected_prevalence  = as.numeric(ind_expected_prevalence),
        ind_precision            = as.numeric(ind_precision),
        ind_nonresponse          = as.numeric(ind_nonresponse),
        ind_design_effect        = as.numeric(ind_design_effect),
        ind_avg_hh_size          = as.numeric(ind_avg_hh_size),
        ind_subpop_prop          = as.numeric(ind_subpop_prop),
        ind_fpc                  = as.logical(ind_fpc),
        ind_result_dummy         = as.numeric(ind_result_dummy),
        mort_indicator           = as.character(mort_indicator),
        mort_expected_death_rate = as.numeric(mort_expected_death_rate),
        mort_precision           = as.numeric(mort_precision),
        mort_nonresponse         = as.numeric(mort_nonresponse),
        mort_design_effect       = as.numeric(mort_design_effect),
        mort_recall_days         = as.numeric(mort_recall_days),
        mort_avg_hh_size         = as.numeric(mort_avg_hh_size),
        mort_fpc                 = as.logical(mort_fpc),
        mort_result_dummy        = as.numeric(mort_result_dummy),
        teams                    = as.numeric(teams),
        avg_interview_time       = as.numeric(avg_interview_time),
        clusters_per_day         = as.numeric(clusters_per_day),
        enumerators_per_team     = as.numeric(enumerators_per_team),
        avg_rest_time            = as.numeric(avg_rest_time),
        avg_travel_time          = as.numeric(avg_travel_time),
        start_time               = as.character(start_time),
        end_time                 = as.character(end_time),
        Final_HH_Sample_Size     = NA_real_,
        stringsAsFactors = FALSE
      )

      if (is.null(self$sample_table)) {
        self$sample_table <- new_row
      } else {
        if (stratum_id %in% self$sample_table$stratum_id) {
          phr_error(
            message = phr_txt("Stratum ID '{stratum_id}' already exists."),
            origin  = "Protocol$add_stratum",
            hint    = phr_txt("Use a unique stratum_id for each call to add_stratum().")
          )
        }
        self$sample_table <- rbind(self$sample_table, new_row)
      }

      self$metadata$modified_date <- Sys.time()
      private$check_issues()
      phr_message(phr_txt("Stratum '{stratum_id}' added."), origin = "Protocol$add_stratum")
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
        phr_validate_dataframe(frame, origin = "Protocol$set_sampling_frame", soft = FALSE)
        phr_assert(
          nrow(frame) > 0,
          message = phr_txt("Sampling frame is empty."),
          origin  = "Protocol$set_sampling_frame",
          hint    = phr_txt("Provide a data frame with at least one PSU row.")
        )

        # 2. Run validate_sampling_frame — stops on hard issues
        val_result <- validate_sampling_frame(frame)
        if (!val_result$valid) {
          hard_issues <- val_result$issues[setdiff(names(val_result$issues), "missing_inclusion")]
          if (length(hard_issues) > 0) {
            phr_error(
              message = phr_txt("Sampling frame validation failed: {paste(names(hard_issues), unlist(hard_issues), sep=': ', collapse='; ')}"),
              origin  = "Protocol$set_sampling_frame",
              hint    = phr_txt("Ensure the frame has a 'psu' column and any 'inclusion' column contains only TRUE/FALSE values.")
            )
          }
        }

        # 3. Add inclusion column (all TRUE) if absent
        if (!"inclusion" %in% names(frame)) {
          frame$inclusion <- TRUE
          phr_message(
            phr_txt("'inclusion' column not found — defaulting all PSUs to TRUE."),
            origin = "Protocol$set_sampling_frame"
          )
        }

        self$sampling_frame <- frame
        self$metadata$modified_date <- Sys.time()
        private$check_issues()
        phr_message(
          phr_txt("Sampling frame set with {nrow(frame)} PSUs."),
          origin = "Protocol$set_sampling_frame"
        )

      }, on_error = "abort", origin = "Protocol$set_sampling_frame")
      invisible(self)
    },

    #' @description Draw sample from the sampling frame
    #'
    #' Applies one of five PSU-level sampling methods to the eligible PSUs in
    #' the sampling frame (those with \code{inclusion == TRUE}).  Results are
    #' stored in \code{drawn_sample_full} (the full frame annotated with
    #' \code{sampled_psu} and \code{allocated_sample} columns) and
    #' \code{drawn_sample} (only the selected rows).
    #'
    #' @param method Character. One of \code{"srs"}, \code{"proportional"},
    #'   \code{"pps_cluster"}, \code{"rlc"}, \code{"systematic"}.
    #'   Default \code{"srs"}.
    #' @param sample_size Integer. Total household sample size to allocate
    #'   across selected PSUs.  Required.
    #' @param n_psu Integer. Number of PSUs to select.  Required for
    #'   \code{"srs"} method.
    #' @param n_clusters Integer. Number of clusters to allocate.  Required
    #'   for \code{"pps_cluster"} method.
    #' @param n_sites Integer. Number of sites to select.  Required for
    #'   \code{"systematic"} method.
    #' @param cluster_size Integer. Households per cluster.  Required for
    #'   \code{"pps_cluster"}; defaults to \code{3} for \code{"rlc"}.
    #' @param seed Integer. Random seed for reproducibility (default \code{42}).
    #' @param stratified Logical. If \code{TRUE}, apply sampling independently
    #'   within each stratum of the sampling frame.  When \code{TRUE}, per-
    #'   stratum sample sizes are taken from \code{sample_table$Final_HH_Sample_Size}
    #'   if available; otherwise \code{sample_size} is divided proportionally
    #'   across strata.  Note: when per-stratum sizes come from \code{sample_table},
    #'   the total allocated across all strata may differ from \code{sample_size}.
    #'   Default \code{FALSE}.
    #' @return Invisibly returns \code{self} for method chaining.
    draw_sample = function(method = "srs",
                           sample_size,
                           n_psu = NULL,
                           n_clusters = NULL,
                           n_sites = NULL,
                           cluster_size = NULL,
                           seed = 42,
                           stratified = FALSE) {
      phr_try({

        phr_assert(
          !is.null(self$sampling_frame),
          message = phr_txt("Must set sampling frame before drawing sample."),
          origin  = "Protocol$draw_sample",
          hint    = phr_txt("Call set_sampling_frame() first.")
        )
        phr_assert(
          !missing(sample_size),
          message = phr_txt("sample_size (total households) is a required argument."),
          origin  = "Protocol$draw_sample"
        )

        frame <- self$sampling_frame

        # Eligible PSUs
        if ("inclusion" %in% names(frame)) {
          eligible_rows <- which(!is.na(frame$inclusion) & frame$inclusion)
        } else {
          eligible_rows <- seq_len(nrow(frame))
        }
        eligible_frame <- frame[eligible_rows, , drop = FALSE]

        # Initialise output columns on full frame
        frame$sampled_psu      <- NA_integer_
        frame$allocated_sample <- NA_real_

        if (stratified && "stratum" %in% names(eligible_frame)) {
          strata <- unique(eligible_frame$stratum)
          cluster_offset <- 0L

          for (st in strata) {
            st_eligible_rows <- which(eligible_frame$stratum == st)
            st_frame <- eligible_frame[st_eligible_rows, , drop = FALSE]

            st_sample_size <- private$stratum_sample_size(st, sample_size,
                                                           nrow(st_frame),
                                                           nrow(eligible_frame))

            st_result <- private$apply_sampling_method(
              frame        = st_frame,
              method       = method,
              sample_size  = st_sample_size,
              n_psu        = n_psu,
              n_clusters   = n_clusters,
              n_sites      = n_sites,
              cluster_size = cluster_size,
              seed         = seed
            )

            sel_mask <- !is.na(st_result$sampled_psu)
            if (any(sel_mask)) {
              st_result$sampled_psu[sel_mask] <-
                st_result$sampled_psu[sel_mask] + cluster_offset
              cluster_offset <- cluster_offset +
                max(st_result$sampled_psu[sel_mask], na.rm = TRUE)
            }

            full_frame_rows <- eligible_rows[st_eligible_rows]
            frame$sampled_psu[full_frame_rows]      <- st_result$sampled_psu
            frame$allocated_sample[full_frame_rows] <- st_result$allocated_sample
          }
        } else {
          result <- private$apply_sampling_method(
            frame        = eligible_frame,
            method       = method,
            sample_size  = sample_size,
            n_psu        = n_psu,
            n_clusters   = n_clusters,
            n_sites      = n_sites,
            cluster_size = cluster_size,
            seed         = seed
          )
          frame$sampled_psu[eligible_rows]      <- result$sampled_psu
          frame$allocated_sample[eligible_rows] <- result$allocated_sample
        }

        self$drawn_sample_full <- frame
        self$drawn_sample      <- frame[!is.na(frame$sampled_psu), , drop = FALSE]

        self$metadata$modified_date <- Sys.time()
        private$check_issues()
        phr_message(
          phr_txt("Sample drawn using method '{method}': {nrow(self$drawn_sample)} PSU(s) selected."),
          origin = "Protocol$draw_sample"
        )

      }, on_error = "abort", origin = "Protocol$draw_sample")
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
    
    #' @description Validate the structure of the master sample table
    #'
    #' Checks that \code{sample_table} exists and contains all required master
    #' table columns.  Returns a list with a \code{valid} flag and a
    #' \code{message}.
    #'
    #' @return Named list with elements \code{valid} (logical) and
    #'   \code{message} (character).
    validate_strata_table = function() {
      if (is.null(self$sample_table)) {
        return(list(valid = FALSE, message = "sample_table is NULL — no strata have been added yet."))
      }

      required_cols <- .strata_table_required_cols

      missing_cols <- setdiff(required_cols, names(self$sample_table))
      if (length(missing_cols) > 0) {
        return(list(
          valid   = FALSE,
          message = paste("sample_table is missing required columns:",
                          paste(missing_cols, collapse = ", "))
        ))
      }

      dupes <- self$sample_table$stratum_id[duplicated(self$sample_table$stratum_id)]
      if (length(dupes) > 0) {
        return(list(
          valid   = FALSE,
          message = paste("Duplicate stratum_id values:", paste(dupes, collapse = ", "))
        ))
      }

      list(valid = TRUE, message = "sample_table structure is valid.")
    },

    #' @description Get the sample table
    #' @return Data frame containing the sample table
    get_sample_table = function() {
      return(self$sample_table)
    },
    
    #' @description Get all issues
    #' @return List of validation issues
    get_issues = function() {
      return(self$issues)
    },
    
    #' @description Get protocol summary
    #' @return List with protocol summary information
    get_protocol_summary = function() {
      summary <- list(
        assessment_title = self$metadata$assessment_title,
        country_name = self$metadata$country_name,
        month_year = self$metadata$month_year,
        created = self$metadata$created_date,
        modified = self$metadata$modified_date,
        num_objectives = count_objectives(self$objectives),
        num_strata = if (is.null(self$sample_table)) 0 else nrow(self$sample_table),
        total_sample_size = if (is.null(self$sample_table)) 0 else sum(self$sample_table$pop_result_dummy, na.rm = TRUE),
        num_tools = length(self$tools),
        num_issues = length(self$issues)
      )
      return(summary)
    },
    
    #' @description Export protocol to a list
    #' @return List containing all protocol data
    export_protocol = function() {
      list(
        metadata = self$metadata,
        objectives = self$objectives,
        objective_schema = self$objective_schema,
        sample_table = self$sample_table,
        sampling_frame = self$sampling_frame,
        drawn_sample = self$drawn_sample,
        drawn_sample_full = self$drawn_sample_full,
        tools = self$tools,
        selected_indicators = self$selected_indicators,
        issues = self$issues,
        summary = self$get_protocol_summary()
      )
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
      
      # Check strata consistency between frame and sample table
      if (!is.null(self$sample_table) && !is.null(self$sampling_frame)) {
        table_strata <- self$sample_table$stratum_id
        frame_strata <- unique(self$sampling_frame$stratum)
        
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

    # Apply a PSU-level sampling method — dispatches to sample_psu_* utilities
    apply_sampling_method = function(frame, method, sample_size, n_psu, n_clusters,
                                     n_sites, cluster_size, seed) {
      origin <- "Protocol$apply_sampling_method"
      valid_methods <- c("srs", "proportional", "pps_cluster", "rlc", "systematic")
      phr_assert(
        method %in% valid_methods,
        message = phr_txt("Unknown sampling method '{method}' — must be one of: {paste(valid_methods, collapse=', ')}."),
        origin  = origin
      )

      if (method == "srs") {
        phr_assert(!is.null(n_psu),
                   message = phr_txt("n_psu is required for the 'srs' method."),
                   origin = origin)
        draw_sample_psu_srs(frame, n_psu, sample_size, seed)
      } else if (method == "proportional") {
        draw_sample_psu_proportional(frame, sample_size, seed)
      } else if (method == "pps_cluster") {
        phr_assert(!is.null(n_clusters),
                   message = phr_txt("n_clusters is required for the 'pps_cluster' method."),
                   origin = origin)
        phr_assert(!is.null(cluster_size),
                   message = phr_txt("cluster_size is required for the 'pps_cluster' method."),
                   origin = origin)
        draw_sample_psu_pps_cluster(frame, n_clusters, cluster_size, seed)
      } else if (method == "rlc") {
        cs <- if (!is.null(cluster_size)) cluster_size else 3L
        draw_sample_psu_rlc(frame, sample_size, cs, seed)
      } else {  # systematic
        phr_assert(!is.null(n_sites),
                   message = phr_txt("n_sites is required for the 'systematic' method."),
                   origin = origin)
        draw_sample_psu_systematic(frame, n_sites, sample_size, seed)
      }
    },

    # Determine the sample size for one stratum when stratified = TRUE.
    # Looks up Final_HH_Sample_Size from sample_table first; falls back to
    # proportional division of the total sample_size.
    stratum_sample_size = function(stratum_id, total_sample_size,
                                   stratum_n_eligible, total_n_eligible) {
      if (!is.null(self$sample_table) &&
          "Final_HH_Sample_Size" %in% names(self$sample_table)) {
        st_row <- self$sample_table[self$sample_table$stratum_id == stratum_id, ]
        if (nrow(st_row) > 0 && !is.na(st_row$Final_HH_Sample_Size[1])) {
          return(as.integer(st_row$Final_HH_Sample_Size[1]))
        }
      }
      # Proportional fallback
      round(total_sample_size * stratum_n_eligible / max(total_n_eligible, 1L))
    }
  )
)
