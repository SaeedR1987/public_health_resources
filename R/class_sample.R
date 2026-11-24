#' Sample R6 Class
#'
#' @description
#' Holds the protocol strata/sample table and related workflows such as adding
#' strata rows and calculating sample sizes.
#'
#' @importFrom R6 R6Class
#' @export
Sample <- R6::R6Class(
  "Sample",
  inherit = Asset,
  public = list(
    #' @field sample_table Data frame with one row per stratum.
    sample_table = NULL,

    #' @field validated Logical indicating whether the sample table has been validated.
    validated = NULL,

    #' @description Create a new Sample object.
    #' @param sample_table Optional sample table data frame.
    initialize = function(sample_table = NULL) {
      origin <- "Sample$initialize"
      super$initialize()
      private$..touch(log = TRUE, touch = FALSE, name = origin, message = "Starting initialization of Sample.")

      result <- private$..phr_try_step(
        {
          self$validated <- FALSE
          if (is.null(sample_table)) {
            self$sample_table <- NULL
          } else {
            phrutils::phr_validate_dataframe(
              sample_table,
              origin = origin,
              soft = FALSE
            )
            self$sample_table <- as.data.frame(
              sample_table,
              stringsAsFactors = FALSE
            )
          }
        },
        step = "Initializing sample table",
        hint = "Check that sample_table is a data frame."
      )
      if (phrutils::phr_failed(result)) {
        private$..touch(log = TRUE, touch = FALSE, name = origin, display = TRUE, message = result$error, level = "error")
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = origin,
        message = paste0("Sample initialized with ", nrow(self$sample_table %||% data.frame()), " stratum row(s).")
      )
      invisible(self)
    },

    #' @description Replace the current sample table.
    #' @param sample_table Data frame.
    set_sample_table = function(sample_table) {
      origin <- "Sample$set_sample_table"
      private$..touch(log = TRUE, touch = FALSE, name = origin, message = "Starting set_sample_table.")

      result <- private$..phr_try_step(
        {
          phrutils::phr_validate_dataframe(
            sample_table,
            origin = origin,
            soft = FALSE
          )
          self$sample_table <- as.data.frame(sample_table, stringsAsFactors = FALSE)
        },
        step = "Setting sample table",
        hint = "Check that sample_table is a data frame."
      )
      if (phrutils::phr_failed(result)) {
        private$..touch(log = TRUE, touch = FALSE, name = origin, display = TRUE, message = result$error, level = "error")
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }

      private$..touch(
        log = TRUE,
        touch = TRUE,
        name = origin,
        message = paste0("Sample table replaced with ", nrow(self$sample_table), " row(s).")
      )
      invisible(self)
    },

    #' @description Return the current sample table data frame.
    get_sample_table = function() {
      out <- self$sample_table
      out
    },

    #' @description Clear sample table data.
    clear_sample_table = function() {
      origin <- "Sample$clear_sample_table"
      private$..touch(log = TRUE, touch = FALSE, name = origin, message = "Starting clear_sample_table.")
      n_removed <- nrow(self$sample_table %||% data.frame())
      self$sample_table <- NULL
      private$..touch(
        log = TRUE,
        touch = TRUE,
        name = origin,
        message = paste0("Sample table cleared; ", n_removed, " row(s) removed.")
      )
      invisible(self)
    },

    #' @description Add a stratum row to the sample table.
    #' @param stratum_id Character or numeric identifier for the stratum.
    #' @param stratum_name Character name for the stratum.
    #' @param population_size Numeric population size (default: NA).
    #' @param total_households Numeric total number of households (default: NA).
    #' @param sampling_method_site Character site sampling method (required).
    #' @param sampling_method_hh Character household sampling method.
    #' @param n_psu Numeric number of primary sampling units (default: NA).
    #' @param cluster_size Numeric cluster size (default: NA).
    #' @param n_sites Numeric number of sites (default: NA).
    #' @param pop_indicator Character population indicator (default: "General").
    #' @param pop_expected_prevalence Numeric expected prevalence for population (default: NA).
    #' @param pop_precision Numeric precision for population (default: NA).
    #' @param pop_nonresponse Numeric nonresponse rate for population (default: NA).
    #' @param pop_design_effect Numeric design effect for population (default: NA).
    #' @param pop_fpc Logical finite population correction for population (default: FALSE).
    #' @param General_HH_Sample_Size Numeric general household sample size (default: NA).
    #' @param ind_indicator Character indicator name for individual survey (default: NA).
    #' @param ind_expected_prevalence Numeric expected prevalence for individual survey (default: NA).
    #' @param ind_precision Numeric precision for individual survey (default: NA).
    #' @param ind_nonresponse Numeric nonresponse rate for individual survey (default: NA).
    #' @param ind_design_effect Numeric design effect for individual survey (default: NA).
    #' @param ind_avg_hh_size Numeric average household size for individual survey (default: NA).
    #' @param ind_subpop_prop Numeric subpopulation proportion for individual survey (default: NA).
    #' @param ind_fpc Logical finite population correction for individual survey (default: FALSE).
    #' @param Ind_Sample_Size Numeric individual sample size (default: NA).
    #' @param Ind_HH_Sample_Size Numeric individual household sample size (default: NA).
    #' @param rate_indicator Character indicator name for rate survey (default: NA).
    #' @param rate_expected_rate Numeric expected rate for rate survey (default: NA).
    #' @param rate_precision Numeric precision for rate survey (default: NA).
    #' @param rate_nonresponse Numeric nonresponse rate for rate survey (default: NA).
    #' @param rate_design_effect Numeric design effect for rate survey (default: NA).
    #' @param rate_recall_days Numeric recall period in days for rate survey (default: NA).
    #' @param rate_avg_hh_size Numeric average household size for rate survey (default: NA).
    #' @param rate_fpc Logical finite population correction for rate survey (default: FALSE).
    #' @param Rate_Ind_Sample_Size Numeric rate individual sample size (default: NA).
    #' @param Rate_PT_Sample_Size Numeric rate person-time sample size (default: NA).
    #' @param Rate_HH_Sample_Size Numeric rate household sample size (default: NA).
    #' @param teams Numeric number of data collection teams (default: NA).
    #' @param avg_interview_time Numeric average interview time (default: NA).
    #' @param clusters_per_day Numeric clusters per day (default: NA).
    #' @param enumerators_per_team Numeric enumerators per team (default: NA).
    #' @param avg_rest_time Numeric average rest time (default: NA).
    #' @param avg_travel_time Numeric average travel time (default: NA).
    #' @param start_time Character start time (default: NA).
    #' @param end_time Character end time (default: NA).
    #' @param design_effect Numeric design effect (legacy parameter, default: NULL).
    #' @param precision Numeric precision (legacy parameter, default: NULL).
    #' @param confidence_level Numeric confidence level (legacy parameter, default: NULL).
    #' @return Invisibly returns \code{self} for method chaining.
    add_stratum = function(
      stratum_id,
      stratum_name,
      population_size = NA_real_,
      total_households = NA_real_,
      sampling_method_site = NULL,
      sampling_method_hh = NULL,
      n_psu = NA_real_,
      cluster_size = NA_real_,
      n_sites = NA_real_,
      pop_indicator = "General",
      pop_expected_prevalence = NA_real_,
      pop_precision = NA_real_,
      pop_nonresponse = NA_real_,
      pop_design_effect = NA_real_,
      pop_fpc = FALSE,
      General_HH_Sample_Size = NA_real_,
      ind_indicator = NA_character_,
      ind_expected_prevalence = NA_real_,
      ind_precision = NA_real_,
      ind_nonresponse = NA_real_,
      ind_design_effect = NA_real_,
      ind_avg_hh_size = NA_real_,
      ind_subpop_prop = NA_real_,
      ind_fpc = FALSE,
      Ind_Sample_Size = NA_real_,
      Ind_HH_Sample_Size = NA_real_,
      rate_indicator = NA_character_,
      rate_expected_rate = NA_real_,
      rate_precision = NA_real_,
      rate_nonresponse = NA_real_,
      rate_design_effect = NA_real_,
      rate_recall_days = NA_real_,
      rate_avg_hh_size = NA_real_,
      rate_fpc = FALSE,
      Rate_Ind_Sample_Size = NA_real_,
      Rate_PT_Sample_Size = NA_real_,
      Rate_HH_Sample_Size = NA_real_,
      teams = NA_real_,
      avg_interview_time = NA_real_,
      clusters_per_day = NA_real_,
      enumerators_per_team = NA_real_,
      avg_rest_time = NA_real_,
      avg_travel_time = NA_real_,
      start_time = NA_character_,
      end_time = NA_character_,
      design_effect = NULL,
      precision = NULL,
      confidence_level = NULL
    ) {
      origin <- "Sample$add_stratum"
      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = origin,
        message = paste0("Starting add_stratum for stratum '", stratum_id, "'.")
      )
      replaced <- FALSE

      result <- private$..phr_try_step(
        {
      if (!is.null(design_effect) && is.na(pop_design_effect)) {
        pop_design_effect <- design_effect
      }
      if (!is.null(precision) && is.na(pop_precision)) {
        pop_precision <- precision
      }

      phrutils::phr_assert(
        !is.null(sampling_method_site),
        message = private$..phr_txt("sampling_method_site is required."),
        origin = "Sample$add_stratum"
      )

      site_selection_methods <- c(
        "simple_random_even",
        "simple_random_proportional",
        "proportional",
        "cluster",
        "systematic_even",
        "systematic_proportional",
        "purposive"
      )
      phrutils::phr_assert(
        sampling_method_site %in% site_selection_methods,
        message = private$..phr_txt(
          "sampling_method_site must be one of: {paste(site_selection_methods, collapse=', ')}."
        ),
        origin = "Sample$add_stratum"
      )

      # if (sampling_method_site %in% site_selection_methods) {
      #   phrutils::phr_assert(
      #     !is.null(n_sites) && !is.na(n_sites),
      #     message = private$..phr_txt(
      #       "n_sites is required for sampling_method '{sampling_method_site}'."
      #     ),
      #     origin = "Sample$add_stratum"
      #   )
      # }

      # Default household sampling method if NULL or missing
      if (is.null(sampling_method_hh) || is.na(sampling_method_hh)) {
        sampling_method_hh <- "simple_random"
      }

      if (
        sampling_method_hh %in%
          "rlc" &&
          (is.null(cluster_size) || is.na(cluster_size))
      ) {
        cluster_size <- 3L
      }

      hh_selection_methods <- c("simple_random", "systematic", "rlc")
      hh_method <- if (sampling_method_hh %in% hh_selection_methods) {
        sampling_method_hh
      } else {
        "simple_random"
      }
      # Warn early if stratum_id already exists and will be overwritten
      if (
        !is.null(self$sample_table) &&
          stratum_id %in% self$sample_table$stratum_id
      ) {
        phrutils::phr_warning(
          message = private$..phr_txt(
            "Stratum ID '{stratum_id}' already exists and will be overwritten."
          ),
          origin = "Sample$add_stratum",
          hint = private$..phr_txt(
            "The existing row for '{stratum_id}' will be replaced with new values."
          )
        )
      }

      new_row <- data.frame(
        stratum_id = stratum_id,
        stratum_name = stratum_name,
        total_households = as.numeric(total_households),
        total_population = as.numeric(population_size),
        sampling_method_site = sampling_method_site,
        sampling_method_hh = hh_method,
        n_psu = as.numeric(n_psu),
        cluster_size = as.numeric(cluster_size),
        n_sites = as.numeric(n_sites),
        pop_indicator = pop_indicator,
        pop_expected_prevalence = as.numeric(pop_expected_prevalence),
        pop_precision = as.numeric(pop_precision),
        pop_nonresponse = as.numeric(pop_nonresponse),
        pop_design_effect = as.numeric(pop_design_effect),
        pop_fpc = as.logical(pop_fpc),
        ind_indicator = as.character(ind_indicator),
        ind_expected_prevalence = as.numeric(ind_expected_prevalence),
        ind_precision = as.numeric(ind_precision),
        ind_nonresponse = as.numeric(ind_nonresponse),
        ind_design_effect = as.numeric(ind_design_effect),
        ind_avg_hh_size = as.numeric(ind_avg_hh_size),
        ind_subpop_prop = as.numeric(ind_subpop_prop),
        ind_fpc = as.logical(ind_fpc),
        rate_indicator = as.character(rate_indicator),
        rate_expected_rate = as.numeric(rate_expected_rate),
        rate_precision = as.numeric(rate_precision),
        rate_nonresponse = as.numeric(rate_nonresponse),
        rate_design_effect = as.numeric(rate_design_effect),
        rate_recall_days = as.numeric(rate_recall_days),
        rate_avg_hh_size = as.numeric(rate_avg_hh_size),
        rate_fpc = as.logical(rate_fpc),
        teams = as.numeric(teams),
        avg_interview_time = as.numeric(avg_interview_time),
        clusters_per_day = as.numeric(clusters_per_day),
        enumerators_per_team = as.numeric(enumerators_per_team),
        avg_rest_time = as.numeric(avg_rest_time),
        avg_travel_time = as.numeric(avg_travel_time),
        start_time = as.character(start_time),
        end_time = as.character(end_time),
        General_HH_Sample_Size = as.numeric(General_HH_Sample_Size),
        Ind_Sample_Size = as.numeric(Ind_Sample_Size),
        Ind_HH_Sample_Size = as.numeric(Ind_HH_Sample_Size),
        Rate_Ind_Sample_Size = as.numeric(Rate_Ind_Sample_Size),
        Rate_PT_Sample_Size = as.numeric(Rate_PT_Sample_Size),
        Rate_HH_Sample_Size = as.numeric(Rate_HH_Sample_Size),
        Final_HH_Sample_Size = NA_real_,
        num_interview_per_enum_per_day = NA_real_,
        num_days = NA_real_,
        stringsAsFactors = FALSE
      )

      if (is.null(self$sample_table)) {
        self$sample_table <- new_row
      } else {
        if (stratum_id %in% self$sample_table$stratum_id) {
          phrutils::phr_warning(
            message = private$..phr_txt(
              "Stratum ID '{stratum_id}' already exists and will be overwritten."
            ),
            origin = "Sample$add_stratum",
            hint = private$..phr_txt(
              "The existing row for '{stratum_id}' has been replaced with the new values."
            )
          )
          self$sample_table <- self$sample_table[
            self$sample_table$stratum_id != stratum_id,
            ,
            drop = FALSE
          ]
          replaced <- TRUE
        }
        self$sample_table <- rbind(self$sample_table, new_row)
      }
        },
        step = "Adding stratum to sample table",
        hint = "Check sampling_method_site and the stratum parameters."
      )
      if (phrutils::phr_failed(result)) {
        private$..touch(log = TRUE, touch = FALSE, name = origin, display = TRUE, message = result$error, level = "error")
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }

      private$..touch(
        log = TRUE,
        touch = TRUE,
        name = origin,
        message = paste0(
          "Stratum '", stratum_id, "' ", if (replaced) "replaced" else "added",
          "; sample table now has ", nrow(self$sample_table), " row(s)."
        )
      )
      invisible(self)
    },

    #' @description Remove a stratum row by strata name.
    #' @param strata_name Character scalar naming the stratum to remove.
    remove_stratum = function(strata_name) {
      origin <- "Sample$remove_stratum"
      private$..touch(log = TRUE, touch = FALSE, name = origin, message = "Starting remove_stratum.")
      n_before <- nrow(self$sample_table %||% data.frame())

      result <- private$..phr_try_step(
        {
          phrutils::phr_assert(
            is.character(strata_name) &&
              length(strata_name) == 1L &&
              nzchar(strata_name),
            message = private$..phr_txt(
              "strata_name must be a single non-empty character value."
            ),
            origin = origin
          )

          st <- self$sample_table
          if (!is.null(st) && is.data.frame(st) && nrow(st) > 0L) {
            stratum_col <- private$..resolve_stratum_name_col(st)
            phrutils::phr_assert(
              !is.null(stratum_col),
              message = private$..phr_txt(
                "sample_table does not contain a stratum name column."
              ),
              origin = origin
            )

            keep_rows <- as.character(st[[stratum_col]]) != strata_name
            self$sample_table <- st[keep_rows, , drop = FALSE]
          }
        },
        step = "Removing stratum",
        hint = "Check strata_name and the sample table columns."
      )
      if (phrutils::phr_failed(result)) {
        private$..touch(log = TRUE, touch = FALSE, name = origin, display = TRUE, message = result$error, level = "error")
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }

      n_removed <- n_before - nrow(self$sample_table %||% data.frame())
      private$..touch(
        log = TRUE,
        touch = n_removed > 0L,
        name = origin,
        message = paste0("Removed ", n_removed, " row(s) for stratum '", strata_name, "'.")
      )
      invisible(self)
    },

    #' @description Validate strata table structure.
    #' @return Logical.
    validate_strata_table = function() {
      origin <- "Sample$validate_strata_table"
      private$..touch(log = TRUE, touch = FALSE, name = origin, message = "Starting validate_strata_table.")

      out <- NULL
      result <- private$..phr_try_step(
        {
          out <- validate_strata_table(self$sample_table)
          self$validated <- out
        },
        step = "Validating strata table",
        hint = "Check the sample table structure."
      )
      if (phrutils::phr_failed(result)) {
        private$..touch(log = TRUE, touch = FALSE, name = origin, display = TRUE, message = result$error, level = "error")
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }

      private$..touch(
        log = TRUE,
        touch = FALSE,
        name = origin,
        message = paste0("Strata table validation completed (valid: ", isTRUE(out), ").")
      )
      out
    },

    #' @description Calculate sample sizes for all strata rows.
    calculate_sample_sizes = function() {
      origin <- "Sample$calculate_sample_sizes"
      private$..touch(log = TRUE, touch = FALSE, name = origin, message = "Starting calculate_sample_sizes.")

      result <- private$..phr_try_step(
        {
          phrutils::phr_assert(
            !is.null(self$sample_table) && nrow(self$sample_table) > 0,
            message = private$..phr_txt("sample_table is empty. Call add_stratum() first."),
            origin = origin
          )
          self$sample_table <- calculate_sample_size_strata_table(self$sample_table)
        },
        step = "Calculating sample sizes",
        hint = "Call add_stratum() and supply the sampling parameters first."
      )
      if (phrutils::phr_failed(result)) {
        private$..touch(log = TRUE, touch = FALSE, name = origin, display = TRUE, message = result$error, level = "error")
        phrutils::phr_error(message = result$error, origin = origin, hint = result$hint)
      }

      private$..touch(
        log = TRUE,
        touch = TRUE,
        name = origin,
        message = paste0("Calculated sample sizes for ", nrow(self$sample_table), " stratum row(s).")
      )
      invisible(self)
    },

    #' @description Return unique sampling methods in the sample table.
    #' @param type Character specifying method type: "site" or "household" (default: "site").
    #' @return Character vector of unique sampling methods.
    get_sampling_methods = function(type = c("site", "household")) {
      type <- match.arg(type)
      col_name <- switch(
        type,
        site = "sampling_method_site",
        household = "sampling_method_hh"
      )
      st <- self$sample_table
      if (is.null(st) || !(col_name %in% names(st))) {
        return(character(0))
      }
      m <- as.character(st[[col_name]])
      out <- unique(m[!is.na(m) & nzchar(m)])
      out
    },

    #' @description Return stratum names.
    #' @return Character vector of stratum names.
    get_strata_names = function() {
      st <- self$sample_table
      if (is.null(st) || nrow(st) == 0) {
        return(character(0))
      }
      col <- private$..resolve_stratum_name_col(st)
      if (is.null(col)) {
        return(character(0))
      }
      n <- as.character(st[[col]])
      out <- n[!is.na(n) & nzchar(n)]
      out
    }
  ),
  private = list(
    # @description Resolve the stratum name column from a sample table.
    # @param st Data frame sample table.
    # @return Character scalar naming the column, or NULL if not found.
    # @keywords internal
    ..resolve_stratum_name_col = function(st) {
      if ("stratum_name" %in% names(st)) {
        "stratum_name"
      } else if ("Population_Name" %in% names(st)) {
        "Population_Name"
      } else if ("stratum_id" %in% names(st)) {
        "stratum_id"
      } else {
        NULL
      }
    }
  )
)
