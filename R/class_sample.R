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
  public = list(
    #' @field sample_table Data frame with one row per stratum.
    sample_table = NULL,

    #' @description Create a new Sample object.
    #' @param sample_table Optional sample table data frame.
    initialize = function(sample_table = NULL) {
      if (is.null(sample_table)) {
        self$sample_table <- NULL
      } else {
        phr_validate_dataframe(sample_table, origin = "Sample$initialize", soft = FALSE)
        self$sample_table <- as.data.frame(sample_table, stringsAsFactors = FALSE)
      }
      invisible(self)
    },

    #' @description Replace the current sample table.
    #' @param sample_table Data frame.
    set_sample_table = function(sample_table) {
      phr_validate_dataframe(sample_table, origin = "Sample$set_sample_table", soft = FALSE)
      self$sample_table <- as.data.frame(sample_table, stringsAsFactors = FALSE)
      invisible(self)
    },

    #' @description Return the current sample table data frame.
    get_sample_table = function() {
      self$sample_table
    },

    #' @description Clear sample table data.
    clear_sample_table = function() {
      self$sample_table <- NULL
      invisible(self)
    },

    #' @description Add a stratum row to the sample table.
    add_stratum = function(
      stratum_id,
      stratum_name,
      population_size        = NA_real_,
      total_households       = NA_real_,
      sampling_method        = NULL,
      allocation_method      = NULL,
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
      design_effect          = NULL,
      precision              = NULL,
      confidence_level       = NULL
    ) {

      if (is.null(sampling_method) && !is.null(allocation_method)) {
        sampling_method <- allocation_method
      }
      if (!is.null(design_effect) && is.na(pop_design_effect)) {
        pop_design_effect <- design_effect
      }
      if (!is.null(precision) && is.na(pop_precision)) {
        pop_precision <- precision
      }

      phr_assert(
        !is.null(sampling_method),
        message = phr_txt("sampling_method is required."),
        origin  = "Sample$add_stratum"
      )

      valid_sampling_methods <- c("simple_random", "proportional", "pps_cluster",
                                  "pps_rlc", "systematic", "simple_random_rlc",
                                  "systematic_rlc", "proportional_rlc", "purposive")
      phr_assert(
        sampling_method %in% valid_sampling_methods,
        message = phr_txt("sampling_method must be one of: {paste(valid_sampling_methods, collapse=', ')}."),
        origin  = "Sample$add_stratum"
      )

      site_selection_methods <- c("simple_random", "pps_rlc",
                                  "systematic", "simple_random_rlc", "systematic_rlc")
      if (sampling_method %in% site_selection_methods) {
        phr_assert(
          !is.null(n_sites) && !is.na(n_sites),
          message = phr_txt("n_sites is required for sampling_method '{sampling_method}'."),
          origin  = "Sample$add_stratum"
        )
      }

      rlc_methods <- c("pps_rlc", "simple_random_rlc", "systematic_rlc", "proportional_rlc")
      if (sampling_method %in% rlc_methods && (is.null(cluster_size) || is.na(cluster_size))) {
        cluster_size <- 3L
      }

      calc_method <- if (sampling_method %in% c("pps_cluster", "pps_rlc",
                                                "simple_random_rlc", "systematic_rlc",
                                                "proportional_rlc")) "cluster" else "simple_random"

      new_row <- data.frame(
        stratum_id               = stratum_id,
        stratum_name             = stratum_name,
        total_households         = as.numeric(total_households),
        total_population         = as.numeric(population_size),
        sampling_method          = sampling_method,
        calc_method              = calc_method,
        n_psu                    = as.numeric(n_psu),
        cluster_size             = as.numeric(cluster_size),
        n_sites                  = as.numeric(n_sites),
        pop_indicator            = pop_indicator,
        pop_expected_prevalence  = as.numeric(pop_expected_prevalence),
        pop_precision            = as.numeric(pop_precision),
        pop_nonresponse          = as.numeric(pop_nonresponse),
        pop_design_effect        = as.numeric(pop_design_effect),
        pop_fpc                  = as.logical(pop_fpc),
        ind_indicator            = as.character(ind_indicator),
        ind_expected_prevalence  = as.numeric(ind_expected_prevalence),
        ind_precision            = as.numeric(ind_precision),
        ind_nonresponse          = as.numeric(ind_nonresponse),
        ind_design_effect        = as.numeric(ind_design_effect),
        ind_avg_hh_size          = as.numeric(ind_avg_hh_size),
        ind_subpop_prop          = as.numeric(ind_subpop_prop),
        ind_fpc                  = as.logical(ind_fpc),
        mort_indicator           = as.character(mort_indicator),
        mort_expected_death_rate = as.numeric(mort_expected_death_rate),
        mort_precision           = as.numeric(mort_precision),
        mort_nonresponse         = as.numeric(mort_nonresponse),
        mort_design_effect       = as.numeric(mort_design_effect),
        mort_recall_days         = as.numeric(mort_recall_days),
        mort_avg_hh_size         = as.numeric(mort_avg_hh_size),
        mort_fpc                 = as.logical(mort_fpc),
        teams                    = as.numeric(teams),
        avg_interview_time       = as.numeric(avg_interview_time),
        clusters_per_day         = as.numeric(clusters_per_day),
        enumerators_per_team     = as.numeric(enumerators_per_team),
        avg_rest_time            = as.numeric(avg_rest_time),
        avg_travel_time          = as.numeric(avg_travel_time),
        start_time               = as.character(start_time),
        end_time                 = as.character(end_time),
        General_HH_Sample_Size         = as.numeric(General_HH_Sample_Size),
        Ind_Sample_Size                = as.numeric(Ind_Sample_Size),
        Ind_HH_Sample_Size             = as.numeric(Ind_HH_Sample_Size),
        Mort_Ind_Sample_Size           = as.numeric(Mort_Ind_Sample_Size),
        Mort_PT_Sample_Size            = as.numeric(Mort_PT_Sample_Size),
        Mort_HH_Sample_Size            = as.numeric(Mort_HH_Sample_Size),
        Final_HH_Sample_Size           = NA_real_,
        num_interview_per_enum_per_day = NA_real_,
        num_days                       = NA_real_,
        stringsAsFactors = FALSE
      )

      if (is.null(self$sample_table)) {
        self$sample_table <- new_row
      } else {
        if (stratum_id %in% self$sample_table$stratum_id) {
          phr_warning(
            message = phr_txt("Stratum ID '{stratum_id}' already exists and will be overwritten."),
            origin  = "Sample$add_stratum",
            hint    = phr_txt("The existing row for '{stratum_id}' has been replaced with the new values.")
          )
          self$sample_table <- self$sample_table[
            self$sample_table$stratum_id != stratum_id, , drop = FALSE
          ]
        }
        self$sample_table <- rbind(self$sample_table, new_row)
      }

      invisible(self)
    },

    #' @description Validate strata table structure.
    #' @return Logical.
    validate_strata_table = function() {
      validate_strata_table(self$sample_table)
    },

    #' @description Calculate sample sizes for all strata rows.
    calculate_sample_sizes = function() {
      phr_assert(
        !is.null(self$sample_table) && nrow(self$sample_table) > 0,
        message = phr_txt("sample_table is empty. Call add_stratum() first."),
        origin  = "Sample$calculate_sample_sizes"
      )
      self$sample_table <- calculate_sample_size_strata_table(self$sample_table)
      invisible(self)
    },

    #' @description Return unique sampling methods in the sample table.
    get_sampling_methods = function() {
      st <- self$sample_table
      if (is.null(st) || !"sampling_method" %in% names(st)) return(character(0))
      m <- as.character(st$sampling_method)
      unique(m[!is.na(m) & nzchar(m)])
    },

    #' @description Return stratum names.
    get_strata_names = function() {
      st <- self$sample_table
      if (is.null(st) || nrow(st) == 0) return(character(0))
      col <- if ("stratum_name" %in% names(st)) {
        "stratum_name"
      } else if ("Population_Name" %in% names(st)) {
        "Population_Name"
      } else if ("stratum_id" %in% names(st)) {
        "stratum_id"
      } else {
        return(character(0))
      }
      n <- as.character(st[[col]])
      n[!is.na(n) & nzchar(n)]
    },

    #' @description Return compact sample-size summary.
    get_sample_size_summary = function() {
      st <- self$sample_table
      if (is.null(st) || nrow(st) == 0) {
        return(data.frame(
          stratum = character(0),
          sampling_method = character(0),
          general_hh_sample_size = integer(0),
          final_hh_sample_size = integer(0),
          stringsAsFactors = FALSE
        ))
      }
      get_col <- function(col) {
        if (col %in% names(st)) as.character(st[[col]]) else rep(NA_character_, nrow(st))
      }
      get_num <- function(col) {
        if (col %in% names(st)) suppressWarnings(as.integer(st[[col]])) else rep(NA_integer_, nrow(st))
      }
      data.frame(
        stratum                = get_col(if ("stratum_name" %in% names(st)) "stratum_name" else "stratum_id"),
        sampling_method        = get_col("sampling_method"),
        general_hh_sample_size = get_num("General_HH_Sample_Size"),
        final_hh_sample_size   = get_num("Final_HH_Sample_Size"),
        stringsAsFactors = FALSE
      )
    }
  )
)
