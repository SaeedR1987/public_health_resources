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

    #' @field drawn_sample Data frame with selected PSUs.
    drawn_sample = NULL,

    #' @field drawn_sample_full Full sampled frame with annotations.
    drawn_sample_full = NULL,

    #' @field validated Logical indicating whether the sample table has been validated.
    validated = NULL,

    #' @field metadata List containing sample metadata.
    metadata = list(
      created_datetime = NULL,
      modified_datetime = NULL
    ),

    #' @description Create a new Sample object.
    #' @param sample_table Optional sample table data frame.
    initialize = function(sample_table = NULL) {
      timestamp <- Sys.time()
      self$metadata$created_datetime <- timestamp
      self$metadata$modified_datetime <- timestamp
      self$validated <- FALSE
      if (is.null(sample_table)) {
        self$sample_table <- NULL
      } else {
        phr_validate_dataframe(
          sample_table,
          origin = "Sample$initialize",
          soft = FALSE
        )
        self$sample_table <- as.data.frame(
          sample_table,
          stringsAsFactors = FALSE
        )
      }
      invisible(self)
    },

    #' @description Replace the current sample table.
    #' @param sample_table Data frame.
    set_sample_table = function(sample_table) {
      phr_validate_dataframe(
        sample_table,
        origin = "Sample$set_sample_table",
        soft = FALSE
      )
      self$sample_table <- as.data.frame(sample_table, stringsAsFactors = FALSE)
      private$..touch()
      invisible(self)
    },

    #' @description Return the current sample table data frame.
    get_sample_table = function() {
      out <- self$sample_table
      out
    },

    #' @description Clear sample table data.
    clear_sample_table = function() {
      self$sample_table <- NULL
      private$..touch()
      invisible(self)
    },

    #' @description Add a stratum row to the sample table.
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
      mort_indicator = NA_character_,
      mort_expected_death_rate = NA_real_,
      mort_precision = NA_real_,
      mort_nonresponse = NA_real_,
      mort_design_effect = NA_real_,
      mort_recall_days = NA_real_,
      mort_avg_hh_size = NA_real_,
      mort_fpc = FALSE,
      Mort_Ind_Sample_Size = NA_real_,
      Mort_PT_Sample_Size = NA_real_,
      Mort_HH_Sample_Size = NA_real_,
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
      if (!is.null(design_effect) && is.na(pop_design_effect)) {
        pop_design_effect <- design_effect
      }
      if (!is.null(precision) && is.na(pop_precision)) {
        pop_precision <- precision
      }

      phr_assert(
        !is.null(sampling_method_site),
        message = phr_txt("sampling_method_site is required."),
        origin = "Sample$add_stratum"
      )

      site_selection_methods <- c(
        "simple_random",
        "proportional",
        "cluster",
        "systematic",
        "purposive"
      )
      phr_assert(
        sampling_method_site %in% site_selection_methods,
        message = phr_txt(
          "sampling_method_site must be one of: {paste(site_selection_methods, collapse=', ')}."
        ),
        origin = "Sample$add_stratum"
      )

      hh_selection_methods <- c("simple_random", "systematic", "rlc")
      if (sampling_method_site %in% site_selection_methods) {
        phr_assert(
          !is.null(n_sites) && !is.na(n_sites),
          message = phr_txt(
            "n_sites is required for sampling_method '{sampling_method_site}'."
          ),
          origin = "Sample$add_stratum"
        )
      }

      rlc_methods <- c(
        "pps_rlc",
        "simple_random_rlc",
        "systematic_rlc",
        "proportional_rlc"
      )
      if (
        sampling_method %in%
          rlc_methods &&
          (is.null(cluster_size) || is.na(cluster_size))
      ) {
        cluster_size <- 3L
      }

      hh_method <- if (
        sampling_method_hh %in% c("simple_random", "systematic", "rlc")
      ) {
        sampling_method_hh
      } else {
        "simple_random"
      }

      new_row <- data.frame(
        stratum_id = stratum_id,
        stratum_name = stratum_name,
        total_households = as.numeric(total_households),
        total_population = as.numeric(population_size),
        sampling_method_site = sampling_method,
        sampling_method_household = hh_method,
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
        mort_indicator = as.character(mort_indicator),
        mort_expected_death_rate = as.numeric(mort_expected_death_rate),
        mort_precision = as.numeric(mort_precision),
        mort_nonresponse = as.numeric(mort_nonresponse),
        mort_design_effect = as.numeric(mort_design_effect),
        mort_recall_days = as.numeric(mort_recall_days),
        mort_avg_hh_size = as.numeric(mort_avg_hh_size),
        mort_fpc = as.logical(mort_fpc),
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
        Mort_Ind_Sample_Size = as.numeric(Mort_Ind_Sample_Size),
        Mort_PT_Sample_Size = as.numeric(Mort_PT_Sample_Size),
        Mort_HH_Sample_Size = as.numeric(Mort_HH_Sample_Size),
        Final_HH_Sample_Size = NA_real_,
        num_interview_per_enum_per_day = NA_real_,
        num_days = NA_real_,
        stringsAsFactors = FALSE
      )

      if (is.null(self$sample_table)) {
        self$sample_table <- new_row
      } else {
        if (stratum_id %in% self$sample_table$stratum_id) {
          phr_warning(
            message = phr_txt(
              "Stratum ID '{stratum_id}' already exists and will be overwritten."
            ),
            origin = "Sample$add_stratum",
            hint = phr_txt(
              "The existing row for '{stratum_id}' has been replaced with the new values."
            )
          )
          self$sample_table <- self$sample_table[
            self$sample_table$stratum_id != stratum_id,
            ,
            drop = FALSE
          ]
        }
        self$sample_table <- rbind(self$sample_table, new_row)
      }

      private$..touch()
      invisible(self)
    },

    #' @description Remove a stratum row by strata name.
    #' @param strata_name Character scalar naming the stratum to remove.
    remove_stratum = function(strata_name) {
      phr_assert(
        is.character(strata_name) &&
          length(strata_name) == 1L &&
          nzchar(strata_name),
        message = phr_txt(
          "strata_name must be a single non-empty character value."
        ),
        origin = "Sample$remove_stratum"
      )

      st <- self$sample_table
      if (is.null(st) || !is.data.frame(st) || nrow(st) == 0L) {
        return(invisible(self))
      }

      stratum_col <- private$..resolve_stratum_name_col(st)
      phr_assert(
        !is.null(stratum_col),
        message = phr_txt(
          "sample_table does not contain a stratum name column."
        ),
        origin = "Sample$remove_stratum"
      )

      keep_rows <- as.character(st[[stratum_col]]) != strata_name
      self$sample_table <- st[keep_rows, , drop = FALSE]
      private$..touch()
      invisible(self)
    },

    #' @description Validate strata table structure.
    #' @return Logical.
    validate_strata_table = function() {
      out <- validate_strata_table(self$sample_table)
      self$validated <- out
      private$..touch()
      out
    },

    #' @description Calculate sample sizes for all strata rows.
    calculate_sample_sizes = function() {
      phr_assert(
        !is.null(self$sample_table) && nrow(self$sample_table) > 0,
        message = phr_txt("sample_table is empty. Call add_stratum() first."),
        origin = "Sample$calculate_sample_sizes"
      )
      self$sample_table <- calculate_sample_size_strata_table(self$sample_table)
      private$..touch()
      invisible(self)
    },

    #' @description Draw a sample from a sampling frame.
    #' @param frame Data frame sampling frame.
    #' @param strata_table Optional strata table. Defaults to \code{sample_table}.
    #' @param seed Integer random seed.
    #' @return Invisibly returns \code{self}.
    draw_sample = function(frame, strata_table = NULL, seed = 42) {
      phr_validate_dataframe(frame, origin = "Sample$draw_sample", soft = FALSE)

      if (is.null(strata_table)) {
        strata_table <- self$sample_table
      }
      phr_validate_dataframe(
        strata_table,
        origin = "Sample$draw_sample",
        soft = FALSE
      )
      phr_assert(
        "sampling_method_site" %in% names(strata_table),
        message = phr_txt(
          "strata_table must contain a 'sampling_method_site' column."
        ),
        origin = "Sample$draw_sample"
      )
      phr_assert(
        "sampling_method_hh" %in% names(strata_table),
        message = phr_txt(
          "strata_table must contain a 'sampling_method_hh' column."
        ),
        origin = "Sample$draw_sample"
      )

      for (col in c(
        "n_psu",
        "cluster_size",
        "n_sites",
        "Final_HH_Sample_Size"
      )) {
        if (!col %in% names(strata_table)) strata_table[[col]] <- NA_real_
      }

      if ("inclusion" %in% names(frame)) {
        eligible_rows <- which(!is.na(frame$inclusion) & frame$inclusion)
      } else {
        eligible_rows <- seq_len(nrow(frame))
      }
      eligible_frame <- frame[eligible_rows, , drop = FALSE]
      frame$sampled_psu <- NA_character_
      frame$allocated_sample <- NA_real_

      is_stratified <- ("stratum" %in% names(eligible_frame)) &&
        ("stratum_id" %in% names(strata_table)) &&
        (nrow(strata_table) > 1L || !is.null(strata_table$stratum_id))

      if (is_stratified) {
        strata_ids <- unique(strata_table$stratum_id)
        cluster_offset <- 0L

        for (st_id in strata_ids) {
          st_row <- strata_table[
            strata_table$stratum_id == st_id,
            ,
            drop = FALSE
          ]
          if (nrow(st_row) == 0L) {
            next
          }

          st_eligible_rows <- which(eligible_frame$stratum == st_id)
          if (length(st_eligible_rows) == 0L) {
            phr_warning(
              message = phr_txt(
                "Stratum '{st_id}' not found in sampling frame — skipping."
              ),
              origin = "Sample$draw_sample"
            )
            next
          }
          st_frame <- eligible_frame[st_eligible_rows, , drop = FALSE]
          st_params <- private$..params_from_strata_row(
            st_row,
            nrow(st_frame),
            nrow(eligible_frame)
          )
          st_result <- tryCatch(
            private$..apply_sampling_method(
              frame = st_frame,
              method_site = st_params$method_site,
              method_hh = st_params$method_hh,
              sample_size = st_params$sample_size,
              n_psu = st_params$n_psu,
              n_sites = st_params$n_sites,
              cluster_size = st_params$cluster_size,
              seed = seed
            ),
            error = function(e) {
              phr_warning(
                message = phr_txt(
                  "Sampling for stratum '{st_id}' failed and will be skipped: {conditionMessage(e)}"
                ),
                origin = "Sample$draw_sample"
              )
              NULL
            }
          )
          if (is.null(st_result)) {
            next
          }

          sel_mask <- !is.na(st_result$sampled_psu)
          if (any(sel_mask)) {
            parse_cluster_labels <- function(s) {
              parts <- trimws(strsplit(as.character(s), ",\\s*")[[1]])
              nums <- suppressWarnings(as.integer(parts))
              nums[!is.na(nums)]
            }
            apply_cluster_offset <- function(s, offset) {
              parts <- trimws(strsplit(as.character(s), ",\\s*")[[1]])
              vapply(
                parts,
                function(p) {
                  n <- suppressWarnings(as.integer(p))
                  if (is.na(n)) p else as.character(n + offset)
                },
                character(1),
                USE.NAMES = FALSE
              )
            }
            st_result$sampled_psu[sel_mask] <- vapply(
              st_result$sampled_psu[sel_mask],
              function(s) {
                paste(apply_cluster_offset(s, cluster_offset), collapse = ", ")
              },
              character(1)
            )
            all_nums <- unlist(lapply(
              st_result$sampled_psu[sel_mask],
              parse_cluster_labels
            ))
            if (length(all_nums) > 0L) {
              cluster_offset <- cluster_offset + max(all_nums)
            }
          }

          full_frame_rows <- eligible_rows[st_eligible_rows]
          frame$sampled_psu[full_frame_rows] <- st_result$sampled_psu
          frame$allocated_sample[full_frame_rows] <- st_result$allocated_sample
        }
      } else {
        st_row <- strata_table[1L, , drop = FALSE]
        st_params <- private$..params_from_strata_row(
          st_row,
          nrow(eligible_frame),
          nrow(eligible_frame)
        )
        result <- tryCatch(
          private$..apply_sampling_method(
            frame = eligible_frame,
            method_site = st_params$method_site,
            method_hh = st_params$method_hh,
            sample_size = st_params$sample_size,
            n_psu = st_params$n_psu,
            n_sites = st_params$n_sites,
            cluster_size = st_params$cluster_size,
            seed = seed
          ),
          error = function(e) {
            phr_warning(
              message = phr_txt("Sampling failed: {conditionMessage(e)}"),
              origin = "Sample$draw_sample"
            )
            NULL
          }
        )
        if (!is.null(result)) {
          frame$sampled_psu[eligible_rows] <- result$sampled_psu
          frame$allocated_sample[eligible_rows] <- result$allocated_sample
        }
      }

      self$drawn_sample_full <- frame
      self$drawn_sample <- frame[!is.na(frame$sampled_psu), , drop = FALSE]
      private$..touch()
      invisible(self)
    },

    #' @description Clear sampled columns from a frame.
    #' @param frame Data frame sampling frame.
    #' @return Cleared frame.
    clear_sample = function(frame) {
      out <- frame
      if (!is.null(out) && is.data.frame(out) && nrow(out) > 0) {
        if ("sampled_psu" %in% names(out)) {
          out$sampled_psu <- NA_character_
        }
        if ("allocated_sample" %in% names(out)) out$allocated_sample <- NA_real_
      }
      self$drawn_sample <- NULL
      self$drawn_sample_full <- NULL
      private$..touch()
      out
    },

    #' @description Return unique sampling methods in the sample table.

    #' @description Return unique sampling methods in the sample table.
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
        private$..touch()
        return(character(0))
      }
      m <- as.character(st[[col_name]])
      out <- unique(m[!is.na(m) & nzchar(m)])
      private$..touch()
      out
    },

    #' @description Return stratum names.
    #' @return Character vector of stratum names.
    get_strata_names = function() {
      st <- self$sample_table
      if (is.null(st) || nrow(st) == 0) {
        private$..touch()
        return(character(0))
      }
      col <- private$..resolve_stratum_name_col(st)
      if (is.null(col)) {
        private$..touch()
        return(character(0))
      }
      n <- as.character(st[[col]])
      out <- n[!is.na(n) & nzchar(n)]
      private$..touch()
      out
    }
  ),
  private = list(
    #' @description Update modified timestamp.
    #' @return Invisibly returns NULL.
    ..touch = function() {
      self$metadata$modified_datetime <- Sys.time()
      invisible(NULL)
    },

    #' @description Resolve the stratum name column from a sample table.
    #' @param st Data frame sample table.
    #' @return Character scalar naming the column, or NULL if not found.
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
    },

    #' @description Apply a sampling method to a frame.
    #' @param frame Data frame sampling frame.
    #' @param method_site Character scalar site sampling method name.
    #' @param method_hh Character scalar household sampling method name.
    #' @param sample_size Integer sample size.
    #' @param n_psu Integer number of primary sampling units.
    #' @param n_sites Integer number of sites.
    #' @param cluster_size Integer cluster size.
    #' @param seed Integer random seed.
    #' @return Data frame with sampled PSUs and allocated sample columns.
    ..apply_sampling_method = function(
      frame,
      method_site,
      method_hh,
      sample_size,
      n_psu,
      n_sites,
      cluster_size,
      seed
    ) {
      origin <- "Sample$..apply_sampling_method"
      valid_methods_site <- c(
        "simple_random",
        "proportional",
        "cluster",
        "systematic",
        "purposive"
      )
      phr_assert(
        method_site %in% valid_methods_site,
        message = phr_txt(
          "Unknown sampling method '{method_site}' — must be one of: {paste(valid_methods_site, collapse=', ')}."
        ),
        origin = origin
      )

      if (method_site == "simple_random") {
        if (method_hh == "rlc") {
          phr_assert(
            !is.null(n_sites) && !is.na(n_sites),
            message = phr_txt(
              "n_sites is required for the 'simple_random_rlc' method — set the 'n_sites' column in the strata table."
            ),
            origin = origin
          )
          cs <- if (!is.null(cluster_size) && !is.na(cluster_size)) {
            cluster_size
          } else {
            3L
          }
          draw_sample_psu_srs_rlc(frame, sample_size, n_sites, cs, seed)
        } else {
          phr_assert(
            !is.null(n_sites) && !is.na(n_sites),
            message = phr_txt(
              "n_sites is required for the 'simple_random' method_site — set the 'n_sites' column in the strata table."
            ),
            origin = origin
          )
          draw_sample_psu_srs(frame, n_sites, sample_size, seed)
        }
      } else if (method_site == "proportional") {
        if (method_hh == "rlc") {
          cs <- if (!is.null(cluster_size) && !is.na(cluster_size)) {
            cluster_size
          } else {
            3L
          }
          draw_sample_psu_proportional_rlc(frame, sample_size, cs, seed)
        } else {
          draw_sample_psu_proportional(frame, sample_size, seed)
        }
      } else if (method_site == "cluster") {
        if (method_hh == "rlc") {
          phr_assert(
            !is.null(n_sites) && !is.na(n_sites),
            message = phr_txt(
              "n_sites is required for the 'cluster' method with 'rlc' household sampling — set the 'n_sites' column in the strata table."
            ),
            origin = origin
          )
          cs <- if (!is.null(cluster_size) && !is.na(cluster_size)) {
            cluster_size
          } else {
            3L
          }
          draw_sample_psu_cluster_rlc(frame, n_sites, sample_size, cs, seed)
        } else {
          phr_assert(
            !is.null(n_psu) && !is.na(n_psu),
            message = phr_txt(
              "n_psu is required for the 'cluster' method_site — set the 'n_psu' column in the strata table."
            ),
            origin = origin
          )
          phr_assert(
            !is.null(cluster_size) && !is.na(cluster_size),
            message = phr_txt(
              "cluster_size is required for the 'cluster' method_site — set the 'cluster_size' column in the strata table."
            ),
            origin = origin
          )
          draw_sample_psu_pps_cluster(frame, n_psu, cluster_size, seed)
        }
      } else if (method_site == "systematic") {
        if (method_hh == "systematic_rlc") {
          phr_assert(
            !is.null(n_sites) && !is.na(n_sites),
            message = phr_txt(
              "n_sites is required for the 'systematic_rlc' method — set the 'n_sites' column in the strata table."
            ),
            origin = origin
          )
          cs <- if (!is.null(cluster_size) && !is.na(cluster_size)) {
            cluster_size
          } else {
            3L
          }
          draw_sample_psu_systematic_rlc(frame, sample_size, n_sites, cs, seed)
        } else {
          phr_assert(
            !is.null(n_sites) && !is.na(n_sites),
            message = phr_txt(
              "n_sites is required for the 'systematic' method — set the 'n_sites' column in the strata table."
            ),
            origin = origin
          )
          draw_sample_psu_systematic(frame, n_sites, sample_size, seed)
        }
      } else {
        draw_sample_psu_purposive(frame, seed)
      }
    },

    #' @description Extract sampling parameters from a strata row.
    #' @param st_row Data frame single strata row.
    #' @param stratum_n_eligible Integer number of eligible units in stratum.
    #' @param total_n_eligible Integer total number of eligible units.
    #' @return List with elements: method_site, method_hh, sample_size, n_psu, cluster_size, n_sites.
    ..params_from_strata_row = function(
      st_row,
      stratum_n_eligible,
      total_n_eligible
    ) {
      method_site <- as.character(st_row$sampling_method_site[1])
      method_hh <- as.character(st_row$sampling_method_hh[1])

      ss <- if (
        "Final_HH_Sample_Size" %in%
          names(st_row) &&
          !is.na(st_row$Final_HH_Sample_Size[1])
      ) {
        as.integer(st_row$Final_HH_Sample_Size[1])
      } else if (
        "General_HH_Sample_Size" %in%
          names(st_row) &&
          !is.na(st_row$General_HH_Sample_Size[1])
      ) {
        as.integer(st_row$General_HH_Sample_Size[1])
      } else {
        as.integer(round(100 * stratum_n_eligible / max(total_n_eligible, 1L)))
      }

      .na_as_null <- function(x) if (length(x) == 0L || is.na(x)) NULL else x
      list(
        method_site = method_site,
        method_hh = method_hh,
        sample_size = ss,
        n_psu = .na_as_null(
          if ("n_psu" %in% names(st_row)) st_row$n_psu[1] else NA
        ),
        cluster_size = .na_as_null(
          if ("cluster_size" %in% names(st_row)) st_row$cluster_size[1] else NA
        ),
        n_sites = .na_as_null(
          if ("n_sites" %in% names(st_row)) st_row$n_sites[1] else NA
        )
      )
    }
  )
)
