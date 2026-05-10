#' Protocol Utility Functions
#'
#' @description
#' Utility functions for working with Protocol objects and protocol workflows.

# Minimum columns every master strata table must contain.
.strata_table_required_cols <- c(
  "stratum_id", "stratum_name", "total_population", "sampling_method", "calc_method",
  "pop_indicator", "General_HH_Sample_Size",
  "ind_indicator", "Ind_HH_Sample_Size",
  "mort_indicator", "Mort_HH_Sample_Size",
  "Ind_Sample_Size", "Mort_Ind_Sample_Size", "Mort_PT_Sample_Size",
  "Final_HH_Sample_Size"
)

#' Create a new survey protocol instance
#'
#' Creates a \code{\link{SurveyProtocol}} object, which extends
#' \code{\link{Protocol}} with strata definition, sample size calculations,
#' sampling frame management, and sample drawing capabilities.
#'
#' @param assessment_title Character. Title of the assessment
#' @param country_name Character. Country where assessment takes place
#' @param month_year Character. Month and year of data collection
#' @param framework_type Character. Type of framework to initialise.  One of
#'   \code{"none"} (default) or \code{"ana"}.
#' @param sampling_frame Optional data frame to initialise the
#'   \code{\link{SamplingFrame}} with.  When \code{NULL} (default), an empty
#'   \code{SamplingFrame} is created.
#' @return A new SurveyProtocol object
#' @export
create_survey_protocol <- function(assessment_title = NULL, country_name = NULL, month_year = NULL,
                                    framework_type = "none", sampling_frame = NULL) {
  SurveyProtocol$new(assessment_title = assessment_title, country_name = country_name,
                     month_year = month_year, framework_type = framework_type,
                     sampling_frame = sampling_frame)
}

#' Validate protocol completeness
#'
#' @param protocol Protocol object to validate
#' @return List with validation results and issues
#' @export
validate_protocol <- function(protocol) {

  origin <- "validate_protocol"

  phr_try({
    phr_assert(
      inherits(protocol, "Protocol"),
      message = phr_txt("Object is not a Protocol instance."),
      origin  = origin,
      hint    = phr_txt("Use Protocol$new() or SurveyProtocol$new() to create a Protocol object.")
    )

    issues <- protocol$get_issues()

    list(
      has_issues = length(issues) > 0,
      issues     = issues,
      summary    = protocol$get_protocol_summary()
    )
  }, on_error = "abort", origin = origin)
}

#' Validate the master strata table structure
#'
#' Checks that a data frame conforms to the expected master strata table
#' structure with all required columns.  This is a standalone helper that
#' mirrors the \code{Protocol$validate_strata_table()} method.
#'
#' @param sample_table A data frame to validate (typically
#'   \code{protocol$sample_table}).
#' @return \code{TRUE} if the table is valid, \code{FALSE} otherwise.
#' @export
validate_strata_table <- function(sample_table) {

  origin <- "validate_strata_table"

  if (is.null(sample_table) || !is.data.frame(sample_table)) {
    return(FALSE)
  }

  if (nrow(sample_table) == 0) {
    return(FALSE)
  }

  required_cols <- .strata_table_required_cols

  missing_cols <- setdiff(required_cols, names(sample_table))
  if (length(missing_cols) > 0) {
    return(FALSE)
  }

  dupes <- sample_table$stratum_id[duplicated(sample_table$stratum_id)]
  if (length(dupes) > 0) {
    return(FALSE)
  }

  TRUE
}

#' Recalculate sample sizes and field plan for all rows of a strata table
#'
#' Goes row by row through a master strata table and recalculates the
#' \code{General_HH_Sample_Size}, \code{Ind_Sample_Size},
#' \code{Ind_HH_Sample_Size}, \code{Mort_Ind_Sample_Size},
#' \code{Mort_PT_Sample_Size}, and \code{Mort_HH_Sample_Size} columns from
#' the stored parameters wherever sufficient information is available.  After
#' recalculation, \code{Final_HH_Sample_Size} is set to the maximum household
#' sample size across the three HH-level calculation types for each row.
#'
#' After computing sample sizes, also calls \code{\link{estimate_field_plan}}
#' for each stratum where the necessary logistics parameters are present.
#' The resulting field-plan values are written back into \code{sample_table}:
#' \itemize{
#'   \item \code{num_interview_per_enum_per_day} — estimated interviews per
#'     enumerator per working day.
#'   \item \code{num_days} — estimated number of data-collection days needed.
#'   \item \code{n_psu} — number of PSUs required (\code{NA} for simple random
#'     designs); written into the existing \code{n_psu} column.
#'   \item \code{cluster_size} — cluster size (\code{NA} for simple random
#'     designs); written into the existing \code{cluster_size} column.
#' }
#'
#' Required parameters per calculation type:
#' \itemize{
#'   \item \strong{General}: \code{pop_expected_prevalence}, \code{pop_precision}
#'   \item \strong{Individual}: \code{ind_expected_prevalence}, \code{ind_precision},
#'     \code{ind_avg_hh_size} (> 0)
#'   \item \strong{Mortality}: \code{mort_expected_death_rate}, \code{mort_precision},
#'     \code{mort_avg_hh_size} (> 0)
#' }
#'
#' Required parameters for the field plan estimate:
#' \itemize{
#'   \item \strong{All designs}: \code{teams}, \code{enumerators_per_team},
#'     \code{start_time}, \code{end_time}, \code{avg_interview_time},
#'     \code{avg_travel_time}, \code{avg_rest_time}, and
#'     \code{Final_HH_Sample_Size}.
#'   \item \strong{Cluster design} (\code{calc_method = "cluster"}):
#'     additionally \code{clusters_per_day}.
#' }
#'
#' @param sample_table A data frame conforming to the master strata table
#'   schema (validated with \code{validate_strata_table}).
#' @return The updated \code{sample_table} with recalculated sample size
#'   and field plan columns.
#' @export
calculate_sample_size_strata_table <- function(sample_table) {

  origin <- "calculate_sample_size_strata_table"

  # Default design effect used as fallback when no value is stored.
  .default_design_effect <- 1.5

  phr_try({

    phr_assert(
      isTRUE(validate_strata_table(sample_table)),
      message = phr_txt("sample_table is invalid. Ensure it was built via add_stratum() or conforms to the required schema."),
      origin  = origin,
      hint    = phr_txt("Ensure the strata table was built via add_stratum() or conforms to the required schema.")
    )

    # Ensure field-plan output columns exist (may be absent in tables not built via add_stratum())
    for (col in c("n_psu", "cluster_size", "num_interview_per_enum_per_day", "num_days")) {
      if (!col %in% names(sample_table)) sample_table[[col]] <- NA_real_
    }

    for (i in seq_len(nrow(sample_table))) {
      row <- sample_table[i, ]

      # Read calc_method directly — it is "simple_random" or "cluster" and
      # maps directly to the design parameter accepted by calculate_sample_size_*
      # functions.  Fall back to "simple_random" for robustness if absent.
      design_type <- if ("calc_method" %in% names(row) && !is.na(row$calc_method) &&
                         identical(row$calc_method, "cluster")) {
        "cluster"
      } else {
        "simple_random"
      }

      # ---- General (population-level) sample size -------------------------
      if (!is.na(row$pop_expected_prevalence) && !is.na(row$pop_precision)) {
        design_effect <- if (!is.na(row$pop_design_effect) && row$pop_design_effect > 1) row$pop_design_effect else .default_design_effect
        nonresponse   <- if (!is.na(row$pop_nonresponse)) row$pop_nonresponse else 5
        fpc           <- if (!is.na(row$pop_fpc)) as.logical(row$pop_fpc) else FALSE
        total_pop     <- if (!is.na(row$total_population) && row$total_population > 0) row$total_population else NULL

        pop_ss <- phr_try(
          calculate_sample_size_general(
            expected_proportion = row$pop_expected_prevalence,
            desired_precision   = row$pop_precision,
            non_response_rate   = nonresponse,
            design              = design_type,
            design_effect       = design_effect,
            fpc                 = fpc,
            total_population    = total_pop
          ),
          on_error = "return",
          origin   = origin,
          step     = phr_txt("General sample size — stratum {row$stratum_id}")
        )
        if (!phr_failed(pop_ss)) {
          sample_table$General_HH_Sample_Size[i] <- pop_ss
        }
      }

      # ---- Individual-level sample size -----------------------------------
      if (!is.na(row$ind_expected_prevalence) && !is.na(row$ind_precision) &&
          !is.na(row$ind_avg_hh_size) && row$ind_avg_hh_size > 0) {
        design_effect <- if (!is.na(row$ind_design_effect) && row$ind_design_effect > 1) row$ind_design_effect else .default_design_effect
        nonresponse   <- if (!is.na(row$ind_nonresponse)) row$ind_nonresponse else 5
        fpc           <- if (!is.na(row$ind_fpc)) as.logical(row$ind_fpc) else FALSE
        total_pop     <- if (!is.na(row$total_population) && row$total_population > 0) row$total_population else NULL
        subpop        <- if (!is.na(row$ind_subpop_prop)) row$ind_subpop_prop else 100

        ind_res <- phr_try(
          calculate_sample_size_individual(
            expected_proportion    = row$ind_expected_prevalence,
            desired_precision      = row$ind_precision,
            non_response_rate      = nonresponse,
            design                 = design_type,
            design_effect          = design_effect,
            fpc                    = fpc,
            total_population       = total_pop,
            average_household_size = row$ind_avg_hh_size,
            sub_population_percent = subpop
          ),
          on_error = "return",
          origin   = origin,
          step     = phr_txt("Individual sample size — stratum {row$stratum_id}")
        )
        if (!phr_failed(ind_res) && !is.null(ind_res)) {
          sample_table$Ind_Sample_Size[i]    <- ind_res$sample_size_individuals
          sample_table$Ind_HH_Sample_Size[i] <- ind_res$sample_size_households
        }
      }

      # ---- Mortality-rate sample size -------------------------------------
      if (!is.na(row$mort_expected_death_rate) && !is.na(row$mort_precision) &&
          !is.na(row$mort_avg_hh_size) && row$mort_avg_hh_size > 0) {
        design_effect <- if (!is.na(row$mort_design_effect) && row$mort_design_effect > 1) row$mort_design_effect else .default_design_effect
        nonresponse   <- if (!is.na(row$mort_nonresponse)) row$mort_nonresponse else 5
        fpc           <- if (!is.na(row$mort_fpc)) as.logical(row$mort_fpc) else FALSE
        total_pop     <- if (!is.na(row$total_population) && row$total_population > 0) row$total_population else NULL
        recall_days   <- if (!is.na(row$mort_recall_days) && row$mort_recall_days > 0) row$mort_recall_days else 90

        mort_res <- phr_try(
          calculate_sample_size_mortality(
            expected_death_rate    = row$mort_expected_death_rate,
            desired_precision      = row$mort_precision,
            non_response_rate      = nonresponse,
            design                 = design_type,
            design_effect          = design_effect,
            recall_days            = recall_days,
            average_household_size = row$mort_avg_hh_size,
            fpc                    = fpc,
            total_population       = total_pop
          ),
          on_error = "return",
          origin   = origin,
          step     = phr_txt("Mortality sample size — stratum {row$stratum_id}")
        )
        if (!phr_failed(mort_res) && !is.null(mort_res)) {
          sample_table$Mort_Ind_Sample_Size[i] <- mort_res$sample_size_individuals
          sample_table$Mort_PT_Sample_Size[i]  <- mort_res$sample_size_person_time
          sample_table$Mort_HH_Sample_Size[i]  <- mort_res$sample_size_households
        }
      }

      # ---- Final household sample size: max across all three HH types -----
      hh_sizes <- c(
        pop_hh  = if (!is.na(sample_table$General_HH_Sample_Size[i])) sample_table$General_HH_Sample_Size[i] else NA_real_,
        ind_hh  = if (!is.na(sample_table$Ind_HH_Sample_Size[i]))     sample_table$Ind_HH_Sample_Size[i]     else NA_real_,
        mort_hh = if (!is.na(sample_table$Mort_HH_Sample_Size[i]))    sample_table$Mort_HH_Sample_Size[i]    else NA_real_
      )

      valid_hh <- hh_sizes[!is.na(hh_sizes)]
      if (length(valid_hh) > 0 && "Final_HH_Sample_Size" %in% names(sample_table)) {
        sample_table$Final_HH_Sample_Size[i] <- max(valid_hh)
      }

      # ---- Field plan estimate --------------------------------------------
      # Re-read the row after sample size updates so Final_HH_Sample_Size is current.
      row <- sample_table[i, ]

      base_fields <- c("teams", "enumerators_per_team", "start_time",
                       "end_time", "avg_interview_time", "avg_travel_time",
                       "avg_rest_time", "Final_HH_Sample_Size")
      has_base <- all(vapply(base_fields, function(f) {
        f %in% names(row) && !is.na(row[[f]]) && nzchar(as.character(row[[f]]))
      }, logical(1L)))

      has_cluster_param <- design_type != "cluster" ||
        ("clusters_per_day" %in% names(row) &&
         !is.na(row$clusters_per_day) &&
         row$clusters_per_day > 0)

      if (has_base && has_cluster_param) {
        fp <- phr_try(
          estimate_field_plan(
            sample_design                  = design_type,
            number_of_teams                = row$teams,
            enumerators_per_team           = row$enumerators_per_team,
            number_of_psu_per_team_per_day = if (design_type == "cluster") row$clusters_per_day else NULL,
            start_time                     = row$start_time,
            end_time                       = row$end_time,
            average_interview_time         = row$avg_interview_time,
            average_travel_time            = row$avg_travel_time,
            average_rest_time              = row$avg_rest_time,
            total_sample_size              = row$Final_HH_Sample_Size
          ),
          on_error = "return",
          origin   = origin,
          step     = phr_txt("Field plan — stratum {row$stratum_id}")
        )
        if (!phr_failed(fp)) {
          sample_table$num_interview_per_enum_per_day[i] <- fp$num_interview_per_enum_per_day
          sample_table$num_days[i]                       <- fp$num_days
          sample_table$n_psu[i]                          <- fp$num_psu_needed
          sample_table$cluster_size[i]                   <- fp$psu_size
        }
      }
    }

    sample_table

  }, on_error = "abort", origin = origin)
}

#' Save protocol to RDS file
#'
#' @param protocol Protocol object to save
#' @param file Character. File path for saving
#' @export
save_protocol <- function(protocol, file) {

  origin <- "save_protocol"

  phr_try({
    phr_assert(
      inherits(protocol, "Protocol"),
      message = phr_txt("Object is not a Protocol instance."),
      origin  = origin
    )

    protocol_export <- protocol$export_protocol()
    saveRDS(protocol_export, file = file)
    phr_message(phr_txt("Protocol saved to: {file}"), origin = origin)
  }, on_error = "abort", origin = origin)
}

#' Load protocol from RDS file
#'
#' @param file Character. File path to load from
#' @return List containing protocol data
#' @export
load_protocol <- function(file) {

  origin <- "load_protocol"

  phr_try({
    phr_assert(
      file.exists(file),
      message = phr_txt("File does not exist: '{file}'"),
      origin  = origin,
      hint    = phr_txt("Check the file path and ensure the file has not been moved or deleted.")
    )

    protocol_data <- readRDS(file)
    phr_message(phr_txt("Protocol loaded from: {file}"), origin = origin)
    protocol_data
  }, on_error = "abort", origin = origin)
}

#' Restore protocol object from exported data
#'
#' Restores a \code{\link{SurveyProtocol}} when the exported data contains
#' sampling fields (\code{sample_table}, \code{sampling_frame}, etc.),
#' otherwise restores a base \code{\link{Protocol}}.
#'
#' @param protocol_data List. Exported protocol data from export_protocol()
#' @return A new Protocol or SurveyProtocol object with restored data
#' @export
restore_protocol <- function(protocol_data) {

  origin <- "restore_protocol"

  phr_try({
    phr_assert(
      is.list(protocol_data) && !is.null(protocol_data$metadata),
      message = phr_txt("protocol_data must be a list produced by export_protocol()."),
      origin  = origin
    )

    has_sampling_data <- any(c("sample_table", "sampling_frame", "drawn_sample",
                               "drawn_sample_full") %in% names(protocol_data) &
                             !vapply(protocol_data[intersect(c("sample_table", "sampling_frame",
                                                               "drawn_sample", "drawn_sample_full"),
                                                             names(protocol_data))],
                                    is.null, logical(1)))

    if (has_sampling_data) {
      protocol <- SurveyProtocol$new(
        assessment_title = protocol_data$metadata$assessment_title,
        country_name     = protocol_data$metadata$country_name,
        month_year       = protocol_data$metadata$month_year
      )
      if (!is.null(protocol_data$sample_object) && inherits(protocol_data$sample_object, "Sample")) {
        protocol$sample_table <- protocol_data$sample_object
      } else if (!is.null(protocol_data$sample_table) && is.data.frame(protocol_data$sample_table)) {
        if (is.null(protocol$sample_table) || !inherits(protocol$sample_table, "Sample")) {
          protocol$sample_table <- Sample$new()
        }
        protocol$sample_table$set_sample_table(protocol_data$sample_table)
      }
      # sampling_frame is exported as a raw data frame; restore into SamplingFrame object.
      if (!is.null(protocol_data$sampling_frame) &&
          is.data.frame(protocol_data$sampling_frame) &&
          nrow(protocol_data$sampling_frame) > 0) {
        protocol$sampling_frame$log_df <- tibble::as_tibble(protocol_data$sampling_frame)
      }
      protocol$drawn_sample      <- protocol_data$drawn_sample
      protocol$drawn_sample_full <- protocol_data$drawn_sample_full
    } else {
      protocol <- Protocol$new(
        assessment_title = protocol_data$metadata$assessment_title,
        country_name     = protocol_data$metadata$country_name,
        month_year       = protocol_data$metadata$month_year
      )
    }

    protocol$metadata            <- protocol_data$metadata
    protocol$conditional_metadata <- protocol_data$conditional_metadata %||% list()
    protocol$objectives          <- protocol_data$objectives %||% list()
    protocol$objective_schema    <- protocol_data$objective_schema %||% protocol$objective_schema
    if (!is.null(protocol_data$framework)) {
      protocol$framework <- restore_framework(protocol_data$framework)
    }
    protocol$tools               <- protocol_data$tools
    protocol$selected_indicators <- protocol_data$selected_indicators
    protocol$objective_catalog_master   <- protocol_data$objective_catalog_master   %||% protocol$objective_catalog_master
    protocol$objective_catalog_adjusted <- protocol_data$objective_catalog_adjusted %||% protocol$objective_catalog_adjusted
    protocol$indicator_catalog_master   <- protocol_data$indicator_catalog_master   %||% protocol$indicator_catalog_master
    protocol$indicator_catalog_adjusted <- protocol_data$indicator_catalog_adjusted %||% protocol$indicator_catalog_adjusted
    protocol$tool_indicator_catalog_master   <- protocol_data$tool_indicator_catalog_master   %||% protocol$tool_indicator_catalog_master
    protocol$tool_indicator_catalog_revised  <- protocol_data$tool_indicator_catalog_revised  %||% protocol$tool_indicator_catalog_revised
    protocol$sampling_frame_strata_names     <- protocol_data$sampling_frame_strata_names     %||% protocol$sampling_frame_strata_names
    protocol$issues              <- protocol_data$issues
    if ("synchronize_state" %in% names(protocol) && is.function(protocol$synchronize_state)) {
      protocol$synchronize_state()
    }
    protocol$touch()

    phr_message(phr_txt("Protocol restored successfully."), origin = origin)
    protocol
  }, on_error = "abort", origin = origin)
}

#' Generate a Word document report from a Protocol or SurveyProtocol object
#'
#' Convenience wrapper around \code{Protocol$generate_reach_tor()} and
#' \code{SurveyProtocol$generate_reach_tor()}.  Dispatches to the correct method
#' based on the class of \code{protocol}.
#'
#' @param protocol A \code{\link{Protocol}} or \code{\link{SurveyProtocol}}
#'   object.
#' @param output_file Character. Output \code{.docx} file path.  Defaults to
#'   \code{"protocol_report.docx"} in the current working directory.
#' @param reference_docx Character or \code{NULL}. Path to a Word style
#'   reference document.  Uses the package-bundled template by default.
#' @param open Logical. Whether to open the file after writing.  Defaults to
#'   \code{FALSE}.
#' @return Invisibly returns the protocol object.
#' @export
generate_protocol_report <- function(protocol,
                                     output_file   = "protocol_report.docx",
                                     reference_docx = NULL,
                                     open           = FALSE) {

  origin <- "generate_protocol_report"

  phr_try({
    phr_assert(
      inherits(protocol, "Protocol"),
      message = phr_txt("Object is not a Protocol or SurveyProtocol instance."),
      origin  = origin,
      hint    = phr_txt("Use Protocol$new() or create_survey_protocol() to create a valid object.")
    )

    protocol$generate_reach_tor(
      output_file    = output_file,
      reference_docx = reference_docx,
      open           = open
    )
  }, on_error = "abort", origin = origin)

  invisible(protocol)
}

#' @param protocol Protocol object to summarize
#' @export
print_protocol_summary <- function(protocol) {

  origin <- "print_protocol_summary"

  phr_try({
    phr_assert(
      inherits(protocol, "Protocol"),
      message = phr_txt("Object is not a Protocol instance."),
      origin  = origin
    )

    summary <- protocol$get_protocol_summary()

    phr_message(
      phr_txt("Protocol: {summary$assessment_title} | Country: {summary$country_name} | {summary$month_year}"),
      origin = origin
    )
    phr_message(
      phr_txt("Objectives: {summary$num_objectives} | Strata: {if (is.null(summary$num_strata)) 'N/A' else summary$num_strata} | Tools: {summary$num_tools}"),
      origin = origin
    )

    if (summary$num_issues > 0) {
      issues <- protocol$get_issues()
      for (issue_name in names(issues)) {
        phr_warning(message = phr_txt("{issues[[issue_name]]}"), origin = origin)
      }
    } else {
      phr_message(phr_txt("No issues detected."), origin = origin)
    }
  }, on_error = "abort", origin = origin)
}
