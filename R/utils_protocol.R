#' Protocol Utility Functions
#'
#' @description
#' Utility functions for working with Protocol objects and protocol workflows.

# Minimum columns every master strata table must contain.
.strata_table_required_cols <- c(
  "stratum_id", "Population_Name", "Total_Population", "Sampling_Method",
  "pop_indicator", "pop_result_dummy",
  "ind_result_dummy",
  "mort_result_dummy",
  "Final_HH_Sample_Size"
)

#' Create a new protocol instance
#'
#' @param assessment_title Character. Title of the assessment
#' @param country_name Character. Country where assessment takes place
#' @param month_year Character. Month and year of data collection
#' @return A new Protocol object
#' @export
create_protocol <- function(assessment_title = NULL, country_name = NULL, month_year = NULL) {
  Protocol$new(assessment_title = assessment_title, country_name = country_name, month_year = month_year)
}

#' Validate protocol completeness
#'
#' @param protocol Protocol object to validate
#' @return List with validation results and issues
#' @export
validate_protocol <- function(protocol) {
  if (!inherits(protocol, "Protocol")) {
    stop("Object is not a Protocol instance")
  }
  
  # Get issues from protocol
  issues <- protocol$get_issues()
  
  results <- list(
    has_issues = length(issues) > 0,
    issues = issues,
    summary = protocol$get_protocol_summary()
  )
  
  return(results)
}

#' Validate the master strata table structure
#'
#' Checks that a data frame conforms to the expected master strata table
#' structure with all required columns.  This is a standalone helper that
#' mirrors the \code{Protocol$validate_strata_table()} method.
#'
#' @param sample_table A data frame to validate (typically
#'   \code{protocol$sample_table}).
#' @return Named list with elements \code{valid} (logical) and
#'   \code{message} (character).
#' @export
validate_strata_table <- function(sample_table) {

  if (is.null(sample_table) || !is.data.frame(sample_table)) {
    return(list(valid = FALSE,
                message = "sample_table must be a non-NULL data frame."))
  }

  if (nrow(sample_table) == 0) {
    return(list(valid = FALSE,
                message = "sample_table is empty (zero rows)."))
  }

  required_cols <- .strata_table_required_cols

  missing_cols <- setdiff(required_cols, names(sample_table))
  if (length(missing_cols) > 0) {
    return(list(
      valid   = FALSE,
      message = paste("sample_table is missing required columns:",
                      paste(missing_cols, collapse = ", "))
    ))
  }

  dupes <- sample_table$stratum_id[duplicated(sample_table$stratum_id)]
  if (length(dupes) > 0) {
    return(list(
      valid   = FALSE,
      message = paste("Duplicate stratum_id values:", paste(dupes, collapse = ", "))
    ))
  }

  list(valid = TRUE, message = "sample_table structure is valid.")
}

#' Recalculate sample sizes for all rows of a strata table
#'
#' Goes row by row through a master strata table and recalculates the
#' \code{pop_result_dummy}, \code{ind_result_dummy}, and
#' \code{mort_result_dummy} columns from the stored parameters wherever
#' sufficient information is available.  After recalculation,
#' \code{Final_HH_Sample_Size} is set to the maximum household sample size
#' across the three calculation types for each row.
#'
#' @param sample_table A data frame conforming to the master strata table
#'   schema (validated with \code{validate_strata_table}).
#' @return The updated \code{sample_table} with recalculated sample size
#'   columns.
#' @export
calculate_sample_size_strata_table <- function(sample_table) {

  val_result <- validate_strata_table(sample_table)
  if (!val_result$valid) {
    stop(val_result$message)
  }

  for (i in seq_len(nrow(sample_table))) {
    row <- sample_table[i, ]

    # ---- General (population-level) sample size -------------------------
    if (!is.na(row$pop_expected_prevalence) && !is.na(row$pop_precision)) {
      design_effect <- if (!is.na(row$pop_design_effect) && row$pop_design_effect > 1) row$pop_design_effect else 1.5
      design_type   <- if (!is.na(row$pop_design_effect) && row$pop_design_effect > 1) "cluster" else "simple_random"
      nonresponse   <- if (!is.na(row$pop_nonresponse)) row$pop_nonresponse else 5
      fpc           <- if (!is.na(row$pop_fpc)) as.logical(row$pop_fpc) else FALSE
      total_pop     <- if (!is.na(row$Total_Population) && row$Total_Population > 0) row$Total_Population else NULL

      pop_ss <- tryCatch(
        calculate_sample_size_general(
          expected_proportion = row$pop_expected_prevalence,
          desired_precision   = row$pop_precision,
          non_response_rate   = nonresponse,
          design              = design_type,
          design_effect       = design_effect,
          fpc                 = fpc,
          total_population    = total_pop
        ),
        error = function(e) NA_real_
      )
      sample_table$pop_result_dummy[i] <- pop_ss
    }

    # ---- Individual-level sample size -----------------------------------
    if (!is.na(row$ind_expected_prevalence) && !is.na(row$ind_precision) &&
        !is.na(row$ind_avg_hh_size) && row$ind_avg_hh_size > 0) {
      design_effect <- if (!is.na(row$ind_design_effect) && row$ind_design_effect > 1) row$ind_design_effect else 1.5
      design_type   <- if (!is.na(row$ind_design_effect) && row$ind_design_effect > 1) "cluster" else "simple_random"
      nonresponse   <- if (!is.na(row$ind_nonresponse)) row$ind_nonresponse else 5
      fpc           <- if (!is.na(row$ind_fpc)) as.logical(row$ind_fpc) else FALSE
      total_pop     <- if (!is.na(row$Total_Population) && row$Total_Population > 0) row$Total_Population else NULL
      subpop        <- if (!is.na(row$ind_subpop_prop)) row$ind_subpop_prop else 100

      ind_res <- tryCatch(
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
        error = function(e) NULL
      )
      if (!is.null(ind_res)) {
        sample_table$ind_result_dummy[i] <- ind_res$sample_size_households
      }
    }

    # ---- Mortality-rate sample size -------------------------------------
    if (!is.na(row$mort_expected_death_rate) && !is.na(row$mort_precision) &&
        !is.na(row$mort_avg_hh_size) && row$mort_avg_hh_size > 0) {
      design_effect <- if (!is.na(row$mort_design_effect) && row$mort_design_effect > 1) row$mort_design_effect else 1.5
      design_type   <- if (!is.na(row$mort_design_effect) && row$mort_design_effect > 1) "cluster" else "simple_random"
      nonresponse   <- if (!is.na(row$mort_nonresponse)) row$mort_nonresponse else 5
      fpc           <- if (!is.na(row$mort_fpc)) as.logical(row$mort_fpc) else FALSE
      total_pop     <- if (!is.na(row$Total_Population) && row$Total_Population > 0) row$Total_Population else NULL
      recall_days   <- if (!is.na(row$mort_recall_days) && row$mort_recall_days > 0) row$mort_recall_days else 90

      mort_res <- tryCatch(
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
        error = function(e) NULL
      )
      if (!is.null(mort_res)) {
        sample_table$mort_result_dummy[i] <- mort_res$sample_size_households
      }
    }

    # ---- Final household sample size: max across all three types --------
    hh_sizes <- c(
      pop_hh  = if (!is.na(sample_table$pop_result_dummy[i]))  sample_table$pop_result_dummy[i]  else NA_real_,
      ind_hh  = if (!is.na(sample_table$ind_result_dummy[i]))  sample_table$ind_result_dummy[i]  else NA_real_,
      mort_hh = if (!is.na(sample_table$mort_result_dummy[i])) sample_table$mort_result_dummy[i] else NA_real_
    )

    valid_hh <- hh_sizes[!is.na(hh_sizes)]
    if (length(valid_hh) > 0 && "Final_HH_Sample_Size" %in% names(sample_table)) {
      sample_table$Final_HH_Sample_Size[i] <- max(valid_hh)
    }
  }

  return(sample_table)
}

#' Save protocol to RDS file
#'
#' @param protocol Protocol object to save
#' @param file Character. File path for saving
#' @export
save_protocol <- function(protocol, file) {
  if (!inherits(protocol, "Protocol")) {
    stop("Object is not a Protocol instance")
  }
  
  protocol_export <- protocol$export_protocol()
  saveRDS(protocol_export, file = file)
  message("Protocol saved to: ", file)
}

#' Load protocol from RDS file
#'
#' @param file Character. File path to load from
#' @return List containing protocol data
#' @export
load_protocol <- function(file) {
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }
  
  protocol_data <- readRDS(file)
  message("Protocol loaded from: ", file)
  return(protocol_data)
}

#' Restore protocol object from exported data
#'
#' @param protocol_data List. Exported protocol data from export_protocol()
#' @return A new Protocol object with restored data
#' @export
restore_protocol <- function(protocol_data) {
  # Create new protocol
  protocol <- Protocol$new(
    assessment_title = protocol_data$metadata$assessment_title,
    country_name = protocol_data$metadata$country_name,
    month_year = protocol_data$metadata$month_year
  )
  
  # Restore data
  protocol$metadata <- protocol_data$metadata
  protocol$primary_objectives <- protocol_data$primary_objectives
  protocol$secondary_objectives <- protocol_data$secondary_objectives
  protocol$objective_schema <- protocol_data$objective_schema %||% protocol$objective_schema
  protocol$sample_table <- protocol_data$sample_table
  protocol$sampling_frame <- protocol_data$sampling_frame
  protocol$drawn_sample <- protocol_data$drawn_sample
  protocol$tools <- protocol_data$tools
  protocol$selected_indicators <- protocol_data$selected_indicators
  protocol$issues <- protocol_data$issues
  
  return(protocol)
}

#' Print protocol summary
#'
#' @param protocol Protocol object to summarize
#' @export
print_protocol_summary <- function(protocol) {
  if (!inherits(protocol, "Protocol")) {
    stop("Object is not a Protocol instance")
  }
  
  summary <- protocol$get_protocol_summary()
  
  cat("Protocol Summary\n")
  cat("================\n\n")
  
  cat("Assessment:", summary$assessment_title, "\n")
  cat("Country:", summary$country_name, "\n")
  cat("Month/Year:", summary$month_year, "\n")
  cat("Created:", format(summary$created, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Modified:", format(summary$modified, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  cat("Content:\n")
  cat("  Primary Objectives:", summary$num_primary_objectives, "\n")
  cat("  Secondary Objectives:", summary$num_secondary_objectives, "\n")
  cat("  Strata:", summary$num_strata, "\n")
  cat("  Total Sample Size (pop level):", summary$total_sample_size, "\n")
  cat("  Tools:", summary$num_tools, "\n\n")
  
  cat("Issues:", summary$num_issues, "\n")
  if (summary$num_issues > 0) {
    issues <- protocol$get_issues()
    for (issue_name in names(issues)) {
      cat("  -", issues[[issue_name]], "\n")
    }
  }
}

