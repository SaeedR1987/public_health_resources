#' Protocol Utility Functions
#'
#' @description
#' Utility functions for working with Protocol objects and protocol workflows.

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
  cat("  Total Sample Size:", summary$total_sample_size, "\n")
  cat("  Tools:", summary$num_tools, "\n\n")
  
  cat("Issues:", summary$num_issues, "\n")
  if (summary$num_issues > 0) {
    issues <- protocol$get_issues()
    for (issue_name in names(issues)) {
      cat("  -", issues[[issue_name]], "\n")
    }
  }
}

