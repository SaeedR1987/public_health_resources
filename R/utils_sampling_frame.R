#' Sampling Frame Validation Functions
#'
#' @description
#' Functions for validating sampling frames before sample drawing.

#' Validate sampling frame structure
#'
#' @param frame Data frame. The sampling frame to validate
#' @param required_cols Character vector. Required column names (default = c("id", "stratum", "population_size"))
#' @return List with validation results
#' @export
validate_sampling_frame <- function(frame, 
                                    required_cols = c("id", "stratum", "population_size")) {
  
  issues <- list()
  
  # Check if frame is a data frame
  if (!is.data.frame(frame)) {
    return(list(valid = FALSE, message = "Frame must be a data frame"))
  }
  
  # Check for empty frame
  if (nrow(frame) == 0) {
    return(list(valid = FALSE, message = "Frame is empty"))
  }
  
  # Check required columns
  missing_cols <- setdiff(required_cols, names(frame))
  if (length(missing_cols) > 0) {
    issues$missing_columns <- missing_cols
  }
  
  # Check for duplicate IDs
  if ("id" %in% names(frame)) {
    if (any(duplicated(frame$id))) {
      issues$duplicate_ids <- frame$id[duplicated(frame$id)]
    }
  }
  
  # Check for missing values in required columns
  for (col in intersect(required_cols, names(frame))) {
    if (any(is.na(frame[[col]]))) {
      issues$missing_values[[col]] <- sum(is.na(frame[[col]]))
    }
  }
  
  # Check population sizes are positive
  if ("population_size" %in% names(frame)) {
    if (any(frame$population_size <= 0, na.rm = TRUE)) {
      issues$invalid_population_size <- sum(frame$population_size <= 0, na.rm = TRUE)
    }
  }
  
  # Check for empty strata
  if ("stratum" %in% names(frame)) {
    strata_counts <- table(frame$stratum)
    empty_strata <- names(strata_counts[strata_counts == 0])
    if (length(empty_strata) > 0) {
      issues$empty_strata <- empty_strata
    }
  }
  
  if (length(issues) == 0) {
    return(list(
      valid = TRUE,
      message = "Sampling frame is valid",
      summary = list(
        num_units = nrow(frame),
        num_strata = length(unique(frame$stratum)),
        total_population = sum(frame$population_size, na.rm = TRUE)
      )
    ))
  } else {
    return(list(valid = FALSE, issues = issues))
  }
}

#' Check frame coverage for sample table
#'
#' @param frame Data frame. The sampling frame
#' @param sample_table Data frame. The sample table with strata
#' @return List with coverage check results
#' @export
check_frame_coverage <- function(frame, sample_table) {
  
  if (!is.data.frame(frame) || !is.data.frame(sample_table)) {
    stop("Both frame and sample_table must be data frames")
  }
  
  # Get strata from each
  frame_strata <- unique(frame$stratum)
  table_strata <- sample_table$stratum_id
  
  # Check coverage
  missing_in_frame <- setdiff(table_strata, frame_strata)
  missing_in_table <- setdiff(frame_strata, table_strata)
  
  # Check if frame has enough units for each stratum
  insufficient_strata <- list()
  for (i in seq_len(nrow(sample_table))) {
    stratum_id <- sample_table$stratum_id[i]
    needed <- sample_table$sample_size[i]
    available <- sum(frame$stratum == stratum_id)
    
    if (available < needed) {
      insufficient_strata[[stratum_id]] <- list(
        needed = needed,
        available = available
      )
    }
  }
  
  issues <- list()
  if (length(missing_in_frame) > 0) {
    issues$missing_in_frame <- missing_in_frame
  }
  if (length(missing_in_table) > 0) {
    issues$missing_in_table <- missing_in_table
  }
  if (length(insufficient_strata) > 0) {
    issues$insufficient_units <- insufficient_strata
  }
  
  if (length(issues) == 0) {
    return(list(
      valid = TRUE,
      message = "Frame provides adequate coverage for sample table"
    ))
  } else {
    return(list(valid = FALSE, issues = issues))
  }
}

#' Create a synthetic sampling frame for testing
#'
#' @param strata_config List. Configuration for each stratum with name, size, pop_range
#' @param seed Integer. Random seed for reproducibility
#' @return Data frame. A synthetic sampling frame
#' @export
create_synthetic_frame <- function(strata_config, seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  frames_list <- list()
  
  for (config in strata_config) {
    stratum_name <- config$name
    num_units <- config$size
    pop_range <- config$pop_range
    
    # Generate random population sizes
    populations <- round(runif(num_units, 
                              min = pop_range[1], 
                              max = pop_range[2]))
    
    # Create frame for this stratum
    stratum_frame <- data.frame(
      id = paste0(stratum_name, "_", seq_len(num_units)),
      stratum = stratum_name,
      population_size = populations,
      stringsAsFactors = FALSE
    )
    
    frames_list[[length(frames_list) + 1]] <- stratum_frame
  }
  
  # Combine all strata
  frame <- do.call(rbind, frames_list)
  rownames(frame) <- NULL
  
  return(frame)
}

#' Get sampling frame summary
#'
#' @param frame Data frame. The sampling frame
#' @return List with summary statistics
#' @export
summarize_sampling_frame <- function(frame) {
  
  if (!is.data.frame(frame)) {
    stop("Frame must be a data frame")
  }
  
  summary <- list(
    total_units = nrow(frame),
    num_strata = length(unique(frame$stratum)),
    strata_names = unique(frame$stratum)
  )
  
  if ("population_size" %in% names(frame)) {
    summary$total_population <- sum(frame$population_size, na.rm = TRUE)
    summary$mean_population_per_unit <- mean(frame$population_size, na.rm = TRUE)
    summary$median_population_per_unit <- median(frame$population_size, na.rm = TRUE)
  }
  
  # Stratum-level summaries
  stratum_summaries <- list()
  for (stratum in unique(frame$stratum)) {
    stratum_data <- frame[frame$stratum == stratum, ]
    
    stratum_summary <- list(
      num_units = nrow(stratum_data),
      total_population = if ("population_size" %in% names(stratum_data)) {
        sum(stratum_data$population_size, na.rm = TRUE)
      } else {
        NA
      }
    )
    
    stratum_summaries[[stratum]] <- stratum_summary
  }
  
  summary$by_stratum <- stratum_summaries
  
  return(summary)
}

#' Print sampling frame summary
#'
#' @param frame Data frame. The sampling frame
#' @export
print_frame_summary <- function(frame) {
  summary <- summarize_sampling_frame(frame)
  
  cat("Sampling Frame Summary\n")
  cat("======================\n\n")
  
  cat("Total sampling units:", summary$total_units, "\n")
  cat("Number of strata:", summary$num_strata, "\n")
  
  if (!is.null(summary$total_population)) {
    cat("Total population:", summary$total_population, "\n")
    cat("Mean population per unit:", round(summary$mean_population_per_unit, 1), "\n")
    cat("Median population per unit:", round(summary$median_population_per_unit, 1), "\n")
  }
  
  cat("\nBy Stratum:\n")
  for (stratum in names(summary$by_stratum)) {
    st_summary <- summary$by_stratum[[stratum]]
    cat(sprintf("  %s: %d units", stratum, st_summary$num_units))
    if (!is.na(st_summary$total_population)) {
      cat(sprintf(", population = %d", st_summary$total_population))
    }
    cat("\n")
  }
}
