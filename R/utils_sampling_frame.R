#' Sampling Frame Validation Functions
#'
#' @description
#' Functions for validating sampling frames before sample drawing.

#' Validate sampling frame structure
#'
#' Validates a sampling frame for use in sample drawing operations.  Uses
#' existing \pkg{phr} validator helpers where possible.
#'
#' Required column: \code{psu} (primary sampling unit identifier).
#'
#' Optional but validated if present:
#' \itemize{
#'   \item \code{inclusion} — logical column marking PSUs for inclusion;
#'     created with all \code{TRUE} values by \code{set_sampling_frame()} if
#'     absent.
#'   \item \code{population_size} — positive numeric sizes.
#'   \item \code{stratum} — strata labels (empty strata are flagged).
#' }
#'
#' @param frame Data frame. The sampling frame to validate.
#' @param required_cols Character vector. Additional required column names
#'   beyond \code{"psu"} (default \code{character(0)}).
#' @return List with \code{valid} (logical), \code{message} (character on
#'   failure), \code{issues} (named list on failure), or \code{summary}
#'   (named list on success).
#' @export
validate_sampling_frame <- function(frame, required_cols = character(0)) {

  issues <- list()

  # ---- Basic data-frame check (reuse phr_validate_dataframe) ----------
  if (!phr_validate_dataframe(frame, soft = TRUE)) {
    return(list(valid = FALSE,
                message = "Frame must be a non-NULL data frame with atomic columns."))
  }

  # ---- Non-empty check ------------------------------------------------
  if (nrow(frame) == 0) {
    return(list(valid = FALSE, message = "Frame is empty."))
  }

  # ---- Required columns (psu is always required) ----------------------
  all_required <- unique(c("psu", required_cols))
  missing_cols <- setdiff(all_required, names(frame))
  if (length(missing_cols) > 0) {
    issues$missing_columns <- missing_cols
  }

  # ---- PSU column checks ----------------------------------------------
  if ("psu" %in% names(frame)) {
    if (any(duplicated(frame$psu))) {
      issues$duplicate_psu <- frame$psu[duplicated(frame$psu)]
    }
    if (any(is.na(frame$psu))) {
      issues$missing_psu_values <- sum(is.na(frame$psu))
    }
  }

  # ---- inclusion column -----------------------------------------------
  if ("inclusion" %in% names(frame)) {
    if (!is.logical(frame$inclusion)) {
      issues$invalid_inclusion <- "The 'inclusion' column must be logical (TRUE/FALSE)."
    }
  } else {
    issues$missing_inclusion <- paste(
      "Frame does not have an 'inclusion' column.",
      "set_sampling_frame() will add it with all TRUE values."
    )
  }

  # ---- population_size checks -----------------------------------------
  if ("population_size" %in% names(frame)) {
    if (any(frame$population_size <= 0, na.rm = TRUE)) {
      issues$invalid_population_size <- sum(frame$population_size <= 0, na.rm = TRUE)
    }
  }

  # ---- Stratum checks -------------------------------------------------
  if ("stratum" %in% names(frame)) {
    strata_counts <- table(frame$stratum)
    empty_strata <- names(strata_counts[strata_counts == 0])
    if (length(empty_strata) > 0) {
      issues$empty_strata <- empty_strata
    }
  }

  # ---- Return result --------------------------------------------------
  # issues only about missing inclusion do not make the frame invalid
  hard_issues <- issues[setdiff(names(issues), "missing_inclusion")]

  if (length(hard_issues) == 0) {
    n_included <- if ("inclusion" %in% names(frame)) sum(frame$inclusion, na.rm = TRUE) else nrow(frame)

    return(list(
      valid = TRUE,
      message = "Sampling frame is valid.",
      issues  = if (length(issues) > 0) issues else NULL,
      summary = list(
        num_units      = nrow(frame),
        num_included   = n_included,
        num_strata     = if ("stratum" %in% names(frame)) length(unique(frame$stratum)) else 1L,
        total_population = if ("population_size" %in% names(frame)) {
          sum(frame$population_size, na.rm = TRUE)
        } else {
          NA_real_
        }
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
      psu             = paste0(stratum_name, "_", seq_len(num_units)),
      stratum         = stratum_name,
      population_size = populations,
      inclusion       = TRUE,
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
