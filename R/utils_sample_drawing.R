#' Sample Drawing Functions
#'
#' @description
#' Functions for drawing samples from sampling frames using various methods.

#' Draw simple random sample
#'
#' @param frame Data frame. The sampling frame
#' @param n Integer. Sample size to draw
#' @param seed Integer. Random seed for reproducibility
#' @return Data frame with drawn sample and metadata attributes
#' @export
draw_sample_srs <- function(frame, n, seed = NULL) {
  
  if (!is.data.frame(frame)) {
    stop("Frame must be a data frame")
  }
  
  if (n > nrow(frame)) {
    warning(paste("Sample size", n, "exceeds frame size", nrow(frame), 
                 ". Drawing all available units."))
    n <- nrow(frame)
  }
  
  # Set seed for reproducibility
  if (is.null(seed)) {
    seed <- as.integer(Sys.time())
  }
  set.seed(seed)
  
  # Draw sample
  sample_idx <- sample(seq_len(nrow(frame)), n, replace = FALSE)
  sample_data <- frame[sample_idx, ]
  
  # Add metadata as attributes
  attr(sample_data, "sampling_method") <- "simple_random"
  attr(sample_data, "seed") <- seed
  attr(sample_data, "n_planned") <- n
  attr(sample_data, "n_drawn") <- nrow(sample_data)
  attr(sample_data, "date_drawn") <- Sys.time()
  
  return(sample_data)
}

#' Draw probability proportional to size (PPS) sample
#'
#' @param frame Data frame. The sampling frame with population_size column
#' @param n Integer. Sample size to draw
#' @param seed Integer. Random seed for reproducibility
#' @return Data frame with drawn sample and metadata attributes
#' @export
draw_sample_pps <- function(frame, n, seed = NULL) {
  
  if (!is.data.frame(frame)) {
    stop("Frame must be a data frame")
  }
  
  if (!"population_size" %in% names(frame)) {
    stop("Frame must have a 'population_size' column for PPS sampling")
  }
  
  if (n > nrow(frame)) {
    warning(paste("Sample size", n, "exceeds frame size", nrow(frame), 
                 ". Drawing all available units."))
    n <- nrow(frame)
  }
  
  # Set seed
  if (is.null(seed)) {
    seed <- as.integer(Sys.time())
  }
  set.seed(seed)
  
  # Calculate sampling probabilities
  total_pop <- sum(frame$population_size, na.rm = TRUE)
  probs <- frame$population_size / total_pop
  
  # Draw sample
  sample_idx <- sample(seq_len(nrow(frame)), n, replace = FALSE, prob = probs)
  sample_data <- frame[sample_idx, ]
  
  # Add selection probabilities
  sample_data$selection_probability <- probs[sample_idx]
  sample_data$sampling_weight <- 1 / sample_data$selection_probability
  
  # Add metadata
  attr(sample_data, "sampling_method") <- "pps"
  attr(sample_data, "seed") <- seed
  attr(sample_data, "n_planned") <- n
  attr(sample_data, "n_drawn") <- nrow(sample_data)
  attr(sample_data, "date_drawn") <- Sys.time()
  
  return(sample_data)
}

#' Draw systematic sample
#'
#' @param frame Data frame. The sampling frame
#' @param n Integer. Sample size to draw
#' @param seed Integer. Random seed for reproducibility
#' @return Data frame with drawn sample and metadata attributes
#' @export
draw_sample_systematic <- function(frame, n, seed = NULL) {
  
  if (!is.data.frame(frame)) {
    stop("Frame must be a data frame")
  }
  
  if (n > nrow(frame)) {
    warning(paste("Sample size", n, "exceeds frame size", nrow(frame), 
                 ". Drawing all available units."))
    n <- nrow(frame)
  }
  
  # Set seed
  if (is.null(seed)) {
    seed <- as.integer(Sys.time())
  }
  set.seed(seed)
  
  # Calculate sampling interval
  interval <- floor(nrow(frame) / n)
  if (interval < 1) interval <- 1
  
  # Random start
  start <- sample(seq_len(interval), 1)
  
  # Generate systematic indices
  sample_idx <- seq(start, nrow(frame), by = interval)[1:n]
  sample_data <- frame[sample_idx, ]
  
  # Add metadata
  attr(sample_data, "sampling_method") <- "systematic"
  attr(sample_data, "seed") <- seed
  attr(sample_data, "interval") <- interval
  attr(sample_data, "start") <- start
  attr(sample_data, "n_planned") <- n
  attr(sample_data, "n_drawn") <- nrow(sample_data)
  attr(sample_data, "date_drawn") <- Sys.time()
  
  return(sample_data)
}

#' Draw stratified sample
#'
#' @param frame Data frame. The sampling frame with stratum column
#' @param sample_table Data frame. Sample table with stratum_id and sample_size
#' @param method Character. Sampling method within strata: "srs", "pps", "systematic"
#' @param seed Integer. Random seed for reproducibility
#' @return List with drawn samples by stratum and combined sample
#' @export
draw_sample_stratified <- function(frame, sample_table, method = "srs", seed = NULL) {
  
  if (!is.data.frame(frame) || !is.data.frame(sample_table)) {
    stop("Both frame and sample_table must be data frames")
  }
  
  if (!"stratum" %in% names(frame)) {
    stop("Frame must have a 'stratum' column")
  }
  
  if (!all(c("stratum_id", "sample_size") %in% names(sample_table))) {
    stop("sample_table must have 'stratum_id' and 'sample_size' columns")
  }
  
  # Set seed
  if (is.null(seed)) {
    seed <- as.integer(Sys.time())
  }
  
  # Draw sample for each stratum
  samples_by_stratum <- list()
  
  for (i in seq_len(nrow(sample_table))) {
    stratum_id <- sample_table$stratum_id[i]
    n_needed <- sample_table$sample_size[i]
    
    # Get stratum frame
    stratum_frame <- frame[frame$stratum == stratum_id, ]
    
    if (nrow(stratum_frame) == 0) {
      warning(paste("No units found in frame for stratum:", stratum_id))
      next
    }
    
    # Draw sample based on method
    if (method == "srs") {
      stratum_sample <- draw_sample_srs(stratum_frame, n_needed, seed = seed + i)
    } else if (method == "pps") {
      stratum_sample <- draw_sample_pps(stratum_frame, n_needed, seed = seed + i)
    } else if (method == "systematic") {
      stratum_sample <- draw_sample_systematic(stratum_frame, n_needed, seed = seed + i)
    } else {
      stop(paste("Unknown sampling method:", method))
    }
    
    samples_by_stratum[[stratum_id]] <- stratum_sample
  }
  
  # Combine all strata
  if (length(samples_by_stratum) > 0) {
    combined_sample <- do.call(rbind, samples_by_stratum)
    rownames(combined_sample) <- NULL
    
    # Add stratified sampling metadata
    attr(combined_sample, "sampling_design") <- "stratified"
    attr(combined_sample, "within_stratum_method") <- method
    attr(combined_sample, "base_seed") <- seed
  } else {
    combined_sample <- NULL
  }
  
  return(list(
    by_stratum = samples_by_stratum,
    combined = combined_sample,
    method = method,
    seed = seed
  ))
}

#' Allocate households to selected clusters
#'
#' @param sample Data frame. Drawn cluster sample
#' @param households_per_cluster Integer. Number of households to allocate per cluster
#' @param method Character. Allocation method: "equal", "pps"
#' @return Data frame with household allocations
#' @export
allocate_households <- function(sample, households_per_cluster, method = "equal") {
  
  if (!is.data.frame(sample)) {
    stop("Sample must be a data frame")
  }
  
  if (method == "equal") {
    # Equal allocation
    sample$households_allocated <- households_per_cluster
    
  } else if (method == "pps") {
    # Proportional to population size
    if (!"population_size" %in% names(sample)) {
      stop("Sample must have 'population_size' column for PPS allocation")
    }
    
    total_pop <- sum(sample$population_size, na.rm = TRUE)
    total_households <- households_per_cluster * nrow(sample)
    
    # Allocate proportionally, with minimum of 1 per cluster
    sample$households_allocated <- pmax(
      round((sample$population_size / total_pop) * total_households),
      1
    )
    
    # Adjust to match total exactly
    diff <- total_households - sum(sample$households_allocated)
    if (diff != 0) {
      largest_idx <- which.max(sample$population_size)
      sample$households_allocated[largest_idx] <- 
        sample$households_allocated[largest_idx] + diff
    }
    
  } else {
    stop("method must be 'equal' or 'pps'")
  }
  
  return(sample)
}

#' Generate replacement samples
#'
#' @param frame Data frame. The sampling frame
#' @param drawn_sample Data frame. Already drawn sample
#' @param n_replacements Integer. Number of replacements per selected unit
#' @param method Character. Sampling method: "srs", "pps"
#' @param seed Integer. Random seed
#' @return Data frame with replacement samples
#' @export
generate_replacements <- function(frame, drawn_sample, n_replacements = 2, 
                                 method = "srs", seed = NULL) {
  
  if (!is.data.frame(frame) || !is.data.frame(drawn_sample)) {
    stop("Both frame and drawn_sample must be data frames")
  }
  
  if (!"id" %in% names(frame) || !"id" %in% names(drawn_sample)) {
    stop("Both frame and drawn_sample must have an 'id' column")
  }
  
  # Set seed
  if (is.null(seed)) {
    seed <- as.integer(Sys.time())
  }
  set.seed(seed)
  
  # Exclude already drawn samples from frame
  remaining_frame <- frame[!frame$id %in% drawn_sample$id, ]
  
  if (nrow(remaining_frame) == 0) {
    warning("No units remaining in frame for replacements")
    return(NULL)
  }
  
  # Draw replacements
  n_needed <- nrow(drawn_sample) * n_replacements
  
  if (method == "srs") {
    replacements <- draw_sample_srs(remaining_frame, 
                                   min(n_needed, nrow(remaining_frame)), 
                                   seed = seed)
  } else if (method == "pps") {
    replacements <- draw_sample_pps(remaining_frame, 
                                   min(n_needed, nrow(remaining_frame)), 
                                   seed = seed)
  } else {
    stop("method must be 'srs' or 'pps'")
  }
  
  # Add replacement metadata
  replacements$replacement_for <- rep(drawn_sample$id, each = n_replacements)[1:nrow(replacements)]
  replacements$replacement_order <- rep(seq_len(n_replacements), 
                                       length.out = nrow(replacements))
  
  return(replacements)
}

#' Get sampling metadata from drawn sample
#'
#' @param sample Data frame. Drawn sample with metadata attributes
#' @return List with sampling metadata
#' @export
get_sample_metadata <- function(sample) {
  
  metadata <- list(
    sampling_method = attr(sample, "sampling_method"),
    seed = attr(sample, "seed"),
    n_planned = attr(sample, "n_planned"),
    n_drawn = attr(sample, "n_drawn"),
    date_drawn = attr(sample, "date_drawn")
  )
  
  # Add method-specific metadata
  if (!is.null(attr(sample, "sampling_design"))) {
    metadata$sampling_design <- attr(sample, "sampling_design")
    metadata$within_stratum_method <- attr(sample, "within_stratum_method")
  }
  
  if (!is.null(attr(sample, "interval"))) {
    metadata$interval <- attr(sample, "interval")
    metadata$start <- attr(sample, "start")
  }
  
  return(metadata)
}
