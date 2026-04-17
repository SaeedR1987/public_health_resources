#' Sample Drawing Functions
#'
#' @description
#' Functions for drawing samples from sampling frames using various methods.
#' PSU-level sampling functions (prefixed \code{sample_psu_}) operate on an
#' already-filtered frame of eligible primary sampling units and return that
#' frame augmented with two new columns:
#' \itemize{
#'   \item \code{sampled_psu} — sequential cluster number for selected PSUs;
#'     \code{NA} for unselected PSUs.
#'   \item \code{allocated_sample} — number of households allocated to that
#'     PSU; \code{NA} for unselected PSUs.
#' }

# ---------------------------------------------------------------------------
# PSU-level sampling utility functions (called from Protocol$draw_sample)
# ---------------------------------------------------------------------------

#' Draw PSUs using simple random sampling (SRS)
#'
#' Randomly selects \code{n_psu} primary sampling units with equal probability
#' (without replacement) and divides \code{sample_size} households evenly
#' across selected PSUs.  Population sizes are not required.
#'
#' @param frame Data frame. Eligible PSUs (already filtered for inclusion).
#' @param n_psu Integer. Number of PSUs to select.
#' @param sample_size Integer. Total household sample size to allocate.
#' @param seed Integer. Random seed for reproducibility (default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added.
#' @export
sample_psu_srs <- function(frame, n_psu, sample_size, seed = 42) {
  if (!is.data.frame(frame)) stop("frame must be a data frame")
  if (n_psu <= 0) stop("n_psu must be a positive integer")
  if (sample_size <= 0) stop("sample_size must be positive")

  set.seed(seed)
  n_available <- nrow(frame)
  if (n_psu > n_available) {
    warning(paste0("n_psu (", n_psu, ") exceeds available PSUs (", n_available,
                   "). Using all available PSUs."))
    n_psu <- n_available
  }

  selected_idx <- sample(seq_len(n_available), n_psu, replace = FALSE)

  frame$sampled_psu      <- NA_integer_
  frame$allocated_sample <- NA_real_

  frame$sampled_psu[selected_idx]      <- seq_len(n_psu)
  per_psu   <- floor(sample_size / n_psu)
  remainder <- sample_size - per_psu * n_psu
  frame$allocated_sample[selected_idx] <- per_psu
  if (remainder > 0) {
    frame$allocated_sample[selected_idx[1]] <-
      frame$allocated_sample[selected_idx[1]] + remainder
  }

  frame
}

#' Allocate households proportional to population size across all PSUs
#'
#' All eligible PSUs are included; household sample is allocated proportionally
#' to each PSU's population size.  Requires a \code{population_size} column.
#'
#' @param frame Data frame. Eligible PSUs with a \code{population_size} column.
#' @param sample_size Integer. Total household sample size to allocate.
#' @param seed Integer. Random seed (not used in allocation itself but kept for
#'   interface consistency; default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added.
#' @export
sample_psu_proportional <- function(frame, sample_size, seed = 42) {
  if (!is.data.frame(frame)) stop("frame must be a data frame")
  if (!"population_size" %in% names(frame)) {
    stop("Frame must have a 'population_size' column for proportional method.")
  }
  if (sample_size <= 0) stop("sample_size must be positive")

  total_pop <- sum(frame$population_size, na.rm = TRUE)
  if (total_pop == 0) stop("Total population cannot be zero for proportional method.")

  frame$sampled_psu      <- seq_len(nrow(frame))
  frame$allocated_sample <- round(frame$population_size / total_pop * sample_size)
  frame$allocated_sample <- pmax(frame$allocated_sample, 1L)

  # Adjust to match total exactly
  diff_val <- sample_size - sum(frame$allocated_sample)
  if (diff_val != 0) {
    largest_idx <- which.max(frame$population_size)
    frame$allocated_sample[largest_idx] <- frame$allocated_sample[largest_idx] + diff_val
  }

  frame
}

#' Draw PSUs using PPS cluster sampling (with replacement)
#'
#' Selects \code{n_clusters} clusters proportional to population size using PPS
#' with replacement (via \code{pps::ppswr}).  A single PSU may receive multiple
#' clusters.  Requires a \code{population_size} column.
#'
#' @param frame Data frame. Eligible PSUs with a \code{population_size} column.
#' @param n_clusters Integer. Number of clusters to allocate.
#' @param cluster_size Integer. Number of households per cluster.
#' @param seed Integer. Random seed for reproducibility (default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added.  For PSUs receiving multiple clusters,
#'   \code{allocated_sample = n_clusters_at_psu * cluster_size}.
#' @export
sample_psu_pps_cluster <- function(frame, n_clusters, cluster_size, seed = 42) {
  if (!is.data.frame(frame)) stop("frame must be a data frame")
  if (!"population_size" %in% names(frame)) {
    stop("Frame must have a 'population_size' column for pps_cluster method.")
  }
  if (n_clusters <= 0) stop("n_clusters must be a positive integer")
  if (cluster_size <= 0) stop("cluster_size must be a positive integer")

  set.seed(seed)
  sizes    <- frame$population_size
  selected <- pps::ppswr(sizes, n_clusters)

  times_selected <- tabulate(selected, nbins = nrow(frame))

  frame$sampled_psu      <- NA_integer_
  frame$allocated_sample <- NA_real_

  selected_psus <- which(times_selected > 0)
  frame$sampled_psu[selected_psus]      <- seq_along(selected_psus)
  frame$allocated_sample[selected_psus] <- times_selected[selected_psus] * cluster_size

  frame
}

#' Draw PSUs using random location cluster (RLC) sampling
#'
#' Uses PPS with replacement (via \code{pps::ppswr}) and a default cluster size
#' of 3 households.  Requires a \code{population_size} column.
#'
#' @param frame Data frame. Eligible PSUs with a \code{population_size} column.
#' @param sample_size Integer. Total household sample size (used to derive
#'   \code{n_clusters = ceiling(sample_size / cluster_size)}).
#' @param cluster_size Integer. Households per cluster (default \code{3}).
#' @param seed Integer. Random seed for reproducibility (default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added.
#' @export
sample_psu_rlc <- function(frame, sample_size, cluster_size = 3, seed = 42) {
  if (!is.data.frame(frame)) stop("frame must be a data frame")
  if (!"population_size" %in% names(frame)) {
    stop("Frame must have a 'population_size' column for rlc method.")
  }
  if (sample_size <= 0) stop("sample_size must be positive")
  if (cluster_size <= 0) stop("cluster_size must be a positive integer")

  n_clusters <- ceiling(sample_size / cluster_size)

  set.seed(seed)
  sizes    <- frame$population_size
  selected <- pps::ppswr(sizes, n_clusters)

  times_selected <- tabulate(selected, nbins = nrow(frame))

  frame$sampled_psu      <- NA_integer_
  frame$allocated_sample <- NA_real_

  selected_psus <- which(times_selected > 0)
  frame$sampled_psu[selected_psus]      <- seq_along(selected_psus)
  frame$allocated_sample[selected_psus] <- times_selected[selected_psus] * cluster_size

  frame
}

#' Draw PSUs using systematic random sampling
#'
#' Selects \code{n_sites} PSUs systematically (population size is ignored).
#' The sampling interval is \code{n_available / n_sites} and a random start
#' is drawn uniformly from \code{1} to \code{round(interval)}.
#'
#' @param frame Data frame. Eligible PSUs.
#' @param n_sites Integer. Number of sites (PSUs) to select.
#' @param sample_size Integer. Total household sample size to allocate evenly
#'   across selected PSUs.
#' @param seed Integer. Random seed for reproducibility (default \code{42}).
#' @return \code{frame} with \code{sampled_psu} and \code{allocated_sample}
#'   columns added.
#' @export
sample_psu_systematic <- function(frame, n_sites, sample_size, seed = 42) {
  if (!is.data.frame(frame)) stop("frame must be a data frame")
  if (n_sites <= 0) stop("n_sites must be a positive integer")
  if (sample_size <= 0) stop("sample_size must be positive")

  set.seed(seed)
  n_available <- nrow(frame)
  if (n_sites > n_available) {
    warning(paste0("n_sites (", n_sites, ") exceeds available PSUs (", n_available,
                   "). Using all available PSUs."))
    n_sites <- n_available
  }

  interval      <- n_available / n_sites
  random_start  <- sample(seq_len(max(1L, round(interval))), 1)
  sample_idx    <- round(seq(random_start, by = interval, length.out = n_sites))
  sample_idx    <- pmin(sample_idx, n_available)

  frame$sampled_psu      <- NA_integer_
  frame$allocated_sample <- NA_real_

  frame$sampled_psu[sample_idx] <- seq_len(n_sites)
  per_site  <- floor(sample_size / n_sites)
  remainder <- sample_size - per_site * n_sites
  frame$allocated_sample[sample_idx] <- per_site
  if (remainder > 0) {
    frame$allocated_sample[sample_idx[1]] <-
      frame$allocated_sample[sample_idx[1]] + remainder
  }

  frame
}

# ---------------------------------------------------------------------------
# Legacy helpers kept for backward compatibility
# ---------------------------------------------------------------------------

#' Draw simple random sample (legacy helper)
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

  if (is.null(seed)) seed <- as.integer(Sys.time())
  set.seed(seed)

  sample_idx  <- sample(seq_len(nrow(frame)), n, replace = FALSE)
  sample_data <- frame[sample_idx, ]

  attr(sample_data, "sampling_method") <- "simple_random"
  attr(sample_data, "seed")            <- seed
  attr(sample_data, "n_planned")       <- n
  attr(sample_data, "n_drawn")         <- nrow(sample_data)
  attr(sample_data, "date_drawn")      <- Sys.time()

  return(sample_data)
}

#' Draw probability proportional to size (PPS) sample (legacy helper)
#'
#' @param frame Data frame. The sampling frame with population_size column
#' @param n Integer. Sample size to draw
#' @param seed Integer. Random seed for reproducibility
#' @return Data frame with drawn sample and metadata attributes
#' @export
draw_sample_pps <- function(frame, n, seed = NULL) {

  if (!is.data.frame(frame)) stop("Frame must be a data frame")
  if (!"population_size" %in% names(frame)) {
    stop("Frame must have a 'population_size' column for PPS sampling")
  }
  if (n > nrow(frame)) {
    warning(paste("Sample size", n, "exceeds frame size", nrow(frame),
                 ". Drawing all available units."))
    n <- nrow(frame)
  }

  if (is.null(seed)) seed <- as.integer(Sys.time())
  set.seed(seed)

  total_pop  <- sum(frame$population_size, na.rm = TRUE)
  probs      <- frame$population_size / total_pop
  sample_idx <- sample(seq_len(nrow(frame)), n, replace = FALSE, prob = probs)
  sample_data <- frame[sample_idx, ]

  sample_data$selection_probability <- probs[sample_idx]
  sample_data$sampling_weight       <- 1 / sample_data$selection_probability

  attr(sample_data, "sampling_method") <- "pps"
  attr(sample_data, "seed")            <- seed
  attr(sample_data, "n_planned")       <- n
  attr(sample_data, "n_drawn")         <- nrow(sample_data)
  attr(sample_data, "date_drawn")      <- Sys.time()

  return(sample_data)
}

#' Draw systematic sample (legacy helper)
#'
#' @param frame Data frame. The sampling frame
#' @param n Integer. Sample size to draw
#' @param seed Integer. Random seed for reproducibility
#' @return Data frame with drawn sample and metadata attributes
#' @export
draw_sample_systematic <- function(frame, n, seed = NULL) {

  if (!is.data.frame(frame)) stop("Frame must be a data frame")
  if (n > nrow(frame)) {
    warning(paste("Sample size", n, "exceeds frame size", nrow(frame),
                 ". Drawing all available units."))
    n <- nrow(frame)
  }

  if (is.null(seed)) seed <- as.integer(Sys.time())
  set.seed(seed)

  interval   <- floor(nrow(frame) / n)
  if (interval < 1) interval <- 1
  start      <- sample(seq_len(interval), 1)
  sample_idx <- seq(start, nrow(frame), by = interval)[seq_len(n)]
  sample_data <- frame[sample_idx, ]

  attr(sample_data, "sampling_method") <- "systematic"
  attr(sample_data, "seed")            <- seed
  attr(sample_data, "interval")        <- interval
  attr(sample_data, "start")           <- start
  attr(sample_data, "n_planned")       <- n
  attr(sample_data, "n_drawn")         <- nrow(sample_data)
  attr(sample_data, "date_drawn")      <- Sys.time()

  return(sample_data)
}

#' Draw stratified sample (legacy helper)
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
  if (!"stratum" %in% names(frame)) stop("Frame must have a 'stratum' column")
  if (!all(c("stratum_id", "sample_size") %in% names(sample_table))) {
    stop("sample_table must have 'stratum_id' and 'sample_size' columns")
  }

  if (is.null(seed)) seed <- as.integer(Sys.time())

  samples_by_stratum <- list()

  for (i in seq_len(nrow(sample_table))) {
    stratum_id   <- sample_table$stratum_id[i]
    n_needed     <- sample_table$sample_size[i]
    stratum_frame <- frame[frame$stratum == stratum_id, ]

    if (nrow(stratum_frame) == 0) {
      warning(paste("No units found in frame for stratum:", stratum_id))
      next
    }

    stratum_sample <- if (method == "srs") {
      draw_sample_srs(stratum_frame, n_needed, seed = seed + i)
    } else if (method == "pps") {
      draw_sample_pps(stratum_frame, n_needed, seed = seed + i)
    } else if (method == "systematic") {
      draw_sample_systematic(stratum_frame, n_needed, seed = seed + i)
    } else {
      stop(paste("Unknown sampling method:", method))
    }

    samples_by_stratum[[stratum_id]] <- stratum_sample
  }

  if (length(samples_by_stratum) > 0) {
    combined_sample <- do.call(rbind, samples_by_stratum)
    rownames(combined_sample) <- NULL
    attr(combined_sample, "sampling_design")       <- "stratified"
    attr(combined_sample, "within_stratum_method") <- method
    attr(combined_sample, "base_seed")             <- seed
  } else {
    combined_sample <- NULL
  }

  return(list(
    by_stratum = samples_by_stratum,
    combined   = combined_sample,
    method     = method,
    seed       = seed
  ))
}

#' Allocate households to selected clusters (legacy helper)
#'
#' @param sample Data frame. Drawn cluster sample
#' @param households_per_cluster Integer. Number of households to allocate per cluster
#' @param method Character. Allocation method: "equal", "pps"
#' @return Data frame with household allocations
#' @export
allocate_households <- function(sample, households_per_cluster, method = "equal") {

  if (!is.data.frame(sample)) stop("Sample must be a data frame")

  if (method == "equal") {
    sample$households_allocated <- households_per_cluster

  } else if (method == "pps") {
    if (!"population_size" %in% names(sample)) {
      stop("Sample must have 'population_size' column for PPS allocation")
    }
    total_pop       <- sum(sample$population_size, na.rm = TRUE)
    total_households <- households_per_cluster * nrow(sample)
    sample$households_allocated <- pmax(
      round((sample$population_size / total_pop) * total_households), 1
    )
    diff_val <- total_households - sum(sample$households_allocated)
    if (diff_val != 0) {
      largest_idx <- which.max(sample$population_size)
      sample$households_allocated[largest_idx] <-
        sample$households_allocated[largest_idx] + diff_val
    }
  } else {
    stop("method must be 'equal' or 'pps'")
  }

  return(sample)
}

#' Generate replacement samples (legacy helper)
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

  if (is.null(seed)) seed <- as.integer(Sys.time())
  set.seed(seed)

  remaining_frame <- frame[!frame$id %in% drawn_sample$id, ]

  if (nrow(remaining_frame) == 0) {
    warning("No units remaining in frame for replacements")
    return(NULL)
  }

  n_needed <- nrow(drawn_sample) * n_replacements

  replacements <- if (method == "srs") {
    draw_sample_srs(remaining_frame, min(n_needed, nrow(remaining_frame)), seed = seed)
  } else if (method == "pps") {
    draw_sample_pps(remaining_frame, min(n_needed, nrow(remaining_frame)), seed = seed)
  } else {
    stop("method must be 'srs' or 'pps'")
  }

  replacements$replacement_for    <- rep(drawn_sample$id, each = n_replacements)[seq_len(nrow(replacements))]
  replacements$replacement_order  <- rep(seq_len(n_replacements), length.out = nrow(replacements))

  return(replacements)
}

#' Get sampling metadata from drawn sample (legacy helper)
#'
#' @param sample Data frame. Drawn sample with metadata attributes
#' @return List with sampling metadata
#' @export
get_sample_metadata <- function(sample) {

  metadata <- list(
    sampling_method = attr(sample, "sampling_method"),
    seed            = attr(sample, "seed"),
    n_planned       = attr(sample, "n_planned"),
    n_drawn         = attr(sample, "n_drawn"),
    date_drawn      = attr(sample, "date_drawn")
  )

  if (!is.null(attr(sample, "sampling_design"))) {
    metadata$sampling_design       <- attr(sample, "sampling_design")
    metadata$within_stratum_method <- attr(sample, "within_stratum_method")
  }
  if (!is.null(attr(sample, "interval"))) {
    metadata$interval <- attr(sample, "interval")
    metadata$start    <- attr(sample, "start")
  }

  return(metadata)
}


