#' Sample Size Calculation Functions
#'
#' @description
#' Three main functions for calculating sample sizes for humanitarian assessments.

#' Calculate sample size for general indicators
#'
#' @param expected_proportion Numeric. Expected proportion/prevalence in percentages (0-100)
#' @param desired_precision Numeric. Desired precision in +/- percentages (0-100)
#' @param non_response_rate Numeric. Non-response rate in percentages (0-100, default = 5)
#' @param design Character. "simple_random" or "cluster" (default = "simple_random")
#' @param design_effect Numeric. Design effect (required if design = "cluster", default = 1.5)
#' @param fpc Logical. Apply finite population correction (default = FALSE)
#' @param total_population Numeric. Total population size (required if fpc = TRUE)
#' @param confidence_level Numeric. Confidence level (default = 0.95)
#' @return Numeric. Estimated sample size
#' @export
calculate_sample_size_general <- function(expected_proportion,
                                         desired_precision,
                                         non_response_rate = 5,
                                         design = "simple_random",
                                         design_effect = 1.5,
                                         fpc = FALSE,
                                         total_population = NULL,
                                         number_clusters = NULL,
                                         confidence_level = 0.95) {

  # Validate inputs
  if (expected_proportion < 0 || expected_proportion > 100) {
    stop("expected_proportion must be between 0 and 100")
  }
  if (desired_precision <= 0 || desired_precision >= 100) {
    stop("desired_precision must be between 0 and 100")
  }
  if (non_response_rate < 0 || non_response_rate >= 100) {
    stop("non_response_rate must be between 0 and 100")
  }
  if (!design %in% c("simple_random", "cluster")) {
    stop("design must be 'simple_random' or 'cluster'")
  }
  if (design == "cluster" && design_effect < 1) {
    stop("design_effect must be >= 1 for cluster designs")
  }
  if (fpc && (is.null(total_population) || total_population <= 0)) {
    stop("total_population must be provided and positive when fpc = TRUE")
  }
  if (design == "cluster" && (is.null(number_clusters) || number_clusters <= 0)) {
    # Per SMART survey guidance with higher number of clusters 25+
    t <- 2.045
  }
  if (design == "cluster" && (!is.null(number_clusters) || number_clusters > 0)) {
    t <- stats::qt((1 + confidence_level) / 2, df = number_clusters - 1)
  }

  # Convert percentages to proportions
  p <- expected_proportion / 100
  d <- desired_precision / 100

  # Calculate z-score
  z <- qnorm((1 + confidence_level) / 2)

  # Calculate base sample size


  if (design == "simple_random") {

    n0 <- (z^2 * p * (1 - p)) / d^2

    if (fpc == TRUE) {
      n <- n0 / (1 + (n0 - 1) / total_population)
    } else {
      n <- n0
    }


  } else if (design == "cluster") {

    n0 <- ((t^2 * p * (1 - p)) / d^2)**design_effect

    if (fpc == TRUE) {

      n <- n0 / (1 + (n0 - 1) / total_population)

    } else {

      n <- n0

    }

  } else {

    phr_error("Invalid design type. Must be 'simple_random' or 'cluster'.")

  }

  # Adjust for non-response
  n_adjusted <- n / (1 - non_response_rate / 100)

  return(ceiling(n_adjusted))
}

#' Calculate sample size for individual-level indicators
#'
#' @param expected_proportion Numeric. Expected proportion/prevalence in percentages (0-100)
#' @param desired_precision Numeric. Desired precision in +/- percentages (0-100)
#' @param non_response_rate Numeric. Non-response rate in percentages (0-100, default = 5)
#' @param design Character. "simple_random" or "cluster" (default = "simple_random")
#' @param design_effect Numeric. Design effect (required if design = "cluster", default = 1.5)
#' @param fpc Logical. Apply finite population correction (default = FALSE)
#' @param total_population Numeric. Total population size (required if fpc = TRUE)
#' @param average_household_size Numeric. Average household size (required)
#' @param sub_population_percent Numeric. Percentage of sub-population (0-100, default = 100)
#' @param confidence_level Numeric. Confidence level (default = 0.95)
#' @return List with sample_size_individuals and sample_size_households
#' @export
calculate_sample_size_individual_household <- function(expected_proportion,
                                            desired_precision,
                                            non_response_rate = 5,
                                            design = "simple_random",
                                            design_effect = 1.5,
                                            fpc = FALSE,
                                            total_population = NULL,
                                            num_clusters = NULL,
                                            average_household_size,
                                            sub_population_percent = 100,
                                            confidence_level = 0.95) {

  # Validate additional inputs
  if (missing(average_household_size) || average_household_size <= 0) {
    stop("average_household_size must be provided and positive")
  }
  if (sub_population_percent <= 0 || sub_population_percent > 100) {
    stop("sub_population_percent must be between 0 and 100")
  }

  # Calculate base sample size using general function
  n_individuals <- calculate_sample_size_general(
    expected_proportion = expected_proportion,
    desired_precision = desired_precision,
    non_response_rate = non_response_rate,
    design = design,
    design_effect = design_effect,
    num_clusters = num_clusters,
    fpc = fpc,
    total_population = total_population,
    confidence_level = confidence_level
  )

  # Adjust for sub-population percentage
  n_individuals <- ceiling(n_individuals / (sub_population_percent / 100))

  # Calculate households needed
  n_households <- ceiling(n_individuals / average_household_size)

  return(list(
    sample_size_individuals = n_individuals,
    sample_size_households = n_households
  ))
}

#' Calculate sample size for mortality surveys
#'
#' @param expected_death_rate Numeric. Expected death rate in deaths per 10,000 people per day
#' @param desired_precision Numeric. Desired precision in deaths per 10,000 people per day
#' @param non_response_rate Numeric. Non-response rate in percentages (0-100, default = 5)
#' @param design Character. "simple_random" or "cluster" (default = "cluster")
#' @param design_effect Numeric. Design effect (required if design = "cluster", default = 1.5)
#' @param recall_days Numeric. Number of recall days (default = 90)
#' @param average_household_size Numeric. Average household size (required)
#' @param fpc Logical. Apply finite population correction (default = FALSE)
#' @param total_population Numeric. Total population size (required if fpc = TRUE)
#' @param confidence_level Numeric. Confidence level (default = 0.95)
#' @return List with sample_size_households, sample_size_individuals, and sample_size_person_time
#' @export
calculate_sample_size_mortality <- function(expected_death_rate,
                                           desired_precision,
                                           non_response_rate = 5,
                                           design = "cluster",
                                           design_effect = 1.5,
                                           number_clusters = NULL,
                                           recall_days = 90,
                                           average_household_size,
                                           fpc = FALSE,
                                           total_population = NULL,
                                           confidence_level = 0.95) {

  # Validate inputs
  if (expected_death_rate <= 0) {
    stop("expected_death_rate must be positive")
  }
  if (desired_precision <= 0) {
    stop("desired_precision must be positive")
  }
  if (missing(average_household_size) || average_household_size <= 0) {
    stop("average_household_size must be provided and positive")
  }
  if (recall_days <= 0) {
    stop("recall_days must be positive")
  }
  if (!design %in% c("simple_random", "cluster")) {
    stop("design must be 'simple_random' or 'cluster'")
  }
  if (design == "cluster" && design_effect < 1) {
    stop("design_effect must be >= 1 for cluster designs")
  }
  if (design == "cluster" && (is.null(number_clusters) || number_clusters <= 0)) {
    # Per SMART survey guidance with higher number of clusters 25+
    t <- 2.045
  }
  if (design == "cluster" && (!is.null(number_clusters) || number_clusters > 0)) {
    t <- stats::qt((1 + confidence_level) / 2, df = number_clusters - 1)
  }

  # Calculate z-score
  z <- qnorm((1 + confidence_level) / 2)

  r <- expected_death_rate / 10000
  d <- desired_precision / 10000

  if (design == "simple_random") {

    numerator <- (z^2 * r * (1 - r))
    denominator <- (d^2)*recall_period
    n_individuals <- numerator / denominator

    if (fpc == TRUE) {
      n_adj_individuals <- (n_individuals*total_population) / (n_individuals + (total_population - 1))
    } else {
      n_adj_individuals <- n_individuals
    }

  } else if (design == "cluster") {

    numerator <- (z^2 * r * (1 - r))
    denominator <- (d^2)*recall_period
    n_individuals <- (numerator / denominator)*design_effect

    if (fpc == TRUE) {
      n_adj_individuals <- (n_individuals*total_population) / (n_individuals + (total_population - 1))
    } else {
      n_adj_individuals <- n_individuals
    }

  } else {
    phr_error("Invalid design type. Must be 'simple_random' or 'cluster'.")
  }

  n_households <- ceiling(n_adj_individuals / average_household_size) / (1 - non_response_rate / 100)
  n_pt <- ceiling(n_adj_individuals * recall_days)
  n_adj_individuals <- ceiling(n_adj_individuals)

  return(list(
    sample_size_households = n_households,
    sample_size_individuals = n_adj_individuals,
    sample_size_person_time = n_pt,
  ))
}

#' Estimate field work plan for data collection
#'
#' @param number_of_teams Integer. Number of data collection teams
#' @param enumerators_per_team Integer. Number of enumerators per team
#' @param start_time Character or POSIXct. Start date/time of data collection
#' @param end_time Character or POSIXct. End date/time of data collection
#' @param average_interview_time Numeric. Average interview time in minutes
#' @param average_travel_time Numeric. Average travel time between clusters in minutes
#' @param clusters_per_day_per_team Numeric. Number of clusters each team can complete per day
#' @param total_sample_size Integer. Total sample size needed (optional)
#' @return List with num_interview_per_enum_per_day, num_days, and if cluster design num_psu_needed and psu_size
#' @export
estimate_field_plan <- function(sample_design,
                                number_of_teams,
                                enumerators_per_team,
                                number_of_psu_per_team_per_day = NULL,
                                start_time,
                                end_time,
                                average_interview_time,
                                average_travel_time,
                                average_rest_time,
                                total_sample_size = NULL) {

  # Validate inputs
  if (number_of_teams <= 0) {
    phr_error("number_of_teams must be positive")
  }
  if (enumerators_per_team <= 0) {
    phr_error("enumerators_per_team must be positive")
  }
  if (average_interview_time <= 0) {
    phr_error("average_interview_time must be positive")
  }
  if (average_travel_time < 0) {
    phr_error("average_travel_time must be non-negative")
  }
  if (!(sample_design %in% c("simple_random", "cluster"))) {
    phr_error("sample_design must be 'simple_random' or 'cluster'")
  }

  if (sample_design == "cluster") {

    if (number_of_psu_per_team_per_day <= 0) {
      phr_error("number_of_psu_per_team_per_day must be positive")
    }

  }

  # Convert times to dates if needed
  if (is.character(start_time)) {
    start_time <- as.Date(start_time)
  }
  if (is.character(end_time)) {
    end_time <- as.Date(end_time)
  }

  # Calculate available days
  total_working_minutes <- as.numeric(difftime(end_time, start_time, units = "mins"))
  total_working_hours <- effective_working_minutes / 60

  if (total_working_minutes <= 0) {
    phr_error("end_time must be after start_time")
  }

  effective_working_time <- total_working_minutes - average_rest_time - average_travel_time
  interviews_per_enumerator_per_day <- floor((effective_working_time) / (average_interview_time))

  if (sample_design == "simple_random") {

    number_days_needed <- (total_sample_size*average_interview_time) / (effective_working_time*number_of_teams*enumerators_per_team)
    number_psu_needed <- NA

    return(list(
      num_interview_per_enum_per_day = interviews_per_enumerator_per_day,
      num_days = number_days_needed,
      num_psu_needed = NA,
      psu_size = NA
      )
    )

  } else if (sample_design == "cluster") {

    psu_size <- (interviews_per_enumerator_per_day*enumerators_per_team)/number_of_psu_per_team_per_day
    number_psu_needed <- ceiling(total_sample_size / psu_size)
    number_days_needed <- ceiling(number_psu_needed / (number_of_psu_per_team_per_day*number_teams))

    return(list(
      num_interview_per_enum_per_day = interviews_per_enumerator_per_day,
      num_days = number_days_needed,
      num_psu_needed = number_psu_needed,
      psu_size = psu_size
      )
    )
  }

}
