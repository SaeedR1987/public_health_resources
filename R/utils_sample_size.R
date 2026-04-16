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
  
  # Convert percentages to proportions
  p <- expected_proportion / 100
  d <- desired_precision / 100
  
  # Calculate z-score
  z <- qnorm((1 + confidence_level) / 2)
  
  # Calculate base sample size
  n <- (z^2 * p * (1 - p)) / d^2
  
  # Apply design effect if cluster design
  if (design == "cluster") {
    n <- n * design_effect
  }
  
  # Apply finite population correction if requested
  if (fpc && !is.null(total_population)) {
    n <- n / (1 + (n - 1) / total_population)
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
calculate_sample_size_individual <- function(expected_proportion,
                                            desired_precision,
                                            non_response_rate = 5,
                                            design = "simple_random",
                                            design_effect = 1.5,
                                            fpc = FALSE,
                                            total_population = NULL,
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
    fpc = fpc,
    total_population = total_population,
    confidence_level = confidence_level
  )
  
  # Adjust for sub-population percentage
  if (sub_population_percent < 100) {
    n_individuals <- ceiling(n_individuals / (sub_population_percent / 100))
  }
  
  # Calculate households needed
  n_households <- ceiling(n_individuals / average_household_size)
  
  return(list(
    sample_size_individuals = n_individuals,
    sample_size_households = n_households,
    average_household_size = average_household_size,
    sub_population_percent = sub_population_percent
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
  
  # Calculate z-score
  z <- qnorm((1 + confidence_level) / 2)
  
  # Calculate deaths expected during recall period per 10,000
  deaths_per_10000 <- expected_death_rate * recall_days
  
  # Calculate sample size in persons
  n_persons <- (z^2 * deaths_per_10000) / (desired_precision^2)
  
  # Apply design effect if cluster design
  if (design == "cluster") {
    n_persons <- n_persons * design_effect
  }
  
  # Apply finite population correction if requested
  if (fpc && !is.null(total_population)) {
    n_persons <- n_persons / (1 + (n_persons - 1) / total_population)
  }
  
  # Adjust for non-response
  n_persons_adjusted <- n_persons / (1 - non_response_rate / 100)
  
  # Calculate households
  n_households <- ceiling(n_persons_adjusted / average_household_size)
  
  # Calculate person-time (persons * days)
  n_person_time <- ceiling(n_persons_adjusted * recall_days)
  
  return(list(
    sample_size_households = n_households,
    sample_size_individuals = ceiling(n_persons_adjusted),
    sample_size_person_time = n_person_time,
    expected_deaths = ceiling(deaths_per_10000 * n_persons_adjusted / 10000),
    recall_days = recall_days,
    design_effect = if (design == "cluster") design_effect else 1
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
#' @return List with estimated_days, recommended_cluster_size, recommended_clusters
#' @export
estimate_field_plan <- function(number_of_teams,
                                enumerators_per_team,
                                start_time,
                                end_time,
                                average_interview_time,
                                average_travel_time,
                                clusters_per_day_per_team,
                                total_sample_size = NULL) {
  
  # Validate inputs
  if (number_of_teams <= 0) {
    stop("number_of_teams must be positive")
  }
  if (enumerators_per_team <= 0) {
    stop("enumerators_per_team must be positive")
  }
  if (average_interview_time <= 0) {
    stop("average_interview_time must be positive")
  }
  if (average_travel_time < 0) {
    stop("average_travel_time must be non-negative")
  }
  if (clusters_per_day_per_team <= 0) {
    stop("clusters_per_day_per_team must be positive")
  }
  
  # Convert times to dates if needed
  if (is.character(start_time)) {
    start_time <- as.Date(start_time)
  }
  if (is.character(end_time)) {
    end_time <- as.Date(end_time)
  }
  
  # Calculate available days
  available_days <- as.numeric(difftime(end_time, start_time, units = "days"))
  
  if (available_days <= 0) {
    stop("end_time must be after start_time")
  }
  
  # Calculate working hours per day (assume 8 hours = 480 minutes)
  working_minutes_per_day <- 480
  
  # Calculate time per cluster (including travel)
  time_per_cluster <- average_travel_time
  
  # If we have sample size, calculate recommended cluster size
  recommended_cluster_size <- NULL
  if (!is.null(total_sample_size)) {
    # Calculate how many interviews can fit in a day per enumerator
    interviews_per_enumerator_per_day <- floor(
      (working_minutes_per_day - average_travel_time * clusters_per_day_per_team) / 
      (average_interview_time * clusters_per_day_per_team)
    )
    
    # Recommended cluster size per team
    recommended_cluster_size <- interviews_per_enumerator_per_day * enumerators_per_team
    
    # Calculate number of clusters needed
    recommended_clusters <- ceiling(total_sample_size / recommended_cluster_size)
    
    # Calculate days needed
    total_clusters_per_day <- number_of_teams * clusters_per_day_per_team
    estimated_days <- ceiling(recommended_clusters / total_clusters_per_day)
    
  } else {
    # Just estimate based on available time
    total_clusters_possible <- available_days * number_of_teams * clusters_per_day_per_team
    estimated_days <- available_days
    recommended_clusters <- total_clusters_possible
  }
  
  return(list(
    estimated_days = estimated_days,
    available_days = available_days,
    recommended_cluster_size = recommended_cluster_size,
    recommended_clusters = recommended_clusters,
    total_teams = number_of_teams,
    enumerators_per_team = enumerators_per_team
  ))
}
