#' @title Add Health Barrier Indicators
#'
#' @description This function identifies specific categories of health barriers from a dataset's column containing select multiple responses, and it creates binary indicator columns for each barrier type.
#'
#' @details The input column (`health_barriers_col`) should contain multiple responses in a single cell, separated by delimiters (e.g., commas, spaces). The function searches for specific values corresponding to various barrier categories using `grepl` and creates the following binary (0/1) columns:
#' * **health_barrier_any.physical**
#' * **health_barrier_any.financial**
#' * **health_barrier_any.safety**
#' * **health_barrier_any.quality**
#' * **health_barrier_any.healthcare_seeking**
#' * **health_barrier_any.availability**
#' * **health_barrier_any.other**
#' * **health_barrier_any.none**
#'
#' These columns are coded as `1` if the corresponding values are found in the specified record or `0` if not.
#'
#' @param .dataset A data frame or tibble containing the relevant column.
#' @param health_barriers_col A character string indicating the column name containing health barriers.
#' @param physical_access_barriers_val A character vector of values indicating "physical access barriers."
#' @param financial_access_barriers_val A character vector of values indicating "financial access barriers."
#' @param safety_access_barriers_val A character vector of values indicating "safety access barriers."
#' @param quality_barriers_val A character vector of values indicating "quality-related barriers."
#' @param healthcare_seeking_barriers_val A character vector of values indicating "healthcare-seeking behavior barriers."
#' @param availability_barriers_val A character vector of values indicating "availability barriers."
#' @param other_barriers_val A character vector of values indicating "other barriers."
#' @param no_barriers_val A character vector of values indicating "no barriers."
#'
#' @return A data frame with the following new binary columns:
#' * **health_barrier_any.physical**
#' * **health_barrier_any.financial**
#' * **health_barrier_any.safety**
#' * **health_barrier_any.quality**
#' * **health_barrier_any.healthcare_seeking**
#' * **health_barrier_any.availability**
#' * **health_barrier_any.other**
#' * **health_barrier_any.none**
#'
#' @examples
#' df <- data.frame(
#'   health_barriers = c(
#'     "long distance,cost",
#'     "poor quality,unsafe facilities",
#'     "no healthcare available",
#'     "no barriers",
#'     "other"
#'   )
#' )
#'
#' df_result <- add_health_barriers(
#'   .dataset = df,
#'   health_barriers_col = "health_barriers",
#'   physical_access_barriers_val = c("long distance", "transport issues"),
#'   financial_access_barriers_val = c("cost", "expensive"),
#'   safety_access_barriers_val = c("unsafe facilities", "insecurity"),
#'   quality_barriers_val = c("poor quality", "lack of equipment"),
#'   healthcare_seeking_barriers_val = c("refusal", "cultural reasons"),
#'   availability_barriers_val = c("no healthcare available", "services unavailable"),
#'   other_barriers_val = c("other"),
#'   no_barriers_val = c("no barriers")
#' )
#'
#' @export
add_health_barriers <- function(
    .dataset,
    health_barriers_col,
    physical_access_barriers_val,
    financial_access_barriers_val,
    safety_access_barriers_val,
    quality_barriers_val,
    healthcare_seeking_barriers_val,
    availability_barriers_val,
    other_barriers_val,
    no_barriers_val,
    did_not_need_val
) {

  origin <- "add_health_barriers"

  phr_try({

    # Use ensure_value for all *_val parameters
    physical_access_barriers_val <- ensure_value(physical_access_barriers_val, "physical")
    financial_access_barriers_val <- ensure_value(financial_access_barriers_val, "financial")
    safety_access_barriers_val <- ensure_value(safety_access_barriers_val, "safety")
    quality_barriers_val <- ensure_value(quality_barriers_val, "quality")
    healthcare_seeking_barriers_val <- ensure_value(healthcare_seeking_barriers_val, "healthcare_seeking")
    availability_barriers_val <- ensure_value(availability_barriers_val, "availability")
    other_barriers_val <- ensure_value(other_barriers_val, "other")
    no_barriers_val <- ensure_value(no_barriers_val, "none")
    did_not_need_val <- ensure_value(did_not_need_val, "did_not_need")


    # Validate dataset

    phr_validate_dataframe(
      .dataset,
      origin = origin,
      hint = phr_txt("Ensure you pass a valid data frame or tibble to `.dataset`."),
      soft = FALSE
    )

    phr_assert(
      nrow(.dataset) > 0,
      origin = origin,
      phr_txt("Dataset is empty.")
    )


    # Validate health barriers column

    phr_validate_columns(
      .dataset,
      health_barriers_col,
      origin = origin,
      hint = phr_txt("Ensure the health barriers column exists in the dataset."),
      soft = FALSE
    )


    # Overwrite warnings for any existing output columns

    output_columns <- c(
      "health_barrier_any.physical",
      "health_barrier_any.financial",
      "health_barrier_any.safety",
      "health_barrier_any.quality",
      "health_barrier_any.healthcare_seeking",
      "health_barrier_any.availability",
      "health_barrier_any.other",
      "health_barrier_any.none",
      "health_barrier_any.did_not_need"
    )

    for (col in output_columns) {
      if (col %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt(glue::glue("Variable `{col}` already exists and will be overwritten."))
        )
      }
    }


    # Detect barriers using grepl

    .dataset <- .dataset %>%
      dplyr::mutate(
        # Physical access barriers
        health_barrier_any.physical = ifelse(
          grepl(paste(physical_access_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Financial access barriers
        health_barrier_any.financial = ifelse(
          grepl(paste(financial_access_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Safety access barriers
        health_barrier_any.safety = ifelse(
          grepl(paste(safety_access_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Quality barriers
        health_barrier_any.quality = ifelse(
          grepl(paste(quality_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Healthcare-seeking behavior barriers
        health_barrier_any.healthcare_seeking = ifelse(
          grepl(paste(healthcare_seeking_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Availability barriers
        health_barrier_any.availability = ifelse(
          grepl(paste(availability_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Other barriers
        health_barrier_any.other = ifelse(
          grepl(paste(other_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # No barriers
        health_barrier_any.none = ifelse(
          grepl(paste(no_barriers_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        ),

        # Did not need
        health_barrier_any.did_not_need = ifelse(
          grepl(paste(did_not_need_val, collapse = "|"), .data[[health_barriers_col]], ignore.case = TRUE),
          1, 0
        )
      )

    phr_message(
      origin = origin,
      message = phr_txt("Health barrier indicators successfully computed.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure all input columns exist and the health barriers column contains valid, well-formatted data."))
}

#' @title Add Healthcare Access Within One Hour
#'
#' @description This function calculates whether healthcare access is available within one hour based on reported travel time, including both numeric and range-based inputs.
#'
#' @details The function adds a new column, `health_healthcare_access_one_hour`, to the dataset. The determination of healthcare access is based on the following logic:
#' 1. **Numeric Minutes**: If travel time in minutes is available, values below 60 minutes are categorized as `"yes"` and 60 minutes or more as `"no"`.
#' 2. **Range-Based Minutes**: If numeric minutes are unavailable, responses from a range-based travel time column are used. Acceptable responses for less than one-hour travel time and one hour or more travel time must be specified.
#' 3. **Unknown or Unavailable Responses**: If neither numeric nor range-based responses are determinable, the result is marked as `"dont_know"`.
#'
#' @param .dataset A data frame or tibble containing the relevant columns.
#' @param health_care_travel_time_col Column name (character) indicating reported travel time type (e.g., numeric, range, or dont_know).
#' @param num_minutes_val Character value indicating "numeric minutes" in the travel time type column.
#' @param range_val Character value indicating "range-based travel time" in the travel time type column.
#' @param health_care_travel_time_minutes_col Column name (character) for numeric travel time in minutes.
#' @param health_care_travel_time_range_col Column name (character) for range-based travel time.
#' @param less_than_one_hour_range_val Character vector of acceptable values indicating less than one hour of travel time.
#' @param one_hour_or_more_range_val Character vector of acceptable values indicating one hour or more of travel time.
#'
#' @return A data frame with a new column:
#' * **health_healthcare_access_one_hour**:
#'   - `"yes"`: Healthcare access is available within one hour.
#'   - `"no"`: Healthcare access requires one hour or more of travel time.
#'   - `"dont_know"`: Unable to determine travel time.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   health_care_travel_time = c("num_minutes", "num_minutes", "range", "dont_know"),
#'   health_care_travel_time_minutes = c(45, 65, NA, NA),
#'   health_care_travel_time_range = c(NA, NA, "<1_hour", ">=1_hour")
#' )
#'
#' # Call the function
#' df_result <- add_healthcare_access_one_hour(
#'   .dataset = df,
#'   health_care_travel_time_col = "health_care_travel_time",
#'   num_minutes_val = "num_minutes",
#'   range_val = "range",
#'   health_care_travel_time_minutes_col = "health_care_travel_time_minutes",
#'   health_care_travel_time_range_col = "health_care_travel_time_range",
#'   less_than_one_hour_range_val = c("<1_hour"),
#'   one_hour_or_more_range_val = c(">=1_hour")
#' )
#'
#' @export
add_healthcare_access_one_hour <- function(
    .dataset,
    health_care_travel_time_col,
    num_minutes_val,
    range_val,
    health_care_travel_time_minutes_col,
    health_care_travel_time_range_col,
    less_than_one_hour_range_val,
    one_hour_or_more_range_val
) {

  origin <- "add_healthcare_access_one_hour"

  phr_try({

    # Use ensure_value for *_val parameters
    num_minutes_val <- ensure_value(num_minutes_val, "num_minutes")
    range_val <- ensure_value(range_val, "range")
    less_than_one_hour_range_val <- ensure_value(less_than_one_hour_range_val, c("<1_hour", "less_than_1_hour"))
    one_hour_or_more_range_val <- ensure_value(one_hour_or_more_range_val, c(">=1_hour", "1_hour_or_more"))

    # Validate dataset

    phr_validate_dataframe(
      .dataset,
      origin = origin,
      hint = phr_txt("Ensure you pass a valid data frame or tibble to `.dataset`."),
      soft = FALSE
    )

    phr_assert(
      nrow(.dataset) > 0,
      origin = origin,
      phr_txt("Dataset is empty.")
    )

    # Validate input columns

    required_columns <- c(
      health_care_travel_time_col,
      health_care_travel_time_minutes_col,
      health_care_travel_time_range_col
    )
    phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = phr_txt("Ensure the required travel time columns exist in the dataset."),
      soft = FALSE
    )

    # Validate health_care_travel_time column

    phr_validate_choice(
      x = .dataset[[health_care_travel_time_col]],
      choices = c(num_minutes_val, range_val, NA_character_),
      origin = origin,
      soft = TRUE
    )

    # Validate health_care_travel_time_minutes column

    phr_validate_all_numeric(
      .dataset[[health_care_travel_time_minutes_col]],
      origin = origin,
      hint = phr_txt("The `health_care_travel_time_minutes_col` column must contain numeric values."),
      soft = TRUE
    )

    # Validate ranges from health_care_travel_time_range_col

    valid_ranges <- c(less_than_one_hour_range_val, one_hour_or_more_range_val, NA_character_)
    phr_validate_choice(
      x = .dataset[[health_care_travel_time_range_col]],
      choices = valid_ranges,
      origin = origin,
      soft = TRUE
    )

    # Overwrite warnings for any existing output column

    output_column <- "health_healthcare_access_one_hour"

    if (output_column %in% names(.dataset)) {
      phr_warning(
        origin = origin,
        message = phr_txt(glue::glue("Variable `{output_column}` already exists and will be overwritten."))
      )
    }

    # Calculate healthcare access within one hour

    .dataset <- .dataset %>%
      dplyr::mutate(
        health_healthcare_access_one_hour = dplyr::case_when(
          # Use numeric minutes if available
          .data[[health_care_travel_time_col]] == num_minutes_val &
            !is.na(.data[[health_care_travel_time_minutes_col]]) &
            .data[[health_care_travel_time_minutes_col]] < 60 ~ phr_txt("yes"),
          .data[[health_care_travel_time_col]] == num_minutes_val &
            !is.na(.data[[health_care_travel_time_minutes_col]]) &
            .data[[health_care_travel_time_minutes_col]] >= 60 ~ phr_txt("no"),

          # Use range-based travel time if numeric minutes are unavailable
          .data[[health_care_travel_time_col]] == range_val &
            .data[[health_care_travel_time_range_col]] %in% less_than_one_hour_range_val ~ phr_txt("yes"),
          .data[[health_care_travel_time_col]] == range_val &
            .data[[health_care_travel_time_range_col]] %in% one_hour_or_more_range_val ~ phr_txt("no"),

          # Mark as `dont_know` if unable to determine
          is.na(.data[[health_care_travel_time_minutes_col]]) &
            is.na(.data[[health_care_travel_time_range_col]]) ~ phr_txt("dont_know"),

          # Default for undefined cases
          TRUE ~ NA_character_
        )
      )

    phr_message(
      origin = origin,
      message = phr_txt("Healthcare access within one hour successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure input columns have valid data, and values align with the ranges and expected inputs provided."))
}

