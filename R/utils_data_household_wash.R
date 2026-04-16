
# add_hwise ####
#' Add Household Water Insecurity (HWISE) Scores to a Dataset
#'
#' This function calculates the Household Water Insecurity Experience (HWISE) scores for a dataset. It computes
#' the HWISE-4 and (optionally) the HWISE-12 scores based on input columns. These scores measure household water
#' insecurity levels based on survey responses, which are categorized into specific severity levels.
#'
#' @param .dataset A data frame or tibble containing the input data.
#' @param wash_hwise_worry_col Column name for the variable representing "Worry about water issues".
#' @param wash_hwise_plans_col Column name for the variable representing "Plans impacted by water issues".
#' @param wash_hwise_hands_col Column name for the variable representing "Lack of water to wash hands".
#' @param wash_hwise_drink_col Column name for the variable representing "Lack of drinking water".
#' @param wash_hwise_interrupt_col Optional column name for the variable representing "Interruptions due to water issues".
#' @param wash_hwise_clothes_col Optional column name for the variable representing "Lack of water to wash clothes".
#' @param wash_hwise_food_col Optional column name for the variable representing "Lack of water to cook food".
#' @param wash_hwise_body_col Optional column name for the variable representing "Lack of water for body hygiene".
#' @param wash_hwise_angry_col Optional column name for the variable representing "Anger due to water issues".
#' @param wash_hwise_sleep_col Optional column name for the variable representing "Sleep disturbances due to water issues".
#' @param wash_hwise_none_col Optional column name for the variable representing "No water-related issues".
#' @param wash_hwise_shame_col Optional column name for the variable representing "Shame resulting from water-related issues".
#' @param never_val Response category representing "Never". Defaults to `"never"`.
#' @param rarely_val Response category representing "Rarely". Defaults to `"rarely"`.
#' @param sometimes_val Response category representing "Sometimes". Defaults to `"sometimes"`.
#' @param often_val Response category representing "Often". Defaults to `"often"`.
#' @param always_val Response category representing "Always". Defaults to `"always"`.
#'
#' @details
#' The function calculates scores for both `HWISE-4` and `HWISE-12` indicators. The `HWISE-4` score is based on
#' the required variables, while the `HWISE-12` score includes additional optional variables if provided. Both
#' scores are categorized into severity levels as follows:
#'
#' - `HWISE-4 Severity Categories`:
#'   - 0-3: No-to-marginal
#'   - 4-6: Low
#'   - 7-8: Moderate
#'   - 9-10: High
#'   - 11+: Very High
#'
#' - `HWISE-12 Severity Categories`:
#'   - 0-2: No-to-marginal
#'   - 3-11: Low
#'   - 12-23: Moderate
#'   - 24-36: High
#'
#' If a column already exists in the dataset for output variables (like `wash_hwise4_score`), a warning will be
#' generated, and the existing column will be overwritten.
#'
#' @return Returns a data frame or tibble with the calculated HWISE scores:
#' - `wash_hwise4_score`: Score for HWISE-4.
#' - `wash_hwise4_severity_cat`: Severity category for HWISE-4.
#' - `wash_hwise4_insecure_cat`: Binary insecurity category for HWISE-4 (0 = secure, 1 = insecure).
#' - `wash_hwise12_score`: Score for HWISE-12 (if applicable).
#' - `wash_hwise12_severity_cat`: Severity category for HWISE-12 (if applicable).
#' - `wash_hwise12_insecure_cat`: Binary insecurity category for HWISE-12 (0 = secure, 1 = insecure, if applicable).
#'
#' @examples
#' # Example dataset
#' example_data <- data.frame(
#'   wash_hwise_worry = c("never", "rarely", "sometimes"),
#'   wash_hwise_plans = c("never", "rarely", "sometimes"),
#'   wash_hwise_hands = c("never", "rarely", "sometimes"),
#'   wash_hwise_drink = c("never", "rarely", "sometimes")
#' )
#'
#' # Calculate HWISE scores
#' result <- add_hwise(
#'   .dataset = example_data,
#'   wash_hwise_worry_col = "wash_hwise_worry",
#'   wash_hwise_plans_col = "wash_hwise_plans",
#'   wash_hwise_hands_col = "wash_hwise_hands",
#'   wash_hwise_drink_col = "wash_hwise_drink"
#' )
#'
#' @importFrom dplyr mutate rowwise ungroup across ends_with
#' @export
add_hwise <- function(
    .dataset,
    wash_hwise_worry_col = "wash_hwise_worry",
    wash_hwise_plans_col = "wash_hwise_plans",
    wash_hwise_hands_col = "wash_hwise_hands",
    wash_hwise_drink_col = "wash_hwise_drink",
    wash_hwise_interrupt_col = NULL,
    wash_hwise_clothes_col = NULL,
    wash_hwise_food_col = NULL,
    wash_hwise_body_col = NULL,
    wash_hwise_angry_col = NULL,
    wash_hwise_sleep_col = NULL,
    wash_hwise_none_col = NULL,
    wash_hwise_shame_col = NULL,
    never_val = "never",
    rarely_val = "rarely",
    sometimes_val = "sometimes",
    often_val = "often",
    always_val = "always"
) {

  origin <- "add_hwise"

  phr_try(

    expr = {

      # Use ensure_value for all *_val parameters
      never_val <- ensure_value(never_val, "never")
      rarely_val <- ensure_value(rarely_val, "rarely")
      sometimes_val <- ensure_value(sometimes_val, "sometimes")
      often_val <- ensure_value(often_val, "often")
      always_val <- ensure_value(always_val, "always")

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

      # Required columns for wash_hwise-4
      hwise4_cols <- c(wash_hwise_worry_col, wash_hwise_plans_col, wash_hwise_hands_col, wash_hwise_drink_col)
      phr_validate_columns(
        .dataset,
        hwise4_cols,
        origin = origin,
        hint = phr_txt("Ensure all required wash_hwise-4 variables are present."),
        soft = FALSE
      )

      # Validate response categories
      valid_responses <- c(never_val, rarely_val, sometimes_val, often_val, always_val)

      for (col in hwise4_cols) {
        phr_validate_choice(
          x = .dataset[[col]],
          choices = c(valid_responses, NA_character_),
          origin = origin,
          soft = FALSE
        )
      }

      # Required columns for wash_hwise-12 (if provided)
      hwise12_cols <- c(
        wash_hwise_interrupt_col, wash_hwise_clothes_col, wash_hwise_food_col,
        wash_hwise_body_col, wash_hwise_angry_col, wash_hwise_sleep_col,
        wash_hwise_none_col, wash_hwise_shame_col
      )
      hwise12_cols <- hwise12_cols[!is.null(hwise12_cols)]

      if (length(hwise12_cols) > 0) {
        phr_validate_columns(
          .dataset,
          hwise12_cols,
          origin = origin,
          hint = phr_txt("Ensure all required wash_hwise-12 variables are present if calculating wash_hwise-12."),
          soft = FALSE
        )

        for (col in hwise12_cols) {
          phr_validate_choice(
            x = .dataset[[col]],
            choices = c(valid_responses, NA_character_),
            origin = origin,
            soft = FALSE
          )
        }
      }


      # Overwrite warnings for any existing output columns

      overwrite_vars <- c(
        "wash_hwise4_score", "wash_hwise4_severity_cat", "wash_hwise4_insecure_cat",
        "wash_hwise12_score", "wash_hwise12_severity_cat", "wash_hwise12_insecure_cat"
      )

      for (var in overwrite_vars) {
        if (var %in% names(.dataset)) {
          phr_warning(
            origin = origin,
            message = phr_txt(glue::glue("Variable {var} already exists and will be overwritten."))
          )
        }
      }

      # Scoring for wash_hwise-4

      .dataset <- .dataset |>
        dplyr::mutate(
          dplyr::across(
            .cols = hwise4_cols,
            .fns = ~ dplyr::case_when(
              . == never_val ~ 0,
              . == rarely_val ~ 1,
              . == sometimes_val ~ 2,
              . == often_val ~ 3,
              . == always_val ~ 3,
              TRUE ~ NA_real_
            ),
            .names = "{.col}_score"
          )
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          wash_hwise4_score = sum(dplyr::c_across(dplyr::all_of(paste0(hwise4_cols, "_score"))), na.rm = FALSE),
          wash_hwise4_severity_cat = dplyr::case_when(
            wash_hwise4_score >= 0 & wash_hwise4_score <= 3 ~ phr_txt("No-to-marginal"),
            wash_hwise4_score >= 4 & wash_hwise4_score <= 6 ~ phr_txt("Low"),
            wash_hwise4_score >= 7 & wash_hwise4_score <= 8 ~ phr_txt("Moderate"),
            wash_hwise4_score >= 9 & wash_hwise4_score <= 10 ~ phr_txt("High"),
            wash_hwise4_score >= 11 ~ phr_txt("Very High"),
            TRUE ~ NA_character_
          ),
          wash_hwise4_cat = dplyr::case_when(
            wash_hwise4_score >= 0 & wash_hwise4_score <= 3 ~ phr_txt("Water Secure"),
            wash_hwise4_score >= 4 & wash_hwise4_score <= 12 ~ phr_txt("Water Insecure"),
            TRUE ~ NA_character_
          )
        ) |>
        dplyr::ungroup()


      # Scoring for wash_hwise-12 (if applicable)

      if (length(hwise12_cols) > 0) {
        .dataset <- .dataset |>
          dplyr::mutate(
            dplyr::across(
              .cols = hwise12_cols,
              .fns = ~ dplyr::case_when(
                . == never_val ~ 0,
                . == rarely_val ~ 1,
                . == sometimes_val ~ 2,
                . == often_val ~ 3,
                . == always_val ~ 3,
                TRUE ~ NA_real_
              ),
              .names = "{.col}_score"
            )
          ) |>
          dplyr::rowwise() |>
          dplyr::mutate(
            wash_hwise12_score = sum(c_across(ends_with("_score")), na.rm = FALSE),
            wash_hwise12_severity_cat = dplyr::case_when(
              wash_hwise12_score >= 0 & wash_hwise12_score <= 2 ~ phr_txt("No-to-marginal"),
              wash_hwise12_score >= 3 & wash_hwise12_score <= 11 ~ phr_txt("Low"),
              wash_hwise12_score >= 12 & wash_hwise12_score <= 23 ~ phr_txt("Moderate"),
              wash_hwise12_score >= 24 & wash_hwise12_score <= 36 ~ phr_txt("High"),
              TRUE ~ NA_character_
            )
          ) |>
          dplyr::ungroup()
      }

      phr_message(
        origin = origin,
        message = phr_txt("wash_hwise-4 and wash_hwise-12 (if applicable) calculations completed successfully.")
      )

      return(.dataset)
    },

    on_error = "abort",
    origin = origin,
    hint = phr_txt("Ensure all input columns exist and contain valid response values.")
  )
}

# Liters per person per day ####

#' Add Liters Per Person Per Day and Related Metrics to a Dataset
#'
#' This function calculates the liters per person per day (`liters_pppd`) and other related metrics
#' for a dataset. The function assumes proper data validation and calculation based on input column names.
#'
#' @param .dataset A data frame or tibble containing input data.
#' @param total_liters_col Column name for total liters of water available.
#' @param household_size_col Column name for household size.
#' @param num_days_col Column name for the number of days of water supply.
#'
#' @details
#' The function calculates and appends the following columns:
#' - `liters_pppd`: Total liters per person per day.
#' - `liters_z_score`: Z-score (standardized value) of the `total_liters`.
#' - `liters_pppd_z_score`: Z-score of the `liters_pppd`.
#' - `liters_log`: Natural logarithm of `total_liters` (log transformation).
#' - `liters_pppd_log`: Natural logarithm of `liters_pppd`.
#' - `liters_log_z_score`: Z-score of the `liters_log`.
#' - `liters_pppd_log_z_score`: Z-score of the `liters_pppd_log`.
#'
#' Uses robust validation to ensure input columns exist, dataset is valid, and calculations align with expectations.
#'
#' @return A data frame or tibble with additional columns for the calculated metrics.
#' @examples
#' example_data <- data.frame(
#'   total_liters = c(100, 200, 300),
#'   household_size = c(4, 5, 6),
#'   num_days = c(1, 2, 3)
#' )
#' result <- add_liters_per_person_per_day(
#'   .dataset = example_data,
#'   total_liters_col = "total_liters",
#'   household_size_col = "household_size",
#'   num_days_col = "num_days"
#' )
#'
#' @importFrom dplyr mutate rowwise ungroup across ends_with
#' @export
add_liters_per_person_per_day <- function(
    .dataset,
    total_liters_col,
    household_size_col,
    num_days_col = NULL  # num_days_col is now optional and can be NULL
) {

  origin <- "add_liters_per_person_per_day"

  phr_try(
    expr = {
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

      # Adjust num_days_column if NULL
      if (is.null(num_days_col)) {
        .dataset <- .dataset |>
          dplyr::mutate(temp_num_days_col = 1)  # Add a temporary column with default value 1
        num_days_col <- "temp_num_days_col"  # Use this temporary column as num_days_col
      }

      # Required columns
      required_cols <- c(total_liters_col, household_size_col, num_days_col)
      phr_validate_columns(
        .dataset,
        required_cols,
        origin = origin,
        hint = phr_txt("Ensure all required columns are present in the dataset."),
        soft = FALSE
      )

      # Validate column contents
      for (col in required_cols) {
        phr_assert(
          is.numeric(.dataset[[col]]) & all(.dataset[[col]] >= 0, na.rm = TRUE),
          origin = origin,
          phr_txt(glue::glue("Column `{col}` must be numeric and contain non-negative values."))
        )
      }

      # Calculations
      .dataset <- .dataset |>
        dplyr::mutate(
          # Calculate liters per person per day
          liters_pppd = .data[[total_liters_col]] /
            (.data[[household_size_col]] * .data[[num_days_col]]),

          # Z-score of total liters
          liters_z_score = (.data[[total_liters_col]] - mean(.data[[total_liters_col]], na.rm = TRUE)) /
            sd(.data[[total_liters_col]], na.rm = TRUE),

          # Z-score of liters per person per day
          liters_pppd_z_score = (liters_pppd - mean(liters_pppd, na.rm = TRUE)) /
            sd(liters_pppd, na.rm = TRUE),

          # Log-transformed values
          liters_log = log(.data[[total_liters_col]]),
          liters_pppd_log = log(liters_pppd),

          # Z-score of log-transformed values
          liters_log_z_score = (liters_log - mean(liters_log, na.rm = TRUE)) /
            sd(liters_log, na.rm = TRUE),
          liters_pppd_log_z_score = (liters_pppd_log - mean(liters_pppd_log, na.rm = TRUE)) /
            sd(liters_pppd_log, na.rm = TRUE),

          # Categorize liters per person per day
          wash_lppd_cat = dplyr::case_when(
            liters_pppd < 3 ~ phr_txt("Less than 3 LPPD"),
            liters_pppd >= 3 & liters_pppd < 7.5 ~ phr_txt("3-7.5 LPPD"),
            liters_pppd >= 7.5 & liters_pppd < 15 ~ phr_txt("7.5- <15 LPPD"),
            liters_pppd >= 15 ~ phr_txt("Greater than 15 LPPD"),
            TRUE ~ NA_character_
          )
        )

      # Remove temporary column if created
      if ("temp_num_days_col" %in% names(.dataset)) {
        .dataset <- .dataset |>
          dplyr::select(-temp_num_days_col)
      }

      phr_message(
        origin = origin,
        message = phr_txt("Liters per person per day and related metrics added successfully.")
      )

      return(.dataset)
    },

    on_error = "abort",
    origin = origin,
    hint = phr_txt("Ensure all input columns exist, are numeric, and do not contain negative values.")
  )
}

# Improved Water Source ####

#' Add Drinking Water Source Categories to a Dataset
#'
#' This function categorizes drinking water sources into the following categories:
#' "Improved", "Unimproved", "Surface Water", and "Undefined". It recodes the
#' drinking water source variable based on provided classifications and adds a new
#' column `wash_drinking_water_source_cat` to the dataset.
#'
#' @param .dataset A data frame or tibble containing the input data.
#' @param drinking_water_source_col The column name for the drinking water source variable
#'   in the dataset. Defaults to `"wash_water_source"`.
#' @param drinking_water_source_cat_improved_val A character vector of values classified as
#'   "Improved" drinking water sources. Defaults to:
#'   \describe{
#'     \item{Includes:}{
#'       "piped_dwelling", "piped_compound", "piped_neighbour",
#'       "tap", "borehole", "protected_well", "protected_spring",
#'       "rainwater_collection", "tank_truck", "cart_tank",
#'       "kiosk", "bottled_water", "sachet_water"
#'     }
#'   }
#' @param drinking_water_source_cat_unimproved_val A character vector of values classified as
#'   "Unimproved" drinking water sources. Defaults to:
#'   \describe{
#'     \item{Includes:}{
#'       "unprotected_well", "unprotected_spring"
#'     }
#'   }
#' @param drinking_water_source_cat_surface_water_val A character value classified as
#'   "Surface Water". Defaults to `"surface_water"`.
#' @param drinking_water_source_cat_undefined_val A character vector of values classified as
#'   "Undefined". Defaults to:
#'   \describe{
#'     \item{Includes:}{
#'       "dnk" (do not know), "pnta" (prefer not to answer), "other"
#'     }
#'   }
#'
#' @details
#' The function performs the following steps:
#' - Validates that the `.dataset` is a valid data frame or tibble and is not empty.
#' - Verifies that the `drinking_water_source_col` column exists in the dataset and contains valid responses.
#' - Categorizes the values in the `drinking_water_source_col` column based on the provided classifications.
#' - Adds a new column `wash_drinking_water_source_cat` to the dataset with the recoded categories:
#'   \describe{
#'     \item{Improved}{Corresponding to entries from `drinking_water_source_cat_improved_val`.}
#'     \item{Unimproved}{Corresponding to entries from `drinking_water_source_cat_unimproved_val`.}
#'     \item{Surface Water}{Matching `drinking_water_source_cat_surface_water_val`.}
#'     \item{Undefined}{Matching `drinking_water_source_cat_undefined_val`.}
#'   }
#'
#' If the output column `wash_drinking_water_source_cat` already exists, a warning is issued, and
#' the column is overwritten.
#'
#' @return A data frame or tibble with an additional column `wash_drinking_water_source_cat`.
#'
#' @examples
#' # Example dataset
#' example_data <- data.frame(
#'   wash_drinking_water_source = c("piped_dwelling", "unprotected_well", "surface_water", "dnk")
#' )
#'
#' # Add drinking water source categories
#' result <- add_drinking_water_source_cat(
#'   .dataset = example_data,
#'   drinking_water_source_col = "wash_drinking_water_source"
#' )
#'
#' @importFrom dplyr mutate
#' @export
add_drinking_water_source_cat <- function(
    .dataset,
    drinking_water_source_col = "wash_water_source",
    drinking_water_source_cat_improved_val = c(
      "piped_dwelling",
      "piped_compound",
      "piped_neighbour",
      "tap",
      "borehole",
      "protected_well",
      "protected_spring",
      "rainwater_collection",
      "tank_truck",
      "cart_tank",
      "kiosk",
      "bottled_water",
      "sachet_water"
    ),
    drinking_water_source_cat_unimproved_val = c(
      "unprotected_well",
      "unprotected_spring"
    ),
    drinking_water_source_cat_surface_water_val = "surface_water",
    drinking_water_source_cat_undefined_val = c("dnk", "pnta", "other")
) {

  origin <- "add_drinking_water_source_cat"

  phr_try({


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


    # Validate column

    phr_validate_columns(
      .dataset,
      drinking_water_source_col,
      origin = origin,
      hint = phr_txt("Drinking water source column must exist in the dataset."),
      soft = FALSE
    )


    # Validate value choices in the water source column

    # allowed_values <- c(
    #   drinking_water_source_cat_improved_val,
    #   drinking_water_source_cat_unimproved_val,
    #   drinking_water_source_cat_surface_water_val,
    #   drinking_water_source_cat_undefined_val
    # )
    phr_validate_choice(
      x = .dataset[[drinking_water_source_col]],
      choices = c(
        drinking_water_source_cat_improved_val,
        drinking_water_source_cat_unimproved_val,
        drinking_water_source_cat_surface_water_val,
        drinking_water_source_cat_undefined_val,
        NA_character_
      ),
      origin = origin,
      soft = FALSE
    )


    # Overwrite warnings for any existing output column

    output_column <- "wash_drinking_water_source_cat"

    if (output_column %in% names(.dataset)) {
      phr_warning(
        origin = origin,
        message = phr_txt(glue::glue("Variable {output_column} already exists and will be overwritten."))
      )
    }


    # Recode drinking water source categories

    .dataset <- .dataset |>
      dplyr::mutate(
        wash_drinking_water_source_cat = dplyr::case_when(
          .data[[drinking_water_source_col]] %in% drinking_water_source_cat_surface_water_val ~ phr_txt("Surface Water"),
          .data[[drinking_water_source_col]] %in% drinking_water_source_cat_unimproved_val ~ phr_txt("Unimproved"),
          .data[[drinking_water_source_col]] %in% drinking_water_source_cat_improved_val ~ phr_txt("Improved"),
          .data[[drinking_water_source_col]] %in% drinking_water_source_cat_undefined_val ~ phr_txt("Undefined"),
          TRUE ~ NA_character_
        )
      )

    phr_message(
      origin = origin,
      message = phr_txt("Drinking water source categories recoded successfully into wash_drinking_water_source_cat.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure the drinking water source column exists and contains valid response values."))
}

# Water Time Less 30min ####

#' Add Drinking Water Collection Time Category to Dataset
#'
#' Categorizes households based on the time taken to fetch drinking water. Produces a numeric output column
#' (`wash_drinking_water_time_cat`) with the following codes:
#' - `1`: Less than or equal to 30 minutes (on premises or quick access).
#' - `0`: More than 30 minutes.
#' - `NA`: Undefined or missing information.
#'
#' The function works flexibly based on the arguments provided:
#' - If a numeric column for minutes (`number_minutes_col`) is provided, it is preferred for categorization.
#' - If a categorical column (`categorical_time_col`) is provided, it is used when numeric minutes are not available.
#'
#' @param .dataset Input data frame or tibble.
#' @param number_minutes_col Column name for the reported number of minutes. Defaults to `"number_minutes"`.
#' @param categorical_time_col Column name for categorical responses indicating time in categories (e.g., "under_30min_val", "more_than_30min_val"). Defaults to `"categorical_time"`.
#' @param under_30min_val Value in `categorical_time_col` indicating less than 30 minutes. Defaults to `"under_30min"`.
#' @param more_than_30min_val Value in `categorical_time_col` indicating more than 30 minutes. Defaults to `"more_than_30min"`.
#' @param undefined_val Values in either column indicating undefined responses. Defaults to `c("dnk", "pnta")`.
#' @param premises_val Value in `categorical_time_col` indicating water is fetched on premises. Defaults to `"on_premises"`.
#'
#' @details
#' If both `number_minutes_col` and `categorical_time_col` are provided, the function prioritizes `number_minutes_col` for calculations.
#' The function creates a new column `wash_drinking_water_time_cat` coded as:
#' - `1`: Fetching water takes less than or equal to 30 minutes (or is on premises).
#' - `0`: Fetching water takes more than 30 minutes.
#' - `NA`: Undefined or missing information.
#'
#' @return A modified dataset with an additional column `wash_drinking_water_time_cat`.
#'
#' @examples
#' example_data <- data.frame(
#'   number_minutes = c(15, 45, NA, 25),
#'   categorical_time = c("under_30min", "more_than_30min", "pnta", "under_30min")
#' )
#' add_drinking_water_time_cat(example_data)
#'
#' @importFrom dplyr mutate case_when
#' @importFrom rlang .data abort sym
#' @export
add_drinking_water_time_cat <- function(
    .dataset,
    number_minutes_col = "number_minutes",
    categorical_time_col = "categorical_time",
    under_30min_val = "under_30min",
    more_than_30min_val = "more_than_30min",
    undefined_val = c("dnk", "pnta"),
    premises_val = "on_premises"
) {
  origin <- "add_drinking_water_time_cat"

  phr_try(
    expr = {
      # Use ensure_value for all *_val parameters
      under_30min_val <- ensure_value(under_30min_val, "under_30min")
      more_than_30min_val <- ensure_value(more_than_30min_val, "more_than_30min")
      undefined_val <- ensure_value(undefined_val, c("dnk", "pnta"))
      premises_val <- ensure_value(premises_val, "on_premises")

      # Validate dataset
      phr_validate_dataframe(
        .dataset,
        origin = origin,
        hint = phr_txt("Ensure you pass a valid data frame or tibble to `.dataset`.")
      )

      phr_assert(
        nrow(.dataset) > 0,
        origin = origin,
        phr_txt("Dataset is empty.")
      )

      # Validate input columns
      has_numeric_col <- number_minutes_col %in% names(.dataset)
      has_categorical_col <- categorical_time_col %in% names(.dataset)

      if (has_numeric_col) {
        phr_assert(
          is.numeric(.dataset[[number_minutes_col]]) | all(is.na(.dataset[[number_minutes_col]])),
          origin = origin,
          phr_txt(glue::glue("Column {number_minutes_col} must be numeric."))
        )
      }

      if (has_categorical_col) {
        phr_validate_columns(
          .dataset,
          categorical_time_col,
          origin = origin,
          hint = phr_txt("Ensure the categorical time column exists if minutes are not provided.")
        )

        phr_validate_choice(
          x = .dataset[[categorical_time_col]],
          choices = c(under_30min_val, more_than_30min_val, undefined_val, premises_val, NA_character_),
          origin = origin,
          soft = FALSE
        )
      }

      if (!has_numeric_col && !has_categorical_col) {
        phr_error(
          message = "Both number_minutes_col and categorical_time_col are missing. At least one is required.",
          origin = origin,
          hint = phr_txt("Ensure one of the valid input columns exists in the dataset.")
        )
      }

      # Overwrite warnings for output column
      output_col <- "wash_drinking_water_time_cat"
      if (output_col %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt(glue::glue("Column {output_col} already exists and will be overwritten."))
        )
      }

      # Categorize drinking water time based on 4 scenarios
      if (has_numeric_col && has_categorical_col) {
        # Scenario 1: Both columns available - prefer numeric when not NA, fallback to categorical
        .dataset <- dplyr::mutate(
          .dataset,
          !!output_col := dplyr::case_when(
            # Use numeric if available and not NA
            !is.na(.data[[number_minutes_col]]) & .data[[number_minutes_col]] <= 30 ~ 1,
            !is.na(.data[[number_minutes_col]]) & .data[[number_minutes_col]] > 30 ~ 0,
            # Fallback to categorical if numeric is NA
            is.na(.data[[number_minutes_col]]) & .data[[categorical_time_col]] %in% c(under_30min_val, premises_val) ~ 1,
            is.na(.data[[number_minutes_col]]) & .data[[categorical_time_col]] %in% more_than_30min_val ~ 0,
            is.na(.data[[number_minutes_col]]) & .data[[categorical_time_col]] %in% undefined_val ~ NA_real_,
            TRUE ~ NA_real_
          )
        )
      } else if (has_numeric_col && !has_categorical_col) {
        # Scenario 2: Only numeric column available
        .dataset <- dplyr::mutate(
          .dataset,
          !!output_col := dplyr::case_when(
            .data[[number_minutes_col]] <= 30 ~ 1,
            .data[[number_minutes_col]] > 30 ~ 0,
            TRUE ~ NA_real_
          )
        )
      } else if (!has_numeric_col && has_categorical_col) {
        # Scenario 3: Only categorical column available
        .dataset <- dplyr::mutate(
          .dataset,
          !!output_col := dplyr::case_when(
            .data[[categorical_time_col]] %in% c(under_30min_val, premises_val) ~ 1,
            .data[[categorical_time_col]] %in% more_than_30min_val ~ 0,
            .data[[categorical_time_col]] %in% undefined_val ~ NA_real_,
            TRUE ~ NA_real_
          )
        )
      } else {
        # Scenario 4: Neither column available (fallback error)
        phr_error(
          message = "Neither number_minutes_col nor categorical_time_col are available.",
          origin = origin,
          hint = phr_txt("This should not happen if validation passed. Check column availability.")
        )
      }

      phr_message(
        origin = origin,
        message = phr_txt("Drinking water time category successfully added as wash_drinking_water_time_cat.")
      )
      return(.dataset)
    },
    on_error = "abort",
    origin = origin
  )
}

# Any Water Treatment ####

#' Add Any Water Treatment Indicator to Dataset
#'
#' This function classifies households as using "yes", "no", or `NA` for any water treatment based on survey responses.
#' It adds a new column \code{wash_any_water_treatment} to the dataset, using user-specified Yes/No/string values for classification.
#'
#' @param .dataset A data frame or tibble with household response data.
#' @param water_treatment_col The column name (character) containing water treatment response values.
#' @param yes_values Character value or vector of response(s) that indicate water treatment occurred.
#' @param no_values Character value or vector of response(s) that indicate no water treatment occurred.
#'
#' @details
#' The function adds \code{wash_any_water_treatment} to \code{.dataset}, with values defined as:
#' - \code{"yes"} for responses matching any entry in \code{yes_values}.
#' - \code{"no"} for responses matching any entry in \code{no_values}.
#' - \code{NA} for anything not matching yes or no (including explicit DNK, PNTA, or missing).
#'
#' Issues warnings or errors as appropriate for empty data, missing columns, or overwriting existing output columns.
#'
#' @return The input data frame or tibble, with a new column \code{wash_any_water_treatment} (\code{"yes"}, \code{"no"}, or \code{NA}).
#'
#' @examples
#' dat <- data.frame(water_treatment = c("boiling", "none", "chlorine", NA, "dnk"))
#' add_any_water_treatment(
#'   dat,
#'   water_treatment_col = "water_treatment",
#'   yes_values = c("boiling", "chlorine", "filter"),
#'   no_values = c("none")
#' )
#'
#' @importFrom dplyr mutate case_when
#' @importFrom rlang .data
#' @export
add_any_water_treatment <- function(
    .dataset,
    water_treatment_col = "water_treatment",
    yes_values = c("boiling", "chlorine", "filter"),
    no_values = c("none")
) {
  origin <- "add_any_water_treatment"
  phr_try(
    expr = {
      # Validate dataset
      phr_validate_dataframe(
        .dataset,
        origin = origin,
        hint = phr_txt("Ensure you pass a valid data frame or tibble to `.dataset`.")
      )
      phr_assert(
        nrow(.dataset) > 0,
        origin = origin,
        phr_txt("Dataset is empty.")
      )

      # Validate column exists
      phr_validate_columns(
        .dataset,
        water_treatment_col,
        origin = origin,
        hint = phr_txt("The water treatment column must exist in the dataset."),
        soft = FALSE
      )

      # Validate yes/no vectors not empty
      phr_assert(
        length(yes_values) > 0 & is.character(yes_values),
        origin = origin,
        phr_txt("Argument `yes_values` must be a non-empty character vector.")
      )
      phr_assert(
        length(no_values) > 0 & is.character(no_values),
        origin = origin,
        phr_txt("Argument `no_values` must be a non-empty character vector.")
      )

      # Warn if output already exists
      output_col <- "wash_any_water_treatment"
      if (output_col %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt(glue::glue("Column {output_col} already exists and will be overwritten."))
        )
      }

      # Categorize
      .dataset <- dplyr::mutate(
        .dataset,
        !!output_col := dplyr::case_when(
          .data[[water_treatment_col]] %in% yes_values ~ "yes",
          .data[[water_treatment_col]] %in% no_values ~ "no",
          TRUE ~ NA_character_
        )
      )

      phr_message(
        origin = origin,
        message = phr_txt("Any water treatment indicator (yes/no/NA) calculation completed successfully.")
      )
      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = phr_txt("Ensure the input column exists and values are appropriate for classification.")
  )
}

# Improved Sanitation Facility ####

#' Add Sanitation Facility Categories to a Dataset
#'
#' This function categorizes sanitation facilities into the following categories:
#' "Improved", "Unimproved", and "Open Defecation". It recodes the sanitation
#' facility variable based on provided classifications and adds a new column
#' `wash_sanitation_facility_cat` to the dataset.
#'
#' @param .dataset A data frame or tibble containing the input data.
#' @param sanitation_facility_col The column name for the sanitation facility variable
#'   in the dataset. Defaults to `"wash_sanitation_facility"`.
#' @param improved_facilities_val A character vector of values classified as
#'   "Improved" sanitation facilities.
#' @param unimproved_facilities_val A character vector of values classified as
#'   "Unimproved" sanitation facilities.
#' @param open_defecation_val A character vector of values classified as
#'   "Open Defecation" practices.
#' @param undefined_val A character vector of values classified as "Undefined" sanitation facilities. Defaults to `NULL` (no undefined category).
#'
#' @details
#' The function performs the following:
#' - Validates that the `.dataset` is a valid data frame or tibble and is not empty.
#' - Verifies that the `sanitation_facility_col` column exists in the dataset and contains valid responses.
#' - Categorizes the values in the `sanitation_facility_col` column based on the provided classifications.
#' - Adds a new column `wash_sanitation_facility_cat` to the dataset with recoded categories:
#'   \describe{
#'     \item{"Improved"}{Corresponding to entries from `improved_facilities_val`.}
#'     \item{"Unimproved"}{Corresponding to entries from `unimproved_facilities_val`.}
#'     \item{"Open Defecation"}{Matching entries from `open_defecation_val`.}
#'     \item{NA}{Any value not matching the above categories.}
#'   }
#'
#' If the output column `wash_sanitation_facility_cat` already exists, a warning is issued, and
#' the column is overwritten.
#'
#' @return A data frame or tibble with an additional column `wash_sanitation_facility_cat`.
#'
#' @examples
#' # Example dataset
#' example_data <- data.frame(
#'   wash_sanitation_facility = c("flush_to_piped", "pit_lat", "open_defecation", "other", NA)
#' )
#'
#' # Add sanitation facility categories
#' result <- add_sanitation_facility_cat(
#'   .dataset = example_data,
#'   sanitation_facility_col = "wash_sanitation_facility",
#'   improved_facilities_val = c("flush_to_piped", "flush_to_septic", "flush_to_pit"),
#'   unimproved_facilities_val = c("pit_lat", "bucket"),
#'   open_defecation_val = c("open_defecation")
#' )
#'
#' @importFrom dplyr mutate case_when
#' @export
add_sanitation_facility_cat <- function(
    .dataset,
    sanitation_facility_col = "wash_sanitation_facility",
    improved_facilities_val,
    unimproved_facilities_val,
    open_defecation_val,
    undefined_val = NULL
) {
  origin <- "add_sanitation_facility_cat"

  phr_try({


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


    # Validate column

    phr_validate_columns(
      .dataset,
      sanitation_facility_col,
      origin = origin,
      hint = phr_txt("Sanitation facility column must exist in the dataset."),
      soft = FALSE
    )


    # Validate value choices in the sanitation facility column

    allowed_values <- c(
      improved_facilities_val,
      unimproved_facilities_val,
      open_defecation_val,
      undefined_val
    )
    phr_validate_choice(
      x = .dataset[[sanitation_facility_col]],
      choices = c(allowed_values, NA_character_),
      origin = origin,
      soft = FALSE
    )


    # Overwrite warnings for any existing output column

    output_column <- "wash_sanitation_facility_cat"

    if (output_column %in% names(.dataset)) {
      phr_warning(
        origin = origin,
        message = phr_txt(glue::glue("Variable {output_column} already exists and will be overwritten."))
      )
    }


    # Recode sanitation facility categories

    .dataset <- .dataset |>
      dplyr::mutate(
        wash_sanitation_facility_cat = dplyr::case_when(
          .data[[sanitation_facility_col]] %in% open_defecation_val ~ phr_txt("Open Defecation"),
          .data[[sanitation_facility_col]] %in% unimproved_facilities_val ~ phr_txt("Unimproved"),
          .data[[sanitation_facility_col]] %in% improved_facilities_val ~ phr_txt("Improved"),
          .data[[sanitation_facility_col]] %in% undefined_val ~ phr_txt("Undefined"),

          TRUE ~ NA_character_
        )
      )

    phr_message(
      origin = origin,
      message = phr_txt("Sanitation facility categories recoded successfully into wash_sanitation_facility_cat.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure the sanitation facility column exists and contains valid response values."))
}

# Shared Sanitation Facility ####

#' @title Add Shared Sanitation Facility Indicator
#'
#' @description Adds a new column to the dataset indicating whether the sanitation facility is shared. If provided with both numerical and categorical inputs, the function prioritizes numerical evaluation, falling back to categorical evaluation for records without numerical data.
#'
#' @details The new column, `wash_sanitation_facility_shared`, is derived using one or both of the following methods:
#' 1. **Numerical Evaluation**: Based on the number of households sharing the sanitation facility. Facilities shared by `shared_threshold` or more households are categorized as "yes" (shared); otherwise, as "no" (not shared).
#' 2. **Categorical Evaluation (Fallback)**: Based on specific character values indicating shared or not shared facilities.
#'
#' If both options are provided, the function computes the indicator using numerical data if available for a given record. If the numerical value is missing or undefined, the categorical data is used as a fallback.
#'
#' @param .dataset A data frame or tibble containing the relevant columns.
#' @param num_households_col (Optional) Column name (as a string) for the number of households sharing the sanitation facility. Facilities with `shared_threshold` or more households are considered "yes" (shared).
#' @param shared_threshold (Numeric, Optional) Threshold for determining if a sanitation facility is shared based on the number of households (default is 2).
#' @param shared_response_col (Optional) Column name (as a string) for categorical responses indicating shared/not shared sanitation facilities.
#' @param shared_values (Optional) A character vector containing values indicating shared sanitation facilities.
#' @param not_shared_values (Optional) A character vector containing values indicating not shared sanitation facilities.
#'
#' @return A data frame with a new column `wash_sanitation_facility_shared`, containing:
#' * **"yes"**: Shared sanitation.
#' * **"no"**: Not shared sanitation.
#'
#' @examples
#' # Example 1: Using both numerical and categorical inputs (numerical prioritized)
#' df <- data.frame(
#'   num_households = c(1, 3, NA, NA),
#'   shared_response = c(NA, NA, "shared", "not_shared")
#' )
#' df_result <- add_sanitation_facility_shared(
#'   .dataset = df,
#'   num_households_col = "num_households",
#'   shared_response_col = "shared_response",
#'   shared_values = c("shared"),
#'   not_shared_values = c("not_shared")
#' )
#'
#' # Example 2: Using only numerical input
#' df <- data.frame(
#'   num_households = c(1, 2, 3, NA)
#' )
#' df_result <- add_sanitation_facility_shared(
#'   .dataset = df,
#'   num_households_col = "num_households"
#' )
#'
#' # Example 3: Using only categorical input
#' df <- data.frame(
#'   shared_response = c("shared", "not_shared", "shared", NA)
#' )
#' df_result <- add_sanitation_facility_shared(
#'   .dataset = df,
#'   shared_response_col = "shared_response",
#'   shared_values = c("shared"),
#'   not_shared_values = c("not_shared")
#' )
#'
#' @export
add_sanitation_facility_shared <- function(
    .dataset,
    num_households_col = NULL,
    shared_threshold = 2,
    shared_response_col = NULL,
    shared_values = NULL,
    not_shared_values = NULL
) {

  origin <- "add_sanitation_facility_shared"

  phr_try({

    # Use ensure_value for *_values parameters
    shared_values <- ensure_value(shared_values, "shared")
    not_shared_values <- ensure_value(not_shared_values, "not_shared")

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

    # Validation of inputs
    phr_assert(
      !is.null(num_households_col) || (!is.null(shared_response_col) && !is.null(shared_values) && !is.null(not_shared_values)),
      origin = origin,
      phr_txt("You must provide either `num_households_col` for numerical evaluation OR `shared_response_col` with `shared_values` and `not_shared_values` for categorical evaluation.")
    )

    # Validate `num_households_col` if provided
    if (!is.null(num_households_col)) {
      phr_validate_columns(
        .dataset,
        num_households_col,
        origin = origin,
        hint = phr_txt("Ensure the column for number of households exists in the dataset."),
        soft = FALSE
      )

      phr_validate_all_numeric(
        .dataset[[num_households_col]],
        origin = origin,
        hint = phr_txt("The `num_households_col` must contain numeric values."),
        soft = TRUE
      )
    }

    # Validate `shared_response_col` if provided
    if (!is.null(shared_response_col)) {
      phr_validate_columns(
        .dataset,
        shared_response_col,
        origin = origin,
        hint = phr_txt("Ensure the column for shared sanitation responses exists in the dataset."),
        soft = FALSE
      )

      phr_validate_choice(
        x = .dataset[[shared_response_col]],
        choices = c(shared_values, not_shared_values, NA_character_),
        origin = origin,
        soft = FALSE
      )
    }

    # Overwrite warnings for any existing output column
    output_column <- "wash_sanitation_facility_shared_cat"

    if (output_column %in% names(.dataset)) {
      phr_warning(
        origin = origin,
        message = phr_txt(glue::glue("Variable {output_column} already exists and will be overwritten."))
      )
    }

    # Determine which columns are present and write the corresponding logic
    if (!is.null(num_households_col) && !is.null(shared_response_col)) {

      # Both `num_households_col` and `shared_response_col` are present
      .dataset <- .dataset |>
        dplyr::mutate(
          wash_sanitation_facility_shared_cat = dplyr::case_when(
            !is.na(.data[[num_households_col]]) & .data[[num_households_col]] >= shared_threshold ~ "yes",
            !is.na(.data[[num_households_col]]) & .data[[num_households_col]] < shared_threshold ~ "no",
            is.na(.data[[num_households_col]]) & !is.na(.data[[shared_response_col]]) & .data[[shared_response_col]] %in% shared_values ~ "yes",
            is.na(.data[[num_households_col]]) & !is.na(.data[[shared_response_col]]) & .data[[shared_response_col]] %in% not_shared_values ~ "no",
            TRUE ~ NA_character_
          )
        )
    } else if (!is.null(num_households_col)) {

      # Only `num_households_col` is present
      .dataset <- .dataset |>
        dplyr::mutate(
          wash_sanitation_facility_shared_cat = dplyr::case_when(
            !is.na(.data[[num_households_col]]) & .data[[num_households_col]] >= shared_threshold ~ "yes",
            !is.na(.data[[num_households_col]]) & .data[[num_households_col]] < shared_threshold ~ "no",
            TRUE ~ NA_character_
          )
        )
    } else if (!is.null(shared_response_col)) {

      # Only `shared_response_col` is present
      .dataset <- .dataset |>
        dplyr::mutate(
          wash_sanitation_facility_shared_cat = dplyr::case_when(
            !is.na(.data[[shared_response_col]]) & .data[[shared_response_col]] %in% shared_values ~ phr_txt("yes"),
            !is.na(.data[[shared_response_col]]) & .data[[shared_response_col]] %in% not_shared_values ~ phr_txt("no"),
            TRUE ~ NA_character_
          )
        )
    } else {
      # In case neither column is provided (shouldn't happen due to earlier validation)
      stop("Invalid input: No valid columns for computation.")
    }

    phr_message(
      origin = origin,
      message = phr_txt("Shared sanitation facility indicator computed successfully.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure input columns and values exist and align with specifications."))
}

# JMP Water Service Ladder ####

#' @title Add JMP Ladder Categories for Drinking Water
#'
#' @description This function categorizes drinking water access based on water source and collection time into JMP (Joint Monitoring Programme) ladder categories.
#'
#' @details The resulting categories are based on ensuring safe and reliable drinking water sources:
#' * **Basic**: Improved water source with water collection time ≤ 30 minutes per round trip.
#' * **Limited**: Improved water source with water collection time > 30 minutes per round trip.
#' * **Unimproved**: Water source categorized as unimproved, regardless of collection time.
#' * **Surface Water**: Surface water source, regardless of collection time.
#'
#' The categories are ordered from "Basic" (best) to "Surface Water" (worst) for analysis or visualization needs.
#'
#' @param .dataset A data frame or tibble containing the required columns.
#' @param drinking_water_source_cat_col Column name (as a string) for the drinking water source category.
#' @param drinking_water_source_cat_improved_val Value indicating improved water sources (defaults to `"Improved"` if NULL or empty).
#' @param drinking_water_source_cat_unimproved_val Value indicating unimproved water sources (defaults to `"Unimproved"` if NULL or empty).
#' @param drinking_water_source_cat_surface_water_val Value indicating surface water sources (defaults to `"Surface Water"` if NULL or empty).
#' @param drinking_water_time_col Column name (as a string) for drinking water time category.
#' @param drinking_water_time_under_30min_val Value indicating water collection time ≤ 30 minutes (defaults to `"1"` if NULL or empty).
#' @param drinking_water_time_over_30min_val Value indicating water collection time > 30 minutes (defaults to `"0"` if NULL or empty).
#'
#' @return A data frame with a newly added column named `wash_jmp_ladder_drinking_water_cat`, which categorizes each record into one of the following JMP ladder categories:
#' * **Basic**
#' * **Limited**
#' * **Unimproved**
#' * **Surface Water**
#'
#' The new column is an ordered factor, in the JMP ladder order of "Basic", "Limited", "Unimproved", and "Surface Water".
#'
#' @export
add_drinking_water_jmp_ladder <- function(
    .dataset,
    drinking_water_source_cat_col = "wash_drinking_water_source_cat",
    drinking_water_source_cat_improved_val = NULL,
    drinking_water_source_cat_unimproved_val = NULL,
    drinking_water_source_cat_surface_water_val = NULL,
    drinking_water_time_col = "wash_drinking_water_time",
    drinking_water_time_under_30min_val = NULL,
    drinking_water_time_over_30min_val = NULL
) {
  origin <- "add_drinking_water_jmp_ladder"

  phr_try({

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

    # Validate required columns
    required_columns <- c(drinking_water_source_cat_col, drinking_water_time_col)
    phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = phr_txt("Ensure the drinking water source and collection time columns are present."),
      soft = FALSE
    )

    # Overwrite warnings for any existing output column
    output_column <- "wash_jmp_ladder_drinking_water_cat"
    if (output_column %in% names(.dataset)) {
      phr_warning(
        origin = origin,
        message = phr_txt(glue::glue("Variable {output_column} already exists and will be overwritten."))
      )
    }

    # Use ensure_value for all the variables
    drinking_water_source_cat_improved_val <- ensure_value(drinking_water_source_cat_improved_val, "Improved")
    drinking_water_source_cat_unimproved_val <- ensure_value(drinking_water_source_cat_unimproved_val, "Unimproved")
    drinking_water_source_cat_surface_water_val <- ensure_value(drinking_water_source_cat_surface_water_val, "Surface Water")
    drinking_water_time_under_30min_val <- ensure_value(drinking_water_time_under_30min_val, "1")
    drinking_water_time_over_30min_val <- ensure_value(drinking_water_time_over_30min_val, "0")

    # JMP ladder categorization
    .dataset <- .dataset |>
      dplyr::mutate(
        wash_jmp_ladder_drinking_water_cat = dplyr::case_when(
          as.character(.data[[drinking_water_source_cat_col]]) == drinking_water_source_cat_surface_water_val ~ phr_txt("Surface Water"),
          as.character(.data[[drinking_water_source_cat_col]]) == drinking_water_source_cat_unimproved_val ~ phr_txt("Unimproved"),
          as.character(.data[[drinking_water_source_cat_col]]) == drinking_water_source_cat_improved_val &
            as.character(.data[[drinking_water_time_col]]) == drinking_water_time_over_30min_val ~ phr_txt("Limited"),
          as.character(.data[[drinking_water_source_cat_col]]) == drinking_water_source_cat_improved_val &
            as.character(.data[[drinking_water_time_col]]) == drinking_water_time_under_30min_val ~ phr_txt("Basic"),
          TRUE ~ NA_character_
        )
      )

    # Convert to an ordered factor
    .dataset <- .dataset |>
      dplyr::mutate(
        wash_jmp_ladder_drinking_water_cat = factor(
          wash_jmp_ladder_drinking_water_cat,
          levels = c(
            phr_txt("Basic"),
            phr_txt("Limited"),
            phr_txt("Unimproved"),
            phr_txt("Surface Water")
          ),
          ordered = TRUE
        )
      )

    phr_message(
      origin = origin,
      message = phr_txt("JMP ladder categories for drinking water computed successfully.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure input columns exist and provide valid categorical values."))
}

# JMP Sanitation Service Ladder ####

#' @title Add JMP Ladder Categories for Sanitation Facilities
#'
#' @description Categorizes sanitation facilities into JMP (Joint Monitoring Programme) ladder categories based on facility type and sharing status.
#'
#' @param .dataset A data frame or tibble containing the required columns.
#' @param sanitation_facility_cat_col Column name (as a string) for the sanitation facility category.
#' @param sanitation_facility_improved_val Character value for improved sanitation facilities (default: "improved").
#' @param sanitation_facility_unimproved_val Character value for unimproved sanitation facilities (default: "unimproved").
#' @param sanitation_facility_open_defecation_val Character value for open defecation (default: "open_defecation").
#' @param shared_sanitation_col Column name (as a string) for shared sanitation facilities.
#' @param shared_sanitation_yes_val Character value for indicating shared sanitation (default: "yes").
#' @param shared_sanitation_no_val Character value for indicating non-shared sanitation (default: "no").
#'
#' @return A data frame with a new column `wash_jmp_ladder_sanitation_cat`, an ordered factor with JMP ladder categories:
#' "Basic", "Limited", "Unimproved", "Open Defecation".
#'
#' @export
#'
add_sanitation_jmp_ladder <- function(
    .dataset,
    sanitation_facility_cat_col,
    sanitation_facility_improved_val = "improved",
    sanitation_facility_unimproved_val = "unimproved",
    sanitation_facility_open_defecation_val = "open_defecation",
    shared_sanitation_col,
    shared_sanitation_yes_val = "yes",
    shared_sanitation_no_val = "no"
) {
  origin <- "add_sanitation_jmp_ladder"

  phr_try({
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

    # Validate required columns
    required_columns <- c(sanitation_facility_cat_col, shared_sanitation_col)
    phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = phr_txt("Ensure the sanitation facility and shared sanitation columns are present."),
      soft = FALSE
    )

    # Validate categories in sanitation facility column
    valid_sanitation_categories <- c(
      sanitation_facility_improved_val,
      sanitation_facility_unimproved_val,
      sanitation_facility_open_defecation_val,
      NA_character_
    )
    phr_validate_choice(
      x = .dataset[[sanitation_facility_cat_col]],
      choices = valid_sanitation_categories,
      origin = origin,
      soft = FALSE
    )

    # Validate categories in shared sanitation column
    valid_shared_categories <- c(shared_sanitation_yes_val, shared_sanitation_no_val, NA_character_)
    phr_validate_choice(
      x = .dataset[[shared_sanitation_col]],
      choices = valid_shared_categories,
      origin = origin,
      soft = FALSE
    )

    # Overwrite warnings for any existing output column
    output_column <- "wash_jmp_ladder_sanitation_cat"
    if (output_column %in% names(.dataset)) {
      phr_warning(
        origin = origin,
        message = phr_txt(glue::glue("Variable {output_column} already exists and will be overwritten."))
      )
    }

    # Use ensure_value to provide defaults
    sanitation_facility_improved_val <- ensure_value(sanitation_facility_improved_val, "improved")
    sanitation_facility_unimproved_val <- ensure_value(sanitation_facility_unimproved_val, "unimproved")
    sanitation_facility_open_defecation_val <- ensure_value(sanitation_facility_open_defecation_val, "open_defecation")
    shared_sanitation_yes_val <- ensure_value(shared_sanitation_yes_val, "yes")
    shared_sanitation_no_val <- ensure_value(shared_sanitation_no_val, "no")

    # Recoding to JMP ladder categories
    .dataset <- .dataset |>
      dplyr::mutate(
        wash_jmp_ladder_sanitation_cat = dplyr::case_when(
          as.character(.data[[sanitation_facility_cat_col]]) == sanitation_facility_open_defecation_val ~ phr_txt("Open Defecation"),
          as.character(.data[[sanitation_facility_cat_col]]) == sanitation_facility_unimproved_val ~ phr_txt("Unimproved"),
          as.character(.data[[sanitation_facility_cat_col]]) == sanitation_facility_improved_val &
            as.character(.data[[shared_sanitation_col]]) == shared_sanitation_yes_val ~ phr_txt("Limited"),
          as.character(.data[[sanitation_facility_cat_col]]) == sanitation_facility_improved_val &
            as.character(.data[[shared_sanitation_col]]) == shared_sanitation_no_val ~ phr_txt("Basic"),
          TRUE ~ NA_character_
        )
      )

    # Convert to factor with ordered levels
    .dataset <- .dataset |>
      dplyr::mutate(
        wash_jmp_ladder_sanitation_cat = factor(
          wash_jmp_ladder_sanitation_cat,
          levels = c(
            phr_txt("Basic"),
            phr_txt("Limited"),
            phr_txt("Unimproved"),
            phr_txt("Open Defecation")
          ),
          ordered = TRUE
        )
      )

    phr_message(
      origin = origin,
      message = phr_txt("JMP ladder categories for sanitation facilities computed successfully.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure input columns exist and contain valid categorical values."))
}

# Square Meters Per Person ####

#' @title Add Shelter Area, Square Meters per Person, and Categorization
#'
#' @description Computes the total area (in square meters), square meters per person, and a categorical classification of square meters per person for a shelter based on its geometric shape (rectangle or circle), dimensions, household size, and confirmation of measurement. The output includes three new columns:
#' * **area_sqm**: The total area of the shelter in square meters.
#' * **sqm_per_person**: The estimated square meters per person, calculated as area divided by household size.
#' * **sqm_per_person_cat**: A categorical variable based on the `sqm_per_person` result with the following categories:
#'   - "<3.5 sqm per person"
#'   - "3.5 - < 4.5 sqm per person"
#'   - "4.5 - < 5.5 sqm per person"
#'   - ">= 5.5 sqm per person"
#'
#' @param .dataset A data frame or tibble containing the relevant columns.
#' @param shelter_shape_col Column name (as a string) indicating the shape of the shelter.
#' @param rectangle_val A character value or vector of character values representing a rectangular shelter.
#' @param circle_val A character value or vector of character values representing a circular shelter.
#' @param shelter_length_col Column name (as a string) for the numeric length (in meters) of rectangular shelters.
#' @param shelter_width_col Column name (as a string) for the numeric width (in meters) of rectangular shelters.
#' @param shelter_diameter_col Column name (as a string) for the numeric diameter (in meters) of circular shelters.
#' @param household_size_col Column name (as a string) for the household size (number of people living in the shelter).
#' @param measure_confirm_col Column name (as a string) for a categorical column confirming if measurement is available ("yes", "no", or "dont_know").
#' @param measure_confirm_yes_val (Optional) Character value indicating "yes" in the measurement confirmation column (default is "yes").
#'
#' @return A data frame with three new columns:
#' * **area_sqm**: Calculated total area (in square meters) of the shelter.
#' * **sqm_per_person**: Square meters per person calculated as `area_sqm` divided by `household_size_col`.
#' * **sqm_per_person_cat**: Categorical classifications of `sqm_per_person`.
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   shelter_shape = c("rectangle", "circle", "rectangle", "circle", "rectangle"),
#'   shelter_length_m = c(2, NA, 5, NA, 3),
#'   shelter_width_m = c(3, NA, 2, NA, 4),
#'   shelter_diameter_m = c(NA, 3, NA, 5, NA),
#'   household_size = c(4, 3, 6, 2, 5),
#'   shelter_measured = c("yes", "no", "yes", "no", "yes")
#' )
#'
#' df_result <- add_sqm_per_person(
#'   .dataset = df,
#'   shelter_shape_col = "shelter_shape",
#'   rectangle_val = "rectangle",
#'   circle_val = "circle",
#'   shelter_length_col = "shelter_length_m",
#'   shelter_width_col = "shelter_width_m",
#'   shelter_diameter_col = "shelter_diameter_m",
#'   household_size_col = "household_size",
#'   measure_confirm_col = "shelter_measured",
#'   measure_confirm_yes_val = "yes"
#' )
#'
#' @export
add_sqm_per_person <- function(
    .dataset,
    shelter_shape_col,
    rectangle_val,
    circle_val,
    shelter_length_col,
    shelter_width_col,
    shelter_diameter_col,
    household_size_col,
    measure_confirm_col,
    measure_confirm_yes_val = "yes"
) {

  origin <- "add_sqm_per_person"

  phr_try({


    # Validate dataset

    phr_validate_dataframe(
      .dataset,
      origin = origin,
      hint = phr_txt("Ensure you pass a valid data frame as `.dataset`."),
      soft = FALSE
    )

    phr_assert(
      nrow(.dataset) > 0,
      origin = origin,
      phr_txt("Dataset is empty.")
    )


    # Validate input columns

    required_columns <- c(shelter_shape_col, household_size_col, measure_confirm_col)
    phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = phr_txt("Ensure the shape, household size, and measurement confirmation columns exist in the dataset."),
      soft = FALSE
    )

    shape_dependent_columns <- c(shelter_length_col, shelter_width_col, shelter_diameter_col)
    phr_validate_columns(
      .dataset,
      shape_dependent_columns,
      origin = origin,
      hint = phr_txt("Ensure the shelter length, width, and diameter columns exist in the dataset."),
      soft = FALSE
    )


    # Validate shape categories

    valid_shapes <- c(rectangle_val, circle_val, NA_character_)
    phr_validate_choice(
      x = .dataset[[shelter_shape_col]],
      choices = valid_shapes,
      origin = origin,
      soft = FALSE
    )


    # Validate household size column

    phr_validate_all_numeric(
      .dataset[[household_size_col]],
      origin = origin,
      hint = phr_txt("Household size column must contain numeric values."),
      soft = TRUE
    )


    # Validate measuring confirmation column

    phr_validate_choice(
      x = .dataset[[measure_confirm_col]],
      choices = c(measure_confirm_yes_val, "no", "dont_know", NA_character_),
      origin = origin,
      soft = FALSE
    )


    # Validate numeric columns

    phr_validate_all_numeric(
      .dataset[[shelter_length_col]],
      origin = origin,
      hint = phr_txt("Length column must contain numeric values."),
      soft = TRUE
    )
    phr_validate_all_numeric(
      .dataset[[shelter_width_col]],
      origin = origin,
      hint = phr_txt("Width column must contain numeric values."),
      soft = TRUE
    )
    phr_validate_all_numeric(
      .dataset[[shelter_diameter_col]],
      origin = origin,
      hint = phr_txt("Diameter column must contain numeric values."),
      soft = TRUE
    )


    # Overwrite warnings for any existing output columns

    output_columns <- c("area_sqm", "sqm_per_person", "sqm_per_person_cat")

    for (col in output_columns) {
      if (col %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt(glue::glue("Variable `{col}` already exists and will be overwritten."))
        )
      }
    }


    # Calculate square meters, square meters per person, and categories

    .dataset <- .dataset |>
      dplyr::mutate(
        # Calculate area based on shelter shape
        area_sqm = dplyr::case_when(
          # Only compute if measurement confirmed as 'yes'
          .data[[measure_confirm_col]] != measure_confirm_yes_val ~ NA_real_,

          # Calculate area for rectangle shelters
          .data[[shelter_shape_col]] %in% rectangle_val &
            !is.na(.data[[shelter_length_col]]) & !is.na(.data[[shelter_width_col]]) ~
            round((.data[[shelter_length_col]] * .data[[shelter_width_col]]), 1),

          # Calculate area for circle shelters
          .data[[shelter_shape_col]] %in% circle_val &
            !is.na(.data[[shelter_diameter_col]]) ~
            round(((.data[[shelter_diameter_col]] / 2)^2 * pi), 1),

          # Default: NA
          TRUE ~ NA_real_
        ),
        # Calculate square meters per person
        sqm_per_person = dplyr::case_when(
          !is.na(area_sqm) & !is.na(.data[[household_size_col]]) &
            .data[[household_size_col]] > 0 ~
            round(area_sqm / .data[[household_size_col]], 1),
          TRUE ~ NA_real_
        ),
        # Categorize square meters per person
        sqm_per_person_cat = dplyr::case_when(
          sqm_per_person < 3.5 ~ phr_txt("<3.5 sqm per person"),
          sqm_per_person >= 3.5 & sqm_per_person < 4.5 ~ phr_txt("3.5 - < 4.5 sqm per person"),
          sqm_per_person >= 4.5 & sqm_per_person < 5.5 ~ phr_txt("4.5 - < 5.5 sqm per person"),
          sqm_per_person >= 5.5 ~ phr_txt(">= 5.5 sqm per person"),
          TRUE ~ NA_character_
        ) |>
          factor(
            levels = c(
              phr_txt("<3.5 sqm per person"),
              phr_txt("3.5 - < 4.5 sqm per person"),
              phr_txt("4.5 - < 5.5 sqm per person"),
              phr_txt(">= 5.5 sqm per person")
            ),
            ordered = TRUE
          )
      )

    phr_message(
      origin = origin,
      message = phr_txt("Shelter area, square meters per person, and categorization successfully computed.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure input columns exist and values align with specifications."))
}

