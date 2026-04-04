#' @title Add Age Categories
#'
#' @description Categorizes individuals into 5-year age bins based on their age in years.
#'
#' @details The function adds a new factor column `age_cat` to the dataset, where age ranges are calculated in 5-year intervals (e.g., 0-4 years, 5-9 years, etc.) up to a maximum of 120 years. The bins are created as:
#'  - "0-4", "5-9", ..., "120-124".
#' Any age values outside the range [0, 125) are treated as missing.
#'
#' @param .dataset A data frame or tibble containing the required column.
#' @param age_years_col Column name (character) indicating the age of individuals in years.
#'
#' @return A data frame or tibble with the following new column:
#' * **age_cat**: A factor column indicating the 5-year age category of each individual.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   age_years = c(0, 3, 7, 10, 45, 87, 122, NA)
#' )
#'
#' # Add age categories
#' df_result <- add_age_cat(
#'   .dataset = df,
#'   age_years_col = "age_years"
#' )
#' print(df_result)
#'
#' @export
add_age_cat <- function(
    .dataset,
    age_years_col
) {
  origin <- "add_age_cat"

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


    # Validate input column

    phr_validate_columns(
      .dataset,
      age_years_col,
      origin = origin,
      hint = phr_txt("Ensure the specified `age_years_col` exists in the dataset."),
      soft = FALSE
    )

    phr_validate_all_numeric(
      .dataset[[age_years_col]],
      origin = origin,
      hint = phr_txt("The `age_years_col` column must contain numeric values."),
      soft = TRUE
    )


    # Overwrite warning for existing output column

    output_column <- "age_cat"

    if (output_column %in% names(.dataset)) {
      phr_warning(
        origin = origin,
        message = phr_txt(glue::glue("The column `{output_column}` already exists and will be overwritten."))
      )
    }


    # Create age categories

    .dataset <- .dataset %>%
      dplyr::mutate(
        age_cat = cut(
          .data[[age_years_col]],
          breaks = seq(0, 125, by = 5),
          right = FALSE,
          labels = paste0(seq(0, 120, by = 5), "-", seq(4, 124, by = 5)),
          include.lowest = TRUE
        )
      )

    phr_message(
      origin = origin,
      message = phr_txt("Age categories successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure `age_years_col` exists and contains valid numeric data."))
}

#' @title Add Age Categories in Months
#'
#' @description Categorizes individuals into specific age bins based on their age in months (0-59 months).
#'
#' @details The function adds a new factor column `age_months_cat` to the dataset, where age ranges are categorized into the following specific bins:
#' * `0-5 months`
#' * `6-11 months`
#' * `12-17 months`
#' * `18-23 months`
#' * `24-29 months`
#' * `30-35 months`
#' * `36-41 months`
#' * `42-47 months`
#' * `48-53 months`
#' * `54-59 months`.
#' Any age values outside the range [0, 60) are treated as missing.
#'
#' @param .dataset A data frame or tibble containing the required column.
#' @param age_months_col Column name (character) indicating the age of individuals in months.
#'
#' @return A data frame or tibble with the following new column:
#' * **age_months_cat**: A factor column indicating the specific age category in months of each individual.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   age_months = c(0, 5, 7, 12, 23, 40, 59, NA)
#' )
#'
#' # Add age categories in months
#' df_result <- add_age_months_cat(
#'   .dataset = df,
#'   age_months_col = "age_months"
#' )
#' print(df_result)
#'
#' @export
add_age_months_cat <- function(
    .dataset,
    age_months_col
) {
  origin <- "add_age_months_cat"

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


    # Validate input column

    phr_validate_columns(
      .dataset,
      age_months_col,
      origin = origin,
      hint = phr_txt("Ensure the specified `age_months_col` exists in the dataset."),
      soft = FALSE
    )

    # Check if all values in the specified column are NA
    phr_assert(
      !all(is.na(.dataset[[age_months_col]])),
      origin = origin,
      phr_txt(paste("The column", age_months_col, "contains only NA values. Ensure it contains valid numeric data."))
    )

    phr_validate_all_numeric(
      .dataset[[age_months_col]],
      origin = origin,
      hint = phr_txt("The `age_months_col` column must contain numeric values."),
      soft = TRUE
    )


    # Overwrite warning for existing output columns

    output_columns <- c("age_months_cat", "age_6_29m", "age_30_59m")

    for (output_column in output_columns) {
      if (output_column %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt(glue::glue("The column `{output_column}` already exists and will be overwritten."))
        )
      }
    }


    # Create age categories in months

    .dataset <- .dataset %>%
      dplyr::mutate(
        age_months_cat = cut(
          .data[[age_months_col]],
          breaks = c(0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60),
          right = FALSE,
          labels = c(
            "0-5 months", "6-11 months", "12-17 months", "18-23 months",
            "24-29 months", "30-35 months", "36-41 months", "42-47 months",
            "48-53 months", "54-59 months"
          )
        ),
        age_6_29m = dplyr::case_when(
          .data[[age_months_col]] < 6 ~ NA_real_,
          .data[[age_months_col]] >= 6 & .data[[age_months_col]] <= 29 ~ 1,
          .data[[age_months_col]] >= 30 & .data[[age_months_col]] <= 59 ~ 0,
          TRUE ~ NA_real_
        ),
        age_30_59m = dplyr::case_when(
          .data[[age_months_col]] < 6 ~ NA_real_,
          .data[[age_months_col]] >= 6 & .data[[age_months_col]] <= 29 ~ 0,
          .data[[age_months_col]] >= 30 & .data[[age_months_col]] <= 59 ~ 1,
          TRUE ~ NA_real_
        )
      )

    phr_message(
      origin = origin,
      message = phr_txt("Age categories in months successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure `age_months_col` exists and contains valid numeric data."))
}

#' @title Add Standardized Age Calculations to a Dataset
#'
#' @description This function adds standardized age calculations to a dataset. It creates new columns for calculated age in years, months, and days (`calc_age_years`, `calc_age_months`, `calc_age_days`), consolidates exact and approximate dates of birth and death into final columns (`calc_date_birth_final`, `calc_date_death_final`), and optionally calculates a `roster_birth` column based on a recall date.
#'
#' @details
#' - The function computes `calc_date_birth_final` based on exact, approximate, or final dates of birth. If both exact and approximate dates are provided, the exact date is prioritized. If no exact or approximate date is provided, the final column is used (if available).
#' - Similarly, `calc_date_death_final` is computed using exact, approximate, or final dates of death, with priority given to exact dates, followed by approximate dates, and then the final column.
#' - If `date_birth_final_col` or `date_death_final_col` are provided, they are used as fallback values for `calc_date_birth_final` and `calc_date_death_final`, respectively.
#' - Optionally, if `date_recall_col` and a valid `calc_date_birth_final` are available, the function calculates a `roster_birth` column, which has a value of `1` if the calculated or provided date of birth is on or after the recall date, `0` otherwise.
#' - Based on the birth and/or death dates, or the provided survey date, the function calculates:
#'   - `calc_age_years`: Age in years.
#'   - `calc_age_months`: Age in months.
#'   - `calc_age_days`: Age in days.
#' - When no dates are provided, the function uses fallback values for `calc_age_years` from the `age_years_col` and for `calc_age_months` from the `age_months_col` (if available); otherwise, `calc_age_months` defaults to `NA`.
#' - If any required column is not provided, the corresponding calculation is skipped.
#'
#' @param .dataset A data frame or tibble containing the input data.
#' @param age_years_col Character. The column name for the age in years. This is a mandatory argument.
#' @param age_months_col Character (optional). The column name for the age in months. This is used as a fallback for `calc_age_months` if birth/death dates are not available.
#' @param date_birth_approx_col Character (optional). The column name for the approximate date of birth.
#' @param date_birth_exact_col Character (optional). The column name for the exact date of birth.
#' @param date_birth_final_col Character (optional). The column name for the final date of birth. Used as a fallback if no exact/approximate dates are provided.
#' @param survey_date_col Character (optional). The column name for the survey date.
#' @param date_death_approx_col Character (optional). The column name for the approximate date of death.
#' @param date_death_exact_col Character (optional). The column name for the exact date of death.
#' @param date_death_final_col Character (optional). The column name for the final date of death. Used as a fallback if no exact/approximate dates are provided.
#' @param date_recall_col Character (optional). The column name for the recall date. When provided alongside a valid birth date column, it is used to populate the `roster_birth` column.
#'
#' @return A data frame or tibble with the following additional columns:
#' - `calc_date_birth_final`: The final date of birth (exact, approximate, or final date).
#' - `calc_date_death_final`: The final date of death (exact, approximate, or final date).
#' - `calc_age_years`: The calculated age in years.
#' - `calc_age_months`: The calculated age in months.
#' - `calc_age_days`: The calculated age in days.
#' - `roster_birth`: A binary column (`1` or `0`) indicating whether the date of birth is on or after the recall date. If either the birth date or recall date is missing, this column is not calculated.
#'
#' @examples
#' # Example input data
#' test_data <- tibble::tibble(
#'   age_years = c(10, 15, 20),
#'   age_months = c(120, 180, 240),
#'   survey_date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
#'   date_birth_approx = as.Date(c("2013-01-01", "2008-01-01", NA)),
#'   date_birth_exact = as.Date(c("2013-01-02", NA, "2003-01-01")),
#'   date_birth_final = as.Date(c(NA, "2008-06-01", "2003-12-31")),
#'   date_death_approx = as.Date(c(NA, NA, "2023-01-05")),
#'   date_death_exact = as.Date(c(NA, "2022-12-15", "2023-01-05")),
#'   date_recall = as.Date(c("2010-01-01", "2009-01-01", "1995-01-01"))
#' )
#'
#' # Call the function
#' result <- add_standardized_age(
#'   .dataset = test_data,
#'   age_years_col = "age_years",
#'   age_months_col = "age_months",
#'   date_birth_approx_col = "date_birth_approx",
#'   date_birth_exact_col = "date_birth_exact",
#'   date_birth_final_col = "date_birth_final",
#'   survey_date_col = "survey_date",
#'   date_death_approx_col = "date_death_approx",
#'   date_death_exact_col = "date_death_exact",
#'   date_recall_col = "date_recall"
#' )
#'
#' # Print result
#' print(result)
#'
#' @importFrom lubridate interval year month
#' @importFrom dplyr mutate case_when
#' @export
add_standardized_age <- function(
    .dataset,
    age_years_col,
    age_months_col = NULL,
    date_birth_approx_col = NULL,
    date_birth_exact_col = NULL,
    date_birth_final_col = NULL,
    survey_date_col = NULL,
    date_death_approx_col = NULL,
    date_death_exact_col = NULL,
    date_death_final_col = NULL,
    date_recall_col = NULL
) {

  origin <- "add_standardized_age"

  phr_try({

    # Validate the dataset
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

    # Validate mandatory and optional columns
    mandatory_cols <- c(age_years_col)

    phr_validate_columns(
      .dataset,
      mandatory_cols,
      origin = origin,
      hint = phr_txt("Ensure the age years column exists in the dataset."),
      soft = FALSE
    )

    # Set optional column arguments to NULL if they don't exist in the dataset
    if (!is.null(age_months_col) && !age_months_col %in% names(.dataset)) {
      age_months_col <- NULL
    }
    if (!is.null(date_birth_approx_col) && !date_birth_approx_col %in% names(.dataset)) {
      date_birth_approx_col <- NULL
    }
    if (!is.null(date_birth_exact_col) && !date_birth_exact_col %in% names(.dataset)) {
      date_birth_exact_col <- NULL
    }
    if (!is.null(date_birth_final_col) && !date_birth_final_col %in% names(.dataset)) {
      date_birth_final_col <- NULL
    }
    if (!is.null(survey_date_col) && !survey_date_col %in% names(.dataset)) {
      survey_date_col <- NULL
    }
    if (!is.null(date_death_approx_col) && !date_death_approx_col %in% names(.dataset)) {
      date_death_approx_col <- NULL
    }
    if (!is.null(date_death_exact_col) && !date_death_exact_col %in% names(.dataset)) {
      date_death_exact_col <- NULL
    }
    if (!is.null(date_death_final_col) && !date_death_final_col %in% names(.dataset)) {
      date_death_final_col <- NULL
    }
    if (!is.null(date_recall_col) && !date_recall_col %in% names(.dataset)) {
      date_recall_col <- NULL
    }

    optional_cols <- c(
      if (!is.null(age_months_col)) age_months_col,
      if (!is.null(date_birth_approx_col)) date_birth_approx_col,
      if (!is.null(date_birth_exact_col)) date_birth_exact_col,
      if (!is.null(date_birth_final_col)) date_birth_final_col,
      if (!is.null(survey_date_col)) survey_date_col,
      if (!is.null(date_death_approx_col)) date_death_approx_col,
      if (!is.null(date_death_exact_col)) date_death_exact_col,
      if (!is.null(date_death_final_col)) date_death_final_col,
      if (!is.null(date_recall_col)) date_recall_col
    )

    if (length(optional_cols) > 0) {
      phr_validate_columns(
        .dataset,
        optional_cols,
        origin = origin,
        hint = phr_txt("Ensure optional columns for age, birth, death, and recall dates exist if provided."),
        soft = TRUE
      )
    }

    # Convert dates using the custom `convert_date` function for only the provided date-related columns
    cols_to_convert <- c(
      if (!is.null(date_birth_approx_col)) date_birth_approx_col,
      if (!is.null(date_birth_exact_col)) date_birth_exact_col,
      if (!is.null(date_birth_final_col)) date_birth_final_col,
      if (!is.null(survey_date_col)) survey_date_col,
      if (!is.null(date_death_approx_col)) date_death_approx_col,
      if (!is.null(date_death_exact_col)) date_death_exact_col,
      if (!is.null(date_death_final_col)) date_death_final_col,
      if (!is.null(date_recall_col)) date_recall_col
    )

    if (length(cols_to_convert) > 0) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          dplyr::across(all_of(cols_to_convert), ~ phr_convert_date(.x))
        )
    }

    # Add overwrite warnings for the calculated output columns
    output_cols <- c("calc_date_birth_final", "calc_date_death_final", "calc_age_years", "calc_age_months", "calc_age_days", "roster_birth")
    for (col in output_cols) {
      if (col %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt(glue::glue("Variable {col} already exists and will be overwritten."))
        )
      }
    }

    # Compute `calc_date_birth_final` taking into account final columns
    # Only include columns that are not NULL and exist in the dataset
    birth_cols <- c(
      if (!is.null(date_birth_exact_col) && date_birth_exact_col %in% names(.dataset)) date_birth_exact_col,
      if (!is.null(date_birth_approx_col) && date_birth_approx_col %in% names(.dataset)) date_birth_approx_col,
      if (!is.null(date_birth_final_col) && date_birth_final_col %in% names(.dataset)) date_birth_final_col
    )

    if (length(birth_cols) > 0) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          calc_date_birth_final = dplyr::coalesce(!!!rlang::syms(birth_cols))
        )
    }

    # Compute `calc_date_death_final` taking into account final columns
    # Only include columns that are not NULL and exist in the dataset
    death_cols <- c(
      if (!is.null(date_death_exact_col) && date_death_exact_col %in% names(.dataset)) date_death_exact_col,
      if (!is.null(date_death_approx_col) && date_death_approx_col %in% names(.dataset)) date_death_approx_col,
      if (!is.null(date_death_final_col) && date_death_final_col %in% names(.dataset)) date_death_final_col
    )

    if (length(death_cols) > 0) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          calc_date_death_final = dplyr::coalesce(!!!rlang::syms(death_cols))
        )
    }

    # Calculate age columns
    if (!is.null(date_birth_approx_col) || !is.null(date_birth_exact_col) || !is.null(date_birth_final_col)) {
      if (!is.null(survey_date_col)) {
        # Check if both survey_date_col and calc_date_death_final are available
        if ("calc_date_death_final" %in% names(.dataset)) {
          .dataset <- .dataset %>%
            dplyr::mutate(
              calc_age_years = dplyr::case_when(
                !is.na(calc_date_birth_final) & !is.na(calc_date_death_final) ~
                  ceiling(as.numeric(difftime(calc_date_death_final, calc_date_birth_final, units = "days")) / 365.25),
                !is.na(calc_date_birth_final) ~
                  ceiling(as.numeric(difftime(.data[[survey_date_col]], calc_date_birth_final, units = "days")) / 365.25),
                TRUE ~ .data[[age_years_col]]
              ),
              calc_age_months = dplyr::case_when(
                !is.na(calc_date_birth_final) & !is.na(calc_date_death_final) ~
                  ceiling(as.numeric(difftime(calc_date_death_final, calc_date_birth_final, units = "days")) / 30.44),
                !is.na(calc_date_birth_final) ~
                  ceiling(as.numeric(difftime(.data[[survey_date_col]], calc_date_birth_final, units = "days")) / 30.44),
                TRUE ~ NA_real_
              ),
              calc_age_days = dplyr::case_when(
                !is.na(calc_date_birth_final) & !is.na(calc_date_death_final) ~
                  floor(as.numeric(difftime(calc_date_death_final, calc_date_birth_final, units = "days"))),
                !is.na(calc_date_birth_final) ~
                  floor(as.numeric(difftime(.data[[survey_date_col]], calc_date_birth_final, units = "days"))),
                TRUE ~ NA_real_
              )
            )
          if (!is.null(age_months_col)) {
            .dataset <- .dataset %>%
              dplyr::mutate(
                calc_age_months = dplyr::coalesce(calc_age_months, ceiling(.data[[age_months_col]])),
                calc_age_days = dplyr::coalesce(calc_age_days, floor(as.numeric(.data[[age_months_col]] * 30.44)))
              )
          }
        } else {
          .dataset <- .dataset %>%
            dplyr::mutate(
              calc_age_years = dplyr::case_when(
                !is.na(calc_date_birth_final) ~
                  ceiling(as.numeric(difftime(.data[[survey_date_col]], calc_date_birth_final, units = "days")) / 365.25),
                TRUE ~ .data[[age_years_col]]
              ),
              calc_age_months = dplyr::case_when(
                !is.na(calc_date_birth_final) ~
                  ceiling(as.numeric(difftime(.data[[survey_date_col]], calc_date_birth_final, units = "days")) / 30.44),
                TRUE ~ NA_real_
              ),
              calc_age_days = dplyr::case_when(
                !is.na(calc_date_birth_final) ~
                  floor(as.numeric(difftime(.data[[survey_date_col]], calc_date_birth_final, units = "days"))),
                TRUE ~ NA_real_
              )
            )
          if (!is.null(age_months_col)) {
            .dataset <- .dataset %>%
              dplyr::mutate(
                calc_age_months = dplyr::coalesce(calc_age_months, ceiling(.data[[age_months_col]])),
                calc_age_days = dplyr::coalesce(calc_age_days, floor(as.numeric(.data[[age_months_col]] * 30.44)))
              )
          }
        }
      }
    } else {
      .dataset <- .dataset %>%
        dplyr::mutate(
          calc_age_years = .data[[age_years_col]]
        )
      if (!is.null(age_months_col)) {
        .dataset <- .dataset %>%
          dplyr::mutate(
            calc_age_months = ceiling(.data[[age_months_col]]),
            calc_age_days = floor(as.numeric(.data[[age_months_col]] * 30.44))
          )
      }
    }

    # Calculate `roster_birth` column if `calc_date_birth_final` is valid and `date_recall_col` is provided
    if ((!"calc_date_birth_final" %in% names(.dataset) && is.null(date_birth_final_col)) || is.null(date_recall_col)) {
      phr_message(
        origin = origin,
        message = phr_txt("roster_birth calculation skipped as required birth or recall date information is missing.")
      )
    } else {
      .dataset <- .dataset %>%
        dplyr::mutate(
          roster_birth = dplyr::case_when(
            !is.na(calc_date_birth_final) & !is.na(.data[[date_recall_col]]) &
              calc_date_birth_final >= .data[[date_recall_col]] ~ 1,
            !is.na(calc_date_birth_final) & !is.na(.data[[date_recall_col]]) ~ 0,
            TRUE ~ NA_real_
          )
        )
    }

    phr_message(
      origin = origin,
      message = phr_txt("Standardized age columns and roster_birth added successfully.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure input data and columns are valid."))
}


#' @title Add Standardized Roster Demographics
#'
#' @description This function adds standardized demographic indicator columns to roster data.
#' Similar to how `add_standardized_deaths` works for death data, this creates binary/numeric
#' columns that can be easily aggregated to household level.
#'
#' @details
#' The function creates the following canonical columns for roster data:
#' - **roster_child_under2**: `1` if age < 2 years, `0` otherwise
#' - **roster_child_under5**: `1` if age < 5 years, `0` otherwise
#' - **roster_male**: `1` if sex matches male value, `0` otherwise
#' - **roster_female**: `1` if sex matches female value, `0` otherwise
#' - **roster_woman_15to49**: `1` if female aged 15-49, `0` otherwise
#'
#' Note: This function expects `calc_age_years` to exist (created by `add_standardized_age`).
#'
#' @param .dataset A data frame or tibble containing roster data
#' @param age_years_col Column name (character) for age in years. Defaults to "calc_age_years"
#' @param sex_col Column name (character) for sex/gender
#' @param male_val Value(s) representing male in sex_col (character vector)
#' @param female_val Value(s) representing female in sex_col (character vector)
#'
#' @return A data frame with added roster demographic columns
#'
#' @examples
#' df <- data.frame(
#'   person_id = 1:5,
#'   calc_age_years = c(1, 4, 8, 25, 40),
#'   sex = c("M", "F", "M", "F", "F")
#' )
#'
#' result <- add_standardized_roster_demographics(
#'   .dataset = df,
#'   age_years_col = "calc_age_years",
#'   sex_col = "sex",
#'   male_val = "M",
#'   female_val = "F"
#' )
#'
#' @export
add_standardized_roster_demographics <- function(
    .dataset,
    age_years_col = "calc_age_years",
    sex_col = NULL,
    male_val = NULL,
    female_val = NULL
) {

  origin <- "add_standardized_roster_demographics"

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

    # Validate age column
    phr_validate_columns(
      .dataset,
      age_years_col,
      origin = origin,
      hint = phr_txt("Ensure age_years_col exists. Run add_standardized_age first."),
      soft = FALSE
    )

    # Use ensure_value for defaults
    male_val <- ensure_value(male_val, c("m", "male", "1", "M", "Male"))
    female_val <- ensure_value(female_val, c("f", "female", "2", "F", "Female"))

    # Warn about overwriting existing columns
    output_cols <- c(
      "roster_child_under2", "roster_child_under5",
      "roster_male", "roster_female", "roster_woman_15to49"
    )

    for (col in output_cols) {
      if (col %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt(glue::glue("Column `{col}` already exists and will be overwritten."))
        )
      }
    }

    # Add age-based columns
    .dataset <- .dataset %>%
      dplyr::mutate(
        roster_child_under2 = dplyr::if_else(
          !is.na(.data[[age_years_col]]) & .data[[age_years_col]] < 2,
          1, 0
        ),
        roster_child_under5 = dplyr::if_else(
          !is.na(.data[[age_years_col]]) & .data[[age_years_col]] < 5,
          1, 0
        )
      )

    # Add sex-based columns if sex_col is provided
    if (!is.null(sex_col) && sex_col %in% names(.dataset)) {

      # Standardize sex values for comparison
      .dataset <- .dataset %>%
        dplyr::mutate(
          .sex_std = tolower(trimws(as.character(.data[[sex_col]])))
        )

      # Create sex columns
      .dataset <- .dataset %>%
        dplyr::mutate(
          roster_male = dplyr::if_else(
            .sex_std %in% tolower(male_val),
            1, 0
          ),
          roster_female = dplyr::if_else(
            .sex_std %in% tolower(female_val),
            1, 0
          ),
          roster_woman_15to49 = dplyr::if_else(
            .sex_std %in% tolower(female_val) &
              !is.na(.data[[age_years_col]]) &
              .data[[age_years_col]] >= 15 &
              .data[[age_years_col]] <= 49,
            1, 0
          )
        ) %>%
        dplyr::select(-.sex_std)

    } else {
      # If no sex column, create columns with 0
      phr_warning(
        origin = origin,
        message = phr_txt("Sex column not provided or not found. Sex-based columns will be set to 0.")
      )

      .dataset <- .dataset %>%
        dplyr::mutate(
          roster_male = 0,
          roster_female = 0,
          roster_woman_15to49 = 0
        )
    }

    phr_message(
      origin = origin,
      message = phr_txt("Standardized roster demographic columns added successfully.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure age and sex columns are valid."))
}

