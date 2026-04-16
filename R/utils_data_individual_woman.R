#' @title Add Maternal MUAC Categorization and Extreme Value Flags
#'
#' @description Categorizes the nutritional status of adult women based on MUAC (Mid-Upper Arm Circumference) measurements and flags extreme values.
#'
#' @details The function detects whether the MUAC values in `muac_col` are in centimeters or millimeters based on thresholds:
#' - If values are all less than 100, they are assumed to be in centimeters. The function will create or overwrite the `woman_muac_mm` column by converting the values to millimeters.
#' - If values are all greater than 100, they are assumed to be in millimeters. The function will create or overwrite the `woman_muac_cm` column by converting the values to centimeters.
#'
#' ## Categorization Logic:
#' - Only women aged 15-49 years are categorized:
#'   - **"Severe"**: MUAC < `severe_threshold_const` (default 21cm).
#'   - **"Moderate"**: MUAC >= `severe_threshold_const` and < `moderate_threshold_const` (default 23cm).
#'   - **"Normal"**: MUAC >= `moderate_threshold_const`.
#' - Women outside the 15-49 age range are assigned `NA` for the `woman_muac_cat` column.
#'
#' ## Extreme Flagging:
#' Flags extreme MUAC values:
#' - `flag_woman_muac_extreme` = 1 if MUAC < 10cm or MUAC >= 70cm.
#'
#' @param .dataset A data frame or tibble containing the required columns.
#' @param age_years_col Column name (character) indicating the age of individuals in years.
#' @param muac_col Column name (character) of the MUAC measurements.
#' @param severe_threshold_const Numeric constant for the cutoff for severe malnutrition. Default is 21cm.
#' @param moderate_threshold_const Numeric constant for the cutoff for moderate malnutrition. Default is 23cm.
#'
#' @return A data frame or tibble with the following new columns:
#' - **woman_muac_cat**: Categorization of nutritional status as "Severe", "Moderate", "Normal", or `NA`.
#' - **flag_woman_muac_extreme**: Flag for extreme MUAC values (1 for MUAC < 10cm or >= 70cm, 0 otherwise).
#' - **woman_muac_mm**: MUAC values in millimeters (if input was in centimeters).
#' - **woman_muac_cm**: MUAC values in centimeters (if input was in millimeters).
#'
#' @examples
#' df <- data.frame(
#'   age_years = c(25, 18, 50, 14),
#'   muac = c(17, 250, 19, 26) # Mixed units, second row in millimeters
#' )
#'
#' result <- add_maternal_muac(
#'   df,
#'   age_years_col = "age_years",
#'   muac_col = "muac"
#' )
#' print(result)
#'
#' @export
add_maternal_muac <- function(
    .dataset,
    age_years_col,
    muac_col,
    severe_threshold_const = 21,
    moderate_threshold_const = 23
) {
  origin <- "add_maternal_muac"

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


    # Validate input columns

    required_columns <- c(age_years_col, muac_col)
    phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = phr_txt("Ensure the specified columns for age and MUAC exist in the dataset."),
      soft = FALSE
    )

    phr_validate_all_numeric(
      .dataset[[muac_col]],
      origin = origin,
      hint = phr_txt("The `muac_col` column must contain numeric values."),
      soft = TRUE
    )

    phr_validate_all_numeric(
      .dataset[[age_years_col]],
      origin = origin,
      hint = phr_txt("The `age_years_col` column must contain numeric values."),
      soft = TRUE
    )


    # Detect MUAC Units and Add/Overwrite Columns

    muac_is_cm <- all(.dataset[[muac_col]] < 100, na.rm = TRUE)
    muac_is_mm <- all(.dataset[[muac_col]] > 100, na.rm = TRUE)

    if (muac_is_cm) {
      # Add or overwrite millimeter column
      if ("woman_muac_mm" %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt("The column `woman_muac_mm` already exists and will be overwritten.")
        )
      }
      .dataset <- .dataset |>
        dplyr::mutate(
          woman_muac_mm = .data[[muac_col]] * 10
        )
    } else if (muac_is_mm) {
      # Add or overwrite centimeter column
      if ("woman_muac_cm" %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt("The column `woman_muac_cm` already exists and will be overwritten.")
        )
      }
      .dataset <- .dataset |>
        dplyr::mutate(
          woman_muac_cm = .data[[muac_col]] / 10
        )
    } else {
      phr_warning(
        origin = origin,
        message = phr_txt("MUAC units could not be definitively identified as centimeters or millimeters.")
      )
    }


    # Overwrite warnings for additional output columns

    output_columns <- c("woman_muac_cat", "flag_woman_muac_extreme")

    for (col in output_columns) {
      if (col %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt(glue::glue("Variable `{col}` already exists and will be overwritten."))
        )
      }
    }


    # Categorize MUAC and Flag Extremes

    .dataset <- .dataset |>
      dplyr::mutate(
        # Converting values to centimeters for categorization if required
        muac_for_classification = if (muac_is_mm) .data$woman_muac_cm else .data[[muac_col]],

        # Apply categorization only to women aged 15-49 years
        woman_muac_cat = dplyr::case_when(
          .data[[age_years_col]] < 15 | .data[[age_years_col]] > 49 ~ NA_character_,
          muac_for_classification < severe_threshold_const ~ "Severe",
          muac_for_classification >= severe_threshold_const & muac_for_classification < moderate_threshold_const ~ "Moderate",
          muac_for_classification >= moderate_threshold_const ~ "Normal",
          TRUE ~ NA_character_
        ),

        # Flag extreme MUAC values (< 10cm or >= 70cm using base muac column)
        flag_woman_muac_extreme = dplyr::case_when(
          .data[[muac_col]] < 10 | .data[[muac_col]] >= 70 ~ 1,
          TRUE ~ 0
        )
      )

    phr_message(
      origin = origin,
      message = phr_txt("Maternal nutritional status based on MUAC successfully categorized.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure input columns exist, contain valid numeric data, and the MUAC values are within realistic thresholds."))
}
