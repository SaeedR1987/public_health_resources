#' @title Add LPPD Correction Factor
#'
#' @description Calculates the LPPD correction factor based on the number of days collected, resulting in a new column `lppd_correction_factor` in the dataset.
#'
#' @details The function takes the number of days collected from the specified column, divides these by 7, and rounds the result to 3 decimal places. Values outside the valid range [0, 7] are ignored, and their corresponding entries in the output column are set to `NA`.
#'
#' @param .dataset A data frame or tibble containing the required column.
#' @param num_days_collect_col Column name (character) indicating the number of days collected. This column must contain numeric values in the range 0-7.
#'
#' @return A data frame or tibble with the following new column:
#' * **lppd_correction_factor**: The calculated correction factor based on the formula:
#'   - `num_days_collected / 7`, rounded to 3 decimal places.
#'   - Set to `NA` for invalid or out-of-range values.
#'
#' @examples
#' # Example dataset
#' df1 <- data.frame(
#'   num_days_collected = c(0, 3, 7, -1, 8, NA)
#' )
#'
#' # Add LPPD correction factor
#' df_result <- add_lppd_correction_factor(
#'   .dataset = df1,
#'   num_days_collect_col = "num_days_collected"
#' )
#'
#' @export
add_lppd_correction_factor <- function(
    .dataset,
    num_days_collect_col
) {
  origin <- "add_lppd_correction_factor"

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
      num_days_collect_col,
      origin = origin,
      hint = phr_txt("Ensure the specified `num_days_collect_col` exists in the dataset."),
      soft = FALSE
    )

    phr_validate_all_numeric(
      .dataset[[num_days_collect_col]],
      origin = origin,
      hint = phr_txt("The `num_days_collect_col` column must contain numeric values."),
      soft = TRUE
    )


    # Overwrite warning for an existing output column

    output_column <- "lppd_correction_factor"

    if (output_column %in% names(.dataset)) {
      phr_warning(
        origin = origin,
        message = phr_txt(glue::glue("The column `{output_column}` already exists and will be overwritten."))
      )
    }


    # Calculate lppd_correction_factor

    .dataset <- .dataset |>
      dplyr::mutate(
        lppd_correction_factor = dplyr::case_when(
          # Values within the valid range [0, 7]
          !is.na(.data[[num_days_collect_col]]) &
            .data[[num_days_collect_col]] >= 0 &
            .data[[num_days_collect_col]] <= 7 ~
            round(.data[[num_days_collect_col]] / 7, 3),
          # Values outside the valid range [0, 7]
          TRUE ~ NA_real_
        )
      )

    phr_message(
      origin = origin,
      message = phr_txt("LPPD correction factor successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure `num_days_collect_col` exists and contains valid numeric values within the range 0-7."))
}

#' @title Add Total Daily Liters Collected
#'
#' @description Calculates the total daily liters of water collected based on the size of a container, the number of journeys, and an optional correction factor.
#'
#' @details The function adds a new column `wash_container_total_litres` to the dataset. This column calculates the daily water collection using the formula:
#'   - `wash_container_size_liters_col` * `wash_container_num_journeys_col`
#' If a correction factor column is provided, this value is further adjusted using:
#'   - `wash_container_total_litres` * `correction_factor_col`.
#'
#' @param .dataset A data frame or tibble containing the required columns.
#' @param wash_container_size_liters_col Column name (character) indicating the size of the container in liters. This column must contain numeric values.
#' @param wash_container_num_journeys_col Column name (character) indicating the number of journeys made. This column must contain numeric values.
#' @param correction_factor_col (Optional) Column name (character) of a correction factor to adjust the total liters collected. This column must contain numeric values.
#'
#' @return A data frame or tibble with the following new column:
#' * **wash_container_total_litres**: The calculated total liters collected daily.
#'
#' @examples
#' # Example dataset
#' df1 <- data.frame(
#'   container_size_liters = c(10, 5, 15),
#'   num_journeys = c(2, 3, NA),
#'   correction_factor = c(1, 0.8, NA)
#' )
#'
#' # Add total daily liters collected (without correction factor)
#' df_result <- add_total_daily_liters(
#'   .dataset = df1,
#'   wash_container_size_liters_col = "container_size_liters",
#'   wash_container_num_journeys_col = "num_journeys"
#' )
#' print(df_result)
#'
#' # Add total daily liters collected (with correction factor)
#' df_result <- add_total_daily_liters(
#'   .dataset = df1,
#'   wash_container_size_liters_col = "container_size_liters",
#'   wash_container_num_journeys_col = "num_journeys",
#'   correction_factor_col = "correction_factor"
#' )
#' print(df_result)
#'
#' @export
add_total_daily_liters <- function(
    .dataset,
    wash_container_size_liters_col,
    wash_container_num_journeys_col,
    correction_factor_col = NULL
) {
  origin <- "add_total_daily_liters"

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

    required_columns <- c(wash_container_size_liters_col, wash_container_num_journeys_col)
    phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = phr_txt("Ensure the specified columns for container size and number of journeys exist in the dataset."),
      soft = FALSE
    )

    # Check if the required columns contain numeric values
    phr_validate_all_numeric(
      .dataset[[wash_container_size_liters_col]],
      origin = origin,
      hint = phr_txt("The `wash_container_size_liters_col` column must contain numeric values."),
      soft = TRUE
    )
    phr_validate_all_numeric(
      .dataset[[wash_container_num_journeys_col]],
      origin = origin,
      hint = phr_txt("The `wash_container_num_journeys_col` column must contain numeric values."),
      soft = TRUE
    )

    # If correction_factor_col is provided, validate it
    if (!is.null(correction_factor_col)) {
      phr_validate_columns(
        .dataset,
        correction_factor_col,
        origin = origin,
        hint = phr_txt("Ensure the specified correction factor column exists in the dataset."),
        soft = FALSE
      )

      phr_validate_all_numeric(
        .dataset[[correction_factor_col]],
        origin = origin,
        hint = phr_txt("The `correction_factor_col` column must contain numeric values."),
        soft = TRUE
      )
    }


    # Overwrite warning for an existing output column

    output_column <- "wash_container_total_litres"

    if (output_column %in% names(.dataset)) {
      phr_warning(
        origin = origin,
        message = phr_txt(glue::glue("The column `{output_column}` already exists and will be overwritten."))
      )
    }


    # Calculate total daily litres

    .dataset <- .dataset |>
      dplyr::mutate(
        wash_container_total_litres = dplyr::case_when(
          !is.na(.data[[wash_container_size_liters_col]]) & !is.na(.data[[wash_container_num_journeys_col]]) ~
            .data[[wash_container_size_liters_col]] * .data[[wash_container_num_journeys_col]],
          TRUE ~ NA_real_
        )
      )

    # If correction_factor_col is provided, apply correction factor
    if (!is.null(correction_factor_col)) {
      .dataset <- .dataset |>
        dplyr::mutate(
          wash_container_total_litres = dplyr::case_when(
            !is.na(wash_container_total_litres) & !is.na(.data[[correction_factor_col]]) ~
              wash_container_total_litres * .data[[correction_factor_col]],
            TRUE ~ wash_container_total_litres
          )
        )
    }

    phr_message(
      origin = origin,
      message = phr_txt("Total daily liters successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure all input columns exist and contain valid numeric data."))
}
