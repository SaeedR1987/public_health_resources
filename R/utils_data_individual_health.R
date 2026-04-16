#' @title Add Foregone Care Category
#'
#' @description Categorizes individuals based on their health care needs and whether they received care.
#'
#' @details The function adds a new column `health_foregone_care_cat` to the dataset. The values in this column are determined based on the following rules:
#' * **"No need"**: The individual had no illness or health care need (`illness_no_val` for illness), or if they had no health need but responded `care_dontknow_val` about care received.
#' * **"Met need"**: The individual had a health care need (`illness_yes_val` for illness) and received care (`care_yes_val` for received care).
#' * **"Foregone care"**: The individual had a health care need (`illness_yes_val` for illness) but did not receive care (`care_no_val` for received care).
#' * **`NA`**: If the response to receiving care is `care_dontknow_val`, or if the response about a health care need is `illness_dontknow_val`, a classification cannot be made.
#'
#' @param .dataset A data frame or tibble containing the required columns.
#' @param ind_illness_col Column name (character) indicating whether the individual had a health care need or illness.
#' @param ind_received_care_col Column name (character) indicating whether the individual received health care.
#' @param illness_yes_val Character value for "yes" indicating a positive response for the illness column.
#' @param illness_no_val Character value for "no" indicating a negative response for the illness column.
#' @param illness_dontknow_val Character value for "dont know" indicating an unknown response for the illness column.
#' @param care_yes_val Character value for "yes" indicating a positive response for the received care column.
#' @param care_no_val Character value for "no" indicating a negative response for the received care column.
#' @param care_dontknow_val Character value for "dont know" indicating an unknown response for the received care column.
#'
#' @return A data frame or tibble with the following new column:
#' * **health_foregone_care_cat**: Categorization of foregone care as `"No need"`, `"Met need"`, `"Foregone care"`, or `NA`.
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   health_need = c("no", "yes", "yes", "no", "dont know"),
#'   received_care = c("dont know", "yes", "no", "dont know", "yes")
#' )
#'
#' # Add foregone care category
#' df_result <- add_foregone_care(
#'   .dataset = df,
#'   ind_illness_col = "health_need",
#'   ind_received_care_col = "received_care",
#'   illness_yes_val = "yes",
#'   illness_no_val = "no",
#'   illness_dontknow_val = "dont know",
#'   care_yes_val = "yes",
#'   care_no_val = "no",
#'   care_dontknow_val = "dont know"
#' )
#' print(df_result)
#'
#' @export
add_foregone_care <- function(
    .dataset,
    ind_illness_col,
    ind_received_care_col,
    illness_yes_val,
    illness_no_val,
    illness_dontknow_val,
    care_yes_val,
    care_no_val,
    care_dontknow_val
) {
  origin <- "add_foregone_care"

  phr_try({

    # Use ensure_value for all *_val parameters
    illness_yes_val <- ensure_value(illness_yes_val, "yes")
    illness_no_val <- ensure_value(illness_no_val, "no")
    illness_dontknow_val <- ensure_value(illness_dontknow_val, "dont_know")
    care_yes_val <- ensure_value(care_yes_val, "yes")
    care_no_val <- ensure_value(care_no_val, "no")
    care_dontknow_val <- ensure_value(care_dontknow_val, "dont_know")

    #------------------------------------------------------------
    # Validate dataset
    #------------------------------------------------------------
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

    #------------------------------------------------------------
    # Validate input columns
    #------------------------------------------------------------
    required_columns <- c(ind_illness_col, ind_received_care_col)
    phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = phr_txt("Ensure the specified columns for illness and received care exist in the dataset."),
      soft = FALSE
    )

    phr_validate_choice(
      .dataset[[ind_illness_col]],
      choices = c(illness_yes_val, illness_no_val, illness_dontknow_val, NA_character_),
      origin = origin,
      soft = TRUE
    )

    phr_validate_choice(
      .dataset[[ind_received_care_col]],
      choices = c(care_yes_val, care_no_val, care_dontknow_val, NA_character_),
      origin = origin,
      soft = TRUE
    )

    #------------------------------------------------------------
    # Overwrite warning for existing output column
    #------------------------------------------------------------
    output_column <- "health_foregone_care_cat"

    if (output_column %in% names(.dataset)) {
      phr_warning(
        origin = origin,
        message = phr_txt(glue::glue("The column `{output_column}` already exists and will be overwritten."))
      )
    }

    #------------------------------------------------------------
    # Calculate health_foregone_care_cat
    #------------------------------------------------------------
    .dataset <- .dataset |>
      dplyr::mutate(
        health_foregone_care_cat = dplyr::case_when(
          # No health need and no illness
          .data[[ind_illness_col]] == illness_no_val ~ phr_txt("No need"),

          # Had health need and received care
          .data[[ind_illness_col]] == illness_yes_val & .data[[ind_received_care_col]] == care_yes_val ~ phr_txt("Met need"),

          # Had health need and did not receive care
          .data[[ind_illness_col]] == illness_yes_val & .data[[ind_received_care_col]] == care_no_val ~ phr_txt("Foregone care"),

          # No need if illness is no but received care is don't know
          .data[[ind_illness_col]] == illness_no_val & .data[[ind_received_care_col]] == care_dontknow_val ~ phr_txt("No need"),

          # Cannot classify if received care is don't know
          .data[[ind_received_care_col]] == care_dontknow_val ~ NA_character_,

          # Cannot classify if health need (illness) is don't know
          .data[[ind_illness_col]] == illness_dontknow_val ~ NA_character_,

          # Default
          TRUE ~ NA_character_
        ),
        # Convert to a factor with specific levels and order
        health_foregone_care_cat = factor(
          health_foregone_care_cat,
          levels = c(
            phr_txt("No need"),
            phr_txt("Met need"),
            phr_txt("Foregone care")
          ),
          ordered = TRUE
        )
      )

    phr_message(
      origin = origin,
      message = phr_txt("Foregone care category successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure input columns exist, contain valid data, and use specified values for the respective arguments."))
}
