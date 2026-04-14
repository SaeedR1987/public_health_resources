#' @title Add EC-FIES Score and Categorization
#'
#' @description Calculates the EC-FIES score and food insecurity category based on responses to 8 EC-FIES questions.
#'
#' @details This function adds two columns to the dataset:
#' 1. **nut_ecfies_score**: A numeric score (0-8) calculated by summing the responses to 8 EC-FIES questions. Each question contributes 1 point for `yes_val` and 0 points for all other values.
#' 2. **nut_ecfies_cat**: A categorical food insecurity category based on the score:
#'    - `"No Food Insecurity"`: Score of 0.
#'    - `"Mild Food Insecurity"`: Score between 1 and 3.
#'    - `"Moderate Food Insecurity"`: Score between 4 and 6.
#'    - `"Severe Food Insecurity"`: Score between 7 and 8.
#'
#' @param .dataset A data frame or tibble containing the EC-FIES columns.
#' @param nut_ecfies_so1_col Column name (character) of EC-FIES survey question 1.
#' @param nut_ecfies_so2_col Column name (character) of EC-FIES survey question 2.
#' @param nut_ecfies_so3_col Column name (character) of EC-FIES survey question 3.
#' @param nut_ecfies_so4_col Column name (character) of EC-FIES survey question 4.
#' @param nut_ecfies_so5_col Column name (character) of EC-FIES survey question 5.
#' @param nut_ecfies_so6_col Column name (character) of EC-FIES survey question 6.
#' @param nut_ecfies_so7_col Column name (character) of EC-FIES survey question 7.
#' @param nut_ecfies_so8_col Column name (character) of EC-FIES survey question 8.
#' @param yes_val Character string indicating a "yes" response, contributing 1 point to the score.
#' @param no_val Character string indicating a "no" response, contributing 0 points to the score.
#' @param dont_know_val Character string indicating the "don't know" response, contributing 0 points to the score.
#' @param prefer_not_to_answer_val Character string indicating "prefer not to answer," contributing 0 points to the score.
#'
#' @return A dataset with two new columns:
#' * **nut_ecfies_score**: Numeric score (0-8).
#' * **nut_ecfies_cat**: Factor variable with categories:
#'   - `"No Food Insecurity"`
#'   - `"Mild Food Insecurity"`
#'   - `"Moderate Food Insecurity"`
#'   - `"Severe Food Insecurity"`
#'
#' @examples
#' # Example dataset
#' df <- data.frame(
#'   so1 = c("yes", "no", "yes", "dont_know", "yes"),
#'   so2 = c("no", "no", "yes", "prefer_not_to_answer", "yes"),
#'   so3 = c("yes", "yes", "yes", "no", "dont_know"),
#'   so4 = c("no", "yes", "no", "yes", "yes"),
#'   so5 = c("no", "no", "yes", "yes", "yes"),
#'   so6 = c("yes", "no", "dont_know", "prefer_not_to_answer", "yes"),
#'   so7 = c("no", "yes", "yes", "yes", "no"),
#'   so8 = c("yes", "yes", "yes", "no", "prefer_not_to_answer")
#' )
#'
#' # Call the function
#' df_result <- add_ecfies(
#'   .dataset = df,
#'   nut_ecfies_so1_col = "so1",
#'   nut_ecfies_so2_col = "so2",
#'   nut_ecfies_so3_col = "so3",
#'   nut_ecfies_so4_col = "so4",
#'   nut_ecfies_so5_col = "so5",
#'   nut_ecfies_so6_col = "so6",
#'   nut_ecfies_so7_col = "so7",
#'   nut_ecfies_so8_col = "so8",
#'   yes_val = "yes",
#'   no_val = "no",
#'   dont_know_val = "dont_know",
#'   prefer_not_to_answer_val = "prefer_not_to_answer"
#' )
#'
#' @export
add_ecfies <- function(
    .dataset,
    nut_ecfies_so1_col,
    nut_ecfies_so2_col,
    nut_ecfies_so3_col,
    nut_ecfies_so4_col,
    nut_ecfies_so5_col,
    nut_ecfies_so6_col,
    nut_ecfies_so7_col,
    nut_ecfies_so8_col,
    yes_val ="yes",
    no_val = "no",
    dont_know_val = "dont_know",
    prefer_not_to_answer_val = "prefer_not_to_answer"
) {

  origin <- "add_ecfies"

  phr_try({

    # Use ensure_value for all *_val parameters
    yes_val <- ensure_value(yes_val, "yes")
    no_val <- ensure_value(no_val, "no")
    dont_know_val <- ensure_value(dont_know_val, "dont_know")
    prefer_not_to_answer_val <- ensure_value(prefer_not_to_answer_val, "prefer_not_to_answer")


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

    ecfies_columns <- c(
      nut_ecfies_so1_col,
      nut_ecfies_so2_col,
      nut_ecfies_so3_col,
      nut_ecfies_so4_col,
      nut_ecfies_so5_col,
      nut_ecfies_so6_col,
      nut_ecfies_so7_col,
      nut_ecfies_so8_col
    )

    phr_validate_columns(
      .dataset,
      ecfies_columns,
      origin = origin,
      hint = phr_txt("Ensure all EC-FIES columns (1-8) exist in the dataset."),
      soft = FALSE
    )


    # Validate values in each EC-FIES column

    valid_values <- c(yes_val, no_val, dont_know_val, prefer_not_to_answer_val, NA_character_)

    for (col in ecfies_columns) {
      phr_validate_choice(
        x = .dataset[[col]],
        choices = valid_values,
        origin = origin,
        soft = TRUE
      )
    }


    # Overwrite warnings for any existing output columns

    output_columns <- c("nut_ecfies_score", "nut_ecfies_cat")

    for (col in output_columns) {
      if (col %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt(glue::glue("Variable `{col}` already exists and will be overwritten."))
        )
      }
    }


    # Calculate ECFIES score and category

    .dataset <- .dataset %>%
      dplyr::mutate(
        # Calculate numeric score
        nut_ecfies_score = rowSums(dplyr::across(c(
          nut_ecfies_so1_col, nut_ecfies_so2_col, nut_ecfies_so3_col,
          nut_ecfies_so4_col, nut_ecfies_so5_col, nut_ecfies_so6_col,
          nut_ecfies_so7_col, nut_ecfies_so8_col
        ), ~ ifelse(. == yes_val, 1, 0)), na.rm = TRUE),

        # Assign food insecurity category
        nut_ecfies_cat = dplyr::case_when(
          nut_ecfies_score == 0 ~ phr_txt("No Food Insecurity"),
          nut_ecfies_score >= 1 & nut_ecfies_score <= 3 ~ phr_txt("Mild Food Insecurity"),
          nut_ecfies_score >= 4 & nut_ecfies_score <= 6 ~ phr_txt("Moderate Food Insecurity"),
          nut_ecfies_score >= 7 & nut_ecfies_score <= 8 ~ phr_txt("Severe Food Insecurity"),
          TRUE ~ NA_character_
        ),

        # Convert nut_ecfies_cat to a factor
        nut_ecfies_cat = factor(
          nut_ecfies_cat,
          levels = c(
            phr_txt("No Food Insecurity"),
            phr_txt("Mild Food Insecurity"),
            phr_txt("Moderate Food Insecurity"),
            phr_txt("Severe Food Insecurity")
          ),
          ordered = TRUE
        )
      )

    phr_message(
      origin = origin,
      message = phr_txt("EC-FIES score and categorization successfully computed.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure input columns exist, contain valid data, and scoring values are correctly specified."))
}

#' @title Add MUAC-Based SAM, MAM, GAM Classifications & Flag Extreme MUAC Values
#'
#' @description
#' This function calculates malnutrition categories (SAM, MAM, GAM) based on MUAC (Mid-Upper Arm Circumference)
#' measurements, and it adds a flag for extreme MUAC values. It also detects whether the provided MUAC values
#' are in centimeters or millimeters and performs the appropriate conversion if necessary.
#'
#' @details
#' The function adds or overwrites the following columns to the dataset:
#' * **nut_muac_cm**: MUAC values in centimeters, calculated from millimeters if necessary.
#' * **nut_muac_mm**: MUAC values in millimeters, calculated from centimeters if necessary.
#' * **sam_muac**: Severe Acute Malnutrition (1 = SAM, 0 = not SAM).
#' * **mam_muac**: Moderate Acute Malnutrition (1 = MAM, 0 = not MAM).
#' * **gam_muac**: Global Acute Malnutrition (1 = GAM, 0 = not GAM).
#' * **nut_muac_cat**: Categorical MUAC classification (`"Normal"`, `"MAM"`, `"SAM"`) wrapped in `phr_txt()`.
#'   This is derived from `sam_muac` and `mam_muac` after edema and age exclusions.
#' * **flag_muac_extreme**: Flag for extreme MUAC values (1 = less than 5 cm or greater than 20 cm, 0 = otherwise).
#' * **sam_muac_noflag**: Same as `sam_muac`, but `NA` if `flag_muac_extreme == 1`.
#' * **mam_muac_noflag**: Same as `mam_muac`, but `NA` if `flag_muac_extreme == 1`.
#' * **gam_muac_noflag**: Same as `gam_muac`, but `NA` if `flag_muac_extreme == 1`.
#'
#' ## Unit Detection:
#' * **Centimeter Detection**: If MUAC values in `nut_muac_cm_col` are all below 30, they are assumed to be in centimeters.
#'   The function will create or overwrite a `nut_muac_mm` column by converting the values to millimeters (multiplying by 10).
#' * **Millimeter Detection**: If MUAC values in `nut_muac_cm_col` are all above 50, they are assumed to be in millimeters.
#'   The function will create or overwrite a `nut_muac_cm` column by converting the values to centimeters (dividing by 10).
#' * If the values do not satisfy the logic for centimeters or millimeters, no additional column is created, and a warning is issued.
#'
#' Children aged 6-59 months are included in the classification. SAM and GAM classifications take edema confirmation into account.
#'
#' @param .dataset A data frame or tibble containing the required columns.
#' @param nut_muac_cm_col Column name (character) of the MUAC measurements (in cm) or (mm).
#'   Unit detection will determine if values are centimeters or millimeters and perform the necessary conversion.
#' @param edema_confirm_col Column name (character) confirming the presence of bilateral pitting edema.
#' @param child_age_months_col Column name (character) of the child's age in months.
#' @param edema_confirm_val Character value representing confirmed edema in the edema confirmation column.
#'
#' @return A data frame or tibble with the added MUAC unit columns (where applicable), classifications, categories,
#' extreme-value flag, and no-flag variants.
#'
#' @examples
#' # Example dataset with MUAC values in centimeters
#' df_cm <- data.frame(
#'   nut_muac_cm = c(4.8, 12.5, 21.0),
#'   child_age_months = c(14, 54, 30),
#'   nut_edema_confirm = c("yes", NA, "no")
#' )
#'
#' # Add MUAC classifications for centimeters
#' df_result_cm <- add_muac(
#'   .dataset = df_cm,
#'   nut_muac_cm_col = "nut_muac_cm",
#'   edema_confirm_col = "nut_edema_confirm",
#'   child_age_months_col = "child_age_months",
#'   edema_confirm_val = "yes"
#' )
#'
#' # Example dataset with MUAC values in millimeters
#' df_mm <- data.frame(
#'   nut_muac_cm = c(48, 125, 210), # Actually in millimeters
#'   child_age_months = c(14, 54, 30),
#'   nut_edema_confirm = c("yes", NA, "no")
#' )
#'
#' # Add MUAC classifications for millimeters
#' df_result_mm <- add_muac(
#'   .dataset = df_mm,
#'   nut_muac_cm_col = "nut_muac_cm",
#'   edema_confirm_col = "nut_edema_confirm",
#'   child_age_months_col = "child_age_months",
#'   edema_confirm_val = "yes"
#' )
#'
#' @export
add_muac <- function(
    .dataset,
    nut_muac_cm_col,
    edema_confirm_col,
    child_age_months_col,
    edema_confirm_val
) {
  origin <- "add_muac"

  phr_try({

    # Use ensure_value for *_val parameter
    edema_confirm_val <- ensure_value(edema_confirm_val, "yes")


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

    required_columns <- c(nut_muac_cm_col, edema_confirm_col, child_age_months_col)
    phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = phr_txt("Ensure the required columns for MUAC calculation exist in the dataset."),
      soft = FALSE
    )

    phr_validate_all_numeric(
      .dataset[[nut_muac_cm_col]],
      origin = origin,
      hint = phr_txt("The `nut_muac_cm_col` column must contain numeric values."),
      soft = TRUE
    )


    # Detect MUAC Units and Add/Overwrite Columns

    muac_is_cm <- all(.dataset[[nut_muac_cm_col]] < 30, na.rm = TRUE)
    muac_is_mm <- all(.dataset[[nut_muac_cm_col]] > 50, na.rm = TRUE)

    if (muac_is_cm) {
      # Add or overwrite millimeter column (convert centimeters to millimeters)
      if ("nut_muac_mm" %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt("The column `nut_muac_mm` already exists and will be overwritten.")
        )
      }
      .dataset <- .dataset %>%
        dplyr::mutate(
          nut_muac_mm = .data[[nut_muac_cm_col]] * 10
        )

      phr_message(
        origin = origin,
        message = phr_txt("MUAC values detected as centimeters. Converted and added `nut_muac_mm` column.")
      )
    } else if (muac_is_mm) {
      # Add or overwrite centimeter column (convert millimeters to centimeters)
      if ("nut_muac_cm" %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt("The column `nut_muac_cm` already exists and will be overwritten.")
        )
      }
      .dataset <- .dataset %>%
        dplyr::mutate(
          `nut_muac_cm` = .data[[nut_muac_cm_col]] / 10
        )

      phr_message(
        origin = origin,
        message = phr_txt("MUAC values detected as millimeters. Converted and added (or overwritten) `nut_muac_cm` column.")
      )
    } else {
      phr_warning(
        origin = origin,
        message = phr_txt("MUAC values could not be clearly identified as centimeters or millimeters. No additional unit columns created.")
      )
    }


    # Overwrite warnings for any existing output columns

    output_columns <- c(
      "sam_muac", "mam_muac", "gam_muac",
      "sam_muac_noflag", "mam_muac_noflag", "gam_muac_noflag",
      "nut_muac_cat", "nut_muac_cat_noflag",
      "nut_muac_cm_noflag", "nut_muac_mm_noflag",
      "flag_muac_extreme"
    )

    for (col in output_columns) {
      if (col %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt(glue::glue("Variable `{col}` already exists and will be overwritten."))
        )
      }
    }


    # Calculate SAM, MAM, GAM categories and flag extreme MUAC values

    .dataset <- .dataset %>%
      dplyr::mutate(
        # Calculate age in months
        age_months = as.numeric(.data[[child_age_months_col]]),

        # Compute SAM, MAM, and GAM based on MUAC thresholds
        sam_muac = dplyr::case_when(
          is.na(.data[[nut_muac_cm_col]]) ~ NA_real_,
          as.numeric(.data[[nut_muac_cm_col]]) < 11.5 ~ 1,
          TRUE ~ 0
        ),
        mam_muac = dplyr::case_when(
          is.na(.data[[nut_muac_cm_col]]) ~ NA_real_,
          as.numeric(.data[[nut_muac_cm_col]]) >= 11.5 & as.numeric(.data[[nut_muac_cm_col]]) < 12.5 ~ 1,
          TRUE ~ 0
        ),
        gam_muac = dplyr::case_when(
          is.na(.data[[nut_muac_cm_col]]) ~ NA_real_,
          as.numeric(.data[[nut_muac_cm_col]]) < 12.5 ~ 1,
          TRUE ~ 0
        ),

        # Adjust SAM and GAM based on edema confirmation
        sam_muac = ifelse(
          !is.na(.data[[edema_confirm_col]]) & .data[[edema_confirm_col]] == edema_confirm_val, 1, sam_muac
        ),
        gam_muac = ifelse(
          !is.na(.data[[edema_confirm_col]]) & .data[[edema_confirm_col]] == edema_confirm_val, 1, gam_muac
        ),

        # Exclude ages outside the 6-59 months range
        sam_muac = ifelse(age_months < 6 | age_months >= 60, NA_real_, sam_muac),
        mam_muac = ifelse(age_months < 6 | age_months >= 60, NA_real_, mam_muac),
        gam_muac = ifelse(age_months < 6 | age_months >= 60, NA_real_, gam_muac),

        # Categorize MUAC after edema + age exclusions have been applied
        nut_muac_cat = dplyr::case_when(
          is.na(sam_muac) | is.na(mam_muac) ~ NA_character_,
          sam_muac == 1 ~ phr_txt("SAM"),
          mam_muac == 1 ~ phr_txt("MAM"),
          TRUE ~ phr_txt("Normal")
        ),
        nut_muac_cat = factor(
          nut_muac_cat,
          levels = c(phr_txt("SAM"), phr_txt("MAM"), phr_txt("Normal")),
          ordered = TRUE
        ),

        # Flag extreme MUAC values (< 5 cm or > 20 cm)
        flag_muac_extreme = dplyr::case_when(
          is.na(.data[[nut_muac_cm_col]]) ~ 0,
          as.numeric(.data[[nut_muac_cm_col]]) < 5 | as.numeric(.data[[nut_muac_cm_col]]) > 20 ~ 1,
          TRUE ~ 0
        ),

        # No-flag versions of MUAC indicators (set to NA when MUAC is extreme)
        sam_muac_noflag = dplyr::if_else(flag_muac_extreme == 1, NA_real_, sam_muac),
        mam_muac_noflag = dplyr::if_else(flag_muac_extreme == 1, NA_real_, mam_muac),
        gam_muac_noflag = dplyr::if_else(flag_muac_extreme == 1, NA_real_, gam_muac),
        nut_muac_cat_noflag = dplyr::if_else(flag_muac_extreme == 1, factor(NA_character_, levels = levels(nut_muac_cat), ordered = TRUE), nut_muac_cat),
        nut_muac_cm_noflag = dplyr::if_else(
          flag_muac_extreme == 1,
          NA_real_,
          as.numeric(.data[["nut_muac_cm"]])
        ),
        nut_muac_mm_noflag = dplyr::if_else(
          flag_muac_extreme == 1,
          NA_real_,
          as.numeric(.data[["nut_muac_mm"]])
        )

      )

    phr_message(
      origin = origin,
      message = phr_txt("MUAC-based SAM, MAM, GAM classifications and extreme value flags successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure input columns exist and contain valid numeric or categorical data."))
}

#' @title Add MFA-Z-Based Classifications and Flags
#'
#' @description
#' Calculates severe, moderate, and global MFA-Z classifications, flags extreme MFA-Z values,
#' and provides "no-flag" versions of MFA-Z outputs (set to `NA` when flagged).
#'
#' @details
#' The function adds the following columns to the dataset:
#' - **mfaz**: The calculated MFA-Z score using the `zscorer` package.
#' - **severe_mfaz**: Severe malnutrition (1 = severe, 0 = not severe).
#' - **moderate_mfaz**: Moderate malnutrition (1 = moderate, 0 = not moderate).
#' - **global_mfaz**: Global malnutrition (1 = global, 0 = not global).
#' - **flag_sd_mfaz**: 1 if the MFA-Z value is less than `mean - 4 * SD` or greater than `mean + 3 * SD`, 0 otherwise.
#' - **mfaz_noflag**: Same as `mfaz`, but `NA` if `flag_sd_mfaz == 1`.
#' - **severe_mfaz_noflag**: Same as `severe_mfaz`, but `NA` if `flag_sd_mfaz == 1`.
#' - **moderate_mfaz_noflag**: Same as `moderate_mfaz`, but `NA` if `flag_sd_mfaz == 1`.
#' - **global_mfaz_noflag**: Same as `global_mfaz`, but `NA` if `flag_sd_mfaz == 1`.
#' - **nut_mfaz_cat**: Ordered factor with levels `"Normal"`, `"MAM"`, `"SAM"`, based on MFA-Z classification.
#' - **nut_mfaz_cat_noflag**: Same as `nut_mfaz_cat`, but `NA` if `flag_sd_mfaz == 1`.
#'
#' The function avoids overwriting any existing `sex` column in the dataset by creating a temporary
#' internal column (`temp_sex_for_zscorer`) for sex recoding (1 for male, 2 for female). This column
#' is used for calculations with `zscorer` and is removed from the dataset after processing.
#'
#' If `grouping` is provided, the mean and standard deviation of MFA-Z values are calculated within
#' those groups for determining `flag_sd_mfaz`.
#'
#' @param .dataset A data frame or tibble containing the required columns.
#' @param nut_muac_cm_col Column name (character) of the MUAC measurements (in cm).
#' @param edema_confirm_col Column name (character) confirming the presence of bilateral pitting edema.
#' @param child_age_months_col Column name (character) of the child's age in months.
#' @param child_sex_col Column name (character) of the child's sex.
#' @param male_sex_val Character value representing "male" in the `child_sex_col` column.
#' @param female_sex_val Character value representing "female" in the `child_sex_col` column.
#' @param edema_confirm_val Character value representing confirmed edema in the `edema_confirm_col` column.
#' @param grouping Optional character vector of column names for grouping mean and SD calculations for `flag_sd_mfaz`.
#'
#' @return A data frame with the added MFA-Z score, classifications, flags, no-flag variants, and categorical variables.
#'
#' @examples
#' df <- data.frame(
#'   nut_muac_cm = c(12.5, 10.5, 14.0),
#'   child_sex = c("m", "f", "f"),
#'   child_age_months = c(24, 30, 18),
#'   nut_edema_confirm = c("yes", NA, "no")
#' )
#'
#' df_result <- add_mfaz(
#'   .dataset = df,
#'   nut_muac_cm_col = "nut_muac_cm",
#'   edema_confirm_col = "nut_edema_confirm",
#'   child_age_months_col = "child_age_months",
#'   child_sex_col = "child_sex",
#'   male_sex_val = "m",
#'   female_sex_val = "f",
#'   edema_confirm_val = "yes"
#' )
#'
#' @export
add_mfaz <- function(
    .dataset,
    nut_muac_cm_col,
    edema_confirm_col,
    child_age_months_col,
    child_sex_col,
    male_sex_val,
    female_sex_val,
    edema_confirm_val,
    grouping = NULL
) {
  origin <- "add_mfaz"

  phr_try({

    # Use ensure_value for *_val parameters
    male_sex_val <- ensure_value(male_sex_val, "Male")
    female_sex_val <- ensure_value(female_sex_val, "Female")
    edema_confirm_val <- ensure_value(edema_confirm_val, "yes")


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

    required_columns <- c(nut_muac_cm_col, edema_confirm_col, child_age_months_col, child_sex_col)
    phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = phr_txt("Ensure the required columns for MFA-Z calculation exist in the dataset."),
      soft = FALSE
    )

    phr_validate_all_numeric(
      .dataset[[nut_muac_cm_col]],
      origin = origin,
      hint = phr_txt("The `nut_muac_cm_col` column must contain numeric values for MFA-Z calculation."),
      soft = TRUE
    )


    # Overwrite warnings for any existing output columns

    output_columns <- c(
      "severe_mfaz", "moderate_mfaz", "global_mfaz", "mfaz",
      "flag_sd_mfaz", "flag_who_mfaz",
      "mfaz_noflag", "severe_mfaz_noflag", "moderate_mfaz_noflag", "global_mfaz_noflag",
      "nut_mfaz_cat", "nut_mfaz_cat_noflag"
    )

    for (col in output_columns) {
      if (col %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt(glue::glue("Variable `{col}` already exists and will be overwritten."))
        )
      }
    }


    # Intermediate sex column for zscorer (to avoid overwrites)

    temp_sex_col <- "temp_sex_for_zscorer"
    if (temp_sex_col %in% names(.dataset)) {
      phr_warning(
        origin = origin,
        message = phr_txt(glue::glue("The temporary column `{temp_sex_col}` already exists and will be overwritten for zscorer compatibility."))
      )
    }

    # Create internal `temp_sex_col` for zscorer (1 = male, 2 = female)
    .dataset <- .dataset %>%
      dplyr::mutate(
        {{ temp_sex_col }} := dplyr::case_when(
          .data[[child_sex_col]] == male_sex_val ~ 1,
          .data[[child_sex_col]] == female_sex_val ~ 2,
          TRUE ~ NA_real_
        )
      )


    # Calculate MFA-Z scores using zscorer

    .dataset <- .dataset %>%
      dplyr::mutate(
        # Ensure numeric conversion and calculate age in days
        !!rlang::sym(nut_muac_cm_col) := as.numeric(.data[[nut_muac_cm_col]]),
        age_months = as.numeric(.data[[child_age_months_col]]),
        age_days = age_months * 30.25
      ) %>%
      zscorer::addWGSR(
        sex = temp_sex_col,
        firstPart = nut_muac_cm_col,
        secondPart = "age_days",
        index = "mfa"
      )


    # Calculate Severe, Moderate, and Global MFAZ classifications

    .dataset <- .dataset %>%
      dplyr::mutate(
        severe_mfaz = dplyr::case_when(
          is.na(mfaz) ~ NA_real_,
          mfaz < -3 ~ 1,
          TRUE ~ 0
        ),
        moderate_mfaz = dplyr::case_when(
          is.na(mfaz) ~ NA_real_,
          mfaz >= -3 & mfaz < -2 ~ 1,
          TRUE ~ 0
        ),
        global_mfaz = dplyr::case_when(
          is.na(mfaz) ~ NA_real_,
          mfaz < -2 ~ 1,
          TRUE ~ 0
        ),
        # Adjust classifications based on confirmed edema
        severe_mfaz = ifelse(
          !is.na(.data[[edema_confirm_col]]) & .data[[edema_confirm_col]] == edema_confirm_val, 1, severe_mfaz
        ),
        global_mfaz = ifelse(
          !is.na(.data[[edema_confirm_col]]) & .data[[edema_confirm_col]] == edema_confirm_val, 1, global_mfaz
        ),
        # Exclude classifications for ages outside the range of 6-59 months
        severe_mfaz = ifelse(age_months < 6 | age_months >= 60, NA_real_, severe_mfaz),
        moderate_mfaz = ifelse(age_months < 6 | age_months >= 60, NA_real_, moderate_mfaz),
        global_mfaz = ifelse(age_months < 6 | age_months >= 60, NA_real_, global_mfaz)
      )


    # Calculate `flag_sd_mfaz` column

    calculate_flags <- function(df) {
      mean_mfaz <- mean(df$mfaz, na.rm = TRUE)
      sd_mfaz <- sd(df$mfaz, na.rm = TRUE)

      df <- df %>%
        dplyr::mutate(
          flag_sd_mfaz = dplyr::case_when(
            is.na(mfaz) ~ 0,
            mfaz < (mean_mfaz - 4) | mfaz > (mean_mfaz + 3) ~ 1,
            TRUE ~ 0
          )
        )
      return(df)
    }

    if (is.null(grouping)) {
      .dataset <- calculate_flags(.dataset)
    } else {
      .dataset <- .dataset %>%
        dplyr::group_by(across(all_of(grouping))) %>%
        dplyr::group_modify(~ calculate_flags(.x)) %>%
        dplyr::ungroup()
    }


    # Add no-flag versions and categorical MFAZ variables

    .dataset <- .dataset %>%
      dplyr::mutate(
        mfaz_noflag = dplyr::if_else(flag_sd_mfaz == 1, NA_real_, mfaz),

        severe_mfaz_noflag = dplyr::if_else(flag_sd_mfaz == 1, NA_real_, severe_mfaz),
        moderate_mfaz_noflag = dplyr::if_else(flag_sd_mfaz == 1, NA_real_, moderate_mfaz),
        global_mfaz_noflag = dplyr::if_else(flag_sd_mfaz == 1, NA_real_, global_mfaz),

        nut_mfaz_cat = dplyr::case_when(
          is.na(severe_mfaz) | is.na(moderate_mfaz) ~ NA_character_,
          severe_mfaz == 1 ~ phr_txt("SAM"),
          moderate_mfaz == 1 ~ phr_txt("MAM"),
          TRUE ~ phr_txt("Normal")
        ),

        nut_mfaz_cat = factor(
          nut_mfaz_cat,
          levels = c(phr_txt("SAM"), phr_txt("MAM"), phr_txt("Normal")),
          ordered = TRUE
        ),

        nut_mfaz_cat_noflag = dplyr::if_else(
          flag_sd_mfaz == 1,
          NA_character_,
          as.character(nut_mfaz_cat)
        ),

        nut_mfaz_cat_noflag = factor(
          nut_mfaz_cat_noflag,
          levels = c(phr_txt("SAM"), phr_txt("MAM"), phr_txt("Normal")),
          ordered = TRUE
        )
      )


    # Clean up temporary columns

    .dataset <- .dataset %>%
      dplyr::select(-all_of(temp_sex_col))

    phr_message(
      origin = origin,
      message = phr_txt("MFA-Z-based classifications and flags were successfully calculated.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure input columns exist and contain valid numeric or categorical data."))
}


#' @title Add Standardized Nutrition Demographics
#'
#' @description This function adds standardized demographic indicator columns to nutrition data.
#' Similar to how `add_standardized_deaths` works for death data, this creates binary/numeric
#' columns that can be easily aggregated to household level.
#'
#' @details
#' The function creates the following canonical columns for nutrition data:
#' - **nutrition_child_under2**: `1` if age < 2 years, `0` otherwise
#' - **nutrition_child_2to5**: `1` if age >= 2 and < 5 years, `0` otherwise
#' - **nutrition_child_under5**: `1` if age < 5 years, `0` otherwise
#'
#' Note: This function expects `calc_age_years` to exist (created by `add_standardized_age`).
#'
#' @param .dataset A data frame or tibble containing nutrition data
#' @param age_years_col Column name (character) for age in years. Defaults to "calc_age_years"
#'
#' @return A data frame with added nutrition demographic columns
#'
#' @examples
#' df <- data.frame(
#'   child_id = 1:5,
#'   calc_age_years = c(0.8, 1.5, 2.5, 4, 6)
#' )
#'
#' result <- add_standardized_nutrition_demographics(
#'   .dataset = df,
#'   age_years_col = "calc_age_years"
#' )
#'
#' @export
add_standardized_nutrition_demographics <- function(
    .dataset,
    age_years_col = "calc_age_years"
) {

  origin <- "add_standardized_nutrition_demographics"

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

    # Warn about overwriting existing columns
    output_cols <- c(
      "nutrition_child_under2", "nutrition_child_2to5", "nutrition_child_under5"
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
        nutrition_child_under2 = dplyr::if_else(
          !is.na(.data[[age_years_col]]) & .data[[age_years_col]] < 2,
          1, 0
        ),
        nutrition_child_2to5 = dplyr::if_else(
          !is.na(.data[[age_years_col]]) &
            .data[[age_years_col]] >= 2 &
            .data[[age_years_col]] < 5,
          1, 0
        ),
        nutrition_child_under5 = dplyr::if_else(
          !is.na(.data[[age_years_col]]) & .data[[age_years_col]] < 5,
          1, 0
        )
      )

    phr_message(
      origin = origin,
      message = phr_txt("Standardized nutrition demographic columns added successfully.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure age column is valid."))
}
