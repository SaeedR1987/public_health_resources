#' @title Add Standardized Deaths and Related Columns
#'
#' @description This function processes a dataset where each row represents a deceased individual. It performs categorizations to standardize the dataset by adding columns for deaths based on age, sex, cause of death, and location-based classifications.
#'
#' The function is flexible and works even if key columns like `date_of_death` or `recall_date` are missing, by defaulting the `death` column to 1. Additionally, age-based, sex-based, and other classifications are added only if their respective columns are explicitly provided.
#'
#' @details
#' Depending on available columns and arguments, the function adds the following columns:
#' - **death**: `1` if death is identified (based on `date_of_death` and `recall_date` when available; defaults to `1` if those columns are absent).
#' - **death_under5**: `1` for deceased individuals younger than 5 years, otherwise `0` (requires `age_years_col`).
#' - **death_male**: `1` for individuals with `male_val` in the `sex_col` column (requires `sex_col`, `male_val`, and `female_val`).
#' - **death_female**: `1` for individuals with `female_val` in the `sex_col` column (requires `sex_col`, `male_val`, and `female_val`).
#' - **death_non_trauma**: `1` if the cause of death is in `non_trauma_vals` (requires `cause_of_death_col` and `non_trauma_vals`).
#' - **death_trauma**: `1` if the cause of death is in `trauma_vals` (requires `cause_of_death_col` and `trauma_vals`).
#' - **death_other**: `1` if the cause of death is in `other_vals` (requires `cause_of_death_col` and `other_vals`).
#' - **death_current_location**: `1` if the location of death matches `current_location_residence_vals` (requires `location_of_death_col` and `current_location_residence_vals`).
#' - **death_migration**: `1` if the location of death matches `migration_vals` (requires `location_of_death_col` and `migration_vals`).
#' - **death_last_location**: `1` if the location of death matches `last_location_residence_vals` (requires `location_of_death_col` and `last_location_residence_vals`).
#' - **death_birth**: `1` if the individual was born after the `recall_date_col` value, `0` otherwise (requires `date_of_birth_col` and `recall_date_col`).
#'
#' This function ensures minimal output by providing a `death` column as `1` even if `date_of_death` or `recall_date` is unavailable. It will process age-based, sex-based, or cause-of-death classifications only if their respective inputs are provided.
#'
#' @param .dataset A data frame or tibble containing rows for deceased individuals.
#' @param age_years_col Column name (character, optional) indicating the age of individuals in years. If NULL, `death_under5` will not be created.
#' @param sex_col Column name (character, optional) for the sex of deceased individuals. If NULL, `death_male` and `death_female` will not be created.
#' @param male_val Character value representing males in the `sex_col` column (required if `sex_col` is provided).
#' @param female_val Character value representing females in the `sex_col` column (required if `sex_col` is provided).
#' @param date_of_death_col Column name (character, optional) indicating the date of death. If NULL, `death` is defaulted to `1`.
#' @param recall_date_col Column name (character, optional) indicating the recall date. If NULL, `death` is defaulted to `1`.
#' @param date_of_birth_col Column name (character, optional) indicating the date of birth. If provided with `recall_date_col`, `death_birth` will be created.
#' @param cause_of_death_col (Optional) Column name (character) for the cause of death.
#' @param non_trauma_vals (Optional) Character vector of acceptable values for non-trauma causes of death.
#' @param trauma_vals (Optional) Character vector of acceptable values for trauma-related causes of death.
#' @param other_vals (Optional) Character vector of acceptable values for other causes of death.
#' @param location_of_death_col (Optional) Column name (character) for reported location of death.
#' @param current_location_residence_vals (Optional) Character vector of acceptable values for death at current location of residence.
#' @param migration_vals (Optional) Character vector of acceptable values for death during migration.
#' @param last_location_residence_vals (Optional) Character vector of acceptable values for death at the last location of residence.
#'
#' @return A data frame or tibble with the added columns as described above. At a minimum, the `death` column is always added, defaulting to `1` if no `date_of_death_col` and `recall_date_col` are provided.
#'
#' @examples
#' df <- data.frame(
#'   age_years = c(2, 30, 50),
#'   sex = c("M", "F", "M"),
#'   date_of_death = as.Date(c("2023-01-01", "2023-02-01", NA)),
#'   recall_date = as.Date(c("2022-01-01", "2023-01-01", "2022-12-31")),
#'   cause_of_death = c("malaria", "trauma", "old age"),
#'   location_of_death = c("home", "road", "last residence")
#' )
#'
#' result <- add_standardized_deaths(
#'   .dataset = df,
#'   age_years_col = "age_years",
#'   sex_col = "sex",
#'   male_val = "M",
#'   female_val = "F",
#'   date_of_death_col = "date_of_death",
#'   recall_date_col = "recall_date",
#'   cause_of_death_col = "cause_of_death",
#'   non_trauma_vals = c("malaria", "diarrhea"),
#'   trauma_vals = c("trauma", "accident"),
#'   other_vals = c("old age", "unknown"),
#'   location_of_death_col = "location_of_death",
#'   current_location_residence_vals = c("home"),
#'   migration_vals = c("road"),
#'   last_location_residence_vals = c("last residence")
#' )
#' print(result)
#'
#' @export
add_standardized_deaths <- function(
    .dataset,
    age_years_col = NULL,
    sex_col = NULL,
    male_val = NULL,
    female_val = NULL,
    date_of_death_col = NULL,
    recall_date_col = NULL,
    date_of_birth_col = NULL,
    cause_of_death_col = NULL,
    non_trauma_vals = NULL,
    trauma_vals = NULL,
    other_vals = NULL,
    location_of_death_col = NULL,
    current_location_residence_vals = NULL,
    migration_vals = NULL,
    last_location_residence_vals = NULL
) {
  origin <- "add_standardized_deaths"

  phr_try({

    # Use ensure_value for all *_val and *_vals parameters
    male_val <- ensure_value(male_val, "Male")
    female_val <- ensure_value(female_val, "Female")
    non_trauma_vals <- ensure_value(non_trauma_vals, "non_trauma")
    trauma_vals <- ensure_value(trauma_vals, "trauma")
    other_vals <- ensure_value(other_vals, "other")
    current_location_residence_vals <- ensure_value(current_location_residence_vals, "current")
    migration_vals <- ensure_value(migration_vals, "migration")
    last_location_residence_vals <- ensure_value(last_location_residence_vals, "last")

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

    # Validate mandatory input columns
    required_columns <- NULL
    if (!is.null(age_years_col)) { required_columns <- c(required_columns, age_years_col) }
    if (!is.null(sex_col)) { required_columns <- c(required_columns, sex_col) }
    if (!is.null(date_of_death_col) && !is.null(recall_date_col)) {
      required_columns <- c(required_columns, date_of_death_col, recall_date_col)
    }
    if (!is.null(date_of_birth_col)) { required_columns <- c(required_columns, date_of_birth_col) }

    if (length(required_columns) > 0) {
      phr_validate_columns(
        .dataset,
        required_columns,
        origin = origin,
        hint = phr_txt("Ensure that the necessary columns exist in the dataset."),
        soft = FALSE
      )
    }

    # Convert date columns (if applicable)
    date_cols_to_convert <- c()
    if (!is.null(date_of_death_col)) date_cols_to_convert <- c(date_cols_to_convert, date_of_death_col)
    if (!is.null(recall_date_col)) date_cols_to_convert <- c(date_cols_to_convert, recall_date_col)
    if (!is.null(date_of_birth_col)) date_cols_to_convert <- c(date_cols_to_convert, date_of_birth_col)
    
    if (length(date_cols_to_convert) > 0) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          across(
            all_of(date_cols_to_convert),
            ~ as.Date(.x)
          )
        )
    }

    # Predefine columns to create
    columns_to_create <- c(
      "death", "death_under5", "death_male", "death_female",
      "death_non_trauma", "death_trauma", "death_other",
      "death_current_location", "death_migration", "death_last_location",
      "death_birth"
    )

    for (col in columns_to_create) {
      if (col %in% names(.dataset)) {
        phr_warning(
          origin = origin,
          message = phr_txt(glue::glue("The column `{col}` already exists and will be overwritten."))
        )
      }
    }

    # Create the `death` column based on date_of_death and recall_date availability
    if (!is.null(date_of_death_col) && !is.null(recall_date_col)) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          death = dplyr::case_when(
            !is.na(.data[[date_of_death_col]]) & !is.na(.data[[recall_date_col]]) & .data[[date_of_death_col]] > .data[[recall_date_col]] ~ 1,
            is.na(.data[[date_of_death_col]]) | is.na(.data[[recall_date_col]]) ~ 1,  # Default to valid death
            TRUE ~ 0
          )
        )
    } else {
      .dataset <- .dataset %>%
        dplyr::mutate(
          death = 1
        )
    }

    # Age-based death classification (optional)
    if (!is.null(age_years_col)) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          death_under5 = dplyr::case_when(
            .data[[age_years_col]] < 5 ~ 1,
            .data[[age_years_col]] >= 5 ~ 0,
            TRUE ~ NA_real_
          )
        )
    }

    # Sex-based death classification (optional)
    if (!is.null(sex_col) && !is.null(male_val) && !is.null(female_val)) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          death_male = dplyr::case_when(
            .data[[sex_col]] == male_val ~ 1,
            .data[[sex_col]] == female_val ~ 0,
            TRUE ~ NA_real_
          ),
          death_female = dplyr::case_when(
            .data[[sex_col]] == female_val ~ 1,
            .data[[sex_col]] == male_val ~ 0,
            TRUE ~ NA_real_
          )
        )
    }

    # Cause of death columns (optional)
    if (!is.null(cause_of_death_col) && !is.null(non_trauma_vals)) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          death_non_trauma = dplyr::case_when(
            .data[[cause_of_death_col]] %in% non_trauma_vals ~ 1,
            TRUE ~ 0
          )
        )
    }

    if (!is.null(cause_of_death_col) && !is.null(trauma_vals)) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          death_trauma = dplyr::case_when(
            .data[[cause_of_death_col]] %in% trauma_vals ~ 1,
            TRUE ~ 0
          )
        )
    }

    if (!is.null(cause_of_death_col) && !is.null(other_vals)) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          death_other = dplyr::case_when(
            .data[[cause_of_death_col]] %in% other_vals ~ 1,
            TRUE ~ 0
          )
        )
    }

    # Location of death columns (optional)
    if (!is.null(location_of_death_col)) {
      if (!is.null(current_location_residence_vals)) {
        .dataset <- .dataset %>%
          dplyr::mutate(
            death_current_location = dplyr::case_when(
              .data[[location_of_death_col]] %in% current_location_residence_vals ~ 1,
              TRUE ~ 0
            )
          )
      }

      if (!is.null(migration_vals)) {
        .dataset <- .dataset %>%
          dplyr::mutate(
            death_migration = dplyr::case_when(
              .data[[location_of_death_col]] %in% migration_vals ~ 1,
              TRUE ~ 0
            )
          )
      }

      if (!is.null(last_location_residence_vals)) {
        .dataset <- .dataset %>%
          dplyr::mutate(
            death_last_location = dplyr::case_when(
              .data[[location_of_death_col]] %in% last_location_residence_vals ~ 1,
              TRUE ~ 0
            )
          )
      }
    }

    # Birth classification (optional)
    # death_birth: 1 if individual was born after recall_date, 0 otherwise
    if (!is.null(date_of_birth_col) && !is.null(recall_date_col)) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          death_birth = dplyr::case_when(
            !is.na(.data[[date_of_birth_col]]) & !is.na(.data[[recall_date_col]]) & .data[[date_of_birth_col]] > .data[[recall_date_col]] ~ 1,
            !is.na(.data[[date_of_birth_col]]) & !is.na(.data[[recall_date_col]]) & .data[[date_of_birth_col]] <= .data[[recall_date_col]] ~ 0,
            TRUE ~ NA_real_
          )
        )
    }

    phr_message(
      origin = origin,
      message = phr_txt("Standardized death calculations successfully added to the dataset.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure input column names, values, and data validity for the dataset."))
}

#' @title Add Person-Time Calculations to Dataset
#'
#' @description This function calculates person-time in days for each row based on specified entry and exit dates.
#' It also adds additional columns flagging age- and sex-based person-time where possible. The function gracefully handles
#' cases where optional columns are not provided or contain invalid values.
#'
#' @details
#' The function determines entry and exit dates as follows:
#' - **Entry Date**: Defaults to `recall_date_col`. If provided, uses `dob_col` or `date_joined_col` if they occur after
#' the recall date, prioritizing the more recent one.
#' - **Exit Date**: Defaults to `survey_date_col`. If provided, uses `date_of_death_col` (if death occurs before the survey date)
#' or `date_left_col` (if the person left before the survey date).
#'
#' If optional columns (`dob_col`, `date_of_death_col`, `date_joined_col`, `date_left_col`) are not provided or contain invalid
#' values, the function falls back to calculating person-time based solely on `recall_date_col` and `survey_date_col`.
#'
#' ## Outputs
#' - **person_time**: Person-time in days between calculated entry and exit dates. Negative values are set to `0`.
#' - **entry_date**: The calculated entry date based on available data.
#' - **exit_date**: The calculated exit date based on available data.
#' - **flag_negative_persontime**: `1` for records with negative person-time, `0` otherwise.
#' - **person_time_under5**: Person-time for individuals under 5 years old (if `age_years_col` is provided).
#' - **person_time_male**: Person-time for males (if `sex_col` is provided).
#' - **person_time_female**: Person-time for females (if `sex_col` is provided).
#'
#' @param .dataset A data frame or tibble containing records for individuals.
#' @param recall_date_col Column name (character) for the recall date. **Required.**
#' @param survey_date_col Column name (character) for the survey date. **Required.**
#' @param dob_col (Optional) Column name (character) for the date of birth. Defaults to `NULL`.
#' @param date_of_death_col (Optional) Column name (character) for the date of death. Defaults to `NULL`.
#' @param sex_col (Optional) Column name (character) for the sex of the individual. Defaults to `NULL`.
#' @param age_years_col (Optional) Column name (character) for the age of the individual (in years). Defaults to `NULL`.
#' @param date_left_col (Optional) Column name (character) for the date the individual left. Defaults to `NULL`.
#' @param date_joined_col (Optional) Column name (character) for the date the individual joined. Defaults to `NULL`.
#' @param male_val Character value representing males in the `sex_col`. Defaults to `"Male"`.
#' @param female_val Character value representing females in the `sex_col`. Defaults to `"Female"`.
#'
#' @return A data frame or tibble with calculated person-time columns:
#' - `person_time`: Person-time in days between entry and exit dates.
#' - `entry_date`: Calculated entry date.
#' - `exit_date`: Calculated exit date.
#' - `flag_negative_persontime`: Flag indicating negative person-time.
#' - Optional columns: `person_time_under5`, `person_time_male`, `person_time_female`.
#'
#' @examples
#' df <- data.frame(
#'   recall_date = as.Date(c("2023-01-01", "2023-01-01")),
#'   survey_date = as.Date(c("2023-12-31", "2023-11-30")),
#'   dob = as.Date(c("2022-06-01", "2021-01-01")),
#'   date_of_death = as.Date(NA),
#'   sex = c("Male", "Female"),
#'   age_years = c(1, 2)
#' )
#'
#' result <- add_persontime(
#'   df,
#'   recall_date_col = "recall_date",
#'   survey_date_col = "survey_date",
#'   dob_col = "dob",
#'   sex_col = "sex",
#'   age_years_col = "age_years",
#'   male_val = "Male",
#'   female_val = "Female"
#' )
#'
#' print(result)
#'
#' @export
add_persontime <- function(
    .dataset,
    recall_date_col,
    survey_date_col,
    dob_col = NULL,
    date_of_death_col = NULL,
    sex_col = NULL,
    age_years_col = NULL,
    date_left_col = NULL,
    date_joined_col = NULL,
    male_val = "Male",
    female_val = "Female"
) {
  origin <- "add_persontime"

  phr_try({

    # Use ensure_value for all *_val parameters
    male_val <- ensure_value(male_val, "Male")
    female_val <- ensure_value(female_val, "Female")

    # Step 1: Validate Dataset
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

    # Step 2: Validate and Convert Input Columns
    required_columns <- c(recall_date_col, survey_date_col)
    phr_validate_columns(
      .dataset,
      required_columns,
      origin = origin,
      hint = phr_txt("Ensure mandatory columns for recall date and survey date exist."),
      soft = FALSE
    )

    # Handle optional columns
    optional_columns <- c(dob_col, date_of_death_col, date_left_col, date_joined_col)
    valid_optional_columns <- optional_columns[!is.null(optional_columns)]
    available_columns <- c(required_columns, valid_optional_columns)

    # Convert valid columns to Date
    .dataset <- .dataset %>%
      dplyr::mutate(
        across(
          all_of(available_columns),
          ~ as.Date(.x, na.rm = TRUE)
        )
      )

    # Step 3: Calculate Entry Date
    .dataset <- .dataset %>%
      dplyr::mutate(
        entry_date = pmax(
          as.Date(.data[[recall_date_col]]),
          if (!is.null(dob_col) && dob_col %in% colnames(.dataset)) as.Date(.data[[dob_col]]) else as.Date(NA),
          if (!is.null(date_joined_col) && date_joined_col %in% colnames(.dataset)) as.Date(.data[[date_joined_col]]) else as.Date(NA),
          na.rm = TRUE
        )
      )

    # Step 4: Calculate Exit Date
    .dataset <- .dataset %>%
      dplyr::mutate(
        exit_date = pmin(
          as.Date(.data[[survey_date_col]]),
          if (!is.null(date_of_death_col) && date_of_death_col %in% colnames(.dataset)) as.Date(.data[[date_of_death_col]]) else as.Date(NA),
          if (!is.null(date_left_col) && date_left_col %in% colnames(.dataset)) as.Date(.data[[date_left_col]]) else as.Date(NA),
          na.rm = TRUE
        )
      )

    # Ensure entry_date and exit_date are valid
    phr_assert(
      all(!is.na(.dataset$entry_date)),  # Ensure there are valid entry dates
      origin = origin,
      phr_txt("No valid entry dates found. Ensure recall_date_col, date_joined_col, or dob_col is available and valid.")
    )

    phr_assert(
      all(!is.na(.dataset$exit_date)),  # Ensure there are valid exit dates
      origin = origin,
      phr_txt("No valid exit dates found. Ensure survey_date_col, date_of_death_col, or date_left_col is available and valid.")
    )

    # Step 5: Calculate Person Time
    .dataset <- .dataset %>%
      dplyr::mutate(
        person_time = as.numeric(exit_date - entry_date),  # Calculate person time in days
        person_time = dplyr::if_else(person_time < 0, 0, person_time),  # Ensure non-negative values
        flag_negative_persontime = as.integer(person_time < 0)  # Flag negative person time
      )

    # Ensure person time is valid
    phr_assert(
      all(!is.na(.dataset$person_time)),
      origin = origin,
      phr_txt("No valid person time could be calculated. Check date columns and their values.")
    )

    # Step 6: Add Optional Columns

    # Add person_time_under5 if age_years_col is provided and exists in dataset
    if (!is.null(age_years_col) && age_years_col %in% colnames(.dataset)) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          person_time_under5 = dplyr::case_when(
            .data[[age_years_col]] < 5 ~ person_time,
            TRUE ~ 0
          )
        )
    }

    # Add person_time_male if sex_col is provided and exists in dataset
    if (!is.null(sex_col) && sex_col %in% colnames(.dataset)) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          person_time_male = dplyr::case_when(
            .data[[sex_col]] == male_val ~ person_time,
            TRUE ~ 0
          )
        )
    }

    # Add person_time_female if sex_col is provided and exists in dataset
    if (!is.null(sex_col) && sex_col %in% colnames(.dataset)) {
      .dataset <- .dataset %>%
        dplyr::mutate(
          person_time_female = dplyr::case_when(
            .data[[sex_col]] == female_val ~ person_time,
            TRUE ~ 0
          )
        )
    }

    phr_message(
      origin = origin,
      message = phr_txt("Person-time calculations successfully added to the dataset.")
    )

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = phr_txt("Ensure required columns are present and contain valid data to calculate person-time."))
}

