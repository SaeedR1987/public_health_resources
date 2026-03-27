#' Calculate Food Consumption Score (FCS)
#'
#' @description
#' Calculates the FCS score and category based on weighted food group frequencies.
#' Applies standard WFP methodology with configurable cutoffs.
#'
#' @param .dataset Data frame containing FCS component variables
#' @param cutoffs Character: "normal" or "alternative" FCS categorization cutoffs
#' @param fsl_fcs_cereal Character: Column name for cereals frequency (0-7 days)
#' @param fsl_fcs_legumes Character: Column name for legumes frequency (0-7 days)
#' @param fsl_fcs_veg Character: Column name for vegetables frequency (0-7 days)
#' @param fsl_fcs_fruit Character: Column name for fruit frequency (0-7 days)
#' @param fsl_fcs_meat Character: Column name for meat frequency (0-7 days)
#' @param fsl_fcs_dairy Character: Column name for dairy frequency (0-7 days)
#' @param fsl_fcs_sugar Character: Column name for sugar frequency (0-7 days)
#' @param fsl_fcs_oil Character: Column name for oil frequency (0-7 days)
#'
#' @return Data frame with added columns: fsl_fcs_score, fsl_fcs_cat
#' @export
add_fcs <- function(.dataset,
                    cutoffs = "normal",
                    fsl_fcs_cereal = "fsl_fcs_cereal",
                    fsl_fcs_legumes = "fsl_fcs_legumes",
                    fsl_fcs_veg = "fsl_fcs_veg",
                    fsl_fcs_fruit = "fsl_fcs_fruit",
                    fsl_fcs_meat = "fsl_fcs_meat",
                    fsl_fcs_dairy = "fsl_fcs_dairy",
                    fsl_fcs_sugar = "fsl_fcs_sugar",
                    fsl_fcs_oil = "fsl_fcs_oil") {

  origin <- "add_fcs"

  iphra_try(
    expr = {


      # Basic dataset validation

      iphra_validate_dataframe(.dataset, origin, soft = FALSE)

      iphra_assert(
        nrow(.dataset) > 0,
        origin,
        iphra_txt("Dataset is empty.")
      )

      fcs_vars <- c(
        fsl_fcs_cereal, fsl_fcs_legumes, fsl_fcs_dairy, fsl_fcs_meat,
        fsl_fcs_veg, fsl_fcs_fruit, fsl_fcs_oil, fsl_fcs_sugar
      )

      iphra_validate_columns(
        .dataset,
        fcs_vars,
        origin,
        hint = iphra_txt("Ensure all required FCS component variables are present."),
        soft = FALSE
      )


      # Overwrite warnings

      if ("fsl_fcs_score" %in% names(.dataset)) {
        var <- "fsl_fcs_score"
        iphra_warning(
          origin,
          iphra_txt(glue::glue("Variable {var} already exists and will be overwritten."))
        )
      }

      if ("fsl_fcs_cat" %in% names(.dataset)) {
        var <- "fsl_fcs_cat"
        iphra_warning(
          origin,
          iphra_txt(glue::glue("Variable {var} already exists and will be overwritten."))
        )
      }


      # Validation (warning only - do NOT modify original values)

      for (v in fcs_vars) {

        # must be numeric
        iphra_validate_all_numeric(.dataset[[v]], origin, soft = TRUE)

        # call validator (warning only)
        iphra_validate_range(
          df = .dataset,
          col = v,
          min = 0,
          max = 7,
          soft = TRUE,
          origin = origin,
          hint = iphra_txt("FCS frequency must be between 0 and 7.")
        )
      }


      # Compute weighted components
      # Out-of-range values are treated as NA during calculation ONLY
      # Original values are preserved for quality checks

      .dataset <- .dataset %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(fcs_vars), as.numeric)) %>%
        dplyr::mutate(
          # Create "safe" versions that treat out-of-range as NA for calculation
          # but don't modify the original columns
          fcs_weight_cereal1 = ifelse(.data[[fsl_fcs_cereal]] < 0 | .data[[fsl_fcs_cereal]] > 7, NA, .data[[fsl_fcs_cereal]]) * 2,
          fcs_weight_legume2 = ifelse(.data[[fsl_fcs_legumes]] < 0 | .data[[fsl_fcs_legumes]] > 7, NA, .data[[fsl_fcs_legumes]]) * 3,
          fcs_weight_dairy3  = ifelse(.data[[fsl_fcs_dairy]] < 0 | .data[[fsl_fcs_dairy]] > 7, NA, .data[[fsl_fcs_dairy]]) * 4,
          fcs_weight_meat4   = ifelse(.data[[fsl_fcs_meat]] < 0 | .data[[fsl_fcs_meat]] > 7, NA, .data[[fsl_fcs_meat]]) * 4,
          fcs_weight_veg5    = ifelse(.data[[fsl_fcs_veg]] < 0 | .data[[fsl_fcs_veg]] > 7, NA, .data[[fsl_fcs_veg]]) * 1,
          fcs_weight_fruit6  = ifelse(.data[[fsl_fcs_fruit]] < 0 | .data[[fsl_fcs_fruit]] > 7, NA, .data[[fsl_fcs_fruit]]) * 1,
          fcs_weight_oil7    = ifelse(.data[[fsl_fcs_oil]] < 0 | .data[[fsl_fcs_oil]] > 7, NA, .data[[fsl_fcs_oil]]) * 0.5,
          fcs_weight_sugar8  = ifelse(.data[[fsl_fcs_sugar]] < 0 | .data[[fsl_fcs_sugar]] > 7, NA, .data[[fsl_fcs_sugar]]) * 0.5
        ) %>%
        dplyr::mutate(
          fsl_fcs_score = rowSums(
            dplyr::across(c(
              fcs_weight_cereal1, fcs_weight_legume2, fcs_weight_dairy3,
              fcs_weight_meat4, fcs_weight_veg5, fcs_weight_fruit6,
              fcs_weight_oil7, fcs_weight_sugar8
            )),
            na.rm = FALSE
          ),
          # Calculate standard deviation of days across all food groups
          sd_foods = apply(
            dplyr::across(c(
              fsl_fcs_cereal, fsl_fcs_legumes, fsl_fcs_dairy, fsl_fcs_meat,
              fsl_fcs_veg, fsl_fcs_fruit, fsl_fcs_oil, fsl_fcs_sugar
            )), 1, stats::sd, na.rm = TRUE
          )
        )


      # Handle cutoffs

      if (!cutoffs %in% c("normal", "alternative")) {
        iphra_warning(
          origin,
          iphra_txt(
            "Invalid cutoff type '{cutoffs}'. Defaulting to 'normal' thresholds."
          )
        )
        cutoffs <- "normal"
      }


      # Categorization

      if (cutoffs == "normal") {

        .dataset <- .dataset %>%
          dplyr::mutate(
            fsl_fcs_cat = dplyr::case_when(
              fsl_fcs_score < 21.5 ~ iphra_txt("Poor"),
              fsl_fcs_score <= 35  ~ iphra_txt("Borderline"),
              fsl_fcs_score > 35   ~ iphra_txt("Acceptable"),
              TRUE ~ NA_character_
            )
          )

      } else {

        .dataset <- .dataset %>%
          dplyr::mutate(
            fsl_fcs_cat = dplyr::case_when(
              fsl_fcs_score <= 28 ~ iphra_txt("Poor"),
              fsl_fcs_score <= 42 ~ iphra_txt("Borderline"),
              fsl_fcs_score > 42  ~ iphra_txt("Acceptable"),
              TRUE ~ NA_character_
            )
          )
      }

      # factorization
      .dataset <- .dataset %>%
        dplyr::mutate(
          fsl_fcs_cat = factor(
            fsl_fcs_cat,
            levels = c(
              iphra_txt("Poor"),
              iphra_txt("Borderline"),
              iphra_txt("Acceptable")
            )
          )
        )


      # Completion

      iphra_message(origin, iphra_txt("FCS score and category computed successfully."))

      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = iphra_txt("Ensure FCS component variables contain numeric values 0–7.")
  )
}

#' Calculate Household Hunger Scale (HHS)
#'
#' @description
#' Calculates HHS score and categories based on three core hunger questions
#' and their frequency responses.
#'
#' @param .dataset Data frame containing HHS component variables
#' @param fsl_hhs_nofoodhh Character: Column for "no food in household" question
#' @param fsl_hhs_nofoodhh_freq Character: Column for frequency of no food
#' @param fsl_hhs_sleephungry Character: Column for "went to sleep hungry" question
#' @param fsl_hhs_sleephungry_freq Character: Column for frequency of sleep hungry
#' @param fsl_hhs_alldaynight Character: Column for "whole day without eating" question
#' @param fsl_hhs_alldaynight_freq Character: Column for frequency of all day/night
#' @param yes_answer Character: Value representing "yes" response
#' @param no_answer Character: Value representing "no" response
#' @param rarely_answer Character: Value representing "rarely" frequency
#' @param sometimes_answer Character: Value representing "sometimes" frequency
#' @param often_answer Character: Value representing "often" frequency
#'
#' @return Data frame with added columns: fsl_hhs_score, fsl_hhs_cat, fsl_hhs_cat_ipc
#' @export
add_hhs <- function(
    .dataset,
    fsl_hhs_nofoodhh = "fsl_hhs_nofoodhh",
    fsl_hhs_nofoodhh_freq = "fsl_hhs_nofoodhh_freq",
    fsl_hhs_sleephungry = "fsl_hhs_sleephungry",
    fsl_hhs_sleephungry_freq = "fsl_hhs_sleephungry_freq",
    fsl_hhs_alldaynight = "fsl_hhs_alldaynight",
    fsl_hhs_alldaynight_freq = "fsl_hhs_alldaynight_freq",
    yes_answer = "yes",
    no_answer = "no",
    rarely_answer = "rarely",
    sometimes_answer = "sometimes",
    often_answer = "often"
) {

  origin <- "add_hhs"

  iphra_try({

    # Use ensure_value for all *_val parameters
    yes_answer <- ensure_value(yes_answer, "yes")
    no_answer <- ensure_value(no_answer, "no")
    rarely_answer <- ensure_value(rarely_answer, "rarely")
    sometimes_answer <- ensure_value(sometimes_answer, "sometimes")
    often_answer <- ensure_value(often_answer, "often")


    # Validate dataset

    iphra_validate_dataframe(.dataset, origin, soft = FALSE)

    iphra_assert(
      nrow(.dataset) > 0,
      origin,
      iphra_txt("Dataset is empty.")
    )

    # Missing column check
    hhs_vars <- c(
      fsl_hhs_nofoodhh, fsl_hhs_nofoodhh_freq,
      fsl_hhs_sleephungry, fsl_hhs_sleephungry_freq,
      fsl_hhs_alldaynight, fsl_hhs_alldaynight_freq
    )

    missing_cols <- setdiff(hhs_vars, names(.dataset))
    if (length(missing_cols) > 0) {
      iphra_error(
        iphra_txt(glue::glue("Missing required HHS columns: {paste(missing_cols, collapse=', ')}")),
        origin = origin
      )
    }


    # Overwrite warnings for any existing output columns

    overwrite_vars <- c(
      "fsl_hhs_comp1", "fsl_hhs_comp2", "fsl_hhs_comp3",
      "fsl_hhs_score",
      "fsl_hhs_cat",
      "fsl_hhs_cat_ipc"
    )

    for (var in overwrite_vars) {
      if (var %in% names(.dataset)) {
        iphra_warning(
          origin = origin,
          message = iphra_txt(glue::glue("Variable {var} already exists and will be overwritten."))
        )
      }
    }


    # Validate choices

    # Yes/No items
    iphra_validate_choice(.dataset[[fsl_hhs_nofoodhh]], c(yes_answer, no_answer, NA_character_), origin, soft = FALSE)
    iphra_validate_choice(.dataset[[fsl_hhs_sleephungry]], c(yes_answer, no_answer, NA_character_), origin, soft = FALSE)
    iphra_validate_choice(.dataset[[fsl_hhs_alldaynight]], c(yes_answer, no_answer, NA_character_), origin, soft = FALSE)

    # Frequency items
    iphra_validate_choice(.dataset[[fsl_hhs_nofoodhh_freq]], c(rarely_answer, sometimes_answer, often_answer, NA_character_), origin, soft = FALSE)
    iphra_validate_choice(.dataset[[fsl_hhs_sleephungry_freq]], c(rarely_answer, sometimes_answer, often_answer, NA_character_), origin, soft = FALSE)
    iphra_validate_choice(.dataset[[fsl_hhs_alldaynight_freq]], c(rarely_answer, sometimes_answer, often_answer, NA_character_), origin, soft = FALSE)


    # Construct numeric HHS components
    # Preserve original values; treat invalid as NA only during calculation

    .dataset <- .dataset %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        # Convert yes/no to numeric 1/0 (treat unrecognized values as NA)
        hhs_nofoodhh_numeric = dplyr::case_when(
          .data[[fsl_hhs_nofoodhh]] == yes_answer ~ 1,
          .data[[fsl_hhs_nofoodhh]] == no_answer  ~ 0,
          TRUE ~ NA_real_
        ),
        hhs_sleephungry_numeric = dplyr::case_when(
          .data[[fsl_hhs_sleephungry]] == yes_answer ~ 1,
          .data[[fsl_hhs_sleephungry]] == no_answer  ~ 0,
          TRUE ~ NA_real_
        ),
        hhs_alldaynight_numeric = dplyr::case_when(
          .data[[fsl_hhs_alldaynight]] == yes_answer ~ 1,
          .data[[fsl_hhs_alldaynight]] == no_answer  ~ 0,
          TRUE ~ NA_real_
        ),

        # Convert frequency to numeric 0/1/2 (treat unrecognized values as NA)
        hhs_nofoodhh_freq_numeric = dplyr::case_when(
          .data[[fsl_hhs_nofoodhh_freq]] %in% c(rarely_answer, sometimes_answer) ~ 1,
          .data[[fsl_hhs_nofoodhh_freq]] == often_answer ~ 2,
          is.na(.data[[fsl_hhs_nofoodhh_freq]]) ~ 0,
          TRUE ~ NA_real_
        ),
        hhs_sleephungry_freq_numeric = dplyr::case_when(
          .data[[fsl_hhs_sleephungry_freq]] %in% c(rarely_answer, sometimes_answer) ~ 1,
          .data[[fsl_hhs_sleephungry_freq]] == often_answer ~ 2,
          is.na(.data[[fsl_hhs_sleephungry_freq]]) ~ 0,
          TRUE ~ NA_real_
        ),
        hhs_alldaynight_freq_numeric = dplyr::case_when(
          .data[[fsl_hhs_alldaynight_freq]] %in% c(rarely_answer, sometimes_answer) ~ 1,
          .data[[fsl_hhs_alldaynight_freq]] == often_answer ~ 2,
          is.na(.data[[fsl_hhs_alldaynight_freq]]) ~ 0,
          TRUE ~ NA_real_
        ),

        # Calculate components using numeric versions
        fsl_hhs_comp1 = hhs_nofoodhh_numeric * hhs_nofoodhh_freq_numeric,
        fsl_hhs_comp2 = hhs_sleephungry_numeric * hhs_sleephungry_freq_numeric,
        fsl_hhs_comp3 = hhs_alldaynight_numeric * hhs_alldaynight_freq_numeric
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        fsl_hhs_score = fsl_hhs_comp1 + fsl_hhs_comp2 + fsl_hhs_comp3
      ) %>%
      # Remove temporary numeric columns (list explicitly to avoid capturing user columns)
      dplyr::select(
        -hhs_nofoodhh_numeric,
        -hhs_sleephungry_numeric,
        -hhs_alldaynight_numeric,
        -hhs_nofoodhh_freq_numeric,
        -hhs_sleephungry_freq_numeric,
        -hhs_alldaynight_freq_numeric
      )


    # Categorisation (IPC and standard) with translatable labels

    .dataset <- .dataset %>%
      dplyr::mutate(
        fsl_hhs_cat_ipc = dplyr::case_when(
          fsl_hhs_score == 0 ~ iphra_txt("None"),
          fsl_hhs_score == 1 ~ iphra_txt("Little"),
          fsl_hhs_score <= 3 ~ iphra_txt("Moderate"),
          fsl_hhs_score == 4 ~ iphra_txt("Severe"),
          fsl_hhs_score <= 6 ~ iphra_txt("Very Severe"),
          TRUE ~ NA_character_
        ),
        fsl_hhs_cat = dplyr::case_when(
          fsl_hhs_score <= 1 ~ iphra_txt("Little to No"),
          fsl_hhs_score <= 3 ~ iphra_txt("Moderate"),
          fsl_hhs_score <= 6 ~ iphra_txt("Severe"),
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::mutate(
        fsl_hhs_cat = factor(fsl_hhs_cat, levels = c(
          iphra_txt("Severe"),
          iphra_txt("Moderate"),
          iphra_txt("Little to No")
        )),
        fsl_hhs_cat_ipc = factor(fsl_hhs_cat_ipc, levels = c(
          iphra_txt("Very Severe"),
          iphra_txt("Severe"),
          iphra_txt("Moderate"),
          iphra_txt("Little"),
          iphra_txt("None")
        ))
      )

    iphra_message(origin, iphra_txt("HHS score and categories computed successfully."))

    return(.dataset)

  },
  on_error = "abort",
  origin = origin,
  hint = iphra_txt("Ensure HHS component variables are appropriately structured."))
}

#' Calculate Reduced Coping Strategies Index (rCSI)
#'
#' @description
#' Calculates rCSI score and category based on five coping strategy questions
#' with weighted frequencies.
#'
#' @param .dataset Data frame containing rCSI component variables
#' @param fsl_rcsi_lessquality Character: Column for "less quality food" frequency (0-7 days)
#' @param fsl_rcsi_borrow Character: Column for "borrow food" frequency (0-7 days)
#' @param fsl_rcsi_mealsize Character: Column for "reduce meal size" frequency (0-7 days)
#' @param fsl_rcsi_mealadult Character: Column for "adults eat less" frequency (0-7 days)
#' @param fsl_rcsi_mealnb Character: Column for "reduce meal number" frequency (0-7 days)
#'
#' @return Data frame with added columns: fsl_rcsi_score, fsl_rcsi_cat
#' @export
add_rcsi <- function(.dataset,
                     fsl_rcsi_lessquality = "fsl_rcsi_lessquality",
                     fsl_rcsi_borrow = "fsl_rcsi_borrow",
                     fsl_rcsi_mealsize = "fsl_rcsi_mealsize",
                     fsl_rcsi_mealadult = "fsl_rcsi_mealadult",
                     fsl_rcsi_mealnb = "fsl_rcsi_mealnb") {

  origin <- "add_rcsi"

  iphra_try(

    expr = {

      # Basic dataset checks

      iphra_validate_dataframe(.dataset, origin, soft = FALSE)

      iphra_assert(
        nrow(.dataset) > 0,
        origin,
        iphra_txt("Dataset is empty.")
      )

      rcsi_vars <- c(
        fsl_rcsi_lessquality,
        fsl_rcsi_borrow,
        fsl_rcsi_mealsize,
        fsl_rcsi_mealadult,
        fsl_rcsi_mealnb
      )

      iphra_validate_columns(
        .dataset,
        rcsi_vars,
        origin,
        hint = iphra_txt("Ensure all required RCSI columns are present."),
        soft = FALSE
      )


      # Overwrite warnings

      if ("fsl_rcsi_score" %in% names(.dataset)) {
        var <- "fsl_rcsi_score"
        iphra_warning(
          origin,
          iphra_txt(glue::glue("Variable {var} already exists and will be overwritten."))
        )
      }

      if ("fsl_rcsi_cat" %in% names(.dataset)) {
        var <- "fsl_rcsi_cat"
        iphra_warning(
          origin,
          iphra_txt(glue::glue("Variable {var} already exists and will be overwritten."))
        )
      }


      # Enforce allowed 0–7 range using iphra_validate_range()

      for (v in rcsi_vars) {
        iphra_validate_all_numeric(.dataset[[v]], origin, soft = TRUE)

        iphra_validate_range(
          df  = .dataset,
          col = v,
          min = 0,
          max = 7,
          origin = origin,
          hint = iphra_txt("RCSI frequency values must be between 0 and 7."),
          soft = TRUE
        )
      }


      # Main computation
      # Treat out-of-range values (< 0 or > 7) as NA during calculation
      # but preserve original values

      rcs_columns <- rcsi_vars

      .dataset <- .dataset %>%
        dplyr::mutate_at(dplyr::vars(rcs_columns), as.numeric) %>%
        dplyr::mutate(
          rcsi_lessquality_weighted = ifelse(
            is.na(.data[[fsl_rcsi_lessquality]]) | .data[[fsl_rcsi_lessquality]] < 0 | .data[[fsl_rcsi_lessquality]] > 7,
            NA,
            .data[[fsl_rcsi_lessquality]] * 1
          ),
          rcsi_borrow_weighted = ifelse(
            is.na(.data[[fsl_rcsi_borrow]]) | .data[[fsl_rcsi_borrow]] < 0 | .data[[fsl_rcsi_borrow]] > 7,
            NA,
            .data[[fsl_rcsi_borrow]] * 2
          ),
          rcsi_mealsize_weighted = ifelse(
            is.na(.data[[fsl_rcsi_mealsize]]) | .data[[fsl_rcsi_mealsize]] < 0 | .data[[fsl_rcsi_mealsize]] > 7,
            NA,
            .data[[fsl_rcsi_mealsize]] * 1
          ),
          rcsi_mealadult_weighted = ifelse(
            is.na(.data[[fsl_rcsi_mealadult]]) | .data[[fsl_rcsi_mealadult]] < 0 | .data[[fsl_rcsi_mealadult]] > 7,
            NA,
            .data[[fsl_rcsi_mealadult]] * 3
          ),
          rcsi_mealnb_weighted = ifelse(
            is.na(.data[[fsl_rcsi_mealnb]]) | .data[[fsl_rcsi_mealnb]] < 0 | .data[[fsl_rcsi_mealnb]] > 7,
            NA,
            .data[[fsl_rcsi_mealnb]] * 1
          ),

          # Check if any weighted columns are NA
          any_weighted_na = is.na(rcsi_lessquality_weighted) |
            is.na(rcsi_borrow_weighted) |
            is.na(rcsi_mealsize_weighted) |
            is.na(rcsi_mealadult_weighted) |
            is.na(rcsi_mealnb_weighted),

          fsl_rcsi_score = ifelse(
            any_weighted_na,
            NA_real_,
            rowSums(
              dplyr::across(
                c(
                  rcsi_lessquality_weighted,
                  rcsi_borrow_weighted,
                  rcsi_mealsize_weighted,
                  rcsi_mealadult_weighted,
                  rcsi_mealnb_weighted
                ),
                as.numeric
              ),
              na.rm = TRUE
            )
          ),

          # Calculate standard deviation of days across all food groups
          sd_rcsicoping = ifelse(
            any_weighted_na,
            NA_real_,
            apply(
              dplyr::across(c(
                rcsi_lessquality_weighted,
                rcsi_borrow_weighted,
                rcsi_mealsize_weighted,
                rcsi_mealadult_weighted,
                rcsi_mealnb_weighted
              )), 1, stats::sd, na.rm = TRUE
            )
          ),

          fsl_rcsi_cat = dplyr::case_when(
            any_weighted_na ~ NA_character_,
            fsl_rcsi_score <= 3 ~ iphra_txt("No to Low"),
            fsl_rcsi_score <= 18 ~ iphra_txt("Medium"),
            fsl_rcsi_score > 18 ~ iphra_txt("High"),
            TRUE ~ NA_character_
          ),

          # Standardize factor levels (ALL iphra_txt)
          fsl_rcsi_cat = factor(
            fsl_rcsi_cat,
            levels = c(
              iphra_txt("High"),
              iphra_txt("Medium"),
              iphra_txt("No to Low")
            )
          )
        ) %>%
        dplyr::select(-any_weighted_na)  # Remove helper column

      iphra_message(origin, iphra_txt("RCSI successfully calculated."))

      return(.dataset)
    },
    on_error = "abort",
    origin = origin,
    hint = iphra_txt("Ensure all RCSI fields exist and contain integer values 0–7.")
  )
}

#' Calculate Livelihood Coping Strategies Index (LCSI)
#'
#' @description
#' Calculates LCSI categories (stress, crisis, emergency) based on yes/no/exhausted
#' responses to livelihood coping strategy questions.
#'
#' @param .dataset Data frame containing LCSI component variables
#' @param fsl_lcsi_stress1 Character: Column for stress strategy 1
#' @param fsl_lcsi_stress2 Character: Column for stress strategy 2
#' @param fsl_lcsi_stress3 Character: Column for stress strategy 3
#' @param fsl_lcsi_stress4 Character: Column for stress strategy 4
#' @param fsl_lcsi_crisis1 Character: Column for crisis strategy 1
#' @param fsl_lcsi_crisis2 Character: Column for crisis strategy 2
#' @param fsl_lcsi_crisis3 Character: Column for crisis strategy 3
#' @param fsl_lcsi_emergency1 Character: Column for emergency strategy 1
#' @param fsl_lcsi_emergency2 Character: Column for emergency strategy 2
#' @param fsl_lcsi_emergency3 Character: Column for emergency strategy 3
#' @param yes_val Character: Value representing "yes" response
#' @param no_val Character: Value representing "no, had no need" response
#' @param exhausted_val Character: Value representing "no, exhausted" response
#' @param not_applicable_val Character: Value representing "not applicable" response
#'
#' @return Data frame with added columns: fsl_lcsi_cat_stress, fsl_lcsi_cat_crisis, fsl_lcsi_cat_emergency
#' @export
add_lcsi <- function(.dataset,
                     fsl_lcsi_stress1    = "fsl_lcsi_stress1",
                     fsl_lcsi_stress2    = "fsl_lcsi_stress2",
                     fsl_lcsi_stress3    = "fsl_lcsi_stress3",
                     fsl_lcsi_stress4    = "fsl_lcsi_stress4",
                     fsl_lcsi_crisis1    = "fsl_lcsi_crisis1",
                     fsl_lcsi_crisis2    = "fsl_lcsi_crisis2",
                     fsl_lcsi_crisis3    = "fsl_lcsi_crisis3",
                     fsl_lcsi_emergency1 = "fsl_lcsi_emergency1",
                     fsl_lcsi_emergency2 = "fsl_lcsi_emergency2",
                     fsl_lcsi_emergency3 = "fsl_lcsi_emergency3",
                     yes_val             = "yes",
                     no_val              = "no_had_no_need",
                     exhausted_val       = "no_exhausted",
                     not_applicable_val  = "not_applicable") {

  origin <- "add_lcsi"

  iphra_try({

    # Use ensure_value for all *_val parameters
    yes_val <- ensure_value(yes_val, "yes")
    no_val <- ensure_value(no_val, "no_had_no_need")
    exhausted_val <- ensure_value(exhausted_val, "no_exhausted")
    not_applicable_val <- ensure_value(not_applicable_val, "not_applicable")

    iphra_message(origin, iphra_txt("Starting LCSI calculation..."))


    # 1. Basic validations

    iphra_validate_dataframe(
      .dataset,
      origin = origin,
      hint   = iphra_txt("The first argument must be a data.frame or tibble."),
      soft   = FALSE
    )

    iphra_assert(
      nrow(.dataset) > 0,
      origin,
      iphra_txt("Dataset is empty.")
    )

    lcsi_vars <- c(
      fsl_lcsi_stress1, fsl_lcsi_stress2, fsl_lcsi_stress3, fsl_lcsi_stress4,
      fsl_lcsi_crisis1, fsl_lcsi_crisis2, fsl_lcsi_crisis3,
      fsl_lcsi_emergency1, fsl_lcsi_emergency2, fsl_lcsi_emergency3
    )

    iphra_validate_columns(
      df           = .dataset,
      required_cols = lcsi_vars,
      origin       = origin,
      hint         = iphra_txt("Ensure all required LCSI variables are present in the dataset."),
      soft         = FALSE
    )


    # Overwrite warnings for any existing output columns

    overwrite_vars <- c(
      "fsl_lcsi_stress_yes", "fsl_lcsi_stress_exhaust", "fsl_lcsi_stress",
      "fsl_lcsi_crisis_yes", "fsl_lcsi_crisis_exhaust", "fsl_lcsi_crisis",
      "fsl_lcsi_emergency_yes", "fsl_lcsi_emergency_exhaust", "fsl_lcsi_emergency",
      "fsl_lcsi_cat_yes",
      "fsl_lcsi_cat_exhaust",
      "fsl_lcsi_cat"
    )

    for (var in overwrite_vars) {
      if (var %in% names(.dataset)) {
        iphra_warning(
          origin = origin,
          message = iphra_txt(glue::glue("Variable {var} already exists and will be overwritten."))
        )
      }
    }


    # 2. Allowed value validation (using iphra_validate_choice)
    #    NA is allowed and should pass silently

    lcsi_cat_values <- c(yes_val, no_val, exhausted_val, not_applicable_val)

    validate_lcsi_col <- function(col_name) {
      iphra_validate_choice(
        x       = as.character(.dataset[[col_name]]),
        choices = c(lcsi_cat_values, NA_character_),
        origin  = origin,
        soft    = FALSE
      )
    }

    purrr::walk(lcsi_vars, validate_lcsi_col)


    # 3. Compute completeness mask (Option A logic)
    #    If any of the 10 vars is NA -> all derived LCSI fields become NA

    complete_cases <- stats::complete.cases(.dataset[, lcsi_vars])


    # 4. Main LCSI derived fields
    # Treat invalid values (not in allowed list) as causing NA in calculations

    .dataset <- .dataset %>%
      dplyr::mutate(
        fsl_lcsi_stress_yes = dplyr::case_when(
          is.na(.data[[fsl_lcsi_stress1]]) |
            !.data[[fsl_lcsi_stress1]] %in% lcsi_cat_values ~ NA_character_,
          .data[[fsl_lcsi_stress1]] == yes_val      |
            .data[[fsl_lcsi_stress2]] == yes_val   |
            .data[[fsl_lcsi_stress3]] == yes_val   |
            .data[[fsl_lcsi_stress4]] == yes_val   ~ "1",
          TRUE ~ "0"
        ),
        fsl_lcsi_stress_exhaust = dplyr::case_when(
          is.na(.data[[fsl_lcsi_stress1]]) |
            !.data[[fsl_lcsi_stress1]] %in% lcsi_cat_values ~ NA_character_,
          .data[[fsl_lcsi_stress1]] == exhausted_val      |
            .data[[fsl_lcsi_stress2]] == exhausted_val   |
            .data[[fsl_lcsi_stress3]] == exhausted_val   |
            .data[[fsl_lcsi_stress4]] == exhausted_val   ~ "1",
          TRUE ~ "0"
        ),
        fsl_lcsi_stress = dplyr::case_when(
          is.na(fsl_lcsi_stress_yes) & is.na(fsl_lcsi_stress_exhaust) ~ NA_character_,
          fsl_lcsi_stress_yes == "1" | fsl_lcsi_stress_exhaust == "1" ~ "1",
          TRUE ~ "0"
        ),

        fsl_lcsi_crisis_yes = dplyr::case_when(
          is.na(.data[[fsl_lcsi_crisis1]]) |
            !.data[[fsl_lcsi_crisis1]] %in% lcsi_cat_values ~ NA_character_,
          .data[[fsl_lcsi_crisis1]] == yes_val      |
            .data[[fsl_lcsi_crisis2]] == yes_val   |
            .data[[fsl_lcsi_crisis3]] == yes_val   ~ "1",
          TRUE ~ "0"
        ),
        fsl_lcsi_crisis_exhaust = dplyr::case_when(
          is.na(.data[[fsl_lcsi_crisis1]]) |
            !.data[[fsl_lcsi_crisis1]] %in% lcsi_cat_values ~ NA_character_,
          .data[[fsl_lcsi_crisis1]] == exhausted_val      |
            .data[[fsl_lcsi_crisis2]] == exhausted_val   |
            .data[[fsl_lcsi_crisis3]] == exhausted_val   ~ "1",
          TRUE ~ "0"
        ),
        fsl_lcsi_crisis = dplyr::case_when(
          is.na(fsl_lcsi_crisis_yes) & is.na(fsl_lcsi_crisis_exhaust) ~ NA_character_,
          fsl_lcsi_crisis_yes == "1" | fsl_lcsi_crisis_exhaust == "1" ~ "1",
          TRUE ~ "0"
        ),

        fsl_lcsi_emergency_yes = dplyr::case_when(
          is.na(.data[[fsl_lcsi_emergency1]]) |
            !.data[[fsl_lcsi_emergency1]] %in% lcsi_cat_values ~ NA_character_,
          .data[[fsl_lcsi_emergency1]] == yes_val      |
            .data[[fsl_lcsi_emergency2]] == yes_val   |
            .data[[fsl_lcsi_emergency3]] == yes_val   ~ "1",
          TRUE ~ "0"
        ),
        fsl_lcsi_emergency_exhaust = dplyr::case_when(
          is.na(.data[[fsl_lcsi_emergency1]]) |
            !.data[[fsl_lcsi_emergency1]] %in% lcsi_cat_values ~ NA_character_,
          .data[[fsl_lcsi_emergency1]] == exhausted_val      |
            .data[[fsl_lcsi_emergency2]] == exhausted_val   |
            .data[[fsl_lcsi_emergency3]] == exhausted_val   ~ "1",
          TRUE ~ "0"
        ),
        fsl_lcsi_emergency = dplyr::case_when(
          is.na(fsl_lcsi_emergency_yes) & is.na(fsl_lcsi_emergency_exhaust) ~ NA_character_,
          fsl_lcsi_emergency_yes == "1" | fsl_lcsi_emergency_exhaust == "1" ~ "1",
          TRUE ~ "0"
        ),

        # Category based on YES only
        fsl_lcsi_cat_yes = dplyr::case_when(
          fsl_lcsi_stress_yes    != "1" &
            fsl_lcsi_crisis_yes  != "1" &
            fsl_lcsi_emergency_yes != "1" ~ iphra_txt("None"),
          fsl_lcsi_stress_yes == "1" &
            fsl_lcsi_crisis_yes != "1" &
            fsl_lcsi_emergency_yes != "1" ~ iphra_txt("Stress"),
          fsl_lcsi_crisis_yes == "1" &
            fsl_lcsi_emergency_yes != "1" ~ iphra_txt("Crisis"),
          fsl_lcsi_emergency_yes == "1" ~ iphra_txt("Emergency"),
          TRUE ~ NA_character_
        ),

        # Category based on EXHAUSTED only
        fsl_lcsi_cat_exhaust = dplyr::case_when(
          fsl_lcsi_stress_exhaust    != "1" &
            fsl_lcsi_crisis_exhaust  != "1" &
            fsl_lcsi_emergency_exhaust != "1" ~ iphra_txt("None"),
          fsl_lcsi_stress_exhaust == "1" &
            fsl_lcsi_crisis_exhaust != "1" &
            fsl_lcsi_emergency_exhaust != "1" ~ iphra_txt("Stress"),
          fsl_lcsi_crisis_exhaust == "1" &
            fsl_lcsi_emergency_exhaust != "1" ~ iphra_txt("Crisis"),
          fsl_lcsi_emergency_exhaust == "1" ~ iphra_txt("Emergency"),
          TRUE ~ NA_character_
        ),

        # Combined category (YES or EXHAUSTED)
        fsl_lcsi_cat = dplyr::case_when(
          fsl_lcsi_stress    != "1" &
            fsl_lcsi_crisis  != "1" &
            fsl_lcsi_emergency != "1" ~ iphra_txt("None"),
          fsl_lcsi_stress == "1" &
            fsl_lcsi_crisis != "1" &
            fsl_lcsi_emergency != "1" ~ iphra_txt("Stress"),
          fsl_lcsi_crisis == "1" &
            fsl_lcsi_emergency != "1" ~ iphra_txt("Crisis"),
          fsl_lcsi_emergency == "1" ~ iphra_txt("Emergency"),
          TRUE ~ NA_character_
        )
      )


    # 5. Enforce Option A: if any of 10 inputs are NA,
    #    all derived LCSI fields become NA for that row

    derived_cols <- c(
      "fsl_lcsi_stress_yes", "fsl_lcsi_stress_exhaust", "fsl_lcsi_stress",
      "fsl_lcsi_crisis_yes", "fsl_lcsi_crisis_exhaust", "fsl_lcsi_crisis",
      "fsl_lcsi_emergency_yes", "fsl_lcsi_emergency_exhaust", "fsl_lcsi_emergency",
      "fsl_lcsi_cat_yes", "fsl_lcsi_cat_exhaust", "fsl_lcsi_cat"
    )

    .dataset <- .dataset %>%
      dplyr::mutate(
        complete_lcsi_inputs = complete_cases
      ) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(derived_cols),
          ~ dplyr::if_else(
            complete_lcsi_inputs,
            .x,
            NA_character_
          )
        )
      ) %>%
      dplyr::select(-complete_lcsi_inputs)


    # 6. Standardize factor outputs

    .dataset <- .dataset %>%
      dplyr::mutate(
        fsl_lcsi_cat_yes = factor(
          fsl_lcsi_cat_yes,
          levels = c(
            iphra_txt("Emergency"),
            iphra_txt("Crisis"),
            iphra_txt("Stress"),
            iphra_txt("None")
          ),
          ordered = TRUE
        ),
        fsl_lcsi_cat_exhaust = factor(
          fsl_lcsi_cat_exhaust,
          levels = c(
            iphra_txt("Emergency"),
            iphra_txt("Crisis"),
            iphra_txt("Stress"),
            iphra_txt("None")
          ),
          ordered = TRUE
        ),
        fsl_lcsi_cat = factor(
          fsl_lcsi_cat,
          levels = c(
            iphra_txt("Emergency"),
            iphra_txt("Crisis"),
            iphra_txt("Stress"),
            iphra_txt("None")
          ),
          ordered = TRUE
        )
      )

    # Add logic for flag_lcsi_na
    # Count the `not_applicable` responses across all LCSI variables dynamically
    .dataset <- .dataset %>%
      dplyr::mutate(
        lcsi_count_na = apply(
          dplyr::select(.dataset, dplyr::all_of(lcsi_vars)),
          1,
          function(x) sum(x == not_applicable_val, na.rm = TRUE)
        ),
        flag_lcsi_na = dplyr::case_when(
          is.na(lcsi_count_na) ~ NA_real_,
          lcsi_count_na >= 4   ~ 1,
          TRUE                 ~ 0
        )
      )

    iphra_message(origin, iphra_txt("LCSI indicators and categories computed successfully."))

    return(.dataset)

  }, on_error = "abort", origin = origin, hint = iphra_txt("Ensure all LCSI columns exist and contain valid response values."))
}

#' Calculate Household Dietary Diversity Score (HDDS)
#'
#' @description
#' Calculates HDDS count and category based on yes/no responses to 12 food
#' group consumption questions.
#'
#' @param .dataset Data frame containing HDDS component variables
#' @param fsl_hdds_cereals Character: Column for cereals consumption (yes/no)
#' @param fsl_hdds_tubers Character: Column for tubers consumption (yes/no)
#' @param fsl_hdds_veg Character: Column for vegetables consumption (yes/no)
#' @param fsl_hdds_fruit Character: Column for fruit consumption (yes/no)
#' @param fsl_hdds_meat Character: Column for meat consumption (yes/no)
#' @param fsl_hdds_eggs Character: Column for eggs consumption (yes/no)
#' @param fsl_hdds_fish Character: Column for fish consumption (yes/no)
#' @param fsl_hdds_legumes Character: Column for legumes consumption (yes/no)
#' @param fsl_hdds_dairy Character: Column for dairy consumption (yes/no)
#' @param fsl_hdds_oil Character: Column for oil consumption (yes/no)
#' @param fsl_hdds_sugar Character: Column for sugar consumption (yes/no)
#' @param fsl_hdds_condiments Character: Column for condiments consumption (yes/no)
#' @param yes_val Character: Value representing "yes" response
#' @param no_val Character: Value representing "no" response
#'
#' @return Data frame with added columns: fsl_hdds_count, fsl_hdds_cat
#' @export
add_hdds <- function(.dataset,
                     fsl_hdds_cereals = "fsl_hdds_cereals",
                     fsl_hdds_tubers = "fsl_hdds_tubers",
                     fsl_hdds_veg = "fsl_hdds_veg",
                     fsl_hdds_fruit = "fsl_hdds_fruit",
                     fsl_hdds_meat = "fsl_hdds_meat",
                     fsl_hdds_eggs = "fsl_hdds_eggs",
                     fsl_hdds_fish = "fsl_hdds_fish",
                     fsl_hdds_legumes = "fsl_hdds_legumes",
                     fsl_hdds_dairy = "fsl_hdds_dairy",
                     fsl_hdds_oil = "fsl_hdds_oil",
                     fsl_hdds_sugar = "fsl_hdds_sugar",
                     fsl_hdds_condiments = "fsl_hdds_condiments",
                     yes_val = "yes",
                     no_val = "no") {

  origin <- "add_hdds"

  iphra_try({

    # Use ensure_value for all *_val parameters
    yes_val <- ensure_value(yes_val, "yes")
    no_val <- ensure_value(no_val, "no")


    # Validate dataset

    iphra_validate_dataframe(
      .dataset,
      origin = origin,
      hint = iphra_txt("Confirm that you passed a valid data.frame from your cleaning pipeline."),
      soft = FALSE
    )

    iphra_assert(
      nrow(.dataset) > 0,
      origin,
      iphra_txt("Dataset is empty."),
      hint = iphra_txt("Check that your input data has rows after filtering or import.")
    )




    # Required columns

    hdds_vars <- c(
      fsl_hdds_cereals, fsl_hdds_tubers, fsl_hdds_veg, fsl_hdds_fruit,
      fsl_hdds_meat, fsl_hdds_eggs, fsl_hdds_fish, fsl_hdds_legumes,
      fsl_hdds_dairy, fsl_hdds_oil, fsl_hdds_sugar, fsl_hdds_condiments
    )

    iphra_validate_columns(
      .dataset,
      hdds_vars,
      origin = origin,
      hint = iphra_txt("Check that all 12 HDDS food group variables are present and correctly named."),
      soft = FALSE
    )


    # Validate text choices

    allowed <- c(yes_val, no_val)

    for (v in hdds_vars) {
      # Allow NA as valid input
      iphra_validate_choice(
        x       = .dataset[[v]],
        choices = c(allowed, NA_character_),
        origin  = origin,
        soft    = FALSE
      )
    }


    # Overwrite warnings for any existing output columns

    overwrite_vars <- c(
      "fsl_hdds_score",
      "fsl_hdds_cat"
    )

    for (var in overwrite_vars) {
      if (var %in% names(.dataset)) {
        iphra_warning(
          origin = origin,
          message = iphra_txt(glue::glue("Variable {var} already exists and will be overwritten."))
        )
      }
    }


    # Compute HDDS Score
    # Treat invalid values (not yes/no) as NA
    # Apply strict NA rule: if any HDDS variable is NA or invalid → score = NA

    .dataset <- .dataset |>
      dplyr::rowwise() |>
      dplyr::mutate(
        fsl_hdds_score = {
          # Check for any NA or invalid values
          hdds_values <- dplyr::c_across(all_of(hdds_vars))
          has_na <- any(is.na(hdds_values))
          has_invalid <- any(!hdds_values %in% c(yes_val, no_val, NA_character_))

          if (has_na || has_invalid) {
            NA_real_
          } else {
            sum(hdds_values == yes_val)
          }
        },

        fsl_hdds_cat = dplyr::case_when(
          is.na(fsl_hdds_score) ~ NA_character_,
          fsl_hdds_score <= 2 ~ iphra_txt("Low"),
          fsl_hdds_score <= 4 ~ iphra_txt("Medium"),
          fsl_hdds_score > 4 ~ iphra_txt("High")
        )
      ) |>
      dplyr::ungroup()


    # Standardize factor levels

    .dataset <- .dataset |>
      dplyr::mutate(
        fsl_hdds_cat = factor(
          fsl_hdds_cat,
          levels = c(
            iphra_txt("Low"),
            iphra_txt("Medium"),
            iphra_txt("High")
          )
        )
      )

    iphra_message(
      origin,
      iphra_txt("HDDS score and category computed successfully.")
    )

    return(.dataset)

  },
  on_error = "abort",
  origin = origin)
}

#' Compute FEWS NET Matrix Classification (FCM Phase)
#'
#' The `add_fcm_phase` function computes the Food Consumption and Livelihood Coping Mechanism (FCM) classification based on the FEWS NET Matrix Guidance. This function dynamically processes categorical data related to food consumption scores (FCS), reduced coping strategy index (rCSI), household hunger score (HHS), and household dietary diversity score (HDDS) to calculate classification outputs.
#'
#' This function implements the methodology as outlined in the [FEWS NET Matrix Guidance Document](https://fews.net/sites/default/files/documents/reports/fews-net-matrix-guidance-document.pdf).
#'
#' @param .dataset A data frame containing columns with food consumption data (e.g., FCS, rCSI, HHS, HDDS).
#' @param fcs_col A character string indicating the column name for the food consumption score (FCS) categories. Default: `"fsl_fcs_cat"`.
#' @param rcsi_col A character string indicating the column name for the reduced coping strategy index (rCSI) categories. Default: `"fsl_rcsi_cat"`.
#' @param hhs_col A character string indicating the column name for the household hunger score (HHS) categories. Default: `"fsl_hhs_cat_ipc"`.
#' @param hdds_col A character string indicating the column name for the household dietary diversity score (HDDS) categories. Default: `"fsl_hdds_cat"`.
#' @param fcs_acceptable_val A character string for the "Acceptable" FCS category. Default: `NULL`. (Backup value: `"Acceptable"`)
#' @param fcs_borderline_val A character string for the "Borderline" FCS category. Default: `NULL`. (Backup value: `"Borderline"`)
#' @param fcs_poor_val A character string for the "Poor" FCS category. Default: `NULL`. (Backup value: `"Poor"`)
#' @param rcsi_low_val A character string for the "Low" rCSI category. Default: `NULL`. (Backup value: `"Low"`)
#' @param rcsi_medium_val A character string for the "Medium" rCSI category. Default: `NULL`. (Backup value: `"Medium"`)
#' @param rcsi_high_val A character string for the "High" rCSI category. Default: `NULL`. (Backup value: `"High"`)
#' @param hdds_low_val A character string for the "Low" HDDS category. Default: `NULL`. (Backup value: `"Low"`)
#' @param hdds_medium_val A character string for the "Medium" HDDS category. Default: `NULL`. (Backup value: `"Medium"`)
#' @param hdds_high_val A character string for the "High" HDDS category. Default: `NULL`. (Backup value: `"High"`)
#' @param hhs_none_val A character string for the "None" HHS category. Default: `NULL`. (Backup value: `"None"`)
#' @param hhs_little_val A character string for the "Little" HHS category. Default: `NULL`. (Backup value: `"Little"`)
#' @param hhs_moderate_val A character string for the "Moderate" HHS category. Default: `NULL`. (Backup value: `"Moderate"`)
#' @param hhs_severe_val A character string for the "Severe" HHS category. Default: `NULL`. (Backup value: `"Severe"`)
#' @param hhs_very_severe_val A character string for the "Very Severe" HHS category. Default: `NULL`. (Backup value: `"Very Severe"`)
#'
#' @return A data frame (`.dataset`) with additional columns:
#'   - `fsl_fc_cell`: A numeric column indicating the classification cell derived from FEWS NET lookup tables.
#'   - `fsl_fc_phase`: A character column indicating the FCM phase classification.
#'
#' @details
#' The function implements the FEWS NET methodology for classifying households or populations across phases of food insecurity based on a matrix of food consumption and livelihood coping indicators:
#' - Food consumption scores (FCS): Represents food security based on dietary diversity, food frequency, and relative nutritional importance of food groups.
#' - Reduced Coping Strategy Index (rCSI): Represents the severity of food access-related coping strategies employed by households.
#' - Household Hunger Score (HHS): Measures household hunger levels.
#' - Household Dietary Diversity Score (HDDS): Represents dietary diversity.
#'
#' The process involves:
#' 1. Setting default values for the `_val` arguments if passed as `NULL`.
#' 2. Using embedded lookup tables specifying relationships between FCS, rCSI, HHS, and HDDS categories to classification cells and phases.
#' 3. Validating expected values for category columns (dynamically based on user-provided or default `_val` arguments).
#' 4. Enriching the `dataset` with derived columns through matrix classification logic.
#'
#' @section Categories:
#' The function supports dynamically passed categories but uses sensible defaults for classification:
#' - FCS: `"Acceptable"`, `"Borderline"`, `"Poor"`.
#' - rCSI: `"Low"`, `"Medium"`, `"High"`.
#' - HDDS: `"Low"`, `"Medium"`, `"High"`.
#' - HHS: `"None"`, `"Little"`, `"Moderate"`, `"Severe"`, `"Very Severe"`.
#'
#' @section Outputs:
#' - `fsl_fc_cell`: A numeric column representing the classification cell.
#' - `fsl_fc_phase`: A character column representing the FCM phase classification (e.g., `"P1"`, `"P2"`, etc.).
#'
#' @examples
#' # Example dataset
#' example_data <- tibble::tibble(
#'   fsl_fcs_cat = c("Acceptable", "Borderline", "Poor"),
#'   fsl_rcsi_cat = c("Low", "Medium", "High"),
#'   fsl_hhs_cat_ipc = c("None", "Little", "Moderate")
#' )
#'
#' # Compute FCM classification with default FEWS NET values
#' output <- add_fcm_phase(
#'   .dataset = example_data
#' )
#'
#' # Compute FCM classification with custom category values
#' output <- add_fcm_phase(
#'   .dataset = example_data,
#'   fcs_acceptable_val = "Good",
#'   fcs_poor_val = "Bad"
#' )
#'
#' @seealso [dplyr::left_join()] for joining the dataset with lookup tables.
#' @export
add_fcm_phase <- function(
    .dataset,
    fcs_col  = "fsl_fcs_cat",
    rcsi_col = "fsl_rcsi_cat",
    hhs_col  = "fsl_hhs_cat_ipc",
    hdds_col = "fsl_hdds_cat",
    fcs_acceptable_val = NULL,
    fcs_borderline_val = NULL,
    fcs_poor_val = NULL,
    rcsi_low_val = NULL,
    rcsi_medium_val = NULL,
    rcsi_high_val = NULL,
    hdds_low_val = NULL,
    hdds_medium_val = NULL,
    hdds_high_val = NULL,
    hhs_none_val = NULL,
    hhs_little_val = NULL,
    hhs_moderate_val = NULL,
    hhs_severe_val = NULL,
    hhs_very_severe_val = NULL
) {

  iphra_try({

    # Use ensure_value for all *_val parameters
    fcs_acceptable_val <- ensure_value(fcs_acceptable_val, "Acceptable")
    fcs_borderline_val <- ensure_value(fcs_borderline_val, "Borderline")
    fcs_poor_val <- ensure_value(fcs_poor_val, "Poor")
    rcsi_low_val <- ensure_value(rcsi_low_val, "Low")
    rcsi_medium_val <- ensure_value(rcsi_medium_val, "Medium")
    rcsi_high_val <- ensure_value(rcsi_high_val, "High")
    hdds_low_val <- ensure_value(hdds_low_val, "Low")
    hdds_medium_val <- ensure_value(hdds_medium_val, "Medium")
    hdds_high_val <- ensure_value(hdds_high_val, "High")
    hhs_none_val <- ensure_value(hhs_none_val, "None")
    hhs_little_val <- ensure_value(hhs_little_val, "Little")
    hhs_moderate_val <- ensure_value(hhs_moderate_val, "Moderate")
    hhs_severe_val <- ensure_value(hhs_severe_val, "Severe")
    hhs_very_severe_val <- ensure_value(hhs_very_severe_val, "Very Severe")


    # 1. EMBEDDED LOOKUP TABLES
    lookup_fcs_rcsi <- tribble(
      ~fcs,          ~rcsi,      ~cell, ~cat,
      fcs_acceptable_val,  rcsi_low_val,       1, "P1",
      fcs_acceptable_val,  rcsi_medium_val,   2, "P2",
      fcs_acceptable_val,  rcsi_high_val,     3, "P3",
      fcs_borderline_val,  rcsi_low_val,      4, "P1",
      fcs_borderline_val,  rcsi_medium_val,   5, "P2",
      fcs_borderline_val,  rcsi_high_val,     6, "P3",
      fcs_poor_val,         rcsi_low_val,     7, "P2",
      fcs_poor_val,         rcsi_medium_val,  8, "P3",
      fcs_poor_val,         rcsi_high_val,    9, "P4",
    )

    lookup_hdds_rcsi <- tribble(
      ~hdds,     ~rcsi,     ~cell, ~cat,
      hdds_high_val,    rcsi_low_val,        1,   "P1",
      hdds_high_val,    rcsi_medium_val,     2,   "P2",
      hdds_high_val,    rcsi_high_val,       3,   "P3",
      hdds_medium_val,  rcsi_low_val,        4,   "P1",
      hdds_medium_val,  rcsi_medium_val,     5,   "P2",
      hdds_medium_val,  rcsi_high_val,       6,   "P3",
      hdds_low_val,     rcsi_low_val,        7,   "P2",
      hdds_low_val,     rcsi_medium_val,     8,   "P3",
      hdds_low_val,     rcsi_high_val,       9,   "P4"
    )

    lookup_hhs_fcs <- tribble(
      ~hhs,          ~fcs,          ~cell, ~cat,
      hhs_none_val,        fcs_acceptable_val,     1, "P1",
      hhs_none_val,        fcs_borderline_val,     6, "P1",
      hhs_none_val,        fcs_poor_val,           11, "P2",
      hhs_little_val,      fcs_acceptable_val,     2, "P2",
      hhs_little_val,      fcs_borderline_val,     7, "P2",
      hhs_little_val,      fcs_poor_val,           12, "P3",
      hhs_moderate_val,    fcs_acceptable_val,     3, "P2",
      hhs_moderate_val,    fcs_borderline_val,     8, "P3",
      hhs_moderate_val,    fcs_poor_val,           13, "P3",
      hhs_severe_val,      fcs_acceptable_val,    4, "P3",
      hhs_severe_val,      fcs_borderline_val,    9, "P4",
      hhs_severe_val,      fcs_poor_val,          14, "P4",
      hhs_very_severe_val, fcs_acceptable_val,    5, "P4",
      hhs_very_severe_val, fcs_borderline_val,    10, "P4",
      hhs_very_severe_val, fcs_poor_val,          15, "P5"
    )

    lookup_hhs_hdds <- tribble(
      ~hhs,          ~hdds,      ~cell, ~cat,
      hhs_none_val,        hdds_high_val,        1, "P1",
      hhs_none_val,        hdds_medium_val,      6, "P1",
      hhs_none_val,        hdds_low_val,         11, "P2",
      hhs_little_val,      hdds_high_val,        2, "P2",
      hhs_little_val,      hdds_medium_val,      7, "P2",
      hhs_little_val,      hdds_low_val,         12, "P3",
      hhs_moderate_val,    hdds_high_val,        3, "P2",
      hhs_moderate_val,    hdds_medium_val,      8, "P3",
      hhs_moderate_val,    hdds_low_val,         13, "P3",
      hhs_severe_val,      hdds_high_val,       4, "P3",
      hhs_severe_val,      hdds_medium_val,     9, "P4",
      hhs_severe_val,      hdds_low_val,        14, "P4",
      hhs_very_severe_val, hdds_high_val,       15, "P4",
      hhs_very_severe_val, hdds_medium_val,     10, "P4",
      hhs_very_severe_val, hdds_low_val,        15, "P5"
    )

    lookup_hhs_rcsi_fcs_sorted2 <- tribble(
      ~hhs,          ~rcsi,      ~fcs,          ~cell, ~cat,

      # HHS = None
      hhs_none_val,        rcsi_low_val,      fcs_acceptable_val,   1, "P1",
      hhs_none_val,        rcsi_low_val,      fcs_borderline_val,   6, "P1",
      hhs_none_val,        rcsi_low_val,      fcs_poor_val,         11, "P2",

      hhs_none_val,        rcsi_medium_val,   fcs_acceptable_val,   16, "P2",
      hhs_none_val,        rcsi_medium_val,   fcs_borderline_val,   21, "P2",
      hhs_none_val,        rcsi_medium_val,   fcs_poor_val,         26, "P2",

      hhs_none_val,        rcsi_high_val,     fcs_acceptable_val,   31, "P2",
      hhs_none_val,        rcsi_high_val,     fcs_borderline_val,   36, "P2",
      hhs_none_val,        rcsi_high_val,     fcs_poor_val,         41, "P3",


      # HHS = Little
      hhs_little_val,      rcsi_low_val,      fcs_acceptable_val,  2, "P2",
      hhs_little_val,      rcsi_low_val,      fcs_borderline_val,  7, "P2",
      hhs_little_val,      rcsi_low_val,      fcs_poor_val,        12, "P2",

      hhs_little_val,      rcsi_medium_val,   fcs_acceptable_val,  17, "P2",
      hhs_little_val,      rcsi_medium_val,   fcs_borderline_val,  22, "P2",
      hhs_little_val,      rcsi_medium_val,   fcs_poor_val,        27, "P3",

      hhs_little_val,      rcsi_high_val,     fcs_acceptable_val,  32, "P2",
      hhs_little_val,      rcsi_high_val,     fcs_borderline_val,  37, "P3",
      hhs_little_val,      rcsi_high_val,     fcs_poor_val,        42, "P3",


      # HHS = Moderate
      hhs_moderate_val,    rcsi_low_val,      fcs_acceptable_val,  3, "P2",
      hhs_moderate_val,    rcsi_low_val,      fcs_borderline_val,  8, "P3",
      hhs_moderate_val,    rcsi_low_val,      fcs_poor_val,        13, "P3",

      hhs_moderate_val,    rcsi_medium_val,   fcs_acceptable_val,  18, "P2",
      hhs_moderate_val,    rcsi_medium_val,   fcs_borderline_val,  23, "P3",
      hhs_moderate_val,    rcsi_medium_val,   fcs_poor_val,        28, "P3",

      hhs_moderate_val,    rcsi_high_val,     fcs_acceptable_val,  33, "P3",
      hhs_moderate_val,    rcsi_high_val,     fcs_borderline_val,  38, "P3",
      hhs_moderate_val,    rcsi_high_val,     fcs_poor_val,        43, "P3",


      # HHS = Severe
      hhs_severe_val,      rcsi_low_val,      fcs_acceptable_val,  4, "P3",
      hhs_severe_val,      rcsi_low_val,      fcs_borderline_val,  9, "P3",
      hhs_severe_val,      rcsi_low_val,      fcs_poor_val,        14, "P4",

      hhs_severe_val,      rcsi_medium_val,   fcs_acceptable_val,  19, "P3",
      hhs_severe_val,      rcsi_medium_val,   fcs_borderline_val,  24, "P3",
      hhs_severe_val,      rcsi_medium_val,   fcs_poor_val,        29, "P4",

      hhs_severe_val,      rcsi_high_val,     fcs_acceptable_val,  34, "P3",
      hhs_severe_val,      rcsi_high_val,     fcs_borderline_val,  39, "P4",
      hhs_severe_val,      rcsi_high_val,     fcs_poor_val,        44, "P4",


      # HHS = Very Severe
      hhs_very_severe_val, rcsi_low_val,      fcs_acceptable_val,  5, "P3",
      hhs_very_severe_val, rcsi_low_val,      fcs_borderline_val,  10, "P4",
      hhs_very_severe_val, rcsi_low_val,      fcs_poor_val,        15, "P4",

      hhs_very_severe_val, rcsi_medium_val,   fcs_acceptable_val,  20, "P3",
      hhs_very_severe_val, rcsi_medium_val,   fcs_borderline_val,  25, "P4",
      hhs_very_severe_val, rcsi_medium_val,   fcs_poor_val,        30, "P5",

      hhs_very_severe_val, rcsi_high_val,     fcs_acceptable_val,  35, "P4",
      hhs_very_severe_val, rcsi_high_val,     fcs_borderline_val,  40, "P4",
      hhs_very_severe_val, rcsi_high_val,     fcs_poor_val,        45, "P5"
    )



    # 2. PRECONDITIONS


    iphra_validate_dataframe(
      .dataset,
      origin = "add_fcm_phase",
      soft = FALSE
    )

    iphra_assert(
      nrow(.dataset) > 0,
      origin = "add_fcm_phase"
    )



    # 3. DETECT AVAILABLE INDICATORS


    has_fcs  <- fcs_col  %in% names(.dataset)
    has_rcsi <- rcsi_col %in% names(.dataset)
    has_hhs  <- hhs_col  %in% names(.dataset)
    has_hdds <- hdds_col %in% names(.dataset)



    # Overwrite warnings for existing output columns (consistent)

    overwrite_vars <- c(
      "fsl_fc_cell",
      "fsl_fc_cat"
    )

    for (var in overwrite_vars) {
      if (var %in% names(.dataset)) {
        iphra_warning(
          origin  = "add_fcm_phase",
          message = iphra_txt(glue::glue("Variable {var} already exists and will be overwritten."))
        )
      }
    }


    # 4. VALIDATE CATEGORIES using iphra_validate_choices


    if (has_fcs) {
      iphra_validate_choice(
        .dataset[[fcs_col]],
        choices = c(fcs_acceptable_val, fcs_borderline_val, fcs_poor_val),
        origin = "add_fcm_phase",
        soft = FALSE
      )
    }

    if (has_rcsi) {
      iphra_validate_choice(
        .dataset[[rcsi_col]],
        choices = c(rcsi_low_val, rcsi_medium_val, rcsi_high_val),
        origin = "add_fcm_phase",
        soft = FALSE
      )
    }

    if (has_hhs) {
      iphra_validate_choice(
        .dataset[[hhs_col]],
        choices = c(hhs_none_val, hhs_little_val, hhs_moderate_val, hhs_severe_val, hhs_very_severe_val),
        origin = "add_fcm_phase",
        soft = FALSE
      )
    }

    if (has_hdds) {
      iphra_validate_choice(
        .dataset[[hdds_col]],
        choices = c(hdds_low_val, hdds_medium_val, hdds_high_val),
        origin = "add_fcm_phase",
        soft = FALSE
      )
    }



    # 5. SELECT CORRECT LOOKUP TABLE


    if (has_fcs && has_rcsi && has_hhs) {

      lookup <- lookup_hhs_rcsi_fcs_sorted2

      join_condition <- dplyr::join_by(
        !!rlang::sym(hhs_col)  == hhs,
        !!rlang::sym(rcsi_col) == rcsi,
        !!rlang::sym(fcs_col)  == fcs
      )

    } else if (has_hdds && has_rcsi && !has_fcs && !has_hhs) {

      lookup <- lookup_hdds_rcsi

      join_condition <- dplyr::join_by(
        !!rlang::sym(hdds_col) == hdds,
        !!rlang::sym(rcsi_col) == rcsi
      )

    } else if (has_fcs && has_rcsi && !has_hhs) {

      lookup <- lookup_fcs_rcsi

      join_condition <- dplyr::join_by(
        !!rlang::sym(fcs_col)  == fcs,
        !!rlang::sym(rcsi_col) == rcsi
      )

    } else if (has_fcs && has_hhs && !has_rcsi) {

      lookup <- lookup_hhs_fcs

      join_condition <- dplyr::join_by(
        !!rlang::sym(hhs_col) == hhs,
        !!rlang::sym(fcs_col) == fcs
      )

    } else if (has_hdds && has_hhs && !has_rcsi && !has_fcs) {

      lookup <- lookup_hhs_hdds

      join_condition <- dplyr::join_by(
        !!rlang::sym(hhs_col)  == hhs,
        !!rlang::sym(hdds_col) == hdds
      )

    } else {
      iphra_error(
        origin = "add_fcm_phase",
        iphra_txt("No valid FEWS NET indicator combination detected."),
        hint = iphra_txt("Provide at least one valid combination: (FCS+rCSI), (HDDS+rCSI), (FCS+HHS), (HDDS+HHS), or (FCS+rCSI+HHS).")
      )
    }



    # 6. JOIN LOOKUP TABLE

    # Preserve factor attributes of join key columns before left_join
    # (dplyr::left_join coerces factor columns to character when joining with character lookup)
    join_key_cols <- intersect(c(fcs_col, rcsi_col, hhs_col, hdds_col), names(.dataset))
    factor_attrs <- Filter(Negate(is.null), lapply(join_key_cols, function(col) {
      if (is.factor(.dataset[[col]])) {
        list(col = col, levels = levels(.dataset[[col]]), ordered = is.ordered(.dataset[[col]]))
      } else NULL
    }))

    out <- .dataset |>
      dplyr::left_join(
        lookup,
        by = join_condition
      )



    # 7. OUTPUT COLUMNS


    out <- out %>%
      dplyr::rename(
        fsl_fc_cell = cell,
        fsl_fc_phase  = cat
      ) %>%
      dplyr::mutate(
        fsl_fc_phase = factor(fsl_fc_phase, levels = c("P5", "P4", "P3", "P2", "P1"), ordered = TRUE)
      )

    # Restore factor attributes lost during dplyr::left_join type coercion
    for (fi in factor_attrs) {
      if (fi$col %in% names(out)) {
        out[[fi$col]] <- factor(out[[fi$col]], levels = fi$levels, ordered = fi$ordered)
      }
    }

    iphra_message(
      iphra_txt("FEWS NET matrix classification computed: fsl_fc_cell, fsl_fc_phase.")
    )

    return(out)

  }, on_error = "abort", origin = "add_fcm_phase")
}

#' Compute FCLCM Phase Classification
#'
#' The `add_fclcm_phase` function computes the Food Consumption and Livelihood Coping Mechanism (FCLCM) composite classifications based on user-specified or default categorical values for FC phase and LCSI categories. It joins the dataset with predefined lookup tables to create two new columns: `fsl_fclcm_cell` (classification cell) and `fsl_fclcm_phase` (classification phase).
#' This function implements the methodology as outlined in the [FEWS NET Matrix Guidance Document](https://fews.net/sites/default/files/documents/reports/fews-net-matrix-guidance-document.pdf).
#'
#' @param .dataset A data frame containing the FC phase and LCSI category columns.
#' @param fc_phase_col A character string specifying the column name for the FC phase. Default: `"fsl_fc_phase"`.
#' @param lcsi_col A character string specifying the column name for the LCSI category. Default: `"fsl_lcsi_cat"`.
#' @param p1_val A character string for the "P1" FC phase. Default: `NULL`. (Backup value: `"P1"`.)
#' @param p2_val A character string for the "P2" FC phase. Default: `NULL`. (Backup value: `"P2"`.)
#' @param p3_val A character string for the "P3" FC phase. Default: `NULL`. (Backup value: `"P3"`.)
#' @param p4_val A character string for the "P4" FC phase. Default: `NULL`. (Backup value: `"P4"`.)
#' @param p5_val A character string for the "P5" FC phase. Default: `NULL`. (Backup value: `"P5"`.)
#' @param lcsi_none_val A character string for the "None" LCSI category. Default: `NULL`. (Backup value: `"None"`.)
#' @param lcsi_stress_val A character string for the "Stress" LCSI category. Default: `NULL`. (Backup value: `"Stress"`.)
#' @param lcsi_crisis_val A character string for the "Crisis" LCSI category. Default: `NULL`. (Backup value: `"Crisis"`.)
#' @param lcsi_emergency_val A character string for the "Emergency" LCSI category. Default: `NULL`. (Backup value: `"Emergency"`.)
#' @param lcsi_exhaustion_val A character string for the "Exhaustion" LCSI category. Default: `NULL`. (Backup value: `"Exhaustion"`.)
#'
#' @return A data frame (`.dataset`) with two additional columns:
#'   - `fsl_fclcm_cell`: Classification cell based on the lookup tables.
#'   - `fsl_fclcm_phase`: Classification phase based on the lookup tables.
#'
#' @details
#' The function performs the following steps:
#' 1. Sets default values for the `_val` arguments if they are NULL.
#' 2. Defines two lookup tables:
#'    - `lookup_fclcm_4`: For cases where the LCSI category excludes "Exhaustion".
#'    - `lookup_fclcm_5`: For cases where the LCSI category includes "Exhaustion".
#' 3. Validates that the `.dataset` contains the required columns (`fc_phase_col` and `lcsi_col`).
#' 4. Joins the `lookup` table with `.dataset` by matching the `fc_phase_col` and `lcsi_col` values.
#' 5. Returns the modified `.dataset` with the newly added classification columns.
#'
#' @section Validation:
#' The function validates the following:
#' - That `.dataset` is a valid data frame.
#' - That `fc_phase_col` and `lcsi_col` exist in `.dataset`.
#' - That the values in `fc_phase_col` and `lcsi_col` match the allowable choices specified in the lookup tables (`p1_val` to `p5_val` for FC phases, and `lcsi_*_val` for LCSI categories).
#'
#' @section Outputs:
#' - `fsl_fclcm_cell`: A numeric column indicating the classification cell as defined in the lookup table.
#' - `fsl_fclcm_phase`: A character column indicating the classification phase (e.g., `"P1"`, `"P2"`, etc.).
#'
#' @examples
#' # Example data
#' example_data <- tibble::tibble(
#'   fsl_fc_phase = c("P1", "P2", "P3", "P4", "P5"),
#'   fsl_lcsi_cat = c("None", "Stress", "Crisis", "Emergency", "Exhaustion")
#' )
#'
#' # Compute FCLCM classification using default values
#' output <- add_fclcm_phase(
#'   .dataset = example_data
#' )
#'
#' # Compute FCLCM classification with customized values
#' output <- add_fclcm_phase(
#'   .dataset = example_data,
#'   p1_val = "Phase 1",
#'   p2_val = "Phase 2",
#'   lcsi_stress_val = "Stressed"
#' )
#'
#' @seealso [dplyr::left_join()]
#' @export
add_fclcm_phase <- function(
    .dataset,
    fc_phase_col = "fsl_fc_phase",
    lcsi_col     = "fsl_lcsi_cat",
    p1_val       = NULL,
    p2_val       = NULL,
    p3_val       = NULL,
    p4_val       = NULL,
    p5_val       = NULL,
    lcsi_none_val      = NULL,
    lcsi_stress_val    = NULL,
    lcsi_crisis_val    = NULL,
    lcsi_emergency_val = NULL,
    lcsi_exhaustion_val = NULL
) {
  iphra_try({

    # Use ensure_value for all *_val parameters
    p1_val <- ensure_value(p1_val, "P1")
    p2_val <- ensure_value(p2_val, "P2")
    p3_val <- ensure_value(p3_val, "P3")
    p4_val <- ensure_value(p4_val, "P4")
    p5_val <- ensure_value(p5_val, "P5")
    lcsi_none_val <- ensure_value(lcsi_none_val, "None")
    lcsi_stress_val <- ensure_value(lcsi_stress_val, "Stress")
    lcsi_crisis_val <- ensure_value(lcsi_crisis_val, "Crisis")
    lcsi_emergency_val <- ensure_value(lcsi_emergency_val, "Emergency")
    lcsi_exhaustion_val <- ensure_value(lcsi_exhaustion_val, "Exhaustion")

    # 1. EMBEDDED LOOKUP TABLES
    lookup_fclcm_4 <- dplyr::tribble(
      ~fc_phase, ~lcsi,        ~cell, ~cat,
      # None: 1–5
      p1_val, lcsi_none_val,        1,  p1_val,
      p2_val, lcsi_none_val,        2,  p2_val,
      p3_val, lcsi_none_val,        3,  p3_val,
      p4_val, lcsi_none_val,        4,  p4_val,
      p5_val, lcsi_none_val,        5,  p5_val,

      # Stress: 6–10
      p1_val, lcsi_stress_val,      6,  p1_val,
      p2_val, lcsi_stress_val,      7,  p2_val,
      p3_val, lcsi_stress_val,      8,  p3_val,
      p4_val, lcsi_stress_val,      9,  p4_val,
      p5_val, lcsi_stress_val,     10,  p5_val,

      # Crisis: 11–15
      p1_val, lcsi_crisis_val,     11,  p1_val,
      p2_val, lcsi_crisis_val,     12,  p2_val,
      p3_val, lcsi_crisis_val,     13,  p3_val,
      p4_val, lcsi_crisis_val,     14,  p4_val,
      p5_val, lcsi_crisis_val,     15,  p5_val,

      # Emergency: 16–20
      p1_val, lcsi_emergency_val,  16,  p1_val,
      p2_val, lcsi_emergency_val,  17,  p2_val,
      p3_val, lcsi_emergency_val,  18,  p3_val,
      p4_val, lcsi_emergency_val,  19,  p4_val,
      p5_val, lcsi_emergency_val,  20,  p5_val
    )

    # -------- 5-category LCSI (+ Exhaustion)
    lookup_fclcm_5 <- dplyr::tribble(
      ~fc_phase, ~lcsi,        ~cell, ~cat,
      # None: 1–5
      p1_val, lcsi_none_val,       1, p1_val,
      p2_val, lcsi_none_val,       2, p2_val,
      p3_val, lcsi_none_val,       3, p3_val,
      p4_val, lcsi_none_val,       4, p4_val,
      p5_val, lcsi_none_val,       5, p5_val,

      # Stress: 6–10
      p1_val, lcsi_stress_val,     6, p1_val,
      p2_val, lcsi_stress_val,     7, p2_val,
      p3_val, lcsi_stress_val,     8, p3_val,
      p4_val, lcsi_stress_val,     9, p4_val,
      p5_val, lcsi_stress_val,    10, p5_val,

      # Crisis: 11–15
      p1_val, lcsi_crisis_val,    11, p1_val,
      p2_val, lcsi_crisis_val,    12, p2_val,
      p3_val, lcsi_crisis_val,    13, p3_val,
      p4_val, lcsi_crisis_val,    14, p4_val,
      p5_val, lcsi_crisis_val,    15, p5_val,

      # Emergency: 16–20
      p1_val, lcsi_emergency_val, 16, p1_val,
      p2_val, lcsi_emergency_val, 17, p2_val,
      p3_val, lcsi_emergency_val, 18, p3_val,
      p4_val, lcsi_emergency_val, 19, p4_val,
      p5_val, lcsi_emergency_val, 20, p5_val,

      # Exhaustion: 21–25
      p1_val, lcsi_exhaustion_val,21, p1_val,
      p2_val, lcsi_exhaustion_val,22, p2_val,
      p3_val, lcsi_exhaustion_val,23, p3_val,
      p4_val, lcsi_exhaustion_val,24, p4_val,
      p5_val, lcsi_exhaustion_val,25, p5_val
    )

    # 2. PRECONDITIONS

    iphra_validate_dataframe(
      .dataset,
      origin = "add_fclcm_phase",
      hint   = iphra_txt("Input must be a valid dataframe."),
      soft   = FALSE
    )

    iphra_assert(
      condition = nrow(.dataset) > 0,
      message   = iphra_txt("Dataset is empty; cannot create FCLCM composite."),
      origin    = "add_fclcm_phase",
      hint      = iphra_txt("Provide at least one row of data.")
    )

    iphra_assert(
      condition = fc_phase_col %in% names(.dataset),
      message   = iphra_txt(glue::glue("Missing FC phase column '{fc_phase_col}'.")),
      origin    = "add_fclcm_phase",
      hint      = iphra_txt("Ensure the dataset contains the FC phase variable.")
    )

    iphra_assert(
      condition = lcsi_col %in% names(.dataset),
      message   = iphra_txt(glue::glue("Missing LCSI column '{lcsi_col}'.")),
      origin    = "add_fclcm_phase",
      hint      = iphra_txt("Ensure the dataset contains the LCSI variable.")
    )

    # 3. CATEGORY VALIDATION

    iphra_validate_choice(
      .dataset[[fc_phase_col]],
      choices = c(p1_val, p2_val, p3_val, p4_val, p5_val, NA_character_),
      origin  = "add_fclcm_phase",
      soft    = FALSE
    )

    iphra_validate_choice(
      .dataset[[lcsi_col]],
      choices = c(lcsi_none_val, lcsi_stress_val, lcsi_crisis_val, lcsi_emergency_val, lcsi_exhaustion_val, NA_character_),
      origin  = "add_fclcm_phase",
      soft    = FALSE
    )

    # Overwrite warnings for existing output columns (consistent)

    overwrite_vars <- c(
      "fsl_fclcm_cell",
      "fsl_fclcm_phase"
    )

    for (var in overwrite_vars) {
      if (var %in% names(.dataset)) {
        iphra_warning(
          origin  = "add_fclcm_phase",
          message = iphra_txt(glue::glue("Variable {var} already exists and will be overwritten."))
        )
      }
    }

    # 4. SELECT LOOKUP TABLE

    has_exhaustion <- lcsi_exhaustion_val %in% unique(stats::na.omit(.dataset[[lcsi_col]]))
    lookup <- if (has_exhaustion) lookup_fclcm_5 else lookup_fclcm_4
    lut_label <- if (has_exhaustion) "5-category LCSI" else "4-category LCSI"

    # 5. JOIN + CREATE OUTPUT

    # Preserve factor attributes of join key columns before left_join
    # (dplyr::left_join coerces factor columns to character when joining with character lookup)
    join_key_cols <- intersect(c(fc_phase_col, lcsi_col), names(.dataset))
    factor_attrs <- Filter(Negate(is.null), lapply(join_key_cols, function(col) {
      if (is.factor(.dataset[[col]])) {
        list(col = col, levels = levels(.dataset[[col]]), ordered = is.ordered(.dataset[[col]]))
      } else NULL
    }))

    out <- .dataset |>
      dplyr::left_join(
        lookup,
        by = dplyr::join_by(
          !!rlang::sym(fc_phase_col) == fc_phase,
          !!rlang::sym(lcsi_col)     == lcsi
        )
      ) |>
      dplyr::rename(
        fsl_fclcm_cell  = cell,
        fsl_fclcm_phase = cat
      ) |>
      dplyr::mutate(
        fsl_fclcm_phase = factor(fsl_fclcm_phase, levels = c("P5", "P4", "P3", "P2", "P1"), ordered = TRUE)
      )

    # Restore factor attributes lost during dplyr::left_join type coercion
    for (fi in factor_attrs) {
      if (fi$col %in% names(out)) {
        out[[fi$col]] <- factor(out[[fi$col]], levels = fi$levels, ordered = fi$ordered)
      }
    }

    iphra_message(
      iphra_txt(
        "FCLCM composite applied using {lut_label}. Output columns: fsl_fclcm_cell and fsl_fclcm_phase."
      )
    )

    return(out)

  }, on_error = "abort", origin = "add_fclcm_phase")
}

