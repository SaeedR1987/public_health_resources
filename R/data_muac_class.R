#' MUACDataset: MUAC Dataset Analysis Class
#'
#' @description
#' A class for analyzing Mid-Upper Arm Circumference (MUAC) data in nutritional surveys.
#' Provides methods for data validation, MUAC classification (SAM/MAM/GAM), MUAC-for-age
#' z-scores (MFAZ) calculation, and summary statistics with optional age weighting.
#'
#' @details
#' This class implements WHO standards for MUAC-based nutritional status assessment in
#' children aged 6-59 months. Key features include:
#' * Validation of required variables (MUAC, sex, age)
#' * Classification of Severe Acute Malnutrition (SAM), Moderate Acute Malnutrition (MAM),
#'   and Global Acute Malnutrition (GAM) using both MUAC cutoffs and MFAZ z-scores
#' * Calculation of MUAC-for-age z-scores using WHO standards via zscorer package
#' * Age-weighted prevalence estimation to adjust for sampling bias
#' * Comprehensive summary tables with confidence intervals
#' * Visualization methods for results presentation
#'
#' @field data A data.frame containing the MUAC survey data
#' @field vars A list mapping variable roles to column names in the data
#' @field muac_unit Character; "mm" or "cm" indicating MUAC measurement unit
#' @field valid Logical; TRUE if dataset has been validated
#' @field messages Character vector of validation messages
#' @field sex_values List mapping 'male' and 'female' to their coded values in the data
#'
#' @examples
#' \dontrun{
#'   # Basic usage
#'   muac_data <- MUACDataset$new(
#'     data = my_survey_data,
#'     muac = "muac_cm",
#'     sex = "child_sex",
#'     age_months = "age_mo",
#'     age_days = "age_days",
#'     muac_unit = "cm"
#'   )
#'   muac_data$normalize_muac()
#'   results <- muac_data$summary()
#' }
#'
#' @export
MUACDataset <- R6::R6Class(
  classname = "MUACDataset",
  
  public = list(
    data = NULL,
    vars = list(),
    muac_unit = NULL,
    valid = FALSE,
    messages = character(),
    sex_values = list(male = "M", female = "F"),
    
    #' Initialize a MUAC Dataset
    #'
    #' @description
    #' Creates a new MUACDataset object with specified variable mappings
    #'
    #' @param data A data.frame containing MUAC survey data
    #' @param muac Character; column name for MUAC measurements
    #' @param sex Character; column name for sex/gender variable
    #' @param age_years Character; column name for age in years (optional)
    #' @param age_months Character; column name for age in months (optional)
    #' @param age_days Character; column name for age in days (optional)
    #' @param dob Character; column name for date of birth (optional)
    #' @param oedema_report Character; column name for reported oedema (optional)
    #' @param oedema_confirmed Character; column name for confirmed oedema (optional)
    #' @param referred Character; column name for referral status (optional)
    #' @param child_present Character; column name for child presence indicator (optional)
    #' @param group Character; column name for grouping variable (optional)
    #' @param cluster Character; column name for cluster identifier (optional)
    #' @param enumerator Character; column name for enumerator ID (optional)
    #' @param muac_unit Character; "mm" or "cm" - unit of MUAC measurement
    #' @param male_value Character or numeric; value representing males in sex variable
    #' @param female_value Character or numeric; value representing females in sex variable
    #'
    #' @return A new MUACDataset R6 object
    #'
    #' @note At least one age variable (age_months, age_days, or dob) must be specified
    initialize = function(data,
                          muac,
                          sex,
                          age_years = NULL,
                          age_months = NULL,
                          age_days = NULL,
                          dob = NULL,
                          oedema_report = NULL,
                          oedema_confirmed = NULL,
                          referred = NULL,
                          child_present = NULL,
                          group = NULL,
                          cluster = NULL,
                          enumerator = NULL,
                          muac_unit = c("mm", "cm"),
                          male_value = "M",
                          female_value = "F") {
      stopifnot(is.data.frame(data))
      self$data <- data
      self$muac_unit <- match.arg(muac_unit)
      
      self$vars <- list(
        muac = muac,
        sex = sex,
        age_years = age_years,
        age_months = age_months,
        age_days = age_days,
        dob = dob,
        oedema_report = oedema_report,
        oedema_confirmed = oedema_confirmed,
        referred = referred,
        child_present = child_present,
        group = group,
        cluster = cluster,
        enumerator = enumerator
      )
      
      self$sex_values <- list(male = male_value, female = female_value)
      
      self$validate_variables()
    },
    
    #' Validate Variable Mappings
    #'
    #' @description
    #' Checks that all required and specified variables exist in the dataset
    #'
    #' @return Invisible TRUE if validation succeeds, stops with error otherwise
    #'
    #' @details
    #' Validates that:
    #' * Required variables (muac, sex) are specified and exist in data
    #' * At least one age variable (age_months, age_days, or dob) is specified
    #' * All specified optional variables exist in the dataset
    validate_variables = function() {
      required <- c("muac", "sex")
      missing_vars <- vapply(required, function(v) {
        is.null(self$vars[[v]]) || !(self$vars[[v]] %in% names(self$data))
      }, logical(1))
      
      if (any(missing_vars)) {
        stop(paste("Missing required variable(s):",
                   paste(names(missing_vars)[missing_vars], collapse = ", ")))
      }
      if (is.null(self$vars$age_months) && is.null(self$vars$dob) && is.null(self$vars$age_days)) {
        stop("Either 'age_months', 'age_days', or 'dob' must be specified.")
      }
      
      all_vars <- unlist(self$vars)
      all_vars <- all_vars[!is.null(all_vars)]
      missing_cols <- setdiff(all_vars, names(self$data))
      if (length(missing_cols) > 0) {
        stop(paste("The following columns are missing in data:",
                   paste(missing_cols, collapse = ", ")))
      }
      
      self$valid <- TRUE
      invisible(TRUE)
    },
    
    #' Normalize MUAC and Calculate Classifications
    #'
    #' @description
    #' Converts MUAC to standard units, calculates SAM/MAM/GAM classifications, and
    #' computes MUAC-for-age z-scores (MFAZ) if age in days is available
    #'
    #' @param min_age Numeric; minimum age in months for valid classifications (default: 6)
    #' @param max_age Numeric; maximum age in months for valid classifications (default: 60)
    #'
    #' @return Invisible TRUE; modifies self$data in place by adding classification columns
    #'
    #' @details
    #' Adds the following columns to the dataset:
    #' * `muac_cm`: MUAC converted to centimeters
    #' * `SAM`, `MAM`, `GAM`: "Yes"/"No" classifications based on MUAC cutoffs
    #'   - SAM: MUAC < 11.5 cm or oedema present
    #'   - MAM: MUAC >= 11.5 and < 12.5 cm (without SAM)
    #'   - GAM: SAM or MAM
    #' * `mfaz`: MUAC-for-age z-score (if age_days available)
    #' * `SAM_MFAZ`, `MAM_MFAZ`, `GAM_MFAZ`: Classifications based on z-scores
    #'   - SAM_MFAZ: mfaz < -3 or oedema
    #'   - MAM_MFAZ: mfaz >= -3 and < -2
    #'   - GAM_MFAZ: mfaz < -2 or oedema
    #'
    #' Classifications are set to NA for ages outside [min_age, max_age) range
    normalize_muac = function(min_age = 6, max_age = 60) {
      if (!self$valid) stop("Dataset not validated yet.")
      
      muac_col       <- self$vars$muac
      sex_col        <- self$vars$sex
      age_days_col   <- self$vars$age_days
      age_months_col <- self$vars$age_months
      oed_col        <- self$vars$oedema_confirmed
      oed_present    <- !is.null(oed_col) && oed_col %in% names(self$data)
      
      d <- self$data
      
      # ---- MUAC to cm for general analyses ----
      muac_values <- suppressWarnings(as.numeric(d[[muac_col]]))
      muac_cm <- if (self$muac_unit == "mm") muac_values / 10 else muac_values
      d[[paste0(muac_col, "_cm")]] <- muac_cm
      
      # ---- Oedema flag (robust coercion) ----
      oedema_flag <- rep(FALSE, nrow(d))
      if (oed_present) {
        oed_vec <- d[[oed_col]]
        if (is.character(oed_vec)) {
          oed_vec <- tolower(trimws(oed_vec))
          oedema_flag <- oed_vec %in% c("yes","y","true","1","t")
        } else if (is.numeric(oed_vec)) {
          oedema_flag <- oed_vec > 0
        } else if (is.logical(oed_vec)) {
          oedema_flag <- oed_vec
        }
      }
      
      # ---- MUAC-based classification (cm) ----
      n <- nrow(d)
      sam <- mam <- gam <- rep("No", n)
      sam_idx <- muac_cm < 11.5 | oedema_flag
      mam_idx <- muac_cm >= 11.5 & muac_cm < 12.5 & !sam_idx
      gam_idx <- sam_idx | mam_idx
      sam[sam_idx] <- "Yes"; mam[mam_idx] <- "Yes"; gam[gam_idx] <- "Yes"
      d$SAM <- sam; d$MAM <- mam; d$GAM <- gam
      
      # ---- MFAZ (MUAC-for-age z-scores) via zscorer ----
      # Needs age in days and sex coded as 1(male)/2(female), MUAC in cm.
      if (!is.null(age_days_col) && age_days_col %in% names(d)) {
        message("Calculating and adding MUAC-for-age z-scores (MFAZ).")
        
        # 1) WHO-compatible sex codes
        male_val   <- self$sex_values$male
        female_val <- self$sex_values$female
        sex_chr <- tolower(trimws(as.character(d[[sex_col]])))
        d$sex_who <- NA_integer_
        d$sex_who[sex_chr %in% c(tolower(as.character(male_val)), "m", "male", "1")] <- 1L
        d$sex_who[sex_chr %in% c(tolower(as.character(female_val)), "f", "female", "2")] <- 2L
        
        # 2) MUAC in cm for WHO
        muac_cm <- if (self$muac_unit == "mm") suppressWarnings(as.numeric(d[[muac_col]]) / 10) else suppressWarnings(as.numeric(d[[muac_col]]))
        d$muac_for_who_cm <- muac_cm
        
        # 3) Age in days
        d$age <- suppressWarnings(as.numeric(d[[age_days_col]]))
        
        # 4) Call zscorer
        d <- zscorer::addWGSR(
          data = d,
          sex = "sex_who",
          firstPart = "muac_for_who_cm",  # <<-- cm!
          secondPart = "age",
          index = "mfa"
        )
        
        # 5) MFAZ classifications
        d$SAM_MFAZ <- ifelse(is.na(d$mfaz), NA_integer_,
                             ifelse(d$mfaz < -3, 1L, 0L))
        d$MAM_MFAZ <- ifelse(is.na(d$mfaz), NA_integer_,
                             ifelse(d$mfaz >= -3 & d$mfaz < -2, 1L, 0L))
        d$GAM_MFAZ <- ifelse(is.na(d$mfaz), NA_integer_,
                             ifelse(d$mfaz < -2, 1L, 0L))
        
        # 6) Oedema overrides SAM/GAM by MFAZ (and ensures override even if mfaz is NA)
        if (oed_present) {
          oed_idx <- which(oedema_flag)
          if (length(oed_idx) > 0) {
            d$SAM_MFAZ[oed_idx] <- 1L
            d$GAM_MFAZ[oed_idx] <- 1L
            # Ensure MAM_MFAZ is 0 for oedema cases (not moderate)
            d$MAM_MFAZ[oed_idx] <- 0L
          }
        }
        
        # 7) Apply valid age range (6–59 months)
        if (!is.null(age_months_col) && age_months_col %in% names(d)) {
          d <- d %>%
            dplyr::mutate(
              SAM_MFAZ = ifelse(.data[[age_months_col]] < min_age | .data[[age_months_col]] >= max_age, NA, SAM_MFAZ),
              MAM_MFAZ = ifelse(.data[[age_months_col]] < min_age | .data[[age_months_col]] >= max_age, NA, MAM_MFAZ),
              GAM_MFAZ = ifelse(.data[[age_months_col]] < min_age | .data[[age_months_col]] >= max_age, NA, GAM_MFAZ)
            )
        }
        
      } else {
        message("Skipping MFAZ: 'age_days' not provided.")
      }
      
      self$data <- d
      invisible(TRUE)
    },
    
    #' Generate Summary Statistics
    #'
    #' @description
    #' Produces comprehensive summary statistics for MUAC data including prevalence
    #' estimates, demographic characteristics, and data quality indicators
    #'
    #' @param disaggregate_by Character; optional grouping variable for stratified summaries.
    #'   Options: NULL (overall only), "group", "enumerator", or "cluster"
    #'
    #' @return A tibble with summary statistics including:
    #'   * N_assessed: Number of children with MUAC measurements
    #'   * Mean_MUAC, SD_MUAC: MUAC descriptive statistics (cm)
    #'   * Mean_MFAZ, SD_MFAZ: MUAC-for-age z-score statistics
    #'   * Prop_U24: Proportion of children under 24 months
    #'   * Ratio_U24_to_24_59: Ratio of children <24mo to children 24-59mo
    #'   * Sex_ratio_M_F: Male to female ratio
    #'   * Digit_Pref_Score: Digit preference score (data quality indicator)
    #'   * SAM_n, MAM_n, GAM_n: Counts and percentages by MUAC cutoffs
    #'   * SAM_MFAZ_n, MAM_MFAZ_n, GAM_MFAZ_n: Counts and percentages by z-scores
    #'   * Oedema_n: Count of confirmed oedema cases
    #'
    #' @note Requires normalize_muac() to be run first
    summary = function(disaggregate_by = c(NULL, "group", "enumerator", "cluster")) {
      if (!self$valid) stop("Dataset not validated yet.")
      disaggregate_by <- match.arg(disaggregate_by)
      
      d <- self$data
      muac_col <- paste0(self$vars$muac, "_cm")
      age_mo_col <- self$vars$age_months
      sex_col <- self$vars$sex
      oed_col <- self$vars$oedema_confirmed
      group_col <- self$vars$group
      enum_col <- self$vars$enumerator
      cluster_col <- self$vars$cluster
      male_value <- self$sex_values$male
      female_value <- self$sex_values$female
      
      # ---- DPS using nipnTK ----
      calc_dps <- function(muac) {
        tryCatch({
          nipnTK::digitPreference(muac = muac)[[1]]
        }, error = function(e) {
          warning("Could not calculate digit preference score: ", conditionMessage(e))
          NA_real_
        })
      }
      
      # ---- Main summarizing function ----
      summarize_subset <- function(df, label = "Overall") {
        assessed <- sum(!is.na(df[[muac_col]]))
        mean_muac <- mean(df[[muac_col]], na.rm = TRUE)
        sd_muac <- sd(df[[muac_col]], na.rm = TRUE)
        
        # --- Age-based statistics ---
        if (!is.null(age_mo_col) && age_mo_col %in% names(df)) {
          under24 <- mean(df[[age_mo_col]] < 24, na.rm = TRUE)
          ratio_under_vs_24to59 <- sum(df[[age_mo_col]] < 24, na.rm = TRUE) /
            sum(df[[age_mo_col]] >= 24 & df[[age_mo_col]] <= 59, na.rm = TRUE)
        } else {
          warning("age_months variable not found; skipping age-based ratios.")
          under24 <- NA_real_
          ratio_under_vs_24to_24_59 <- NA_real_
        }
        
        # --- Sex ratio ---
        sex_vec <- df[[sex_col]]
        male_n <- sum(sex_vec == male_value, na.rm = TRUE)
        female_n <- sum(sex_vec == female_value, na.rm = TRUE)
        sex_ratio <- if (female_n > 0) male_n / female_n else NA_real_
        
        # --- DPS ---
        dps <- calc_dps(df[[muac_col]])
        
        # --- MUAC-based SAM/MAM/GAM ---
        sam_n <- sum(df$SAM == "Yes", na.rm = TRUE)
        mam_n <- sum(df$MAM == "Yes", na.rm = TRUE)
        gam_n <- sum(df$GAM == "Yes", na.rm = TRUE)
        
        # --- MFAZ-based statistics (safe) ---
        if ("mfaz" %in% names(df)) {
          mean_mfaz <- mean(df$mfaz, na.rm = TRUE)
          sd_mfaz <- sd(df$mfaz, na.rm = TRUE)
          sam_mfaz_n <- sum(df$SAM_MFAZ == 1, na.rm = TRUE)
          mam_mfaz_n <- sum(df$MAM_MFAZ == 1, na.rm = TRUE)
          gam_mfaz_n <- sum(df$GAM_MFAZ == 1, na.rm = TRUE)
        } else {
          warning("MFAZ variables not found; skipping MFAZ summary columns.")
          mean_mfaz <- sd_mfaz <- NA_real_
          sam_mfaz_n <- mam_mfaz_n <- gam_mfaz_n <- NA_integer_
        }
        
        # --- Oedema count ---
        if (!is.null(oed_col) && oed_col %in% names(df)) {
          oed_vec <- df[[oed_col]]
          if (is.character(oed_vec)) {
            oed_vec <- tolower(trimws(oed_vec))
            oed_vec <- oed_vec %in% c("yes", "y", "1", "true", "t")
          } else if (is.numeric(oed_vec)) {
            oed_vec <- oed_vec > 0
          } else if (is.logical(oed_vec)) {
            oed_vec <- oed_vec
          }
          oed_n <- sum(oed_vec, na.rm = TRUE)
        } else {
          oed_n <- NA_integer_
        }
        
        tibble::tibble(
          Level = label,
          N_assessed = assessed,
          Mean_MUAC = round(mean_muac, 2),
          SD_MUAC = round(sd_muac, 2),
          Mean_MFAZ = round(mean_mfaz, 2),
          SD_MFAZ = round(sd_mfaz, 2),
          Prop_U24 = round(under24, 3),
          Ratio_U24_to_24_59 = round(ratio_under_vs_24to59, 2),
          Sex_ratio_M_F = round(sex_ratio, 2),
          Digit_Pref_Score = round(dps, 2),
          SAM_n = sam_n,
          MAM_n = mam_n,
          GAM_n = gam_n,
          Oedema_n = oed_n,
          SAM_MFAZ_n = sam_mfaz_n,
          MAM_MFAZ_n = mam_mfaz_n,
          GAM_MFAZ_n = gam_mfaz_n,
          SAM_pct = round(100 * sam_n / assessed, 1),
          MAM_pct = round(100 * mam_n / assessed, 1),
          GAM_pct = round(100 * gam_n / assessed, 1),
          SAM_MFAZ_pct = round(100 * sam_mfaz_n / assessed, 1),
          MAM_MFAZ_pct = round(100 * mam_mfaz_n / assessed, 1),
          GAM_MFAZ_pct = round(100 * gam_mfaz_n / assessed, 1)
        )
      }
      
      # ---- Overall summary ----
      result <- summarize_subset(d)
      
      # ---- Optional disaggregation ----
      if (!is.null(disaggregate_by) && disaggregate_by %in% c("group", "enumerator", "cluster")) {
        col <- switch(disaggregate_by,
                      group = group_col,
                      enumerator = enum_col,
                      cluster = cluster_col)
        if (!is.null(col) && col %in% names(d)) {
          res_groups <- d %>%
            group_split(!!sym(col)) %>%
            purrr::map_dfr(~ summarize_subset(.x, label = unique(.x[[col]])[1]))
          result <- dplyr::bind_rows(result, res_groups)
        } else {
          warning(paste("No", disaggregate_by, "variable found; returning overall only."))
        }
      }
      
      return(result)
    },
    
    #' Plot Age Distribution
    #'
    #' @description
    #' Creates a histogram showing the age distribution of children in the sample
    #'
    #' @param disaggregate_by Character; optional grouping variable for faceted plots.
    #'   Options: NULL (overall only), "group", "enumerator", or "cluster"
    #'
    #' @return A ggplot2 object showing age distribution histogram, or NULL if age_months not available
    #'
    #' @note Requires ggplot2 package
    plot_age_distribution = function(disaggregate_by = c(NULL, "group", "enumerator", "cluster")) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' is required for this plot.")
      }
      
      disaggregate_by <- match.arg(disaggregate_by)
      d <- self$data
      age_mo_col <- self$vars$age_months
      group_col <- self$vars$group
      enum_col <- self$vars$enumerator
      cluster_col <- self$vars$cluster
      
      if (is.null(age_mo_col) || !(age_mo_col %in% names(d))) {
        warning("age_months variable not found; cannot plot age distribution.")
        return(NULL)
      }
      
      p <- ggplot2::ggplot(d, ggplot2::aes(x = !!sym(age_mo_col))) +
        ggplot2::geom_histogram(binwidth = 2, fill = "#1f77b4", color = "white") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::labs(
          title = "Age Distribution of Children (Months)",
          x = "Age (months)",
          y = "Count of children"
        )
      
      # Optional disaggregation
      col <- switch(disaggregate_by,
                    group = group_col,
                    enumerator = enum_col,
                    cluster = cluster_col,
                    NULL)
      if (!is.null(col) && col %in% names(d)) {
        p <- p + ggplot2::facet_wrap(ggplot2::vars(!!sym(col)), scales = "free_y")
      }
      
      return(p)
    },
    
    #' Plot SAM/MAM Classification Chart
    #'
    #' @description
    #' Creates a stacked bar chart showing nutritional status (SAM/MAM/Healthy) by sex
    #' and overall, with optional age weighting
    #'
    #' @param by Character; classification method - "muac" for MUAC cutoffs or "mfaz" for z-scores
    #' @param weight_by_age Logical; if TRUE, applies age weighting to adjust for sampling bias
    #' @param pop_under24_prop Numeric; expected population proportion under 24 months (default: 0.333)
    #'
    #' @return A ggplot2 object with stacked bar chart, or NULL if classifications not available
    #'
    #' @details
    #' The chart displays three categories:
    #' * SAM (Severe Acute Malnutrition) - red
    #' * MAM (Moderate Acute Malnutrition) - yellow
    #' * Healthy - green
    #'
    #' Age weighting adjusts prevalence estimates when the sample age distribution differs
    #' from the population. Uses pop_under24_prop as the target proportion <24 months.
    #'
    #' @note Requires ggplot2 and scales packages; requires normalize_muac() to be run first
    plot_sam_mam_chart = function(
    by = c("muac", "mfaz"),
    weight_by_age = FALSE,
    pop_under24_prop = 0.333
    ) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' is required for this plot.")
      }
      if (!requireNamespace("scales", quietly = TRUE)) {
        stop("Package 'scales' is required for percent formatting.")
      }
      
      by <- match.arg(by)
      d <- self$data
      sex_col <- self$vars$sex
      age_mo_col <- self$vars$age_months
      male_val <- self$sex_values$male
      female_val <- self$sex_values$female
      
      # --- Classification logic ---
      if (by == "muac") {
        if (!all(c("SAM", "MAM") %in% names(d))) {
          warning("MUAC-based classifications not found; please run normalize_muac() first.")
          return(NULL)
        }
        d <- d %>%
          dplyr::mutate(
            Category = dplyr::case_when(
              SAM == "Yes" ~ "SAM",
              MAM == "Yes" ~ "MAM",
              TRUE ~ "Healthy"
            )
          )
        title_txt <- "Nutritional Status by MUAC"
      } else if (by == "mfaz") {
        if (!all(c("SAM_MFAZ", "MAM_MFAZ") %in% names(d))) {
          warning("MFAZ-based classifications not found; please run normalize_muac() with age_days.")
          return(NULL)
        }
        d <- d %>%
          dplyr::mutate(
            Category = dplyr::case_when(
              SAM_MFAZ == 1 ~ "SAM",
              MAM_MFAZ == 1 ~ "MAM",
              TRUE ~ "Healthy"
            )
          )
        title_txt <- "Nutritional Status by MFAZ"
      }
      
      # --- Clean up Sex values ---
      d <- d %>%
        dplyr::mutate(
          Sex = dplyr::case_when(
            !!sym(sex_col) == male_val ~ "Male",
            !!sym(sex_col) == female_val ~ "Female",
            TRUE ~ "Unknown"
          )
        )
      
      # --- Apply age weighting if requested ---
      if (weight_by_age) {
        if (is.null(age_mo_col) || !(age_mo_col %in% names(d))) {
          warning("age_months variable not found; skipping weighting.")
          d$age_weight <- 1
        } else {
          valid_age <- dplyr::filter(d, !is.na(.data[[age_mo_col]]) & .data[[age_mo_col]] >= 6 & .data[[age_mo_col]] <= 59)
          if (nrow(valid_age) == 0) {
            warning("No valid age range for weighting; skipping weighting.")
            d$age_weight <- 1
          } else {
            p_samp_under24 <- mean(valid_age[[age_mo_col]] < 24, na.rm = TRUE)
            p_pop_under24 <- pop_under24_prop
            w_under <- p_pop_under24 / p_samp_under24
            w_over <- (1 - p_pop_under24) / (1 - p_samp_under24)
            d <- d %>%
              dplyr::mutate(
                age_weight = dplyr::case_when(
                  !!sym(age_mo_col) < 24 ~ w_under,
                  !!sym(age_mo_col) >= 24 & !!sym(age_mo_col) <= 59 ~ w_over,
                  TRUE ~ 1
                )
              )
            message(glue::glue(
              "Applied age weighting: pop_under24={round(p_pop_under24,3)}, sample_under24={round(p_samp_under24,3)}, weights=({round(w_under,2)}, {round(w_over,2)})"
            ))
          }
        }
      } else {
        d$age_weight <- 1
      }
      
      # --- Prepare summary data ---
      d_summary <- d %>%
        dplyr::mutate(Level = dplyr::case_when(
          Sex == "Male" ~ "Male",
          Sex == "Female" ~ "Female",
          TRUE ~ "Unknown"
        )) %>%
        dplyr::bind_rows(d %>% dplyr::mutate(Level = "Overall"))
      
      # --- Weighted proportions ---
      d_plot <- d_summary %>%
        dplyr::group_by(Level, Category) %>%
        dplyr::summarise(weighted_n = sum(age_weight, na.rm = TRUE), .groups = "drop_last") %>%
        dplyr::mutate(pct = weighted_n / sum(weighted_n, na.rm = TRUE)) %>%
        dplyr::ungroup()
      
      # --- Factor levels and label positions ---
      d_plot$Category <- factor(d_plot$Category, levels = c("SAM", "MAM", "Healthy"))
      d_plot$Level <- factor(d_plot$Level, levels = c("Overall", "Male", "Female"))
      
      d_plot <- d_plot %>%
        dplyr::group_by(Level) %>%
        dplyr::mutate(ypos = cumsum(pct) - (pct / 2)) %>%
        dplyr::ungroup()
      
      # --- Styling ---
      fill_colors <- c("SAM" = "#d73027", "MAM" = "#fee08b", "Healthy" = "#1a9850")
      text_colors <- c("SAM" = "white", "MAM" = "black", "Healthy" = "white")
      
      # --- Plot ---
      p <- ggplot2::ggplot(d_plot, ggplot2::aes(x = Level, y = pct, fill = Category)) +
        ggplot2::geom_bar(stat = "identity", position = "fill", width = 0.7, color = "white", linewidth = 0.4) +
        ggplot2::geom_text(
          ggplot2::aes(label = paste0(round(pct * 100, 1), "%"), y = ypos, color = Category),
          size = 4.2,
          fontface = "bold"
        ) +
        ggplot2::scale_color_manual(values = text_colors, guide = "none") +
        ggplot2::scale_fill_manual(values = fill_colors, name = "Category") +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::labs(
          title = ifelse(weight_by_age,
                         paste0(title_txt, " (Age-weighted)"),
                         title_txt),
          x = NULL,
          y = "Percentage of children"
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 15, hjust = 0.5),
          axis.text.x = ggplot2::element_text(face = "bold", size = 13),
          axis.title.y = ggplot2::element_text(face = "bold", size = 13),
          panel.grid.major.y = ggplot2::element_line(color = "gray85", linewidth = 0.4),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(face = "bold"),
          legend.text = ggplot2::element_text(size = 12)
        )
      
      return(p)
    },
    
    #' Recommend Age Weighting
    #'
    #' @description
    #' Tests whether the sample age distribution differs significantly from expected
    #' population proportions and recommends whether to use age weighting
    #'
    #' @param expected_under24 Numeric; expected population proportion under 24 months (default: 0.333)
    #'
    #' @return A tibble with:
    #'   * Prop_under24: Observed proportion of children <24 months in sample
    #'   * Prop_24to59: Observed proportion of children 24-59 months
    #'   * p_value: P-value from binomial test against expected proportion
    #'   * test_used: Description of statistical test
    #'   * recommendation: Text recommendation on whether to use age weighting
    #'
    #' @details
    #' Uses a two-sided binomial test to compare sample age distribution to expected
    #' population distribution. If p < 0.05, recommends age weighting to adjust for
    #' sampling bias in age distribution.
    recommend_age_weights = function(expected_under24 = 0.333) {
      if (is.null(self$vars$age_months) || !(self$vars$age_months %in% names(self$data))) {
        warning("age_months variable not found; cannot compute age weighting recommendation.")
        return(tibble::tibble(
          Prop_under24 = NA_real_,
          Prop_24to59 = NA_real_,
          p_value = NA_real_,
          test_used = "N/A",
          recommendation = "Insufficient data"
        ))
      }
      
      d <- self$data
      age_mo_col <- self$vars$age_months
      
      # Clean and filter valid ages (6–59 months typical)
      df <- dplyr::filter(d, !is.na(.data[[age_mo_col]]) & .data[[age_mo_col]] >= 6 & .data[[age_mo_col]] <= 59)
      
      if (nrow(df) == 0) {
        warning("No valid age data found between 6–59 months.")
        return(tibble::tibble(
          Prop_under24 = NA_real_,
          Prop_24to59 = NA_real_,
          p_value = NA_real_,
          test_used = "N/A",
          recommendation = "Insufficient data"
        ))
      }
      
      n_under24 <- sum(df[[age_mo_col]] < 24, na.rm = TRUE)
      n_total <- nrow(df)
      prop_under24 <- n_under24 / n_total
      prop_24to59 <- 1 - prop_under24
      
      # --- Binomial test ---
      test_res <- stats::binom.test(n_under24, n_total, p = expected_under24, alternative = "two.sided")
      p_val <- test_res$p.value
      
      # --- Recommendation ---
      recommendation <- if (p_val < 0.05) {
        "Use age weighting (significant deviation)"
      } else {
        "Unweighted results acceptable (no significant deviation)"
      }
      
      tibble::tibble(
        Prop_under24 = round(prop_under24, 3),
        Prop_24to59 = round(prop_24to59, 3),
        p_value = round(p_val, 4),
        test_used = "Binomial test (expected 33% <24m)",
        recommendation = recommendation
      )
    },
    
    #' Summarize Final Results with Confidence Intervals
    #'
    #' @description
    #' Generates a comprehensive final results table with SAM/MAM/GAM prevalence estimates,
    #' confidence intervals, and optional disaggregation by sex and grouping variables
    #'
    #' @param by Character; classification method - "muac" for MUAC cutoffs or "mfaz" for z-scores
    #' @param disaggregate_by Character; optional grouping variable (e.g., "group")
    #' @param weight_by_age Logical; if TRUE, applies age weighting to prevalence estimates
    #' @param pop_under24_prop Numeric; expected population proportion under 24 months (default: 0.333)
    #' @param conf_level Numeric; confidence level for intervals (default: 0.95 for 95\% CI)
    #'
    #' @return A wide-format tibble with prevalence estimates and confidence intervals by:
    #'   * Status (SAM, MAM, GAM)
    #'   * Sex (Overall, Male, Female)
    #'   * Optional grouping variable
    #'
    #' @details
    #' Produces publication-ready results table with:
    #' * Weighted or unweighted prevalence estimates
    #' * Wilson score confidence intervals (robust for small samples)
    #' * Separate columns for overall, male, and female estimates
    #' * Optional stratification by group/cluster/other variable
    #'
    #' @note Requires dplyr, tidyr, tibble, purrr, and binom packages
    summarize_final_results = function(
    by = c("muac", "mfaz"),
    disaggregate_by = NULL,
    weight_by_age = FALSE,
    pop_under24_prop = 0.333,
    conf_level = 0.95
    ) {
      if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' required.")
      if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' required.")
      if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' required.")
      if (!requireNamespace("purrr", quietly = TRUE)) stop("Package 'purrr' required.")
      if (!requireNamespace("binom", quietly = TRUE)) stop("Package 'binom' required.")
      
      by <- match.arg(by)
      d <- self$data
      sex_col <- self$vars$sex
      age_mo_col <- self$vars$age_months
      group_col <- self$vars$group
      male_val <- self$sex_values$male
      female_val <- self$sex_values$female
      
      # --- Classification logic ---
      if (by == "muac") {
        if (!all(c("SAM", "MAM", "GAM") %in% names(d))) {
          warning("MUAC-based classifications not found; please run normalize_muac() first.")
          return(NULL)
        }
        d <- d %>%
          dplyr::mutate(
            SAM_flag = SAM == "Yes",
            MAM_flag = MAM == "Yes",
            GAM_flag = GAM == "Yes"
          )
        title_txt <- "Final Results by MUAC"
      } else {
        if (!all(c("SAM_MFAZ", "MAM_MFAZ", "GAM_MFAZ") %in% names(d))) {
          warning("MFAZ-based classifications not found; please run normalize_muac() with age_days.")
          return(NULL)
        }
        d <- d %>%
          dplyr::mutate(
            SAM_flag = SAM_MFAZ == 1,
            MAM_flag = MAM_MFAZ == 1,
            GAM_flag = GAM_MFAZ == 1
          )
        title_txt <- "Final Results by MFAZ"
      }
      
      # --- Clean up Sex values ---
      d <- d %>%
        dplyr::mutate(
          Sex = dplyr::case_when(
            !!sym(sex_col) == male_val ~ "Male",
            !!sym(sex_col) == female_val ~ "Female",
            TRUE ~ "Unknown"
          )
        )
      
      # --- Apply age weighting if requested ---
      if (weight_by_age) {
        if (is.null(age_mo_col) || !(age_mo_col %in% names(d))) {
          warning("age_months variable not found; skipping weighting.")
          d$age_weight <- 1
        } else {
          valid_age <- dplyr::filter(d, !is.na(.data[[age_mo_col]]) & .data[[age_mo_col]] >= 6 & .data[[age_mo_col]] <= 59)
          if (nrow(valid_age) == 0) {
            warning("No valid age range for weighting; skipping weighting.")
            d$age_weight <- 1
          } else {
            p_samp_under24 <- mean(valid_age[[age_mo_col]] < 24, na.rm = TRUE)
            p_pop_under24 <- pop_under24_prop
            w_under <- p_pop_under24 / p_samp_under24
            w_over <- (1 - p_pop_under24) / (1 - p_samp_under24)
            d <- d %>%
              dplyr::mutate(
                age_weight = dplyr::case_when(
                  !!sym(age_mo_col) < 24 ~ w_under,
                  !!sym(age_mo_col) >= 24 & !!sym(age_mo_col) <= 59 ~ w_over,
                  TRUE ~ 1
                )
              )
            message(glue::glue(
              "Applied age weighting: pop_under24={round(p_pop_under24,3)}, sample_under24={round(p_samp_under24,3)}, weights=({round(w_under,2)}, {round(w_over,2)})"
            ))
          }
        }
      } else {
        d$age_weight <- 1
      }
      
      # --- Helper function for CI calculation ---
      get_ci <- function(x, n, conf.level = 0.90) {
        if (is.na(x) || is.na(n) || n == 0) return(c(NA, NA))
        ci <- binom::binom.confint(x, n, conf.level = conf.level, methods = "wilson")
        return(c(ci$lower * 100, ci$upper * 100))
      }
      
      # --- Helper function to compute one table ---
      make_summary_table <- function(df, label = "Overall") {
        statuses <- c("GAM", "MAM", "SAM")
        
        sex_levels <- c("Overall", "Male", "Female")
        
        out <- list()
        
        for (sex in sex_levels) {
          if (sex == "Overall") {
            df_sex <- df
          } else {
            df_sex <- df %>% dplyr::filter(Sex == sex)
          }
          
          total_wt <- sum(df_sex$age_weight, na.rm = TRUE)
          
          for (st in statuses) {
            flag_var <- paste0(st, "_flag")
            if (!flag_var %in% names(df_sex)) next
            
            num <- sum(df_sex[[flag_var]] * df_sex$age_weight, na.rm = TRUE)
            pct <- 100 * num / total_wt
            
            # Compute approximate CIs using unweighted N (for reporting)
            ci <- get_ci(sum(df_sex[[flag_var]], na.rm = TRUE), nrow(df_sex), conf_level)
            
            out[[length(out) + 1]] <- tibble::tibble(
              Group = label,
              Status = st,
              Sex = sex,
              N = round(num, 1),
              Percent = sprintf("%.1f%% (%.1f–%.1f)", pct, ci[1], ci[2])
            )
          }
        }
        
        df_out <- dplyr::bind_rows(out) %>%
          tidyr::pivot_wider(
            names_from = Sex,
            values_from = c(N, Percent),
            names_glue = "{Sex}_{.value}"
          ) %>%
          dplyr::mutate(Status = factor(Status, levels = statuses)) %>%
          dplyr::arrange(Status)
        
        return(df_out)
      }
      
      # --- Run table build ---
      if (is.null(disaggregate_by)) {
        result <- make_summary_table(d)
      } else if (disaggregate_by == "group" && !is.null(group_col) && group_col %in% names(d)) {
        result <- d %>%
          dplyr::group_split(!!sym(group_col)) %>%
          purrr::map_dfr(~ make_summary_table(.x, label = unique(.x[[group_col]])[1]))
      } else {
        warning("Group variable not found; producing overall table only.")
        result <- make_summary_table(d)
      }
      
      msg_suffix <- if (weight_by_age) " (Age-weighted Ns and %s)" else ""
      message(paste0(title_txt, " — Final results table with 95% CI", msg_suffix, "."))
      
      return(result)
    }
  )
)