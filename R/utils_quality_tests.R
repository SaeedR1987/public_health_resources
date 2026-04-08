#' Statistical Quality Test Functions
#'
#' This file contains statistical test functions used by the DataQuality class
#' for plausibility testing. Each function returns a test statistic or metric
#' that can be compared against thresholds to determine data quality.
#'
#' @name quality_tests
NULL

#' Correlation Test
#'
#' Calculate correlation coefficient between two numeric variables
#'
#' @param data Data frame containing the variables
#' @param variables Character vector of exactly 2 variable names
#' @param method Correlation method: "pearson", "spearman", or "kendall"
#' @return List with statistic (correlation coefficient) and p_value
#' @export
quality_test_correlation <- function(data, variables, method = "pearson") {

  phr_try({

    if (length(variables) != 2) {
      phr_error(
        origin = "quality_test_correlation",
        message = "Correlation test requires exactly 2 variables"
      )
    }

    var1 <- variables[1]
    var2 <- variables[2]

    if (!var1 %in% names(data) || !var2 %in% names(data)) {
      phr_warning(
        origin = "quality_test_correlation",
        message = "One or both variables not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    x <- as.numeric(data[[var1]])
    y <- as.numeric(data[[var2]])

    # Remove pairs with missing values
    complete_cases <- complete.cases(x, y)
    if (sum(complete_cases) < 3) {
      phr_warning(
        origin = "quality_test_correlation",
        message = "Insufficient complete cases for correlation"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Use cor.test to get both correlation and p-value
    test_result <- cor.test(x[complete_cases], y[complete_cases], method = method)

    return(list(
      statistic = as.numeric(test_result$estimate),
      p_value = test_result$p.value
    ))

  }, on_error = "warn", origin = "quality_test_correlation")
}

#' T-Test for Mean Comparison
#'
#' Perform a one-sample t-test or two-sample t-test
#'
#' @param data Data frame containing the variables
#' @param variables Character vector of 1 or 2 variable names
#' @param mu Expected mean for one-sample test (default: 0)
#' @param paired Logical, whether to perform paired t-test for two variables
#' @return List with statistic (t-value) and p-value
#' @export
quality_test_ttest <- function(data, variables, mu = 0, paired = FALSE) {

  phr_try({

    if (length(variables) == 0 || length(variables) > 2) {
      phr_error(
        origin = "quality_test_ttest",
        message = "T-test requires 1 or 2 variables"
      )
    }

    var1 <- variables[1]

    if (!var1 %in% names(data)) {
      phr_warning(
        origin = "quality_test_ttest",
        message = "Variable not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    x <- as.numeric(data[[var1]])
    x <- x[!is.na(x)]

    if (length(x) < 2) {
      phr_warning(
        origin = "quality_test_ttest",
        message = "Insufficient data for t-test"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    if (length(variables) == 1) {
      # One-sample t-test
      test_result <- t.test(x, mu = mu)
    } else {
      # Two-sample t-test
      var2 <- variables[2]
      if (!var2 %in% names(data)) {
        phr_warning(
          origin = "quality_test_ttest",
          message = "Second variable not found in data"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      y <- as.numeric(data[[var2]])
      y <- y[!is.na(y)]

      if (length(y) < 2) {
        phr_warning(
          origin = "quality_test_ttest",
          message = "Insufficient data in second variable"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }

      test_result <- t.test(x, y, paired = paired)
    }

    return(list(
      statistic = as.numeric(test_result$statistic),
      p_value = test_result$p.value
    ))

  }, on_error = "warn", origin = "quality_test_ttest")
}

#' Chi-Squared Test for Independence
#'
#' Perform chi-squared test of independence between two categorical variables
#'
#' @param data Data frame containing the variables
#' @param variables Character vector of exactly 2 variable names
#' @return List with statistic (chi-squared value) and p-value
#' @export
quality_test_chisq <- function(data, variables) {

  phr_try({

    if (length(variables) != 2) {
      phr_error(
        origin = "quality_test_chisq",
        message = "Chi-squared test requires exactly 2 variables"
      )
    }

    var1 <- variables[1]
    var2 <- variables[2]

    if (!var1 %in% names(data) || !var2 %in% names(data)) {
      phr_warning(
        origin = "quality_test_chisq",
        message = "One or both variables not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    x <- data[[var1]]
    y <- data[[var2]]

    # Remove missing values
    complete_cases <- complete.cases(x, y)
    x <- x[complete_cases]
    y <- y[complete_cases]

    if (length(x) < 5) {
      phr_warning(
        origin = "quality_test_chisq",
        message = "Insufficient data for chi-squared test"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Create contingency table
    cont_table <- table(x, y)

    # Check if table has sufficient cells
    if (any(dim(cont_table) < 2)) {
      phr_warning(
        origin = "quality_test_chisq",
        message = "Contingency table too small for chi-squared test"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    test_result <- chisq.test(cont_table)

    return(list(
      statistic = as.numeric(test_result$statistic),
      p_value = test_result$p.value
    ))

  }, on_error = "warn", origin = "quality_test_chisq")
}

#' Flag Percentage Test
#'
#' Calculate the percentage of records with a specific flag value
#'
#' @param data Data frame containing the variable
#' @param variables Character vector with 1 variable name
#' @param flag_value The value to count (default: TRUE or 1)
#' @return List with statistic (percentage 0-100) and p_value (NA)
#' @export
quality_test_flag_percentage <- function(data, variables, flag_value = TRUE) {

  phr_try({

    if (length(variables) != 1) {
      phr_error(
        origin = "quality_test_flag_percentage",
        message = "Flag percentage test requires exactly 1 variable"
      )
    }

    var <- variables[1]

    if (!var %in% names(data)) {
      phr_warning(
        origin = "quality_test_flag_percentage",
        message = "Variable not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    x <- data[[var]]
    x <- x[!is.na(x)]

    if (length(x) == 0) {
      phr_warning(
        origin = "quality_test_flag_percentage",
        message = "No non-missing values found"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Count matching values
    flag_count <- sum(x == flag_value)
    percentage <- (flag_count / length(x)) * 100

    return(list(
      statistic = percentage,
      p_value = NA_real_
    ))

  }, on_error = "warn", origin = "quality_test_flag_percentage")
}

#' Missing Data Percentage Test
#'
#' Calculate the percentage of missing values across specified variables
#'
#' @param data Data frame containing the variables
#' @param variables Character vector of variable names
#' @return List with statistic (percentage 0-100) and p_value (NA)
#' @export
quality_test_missing_percentage <- function(data, variables) {

  phr_try({

    if (length(variables) == 0) {
      phr_error(
        origin = "quality_test_missing_percentage",
        message = "Missing percentage test requires at least 1 variable"
      )
    }

    # Check which variables exist
    existing_vars <- intersect(variables, names(data))

    if (length(existing_vars) == 0) {
      phr_warning(
        origin = "quality_test_missing_percentage",
        message = "No specified variables found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Calculate missing percentage across all specified variables
    total_cells <- 0
    missing_cells <- 0

    for (var in existing_vars) {
      x <- data[[var]]
      total_cells <- total_cells + length(x)
      missing_cells <- missing_cells + sum(is.na(x))
    }

    if (total_cells == 0) {
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    percentage <- (missing_cells / total_cells) * 100

    return(list(
      statistic = percentage,
      p_value = NA_real_
    ))

  }, on_error = "warn", origin = "quality_test_missing_percentage")
}

#' Outlier Detection Test (Z-Score Method)
#'
#' Calculate the percentage of outliers based on z-score threshold
#'
#' @param data Data frame containing the variable
#' @param variables Character vector with 1 variable name
#' @param z_threshold Z-score threshold (default: 3)
#' @return List with statistic (percentage 0-100) and p_value (NA)
#' @export
quality_test_outlier_percentage <- function(data, variables, z_threshold = 3) {

  phr_try({

    # Validate data is a data frame
    phr_validate_dataframe(
      data,
      origin = "quality_test_outlier_percentage",
      hint = phr_txt("Ensure you pass a valid data frame or tibble."),
      soft = FALSE
    )

    # Validate variables is not NULL and has correct length
    if (is.null(variables) || length(variables) != 1) {
      phr_error(
        origin = "quality_test_outlier_percentage",
        message = "Outlier test requires exactly 1 variable"
      )
    }

    # Validate variables is character
    if (!is.character(variables)) {
      phr_error(
        origin = "quality_test_outlier_percentage",
        message = "Variable name must be a character string"
      )
    }

    # Validate z_threshold is numeric and positive
    if (!is.numeric(z_threshold) || length(z_threshold) != 1) {
      phr_error(
        origin = "quality_test_outlier_percentage",
        message = "z_threshold must be a single numeric value"
      )
    }

    if (is.na(z_threshold) || z_threshold <= 0) {
      phr_error(
        origin = "quality_test_outlier_percentage",
        message = "z_threshold must be a positive number"
      )
    }

    var <- variables[1]

    if (!var %in% names(data)) {
      phr_warning(
        origin = "quality_test_outlier_percentage",
        message = "Variable not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Validate that the variable is numeric
    is_valid <- phr_validate_numeric(
      data[[var]],
      origin = "quality_test_outlier_percentage",
      hint = phr_txt("Outlier detection requires numeric data for z-score calculations."),
      soft = TRUE
    )

    # If validation failed, return NA
    if (isFALSE(is_valid)) {
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    x <- as.numeric(data[[var]])
    x <- x[!is.na(x)]

    if (length(x) < 3) {
      phr_warning(
        origin = "quality_test_outlier_percentage",
        message = "Insufficient data for outlier detection (need at least 3 non-missing values)"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Check for zero variance (all values identical)
    if (sd(x) == 0) {
      phr_warning(
        origin = "quality_test_outlier_percentage",
        message = "Variable has zero variance (all values are identical)"
      )
      return(list(statistic = 0, p_value = NA_real_))
    }

    # Calculate z-scores
    z_scores <- (x - mean(x)) / sd(x)

    # Additional safety check for Inf/NaN in z-scores
    if (any(is.infinite(z_scores)) || any(is.nan(z_scores))) {
      phr_warning(
        origin = "quality_test_outlier_percentage",
        message = "Unable to calculate valid z-scores"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    outlier_count <- sum(abs(z_scores) > z_threshold)
    percentage <- (outlier_count / length(x)) * 100

    return(list(
      statistic = percentage,
      p_value = NA_real_
    ))

  }, on_error = "warn", origin = "quality_test_outlier_percentage")
}

#' Coefficient of Variation Test
#'
#' Calculate the coefficient of variation (CV) for a numeric variable
#'
#' @param data Data frame containing the variable
#' @param variables Character vector with 1 variable name
#' @return List with statistic (CV percentage) and p_value (NA)
#' @export
quality_test_coefficient_variation <- function(data, variables) {

  phr_try({

    # Validate data is a data frame
    phr_validate_dataframe(
      data,
      origin = "quality_test_coefficient_variation",
      hint = phr_txt("Ensure you pass a valid data frame or tibble."),
      soft = FALSE
    )

    # Validate variables is not NULL and has correct length
    if (is.null(variables) || length(variables) != 1) {
      phr_error(
        origin = "quality_test_coefficient_variation",
        message = "CV test requires exactly 1 variable"
      )
    }

    # Validate variables is character
    if (!is.character(variables)) {
      phr_error(
        origin = "quality_test_coefficient_variation",
        message = "Variable name must be a character string"
      )
    }

    var <- variables[1]

    if (!var %in% names(data)) {
      phr_warning(
        origin = "quality_test_coefficient_variation",
        message = "Variable not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Validate that the variable is numeric
    is_valid <- phr_validate_numeric(
      data[[var]],
      origin = "quality_test_coefficient_variation",
      hint = phr_txt("CV calculation requires numeric data."),
      soft = TRUE
    )

    # If validation failed, return NA
    if (isFALSE(is_valid)) {
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    x <- as.numeric(data[[var]])
    x <- x[!is.na(x)]

    if (length(x) < 2) {
      phr_warning(
        origin = "quality_test_coefficient_variation",
        message = "Insufficient data for CV calculation (need at least 2 non-missing values)"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    mean_x <- mean(x)
    sd_x <- sd(x)

    # Check for zero variance (all values identical)
    if (sd_x == 0) {
      phr_warning(
        origin = "quality_test_coefficient_variation",
        message = "Variable has zero variance (all values are identical), CV is 0"
      )
      return(list(statistic = 0, p_value = NA_real_))
    }

    # Check for zero mean
    if (mean_x == 0) {
      phr_warning(
        origin = "quality_test_coefficient_variation",
        message = "Mean is zero, CV is undefined"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    cv <- (sd_x / abs(mean_x)) * 100

    # Additional safety check for Inf/NaN in CV
    if (is.infinite(cv) || is.nan(cv)) {
      phr_warning(
        origin = "quality_test_coefficient_variation",
        message = "Unable to calculate valid CV"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    return(list(
      statistic = cv,
      p_value = NA_real_
    ))

  }, on_error = "warn", origin = "quality_test_coefficient_variation")
}

#' Range Violation Test
#'
#' Calculate the percentage of values outside the specified range
#'
#' @param data Data frame containing the variable
#' @param variables Character vector with 1 variable name
#' @param min_value Minimum allowed value (default: -Inf)
#' @param max_value Maximum allowed value (default: Inf)
#' @return List with statistic (percentage 0-100) and p_value (NA)
#' @export
quality_test_range_violation <- function(data, variables, min_value = -Inf, max_value = Inf) {

  phr_try({

    # Validate data is a data frame
    phr_validate_dataframe(
      data,
      origin = "quality_test_range_violation",
      hint = phr_txt("Ensure you pass a valid data frame or tibble."),
      soft = FALSE
    )

    # Validate variables is not NULL and has correct length
    if (is.null(variables) || length(variables) != 1) {
      phr_error(
        origin = "quality_test_range_violation",
        message = "Range test requires exactly 1 variable"
      )
    }

    # Validate variables is character
    if (!is.character(variables)) {
      phr_error(
        origin = "quality_test_range_violation",
        message = "Variable name must be a character string"
      )
    }

    # Validate min_value is numeric
    if (!is.numeric(min_value) || length(min_value) != 1) {
      phr_error(
        origin = "quality_test_range_violation",
        message = "min_value must be a single numeric value"
      )
    }

    # Validate max_value is numeric
    if (!is.numeric(max_value) || length(max_value) != 1) {
      phr_error(
        origin = "quality_test_range_violation",
        message = "max_value must be a single numeric value"
      )
    }

    # Check that min_value and max_value are not NA
    if (is.na(min_value)) {
      phr_error(
        origin = "quality_test_range_violation",
        message = "min_value cannot be NA"
      )
    }

    if (is.na(max_value)) {
      phr_error(
        origin = "quality_test_range_violation",
        message = "max_value cannot be NA"
      )
    }

    # Validate that min_value <= max_value
    if (min_value > max_value) {
      phr_error(
        origin = "quality_test_range_violation",
        message = "min_value must be less than or equal to max_value"
      )
    }

    var <- variables[1]

    if (!var %in% names(data)) {
      phr_warning(
        origin = "quality_test_range_violation",
        message = "Variable not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Validate that the variable is numeric
    is_valid <- phr_validate_numeric(
      data[[var]],
      origin = "quality_test_range_violation",
      hint = phr_txt("Range violation test requires numeric data."),
      soft = TRUE
    )

    # If validation failed, return NA
    if (isFALSE(is_valid)) {
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    x <- as.numeric(data[[var]])
    x <- x[!is.na(x)]

    if (length(x) == 0) {
      phr_warning(
        origin = "quality_test_range_violation",
        message = "No non-missing values found"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Check for infinite values in the data
    if (any(is.infinite(x))) {
      phr_warning(
        origin = "quality_test_range_violation",
        message = "Variable contains infinite values which will be excluded from range check"
      )
      x <- x[is.finite(x)]

      if (length(x) == 0) {
        phr_warning(
          origin = "quality_test_range_violation",
          message = "No finite values found after removing infinite values"
        )
        return(list(statistic = NA_real_, p_value = NA_real_))
      }
    }

    out_of_range <- sum(x < min_value | x > max_value)
    percentage <- (out_of_range / length(x)) * 100

    return(list(
      statistic = percentage,
      p_value = NA_real_
    ))

  }, on_error = "warn", origin = "quality_test_range_violation")
}

#' Standard Deviation Test
#'
#' Calculate the standard deviation for a numeric variable
#'
#' @param data Data frame containing the variable
#' @param variables Character vector with 1 variable name
#' @return List with statistic (standard deviation) and p_value (NA)
#' @export
quality_test_sd <- function(data, variables) {

  phr_try({

    # Validate data is a data frame
    phr_validate_dataframe(
      data,
      origin = "quality_test_sd",
      hint = phr_txt("Ensure you pass a valid data frame or tibble."),
      soft = FALSE
    )

    # Validate variables is not NULL and has correct length
    if (is.null(variables) || length(variables) != 1) {
      phr_error(
        origin = "quality_test_sd",
        message = "Standard deviation test requires exactly 1 variable"
      )
    }

    # Validate variables is character
    if (!is.character(variables)) {
      phr_error(
        origin = "quality_test_sd",
        message = "Variable name must be a character string"
      )
    }

    var <- variables[1]

    if (!var %in% names(data)) {
      phr_warning(
        origin = "quality_test_sd",
        message = "Variable not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Validate that the variable is numeric
    is_valid <- phr_validate_numeric(
      data[[var]],
      origin = "quality_test_sd",
      hint = phr_txt("Standard deviation calculation requires numeric data."),
      soft = TRUE
    )

    # If validation failed, return NA
    if (isFALSE(is_valid)) {
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    x <- as.numeric(data[[var]])
    x <- x[!is.na(x)]

    if (length(x) < 2) {
      phr_warning(
        origin = "quality_test_sd",
        message = "Insufficient data for standard deviation calculation (need at least 2 non-missing values)"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    sd_x <- sd(x)

    # Additional safety check for Inf/NaN in standard deviation
    if (is.infinite(sd_x) || is.nan(sd_x)) {
      phr_warning(
        origin = "quality_test_sd",
        message = "Unable to calculate valid standard deviation"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    return(list(
      statistic = sd_x,
      p_value = NA_real_
    ))

  }, on_error = "warn", origin = "quality_test_sd")
}

#' Standard Deviation Across Columns Percentage Test
#'
#' Calculate the percentage of rows where the standard deviation across
#' specified columns falls below a threshold value
#'
#' @param data Data frame containing the variables
#' @param variables Character vector of variable names (minimum 2)
#' @param threshold Threshold value for SD comparison (default: 0.8)
#' @return List with statistic (percentage 0-100) and p_value (NA)
#' @export
quality_test_sd_across_percentage <- function(data, variables, threshold = 0.8) {

  phr_try({

    # Validate data is a data frame
    phr_validate_dataframe(
      data,
      origin = "quality_test_sd_across_percentage",
      hint = phr_txt("Ensure you pass a valid data frame or tibble."),
      soft = FALSE
    )

    # Validate variables is not NULL and has at least 2 variables
    if (is.null(variables) || length(variables) < 2) {
      phr_error(
        origin = "quality_test_sd_across_percentage",
        message = "SD across percentage test requires at least 2 variables"
      )
    }

    # Validate variables is character
    if (!is.character(variables)) {
      phr_error(
        origin = "quality_test_sd_across_percentage",
        message = "Variable names must be a character vector"
      )
    }

    # Validate threshold is numeric and positive
    if (!is.numeric(threshold) || length(threshold) != 1) {
      phr_error(
        origin = "quality_test_sd_across_percentage",
        message = "threshold must be a single numeric value"
      )
    }

    if (is.na(threshold) || threshold < 0) {
      phr_error(
        origin = "quality_test_sd_across_percentage",
        message = "threshold must be a non-negative number"
      )
    }

    # Check which variables exist in the data
    existing_vars <- intersect(variables, names(data))
    missing_vars <- setdiff(variables, names(data))

    if (length(missing_vars) > 0) {
      phr_warning(
        origin = "quality_test_sd_across_percentage",
        message = paste0("Variables not found in data: ", paste(missing_vars, collapse = ", "))
      )
    }

    if (length(existing_vars) < 2) {
      phr_warning(
        origin = "quality_test_sd_across_percentage",
        message = "At least 2 variables must exist in data for SD calculation"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Validate that all existing variables are numeric
    for (var in existing_vars) {
      is_valid <- phr_validate_numeric(
        data[[var]],
        origin = "quality_test_sd_across_percentage",
        hint = phr_txt(paste0("Variable '", var, "' must be numeric for SD calculation.")),
        soft = TRUE
      )

      if (isFALSE(is_valid)) {
        phr_warning(
          origin = "quality_test_sd_across_percentage",
          message = paste0("Variable '", var, "' is not numeric and will be excluded")
        )
        existing_vars <- setdiff(existing_vars, var)
      }
    }

    # Check again after removing non-numeric variables
    if (length(existing_vars) < 2) {
      phr_warning(
        origin = "quality_test_sd_across_percentage",
        message = "At least 2 numeric variables required after validation"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Extract the subset of data with selected variables
    subset_data <- data[, existing_vars, drop = FALSE]

    # Convert all columns to numeric
    subset_data <- as.data.frame(lapply(subset_data, as.numeric))

    # Calculate row-wise standard deviation
    # Using apply with MARGIN = 1 for row-wise calculation
    row_sd <- apply(subset_data, 1, function(row) {
      # Remove NA values for each row
      row_clean <- row[!is.na(row)]

      # Need at least 2 non-missing values to calculate SD
      if (length(row_clean) < 2) {
        return(NA_real_)
      }

      return(sd(row_clean))
    })

    # Count rows with valid SD calculations
    valid_rows <- !is.na(row_sd)

    if (sum(valid_rows) == 0) {
      phr_warning(
        origin = "quality_test_sd_across_percentage",
        message = "No rows with sufficient non-missing values for SD calculation"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Create flag: 1 if SD < threshold, 0 otherwise
    flag_sd_rcsicoping <- ifelse(valid_rows & row_sd < threshold, 1, 0)

    # Calculate percentage of flagged rows (only among valid rows)
    flagged_count <- sum(flag_sd_rcsicoping[valid_rows] == 1)
    percentage <- (flagged_count / sum(valid_rows)) * 100

    return(list(
      statistic = percentage,
      p_value = NA_real_
    ))

  }, on_error = "warn", origin = "quality_test_sd_across_percentage")
}

#' Any Flag Percentage Test
#'
#' Calculate the percentage of rows where at least one of the specified
#' variables has the flag value
#'
#' @param data Data frame containing the variables
#' @param variables Character vector of variable names (minimum 1)
#' @param flag_value The value to check for (default: 1)
#' @return List with statistic (percentage 0-100) and p_value (NA)
#' @export
quality_test_any_flag_percentage <- function(data, variables, flag_value = 1) {

  phr_try({

    # Validate data is a data frame
    phr_validate_dataframe(
      data,
      origin = "quality_test_any_flag_percentage",
      hint = phr_txt("Ensure you pass a valid data frame or tibble."),
      soft = FALSE
    )

    # Validate variables is not NULL and has at least 1 variable
    if (is.null(variables) || length(variables) < 1) {
      phr_error(
        origin = "quality_test_any_flag_percentage",
        message = "Any flag percentage test requires at least 1 variable"
      )
    }

    # Validate variables is character
    if (!is.character(variables)) {
      phr_error(
        origin = "quality_test_any_flag_percentage",
        message = "Variable names must be a character vector"
      )
    }

    # Validate flag_value is provided and not NULL
    if (is.null(flag_value) || length(flag_value) != 1) {
      phr_error(
        origin = "quality_test_any_flag_percentage",
        message = "flag_value must be a single value"
      )
    }

    # Check which variables exist in the data
    existing_vars <- intersect(variables, names(data))
    missing_vars <- setdiff(variables, names(data))

    if (length(missing_vars) > 0) {
      phr_warning(
        origin = "quality_test_any_flag_percentage",
        message = paste0("Variables not found in data: ", paste(missing_vars, collapse = ", "))
      )
    }

    if (length(existing_vars) == 0) {
      phr_warning(
        origin = "quality_test_any_flag_percentage",
        message = "No specified variables found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Extract the subset of data with selected variables
    subset_data <- data[, existing_vars, drop = FALSE]

    # Check if we have any rows
    if (nrow(subset_data) == 0) {
      phr_warning(
        origin = "quality_test_any_flag_percentage",
        message = "Data has no rows"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Calculate row-wise: does any column have the flag value?
    # Using apply with MARGIN = 1 for row-wise calculation
    any_flag <- apply(subset_data, 1, function(row) {
      # Remove NA values for each row
      row_clean <- row[!is.na(row)]

      # If all values are NA, return NA
      if (length(row_clean) == 0) {
        return(NA)
      }

      # Check if any value matches flag_value
      any(row_clean == flag_value)
    })

    # Convert logical to numeric (TRUE -> 1, FALSE -> 0)
    flag_column <- as.integer(any_flag)

    # Count rows with valid flag calculations (non-NA)
    valid_rows <- !is.na(flag_column)

    if (sum(valid_rows) == 0) {
      phr_warning(
        origin = "quality_test_any_flag_percentage",
        message = "No rows with non-missing values for flag calculation"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Calculate percentage of flagged rows (only among valid rows)
    flagged_count <- sum(flag_column[valid_rows] == 1)
    percentage <- (flagged_count / sum(valid_rows)) * 100

    return(list(
      statistic = percentage,
      p_value = NA_real_
    ))

  }, on_error = "warn", origin = "quality_test_any_flag_percentage")
}

#' Sex Ratio Test (Male:Female)
#'
#' Perform a chi-squared goodness-of-fit test comparing the observed male/female
#' counts to an expected male:female ratio (default 1:1).
#'
#' @param data Data frame containing the sex column
#' @param variables Character scalar. Name of the sex column in `data`
#' @param male_val Value in `variables` that indicates "male"
#' @param female_val Value in `variables` that indicates "female"
#' @param expected_ratio_val Single positive numeric giving expected male:female ratio.
#'   Default 1 (i.e., 1:1).
#' @return List with statistic (chi-squared value) and p-value
#' @export
quality_test_sexratio <- function(
    data,
    variables,
    male_val,
    female_val,
    expected_ratio_val = 1
) {

  phr_try({

    if (!is.character(variables) || length(variables) != 1L) {
      phr_error(
        origin = "quality_test_sexratio",
        message = "`variables` must be a single character column name"
      )
    }

    if (!variables %in% names(data)) {
      phr_warning(
        origin = "quality_test_sexratio",
        message = "Sex column not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    if (!is.numeric(expected_ratio_val) || length(expected_ratio_val) != 1L) {
      phr_error(
        origin = "quality_test_sexratio",
        message = "`expected_ratio_val` must be a single numeric value (male:female)"
      )
    }

    if (!is.finite(expected_ratio_val) || expected_ratio_val <= 0) {
      phr_error(
        origin = "quality_test_sexratio",
        message = "`expected_ratio_val` must be a finite positive number"
      )
    }

    if (identical(male_val, female_val)) {
      phr_error(
        origin = "quality_test_sexratio",
        message = "`male_val` and `female_val` must be different"
      )
    }

    s <- data[[variables]]
    s <- s[!is.na(s)]

    if (length(s) < 5) {
      phr_warning(
        origin = "quality_test_sexratio",
        message = "Insufficient data for sex ratio test"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Exclude values other than male_val/female_val, and warn if any are found
    is_mf <- (s == male_val) | (s == female_val)
    n_excluded <- sum(!is_mf)

    if (n_excluded > 0) {
      phr_warning(
        origin = "quality_test_sexratio",
        message = paste0(
          "Excluding ", n_excluded,
          " record(s) with sex values not equal to `male_val` or `female_val`"
        )
      )
    }

    s <- s[is_mf]

    if (length(s) < 5) {
      phr_warning(
        origin = "quality_test_sexratio",
        message = "Insufficient male/female data for sex ratio test"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    obs_male <- sum(s == male_val, na.rm = TRUE)
    obs_female <- sum(s == female_val, na.rm = TRUE)

    if (obs_male == 0L || obs_female == 0L) {
      phr_warning(
        origin = "quality_test_sexratio",
        message = "Only one sex category present; cannot test sex ratio"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    obs <- c(male = obs_male, female = obs_female)

    # expected_ratio_val is male:female
    p_male <- expected_ratio_val / (expected_ratio_val + 1)
    p_female <- 1 / (expected_ratio_val + 1)
    p <- c(p_male, p_female)

    test_result <- chisq.test(x = obs, p = p)

    return(list(
      statistic = as.numeric(test_result$statistic),
      p_value = test_result$p.value
    ))

  }, on_error = "warn", origin = "quality_test_sexratio")
}

#' Age Group Ratio Test (Two Indicator Columns)
#'
#' Perform a chi-squared goodness-of-fit test comparing the observed counts of "yes"
#' in two different age-group indicator columns to an expected ratio (default 1:1).
#'
#' @param data Data frame containing the indicator columns
#' @param variables Character vector of length 2. Names of the two age-group indicator columns in `data`
#' @param yes_val Value indicating "yes" in the indicator columns. Default 1.
#' @param no_val Value indicating "no" in the indicator columns. Default 0.
#' @param expected_ratio_val Single positive numeric giving expected ratio of
#'   variables[1]:variables[2]. Default 1 (i.e., 1:1).
#' @return List with statistic (chi-squared value) and p-value
#' @export
quality_test_ageratio <- function(
    data,
    variables,
    yes_val = 1,
    no_val = 0,
    expected_ratio_val = 0.85
) {

  phr_try({

    if (!is.character(variables) || length(variables) != 2L) {
      phr_error(
        origin = "quality_test_ageratio",
        message = "`variables` must be a character vector of length 2 (two column names)"
      )
    }

    age_group_col1 <- variables[[1]]
    age_group_col2 <- variables[[2]]

    if (!age_group_col1 %in% names(data) || !age_group_col2 %in% names(data)) {
      phr_warning(
        origin = "quality_test_ageratio",
        message = "One or both age group columns not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    if (identical(age_group_col1, age_group_col2)) {
      phr_error(
        origin = "quality_test_ageratio",
        message = "The two column names in `variables` must be different"
      )
    }

    if (identical(yes_val, no_val)) {
      phr_error(
        origin = "quality_test_ageratio",
        message = "`yes_val` and `no_val` must be different"
      )
    }

    if (!is.numeric(expected_ratio_val) || length(expected_ratio_val) != 1L) {
      phr_error(
        origin = "quality_test_ageratio",
        message = "`expected_ratio_val` must be a single numeric value (col1:col2)"
      )
    }

    if (!is.finite(expected_ratio_val) || expected_ratio_val <= 0) {
      phr_error(
        origin = "quality_test_ageratio",
        message = "`expected_ratio_val` must be a finite positive number"
      )
    }

    x <- data[[age_group_col1]]
    y <- data[[age_group_col2]]

    # Remove missing values
    complete_cases <- complete.cases(x, y)
    x <- x[complete_cases]
    y <- y[complete_cases]

    if (length(x) < 5) {
      phr_warning(
        origin = "quality_test_ageratio",
        message = "Insufficient data for age ratio test"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Exclude values not equal to yes_val/no_val in either column, and warn
    valid_x <- (x == yes_val) | (x == no_val)
    valid_y <- (y == yes_val) | (y == no_val)
    valid <- valid_x & valid_y

    n_excluded <- sum(!valid)
    if (n_excluded > 0) {
      phr_warning(
        origin = "quality_test_ageratio",
        message = paste0(
          "Excluding ", n_excluded,
          " record(s) with values not equal to `yes_val` or `no_val` in one or both age group columns"
        )
      )
    }

    x <- x[valid]
    y <- y[valid]

    if (length(x) < 5) {
      phr_warning(
        origin = "quality_test_ageratio",
        message = "Insufficient valid data for age ratio test"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # Observed "yes" counts in each indicator column
    yes1 <- sum(x == yes_val, na.rm = TRUE)
    yes2 <- sum(y == yes_val, na.rm = TRUE)

    if (yes1 == 0L || yes2 == 0L) {
      phr_warning(
        origin = "quality_test_ageratio",
        message = "One of the age groups has zero 'yes' counts; cannot test age ratio"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    obs <- c(col1_yes = yes1, col2_yes = yes2)

    # expected_ratio_val is col1:col2
    p1 <- expected_ratio_val / (expected_ratio_val + 1)
    p2 <- 1 / (expected_ratio_val + 1)
    p <- c(p1, p2)

    test_result <- chisq.test(x = obs, p = p)

    return(list(
      statistic = as.numeric(test_result$statistic),
      p_value = test_result$p.value
    ))

  }, on_error = "warn", origin = "quality_test_ageratio")
}

#' Digit Preference Score (MUAC)
#'
#' Compute digit preference score for a numeric MUAC (cm) variable using nipnTK.
#'
#' @param data Data frame containing the MUAC variable
#' @param variables Character scalar. Name of the MUAC (cm) column in `data`
#' @return List with statistic (digit preference score) and p_value (NA_real_)
#' @export
quality_test_digit_preference <- function(data, variables) {

  phr_try({

    if (!is.character(variables) || length(variables) != 1L) {
      phr_error(
        origin = "quality_test_digit_preference",
        message = "`variables` must be a single character column name"
      )
    }

    if (!variables %in% names(data)) {
      phr_warning(
        origin = "quality_test_digit_preference",
        message = "Variable not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    x <- data[[variables]]

    # Remove missing values
    x <- x[!is.na(x)]

    if (length(x) < 5) {
      phr_warning(
        origin = "quality_test_digit_preference",
        message = "Insufficient data for digit preference calculation"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    if (!is.numeric(x)) {
      phr_warning(
        origin = "quality_test_digit_preference",
        message = "Variable is not numeric; cannot compute digit preference score"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    # nipnTK::digitPreference() returns a list; first element is the score
    dp <- nipnTK::digitPreference(x)[[1]]

    return(list(
      statistic = as.numeric(dp),
      p_value = NA_real_
    ))

  }, on_error = "warn", origin = "quality_test_digit_preference")
}

#' ANOVA Test
#'
#' Perform a one-way analysis of variance (ANOVA) to compare group means
#' across two or more groups.
#'
#' @param data Data frame containing the variables
#' @param outcome_col Character scalar. Name of the numeric outcome column
#' @param group_col Character scalar. Name of the grouping column (must have
#'   at least 2 distinct levels)
#' @return List with statistic (F-value) and p_value
#' @export
quality_test_anova <- function(data, outcome_col, group_col) {

  phr_try({

    phr_validate_dataframe(
      data,
      origin = "quality_test_anova",
      hint = phr_txt("Ensure you pass a valid data frame or tibble."),
      soft = FALSE
    )

    if (!is.character(outcome_col) || length(outcome_col) != 1L) {
      phr_error(
        origin = "quality_test_anova",
        message = "`outcome_col` must be a single character column name"
      )
    }

    if (!is.character(group_col) || length(group_col) != 1L) {
      phr_error(
        origin = "quality_test_anova",
        message = "`group_col` must be a single character column name"
      )
    }

    if (!outcome_col %in% names(data)) {
      phr_warning(
        origin = "quality_test_anova",
        message = "Outcome column not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    if (!group_col %in% names(data)) {
      phr_warning(
        origin = "quality_test_anova",
        message = "Group column not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    is_valid <- phr_validate_numeric(
      data[[outcome_col]],
      origin = "quality_test_anova",
      hint = phr_txt("ANOVA requires a numeric outcome variable."),
      soft = TRUE
    )

    if (isFALSE(is_valid)) {
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    outcome <- as.numeric(data[[outcome_col]])
    group   <- data[[group_col]]

    complete_cases <- complete.cases(outcome, group)
    outcome <- outcome[complete_cases]
    group   <- group[complete_cases]

    if (length(outcome) < 3) {
      phr_warning(
        origin = "quality_test_anova",
        message = "Insufficient data for ANOVA (need at least 3 non-missing observations)"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    group <- as.factor(group)

    if (nlevels(group) < 2) {
      phr_warning(
        origin = "quality_test_anova",
        message = "Group column must have at least 2 distinct levels for ANOVA"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    model       <- aov(outcome ~ group, data = data.frame(outcome = outcome, group = group))
    anova_table <- summary(model)[[1]]

    return(list(
      statistic = as.numeric(anova_table[["F value"]][1]),
      p_value   = as.numeric(anova_table[["Pr(>F)"]][1])
    ))

  }, on_error = "warn", origin = "quality_test_anova")
}

#' Chi-Squared Test with Binary Contingency Table
#'
#' Perform a chi-squared test on a pre-aggregated contingency table where each
#' row in \code{data} represents one group and two columns supply the counts
#' for the two categories.
#'
#' @param data Data frame where each row represents a group, with two columns
#'   containing the count data for each category
#' @param group_col Character scalar. Name of the column containing group
#'   labels (used as row identifiers for the contingency table)
#' @param cat1_col Character scalar. Name of the column containing counts for
#'   the first category
#' @param cat2_col Character scalar. Name of the column containing counts for
#'   the second category
#' @return List with statistic (chi-squared value) and p_value
#' @export
quality_test_chisq_binary <- function(data, group_col, cat1_col, cat2_col) {

  phr_try({

    phr_validate_dataframe(
      data,
      origin = "quality_test_chisq_binary",
      hint = phr_txt("Ensure you pass a valid data frame or tibble."),
      soft = FALSE
    )

    if (!is.character(group_col) || length(group_col) != 1L) {
      phr_error(
        origin = "quality_test_chisq_binary",
        message = "`group_col` must be a single character column name"
      )
    }

    if (!is.character(cat1_col) || length(cat1_col) != 1L) {
      phr_error(
        origin = "quality_test_chisq_binary",
        message = "`cat1_col` must be a single character column name"
      )
    }

    if (!is.character(cat2_col) || length(cat2_col) != 1L) {
      phr_error(
        origin = "quality_test_chisq_binary",
        message = "`cat2_col` must be a single character column name"
      )
    }

    missing_cols <- setdiff(c(group_col, cat1_col, cat2_col), names(data))
    if (length(missing_cols) > 0) {
      phr_warning(
        origin = "quality_test_chisq_binary",
        message = paste0("Columns not found in data: ", paste(missing_cols, collapse = ", "))
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    complete_cases <- complete.cases(data[[group_col]], data[[cat1_col]], data[[cat2_col]])
    group_vals <- as.character(data[[group_col]][complete_cases])
    cat1_vals  <- as.numeric(data[[cat1_col]][complete_cases])
    cat2_vals  <- as.numeric(data[[cat2_col]][complete_cases])

    if (length(group_vals) < 2) {
      phr_warning(
        origin = "quality_test_chisq_binary",
        message = "Insufficient rows for chi-squared binary test (need at least 2)"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    contingency_matrix <- matrix(
      c(cat1_vals, cat2_vals),
      nrow = length(group_vals),
      ncol = 2,
      dimnames = list(group_vals, c(cat1_col, cat2_col))
    )

    test_result <- chisq.test(contingency_matrix)

    return(list(
      statistic = as.numeric(test_result$statistic),
      p_value   = test_result$p.value
    ))

  }, on_error = "warn", origin = "quality_test_chisq_binary")
}

#' Binomial Ratio Test (Row-wise)
#'
#' Perform a row-wise exact binomial proportion test comparing the observed
#' success rate against an expected ratio, appending a p-value column to the
#' data frame.
#'
#' @param data Data frame containing the variables
#' @param success_col Character scalar. Name of the column containing the
#'   number of successes (non-negative integer)
#' @param total_col Character scalar. Name of the column containing the total
#'   number of trials (positive integer \eqn{\geq} success count)
#' @param expected_ratio Numeric scalar. Expected proportion under the null
#'   hypothesis (default: 0.5; must be strictly between 0 and 1)
#' @param alternative Character scalar. Alternative hypothesis: \code{"two.sided"},
#'   \code{"greater"}, or \code{"less"} (default: \code{"two.sided"})
#' @param pval_colname Character scalar. Name for the new p-value column
#'   (default: \code{"p_value"})
#' @return Data frame with an additional column containing the row-wise
#'   binomial test p-values (rounded to 5 decimal places); rows with missing
#'   or invalid values receive \code{NA}
#' @export
quality_test_binomial_ratio_rowwise <- function(data,
                                                success_col,
                                                total_col,
                                                expected_ratio = 0.5,
                                                alternative    = "two.sided",
                                                pval_colname   = "p_value") {

  phr_try({

    phr_validate_dataframe(
      data,
      origin = "quality_test_binomial_ratio_rowwise",
      hint = phr_txt("Ensure you pass a valid data frame or tibble."),
      soft = FALSE
    )

    if (!is.character(success_col) || length(success_col) != 1L) {
      phr_error(
        origin = "quality_test_binomial_ratio_rowwise",
        message = "`success_col` must be a single character column name"
      )
    }

    if (!is.character(total_col) || length(total_col) != 1L) {
      phr_error(
        origin = "quality_test_binomial_ratio_rowwise",
        message = "`total_col` must be a single character column name"
      )
    }

    if (!is.numeric(expected_ratio) || length(expected_ratio) != 1L ||
        is.na(expected_ratio) || expected_ratio <= 0 || expected_ratio >= 1) {
      phr_error(
        origin = "quality_test_binomial_ratio_rowwise",
        message = "`expected_ratio` must be a single numeric value strictly between 0 and 1"
      )
    }

    if (!alternative %in% c("two.sided", "greater", "less")) {
      phr_error(
        origin = "quality_test_binomial_ratio_rowwise",
        message = "`alternative` must be one of 'two.sided', 'greater', or 'less'"
      )
    }

    if (!is.character(pval_colname) || length(pval_colname) != 1L) {
      phr_error(
        origin = "quality_test_binomial_ratio_rowwise",
        message = "`pval_colname` must be a single character string"
      )
    }

    missing_cols <- setdiff(c(success_col, total_col), names(data))
    if (length(missing_cols) > 0) {
      phr_warning(
        origin = "quality_test_binomial_ratio_rowwise",
        message = paste0("Columns not found in data: ", paste(missing_cols, collapse = ", "))
      )
      return(data)
    }

    success_vals <- data[[success_col]]
    total_vals   <- data[[total_col]]

    p_values <- vapply(seq_len(nrow(data)), function(i) {
      s <- success_vals[i]
      n <- total_vals[i]

      if (is.na(s) || is.na(n) || n <= 0 || n < s ||
          n != floor(n) || s != floor(s)) {
        return(NA_real_)
      }

      tryCatch(
        round(
          binom.test(
            x           = as.integer(s),
            n           = as.integer(n),
            p           = expected_ratio,
            alternative = alternative
          )$p.value,
          digits = 5
        ),
        error = function(e) NA_real_
      )
    }, numeric(1))

    data[[pval_colname]] <- p_values
    return(data)

  }, on_error = "warn", origin = "quality_test_binomial_ratio_rowwise")
}

#' One-Sample T-Test from Summary Statistics (Row-wise)
#'
#' Perform a row-wise one-sample t-test computed analytically from summary
#' statistics (mean, standard deviation, and sample size), appending a
#' p-value column to the data frame.
#'
#' @param data Data frame containing the variables
#' @param mean_col Character scalar. Name of the column containing sample means
#' @param sd_col Character scalar. Name of the column containing sample
#'   standard deviations (must be positive)
#' @param n_col Character scalar. Name of the column containing sample sizes
#'   (must be \eqn{\geq 2})
#' @param expected_mean Numeric scalar. Hypothesised population mean under the
#'   null hypothesis (default: 0)
#' @param alternative Character scalar. Alternative hypothesis: \code{"two.sided"},
#'   \code{"greater"}, or \code{"less"} (default: \code{"two.sided"})
#' @param pval_colname Character scalar. Name for the new p-value column
#'   (default: \code{"p_value"})
#' @return Data frame with an additional column containing the row-wise
#'   t-test p-values (rounded to 3 decimal places); rows with missing or
#'   invalid values receive \code{NA}
#' @export
quality_test_ttest_summary_rowwise <- function(data,
                                               mean_col,
                                               sd_col,
                                               n_col,
                                               expected_mean = 0,
                                               alternative   = "two.sided",
                                               pval_colname  = "p_value") {

  phr_try({

    phr_validate_dataframe(
      data,
      origin = "quality_test_ttest_summary_rowwise",
      hint = phr_txt("Ensure you pass a valid data frame or tibble."),
      soft = FALSE
    )

    if (!is.character(mean_col) || length(mean_col) != 1L) {
      phr_error(
        origin = "quality_test_ttest_summary_rowwise",
        message = "`mean_col` must be a single character column name"
      )
    }

    if (!is.character(sd_col) || length(sd_col) != 1L) {
      phr_error(
        origin = "quality_test_ttest_summary_rowwise",
        message = "`sd_col` must be a single character column name"
      )
    }

    if (!is.character(n_col) || length(n_col) != 1L) {
      phr_error(
        origin = "quality_test_ttest_summary_rowwise",
        message = "`n_col` must be a single character column name"
      )
    }

    if (!is.numeric(expected_mean) || length(expected_mean) != 1L || is.na(expected_mean)) {
      phr_error(
        origin = "quality_test_ttest_summary_rowwise",
        message = "`expected_mean` must be a single non-missing numeric value"
      )
    }

    if (!alternative %in% c("two.sided", "greater", "less")) {
      phr_error(
        origin = "quality_test_ttest_summary_rowwise",
        message = "`alternative` must be one of 'two.sided', 'greater', or 'less'"
      )
    }

    if (!is.character(pval_colname) || length(pval_colname) != 1L) {
      phr_error(
        origin = "quality_test_ttest_summary_rowwise",
        message = "`pval_colname` must be a single character string"
      )
    }

    missing_cols <- setdiff(c(mean_col, sd_col, n_col), names(data))
    if (length(missing_cols) > 0) {
      phr_warning(
        origin = "quality_test_ttest_summary_rowwise",
        message = paste0("Columns not found in data: ", paste(missing_cols, collapse = ", "))
      )
      return(data)
    }

    mean_vals <- as.numeric(data[[mean_col]])
    sd_vals   <- as.numeric(data[[sd_col]])
    n_vals    <- as.numeric(data[[n_col]])

    p_values <- vapply(seq_len(nrow(data)), function(i) {
      m <- mean_vals[i]
      s <- sd_vals[i]
      n <- n_vals[i]

      if (is.na(m) || is.na(s) || is.na(n) || s <= 0 || n < 2) {
        return(NA_real_)
      }

      tryCatch({
        t_stat <- (m - expected_mean) / (s / sqrt(n))
        df_val <- n - 1
        p <- switch(
          alternative,
          "two.sided" = 2 * pt(-abs(t_stat), df_val),
          "greater"   = pt(t_stat, df_val, lower.tail = FALSE),
          "less"      = pt(t_stat, df_val, lower.tail = TRUE),
          NA_real_
        )
        round(p, digits = 3)
      }, error = function(e) NA_real_)
    }, numeric(1))

    data[[pval_colname]] <- p_values
    return(data)

  }, on_error = "warn", origin = "quality_test_ttest_summary_rowwise")
}

#' Poisson Rate Test (Row-wise)
#'
#' Perform a row-wise exact Poisson test comparing an observed event count
#' against an expected rate, appending a p-value column to the data frame.
#'
#' @param data Data frame containing the variables
#' @param event_col Character scalar. Name of the column containing the number
#'   of events (non-negative integer)
#' @param exposure_col Character scalar. Name of the column containing the
#'   exposure units (positive numeric, e.g., person-time or number of
#'   households)
#' @param expected_rate Numeric scalar. Expected event rate per unit exposure
#'   under the null hypothesis (default: 0.02; must be positive)
#' @param alternative Character scalar. Alternative hypothesis: \code{"two.sided"},
#'   \code{"greater"}, or \code{"less"} (default: \code{"two.sided"})
#' @param pval_colname Character scalar. Name for the new p-value column
#'   (default: \code{"p_value"})
#' @return Data frame with an additional column containing the row-wise
#'   Poisson test p-values (rounded to 5 decimal places); rows with missing
#'   or invalid values receive \code{NA}
#' @export
quality_test_poisson_ratio_rowwise <- function(data,
                                               event_col,
                                               exposure_col,
                                               expected_rate = 0.02,
                                               alternative   = "two.sided",
                                               pval_colname  = "p_value") {

  phr_try({

    phr_validate_dataframe(
      data,
      origin = "quality_test_poisson_ratio_rowwise",
      hint = phr_txt("Ensure you pass a valid data frame or tibble."),
      soft = FALSE
    )

    if (!is.character(event_col) || length(event_col) != 1L) {
      phr_error(
        origin = "quality_test_poisson_ratio_rowwise",
        message = "`event_col` must be a single character column name"
      )
    }

    if (!is.character(exposure_col) || length(exposure_col) != 1L) {
      phr_error(
        origin = "quality_test_poisson_ratio_rowwise",
        message = "`exposure_col` must be a single character column name"
      )
    }

    if (!is.numeric(expected_rate) || length(expected_rate) != 1L ||
        is.na(expected_rate) || expected_rate <= 0) {
      phr_error(
        origin = "quality_test_poisson_ratio_rowwise",
        message = "`expected_rate` must be a single positive numeric value"
      )
    }

    if (!alternative %in% c("two.sided", "greater", "less")) {
      phr_error(
        origin = "quality_test_poisson_ratio_rowwise",
        message = "`alternative` must be one of 'two.sided', 'greater', or 'less'"
      )
    }

    if (!is.character(pval_colname) || length(pval_colname) != 1L) {
      phr_error(
        origin = "quality_test_poisson_ratio_rowwise",
        message = "`pval_colname` must be a single character string"
      )
    }

    missing_cols <- setdiff(c(event_col, exposure_col), names(data))
    if (length(missing_cols) > 0) {
      phr_warning(
        origin = "quality_test_poisson_ratio_rowwise",
        message = paste0("Columns not found in data: ", paste(missing_cols, collapse = ", "))
      )
      return(data)
    }

    event_vals    <- data[[event_col]]
    exposure_vals <- data[[exposure_col]]

    p_values <- vapply(seq_len(nrow(data)), function(i) {
      ev  <- event_vals[i]
      exp <- exposure_vals[i]

      if (is.na(ev) || is.na(exp) || exp <= 0 || ev < 0) {
        return(NA_real_)
      }

      tryCatch(
        round(
          poisson.test(
            x           = as.integer(ev),
            T           = exp,
            r           = expected_rate,
            alternative = alternative
          )$p.value,
          digits = 5
        ),
        error = function(e) NA_real_
      )
    }, numeric(1))

    data[[pval_colname]] <- p_values
    return(data)

  }, on_error = "warn", origin = "quality_test_poisson_ratio_rowwise")
}

#' Non-missing Count Test
#'
#' Return the number of non-missing (non-NA) values in a specified column.
#'
#' @param data Data frame
#' @param variables Character scalar. Name of the column to count non-missing values for
#' @return List with statistic (non-missing count) and p_value (NA_real_)
#' @export
quality_test_count <- function(data, variables) {

  phr_try({

    if (!is.character(variables) || length(variables) != 1L) {
      phr_error(
        origin = "quality_test_count",
        message = "`variables` must be a single character column name"
      )
    }

    if (!variables %in% names(data)) {
      phr_warning(
        origin = "quality_test_count",
        message = "Column not found in data"
      )
      return(list(statistic = NA_real_, p_value = NA_real_))
    }

    x <- data[[variables]]
    n_non_missing <- sum(!is.na(x))

    return(list(
      statistic = as.numeric(n_non_missing),
      p_value = NA_real_
    ))

  }, on_error = "warn", origin = "quality_test_count")
}
