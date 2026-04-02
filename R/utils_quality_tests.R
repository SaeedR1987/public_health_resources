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
