

#' Validate if input is a dataframe and optionally check for required columns
#'
#' Checks whether the input is a dataframe, and if specified,
#' whether it contains a given set of required columns.
#'
#' @param df The object to check.
#' @param required_cols Optional character vector of column names that must be present.
#'
#' @return TRUE if `df` is a dataframe and contains all required columns (if specified), FALSE otherwise.
#'
#' @examples
#' validate_dataframe(mtcars)                      # TRUE
#' validate_dataframe(mtcars, c("mpg", "cyl"))     # TRUE
#' validate_dataframe(mtcars, c("mpg", "notacol")) # FALSE
#'
#' @export
validate_dataframe <- function(df, required_cols = NULL) {
  if (!is.data.frame(df)) {
    stop("The provided object is not a data.frame (or tibble).")
  }
  
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, colnames(df))
    if (length(missing_cols) > 0) {
      stop(
        sprintf(
          "The following required columns are missing from the dataframe: %s",
          paste(missing_cols, collapse = ", ")
        )
      )
    }
  }
  
  return(TRUE)
}

#' Validate Type of Input (Scalar or Vector)
#'
#' Validates if an input is of the expected type, or can be safely coerced to that type.
#'
#' @param x A value, vector, or dataframe column.
#' @param expected_type The expected R type: "numeric", "logical", "character", or "Date".
#' @return TRUE if input is of or can be coerced to the expected type. Throws an error otherwise.
#' @examples
#' validate_type(c("1", "2"), "numeric") # TRUE
#' validate_type(c("TRUE", "FALSE"), "logical") # TRUE
#' validate_type("2023-01-01", "Date") # TRUE
#' validate_type(c("01/01/2020", "2020-12-31"), "Date") # TRUE
validate_type <- function(x, expected_type) {
  acceptable_date_formats <- c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y", "%Y/%m/%d")
  
  is_all_numeric <- function(vec) {
    suppressWarnings(!any(is.na(as.numeric(vec))))
  }
  
  is_all_logical <- function(vec) {
    all(tolower(vec) %in% c("true", "false"))
  }
  
  is_all_dates <- function(vec) {
    for (fmt in acceptable_date_formats) {
      parsed <- suppressWarnings(as.Date(vec, format = fmt))
      if (!any(is.na(parsed))) {
        if (any(grepl(":", vec))) {
          warning("Some dates include time. Only day/month/year will be used.")
        }
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  actual_type <- class(x)[1]
  
  # If input is already the expected type
  if (actual_type == expected_type) {
    return(TRUE)
  }
  
  # If input is character and we expect numeric
  if (expected_type == "numeric" && actual_type == "character") {
    if (is_all_numeric(x)) return(TRUE)
    else stop("Character input could not be safely converted to numeric.")
  }
  
  # If input is character and we expect logical
  if (expected_type == "logical" && actual_type == "character") {
    if (is_all_logical(x)) return(TRUE)
    else stop("Character input could not be safely converted to logical (TRUE/FALSE).")
  }
  
  # If input is character and we expect Date
  if (expected_type == "Date" && actual_type == "character") {
    if (is_all_dates(x)) return(TRUE)
    else stop(paste0(
      "Character input could not be safely converted to Date.\n",
      "Found format: ", unique(x[is.na(as.Date(x, tryFormats = acceptable_date_formats))])[1], "\n",
      "Acceptable formats include: ", paste(acceptable_date_formats, collapse = ", ")
    ))
  }
  
  stop(paste0("Input is of type ", actual_type, ", which does not match expected type: ", expected_type))
}

#' Check if values are valid against an allowed list, with error on invalid values
#'
#' Checks whether a single character value or all values in a dataframe column
#' belong to a specified vector of allowed character values.
#' Stops with an error message listing invalid values if any are found.
#' Uses `validate_type()` to confirm input type is character or coercible.
#'
#' @param x A single character value or a character vector (e.g., a dataframe column).
#' @param allowed_values A character vector of allowed values.
#'
#' @return TRUE if all values are valid (in allowed_values), otherwise stops with error.
#'
#' @examples
#' validate_char_choices("apple", c("apple", "banana", "cherry"))       # TRUE
#' validate_char_choices(c("apple", "banana"), c("apple", "banana"))    # TRUE
#' # validate_char_choices(c("apple", "grape"), c("apple", "banana"))   # ERROR with invalid values listed
#'
#' @export
validate_char_choices <- function(x, allowed_values) {
  # Use validate_type() to check if input is character or safely coercible
  if (!validate_type(x, expected = "character")) {
    stop("Input `x` is not character or safely coercible to character.")
  }
  if (!is.character(allowed_values)) {
    stop("`allowed_values` must be a character vector.")
  }
  
  invalid_values <- unique(x[!(x %in% allowed_values) | is.na(x)])
  if (length(invalid_values) > 0) {
    stop(
      "Invalid values detected: ", 
      paste0("'", invalid_values, "'", collapse = ", "),
      ". Allowed values are: ", 
      paste0("'", allowed_values, "'", collapse = ", "), "."
    )
  }
  
  return(TRUE)
}

#' Convert character or POSIX dates to Date, strictly
#'
#' Converts input to Date, strips any time components, and errors if any value cannot be parsed.
#'
#' @param x Character, Date, or POSIX vector.
#'
#' @return Date vector
#' @examples
#' convert_date(c("2025-07-13", "2025-08-01"))
#' convert_date(as.POSIXct("2025-07-13 12:00:00"))
#'
#' @export
convert_date <- function(x) {
  # If already Date or POSIX*
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  
  # If numeric
  if (is.numeric(x)) {
    origin <- if (all(x > 20000 & x < 60000, na.rm = TRUE)) "1899-12-30" else "1970-01-01"
    return(as.Date(x, origin = origin))
  }
  
  # If character but all values are numeric-like
  if (is.character(x) && all(grepl("^[0-9]+$", x[!is.na(x)]))) {
    x_num <- as.numeric(x)
    origin <- if (all(x_num > 20000 & x_num < 60000, na.rm = TRUE)) "1899-12-30" else "1970-01-01"
    return(as.Date(x_num, origin = origin))
  }
  
  # Convert to character for parsing
  x_vec <- as.character(x)
  is_na <- is.na(x_vec)
  x_to_parse <- x_vec[!is_na]
  
  x_clean <- trimws(x_to_parse)
  
  # Remove timezone suffixes (UTC, +03:00, etc.)
  x_clean <- gsub("\\s*(UTC|GMT|CET|CEST|EST|PST|EDT|PDT|\\+\\d{2}:\\d{2})$", "", x_clean, ignore.case = TRUE)
  
  # Strip ISO T timestamps
  x_clean <- sub("T.*$", "", x_clean)
  
  if (any(grepl("\\d{2}:\\d{2}:\\d{2}", x_to_parse))) {
    warning("Time components detected and will be removed during date conversion.")
  }
  
  # Parse using lubridate
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x_clean,
    orders = c("ymd", "dmy", "mdy", "Ymd HMS", "dmY HMS"),
    exact = FALSE
  ))
  
  parsed <- as.Date(parsed)
  
  # Check for unparsed values
  if (any(is.na(parsed))) {
    invalid_vals <- unique(x_to_parse[is.na(parsed)])
    stop(
      "Could not convert the following values to Date: ", 
      paste0("'", invalid_vals, "'", collapse = ", "), 
      ".\nPlease ensure they match one of the accepted formats: ymd, dmy, mdy, ymd HMS, dmY HMS."
    )
  }
  
  # Re-insert original NAs
  result <- rep(NA, length(x_vec))
  result[!is_na] <- parsed
  result[is_na] <- NA
  
  # Ensure class Date
  class(result) <- "Date"
  
  return(result)
}

#' Validate Numeric Range of Values with Type Checking
#'
#' Checks if input is numeric (or character safely convertible to numeric)
#' and all values lie within the specified inclusive range.
#'
#' @param x A single value, vector, or dataframe column.
#' @param min_val Numeric scalar specifying minimum allowed value (inclusive).
#' @param max_val Numeric scalar specifying maximum allowed value (inclusive).
#'
#' @return Logical TRUE if all values are numeric (or safely coercible) and within range; FALSE otherwise.
#' @examples
#' validate_numeric_range("5", 1, 10)                # TRUE
#' validate_numeric_range(c("2", "5", "8"), 1, 10)   # TRUE
#' validate_numeric_range(c("0", "5", "8"), 1, 10)   # FALSE
#' validate_numeric_range(c(2, NA, 8), 1, 10)        # FALSE
#'
#' @export
validate_numeric_range <- function(x, min_val, max_val) {
  # Assume validate_type is already defined and loaded
  if (!validate_type(x, expected = "numeric")) {
    return(FALSE)
  }
  
  # If character, safely convert to numeric
  if (is.character(x)) {
    x <- suppressWarnings(as.numeric(x))
  }
  
  if (any(is.na(x))) {
    return(FALSE)
  }
  
  all(x >= min_val & x <= max_val)
}

#' Check Required Fields Are Not Empty
#'
#' Validates that specified named fields are not empty.  
#' Considers a field empty if it is `NULL`, empty vector, empty string(s),  
#' empty data frame, empty list, or a single `NA` value.  
#' Stops with an informative error listing which fields are empty or missing.  
#' Allows an optional custom error message to prepend to the error text.
#'
#' @param ... Named arguments representing fields to check (e.g., `name = .self$name`).
#'   Each argument must be named for meaningful error messages.
#' @param message Optional character string to prepend to the error message.
#'
#' @return Invisibly returns `TRUE` if all fields are valid (non-empty).  
#'   Throws an error if any field is empty or missing.
#'
#' @examples
#' \dontrun{
#' check_required_fields(
#'   username = "alice",
#'   data = data.frame(x = 1),
#'   message = "Form validation failed."
#' )
#'
#' check_required_fields(
#'   username = "",
#'   data = data.frame(),
#'   message = "Form validation failed."
#' )
#' # Error: Form validation failed.
#' # The following required fields are missing or empty: username, data
#' }
#'
#' @export
check_required_fields <- function(self, ..., message = NULL) {
  # Collect field names to check
  field_names <- c(...)
  
  if (length(field_names) == 0) {
    stop("You must provide at least one field name to check.")
  }
  
  # Verify the fields exist in .self
  missing_in_class <- setdiff(field_names, names(self$getRefClass()$fields()))
  if (length(missing_in_class) > 0) {
    stop("The following fields do not exist in the class: ", 
         paste(missing_in_class, collapse = ", "))
  }
  
  # Check if fields are NULL, NA, or empty
  is_empty <- sapply(field_names, function(f) {
    val <- self[[f]]
    is.null(val) ||
      (is.atomic(val) && length(val) == 0) ||
      (is.character(val) && all(val == "")) ||
      (is.list(val) && length(val) == 0) ||
      (length(val) == 1 && is.na(val))
  })
  
  if (any(is_empty)) {
    missing_fields <- field_names[is_empty]
    msg <- paste(
      "The following required fields are missing or empty:",
      paste(missing_fields, collapse = ", ")
    )
    if (!is.null(message)) {
      msg <- paste0(message, "\n", msg)
    }
    stop(msg, call. = FALSE)
  }
  
  invisible(TRUE)
}


ensure_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Package '%s' not found. Installing now...", pkg))
    install.packages(pkg)
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Failed to install package '%s'. Please install it manually.", pkg))
    }
  }
  # Load package quietly
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}


validate_list <- function(x, message = "Input must be a non-empty character vector.") {
  if (is.null(x)) {
    stop(message)
  }
  
  if (!is.vector(x) || !is.character(x)) {
    stop(message)
  }
  
  if (length(x) == 0) {
    stop(message)
  }
  
  return(TRUE)
}

#' Check if an object is filled (not NULL, not empty, not just "")
#'
#' @param x An object to check (usually a character string or vector).
#'
#' @return TRUE if the object is non-null, non-empty, and not all blank strings.
#' @examples
#' is_filled("abc")          # TRUE
#' is_filled(c("a", "b"))    # TRUE
#' is_filled(NULL)           # FALSE
#' is_filled(character(0))   # FALSE
#' is_filled("")             # FALSE
#' is_filled(c("a", ""))     # TRUE (since at least one is non-empty)
is_filled <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  
  # Handle survey design objects (S3/S4 from survey or srvyr)
  if (inherits(x, "survey.design") || inherits(x, "svyrep.design") || inherits(x, "tbl_svy")) {
    return(nrow(x$variables) > 0)  # check that the underlying data has rows
  }
  
  # Handle data frames and tibbles
  if (is.data.frame(x)) {
    return(nrow(x) > 0 && ncol(x) > 0)
  }
  
  # Handle vectors, lists, characters, etc.
  if (is.atomic(x) || is.list(x)) {
    return(length(x) > 0 && any(nzchar(as.character(x))))
  }
  
  # Fallback: just check length
  return(length(x) > 0)
}

#' Calculate difference in days between two dates
#'
#' Computes the number of whole days between two Date objects (end - start). Returns integer days.
#'
#' @param start_date Date or vector of Dates representing the start of the period.
#' @param end_date Date or vector of Dates representing the end of the period.
#'
#' @return Integer vector of days (end_date - start_date). Returns NA if either input is NA.
#'
#' @examples
#' calculate_days_diff(as.Date("2025-01-01"), as.Date("2025-01-10"))
#' calculate_days_diff(as.Date(c("2025-01-01", "2025-01-05")), as.Date(c("2025-01-03", "2025-01-10")))
#'
#' @export
calculate_days_diff <- function(start_date, end_date) {
  if (!inherits(start_date, "Date") || !inherits(end_date, "Date")) {
    stop("Both start_date and end_date must be Date objects.")
  }
  
  # Calculate difference in days
  diff_days <- as.integer(end_date - start_date)
  
  # Return NA if either input is NA
  diff_days[is.na(start_date) | is.na(end_date)] <- NA
  
  return(diff_days + 1)
}

replace_values <- function(df, col, mapping) {
  col <- enquo(col)  # capture column name
  df %>%
    mutate(!!col := recode(!!col, !!!mapping))
}

phr_pick_ci_method <- function(n_unweighted = NULL,
                                 n_eff = NULL,
                                 p_estimate = NULL,
                                 deff = NULL,
                                 lonely_psu = FALSE,
                                 high_design_complexity = FALSE,
                                 is_numeric = FALSE) {
  #------------------------------------------------------------
  # Purpose:
  # Decide whether to use Wald (default), logit-transformed, or
  # mean-based CIs for survey estimates.
  #
  # Returns a list with:
  #   $method : "design-wald", "design-logit", or "mean-wald"
  #   $note   : text summary of decision
  #   $flags  : named logical vector for diagnostics
  #------------------------------------------------------------
  
  # --- Initialize --------------------------------------------
  flags <- c(
    flag_small_n    = FALSE,
    flag_low_neff   = FALSE,
    flag_rare_p     = FALSE,
    flag_high_deff  = FALSE,
    flag_lonely_psu = FALSE
  )
  notes <- c()
  
  # --- Handle missing inputs gracefully ----------------------
  if (is.null(n_unweighted)) n_unweighted <- NA_real_
  if (is.null(n_eff)) n_eff <- NA_real_
  if (is.null(p_estimate)) p_estimate <- NA_real_
  if (is.null(deff)) deff <- NA_real_
  
  # --- Lonely PSU handling -----------------------------------
  if (isTRUE(lonely_psu)) {
    options(survey.lonely.psu = "adjust")
    flags["flag_lonely_psu"] <- TRUE
    notes <- c(notes, "lonely PSU adjusted (variance mode = 'adjust')")
  }
  
  # --- Trigger checks ----------------------------------------
  if (is.finite(n_unweighted) && n_unweighted < 30) {
    flags["flag_small_n"] <- TRUE
    notes <- c(notes, "small sample size (n < 30)")
  }
  
  if (is.finite(n_eff) && n_eff < 10) {
    flags["flag_low_neff"] <- TRUE
    notes <- c(notes, "low effective sample size (n_eff < 10)")
  }
  
  if (!is_numeric && is.finite(p_estimate) && (p_estimate < 0.05 || p_estimate > 0.95)) {
    # Rare proportions only apply for binary variables
    flags["flag_rare_p"] <- TRUE
    notes <- c(notes, "rare/extreme proportion (p < 0.05 or > 0.95)")
  }
  
  if (isTRUE(high_design_complexity) || (is.finite(deff) && deff > 5)) {
    flags["flag_high_deff"] <- TRUE
    notes <- c(notes, "high design effect or complex design (DEFF > 5)")
  }
  
  # --- Decision logic ----------------------------------------
  if (is_numeric) {
    # Numeric means: logit not meaningful
    method <- "mean-wald"
    notes <- c(notes, "numeric variable: Wald CI for mean selected")
  } else if (any(flags[c("flag_small_n", "flag_low_neff", "flag_rare_p", "flag_high_deff")])) {
    method <- "design-logit"
    notes <- c(notes, "logit-transformed CI selected")
  } else {
    method <- "design-wald"
    notes <- c(notes, "Wald CI (default) selected")
  }
  
  # --- Output ------------------------------------------------
  note_text <- paste(unique(notes), collapse = "; ")
  
  return(list(
    method = method,
    note   = note_text,
    flags  = as.list(flags)
  ))
}

phr_calc_survey_prop_single <- function(design,
                                          var_name,
                                          group = NULL,
                                          indicator_name = "Proportion",
                                          indicator_unit = "%",
                                          multiplier = 100,
                                          group_name_label = NULL,
                                          high_design_complexity = FALSE) {
  #------------------------------------------------------------
  # Purpose:
  # Safely calculate a single survey-weighted proportion with
  # adaptive CI logic (Wald vs logit), diagnostic flags, and
  # denominators (weighted/unweighted).
  #------------------------------------------------------------
  
  if (!var_name %in% names(design$variables)) {
    return(tibble::tibble(
      variable           = var_name,
      indicator_name     = indicator_name,
      indicator_unit     = indicator_unit,
      point.estimate     = NA_real_,
      lower_ci           = NA_real_,
      upper_ci           = NA_real_,
      ci_method          = policy$method,
      
      # Updated counts to match new structure
      n_event_unweighted = n_event_unweighted,
      n_event_weighted   = n_event_weighted,
      n_unweighted       = n_unweighted,
      n_weighted         = n_weighted,
      
      n_eff              = n_eff,
      deff               = NA_real_,
      
      flag_small_n       = policy$flags$flag_small_n,
      flag_low_neff      = policy$flags$flag_low_neff,
      flag_rare_p        = policy$flags$flag_rare_p,
      flag_high_deff     = policy$flags$flag_high_deff,
      flag_lonely_psu    = policy$flags$flag_lonely_psu,
      
      note               = paste("estimation failed;", policy$note)
    ))
  }
  
  var_sym <- rlang::sym(var_name)
  data <- design$variables
  
  # Convert srvyr design to native survey design if needed
  if (inherits(design, "tbl_svy")) {
    design <- srvyr::as_survey(design)
  }
  
  # --- Weighted and unweighted totals using srvyr ------------------
  # Convert to srvyr design if needed
  design_srvyr <- if (inherits(design, "tbl_svy")) design else srvyr::as_survey(design)
  
  # Compute weighted and unweighted denominators and numerators
  dsn_sum <- design_srvyr %>%
    srvyr::summarise(
      n_event_weighted   = srvyr::survey_total(!!var_sym, vartype = NULL, na.rm = TRUE),
      n_event_unweighted = sum(!!var_sym, na.rm = TRUE),
      n_weighted         = srvyr::survey_total(!is.na(!!var_sym), vartype = NULL, na.rm = TRUE),
      n_unweighted       = sum(!is.na(!!var_sym))
    )
  
  # Extract as numeric
  n_event_weighted   <- as.numeric(dsn_sum$n_event_weighted)
  n_event_unweighted <- as.numeric(dsn_sum$n_event_unweighted)
  n_weighted         <- as.numeric(dsn_sum$n_weighted)
  n_unweighted       <- as.numeric(dsn_sum$n_unweighted)
  
  # Calculate effective sample size
  w <- tryCatch(survey::weights(design), error = function(e) rep(1, n_unweighted))
  n_eff <- if (sum(w^2, na.rm = TRUE) > 0) (sum(w, na.rm = TRUE)^2) / sum(w^2, na.rm = TRUE) else NA_real_
  
  # Quick mean estimate for CI policy decisions
  quick_est <- mean(data[[var_name]], na.rm = TRUE)
  
  # --- Lonely PSU detection (robust) --------------------------
  lonely_psu <- tryCatch({
    strata_vec <- if (is.data.frame(design$strata)) design$strata[[1]] else design$strata
    ids_vec <- if (is.data.frame(design$ids)) design$ids[[1]] else design$ids
    any(tapply(ids_vec, strata_vec, function(x) length(unique(x)) == 1))
  }, error = function(e) FALSE)
  
  # --- Apply policy ------------------------------------------
  policy <- phr_pick_ci_method(
    n_unweighted = n_unweighted,
    n_eff = n_eff,
    p_estimate = quick_est,
    deff = NA,
    lonely_psu = lonely_psu,
    high_design_complexity = high_design_complexity
  )
  
  print(paste0("I am here 1"))
  
  # --- Compute estimate (final stable version) ----------------
  est <- tryCatch({
    fmla <- as.formula(paste0("~I(", var_name, " == 1)"))
    
    suppressWarnings({
      if (policy$method == "design-logit") {
        survey::svyciprop(fmla, design, method = "logit", level = 0.95, na.rm = TRUE)
      } else {
        print(paste0("I am here 2"))
        survey::svyciprop(fmla, design, method = "mean", level = 0.95, na.rm = TRUE)
        
      }
    })
  },
  error = function(e) {
    message("❌ Survey error: ", conditionMessage(e))
    NULL
  })
  
  print(paste0("I am here 4"))
  print(est)
  
  # --- Handle failed estimation -------------------------------
  if (is.null(est)) {
    return(tibble::tibble(
      variable           = var_name,
      indicator_name     = indicator_name,
      indicator_unit     = indicator_unit,
      point.estimate     = NA_real_,
      lower_ci           = NA_real_,
      upper_ci           = NA_real_,
      ci_method          = policy$method,
      
      # Updated counts to match new structure
      n_event_unweighted = n_event_unweighted,
      n_event_weighted   = n_event_weighted,
      n_unweighted       = n_unweighted,
      n_weighted         = n_weighted,
      
      n_eff              = n_eff,
      deff               = NA_real_,
      
      flag_small_n       = policy$flags$flag_small_n,
      flag_low_neff      = policy$flags$flag_low_neff,
      flag_rare_p        = policy$flags$flag_rare_p,
      flag_high_deff     = policy$flags$flag_high_deff,
      flag_lonely_psu    = policy$flags$flag_lonely_psu,
      
      note               = paste("estimation failed;", policy$note)
    ))
  }
  
  print(paste0("I am here 5"))
  
  # --- Extract results ----------------------------------------
  p_est <- as.numeric(coef(est))
  ci <- tryCatch({
    as.numeric(confint(est)[1, ])
  }, error = function(e) rep(NA_real_, 2))
  
  lower_ci <- ifelse(length(ci) >= 1, ci[1], NA_real_)
  upper_ci <- ifelse(length(ci) >= 2, ci[2], NA_real_)
  deff <- tryCatch(suppressWarnings(as.numeric(attr(est, "deff"))), error = function(e) NA_real_)
  
  
  # --- Construct output safely --------------------------------------
  # Guarantee scalar (length-1) values for every field
  safe_num <- function(x) if (length(x) == 1 && !is.null(x)) x else NA_real_
  safe_chr <- function(x) if (length(x) == 1 && !is.null(x)) as.character(x) else NA_character_
  safe_lgl <- function(x) if (length(x) == 1 && !is.null(x)) as.logical(x) else NA
  
  out <- tibble::tibble(
    variable        = safe_chr(var_name),
    indicator_name  = safe_chr(indicator_name),
    indicator_unit  = safe_chr(indicator_unit),
    
    point.estimate  = round(safe_num(p_est) * multiplier, 2),
    lower_ci        = round(safe_num(lower_ci) * multiplier, 2),
    upper_ci        = round(safe_num(upper_ci) * multiplier, 2),
    
    ci_method       = safe_chr(policy$method),
    
    n_event_unweighted = safe_num(n_event_unweighted),  # 👇 new
    n_event_weighted   = safe_num(n_event_weighted),    # 👇 new
    n_unweighted       = safe_num(n_unweighted),
    n_weighted         = safe_num(n_weighted),          # 👇 renamed for clarity
    n_eff              = round(safe_num(n_eff), 2),
    deff               = round(safe_num(deff), 2),
    
    flag_small_n    = safe_lgl(policy$flags$flag_small_n),
    flag_low_neff   = safe_lgl(policy$flags$flag_low_neff),
    flag_rare_p     = safe_lgl(policy$flags$flag_rare_p),
    flag_high_deff  = safe_lgl(policy$flags$flag_high_deff),
    flag_lonely_psu = safe_lgl(policy$flags$flag_lonely_psu),
    
    note            = safe_chr(policy$note)
  )
  
  print(paste0("I am here 7"))
  print(out)
  
  if (!is.null(group_name_label)) out$group_name <- group_name_label
  if (!is.null(group)) out$group <- as.character(group)
  
  print(paste0("I am here 8"))
  
  return(out)
}

phr_calc_survey_mean_single <- function(design,
                                          var_name,
                                          group = NULL,
                                          indicator_name = "Mean",
                                          indicator_unit = "",
                                          multiplier = 1,
                                          group_name_label = NULL,
                                          high_design_complexity = FALSE) {
  #------------------------------------------------------------
  # Purpose:
  # Safely calculate a single survey-weighted mean with
  # adaptive CI logic, diagnostic flags, and denominators.
  #------------------------------------------------------------
  
  if (!var_name %in% names(design$variables)) {
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = NA_character_,
      n_event_unweighted = NA_real_,
      n_event_weighted = NA_real_,
      n_unweighted = NA_real_,
      n_weighted = NA_real_,
      n_eff = NA_real_,
      deff = NA_real_,
      flag_small_n = NA,
      flag_low_neff = NA,
      flag_rare_p = NA,
      flag_high_deff = NA,
      flag_lonely_psu = NA,
      flag_outlier_var = NA,
      note = "variable not found"
    ))
  }
  
  var_sym <- rlang::sym(var_name)
  data <- design$variables
  
  # Convert srvyr design to survey design if needed
  if (inherits(design, "tbl_svy")) {
    design <- srvyr::as_survey(design)
  }
  
  # --- Weighted and unweighted totals --------------------------
  design_srvyr <- if (inherits(design, "tbl_svy")) design else srvyr::as_survey(design)
  
  dsn_sum <- design_srvyr %>%
    srvyr::summarise(
      n_event_weighted   = srvyr::survey_total(!!var_sym, vartype = NULL, na.rm = TRUE),
      n_event_unweighted = sum(!!var_sym, na.rm = TRUE),
      n_weighted         = srvyr::survey_total(!is.na(!!var_sym), vartype = NULL, na.rm = TRUE),
      n_unweighted       = sum(!is.na(!!var_sym))
    )
  
  n_event_weighted   <- as.numeric(dsn_sum$n_event_weighted)
  n_event_unweighted <- as.numeric(dsn_sum$n_event_unweighted)
  n_weighted         <- as.numeric(dsn_sum$n_weighted)
  n_unweighted       <- as.numeric(dsn_sum$n_unweighted)
  
  # --- Effective sample size & diagnostics ---------------------
  w <- tryCatch(survey::weights(design), error = function(e) rep(1, n_unweighted))
  n_eff <- if (sum(w^2, na.rm = TRUE) > 0) (sum(w, na.rm = TRUE)^2) / sum(w^2, na.rm = TRUE) else NA_real_
  
  lonely_psu <- tryCatch({
    strata_vec <- if (is.data.frame(design$strata)) design$strata[[1]] else design$strata
    ids_vec <- if (is.data.frame(design$ids)) design$ids[[1]] else design$ids
    any(tapply(ids_vec, strata_vec, function(x) length(unique(x)) == 1))
  }, error = function(e) FALSE)
  
  quick_est <- mean(data[[var_name]], na.rm = TRUE)
  quick_var <- var(data[[var_name]], na.rm = TRUE)
  flag_outlier_var <- ifelse(!is.na(quick_var) && quick_var > (10 * quick_est^2), TRUE, FALSE)
  
  # --- CI method policy ----------------------------------------
  policy <- phr_pick_ci_method(
    n_unweighted = n_unweighted,
    n_eff = n_eff,
    p_estimate = quick_est,
    deff = NA,
    lonely_psu = lonely_psu,
    high_design_complexity = high_design_complexity
  )
  
  # --- Estimation ----------------------------------------------
  est <- tryCatch({
    fmla <- as.formula(paste0("~", var_name))
    survey::svymean(fmla, design, na.rm = TRUE, deff = "replace")
  }, error = function(e) {
    message("❌ Survey error: ", conditionMessage(e))
    NULL
  })
  
  if (is.null(est)) {
    return(tibble::tibble(
      variable = var_name,
      indicator_name = indicator_name,
      indicator_unit = indicator_unit,
      point.estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      ci_method = policy$method,
      n_event_unweighted = n_event_unweighted,
      n_event_weighted = n_event_weighted,
      n_unweighted = n_unweighted,
      n_weighted = n_weighted,
      n_eff = n_eff,
      deff = NA_real_,
      flag_small_n = policy$flags$flag_small_n,
      flag_low_neff = policy$flags$flag_low_neff,
      flag_rare_p = policy$flags$flag_rare_p,
      flag_high_deff = policy$flags$flag_high_deff,
      flag_lonely_psu = policy$flags$flag_lonely_psu,
      flag_outlier_var = flag_outlier_var,
      note = paste("estimation failed;", policy$note)
    ))
  }
  
  p_est <- as.numeric(coef(est))
  se <- as.numeric(sqrt(diag(vcov(est))))
  deff <- tryCatch(as.numeric(attr(est, "deff")), error = function(e) NA_real_)
  
  # 95% Wald CI
  ci <- p_est + c(-1.96, 1.96) * se
  lower_ci <- ci[1]
  upper_ci <- ci[2]
  
  # --- Output --------------------------------------------------
  safe_num <- function(x) if (length(x) == 1 && !is.null(x)) x else NA_real_
  safe_chr <- function(x) if (length(x) == 1 && !is.null(x)) as.character(x) else NA_character_
  safe_lgl <- function(x) if (length(x) == 1 && !is.null(x)) as.logical(x) else NA
  
  out <- tibble::tibble(
    variable           = safe_chr(var_name),
    indicator_name     = safe_chr(indicator_name),
    indicator_unit     = safe_chr(indicator_unit),
    point.estimate     = round(safe_num(p_est) * multiplier, 2),
    lower_ci           = round(safe_num(lower_ci) * multiplier, 2),
    upper_ci           = round(safe_num(upper_ci) * multiplier, 2),
    ci_method          = safe_chr("mean-wald"),
    n_event_unweighted = safe_num(n_event_unweighted),
    n_event_weighted   = safe_num(n_event_weighted),
    n_unweighted       = safe_num(n_unweighted),
    n_weighted         = safe_num(n_weighted),
    n_eff              = round(safe_num(n_eff), 2),
    deff               = round(safe_num(deff), 2),
    flag_small_n       = safe_lgl(policy$flags$flag_small_n),
    flag_low_neff      = safe_lgl(policy$flags$flag_low_neff),
    flag_rare_p        = safe_lgl(policy$flags$flag_rare_p),
    flag_high_deff     = safe_lgl(policy$flags$flag_high_deff),
    flag_lonely_psu    = safe_lgl(policy$flags$flag_lonely_psu),
    flag_outlier_var   = safe_lgl(flag_outlier_var),
    note               = safe_chr(policy$note)
  )
  
  if (!is.null(group_name_label)) out$group_name <- group_name_label
  if (!is.null(group)) out$group <- as.character(group)
  
  return(out)
}
