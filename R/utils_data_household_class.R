
# GPS UTILITIES (IPHRA-AWARE)

#' Snap GPS coordinates to fixed decimal precision
#' @param x Numeric vector of GPS coordinate values to round.
#' @param digits Integer specifying the number of decimal places to round to. Default: 6.
hh_snap_coord <- function(x, digits = 6) {
  phr_try({
    if (is.null(x)) return(NULL)
    as.numeric(round(as.numeric(x), digits = digits))
  }, on_error = "warn", origin = "hh_snap_coord")
}

#' Convert DMS to decimal degrees
#' @param x Character or numeric vector of coordinates in degrees-minutes-seconds (DMS) format.
hh_dms_to_decimal <- function(x) {
  phr_try({

    parse_one <- function(val) {
      val <- trimws(val)
      val <- gsub("[°'\"\\\"]", " ", val)

      nums <- suppressWarnings(as.numeric(strsplit(val, "\\s+")[[1]]))
      if (length(nums) == 3) nums[1] + nums[2] / 60 + nums[3] / 3600
      else suppressWarnings(as.numeric(val))
    }

    vapply(x, parse_one, numeric(1))

  }, on_error = "warn", origin = "hh_dms_to_decimal")
}


# WEIGHT UTILITIES


#' Identify outliers using IQR rule
#' @param w Numeric vector of weight values to check for outliers.
hh_flag_weight_outliers <- function(w) {
  phr_try({

    w <- suppressWarnings(as.numeric(w))
    if (length(w) == 0) return(integer(0))

    q <- stats::quantile(w, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]

    if (iqr == 0) return(integer(0))

    lower <- q[1] - 3 * iqr
    upper <- q[2] + 3 * iqr

    outliers <- which(w < lower | w > upper)

    if (length(outliers) > 0) {
      phr_warning(
        "HouseholdData",
        phr_txt(glue::glue("Weight outliers detected at rows: {paste(outliers, collapse=', ')}"))
      )
    }

    outliers

  }, on_error = "warn", origin = "hh_flag_weight_outliers")
}

#' Detect invalid weights: negative, zero, missing
#' @param w Numeric vector of weight values to check for integrity issues.
hh_check_weight_integrity <- function(w) {
  phr_try({

    w <- suppressWarnings(as.numeric(w))
    issues <- list(
      negative = which(w < 0),
      zero     = which(w == 0),
      missing  = which(is.na(w))
    )

    if (length(unlist(issues)) > 0) {
      phr_warning(
        "HouseholdData",
        phr_txt(
          "Weight integrity issues: {length(issues$negative)} negative, {length(issues$zero)} zero, {length(issues$missing)} missing."
        )
      )
    }

    issues

  }, on_error = "warn", origin = "hh_check_weight_integrity")
}


# ADM UNIT NORMALIZATION


#' Normalize administrative names
#' @param x Character vector of administrative unit names to normalize.
hh_normalize_adm <- function(x) {
  phr_try({
    x <- trimws(as.character(x))
    x <- ifelse(x == "", NA_character_, x)
    tools::toTitleCase(tolower(x))
  }, on_error = "warn", origin = "hh_normalize_adm")
}

#' Validate admin codes/names against reference values
#' @param x Character vector of administrative unit values to validate.
#' @param valid Character vector of valid/accepted administrative unit values.
hh_validate_adm <- function(x, valid) {
  phr_try({

    bad <- setdiff(unique(x), valid)

    if (length(bad) > 0) {
      phr_warning(
        "HouseholdData",
        phr_txt(glue::glue("Invalid administrative units detected: {paste(bad, collapse=', ')}"))
      )
    }

    bad[!is.na(bad)]

  }, on_error = "warn", origin = "hh_validate_adm")
}


# SKIP-LOGIC & CONSISTENCY CHECKS


#' Enforce consent-based skip logic
#' @param df A data frame containing the consent and skip columns.
#' @param consent_col Character string specifying the column name for consent responses.
#' @param skip_cols Character vector of column names that should be skipped when consent is "no".
hh_check_consent_skip <- function(df, consent_col, skip_cols) {
  phr_try({

    if (!consent_col %in% names(df)) {
      phr_warning("HouseholdData", phr_txt(glue::glue("Consent column '{consent_col}' not found.")))
      return(list())
    }

    issues <- list()
    deny <- which(tolower(df[[consent_col]]) == "no")

    for (col in skip_cols) {
      if (col %in% names(df)) {
        bad <- intersect(deny, which(!is.na(df[[col]]) & df[[col]] != ""))
        if (length(bad) > 0) issues[[col]] <- bad
      }
    }

    if (length(issues) > 0) {
      phr_warning(
        "HouseholdData",
        phr_txt(glue::glue("Consent skip-logic violations detected in columns: {paste(names(issues), collapse=', ')}"))
      )
    }

    issues

  }, on_error = "warn", origin = "hh_check_consent_skip")
}

#' Flag interview dates earlier than project start
#' @param dates Character, Date, or numeric vector of interview dates to check.
#' @param project_start A Date or character value specifying the project start date.
hh_flag_early_interviews <- function(dates, project_start) {
  phr_try({

    d <- suppressWarnings(phr_convert_date(dates))
    bad <- which(!is.na(d) & d < project_start)

    if (length(bad) > 0) {
      phr_warning(
        "HouseholdData",
        phr_txt(glue::glue("Interview dates earlier than project start detected at rows: {paste(bad, collapse=', ')}"))
      )
    }

    bad

  }, on_error = "warn", origin = "hh_flag_early_interviews")
}


# INTERVIEW TIME UTILITIES


#' Calculate and add interview duration in minutes
#'
#' @description
#' Computes the difference in time in minutes (rounded to 2 decimal places)
#' between a start datetime column and an end datetime column, and adds the
#' result as a new column in the dataset.
#'
#' Both columns must contain POSIXct, POSIXlt, or character datetime values
#' with a time component (e.g. `"2025-10-16 14:32:00"`).
#'
#' @param .dataset A data frame containing the start and end datetime columns.
#' @param start_col Character; name of the column containing start datetimes.
#'   Defaults to `"interview_start"`.
#' @param end_col Character; name of the column containing end datetimes.
#'   Defaults to `"interview_end"`.
#' @param new_col Character; name of the new duration column to create.
#'   Defaults to `"interview_duration_mins"`.
#'
#' @return The input data frame with a new numeric column containing interview
#'   duration in minutes (to 2 decimal places).
#' @export
add_interview_time <- function(.dataset,
                               start_col = "interview_start",
                               end_col = "interview_end",
                               new_col = "interview_duration_mins") {

  origin <- "add_interview_time"

  phr_try(
    expr = {

      # Validate dataset structure
      phr_validate_dataframe(.dataset, origin = origin, soft = FALSE)
      phr_assert(nrow(.dataset) > 0, origin, phr_txt("Dataset must not be empty."))

      # Validate required columns exist
      phr_validate_columns(.dataset, c(start_col, end_col), origin = origin, soft = FALSE)

      # Validate that both columns contain datetime values
      phr_validate_datetime(.dataset[[start_col]], origin = origin, soft = FALSE)
      phr_validate_datetime(.dataset[[end_col]], origin = origin, soft = FALSE)

      # Warn if the output column already exists
      if (new_col %in% names(.dataset)) {
        phr_warning(
          origin,
          phr_txt(glue::glue("Column '{new_col}' already exists and will be overwritten."))
        )
      }

      # Calculate interview duration in minutes, rounded to 2 decimal places
      .dataset <- .dataset %>%
        dplyr::mutate(
          !!new_col := round(
            as.numeric(difftime(.data[[end_col]], .data[[start_col]], units = "mins")),
            2
          )
        )

      return(.dataset)

    },
    on_error = "warn", origin = origin
  )
}


# LINKAGE UTILITIES


#' Validate loop dataset counts against parent dataset
#'
#' @description
#' Checks that the number of loop records per parent UUID matches the expected
#' count recorded in the parent dataset. Identifies mismatches and orphan records.
#'
#' @param parent_df Data frame: Parent dataset (e.g., household data)
#' @param child_df Data frame: Loop/child dataset (e.g., individual roster)
#' @param parent_uuid Character: Column name for UUID in parent dataset
#' @param child_uuid Character: Column name for parent UUID in child dataset
#' @param parent_count Character: Column name for expected count in parent dataset
#' @param soft_missing_parent Logical: If TRUE, allows missing counts when actual=0; if FALSE, flags as mismatch
#'
#' @return Character vector of UUIDs with mismatches
#' @export
loop_count_check <- function(parent_df,
                             child_df,
                             parent_uuid,
                             child_uuid,
                             parent_count,
                             soft_missing_parent = TRUE) {

  phr_try({


    # 1. Structural validation

    if (!parent_uuid %in% names(parent_df)) {
      phr_error("LoopCheck",
                  phr_txt(glue::glue("Parent UUID column '{parent_uuid}' not found in parent dataset."))
      )
    }

    if (!child_uuid %in% names(child_df)) {
      phr_error("LoopCheck",
                  phr_txt(glue::glue("Child UUID column '{child_uuid}' not found in child dataset."))
      )
    }

    if (!parent_count %in% names(parent_df)) {
      phr_error("LoopCheck",
                  phr_txt(glue::glue("Parent count column '{parent_count}' missing in parent dataset."))
      )
    }


    # 2. Prepare vectors

    parent_ids <- as.character(parent_df[[parent_uuid]])
    expected_counts <- parent_df[[parent_count]]

    # Compute loop counts
    child_ids <- as.character(child_df[[child_uuid]])
    actual_counts <- table(child_ids)

    # Convert table → numeric vector
    actual_counts <- as.numeric(actual_counts)
    names(actual_counts) <- names(table(child_ids))

    # Build comparison table
    df_out <- data.frame(
      uuid = parent_ids,
      expected = expected_counts,
      actual = actual_counts[parent_ids],
      stringsAsFactors = FALSE
    )

    df_out$actual[is.na(df_out$actual)] <- 0

    mismatches <- c()


    # 3. Loop UUIDs not in parent → mismatch

    extra_loop_ids <- setdiff(names(table(child_ids)), parent_ids)

    if (length(extra_loop_ids) > 0) {
      mismatches <- c(mismatches, extra_loop_ids)
      phr_warning(
        "LoopCheck",
        phr_txt(glue::glue("Loop dataset contains UUIDs not found in parent dataset: {paste(extra_loop_ids, collapse=', ')}"))
      )
    }


    # 4. Parent expected = NA AND actual > 0 → mismatch

    problem_na_child <- df_out$uuid[
      is.na(df_out$expected) & df_out$actual > 0
    ]

    if (length(problem_na_child) > 0) {
      mismatches <- c(mismatches, problem_na_child)
      phr_warning(
        "LoopCheck",
        phr_txt(glue::glue("Parent has missing expected count but loop contains rows for: {paste(problem_na_child, collapse=', ')}"))
      )
    }


    # 5. NEW RULE: Parent expected = NA AND actual = 0

    if (!soft_missing_parent) {
      problem_na_soft <- df_out$uuid[
        is.na(df_out$expected) & df_out$actual == 0
      ]

      if (length(problem_na_soft) > 0) {
        mismatches <- c(mismatches, problem_na_soft)
        phr_warning(
          "LoopCheck",
          phr_txt(glue::glue("Missing count detected for UUIDs with no loop rows under 'hard' rule: {paste(problem_na_soft, collapse=', ')}"))
        )
      }
    }


    # 6. Numeric mismatch when expected is known

    numeric_mismatch <- df_out$uuid[
      !is.na(df_out$expected) &
        df_out$expected != df_out$actual
    ]

    if (length(numeric_mismatch) > 0) {
      mismatches <- c(mismatches, numeric_mismatch)
      phr_warning(
        "LoopCheck",
        phr_txt(glue::glue("Count mismatches detected for: {paste(numeric_mismatch, collapse=', ')}"))
      )
    }

    mismatches <- unique(mismatches)


    # 7. Return results

    return(list(
      ok = (length(mismatches) == 0),
      mismatches = mismatches,
      details = df_out
    ))

  }, on_error = "warn", origin = "loop_count_check")
}

#' Check 1:many linkage integrity
#' @param hh_df A data frame containing household-level data.
#' @param roster_df A data frame containing roster (individual-level) data.
#' @param uuid_hh Character string specifying the UUID column name in the household data frame.
#' @param uuid_roster Character string specifying the UUID column name in the roster data frame that links to households.
hh_check_roster_1_to_many <- function(hh_df, roster_df, uuid_hh, uuid_roster) {
  phr_try({

    res <- list(
      missing_households = hh_roster_link_check(hh_df, roster_df, uuid_hh, uuid_roster),
      households_with_members = unique(roster_df[[uuid_roster]])
    )

    res

  }, on_error = "warn", origin = "hh_check_roster_1_to_many")
}

#' Check relationship-to-head plausibility
#' @param rel_vec Character vector of relationship-to-head codes to validate.
hh_check_roster_relationships <- function(rel_vec) {
  phr_try({

    allowed <- c("head", "spouse", "child", "parent", "relative", "other")
    bad <- setdiff(unique(rel_vec), allowed)

    if (length(bad) > 0) {
      phr_warning(
        "HouseholdData",
        phr_txt(glue::glue("Invalid relationship codes detected: {paste(bad, collapse=', ')}"))
      )
    }

    bad[!is.na(bad)]

  }, on_error = "warn", origin = "hh_check_roster_relationships")
}
