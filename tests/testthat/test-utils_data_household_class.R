# HH_SNAP_COORD Testing ####

test_that("hh_snap_coord() — rounds coordinates to specified decimal places", {

  coord <- 12.3456789

  result <- hh_snap_coord(coord, digits = 6)

  expect_equal(result, 12.345679)
})

test_that("hh_snap_coord() — default digits is 6", {

  coord <- 45.123456789

  result <- hh_snap_coord(coord)

  expect_equal(result, 45.123457)
})

test_that("hh_snap_coord() — handles different decimal precisions", {

  coord <- 12.3456789

  expect_equal(hh_snap_coord(coord, digits = 2), 12.35)
  expect_equal(hh_snap_coord(coord, digits = 4), 12.3457)
  expect_equal(hh_snap_coord(coord, digits = 8), 12.3456789)
})

test_that("hh_snap_coord() — handles NULL input", {

  result <- hh_snap_coord(NULL)

  expect_null(result)
})

test_that("hh_snap_coord() — handles NA values", {

  result <- hh_snap_coord(NA)

  expect_true(is.na(result))
})

test_that("hh_snap_coord() — handles negative coordinates", {

  coord <- -12.3456789

  result <- hh_snap_coord(coord, digits = 4)

  expect_equal(result, -12.3457)
})

# HH_DMS_TO_DECIMAL Testing ####

test_that("hh_dms_to_decimal() — converts DMS format to decimal", {

  dms <- "40°26'46\""

  result <- hh_dms_to_decimal(dms)

  # 40 + 26/60 + 46/3600 = 40.44611...
  expect_true(abs(result - 40.44611) < 0.001)
})

test_that("hh_dms_to_decimal() — handles different separators", {

  dms1 <- "40°26'46\""
  dms2 <- "40 26 46"

  result1 <- hh_dms_to_decimal(dms1)
  result2 <- hh_dms_to_decimal(dms2)

  expect_equal(unname(result1), unname(result2))
})

test_that("hh_dms_to_decimal() — handles already decimal values", {

  decimal_str <- "40.44611"

  result <- hh_dms_to_decimal(decimal_str)

  expect_equal(unname(result), 40.44611)
})

test_that("hh_dms_to_decimal() — handles vector input", {

  dms_vec <- c("40°26'46\"", "50 30 20", "60.5")

  result <- hh_dms_to_decimal(dms_vec)

  expect_equal(length(result), 3)
  expect_true(abs(result[1] - 40.44611) < 0.001)
  expect_true(abs(result[2] - 50.50556) < 0.001)
  expect_equal(unname(result[3]), 60.5)
})

test_that("hh_dms_to_decimal() — handles whitespace", {

  dms <- "  40°26'46\"  "

  result <- hh_dms_to_decimal(dms)

  expect_true(abs(result - 40.44611) < 0.001)
})

# HH_FLAG_WEIGHT_OUTLIERS Testing ####

test_that("hh_flag_weight_outliers() — detects outliers using IQR rule", {

  weights <- c(1, 2, 2, 3, 3, 3, 4, 4, 5, 100)

  expect_warning(
    outliers <- hh_flag_weight_outliers(weights)
  )

  expect_true(10 %in% outliers)  # 100 is an outlier
})

test_that("hh_flag_weight_outliers() — returns empty when no outliers", {

  weights <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  outliers <- hh_flag_weight_outliers(weights)

  expect_equal(length(outliers), 0)
})

test_that("hh_flag_weight_outliers() — handles empty input", {

  weights <- numeric(0)

  outliers <- hh_flag_weight_outliers(weights)

  expect_equal(length(outliers), 0)
})

test_that("hh_flag_weight_outliers() — handles NA values", {

  weights <- c(1, 2, 3, NA, 5, 6, 7, 8, 9, 10)

  outliers <- hh_flag_weight_outliers(weights)

  expect_true(is.integer(outliers))
})

test_that("hh_flag_weight_outliers() — handles zero IQR", {

  weights <- c(5, 5, 5, 5, 5)

  outliers <- hh_flag_weight_outliers(weights)

  expect_equal(length(outliers), 0)
})

# HH_CHECK_WEIGHT_INTEGRITY Testing ####

test_that("hh_check_weight_integrity() — detects negative weights", {

  weights <- c(1, -2, 3, 4)

  expect_warning(
    issues <- hh_check_weight_integrity(weights)
  )

  expect_equal(issues$negative, 2)
})

test_that("hh_check_weight_integrity() — detects zero weights", {

  weights <- c(1, 0, 3, 0, 5)

  expect_warning(
    issues <- hh_check_weight_integrity(weights)
  )

  expect_equal(issues$zero, c(2, 4))
})

test_that("hh_check_weight_integrity() — detects missing weights", {

  weights <- c(1, NA, 3, NA, 5)

  expect_warning(
    issues <- hh_check_weight_integrity(weights)
  )

  expect_equal(issues$missing, c(2, 4))
})

test_that("hh_check_weight_integrity() — detects multiple issue types", {

  weights <- c(1, -2, 0, NA, 5)

  expect_warning(
    issues <- hh_check_weight_integrity(weights)
  )

  expect_equal(issues$negative, 2)
  expect_equal(issues$zero, 3)
  expect_equal(issues$missing, 4)
})

test_that("hh_check_weight_integrity() — returns empty lists when no issues", {

  weights <- c(1, 2, 3, 4, 5)

  issues <- hh_check_weight_integrity(weights)

  expect_equal(length(issues$negative), 0)
  expect_equal(length(issues$zero), 0)
  expect_equal(length(issues$missing), 0)
})

# HH_NORMALIZE_ADM Testing ####

test_that("hh_normalize_adm() — converts to title case", {

  names <- c("BAGHDAD", "basra", "MoSuL")

  result <- hh_normalize_adm(names)

  expect_equal(result, c("Baghdad", "Basra", "Mosul"))
})

test_that("hh_normalize_adm() — trims whitespace", {

  names <- c("  Baghdad  ", "Basra   ", "   Mosul")

  result <- hh_normalize_adm(names)

  expect_equal(result, c("Baghdad", "Basra", "Mosul"))
})

test_that("hh_normalize_adm() — converts empty strings to NA", {

  names <- c("Baghdad", "", "Mosul")

  result <- hh_normalize_adm(names)

  expect_true(is.na(result[2]))
  expect_equal(result[1], "Baghdad")
  expect_equal(result[3], "Mosul")
})

test_that("hh_normalize_adm() — handles mixed case correctly", {

  names <- c("al-Anbar", "al-basra", "AL-MOSUL")

  result <- hh_normalize_adm(names)

  expect_equal(result, c("Al-Anbar", "Al-Basra", "Al-Mosul"))
})

# HH_VALIDATE_ADM Testing ####

test_that("hh_validate_adm() — detects invalid admin units", {

  units <- c("Baghdad", "Basra", "InvalidCity", "Mosul")
  valid <- c("Baghdad", "Basra", "Mosul", "Erbil")

  expect_warning(
    bad <- hh_validate_adm(units, valid)
  )

  expect_equal(bad, "InvalidCity")
})

test_that("hh_validate_adm() — returns empty when all valid", {

  units <- c("Baghdad", "Basra", "Mosul")
  valid <- c("Baghdad", "Basra", "Mosul", "Erbil")

  bad <- hh_validate_adm(units, valid)

  expect_equal(length(bad), 0)
})

test_that("hh_validate_adm() — handles NA values correctly", {

  units <- c("Baghdad", NA, "InvalidCity")
  valid <- c("Baghdad", "Basra", "Mosul")

  expect_warning(
    bad <- hh_validate_adm(units, valid)
  )

  expect_equal(bad, "InvalidCity")
  expect_false(any(is.na(bad)))
})

test_that("hh_validate_adm() — detects multiple invalid units", {

  units <- c("Baghdad", "Invalid1", "Basra", "Invalid2")
  valid <- c("Baghdad", "Basra", "Mosul")

  expect_warning(
    bad <- hh_validate_adm(units, valid)
  )

  expect_equal(length(bad), 2)
  expect_true("Invalid1" %in% bad)
  expect_true("Invalid2" %in% bad)
})

# HH_CHECK_CONSENT_SKIP Testing ####

test_that("hh_check_consent_skip() — detects skip logic violations", {

  df <- tibble::tibble(
    consent = c("yes", "no", "no", "yes"),
    question1 = c("answer", "should_be_empty", NA, "answer"),
    question2 = c("answer", NA, "should_be_empty", "answer")
  )

  expect_warning(
    issues <- hh_check_consent_skip(df, "consent", c("question1", "question2"))
  )

  expect_equal(issues$question1, 2)
  expect_equal(issues$question2, 3)
})

test_that("hh_check_consent_skip() — returns empty list when no violations", {

  df <- tibble::tibble(
    consent = c("yes", "no", "yes"),
    question1 = c("answer", NA, "answer"),
    question2 = c("answer", NA, "answer")
  )

  issues <- hh_check_consent_skip(df, "consent", c("question1", "question2"))

  expect_equal(length(issues), 0)
})

test_that("hh_check_consent_skip() — handles missing consent column", {

  df <- tibble::tibble(
    question1 = c("answer", "answer")
  )

  expect_warning(
    issues <- hh_check_consent_skip(df, "consent", c("question1"))
  )

  expect_equal(length(issues), 0)
})

test_that("hh_check_consent_skip() — ignores missing skip columns", {

  df <- tibble::tibble(
    consent = c("yes", "no"),
    question1 = c("answer", "answer")
  )

  issues <- hh_check_consent_skip(df, "consent", c("question1", "nonexistent"))

  expect_true(is.list(issues))
})

test_that("hh_check_consent_skip() — case insensitive for 'no'", {

  df <- tibble::tibble(
    consent = c("yes", "No", "NO", "no"),
    question1 = c("answer", "violation", "violation", "violation")
  )

  expect_warning(
    issues <- hh_check_consent_skip(df, "consent", c("question1"))
  )

  expect_equal(length(issues$question1), 3)
})

# ADD_INTERVIEW_TIME Testing ####

test_that("add_interview_time() — adds duration column in minutes (POSIXct input)", {

  df <- tibble::tibble(
    interview_start = as.POSIXct(c("2025-10-16 09:00:00", "2025-10-17 10:00:00"), tz = "UTC"),
    interview_end   = as.POSIXct(c("2025-10-16 09:30:00", "2025-10-17 10:45:00"), tz = "UTC")
  )

  result <- add_interview_time(df)

  expect_true("interview_duration_mins" %in% names(result))
  expect_equal(result$interview_duration_mins, c(30.00, 45.00))
})

test_that("add_interview_time() — rounds to 2 decimal places", {

  df <- tibble::tibble(
    interview_start = as.POSIXct("2025-10-16 09:00:00", tz = "UTC"),
    interview_end   = as.POSIXct("2025-10-16 09:01:40", tz = "UTC")  # 1 min 40 sec = 1.666... mins
  )

  result <- add_interview_time(df)

  expect_equal(result$interview_duration_mins, 1.67)
})

test_that("add_interview_time() — accepts custom column names and output column name", {

  df <- tibble::tibble(
    start_time = as.POSIXct("2025-10-16 08:00:00", tz = "UTC"),
    end_time   = as.POSIXct("2025-10-16 08:20:00", tz = "UTC")
  )

  result <- add_interview_time(df, start_col = "start_time", end_col = "end_time",
                               new_col = "duration_mins")

  expect_true("duration_mins" %in% names(result))
  expect_equal(result$duration_mins, 20.00)
})

test_that("add_interview_time() — warns when output column already exists", {

  df <- tibble::tibble(
    interview_start          = as.POSIXct("2025-10-16 09:00:00", tz = "UTC"),
    interview_end            = as.POSIXct("2025-10-16 09:15:00", tz = "UTC"),
    interview_duration_mins  = 999
  )

  expect_warning(
    result <- add_interview_time(df)
  )

  expect_equal(result$interview_duration_mins, 15.00)
})

test_that("add_interview_time() — warns when start_col is not a datetime", {

  df <- tibble::tibble(
    interview_start = c("not-a-datetime"),
    interview_end   = as.POSIXct("2025-10-16 09:15:00", tz = "UTC")
  )

  expect_warning(
    add_interview_time(df)
  )
})

test_that("add_interview_time() — warns when end_col is missing from dataset", {

  df <- tibble::tibble(
    interview_start = as.POSIXct("2025-10-16 09:00:00", tz = "UTC")
  )

  expect_warning(
    add_interview_time(df)
  )
})

test_that("add_interview_time() — handles NA values in datetime columns", {

  df <- tibble::tibble(
    interview_start = as.POSIXct(c("2025-10-16 09:00:00", NA), tz = "UTC"),
    interview_end   = as.POSIXct(c("2025-10-16 09:30:00", "2025-10-17 10:45:00"), tz = "UTC")
  )

  result <- add_interview_time(df)

  expect_true(is.na(result$interview_duration_mins[2]))
  expect_equal(result$interview_duration_mins[1], 30.00)
})


