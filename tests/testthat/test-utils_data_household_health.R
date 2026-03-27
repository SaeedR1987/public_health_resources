# ADD_HEALTH_BARRIERS Testing ####

test_that("add_health_barriers() — valid dataset creates barrier indicators", {

  df <- tibble::tibble(
    health_barriers = c(
      "long distance,cost",
      "poor quality,unsafe facilities",
      "no healthcare available",
      "no barriers",
      "other"
    )
  )

  out <- add_health_barriers(
    .dataset = df,
    health_barriers_col = "health_barriers",
    physical_access_barriers_val = c("long distance", "transport issues"),
    financial_access_barriers_val = c("cost", "expensive"),
    safety_access_barriers_val = c("unsafe facilities", "insecurity"),
    quality_barriers_val = c("poor quality", "lack of equipment"),
    healthcare_seeking_barriers_val = c("refusal", "cultural reasons"),
    availability_barriers_val = c("no healthcare available", "services unavailable"),
    other_barriers_val = c("other"),
    no_barriers_val = c("no barriers"),
    did_not_need_val = c("did not need")
  )

  expect_equal(nrow(out), 5)
  expect_true("health_barrier_any.physical" %in% names(out))
  expect_true("health_barrier_any.financial" %in% names(out))
  expect_true("health_barrier_any.safety" %in% names(out))
  expect_true("health_barrier_any.quality" %in% names(out))
  expect_true("health_barrier_any.availability" %in% names(out))
  expect_true("health_barrier_any.other" %in% names(out))
  expect_true("health_barrier_any.none" %in% names(out))
})

test_that("add_health_barriers() — barrier detection works correctly", {

  df <- tibble::tibble(
    barriers = c(
      "long distance",
      "cost",
      "unsafe facilities",
      "no barriers"
    )
  )

  out <- add_health_barriers(
    .dataset = df,
    health_barriers_col = "barriers",
    physical_access_barriers_val = c("long distance"),
    financial_access_barriers_val = c("cost"),
    safety_access_barriers_val = c("unsafe facilities"),
    quality_barriers_val = c("poor quality"),
    healthcare_seeking_barriers_val = c("refusal"),
    availability_barriers_val = c("no healthcare"),
    other_barriers_val = c("other"),
    no_barriers_val = c("no barriers"),
    did_not_need_val = c("did not need")
  )

  expect_equal(out$health_barrier_any.physical[1], 1)
  expect_equal(out$health_barrier_any.financial[2], 1)
  expect_equal(out$health_barrier_any.safety[3], 1)
  expect_equal(out$health_barrier_any.none[4], 1)
})

test_that("add_health_barriers() — multiple barriers detected in one response", {

  df <- tibble::tibble(
    barriers = c("long distance,cost,unsafe facilities")
  )

  out <- add_health_barriers(
    .dataset = df,
    health_barriers_col = "barriers",
    physical_access_barriers_val = c("long distance"),
    financial_access_barriers_val = c("cost"),
    safety_access_barriers_val = c("unsafe facilities"),
    quality_barriers_val = c("poor quality"),
    healthcare_seeking_barriers_val = c("refusal"),
    availability_barriers_val = c("no healthcare"),
    other_barriers_val = c("other"),
    no_barriers_val = c("no barriers"),
    did_not_need_val = c("did not need")
  )

  expect_equal(out$health_barrier_any.physical[1], 1)
  expect_equal(out$health_barrier_any.financial[1], 1)
  expect_equal(out$health_barrier_any.safety[1], 1)
})

test_that("add_health_barriers() — error on empty dataset", {

  df_empty <- tibble::tibble(
    barriers = character(0)
  )

  expect_error(
    add_health_barriers(
      .dataset = df_empty,
      health_barriers_col = "barriers",
      physical_access_barriers_val = c("distance"),
      financial_access_barriers_val = c("cost"),
      safety_access_barriers_val = c("unsafe"),
      quality_barriers_val = c("quality"),
      healthcare_seeking_barriers_val = c("refusal"),
      availability_barriers_val = c("none"),
      other_barriers_val = c("other"),
      no_barriers_val = c("no barriers"),
      did_not_need_val = c("did not need")
    )
  )
})

test_that("add_health_barriers() — error on missing column", {

  df <- tibble::tibble(
    wrong_col = c("barriers")
  )

  expect_error(
    add_health_barriers(
      .dataset = df,
      health_barriers_col = "barriers",
      physical_access_barriers_val = c("distance"),
      financial_access_barriers_val = c("cost"),
      safety_access_barriers_val = c("unsafe"),
      quality_barriers_val = c("quality"),
      healthcare_seeking_barriers_val = c("refusal"),
      availability_barriers_val = c("none"),
      other_barriers_val = c("other"),
      no_barriers_val = c("no barriers"),
      did_not_need_val = c("did not need")
    )
  )
})

test_that("add_health_barriers() — warning when overwriting existing columns", {

  df <- tibble::tibble(
    barriers = c("long distance"),
    health_barrier_any.physical = 0
  )

  expect_warning(
    add_health_barriers(
      .dataset = df,
      health_barriers_col = "barriers",
      physical_access_barriers_val = c("long distance"),
      financial_access_barriers_val = c("cost"),
      safety_access_barriers_val = c("unsafe"),
      quality_barriers_val = c("quality"),
      healthcare_seeking_barriers_val = c("refusal"),
      availability_barriers_val = c("none"),
      other_barriers_val = c("other"),
      no_barriers_val = c("no barriers"),
      did_not_need_val = c("did not need")
    )
  )
})

# ADD_HEALTHCARE_ACCESS_ONE_HOUR Testing ####

test_that("add_healthcare_access_one_hour() — numeric minutes works correctly", {

  df <- tibble::tibble(
    travel_time_type = c("num_minutes", "num_minutes", "num_minutes"),
    travel_minutes = c(45, 65, 30),
    travel_range = c(NA, NA, NA)
  )

  out <- add_healthcare_access_one_hour(
    .dataset = df,
    health_care_travel_time_col = "travel_time_type",
    num_minutes_val = "num_minutes",
    range_val = "range",
    health_care_travel_time_minutes_col = "travel_minutes",
    health_care_travel_time_range_col = "travel_range",
    less_than_one_hour_range_val = c("<1_hour"),
    one_hour_or_more_range_val = c(">=1_hour")
  )

  expect_equal(nrow(out), 3)
  expect_true("health_healthcare_access_one_hour" %in% names(out))
  expect_true(grepl("yes", out$health_healthcare_access_one_hour[1]))
  expect_true(grepl("no", out$health_healthcare_access_one_hour[2]))
  expect_true(grepl("yes", out$health_healthcare_access_one_hour[3]))
})

test_that("add_healthcare_access_one_hour() — range-based classification works", {

  df <- tibble::tibble(
    travel_time_type = c("range", "range", "range"),
    travel_minutes = c(NA, NA, NA),
    travel_range = c("<1_hour", ">=1_hour", "<1_hour")
  )

  out <- add_healthcare_access_one_hour(
    .dataset = df,
    health_care_travel_time_col = "travel_time_type",
    num_minutes_val = "num_minutes",
    range_val = "range",
    health_care_travel_time_minutes_col = "travel_minutes",
    health_care_travel_time_range_col = "travel_range",
    less_than_one_hour_range_val = c("<1_hour"),
    one_hour_or_more_range_val = c(">=1_hour")
  )

  expect_true(grepl("yes", out$health_healthcare_access_one_hour[1]))
  expect_true(grepl("no", out$health_healthcare_access_one_hour[2]))
  expect_true(grepl("yes", out$health_healthcare_access_one_hour[3]))
})

test_that("add_healthcare_access_one_hour() — numeric takes priority over range", {

  df <- tibble::tibble(
    travel_time_type = c("num_minutes"),
    travel_minutes = c(45),
    travel_range = c(">=1_hour")  # conflicting but should be ignored
  )

  out <- add_healthcare_access_one_hour(
    .dataset = df,
    health_care_travel_time_col = "travel_time_type",
    num_minutes_val = "num_minutes",
    range_val = "range",
    health_care_travel_time_minutes_col = "travel_minutes",
    health_care_travel_time_range_col = "travel_range",
    less_than_one_hour_range_val = c("<1_hour"),
    one_hour_or_more_range_val = c(">=1_hour")
  )

  # Should use numeric (45 minutes) not range (>=1_hour)
  expect_true(grepl("yes", out$health_healthcare_access_one_hour[1]))
})

test_that("add_healthcare_access_one_hour() — missing data returns dont_know", {

  df <- tibble::tibble(
    travel_time_type = c("num_minutes"),
    travel_minutes = c(NA),
    travel_range = c(NA)
  )

  out <- add_healthcare_access_one_hour(
    .dataset = df,
    health_care_travel_time_col = "travel_time_type",
    num_minutes_val = "num_minutes",
    range_val = "range",
    health_care_travel_time_minutes_col = "travel_minutes",
    health_care_travel_time_range_col = "travel_range",
    less_than_one_hour_range_val = c("<1_hour"),
    one_hour_or_more_range_val = c(">=1_hour")
  )

  expect_true(grepl("dont_know", out$health_healthcare_access_one_hour[1]))
})

test_that("add_healthcare_access_one_hour() — error on empty dataset", {

  df_empty <- tibble::tibble(
    travel_time_type = character(0),
    travel_minutes = numeric(0),
    travel_range = character(0)
  )

  expect_error(
    add_healthcare_access_one_hour(
      .dataset = df_empty,
      health_care_travel_time_col = "travel_time_type",
      num_minutes_val = "num_minutes",
      range_val = "range",
      health_care_travel_time_minutes_col = "travel_minutes",
      health_care_travel_time_range_col = "travel_range",
      less_than_one_hour_range_val = c("<1_hour"),
      one_hour_or_more_range_val = c(">=1_hour")
    )
  )
})

test_that("add_healthcare_access_one_hour() — error on missing columns", {

  df <- tibble::tibble(
    travel_time_type = c("num_minutes")
  )

  expect_error(
    add_healthcare_access_one_hour(
      .dataset = df,
      health_care_travel_time_col = "travel_time_type",
      num_minutes_val = "num_minutes",
      range_val = "range",
      health_care_travel_time_minutes_col = "travel_minutes",
      health_care_travel_time_range_col = "travel_range",
      less_than_one_hour_range_val = c("<1_hour"),
      one_hour_or_more_range_val = c(">=1_hour")
    )
  )
})

test_that("add_healthcare_access_one_hour() — warning when overwriting existing column", {

  df <- tibble::tibble(
    travel_time_type = c("num_minutes"),
    travel_minutes = c(45),
    travel_range = c(NA),
    health_healthcare_access_one_hour = "old"
  )

  expect_warning(
    add_healthcare_access_one_hour(
      .dataset = df,
      health_care_travel_time_col = "travel_time_type",
      num_minutes_val = "num_minutes",
      range_val = "range",
      health_care_travel_time_minutes_col = "travel_minutes",
      health_care_travel_time_range_col = "travel_range",
      less_than_one_hour_range_val = c("<1_hour"),
      one_hour_or_more_range_val = c(">=1_hour")
    )
  )
})

test_that("add_healthcare_access_one_hour() — boundary value at 60 minutes", {

  df <- tibble::tibble(
    travel_time_type = c("num_minutes", "num_minutes"),
    travel_minutes = c(59, 60),
    travel_range = c(NA, NA)
  )

  out <- add_healthcare_access_one_hour(
    .dataset = df,
    health_care_travel_time_col = "travel_time_type",
    num_minutes_val = "num_minutes",
    range_val = "range",
    health_care_travel_time_minutes_col = "travel_minutes",
    health_care_travel_time_range_col = "travel_range",
    less_than_one_hour_range_val = c("<1_hour"),
    one_hour_or_more_range_val = c(">=1_hour")
  )

  expect_true(grepl("yes", out$health_healthcare_access_one_hour[1]))  # < 60
  expect_true(grepl("no", out$health_healthcare_access_one_hour[2]))   # >= 60
})

