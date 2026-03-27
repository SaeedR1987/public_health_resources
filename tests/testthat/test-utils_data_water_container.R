# ADD_LPPD_CORRECTION_FACTOR Testing ####

test_that("add_lppd_correction_factor() — valid dataset creates correction factor", {

  df <- tibble::tibble(
    num_days_collected = c(0, 3, 7)
  )

  out <- add_lppd_correction_factor(
    .dataset = df,
    num_days_collect_col = "num_days_collected"
  )

  expect_equal(nrow(out), 3)
  expect_true("lppd_correction_factor" %in% names(out))
})

test_that("add_lppd_correction_factor() — calculation is correct", {

  df <- tibble::tibble(
    num_days = c(0, 7, 3, 5)
  )

  out <- add_lppd_correction_factor(
    .dataset = df,
    num_days_collect_col = "num_days"
  )

  # 0/7 = 0, 7/7 = 1, 3/7 = 0.429, 5/7 = 0.714
  expect_equal(out$lppd_correction_factor[1], 0)
  expect_equal(out$lppd_correction_factor[2], 1)
  expect_equal(round(out$lppd_correction_factor[3], 3), 0.429)
  expect_equal(round(out$lppd_correction_factor[4], 3), 0.714)
})

test_that("add_lppd_correction_factor() — values rounded to 3 decimal places", {

  df <- tibble::tibble(
    num_days = c(1, 2, 3, 4, 5, 6)
  )

  out <- add_lppd_correction_factor(
    .dataset = df,
    num_days_collect_col = "num_days"
  )

  # Check all values are rounded to 3 decimal places
  for (i in 1:6) {
    expect_equal(
      out$lppd_correction_factor[i],
      round(i / 7, 3)
    )
  }
})

test_that("add_lppd_correction_factor() — values outside 0-7 return NA", {

  df <- tibble::tibble(
    num_days = c(-1, 0, 3, 7, 8, 10)
  )

  out <- add_lppd_correction_factor(
    .dataset = df,
    num_days_collect_col = "num_days"
  )

  expect_true(is.na(out$lppd_correction_factor[1]))   # -1
  expect_false(is.na(out$lppd_correction_factor[2]))  # 0
  expect_false(is.na(out$lppd_correction_factor[3]))  # 3
  expect_false(is.na(out$lppd_correction_factor[4]))  # 7
  expect_true(is.na(out$lppd_correction_factor[5]))   # 8
  expect_true(is.na(out$lppd_correction_factor[6]))   # 10
})

test_that("add_lppd_correction_factor() — NA values remain NA", {

  df <- tibble::tibble(
    num_days = c(3, NA, 5)
  )

  out <- add_lppd_correction_factor(
    .dataset = df,
    num_days_collect_col = "num_days"
  )

  expect_false(is.na(out$lppd_correction_factor[1]))
  expect_true(is.na(out$lppd_correction_factor[2]))
  expect_false(is.na(out$lppd_correction_factor[3]))
})

test_that("add_lppd_correction_factor() — error on empty dataset", {

  df_empty <- tibble::tibble(
    num_days = numeric(0)
  )

  expect_error(
    add_lppd_correction_factor(
      .dataset = df_empty,
      num_days_collect_col = "num_days"
    )
  )
})

test_that("add_lppd_correction_factor() — error on missing column", {

  df <- tibble::tibble(
    wrong_col = c(3, 5, 7)
  )

  expect_error(
    add_lppd_correction_factor(
      .dataset = df,
      num_days_collect_col = "num_days"
    )
  )
})

test_that("add_lppd_correction_factor() — warning when overwriting existing column", {

  df <- tibble::tibble(
    num_days = c(3, 5),
    lppd_correction_factor = c(99, 99)
  )

  expect_warning(
    add_lppd_correction_factor(
      .dataset = df,
      num_days_collect_col = "num_days"
    )
  )
})

test_that("add_lppd_correction_factor() — non-numeric values trigger warning", {

  df <- tibble::tibble(
    num_days = c(3, "five", 7)
  )

  expect_error(
    add_lppd_correction_factor(
      .dataset = df,
      num_days_collect_col = "num_days"
    )
  )
})

test_that("add_lppd_correction_factor() — boundary values at 0 and 7", {

  df <- tibble::tibble(
    num_days = c(0, 7)
  )

  out <- add_lppd_correction_factor(
    .dataset = df,
    num_days_collect_col = "num_days"
  )

  expect_equal(out$lppd_correction_factor[1], 0)
  expect_equal(out$lppd_correction_factor[2], 1)
})

# ADD_TOTAL_DAILY_LITERS Testing ####

test_that("add_total_daily_liters() — valid dataset without correction factor", {

  df <- tibble::tibble(
    container_size = c(10, 5, 15),
    num_journeys = c(2, 3, 4)
  )

  out <- add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "container_size",
    wash_container_num_journeys_col = "num_journeys"
  )

  expect_equal(nrow(out), 3)
  expect_true("wash_container_total_litres" %in% names(out))
})

test_that("add_total_daily_liters() — calculation without correction factor is correct", {

  df <- tibble::tibble(
    size = c(10, 20, 5),
    journeys = c(2, 3, 4)
  )

  out <- add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys"
  )

  # 10*2=20, 20*3=60, 5*4=20
  expect_equal(out$wash_container_total_litres[1], 20)
  expect_equal(out$wash_container_total_litres[2], 60)
  expect_equal(out$wash_container_total_litres[3], 20)
})

test_that("add_total_daily_liters() — valid dataset with correction factor", {

  df <- tibble::tibble(
    container_size = c(10, 5, 15),
    num_journeys = c(2, 3, 4),
    correction = c(1, 0.8, 0.5)
  )

  out <- add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "container_size",
    wash_container_num_journeys_col = "num_journeys",
    correction_factor_col = "correction"
  )

  expect_equal(nrow(out), 3)
  expect_true("wash_container_total_litres" %in% names(out))
})

test_that("add_total_daily_liters() — calculation with correction factor is correct", {

  df <- tibble::tibble(
    size = c(10, 20),
    journeys = c(2, 3),
    correction = c(1, 0.5)
  )

  out <- add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys",
    correction_factor_col = "correction"
  )

  # (10*2)*1=20, (20*3)*0.5=30
  expect_equal(out$wash_container_total_litres[1], 20)
  expect_equal(out$wash_container_total_litres[2], 30)
})

test_that("add_total_daily_liters() — NA in size or journeys returns NA", {

  df <- tibble::tibble(
    size = c(10, NA, 15),
    journeys = c(2, 3, NA)
  )

  out <- add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys"
  )

  expect_false(is.na(out$wash_container_total_litres[1]))
  expect_true(is.na(out$wash_container_total_litres[2]))
  expect_true(is.na(out$wash_container_total_litres[3]))
})

test_that("add_total_daily_liters() — NA in correction factor preserves base calculation", {

  df <- tibble::tibble(
    size = c(10, 20),
    journeys = c(2, 3),
    correction = c(0.5, NA)
  )

  out <- add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys",
    correction_factor_col = "correction"
  )

  # (10*2)*0.5=10, (20*3)*NA=60 (no correction applied)
  expect_equal(out$wash_container_total_litres[1], 10)
  expect_equal(out$wash_container_total_litres[2], 60)
})

test_that("add_total_daily_liters() — error on empty dataset", {

  df_empty <- tibble::tibble(
    size = numeric(0),
    journeys = numeric(0)
  )

  expect_error(
    add_total_daily_liters(
      .dataset = df_empty,
      wash_container_size_liters_col = "size",
      wash_container_num_journeys_col = "journeys"
    )
  )
})

test_that("add_total_daily_liters() — error on missing required columns", {

  df <- tibble::tibble(
    size = c(10, 20)
  )

  expect_error(
    add_total_daily_liters(
      .dataset = df,
      wash_container_size_liters_col = "size",
      wash_container_num_journeys_col = "journeys"
    )
  )
})

test_that("add_total_daily_liters() — error when correction factor column missing", {

  df <- tibble::tibble(
    size = c(10, 20),
    journeys = c(2, 3)
  )

  expect_error(
    add_total_daily_liters(
      .dataset = df,
      wash_container_size_liters_col = "size",
      wash_container_num_journeys_col = "journeys",
      correction_factor_col = "correction"
    )
  )
})

test_that("add_total_daily_liters() — warning when overwriting existing column", {

  df <- tibble::tibble(
    size = c(10),
    journeys = c(2),
    wash_container_total_litres = 99
  )

  expect_warning(
    add_total_daily_liters(
      .dataset = df,
      wash_container_size_liters_col = "size",
      wash_container_num_journeys_col = "journeys"
    )
  )
})

test_that("add_total_daily_liters() — non-numeric values trigger warning", {

  df <- tibble::tibble(
    size = c(10, "twenty", 30),
    journeys = c(2, 3, 4)
  )

  expect_error(
    add_total_daily_liters(
      .dataset = df,
      wash_container_size_liters_col = "size",
      wash_container_num_journeys_col = "journeys"
    )
  )
})

test_that("add_total_daily_liters() — zero values handled correctly", {

  df <- tibble::tibble(
    size = c(10, 0, 20),
    journeys = c(0, 5, 2)
  )

  out <- add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys"
  )

  # 10*0=0, 0*5=0, 20*2=40
  expect_equal(out$wash_container_total_litres[1], 0)
  expect_equal(out$wash_container_total_litres[2], 0)
  expect_equal(out$wash_container_total_litres[3], 40)
})

test_that("add_total_daily_liters() — correction factor of 0 works correctly", {

  df <- tibble::tibble(
    size = c(10, 20),
    journeys = c(2, 3),
    correction = c(0, 1)
  )

  out <- add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys",
    correction_factor_col = "correction"
  )

  # (10*2)*0=0, (20*3)*1=60
  expect_equal(out$wash_container_total_litres[1], 0)
  expect_equal(out$wash_container_total_litres[2], 60)
})

test_that("add_total_daily_liters() — large values handled correctly", {

  df <- tibble::tibble(
    size = c(100, 500),
    journeys = c(10, 20),
    correction = c(1.5, 2)
  )

  out <- add_total_daily_liters(
    .dataset = df,
    wash_container_size_liters_col = "size",
    wash_container_num_journeys_col = "journeys",
    correction_factor_col = "correction"
  )

  # (100*10)*1.5=1500, (500*20)*2=20000
  expect_equal(out$wash_container_total_litres[1], 1500)
  expect_equal(out$wash_container_total_litres[2], 20000)
})

