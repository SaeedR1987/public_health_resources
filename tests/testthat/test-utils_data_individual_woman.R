# ADD_MATERNAL_MUAC Testing ####

test_that("add_maternal_muac() — valid dataset with cm values works", {

  df <- tibble::tibble(
    age_years = c(25, 18, 30, 14),
    muac = c(17, 25, 19, 26)
  )

  out <- add_maternal_muac(
    .dataset = df,
    age_years_col = "age_years",
    muac_col = "muac"
  )

  expect_equal(nrow(out), 4)
  expect_true("woman_muac_cat" %in% names(out))
  expect_true("flag_woman_muac_extreme" %in% names(out))
  expect_true("woman_muac_mm" %in% names(out))
})

test_that("add_maternal_muac() — categorization thresholds work correctly", {

  df <- tibble::tibble(
    age_years = c(20, 25, 30, 35),
    muac = c(20, 22, 24, 25)
  )

  out <- add_maternal_muac(
    .dataset = df,
    age_years_col = "age_years",
    muac_col = "muac",
    severe_threshold_const = 21,
    moderate_threshold_const = 23
  )

  expect_equal(out$woman_muac_cat[1], "Severe")    # < 21
  expect_equal(out$woman_muac_cat[2], "Moderate")  # >= 21 & < 23
  expect_equal(out$woman_muac_cat[3], "Normal")    # >= 23
  expect_equal(out$woman_muac_cat[4], "Normal")    # >= 23
})

test_that("add_maternal_muac() — women outside 15-49 age range get NA", {

  df <- tibble::tibble(
    age_years = c(14, 20, 50, 55),
    muac = c(22, 22, 22, 22)
  )

  out <- add_maternal_muac(
    .dataset = df,
    age_years_col = "age_years",
    muac_col = "muac"
  )

  expect_true(is.na(out$woman_muac_cat[1]))  # age 14
  expect_false(is.na(out$woman_muac_cat[2])) # age 20
  expect_true(is.na(out$woman_muac_cat[3]))  # age 50
  expect_true(is.na(out$woman_muac_cat[4]))  # age 55
})

test_that("add_maternal_muac() — detects cm and converts to mm", {

  df <- tibble::tibble(
    age_years = c(25, 30),
    muac = c(22, 24)  # in cm
  )

  out <- add_maternal_muac(
    .dataset = df,
    age_years_col = "age_years",
    muac_col = "muac"
  )

  expect_true("woman_muac_mm" %in% names(out))
  expect_equal(out$woman_muac_mm[1], 220)
  expect_equal(out$woman_muac_mm[2], 240)
})

test_that("add_maternal_muac() — detects mm and converts to cm", {

  df <- tibble::tibble(
    age_years = c(25, 30),
    muac = c(220, 240)  # in mm
  )

  out <- add_maternal_muac(
    .dataset = df,
    age_years_col = "age_years",
    muac_col = "muac"
  )

  expect_true("woman_muac_cm" %in% names(out))
  expect_equal(out$woman_muac_cm[1], 22)
  expect_equal(out$woman_muac_cm[2], 24)
})

test_that("add_maternal_muac() — flags extreme values correctly", {

  df <- tibble::tibble(
    age_years = c(25, 30, 35, 40),
    muac = c(9, 22, 70, 75)
  )

  out <- add_maternal_muac(
    .dataset = df,
    age_years_col = "age_years",
    muac_col = "muac"
  )

  expect_equal(out$flag_woman_muac_extreme[1], 1)  # < 10
  expect_equal(out$flag_woman_muac_extreme[2], 0)  # normal
  expect_equal(out$flag_woman_muac_extreme[3], 1)  # >= 70
  expect_equal(out$flag_woman_muac_extreme[4], 1)  # >= 70
})

test_that("add_maternal_muac() — error on empty dataset", {

  df_empty <- tibble::tibble(
    age_years = numeric(0),
    muac = numeric(0)
  )

  expect_error(
    add_maternal_muac(
      .dataset = df_empty,
      age_years_col = "age_years",
      muac_col = "muac"
    )
  )
})

test_that("add_maternal_muac() — error on missing columns", {

  df <- tibble::tibble(
    age_years = c(25, 30)
  )

  expect_error(
    add_maternal_muac(
      .dataset = df,
      age_years_col = "age_years",
      muac_col = "muac"
    )
  )
})

test_that("add_maternal_muac() — warning when overwriting existing columns", {

  df <- tibble::tibble(
    age_years = c(25, 30),
    muac = c(22, 24),
    woman_muac_cat = c("old", "old")
  )

  expect_warning(
    add_maternal_muac(
      .dataset = df,
      age_years_col = "age_years",
      muac_col = "muac"
    )
  )
})

test_that("add_maternal_muac() — NA values in input columns", {

  df <- tibble::tibble(
    age_years = c(25, NA, 30),
    muac = c(22, 24, NA)
  )

  out <- add_maternal_muac(
    .dataset = df,
    age_years_col = "age_years",
    muac_col = "muac"
  )

  expect_equal(nrow(out), 3)

  expect_true(is.na(out$woman_muac_cat[3]))
})

test_that("add_maternal_muac() — non-numeric values trigger warning", {

  df <- tibble::tibble(
    age_years = c(25, "thirty", 35),
    muac = c(22, 24, 26)
  )

  expect_warning(
    add_maternal_muac(
      .dataset = df,
      age_years_col = "age_years",
      muac_col = "muac"
    )
  )
})

# Defensive coding test for conditional variable reference
test_that("add_maternal_muac() handles mixed unit detection correctly", {
  # Test with millimeter values
  df_mm <- tibble::tibble(
    age_years = c(25, 30, 35),
    muac = c(210, 240, 250)  # All > 100, should be detected as mm
  )

  result_mm <- add_maternal_muac(
    .dataset = df_mm,
    age_years_col = "age_years",
    muac_col = "muac"
  )

  # Should create woman_muac_cm column and use it for classification
  expect_true("woman_muac_cm" %in% names(result_mm))
  expect_true("woman_muac_cat" %in% names(result_mm))

  # Test with centimeter values
  df_cm <- tibble::tibble(
    age_years = c(25, 30, 35),
    muac = c(21, 24, 25)  # All < 100, should be detected as cm
  )

  result_cm <- add_maternal_muac(
    .dataset = df_cm,
    age_years_col = "age_years",
    muac_col = "muac"
  )

  # Should create woman_muac_mm column and use muac directly for classification
  expect_true("woman_muac_mm" %in% names(result_cm))
  expect_true("woman_muac_cat" %in% names(result_cm))
})

test_that("add_maternal_muac() handles pre-existing converted columns", {
  # Test with pre-existing woman_muac_cm column
  df <- tibble::tibble(
    age_years = c(25, 30, 35),
    muac = c(210, 240, 250),  # mm values
    woman_muac_cm = c(20, 23, 24)  # pre-existing cm column
  )

  expect_warning(
    result <- add_maternal_muac(
      .dataset = df,
      age_years_col = "age_years",
      muac_col = "muac"
    ),
    "woman_muac_cm.*already exists"
  )

  # Should still work and overwrite the existing column
  expect_true("woman_muac_cm" %in% names(result))
  expect_true("woman_muac_cat" %in% names(result))
})
