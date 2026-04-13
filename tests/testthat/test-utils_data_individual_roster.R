# ADD_AGE_CAT Testing ####

test_that("add_age_cat() — valid dataset creates age categories", {

  df <- tibble::tibble(
    age_years = c(0, 3, 7, 10, 45, 87, 122)
  )

  out <- add_age_cat(
    .dataset = df,
    age_years_col = "age_years"
  )

  expect_equal(nrow(out), 7)
  expect_true("age_cat" %in% names(out))
  expect_s3_class(out$age_cat, "factor")

  # Check specific categories
  expect_equal(as.character(out$age_cat[1]), "0-4")
  expect_equal(as.character(out$age_cat[3]), "5-9")
  expect_equal(as.character(out$age_cat[5]), "45-49")
})

test_that("add_age_cat() — NA values are handled correctly", {

  df <- tibble::tibble(
    age_years = c(5, 10, NA, 25)
  )

  out <- add_age_cat(
    .dataset = df,
    age_years_col = "age_years"
  )

  expect_equal(nrow(out), 4)
  expect_true(is.na(out$age_cat[3]))
  expect_false(is.na(out$age_cat[1]))
})

test_that("add_age_cat() — error on empty dataset", {

  df_empty <- tibble::tibble(
    age_years = numeric(0)
  )

  expect_error(
    add_age_cat(
      .dataset = df_empty,
      age_years_col = "age_years"
    )
  )
})

test_that("add_age_cat() — error on missing column", {

  df <- tibble::tibble(
    wrong_col = c(5, 10, 15)
  )

  expect_error(
    add_age_cat(
      .dataset = df,
      age_years_col = "age_years"
    )
  )
})

test_that("add_age_cat() — warning when overwriting existing column", {

  df <- tibble::tibble(
    age_years = c(5, 10, 15),
    age_cat = c("old", "old", "old")
  )

  expect_warning(
    add_age_cat(
      .dataset = df,
      age_years_col = "age_years"
    )
  )
})

test_that("add_age_cat() — non-numeric values trigger warning", {

  df <- tibble::tibble(
    age_years = c(5, 10, "fifteen", 20)
  )

  expect_error(
    a <- add_age_cat(
      .dataset = df,
      age_years_col = "age_years"
    )
  )
})

test_that("add_age_cat() — boundary values are categorized correctly", {

  df <- tibble::tibble(
    age_years = c(0, 4, 5, 9, 10, 14, 15, 124)
  )

  out <- add_age_cat(
    .dataset = df,
    age_years_col = "age_years"
  )

  expect_equal(as.character(out$age_cat[1]), "0-4")
  expect_equal(as.character(out$age_cat[2]), "0-4")
  expect_equal(as.character(out$age_cat[3]), "5-9")
  expect_equal(as.character(out$age_cat[4]), "5-9")
  expect_equal(as.character(out$age_cat[5]), "10-14")
  expect_equal(as.character(out$age_cat[8]), "120-124")
})

# ADD_AGE_MONTHS_CAT Testing ####

test_that("add_age_months_cat() — valid dataset creates age month categories", {

  df <- tibble::tibble(
    age_months = c(0, 5, 7, 12, 23, 40, 59)
  )

  out <- add_age_months_cat(
    .dataset = df,
    age_months_col = "age_months"
  )

  expect_equal(nrow(out), 7)
  expect_true("age_months_cat" %in% names(out))
  expect_s3_class(out$age_months_cat, "factor")

  # Check specific categories
  expect_equal(as.character(out$age_months_cat[1]), "0-5 months")
  expect_equal(as.character(out$age_months_cat[3]), "6-11 months")
  expect_equal(as.character(out$age_months_cat[4]), "12-17 months")

  # Check roster_age_6_29m and roster_age_30_59m columns exist
  expect_true("roster_age_6_29m" %in% names(out))
  expect_true("roster_age_30_59m" %in% names(out))
})

test_that("add_age_months_cat() — roster_age_6_29m and roster_age_30_59m values are correct", {

  df <- tibble::tibble(
    age_months = c(0, 5, 6, 29, 30, 59, 60)
  )

  out <- add_age_months_cat(
    .dataset = df,
    age_months_col = "age_months"
  )

  # Under 6 months → NA for both columns
  expect_true(is.na(out$roster_age_6_29m[1]))
  expect_true(is.na(out$roster_age_6_29m[2]))
  expect_true(is.na(out$roster_age_30_59m[1]))
  expect_true(is.na(out$roster_age_30_59m[2]))

  # 6-29 months → roster_age_6_29m = 1, roster_age_30_59m = 0
  expect_equal(out$roster_age_6_29m[3], 1)
  expect_equal(out$roster_age_6_29m[4], 1)
  expect_equal(out$roster_age_30_59m[3], 0)
  expect_equal(out$roster_age_30_59m[4], 0)

  # 30-59 months → roster_age_6_29m = 0, roster_age_30_59m = 1
  expect_equal(out$roster_age_6_29m[5], 0)
  expect_equal(out$roster_age_6_29m[6], 0)
  expect_equal(out$roster_age_30_59m[5], 1)
  expect_equal(out$roster_age_30_59m[6], 1)

  # 60+ months → NA for both columns
  expect_true(is.na(out$roster_age_6_29m[7]))
  expect_true(is.na(out$roster_age_30_59m[7]))
})

test_that("add_age_months_cat() — NA values are handled correctly", {

  df <- tibble::tibble(
    age_months = c(5, 10, NA, 25)
  )

  out <- add_age_months_cat(
    .dataset = df,
    age_months_col = "age_months"
  )

  expect_equal(nrow(out), 4)
  expect_true(is.na(out$age_months_cat[3]))
  expect_false(is.na(out$age_months_cat[1]))
})

test_that("add_age_months_cat() — error on empty dataset", {

  df_empty <- tibble::tibble(
    age_months = numeric(0)
  )

  expect_error(
    add_age_months_cat(
      .dataset = df_empty,
      age_months_col = "age_months"
    )
  )
})

test_that("add_age_months_cat() — error on missing column", {

  df <- tibble::tibble(
    wrong_col = c(5, 10, 15)
  )

  expect_error(
    add_age_months_cat(
      .dataset = df,
      age_months_col = "age_months"
    )
  )
})

test_that("add_age_months_cat() — warning when overwriting existing column", {

  df <- tibble::tibble(
    age_months = c(5, 10, 15),
    age_months_cat = c("old", "old", "old")
  )

  expect_warning(
    add_age_months_cat(
      .dataset = df,
      age_months_col = "age_months"
    )
  )
})

test_that("add_age_months_cat() — boundary values are categorized correctly", {

  df <- tibble::tibble(
    age_months = c(0, 5, 6, 11, 12, 17, 54, 59)
  )

  out <- add_age_months_cat(
    .dataset = df,
    age_months_col = "age_months"
  )

  expect_equal(as.character(out$age_months_cat[1]), "0-5 months")
  expect_equal(as.character(out$age_months_cat[2]), "0-5 months")
  expect_equal(as.character(out$age_months_cat[3]), "6-11 months")
  expect_equal(as.character(out$age_months_cat[4]), "6-11 months")
  expect_equal(as.character(out$age_months_cat[5]), "12-17 months")
  expect_equal(as.character(out$age_months_cat[7]), "54-59 months")
  expect_equal(as.character(out$age_months_cat[8]), "54-59 months")
})

test_that("add_age_months_cat() — values outside range return NA", {

  df <- tibble::tibble(
    age_months = c(60, 70, 100)
  )

  out <- add_age_months_cat(
    .dataset = df,
    age_months_col = "age_months"
  )

  expect_true(all(is.na(out$age_months_cat)))
})

# add_standardized_age ####

test_that("add_standardized_age handles basic input with only age_years", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30)
  )

  result <- add_standardized_age(
    .dataset = df,
    age_years_col = "age_years"
  )

  expect_equal(result$calc_age_years, df$age_years)  # calc_age_years should match age_years_col

})

test_that("add_standardized_age handles age_years with age_months_col", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    age_months = c(120, 240, 360)
  )

  result <- add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    age_months_col = "age_months"
  )

  expect_equal(result$calc_age_years, df$age_years)  # calc_age_years should match
  expect_equal(result$calc_age_months, df$age_months) # calc_age_months should match
  expect_true(all(abs(result$calc_age_days - df$age_months*30.44) < 1, na.rm = TRUE))
})

test_that("add_standardized_age computes calc_date_birth_final using exact birth first", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    date_birth_exact = as.Date(c("2013-01-01", "2003-01-01", NA)),
    date_birth_approx = as.Date(c(NA, "2003-06-01", "1993-01-01"))
  )

  result <- add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = "date_birth_exact",
    date_birth_approx_col = "date_birth_approx"
  )

  expect_equal(result$calc_date_birth_final, as.Date(c("2013-01-01", "2003-01-01", "1993-01-01")))
})

test_that("add_standardized_age computes calc_date_death_final using exact death first", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    date_death_exact = as.Date(c("2023-01-01", NA, "2023-01-01")),
    date_death_approx = as.Date(c(NA, "2023-06-01", "2023-12-01"))
  )

  result <- add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_death_exact_col = "date_death_exact",
    date_death_approx_col = "date_death_approx"
  )

  expect_equal(result$calc_date_death_final, as.Date(c("2023-01-01", "2023-06-01", "2023-01-01")))
})


test_that("add_standardized_age calculates age correctly from survey date", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    date_birth_exact = as.Date(c("2013-01-01", "2003-01-01", "1993-01-01")),
    survey_date = as.Date("2023-01-01")
  )

  result <- add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = "date_birth_exact",
    survey_date_col = "survey_date"
  )

  expect_equal(result$calc_age_years, c(10, 20, 30))
  expect_equal(result$calc_age_months, c(120, 240, 360))
  expect_equal(result$calc_age_days, c(3652, 7305, 10957))  # Approximate days in 10, 20, 30 years.
})

test_that("add_standardized_age does not calculate ages when no age_years values are provided", {
  df <- tibble::tibble(
    age_years = c(NA, NA, NA),
    date_birth_exact = as.Date(c("2013-01-01", "2003-01-01", "1993-01-01")),
    date_death_exact = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01"))
  )

  result <- add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = "date_birth_exact",
    date_death_exact_col = "date_death_exact"
  )

  expect_message(
    result <- add_standardized_age(
      .dataset = df,
      age_years_col = "age_years",
      date_birth_exact_col = "date_birth_exact",
      date_death_exact_col = "date_death_exact"
    )
  )

})

test_that("add_standardized_age handles missing age_months_col gracefully", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30)
  )

  result <- add_standardized_age(
    .dataset = df,
    age_years_col = "age_years"
  )

  expect_equal(result$calc_age_years, df$age_years)  # calc_age_years should match age_years_col
  expect_true(all(is.na(result$calc_age_months)))    # calc_age_months should default to NA
  expect_true(all(is.na(result$calc_age_days)))      # calc_age_days should default to NA
})

test_that("add_standardized_age handles no date_of_birth or date_of_death inputs", {
  df <- tibble::tibble(
    age_years = c(10, 15, 20)
  )

  result <- add_standardized_age(
    .dataset = df,
    age_years_col = "age_years"
  )

  expect_equal(result$calc_age_years, df$age_years)  # calc_age_years should match age_years_col
  # calc_date_birth_final and calc_date_death_final should not be created if no date columns provided
  expect_false("calc_date_birth_final" %in% names(result))
  expect_false("calc_date_death_final" %in% names(result))
})

test_that("add_standardized_age returns error for missing age_years_col", {
  df <- tibble::tibble(
    age_months = c(120, 180, 240)
  )

  expect_error(
    add_standardized_age(
      .dataset = df,
      age_months_col = "age_months"
    ),
    regexp = "add_standardized_age: argument \"age_years_col\" is missing, with no default",
    fixed = TRUE
  )
})

test_that("add_standardized_age gracefully handles empty dataset", {
  df <- tibble::tibble()

  expect_error(
    add_standardized_age(
      .dataset = df,
      age_years_col = "age_years"
    ),
    "Dataset is empty."
  )
})

test_that("add_standardized_age warns about overwriting columns", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    calc_age_years = c(1, 1, 1)
  )

  expect_warning(
    add_standardized_age(
      .dataset = df,
      age_years_col = "age_years"
    ),
    "Variable calc_age_years already exists and will be overwritten."
  )
})

# Defensive coding tests for optional columns
test_that("add_standardized_age handles NULL date columns without error", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30)
  )

  # Test with NULL date_birth columns
  result <- add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = NULL,
    date_birth_approx_col = NULL,
    date_birth_final_col = NULL
  )

  # Columns should not be created if no date columns are provided
  expect_false("calc_date_birth_final" %in% names(result))
  expect_equal(result$calc_age_years, df$age_years)
})

test_that("add_standardized_age handles non-existent date columns gracefully", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30)
  )

  # Test with columns that don't exist in the dataset
  # This should not error even though columns don't exist
  result <- add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = "nonexistent_col",
    date_death_exact_col = "another_nonexistent_col"
  )

  # The function should not create calc columns when columns don't exist
  expect_false("calc_date_birth_final" %in% names(result))
  expect_false("calc_date_death_final" %in% names(result))
  expect_equal(result$calc_age_years, df$age_years)
})

test_that("add_standardized_age uses only existing date columns in coalesce", {
  df <- tibble::tibble(
    age_years = c(10, 20, 30),
    date_birth_approx = as.Date(c("2013-01-01", "2003-01-01", "1993-01-01"))
  )

  # Pass both existing and non-existing columns
  result <- add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = "nonexistent_exact",  # doesn't exist
    date_birth_approx_col = "date_birth_approx", # exists
    date_birth_final_col = "nonexistent_final"   # doesn't exist
  )

  # Should use the only existing column (date_birth_approx)
  expect_equal(result$calc_date_birth_final, as.Date(c("2013-01-01", "2003-01-01", "1993-01-01")))
})

test_that("add_standardized_age — calc_date_birth_final is NA when all birth date columns are NA", {
  # Regression test: death date columns exist and have values, but birth date columns are all NA.
  # calc_date_birth_final should remain NA for all records, NOT pick up values from death date columns.
  df <- tibble::tibble(
    age_years = c(5, 10, 30),
    dob_exact = as.Date(c(NA, NA, NA)),
    dob_approx = as.Date(c(NA, NA, NA)),
    date_death_exact = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    date_death_approx = as.Date(c(NA, "2023-02-15", NA))
  )

  result <- add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_birth_exact_col = "dob_exact",
    date_birth_approx_col = "dob_approx",
    date_death_exact_col = "date_death_exact",
    date_death_approx_col = "date_death_approx"
  )

  # calc_date_birth_final should be NA for all rows — death dates must not bleed into birth dates
  expect_true("calc_date_birth_final" %in% names(result))
  expect_true(all(is.na(result$calc_date_birth_final)))
})

test_that("add_standardized_age — calc_date_death_final uses exact death date when approx is NA", {
  # Regression test: when date_death_exact_col and date_death_approx_col point to different columns,
  # calc_date_death_final should prefer the exact date and fall back to approx only when exact is NA.
  df <- tibble::tibble(
    age_years = c(5, 10, 30),
    date_death_exact = as.Date(c("2023-01-01", NA, "2023-03-01")),
    date_death_approx = as.Date(c(NA, "2023-02-15", "2023-03-20"))
  )

  result <- add_standardized_age(
    .dataset = df,
    age_years_col = "age_years",
    date_death_exact_col = "date_death_exact",
    date_death_approx_col = "date_death_approx"
  )

  # Row 1: exact date available — use it
  expect_equal(result$calc_date_death_final[1], as.Date("2023-01-01"))
  # Row 2: only approx available — use approx
  expect_equal(result$calc_date_death_final[2], as.Date("2023-02-15"))
  # Row 3: both available — prefer exact
  expect_equal(result$calc_date_death_final[3], as.Date("2023-03-01"))
})


# add_standardized_roster_demographics ####

test_that("add_standardized_roster_demographics creates canonical columns", {

  df <- tibble::tibble(
    person_id = 1:6,
    calc_age_years = c(1, 4, 8, 25, 40, 18),
    sex = c("M", "F", "M", "F", "F", "F")
  )

  result <- add_standardized_roster_demographics(
    .dataset = df,
    age_years_col = "calc_age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F"
  )

  # Check all canonical columns exist
  expect_true("roster_child_under2" %in% names(result))
  expect_true("roster_child_under5" %in% names(result))
  expect_true("roster_2to5" %in% names(result))
  expect_true("roster_5plus" %in% names(result))
  expect_true("roster_5_10" %in% names(result))
  expect_true("roster_male" %in% names(result))
  expect_true("roster_female" %in% names(result))
  expect_true("roster_woman_15to49" %in% names(result))

  # Check values
  expect_equal(result$roster_child_under2, c(1, 0, 0, 0, 0, 0))
  expect_equal(result$roster_child_under5, c(1, 1, 0, 0, 0, 0))
  expect_equal(result$roster_2to5, c(0, 1, 0, 0, 0, 0))
  expect_equal(result$roster_5plus, c(0, 0, 1, 1, 1, 1))
  expect_equal(result$roster_5_10, c(0, 0, 1, NA, NA, NA))
  expect_equal(result$roster_male, c(1, 0, 1, 0, 0, 0))
  expect_equal(result$roster_female, c(0, 1, 0, 1, 1, 1))
  expect_equal(result$roster_woman_15to49, c(0, 0, 0, 1, 1, 1))
})

test_that("add_standardized_roster_demographics handles missing sex column", {

  df <- tibble::tibble(
    person_id = 1:3,
    calc_age_years = c(1, 4, 8)
  )

  expect_warning(
    result <- add_standardized_roster_demographics(
      .dataset = df,
      age_years_col = "calc_age_years"
    ),
    "Sex column not provided"
  )

  # Sex-based columns should be 0
  expect_equal(result$roster_male, c(0, 0, 0))
  expect_equal(result$roster_female, c(0, 0, 0))
  expect_equal(result$roster_woman_15to49, c(0, 0, 0))

  # Age-based columns should still work
  expect_equal(result$roster_child_under2, c(1, 0, 0))
  expect_equal(result$roster_child_under5, c(1, 1, 0))
  expect_equal(result$roster_2to5, c(0, 1, 0))
  expect_equal(result$roster_5plus, c(0, 0, 1))
  expect_equal(result$roster_5_10, c(0, 0, 1))
})

test_that("add_standardized_roster_demographics handles NA values", {

  df <- tibble::tibble(
    person_id = 1:4,
    calc_age_years = c(1, NA, 8, 25),
    sex = c("M", "F", NA, "F")
  )

  result <- add_standardized_roster_demographics(
    .dataset = df,
    age_years_col = "calc_age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F"
  )

  # NA age should result in 0 for age-based indicators
  expect_equal(result$roster_child_under2, c(1, 0, 0, 0))
  expect_equal(result$roster_child_under5, c(1, 0, 0, 0))
  expect_equal(result$roster_2to5, c(0, 0, 0, 0))
  expect_equal(result$roster_5plus, c(0, 0, 1, 1))
  # NA age → NA for roster_5_10; age >= 10 → NA
  expect_equal(result$roster_5_10, c(0, NA, NA, NA))

  # NA sex should result in 0 for sex-based indicators
  expect_equal(result$roster_male, c(1, 0, 0, 0))
  expect_equal(result$roster_female, c(0, 1, 0, 1))
})

test_that("add_standardized_roster_demographics roster_2to5 boundary values", {

  df <- tibble::tibble(
    person_id = 1:5,
    calc_age_years = c(1.9, 2, 4.9, 5, 6)
  )

  result <- add_standardized_roster_demographics(
    .dataset = df,
    age_years_col = "calc_age_years"
  )

  expect_equal(result$roster_2to5, c(0, 1, 1, 0, 0))
  expect_equal(result$roster_5plus, c(0, 0, 0, 1, 1))
})

test_that("add_standardized_roster_demographics roster_5_10 boundary values", {

  df <- tibble::tibble(
    person_id = 1:6,
    calc_age_years = c(4.9, 5, 7, 9.9, 10, 25)
  )

  result <- add_standardized_roster_demographics(
    .dataset = df,
    age_years_col = "calc_age_years"
  )

  expect_equal(result$roster_5_10, c(0, 1, 1, 1, NA, NA))
})

test_that("add_standardized_roster_demographics error on missing age column", {

  df <- tibble::tibble(
    person_id = 1:3,
    sex = c("M", "F", "M")
  )

  expect_error(
    add_standardized_roster_demographics(
      .dataset = df,
      age_years_col = "calc_age_years",
      sex_col = "sex"
    ),
    "calc_age_years"
  )
})

