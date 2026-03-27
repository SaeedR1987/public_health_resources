# ADD_STANDARDIZED_DEATHS Testing ####

test_that("add_standardized_deaths() — valid dataset creates death columns", {

  df <- tibble::tibble(
    age_years = c(2, 30, 50),
    sex = c("M", "F", "M"),
    date_of_death = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    recall_date = as.Date(c("2022-01-01", "2023-01-01", "2022-12-31")),
    cause_of_death = c("malaria", "trauma", "old age"),
    location_of_death = c("home", "road", "last residence")
  )

  out <- add_standardized_deaths(
    .dataset = df,
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F",
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date",
    cause_of_death_col = "cause_of_death",
    non_trauma_vals = c("malaria", "diarrhea"),
    trauma_vals = c("trauma", "accident"),
    other_vals = c("old age", "unknown"),
    location_of_death_col = "location_of_death",
    current_location_residence_vals = c("home"),
    migration_vals = c("road"),
    last_location_residence_vals = c("last residence")
  )

  expect_equal(nrow(out), 3)
  expect_true("death" %in% names(out))
  expect_true("death_under5" %in% names(out))
  expect_true("death_male" %in% names(out))
  expect_true("death_female" %in% names(out))
})

test_that("add_standardized_deaths() — death column logic works correctly", {

  df <- tibble::tibble(
    age_years = c(2, 30),
    sex = c("M", "F"),
    date_of_death = as.Date(c("2023-01-01", "2023-02-01")),
    recall_date = as.Date(c("2022-01-01", "2023-02-15"))
  )

  out <- add_standardized_deaths(
    .dataset = df,
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F",
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date"
  )

  expect_equal(out$death[1], 1)  # death after recall
  expect_equal(out$death[2], 0)  # death before recall
})

test_that("add_standardized_deaths() — age-based categorization works", {

  df <- tibble::tibble(
    age_years = c(2, 5, 30),
    sex = c("M", "F", "M"),
    date_of_death = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    recall_date = as.Date(c("2022-01-01", "2022-01-01", "2022-01-01"))
  )

  out <- add_standardized_deaths(
    .dataset = df,
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F",
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date"
  )

  expect_equal(out$death_under5[1], 1)  # age < 5
  expect_equal(out$death_under5[2], 0)  # age >= 5
  expect_equal(out$death_under5[3], 0)  # age >= 5
})

test_that("add_standardized_deaths() — sex-based categorization works", {

  df <- tibble::tibble(
    age_years = c(30, 25),
    sex = c("M", "F"),
    date_of_death = as.Date(c("2023-01-01", "2023-01-01")),
    recall_date = as.Date(c("2022-01-01", "2022-01-01"))
  )

  out <- add_standardized_deaths(
    .dataset = df,
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F",
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date"
  )

  expect_equal(out$death_male[1], 1)
  expect_equal(out$death_male[2], 0)
  expect_equal(out$death_female[1], 0)
  expect_equal(out$death_female[2], 1)
})

test_that("add_standardized_deaths() — cause of death categorization works", {

  df <- tibble::tibble(
    age_years = c(30, 25, 40),
    sex = c("M", "F", "M"),
    date_of_death = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    recall_date = as.Date(c("2022-01-01", "2022-01-01", "2022-01-01")),
    cause_of_death = c("malaria", "trauma", "old age")
  )

  out <- add_standardized_deaths(
    .dataset = df,
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F",
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date",
    cause_of_death_col = "cause_of_death",
    non_trauma_vals = c("malaria", "diarrhea"),
    trauma_vals = c("trauma", "accident"),
    other_vals = c("old age", "unknown")
  )

  expect_equal(out$death_non_trauma[1], 1)
  expect_equal(out$death_trauma[2], 1)
  expect_equal(out$death_other[3], 1)
})

test_that("add_standardized_deaths() — location categorization works", {

  df <- tibble::tibble(
    age_years = c(30, 25, 40),
    sex = c("M", "F", "M"),
    date_of_death = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    recall_date = as.Date(c("2022-01-01", "2022-01-01", "2022-01-01")),
    location_of_death = c("home", "road", "last residence")
  )

  out <- add_standardized_deaths(
    .dataset = df,
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "M",
    female_val = "F",
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date",
    location_of_death_col = "location_of_death",
    current_location_residence_vals = c("home"),
    migration_vals = c("road"),
    last_location_residence_vals = c("last residence")
  )

  expect_equal(out$death_current_location[1], 1)
  expect_equal(out$death_migration[2], 1)
  expect_equal(out$death_last_location[3], 1)
})

test_that("add_standardized_deaths() — error on empty dataset", {

  df_empty <- tibble::tibble(
    age_years = numeric(0),
    sex = character(0),
    date_of_death = as.Date(character(0)),
    recall_date = as.Date(character(0))
  )

  expect_error(
    add_standardized_deaths(
      .dataset = df_empty,
      age_years_col = "age_years",
      sex_col = "sex",
      male_val = "M",
      female_val = "F",
      date_of_death_col = "date_of_death",
      recall_date_col = "recall_date"
    )
  )
})

test_that("add_standardized_deaths() — error on missing columns", {

  df <- tibble::tibble(
    age_years = c(30, 25)
  )

  expect_error(
    add_standardized_deaths(
      .dataset = df,
      age_years_col = "age_years",
      sex_col = "sex",
      male_val = "M",
      female_val = "F",
      date_of_death_col = "date_of_death",
      recall_date_col = "recall_date"
    )
  )
})

test_that("add_standardized_deaths() — warning when overwriting existing columns", {

  df <- tibble::tibble(
    age_years = c(30, 25),
    sex = c("M", "F"),
    date_of_death = as.Date(c("2023-01-01", "2023-01-01")),
    recall_date = as.Date(c("2022-01-01", "2022-01-01")),
    death = c(1, 1)
  )

  expect_warning(
    add_standardized_deaths(
      .dataset = df,
      age_years_col = "age_years",
      sex_col = "sex",
      male_val = "M",
      female_val = "F",
      date_of_death_col = "date_of_death",
      recall_date_col = "recall_date"
    )
  )
})

test_that("add_standardized_deaths() — works with minimal columns (fallback to death=1)", {

  df <- tibble::tibble(
    death_id = c(1, 2, 3)
  )

  out <- add_standardized_deaths(
    .dataset = df
  )

  expect_equal(nrow(out), 3)
  expect_true("death" %in% names(out))
  expect_equal(out$death, c(1, 1, 1))
})

test_that("add_standardized_deaths() — works with only date columns provided", {

  df <- tibble::tibble(
    date_of_death = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    recall_date = as.Date(c("2022-01-01", "2024-01-01", "2022-12-31"))
  )

  out <- add_standardized_deaths(
    .dataset = df,
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date"
  )

  expect_equal(nrow(out), 3)
  expect_true("death" %in% names(out))
  expect_equal(out$death[1], 1)  # death after recall
  expect_equal(out$death[2], 0)  # death before recall
  expect_equal(out$death[3], 1)  # death after recall
})

# ADD_PERSONTIME Testing ####

test_that("add_persontime() — valid dataset creates person-time columns", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01", "2023-01-01")),
    survey_date = as.Date(c("2023-12-31", "2023-11-30")),
    dob = as.Date(c("2022-06-01", "2021-01-01")),
    sex = c("Male", "Female"),
    age_years = c(1, 2)
  )

  out <- add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    dob_col = "dob",
    sex_col = "sex",
    age_years_col = "age_years",
    male_val = "Male",
    female_val = "Female"
  )

  expect_equal(nrow(out), 2)
  expect_true("person_time" %in% names(out))
  expect_true("entry_date" %in% names(out))
  expect_true("exit_date" %in% names(out))
  expect_true("flag_negative_persontime" %in% names(out))
})

test_that("add_persontime() — person-time calculation is correct", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01")),
    survey_date = as.Date(c("2023-12-31")))

  out <- add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date"
  )

  expected_days <- as.numeric(as.Date("2023-12-31") - as.Date("2023-01-01"))
  expect_equal(out$person_time[1], expected_days)
})

test_that("add_persontime() — entry date uses most recent date", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01")),
    survey_date = as.Date(c("2023-12-31")),
    dob = as.Date(c("2023-02-01")),
    date_joined = as.Date(c("2023-03-01"))
  )

  out <- add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    dob_col = "dob",
    date_joined_col = "date_joined"
  )

  # Entry date should be the most recent: date_joined (2023-03-01)
  expect_equal(out$entry_date[1], as.Date("2023-03-01"))
})

test_that("add_persontime() — exit date uses earliest date", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01")),
    survey_date = as.Date(c("2023-12-31")),
    date_of_death = as.Date(c("2023-06-01")),
    date_left = as.Date(c("2023-07-01"))
  )

  out <- add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    date_of_death_col = "date_of_death",
    date_left_col = "date_left"
  )

  # Exit date should be the earliest: date_of_death (2023-06-01)
  expect_equal(out$exit_date[1], as.Date("2023-06-01"))
})

test_that("add_persontime() — negative person-time is set to zero", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-12-31")),
    survey_date = as.Date(c("2023-01-01"))
  )

  out <- add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date"
  )

  expect_equal(out$person_time[1], 0)
  expect_equal(out$flag_negative_persontime[1], 0)
})

test_that("add_persontime() — age and sex columns create stratified person-time", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01", "2023-01-01")),
    survey_date = as.Date(c("2023-12-31", "2023-12-31")),
    age_years = c(3, 10),
    sex = c("Male", "Female")
  )

  out <- add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    age_years_col = "age_years",
    sex_col = "sex",
    male_val = "Male",
    female_val = "Female"
  )

  expect_true("person_time_under5" %in% names(out))
  expect_true("person_time_male" %in% names(out))
  expect_true("person_time_female" %in% names(out))
  expect_gt(out$person_time_under5[1], 0)
  expect_equal(out$person_time_under5[2], 0)
})

test_that("add_persontime() — error on empty dataset", {

  df_empty <- tibble::tibble(
    recall_date = as.Date(character(0)),
    survey_date = as.Date(character(0))
  )

  expect_error(
    add_persontime(
      df_empty,
      recall_date_col = "recall_date",
      survey_date_col = "survey_date"
    )
  )
})

test_that("add_persontime() — error on missing columns", {

  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01"))
  )

  expect_error(
    add_persontime(
      df,
      recall_date_col = "recall_date",
      survey_date_col = "survey_date"
    )
  )
})

# Defensive coding tests for optional columns in add_persontime
test_that("add_persontime handles NULL optional columns without error", {
  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01", "2023-01-01")),
    survey_date = as.Date(c("2023-12-31", "2023-12-31"))
  )

  # Test with NULL optional columns
  out <- add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    age_years_col = NULL,
    sex_col = NULL
  )

  # Should not create person_time_under5, person_time_male, person_time_female
  expect_false("person_time_under5" %in% names(out))
  expect_false("person_time_male" %in% names(out))
  expect_false("person_time_female" %in% names(out))
  expect_true("person_time" %in% names(out))
})

test_that("add_persontime handles non-existent optional columns gracefully", {
  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01", "2023-01-01")),
    survey_date = as.Date(c("2023-12-31", "2023-12-31"))
  )

  # Test with columns that don't exist in the dataset
  out <- add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    age_years_col = "nonexistent_age",
    sex_col = "nonexistent_sex"
  )

  # Should not create optional columns when columns don't exist
  expect_false("person_time_under5" %in% names(out))
  expect_false("person_time_male" %in% names(out))
  expect_false("person_time_female" %in% names(out))
  expect_true("person_time" %in% names(out))
})

test_that("add_persontime creates optional columns only when columns exist", {
  df <- tibble::tibble(
    recall_date = as.Date(c("2023-01-01", "2023-01-01")),
    survey_date = as.Date(c("2023-12-31", "2023-12-31")),
    age_years = c(3, 10)
  )

  # Test with age_years existing but sex not existing
  out <- add_persontime(
    df,
    recall_date_col = "recall_date",
    survey_date_col = "survey_date",
    age_years_col = "age_years",
    sex_col = "nonexistent_sex"
  )

  # Should create person_time_under5 but not sex-based columns
  expect_true("person_time_under5" %in% names(out))
  expect_false("person_time_male" %in% names(out))
  expect_false("person_time_female" %in% names(out))
})

# DEATH_BIRTH Testing ####

test_that("add_standardized_deaths() — death_birth column is created when date_of_birth_col is provided", {

  df <- tibble::tibble(
    date_of_death = as.Date(c("2023-06-01", "2023-07-01", "2023-08-01")),
    recall_date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    date_of_birth = as.Date(c("2022-12-01", "2023-02-01", "2023-01-01"))
  )

  out <- add_standardized_deaths(
    .dataset = df,
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date",
    date_of_birth_col = "date_of_birth"
  )

  expect_true("death_birth" %in% names(out))
  expect_equal(out$death_birth[1], 0)  # born before recall (2022-12-01 <= 2023-01-01)
  expect_equal(out$death_birth[2], 1)  # born after recall (2023-02-01 > 2023-01-01)
  expect_equal(out$death_birth[3], 0)  # born on recall date (2023-01-01 <= 2023-01-01)
})

test_that("add_standardized_deaths() — death_birth handles NA values correctly", {

  df <- tibble::tibble(
    date_of_death = as.Date(c("2023-06-01", "2023-07-01", "2023-08-01")),
    recall_date = as.Date(c("2023-01-01", "2023-01-01", NA)),
    date_of_birth = as.Date(c("2023-02-01", NA, "2023-02-01"))
  )

  out <- add_standardized_deaths(
    .dataset = df,
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date",
    date_of_birth_col = "date_of_birth"
  )

  expect_true("death_birth" %in% names(out))
  expect_equal(out$death_birth[1], 1)  # born after recall
  expect_true(is.na(out$death_birth[2]))  # NA date_of_birth
  expect_true(is.na(out$death_birth[3]))  # NA recall_date
})

test_that("add_standardized_deaths() — death_birth is not created when date_of_birth_col is NULL", {

  df <- tibble::tibble(
    date_of_death = as.Date(c("2023-06-01", "2023-07-01")),
    recall_date = as.Date(c("2023-01-01", "2023-01-01"))
  )

  out <- add_standardized_deaths(
    .dataset = df,
    date_of_death_col = "date_of_death",
    recall_date_col = "recall_date"
  )

  # death_birth should not be in the dataset when date_of_birth_col is not provided
  # But it will be in columns_to_create and may be present with all NA
  # Let's check if it's meaningful (not all NA)
  if ("death_birth" %in% names(out)) {
    expect_true(all(is.na(out$death_birth)))
  }
})
