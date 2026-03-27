# ADD_FOREGONE_CARE Testing ####

test_that("add_foregone_care() — valid dataset creates foregone care category", {

  df <- tibble::tibble(
    health_need = c("no", "yes", "yes", "no", "dont know"),
    received_care = c("dont know", "yes", "no", "dont know", "yes")
  )

  out <- add_foregone_care(
    .dataset = df,
    ind_illness_col = "health_need",
    ind_received_care_col = "received_care",
    illness_yes_val = "yes",
    illness_no_val = "no",
    illness_dontknow_val = "dont know",
    care_yes_val = "yes",
    care_no_val = "no",
    care_dontknow_val = "dont know"
  )

  expect_equal(nrow(out), 5)
  expect_true("health_foregone_care_cat" %in% names(out))
})

test_that("add_foregone_care() — correct categorization logic", {

  df <- tibble::tibble(
    illness = c("no", "yes", "yes", "no"),
    care = c("dont know", "yes", "no", "no")
  )

  out <- add_foregone_care(
    .dataset = df,
    ind_illness_col = "illness",
    ind_received_care_col = "care",
    illness_yes_val = "yes",
    illness_no_val = "no",
    illness_dontknow_val = "dont know",
    care_yes_val = "yes",
    care_no_val = "no",
    care_dontknow_val = "dont know"
  )

  expect_true(grepl("No need", out$health_foregone_care_cat[1]))
  expect_true(grepl("Met need", out$health_foregone_care_cat[2]))
  expect_true(grepl("Foregone care", out$health_foregone_care_cat[3]))
  expect_true(grepl("No need", out$health_foregone_care_cat[4]))
})

test_that("add_foregone_care() — dont know values return NA", {

  df <- tibble::tibble(
    illness = c("dont know", "yes"),
    care = c("yes", "dont know")
  )

  out <- add_foregone_care(
    .dataset = df,
    ind_illness_col = "illness",
    ind_received_care_col = "care",
    illness_yes_val = "yes",
    illness_no_val = "no",
    illness_dontknow_val = "dont know",
    care_yes_val = "yes",
    care_no_val = "no",
    care_dontknow_val = "dont know"
  )

  expect_true(is.na(out$health_foregone_care_cat[1]))
  expect_true(is.na(out$health_foregone_care_cat[2]))
})

test_that("add_foregone_care() — error on empty dataset", {

  df_empty <- tibble::tibble(
    illness = character(0),
    care = character(0)
  )

  expect_error(
    add_foregone_care(
      .dataset = df_empty,
      ind_illness_col = "illness",
      ind_received_care_col = "care",
      illness_yes_val = "yes",
      illness_no_val = "no",
      illness_dontknow_val = "dont know",
      care_yes_val = "yes",
      care_no_val = "no",
      care_dontknow_val = "dont know"
    )
  )
})

test_that("add_foregone_care() — error on missing columns", {

  df <- tibble::tibble(
    illness = c("yes", "no")
  )

  expect_error(
    add_foregone_care(
      .dataset = df,
      ind_illness_col = "illness",
      ind_received_care_col = "care",
      illness_yes_val = "yes",
      illness_no_val = "no",
      illness_dontknow_val = "dont know",
      care_yes_val = "yes",
      care_no_val = "no",
      care_dontknow_val = "dont know"
    )
  )
})

test_that("add_foregone_care() — warning when overwriting existing column", {

  df <- tibble::tibble(
    illness = c("yes", "no"),
    care = c("yes", "no"),
    health_foregone_care_cat = c("old", "old")
  )

  expect_warning(
    add_foregone_care(
      .dataset = df,
      ind_illness_col = "illness",
      ind_received_care_col = "care",
      illness_yes_val = "yes",
      illness_no_val = "no",
      illness_dontknow_val = "dont know",
      care_yes_val = "yes",
      care_no_val = "no",
      care_dontknow_val = "dont know"
    )
  )
})

test_that("add_foregone_care() — invalid values trigger message", {

  df <- tibble::tibble(
    illness = c("yes", "maybe", "no"),
    care = c("yes", "yes", "no")
  )

  expect_message(
    add_foregone_care(
      .dataset = df,
      ind_illness_col = "illness",
      ind_received_care_col = "care",
      illness_yes_val = "yes",
      illness_no_val = "no",
      illness_dontknow_val = "dont know",
      care_yes_val = "yes",
      care_no_val = "no",
      care_dontknow_val = "dont know"
    )
  )
})

test_that("add_foregone_care() — NA values handled appropriately", {

  df <- tibble::tibble(
    illness = c("yes", NA, "no"),
    care = c("yes", "no", NA)
  )

  out <- add_foregone_care(
    .dataset = df,
    ind_illness_col = "illness",
    ind_received_care_col = "care",
    illness_yes_val = "yes",
    illness_no_val = "no",
    illness_dontknow_val = "dont know",
    care_yes_val = "yes",
    care_no_val = "no",
    care_dontknow_val = "dont know"
  )

  expect_equal(nrow(out), 3)
  expect_true(is.na(out$health_foregone_care_cat[2]))
  expect_true(!is.na(out$health_foregone_care_cat[3]))
})

