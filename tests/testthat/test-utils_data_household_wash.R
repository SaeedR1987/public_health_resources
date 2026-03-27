# ADD_HWISE Testing ####

test_that("add_hwise() — valid dataset creates HWISE-4 scores and categories", {

  df <- tibble::tibble(
    wash_hwise_worry = c("never", "rarely", "sometimes"),
    wash_hwise_plans = c("never", "rarely", "sometimes"),
    wash_hwise_hands = c("never", "rarely", "sometimes"),
    wash_hwise_drink = c("never", "rarely", "sometimes")
  )

  out <- add_hwise(
    .dataset = df,
    wash_hwise_worry_col = "wash_hwise_worry",
    wash_hwise_plans_col = "wash_hwise_plans",
    wash_hwise_hands_col = "wash_hwise_hands",
    wash_hwise_drink_col = "wash_hwise_drink"
  )

  expect_equal(nrow(out), 3)
  expect_true("wash_hwise4_score" %in% names(out))
  expect_true("wash_hwise4_severity_cat" %in% names(out))
  expect_true("wash_hwise4_cat" %in% names(out))
})

test_that("add_hwise() — HWISE-4 score calculation is correct", {

  df <- tibble::tibble(
    wash_hwise_worry = c("always", "never"),
    wash_hwise_plans = c("always", "never"),
    wash_hwise_hands = c("always", "never"),
    wash_hwise_drink = c("always", "never")
  )

  out <- add_hwise(
    .dataset = df,
    wash_hwise_worry_col = "wash_hwise_worry",
    wash_hwise_plans_col = "wash_hwise_plans",
    wash_hwise_hands_col = "wash_hwise_hands",
    wash_hwise_drink_col = "wash_hwise_drink",
    never_val = "never",
    rarely_val = "rarely",
    sometimes_val = "sometimes",
    often_val = "often",
    always_val = "always"
  )

  expect_equal(out$wash_hwise4_score[1], 12)  # 4 * 3
  expect_equal(out$wash_hwise4_score[2], 0)
})

test_that("add_hwise() — HWISE-4 severity categories are correct", {

  df <- tibble::tibble(
    wash_hwise_worry = c("never", "sometimes", "sometimes", "often", "always"),
    wash_hwise_plans = c("rarely", "never", "often", "sometimes", "always"),
    wash_hwise_hands = c("never", "sometimes", "sometimes", "always", "always"),
    wash_hwise_drink = c("never", "never", "never", "sometimes", "always")
  )

  out <- add_hwise(
    .dataset = df,
    wash_hwise_worry_col = "wash_hwise_worry",
    wash_hwise_plans_col = "wash_hwise_plans",
    wash_hwise_hands_col = "wash_hwise_hands",
    wash_hwise_drink_col = "wash_hwise_drink"
  )

  expect_true(grepl("No-to-marginal", out$wash_hwise4_severity_cat[1]))
  expect_true(grepl("Low", out$wash_hwise4_severity_cat[2]))
  expect_true(grepl("Moderate", out$wash_hwise4_severity_cat[3]))
  expect_true(grepl("High", out$wash_hwise4_severity_cat[4]))
  expect_true(grepl("Very High", out$wash_hwise4_severity_cat[5]))
})

test_that("add_hwise() — error on empty dataset", {

  df_empty <- tibble::tibble(
    wash_hwise_worry = character(0),
    wash_hwise_plans = character(0),
    wash_hwise_hands = character(0),
    wash_hwise_drink = character(0)
  )

  expect_error(
    add_hwise(
      .dataset = df_empty,
      wash_hwise_worry_col = "wash_hwise_worry",
      wash_hwise_plans_col = "wash_hwise_plans",
      wash_hwise_hands_col = "wash_hwise_hands",
      wash_hwise_drink_col = "wash_hwise_drink"
    )
  )
})

test_that("add_hwise() — error on missing columns", {

  df <- tibble::tibble(
    wash_hwise_worry = c("never", "rarely")
  )

  expect_error(
    add_hwise(
      .dataset = df,
      wash_hwise_worry_col = "wash_hwise_worry",
      wash_hwise_plans_col = "wash_hwise_plans",
      wash_hwise_hands_col = "wash_hwise_hands",
      wash_hwise_drink_col = "wash_hwise_drink"
    )
  )
})

test_that("add_hwise() — warning when overwriting existing columns", {

  df <- tibble::tibble(
    wash_hwise_worry = c("never"),
    wash_hwise_plans = c("never"),
    wash_hwise_hands = c("never"),
    wash_hwise_drink = c("never"),
    wash_hwise4_score = 99
  )

  expect_warning(
    add_hwise(
      .dataset = df,
      wash_hwise_worry_col = "wash_hwise_worry",
      wash_hwise_plans_col = "wash_hwise_plans",
      wash_hwise_hands_col = "wash_hwise_hands",
      wash_hwise_drink_col = "wash_hwise_drink"
    )
  )
})

# ADD_DRINKING_WATER_SOURCE_CAT Testing ####

test_that("add_drinking_water_source_cat() — valid dataset creates water source category", {

  df <- tibble::tibble(
    wash_water_source = c("piped_dwelling", "unprotected_well", "surface_water", "dnk")
  )

  out <- add_drinking_water_source_cat(
    .dataset = df,
    drinking_water_source_col = "wash_water_source"
  )

  expect_equal(nrow(out), 4)
  expect_true("wash_drinking_water_source_cat" %in% names(out))
})

test_that("add_drinking_water_source_cat() — categorization works correctly", {

  df <- tibble::tibble(
    source = c("piped_dwelling", "unprotected_well", "surface_water", "other")
  )

  out <- add_drinking_water_source_cat(
    .dataset = df,
    drinking_water_source_col = "source"
  )

  expect_true(grepl("Improved", out$wash_drinking_water_source_cat[1]))
  expect_true(grepl("Unimproved", out$wash_drinking_water_source_cat[2]))
  expect_true(grepl("Surface Water", out$wash_drinking_water_source_cat[3]))
  expect_true(grepl("Undefined", out$wash_drinking_water_source_cat[4]))
})

test_that("add_drinking_water_source_cat() — error on empty dataset", {

  df_empty <- tibble::tibble(
    source = character(0)
  )

  expect_error(
    add_drinking_water_source_cat(
      .dataset = df_empty,
      drinking_water_source_col = "source"
    )
  )
})

test_that("add_drinking_water_source_cat() — error on missing column", {

  df <- tibble::tibble(
    wrong_col = c("piped_dwelling")
  )

  expect_error(
    add_drinking_water_source_cat(
      .dataset = df,
      drinking_water_source_col = "source"
    )
  )
})

# ADD_SANITATION_FACILITY_CAT Testing ####

test_that("add_sanitation_facility_cat() — valid dataset creates sanitation category", {

  df <- tibble::tibble(
    wash_sanitation_facility = c("flush_to_piped", "pit_lat", "open_defecation", "other")
  )

  out <- add_sanitation_facility_cat(
    .dataset = df,
    sanitation_facility_col = "wash_sanitation_facility",
    improved_facilities_val = c("flush_to_piped", "flush_to_septic"),
    unimproved_facilities_val = c("pit_lat", "bucket"),
    open_defecation_val = c("open_defecation"),
    undefined_val = c("other", "dnk")
  )

  expect_equal(nrow(out), 4)
  expect_true("wash_sanitation_facility_cat" %in% names(out))
})

test_that("add_sanitation_facility_cat() — categorization works correctly", {

  df <- tibble::tibble(
    facility = c("flush_to_piped", "pit_lat", "open_defecation", "dnk")
  )

  out <- add_sanitation_facility_cat(
    .dataset = df,
    sanitation_facility_col = "facility",
    improved_facilities_val = c("flush_to_piped"),
    unimproved_facilities_val = c("pit_lat"),
    open_defecation_val = c("open_defecation"),
    undefined_val = c("dnk")
  )

  expect_true(grepl("Improved", out$wash_sanitation_facility_cat[1]))
  expect_true(grepl("Unimproved", out$wash_sanitation_facility_cat[2]))
  expect_true(grepl("Open Defecation", out$wash_sanitation_facility_cat[3]))
  expect_true(grepl("Undefined", out$wash_sanitation_facility_cat[4]))
})

test_that("add_sanitation_facility_cat() — error on empty dataset", {

  df_empty <- tibble::tibble(
    facility = character(0)
  )

  expect_error(
    add_sanitation_facility_cat(
      .dataset = df_empty,
      sanitation_facility_col = "facility",
      improved_facilities_val = c("flush"),
      unimproved_facilities_val = c("pit"),
      open_defecation_val = c("open"),
      undefined_val = c("other")
    )
  )
})

# ADD_DRINKING_WATER_JMP_LADDER Testing ####

test_that("add_drinking_water_jmp_ladder() — valid dataset creates JMP ladder", {

  df <- tibble::tibble(
    wash_drinking_water_source_cat = c("improved", "improved", "unimproved", "surface_water"),
    wash_drinking_water_time = c("under_30min", "over_30min", "over_30min", "under_30min")
  )

  out <- add_drinking_water_jmp_ladder(
    .dataset = df,
    drinking_water_source_cat_col = "wash_drinking_water_source_cat",
    drinking_water_source_cat_improved_val = "improved",
    drinking_water_source_cat_unimproved_val = "unimproved",
    drinking_water_source_cat_surface_water_val = "surface_water",
    drinking_water_time_col = "wash_drinking_water_time",
    drinking_water_time_under_30min_val = "under_30min",
    drinking_water_time_over_30min_val = "over_30min"
  )

  expect_equal(nrow(out), 4)
  expect_true("wash_jmp_ladder_drinking_water_cat" %in% names(out))
  expect_s3_class(out$wash_jmp_ladder_drinking_water_cat, "factor")
})

test_that("add_drinking_water_jmp_ladder() — categorization logic is correct", {

  df <- tibble::tibble(
    source_cat = c("improved", "improved", "unimproved", "surface_water"),
    time_cat = c("under_30min", "over_30min", "under_30min", "under_30min")
  )

  out <- add_drinking_water_jmp_ladder(
    .dataset = df,
    drinking_water_source_cat_col = "source_cat",
    drinking_water_source_cat_improved_val = "improved",
    drinking_water_source_cat_unimproved_val = "unimproved",
    drinking_water_source_cat_surface_water_val = "surface_water",
    drinking_water_time_col = "time_cat",
    drinking_water_time_under_30min_val = "under_30min",
    drinking_water_time_over_30min_val = "over_30min"
  )

  expect_true(grepl("Basic", out$wash_jmp_ladder_drinking_water_cat[1]))
  expect_true(grepl("Limited", out$wash_jmp_ladder_drinking_water_cat[2]))
  expect_true(grepl("Unimproved", out$wash_jmp_ladder_drinking_water_cat[3]))
  expect_true(grepl("Surface Water", out$wash_jmp_ladder_drinking_water_cat[4]))
})

test_that("add_drinking_water_jmp_ladder() — error on empty dataset", {

  df_empty <- tibble::tibble(
    source = character(0),
    time = character(0)
  )

  expect_error(
    add_drinking_water_jmp_ladder(
      .dataset = df_empty,
      drinking_water_source_cat_col = "source",
      drinking_water_source_cat_improved_val = "improved",
      drinking_water_source_cat_unimproved_val = "unimproved",
      drinking_water_source_cat_surface_water_val = "surface_water",
      drinking_water_time_col = "time",
      drinking_water_time_under_30min_val = "under_30min",
      drinking_water_time_over_30min_val = "over_30min"
    )
  )
})

test_that("add_drinking_water_jmp_ladder() — ordered factor levels are correct", {

  df <- tibble::tibble(
    source_cat = c("improved", "improved", "unimproved", "surface_water"),
    time_cat = c("under_30min", "over_30min", "under_30min", "under_30min")
  )

  out <- add_drinking_water_jmp_ladder(
    .dataset = df,
    drinking_water_source_cat_col = "source_cat",
    drinking_water_source_cat_improved_val = "improved",
    drinking_water_source_cat_unimproved_val = "unimproved",
    drinking_water_source_cat_surface_water_val = "surface_water",
    drinking_water_time_col = "time_cat",
    drinking_water_time_under_30min_val = "under_30min",
    drinking_water_time_over_30min_val = "over_30min"
  )

  expect_true(is.ordered(out$wash_jmp_ladder_drinking_water_cat))
  levels_order <- levels(out$wash_jmp_ladder_drinking_water_cat)
  expect_true(grepl("Basic", levels_order[1]))
  expect_true(grepl("Surface Water", levels_order[4]))
})

# ADD_SANITATION_JMP_LADDER Testing ####

test_that("add_sanitation_jmp_ladder() — valid dataset creates JMP ladder", {

  df <- tibble::tibble(
    wash_sanitation_facility_cat = c("improved", "improved", "unimproved", "open_defecation"),
    wash_shared_sanitation = c("no", "yes", "no", "no")
  )

  out <- add_sanitation_jmp_ladder(
    .dataset = df,
    sanitation_facility_cat_col = "wash_sanitation_facility_cat",
    sanitation_facility_improved_val = "improved",
    sanitation_facility_unimproved_val = "unimproved",
    sanitation_facility_open_defecation_val = "open_defecation",
    shared_sanitation_col = "wash_shared_sanitation",
    shared_sanitation_yes_val = "yes",
    shared_sanitation_no_val = "no"
  )

  expect_equal(nrow(out), 4)
  expect_true("wash_jmp_ladder_sanitation_cat" %in% names(out))
  expect_s3_class(out$wash_jmp_ladder_sanitation_cat, "factor")
})

test_that("add_sanitation_jmp_ladder() — categorization logic is correct", {

  df <- tibble::tibble(
    facility = c("improved", "improved", "unimproved", "open_defecation"),
    shared = c("no", "yes", "yes", "no")
  )

  out <- add_sanitation_jmp_ladder(
    .dataset = df,
    sanitation_facility_cat_col = "facility",
    sanitation_facility_improved_val = "improved",
    sanitation_facility_unimproved_val = "unimproved",
    sanitation_facility_open_defecation_val = "open_defecation",
    shared_sanitation_col = "shared",
    shared_sanitation_yes_val = "yes",
    shared_sanitation_no_val = "no"
  )

  expect_true(grepl("Basic", out$wash_jmp_ladder_sanitation_cat[1]))
  expect_true(grepl("Limited", out$wash_jmp_ladder_sanitation_cat[2]))
  expect_true(grepl("Unimproved", out$wash_jmp_ladder_sanitation_cat[3]))
  expect_true(grepl("Open Defecation", out$wash_jmp_ladder_sanitation_cat[4]))
})

test_that("add_sanitation_jmp_ladder() — ordered factor levels are correct", {

  df <- tibble::tibble(
    facility = c("improved", "improved", "unimproved", "open_defecation"),
    shared = c("no", "yes", "yes", "no")
  )

  out <- add_sanitation_jmp_ladder(
    .dataset = df,
    sanitation_facility_cat_col = "facility",
    sanitation_facility_improved_val = "improved",
    sanitation_facility_unimproved_val = "unimproved",
    sanitation_facility_open_defecation_val = "open_defecation",
    shared_sanitation_col = "shared",
    shared_sanitation_yes_val = "yes",
    shared_sanitation_no_val = "no"
  )

  expect_true(is.ordered(out$wash_jmp_ladder_sanitation_cat))
  levels_order <- levels(out$wash_jmp_ladder_sanitation_cat)
  expect_true(grepl("Basic", levels_order[1]))
  expect_true(grepl("Open Defecation", levels_order[4]))
})

test_that("add_sanitation_jmp_ladder() — error on empty dataset", {

  df_empty <- tibble::tibble(
    facility = character(0),
    shared = character(0)
  )

  expect_error(
    add_sanitation_jmp_ladder(
      .dataset = df_empty,
      sanitation_facility_cat_col = "facility",
      sanitation_facility_improved_val = "improved",
      sanitation_facility_unimproved_val = "unimproved",
      sanitation_facility_open_defecation_val = "open_defecation",
      shared_sanitation_col = "shared",
      shared_sanitation_yes_val = "yes",
      shared_sanitation_no_val = "no"
    )
  )
})

# ADD_LITERS_PER_PERSON_PER_DAY Testing ####

test_that("add_liters_per_person_per_day() — valid dataset creates all columns", {

  df <- tibble::tibble(
    wash_total_liters = c(100, 200, 300),
    hh_size = c(4, 5, 6),
    wash_num_days_water_collection = c(1, 2, 3)
  )

  out <- add_liters_per_person_per_day(
    .dataset = df,
    total_liters_col = "wash_total_liters",
    household_size_col = "hh_size",
    num_days_col = "wash_num_days_water_collection"
  )

  expect_equal(nrow(out), 3)
  expect_true("liters_pppd" %in% names(out))
  expect_true("liters_z_score" %in% names(out))
  expect_true("liters_pppd_z_score" %in% names(out))
  expect_true("liters_log" %in% names(out))
  expect_true("liters_pppd_log" %in% names(out))
  expect_true("wash_lppd_cat" %in% names(out))
})

test_that("add_liters_per_person_per_day() — calculation is correct", {

  df <- tibble::tibble(
    total_liters = c(100),
    hh_size = c(5),
    num_days = c(2)
  )

  out <- add_liters_per_person_per_day(
    .dataset = df,
    total_liters_col = "total_liters",
    household_size_col = "hh_size",
    num_days_col = "num_days"
  )

  # 100 / (5 * 2) = 10 liters per person per day
  expect_equal(out$liters_pppd[1], 10)
})

test_that("add_liters_per_person_per_day() — categorization works correctly", {

  df <- tibble::tibble(
    total_liters = c(10, 30, 80, 150),
    hh_size = c(5, 5, 5, 5),
    num_days = c(1, 1, 1, 1)
  )

  out <- add_liters_per_person_per_day(
    .dataset = df,
    total_liters_col = "total_liters",
    household_size_col = "hh_size",
    num_days_col = "num_days"
  )

  # Row 1: 10/5/1 = 2 LPPD (< 3)
  expect_true(grepl("Less than 3 LPPD", out$wash_lppd_cat[1]))
  # Row 2: 30/5/1 = 6 LPPD (3-7.5)
  expect_true(grepl("3-7.5 LPPD", out$wash_lppd_cat[2]))
  # Row 3: 80/5/1 = 16 LPPD (>= 15)
  expect_true(grepl("Greater than 15 LPPD", out$wash_lppd_cat[3]))
})

test_that("add_liters_per_person_per_day() — error on empty dataset", {

  df_empty <- tibble::tibble(
    total_liters = numeric(0),
    hh_size = numeric(0),
    num_days = numeric(0)
  )

  expect_error(
    add_liters_per_person_per_day(
      .dataset = df_empty,
      total_liters_col = "total_liters",
      household_size_col = "hh_size",
      num_days_col = "num_days"
    )
  )
})

test_that("add_liters_per_person_per_day() — error on missing columns", {

  df <- tibble::tibble(
    total_liters = c(100, 200)
  )

  expect_error(
    add_liters_per_person_per_day(
      .dataset = df,
      total_liters_col = "total_liters",
      household_size_col = "hh_size",
      num_days_col = "num_days"
    )
  )
})

test_that("add_liters_per_person_per_day() — handles NA values", {

  df <- tibble::tibble(
    total_liters = c(100, NA, 200),
    hh_size = c(5, 5, NA),
    num_days = c(2, 2, 2)
  )

  out <- add_liters_per_person_per_day(
    .dataset = df,
    total_liters_col = "total_liters",
    household_size_col = "hh_size",
    num_days_col = "num_days"
  )

  expect_equal(nrow(out), 3)
  expect_false(is.na(out$liters_pppd[1]))
  expect_true(is.na(out$liters_pppd[2]))
  expect_true(is.na(out$liters_pppd[3]))
})

# ADD_DRINKING_WATER_TIME_CAT Testing ####

test_that("add_drinking_water_time_cat() — numeric minutes categorization works", {

  df <- tibble::tibble(
    number_minutes = c(15, 30, 45, 60),
    categorical_time = c(NA, NA, NA, NA)
  )

  out <- add_drinking_water_time_cat(
    .dataset = df,
    number_minutes_col = "number_minutes",
    categorical_time_col = "categorical_time"
  )

  expect_equal(nrow(out), 4)
  expect_true("wash_drinking_water_time_cat" %in% names(out))
  expect_equal(out$wash_drinking_water_time_cat[1], 1)  # <= 30
  expect_equal(out$wash_drinking_water_time_cat[2], 1)  # <= 30
  expect_equal(out$wash_drinking_water_time_cat[3], 0)  # > 30
  expect_equal(out$wash_drinking_water_time_cat[4], 0)  # > 30
})

test_that("add_drinking_water_time_cat() — categorical time works", {

  df <- tibble::tibble(
    number_minutes = c(NA, NA),
    categorical_time = c("under_30min", "more_than_30min")
  )

  out <- add_drinking_water_time_cat(
    .dataset = df,
    number_minutes_col = "number_minutes",
    categorical_time_col = "categorical_time",
    under_30min = "under_30min",
    more_than_30min = "more_than_30min"
  )

  expect_equal(out$wash_drinking_water_time_cat[1], 1)
  expect_equal(out$wash_drinking_water_time_cat[2], 0)
})

test_that("add_drinking_water_time_cat() — numeric takes priority over categorical", {

  df <- tibble::tibble(
    number_minutes = c(20),
    categorical_time = c("more_than_30min")  # Conflicting but should be ignored
  )

  out <- add_drinking_water_time_cat(
    .dataset = df,
    number_minutes_col = "number_minutes",
    categorical_time_col = "categorical_time",
    under_30min = "under_30min",
    more_than_30min = "more_than_30min"
  )

  # Should use numeric (20 minutes) not categorical
  expect_equal(out$wash_drinking_water_time_cat[1], 1)
})

test_that("add_drinking_water_time_cat() — undefined values return NA", {

  df <- tibble::tibble(
    number_minutes = c(NA, NA),
    categorical_time = c("dnk", "pnta")
  )

  out <- add_drinking_water_time_cat(
    .dataset = df,
    number_minutes_col = "number_minutes",
    categorical_time_col = "categorical_time",
    under_30min = "under_30min",
    more_than_30min = "more_than_30min",
    undefined = c("dnk", "pnta")
  )

  expect_true(is.na(out$wash_drinking_water_time_cat[1]))
  expect_true(is.na(out$wash_drinking_water_time_cat[2]))
})

test_that("add_drinking_water_time_cat() — error on empty dataset", {

  df_empty <- tibble::tibble(
    number_minutes = numeric(0),
    categorical_time = character(0)
  )

  expect_error(
    add_drinking_water_time_cat(
      .dataset = df_empty,
      number_minutes_col = "number_minutes",
      categorical_time_col = "categorical_time"
    )
  )
})

test_that("add_drinking_water_time_cat() — error when both columns missing", {

  df <- tibble::tibble(
    other_col = c(1, 2, 3)
  )

  expect_error(
    add_drinking_water_time_cat(
      .dataset = df,
      number_minutes_col = "number_minutes",
      categorical_time_col = "categorical_time"
    )
  )
})

test_that("add_drinking_water_time_cat() — warning when overwriting existing column", {

  df <- tibble::tibble(
    number_minutes = c(20),
    categorical_time = c("under_30min"),
    wash_drinking_water_time_cat = 99
  )

  expect_warning(
    add_drinking_water_time_cat(
      .dataset = df,
      number_minutes_col = "number_minutes",
      categorical_time_col = "categorical_time"
    )
  )
})

# ADD_ANY_WATER_TREATMENT Testing ####

test_that("add_any_water_treatment() — valid dataset creates treatment indicator", {

  df <- tibble::tibble(
    water_treatment = c("boiling", "none", "chlorine", "filter")
  )

  out <- add_any_water_treatment(
    .dataset = df,
    water_treatment_col = "water_treatment",
    yes_values = c("boiling", "chlorine", "filter"),
    no_values = c("none")
  )

  expect_equal(nrow(out), 4)
  expect_true("wash_any_water_treatment" %in% names(out))
})

test_that("add_any_water_treatment() — categorization logic works", {

  df <- tibble::tibble(
    treatment = c("boiling", "chlorine", "none", "filter", "other")
  )

  out <- add_any_water_treatment(
    .dataset = df,
    water_treatment_col = "treatment",
    yes_values = c("boiling", "chlorine", "filter"),
    no_values = c("none")
  )

  expect_equal(out$wash_any_water_treatment[1], "yes")
  expect_equal(out$wash_any_water_treatment[2], "yes")
  expect_equal(out$wash_any_water_treatment[3], "no")
  expect_equal(out$wash_any_water_treatment[4], "yes")
  expect_true(is.na(out$wash_any_water_treatment[5]))
})

test_that("add_any_water_treatment() — NA and unknown values return NA", {

  df <- tibble::tibble(
    treatment = c(NA, "dnk", "pnta")
  )

  out <- add_any_water_treatment(
    .dataset = df,
    water_treatment_col = "treatment",
    yes_values = c("boiling"),
    no_values = c("none")
  )

  expect_true(all(is.na(out$wash_any_water_treatment)))
})

test_that("add_any_water_treatment() — error on empty dataset", {

  df_empty <- tibble::tibble(
    treatment = character(0)
  )

  expect_error(
    add_any_water_treatment(
      .dataset = df_empty,
      water_treatment_col = "treatment",
      yes_values = c("boiling"),
      no_values = c("none")
    )
  )
})

test_that("add_any_water_treatment() — error on missing column", {

  df <- tibble::tibble(
    wrong_col = c("boiling", "none")
  )

  expect_error(
    add_any_water_treatment(
      .dataset = df,
      water_treatment_col = "treatment",
      yes_values = c("boiling"),
      no_values = c("none")
    )
  )
})

test_that("add_any_water_treatment() — warning when overwriting existing column", {

  df <- tibble::tibble(
    treatment = c("boiling"),
    wash_any_water_treatment = "old"
  )

  expect_warning(
    add_any_water_treatment(
      .dataset = df,
      water_treatment_col = "treatment",
      yes_values = c("boiling"),
      no_values = c("none")
    )
  )
})

# ADD_SANITATION_FACILITY_SHARED Testing ####

test_that("add_sanitation_facility_shared() — numeric input works correctly", {

  df <- tibble::tibble(
    num_households = c(1, 2, 3, 5)
  )

  out <- add_sanitation_facility_shared(
    .dataset = df,
    num_households_col = "num_households",
    shared_threshold = 2
  )

  expect_equal(nrow(out), 4)
  expect_true("wash_sanitation_facility_shared_cat" %in% names(out))
  expect_equal(out$wash_sanitation_facility_shared_cat[1], "no")   # < 2
  expect_equal(out$wash_sanitation_facility_shared_cat[2], "yes")  # >= 2
  expect_equal(out$wash_sanitation_facility_shared_cat[3], "yes")  # >= 2
  expect_equal(out$wash_sanitation_facility_shared_cat[4], "yes")  # >= 2
})

test_that("add_sanitation_facility_shared() — categorical input works correctly", {

  df <- tibble::tibble(
    shared_response = c("shared", "not_shared", "shared")
  )

  out <- add_sanitation_facility_shared(
    .dataset = df,
    shared_response_col = "shared_response",
    shared_values = c("shared"),
    not_shared_values = c("not_shared")
  )

  expect_equal(out$wash_sanitation_facility_shared_cat[1], "⧫yes⧫")
  expect_equal(out$wash_sanitation_facility_shared_cat[2], "⧫no⧫")
  expect_equal(out$wash_sanitation_facility_shared_cat[3], "⧫yes⧫")
})

test_that("add_sanitation_facility_shared() — numeric takes priority over categorical", {

  df <- tibble::tibble(
    num_households = c(1, 3),
    shared_response = c("shared", "not_shared")
  )

  out <- add_sanitation_facility_shared(
    .dataset = df,
    num_households_col = "num_households",
    shared_threshold = 2,
    shared_response_col = "shared_response",
    shared_values = c("shared"),
    not_shared_values = c("not_shared")
  )

  # Should use numeric values
  expect_equal(out$wash_sanitation_facility_shared_cat[1], "no")   # num=1
  expect_equal(out$wash_sanitation_facility_shared_cat[2], "yes")  # num=3
})

test_that("add_sanitation_facility_shared() — fallback to categorical when numeric is NA", {

  df <- tibble::tibble(
    num_households = c(1, NA, NA),
    shared_response = c(NA, "shared", "not_shared")
  )

  out <- add_sanitation_facility_shared(
    .dataset = df,
    num_households_col = "num_households",
    shared_threshold = 2,
    shared_response_col = "shared_response",
    shared_values = c("shared"),
    not_shared_values = c("not_shared")
  )

  expect_equal(out$wash_sanitation_facility_shared_cat[1], "no")   # numeric
  expect_equal(out$wash_sanitation_facility_shared_cat[2], "yes")  # categorical fallback
  expect_equal(out$wash_sanitation_facility_shared_cat[3], "no")   # categorical fallback
})

test_that("add_sanitation_facility_shared() — error on empty dataset", {

  df_empty <- tibble::tibble(
    num_households = numeric(0)
  )

  expect_error(
    add_sanitation_facility_shared(
      .dataset = df_empty,
      num_households_col = "num_households"
    )
  )
})

test_that("add_sanitation_facility_shared() — error when no input provided", {

  df <- tibble::tibble(
    some_col = c(1, 2, 3)
  )

  expect_error(
    add_sanitation_facility_shared(
      .dataset = df
    )
  )
})

test_that("add_sanitation_facility_shared() — warning when overwriting existing column", {

  df <- tibble::tibble(
    num_households = c(1, 2),
    wash_sanitation_facility_shared_cat = c("old", "old")
  )

  expect_warning(
    add_sanitation_facility_shared(
      .dataset = df,
      num_households_col = "num_households"
    )
  )
})

# ADD_SQM_PER_PERSON Testing ####

test_that("add_sqm_per_person() — valid dataset creates all columns", {

  df <- tibble::tibble(
    shelter_shape = c("rectangle", "circle"),
    shelter_length_m = c(5, NA),
    shelter_width_m = c(4, NA),
    shelter_diameter_m = c(NA, 6),
    household_size = c(4, 3),
    shelter_measured = c("yes", "yes")
  )

  out <- add_sqm_per_person(
    .dataset = df,
    shelter_shape_col = "shelter_shape",
    rectangle_val = "rectangle",
    circle_val = "circle",
    shelter_length_col = "shelter_length_m",
    shelter_width_col = "shelter_width_m",
    shelter_diameter_col = "shelter_diameter_m",
    household_size_col = "household_size",
    measure_confirm_col = "shelter_measured",
    measure_confirm_yes_val = "yes"
  )

  expect_equal(nrow(out), 2)
  expect_true("area_sqm" %in% names(out))
  expect_true("sqm_per_person" %in% names(out))
  expect_true("sqm_per_person_cat" %in% names(out))
  expect_s3_class(out$sqm_per_person_cat, "factor")
})

test_that("add_sqm_per_person() — rectangle area calculation is correct", {

  df <- tibble::tibble(
    shape = "rectangle",
    length = 10,
    width = 5,
    diameter = NA,
    hh_size = 10,
    measured = "yes"
  )

  out <- add_sqm_per_person(
    .dataset = df,
    shelter_shape_col = "shape",
    rectangle_val = "rectangle",
    circle_val = "circle",
    shelter_length_col = "length",
    shelter_width_col = "width",
    shelter_diameter_col = "diameter",
    household_size_col = "hh_size",
    measure_confirm_col = "measured",
    measure_confirm_yes_val = "yes"
  )

  # 10 * 5 = 50 sqm, 50 / 10 = 5 sqm per person
  expect_equal(out$area_sqm[1], 50)
  expect_equal(out$sqm_per_person[1], 5)
})

test_that("add_sqm_per_person() — circle area calculation is correct", {

  df <- tibble::tibble(
    shape = "circle",
    length = NA,
    width = NA,
    diameter = 4,
    hh_size = 5,
    measured = "yes"
  )

  out <- add_sqm_per_person(
    .dataset = df,
    shelter_shape_col = "shape",
    rectangle_val = "rectangle",
    circle_val = "circle",
    shelter_length_col = "length",
    shelter_width_col = "width",
    shelter_diameter_col = "diameter",
    household_size_col = "hh_size",
    measure_confirm_col = "measured",
    measure_confirm_yes_val = "yes"
  )

  # pi * (4/2)^2 = pi * 4 = ~12.6 sqm
  expect_true(out$area_sqm[1] > 12 & out$area_sqm[1] < 13)
  expect_true(out$sqm_per_person[1] > 2 & out$sqm_per_person[1] < 3)
})

test_that("add_sqm_per_person() — categorization works correctly", {

  df <- tibble::tibble(
    shape = rep("rectangle", 4),
    length = c(10, 15, 20, 25),
    width = c(3, 4, 5, 6),
    diameter = rep(NA, 4),
    hh_size = rep(10, 4),
    measured = rep("yes", 4)
  )

  out <- add_sqm_per_person(
    .dataset = df,
    shelter_shape_col = "shape",
    rectangle_val = "rectangle",
    circle_val = "circle",
    shelter_length_col = "length",
    shelter_width_col = "width",
    shelter_diameter_col = "diameter",
    household_size_col = "hh_size",
    measure_confirm_col = "measured",
    measure_confirm_yes_val = "yes"
  )

  # Row 1: 30/10 = 3 sqm per person (< 3.5)
  expect_true(grepl("<3.5", out$sqm_per_person_cat[1]))
  # Row 2: 60/10 = 6 sqm per person (>= 5.5)
  expect_true(grepl(">= 5.5", out$sqm_per_person_cat[2]))
})

test_that("add_sqm_per_person() — measurement not confirmed returns NA", {

  df <- tibble::tibble(
    shape = "rectangle",
    length = 10,
    width = 5,
    diameter = NA,
    hh_size = 10,
    measured = "no"
  )

  out <- add_sqm_per_person(
    .dataset = df,
    shelter_shape_col = "shape",
    rectangle_val = "rectangle",
    circle_val = "circle",
    shelter_length_col = "length",
    shelter_width_col = "width",
    shelter_diameter_col = "diameter",
    household_size_col = "hh_size",
    measure_confirm_col = "measured",
    measure_confirm_yes_val = "yes"
  )

  expect_true(is.na(out$area_sqm[1]))
  expect_true(is.na(out$sqm_per_person[1]))
  expect_true(is.na(out$sqm_per_person_cat[1]))
})

test_that("add_sqm_per_person() — error on empty dataset", {

  df_empty <- tibble::tibble(
    shape = character(0),
    length = numeric(0),
    width = numeric(0),
    diameter = numeric(0),
    hh_size = numeric(0),
    measured = character(0)
  )

  expect_error(
    add_sqm_per_person(
      .dataset = df_empty,
      shelter_shape_col = "shape",
      rectangle_val = "rectangle",
      circle_val = "circle",
      shelter_length_col = "length",
      shelter_width_col = "width",
      shelter_diameter_col = "diameter",
      household_size_col = "hh_size",
      measure_confirm_col = "measured"
    )
  )
})

test_that("add_sqm_per_person() — error on missing columns", {

  df <- tibble::tibble(
    shape = c("rectangle")
  )

  expect_error(
    add_sqm_per_person(
      .dataset = df,
      shelter_shape_col = "shape",
      rectangle_val = "rectangle",
      circle_val = "circle",
      shelter_length_col = "length",
      shelter_width_col = "width",
      shelter_diameter_col = "diameter",
      household_size_col = "hh_size",
      measure_confirm_col = "measured"
    )
  )
})

test_that("add_sqm_per_person() — warning when overwriting existing columns", {

  df <- tibble::tibble(
    shape = "rectangle",
    length = 10,
    width = 5,
    diameter = NA,
    hh_size = 10,
    measured = "yes",
    area_sqm = 99
  )

  expect_warning(
    add_sqm_per_person(
      .dataset = df,
      shelter_shape_col = "shape",
      rectangle_val = "rectangle",
      circle_val = "circle",
      shelter_length_col = "length",
      shelter_width_col = "width",
      shelter_diameter_col = "diameter",
      household_size_col = "hh_size",
      measure_confirm_col = "measured"
    )
  )
})

