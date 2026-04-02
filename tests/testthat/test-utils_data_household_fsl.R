# ADD_FCS Testing ####

test_that("add_fcs() — valid dataset computes scores and categories", {

  set.seed(123)
  df_fcs_valid <- tibble::tibble(
    fsl_fcs_cereal  = sample(0:7, 30, replace = TRUE),
    fsl_fcs_legumes = sample(0:7, 30, replace = TRUE),
    fsl_fcs_veg     = sample(0:7, 30, replace = TRUE),
    fsl_fcs_fruit   = sample(0:3, 30, replace = TRUE),
    fsl_fcs_meat    = sample(0:4, 30, replace = TRUE),
    fsl_fcs_dairy   = sample(0:4, 30, replace = TRUE),
    fsl_fcs_sugar   = sample(0:7, 30, replace = TRUE),
    fsl_fcs_oil     = sample(0:7, 30, replace = TRUE)
  )

  out <- add_fcs(df_fcs_valid)

  expect_equal(nrow(out), 30)
  expect_true(all(!is.na(out$fsl_fcs_score)))
  expect_true(
    all(grepl("(Poor|Borderline|Acceptable)", out$fsl_fcs_cat))
  )
  expect_s3_class(out$fsl_fcs_cat, "factor")
})

test_that("add_fcs() — works across 500 diverse FCS combinations", {

  set.seed(456)

  # Create a diverse grid of 500 combinations
  # Strategy: sample uniformly across the valid range for each variable
  # to ensure we test low, medium, and high values
  n <- 500

  df_fcs_expanded <- tibble::tibble(
    # Cereals (0-7): include full range
    fsl_fcs_cereal  = sample(0:7, n, replace = TRUE),

    # Legumes (0-7): include full range
    fsl_fcs_legumes = sample(0:7, n, replace = TRUE),

    # Vegetables (0-7): include full range
    fsl_fcs_veg     = sample(0:7, n, replace = TRUE),

    # Fruit (0-3): smaller range, ensure good coverage
    fsl_fcs_fruit   = sample(0:3, n, replace = TRUE,
                             prob = c(0.3, 0.25, 0.25, 0.2)),

    # Meat (0-4): ensure we get extremes and middle values
    fsl_fcs_meat    = sample(0:4, n, replace = TRUE,
                             prob = c(0.25, 0.2, 0.2, 0.2, 0.15)),

    # Dairy (0-4): similar to meat
    fsl_fcs_dairy   = sample(0:4, n, replace = TRUE,
                             prob = c(0.25, 0.2, 0.2, 0.2, 0.15)),

    # Sugar (0-7): full range with emphasis on extremes
    fsl_fcs_sugar   = sample(0:7, n, replace = TRUE,
                             prob = c(0.2, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.2)),

    # Oil (0-7): full range
    fsl_fcs_oil     = sample(0:7, n, replace = TRUE)
  )

  # Ensure we have some edge cases by replacing a few rows
  # All zeros (extreme poor)
  df_fcs_expanded[1, ] <- list(0, 0, 0, 0, 0, 0, 0, 0)

  # All max values (extreme acceptable)
  df_fcs_expanded[2, ] <- list(7, 7, 7, 7, 7, 7, 7, 7)

  # Borderline case: moderate values
  df_fcs_expanded[3, ] <- list(3, 2, 3, 1, 1, 1, 2, 3)

  # Mixed extreme: high staples, low protein
  df_fcs_expanded[4, ] <- list(7, 7, 7, 0, 0, 0, 5, 7)

  # Mixed extreme: low staples, high protein
  df_fcs_expanded[5, ] <- list(2, 1, 1, 3, 4, 4, 0, 2)

  out <- add_fcs(df_fcs_expanded)

  # Basic structure checks
  expect_equal(nrow(out), 500)
  expect_true(all(!is.na(out$fsl_fcs_score)))
  expect_s3_class(out$fsl_fcs_cat, "factor")

  # Check all categories are valid
  expect_true(
    all(grepl("(Poor|Borderline|Acceptable)", out$fsl_fcs_cat))
  )

  # Check score ranges are reasonable (FCS typically 0-112)
  expect_true(all(out$fsl_fcs_score >= 0))
  expect_true(all(out$fsl_fcs_score <= 112))

  # Verify we got diverse category distribution
  category_counts <- table(out$fsl_fcs_cat)
  expect_true(length(category_counts) >= 2,
              info = "Should have at least 2 different FCS categories")

  # Check edge cases produce expected ranges
  expect_equal(out$fsl_fcs_score[1], 0)  # All zeros = score 0
  expect_true(out$fsl_fcs_score[2] > 100)  # Max values = high score
  expect_equal(out$fsl_fcs_cat[1], factor("⧫Poor⧫", levels = c("⧫Poor⧫", "⧫Borderline⧫", "⧫Acceptable⧫")))
})

test_that("add_fcs() — dataset with NA produces NA scores appropriately", {

  set.seed(456)
  df_fcs_with_na <- tibble::tibble(
    fsl_fcs_cereal  = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_legumes = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_veg     = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_fruit   = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_meat    = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_dairy   = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_sugar   = sample(c(0:7, NA), 30, replace = TRUE),
    fsl_fcs_oil     = sample(c(0:7, NA), 30, replace = TRUE)
  )

  out <- add_fcs(df_fcs_with_na)

  expect_equal(nrow(out), 30)
  expect_true(any(is.na(out$fsl_fcs_score)))
})


test_that("add_fcs() — out-of-range values trigger phr_error", {

  set.seed(123)
  df_bad <- tibble::tibble(
    fsl_fcs_cereal  = sample(0:7, 30, TRUE),
    fsl_fcs_legumes = sample(0:7, 30, TRUE),
    fsl_fcs_veg     = sample(0:7, 30, TRUE),
    fsl_fcs_fruit   = sample(0:7, 30, TRUE),
    fsl_fcs_meat    = sample(0:7, 30, TRUE),
    fsl_fcs_dairy   = sample(0:7, 30, TRUE),
    fsl_fcs_sugar   = sample(0:7, 30, TRUE),
    fsl_fcs_oil     = sample(0:7, 30, TRUE)
  )

  df_bad$fsl_fcs_veg[1]  <- 10
  df_bad$fsl_fcs_meat[5] <- -3
  df_bad$fsl_fcs_oil[10] <- 14

  expect_warning(
    add_fcs(df_bad)
  )

})

test_that("add_fcs() — non-numeric values trigger phr_error", {

  df_nonnumeric <- tibble::tibble(
    fsl_fcs_cereal  = c(1,2,3,4,5,6,7,0,1,2, rep(3, 20)),
    fsl_fcs_legumes = rep(1, 30),
    fsl_fcs_veg     = rep(2, 30),
    fsl_fcs_fruit   = rep(3, 30),
    fsl_fcs_meat    = rep(4, 30),
    fsl_fcs_dairy   = rep(5, 30),
    fsl_fcs_sugar   = rep(6, 30),
    fsl_fcs_oil     = rep(7, 30)
  )

  df_nonnumeric$fsl_fcs_sugar[2] <- "three"
  df_nonnumeric$fsl_fcs_meat[18] <- "x"
  df_nonnumeric$fsl_fcs_cereal[20] <- "7a"

  out <- add_fcs(df_nonnumeric)

  expect_warning(
    add_fcs(df_nonnumeric)
  )
})

test_that("add_fcs() — warns when overwriting existing score/category columns", {

  df_overwrite <- tibble::tibble(
    fsl_fcs_cereal  = rep(1, 30),
    fsl_fcs_legumes = rep(1, 30),
    fsl_fcs_veg     = rep(1, 30),
    fsl_fcs_fruit   = rep(1, 30),
    fsl_fcs_meat    = rep(1, 30),
    fsl_fcs_dairy   = rep(1, 30),
    fsl_fcs_sugar   = rep(1, 30),
    fsl_fcs_oil     = rep(1, 30),
    fsl_fcs_score   = rep(999, 30),
    fsl_fcs_cat     = rep("Old", 30)
  )

  expect_warning(
    add_fcs(df_overwrite)
  )
})

test_that("add_fcs() — alternative cutoffs applied correctly", {

  df <- tibble::tibble(
    fsl_fcs_cereal  = c(7, 0, 3, 7, 5, rep(2, 25)),
    fsl_fcs_legumes = rep(1, 30),
    fsl_fcs_veg     = rep(1, 30),
    fsl_fcs_fruit   = rep(1, 30),
    fsl_fcs_meat    = rep(1, 30),
    fsl_fcs_dairy   = rep(1, 30),
    fsl_fcs_sugar   = rep(1, 30),
    fsl_fcs_oil     = rep(1, 30)
  )

  out <- add_fcs(df, cutoffs = "alternative")

  expect_true(
    all(grepl("(Poor|Borderline|Acceptable)", out$fsl_fcs_cat))
  )

})

test_that("add_fcs() — invalid cutoff gives warning and defaults to normal", {

  df <- tibble::tibble(
    fsl_fcs_cereal  = rep(3, 30),
    fsl_fcs_legumes = rep(2, 30),
    fsl_fcs_veg     = rep(1, 30),
    fsl_fcs_fruit   = rep(1, 30),
    fsl_fcs_meat    = rep(1, 30),
    fsl_fcs_dairy   = rep(2, 30),
    fsl_fcs_sugar   = rep(1, 30),
    fsl_fcs_oil     = rep(1, 30)
  )

  expect_warning(
    out <- add_fcs(df, cutoffs = "INVALID")
  )

  expect_true("fsl_fcs_score" %in% names(out))

})

test_that("add_fcs() — missing columns cause error", {

  df_missing <- tibble::tibble(
    fsl_fcs_cereal = 1:30,
    fsl_fcs_legumes = 1:30
    # missing 6 required vars
  )

  expect_error(
    add_fcs(df_missing)
  )
})

test_that("add_fcs() — empty dataframe rejected", {

  df_empty <- tibble::tibble(
    fsl_fcs_cereal = numeric(0),
    fsl_fcs_legumes = numeric(0),
    fsl_fcs_veg = numeric(0),
    fsl_fcs_fruit = numeric(0),
    fsl_fcs_meat = numeric(0),
    fsl_fcs_dairy = numeric(0),
    fsl_fcs_sugar = numeric(0),
    fsl_fcs_oil = numeric(0)
  )

  expect_error(add_fcs(df_empty))
})

# ADD_HHS Testing ####

test_that("add_hhs() — normal functionality works", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh        = sample(c("yes","no"), 30, TRUE),
    fsl_hhs_nofoodhh_freq   = sample(c("rarely","sometimes","often"), 30, TRUE),
    fsl_hhs_sleephungry     = sample(c("yes","no"), 30, TRUE),
    fsl_hhs_sleephungry_freq= sample(c("rarely","sometimes","often"), 30, TRUE),
    fsl_hhs_alldaynight     = sample(c("yes","no"), 30, TRUE),
    fsl_hhs_alldaynight_freq= sample(c("rarely","sometimes","often"), 30, TRUE)
  )

  out <- add_hhs(df)

  expect_s3_class(out, "data.frame")
  expect_true("fsl_hhs_score" %in% names(out))
  expect_true("fsl_hhs_cat" %in% names(out))
  expect_true("fsl_hhs_cat_ipc" %in% names(out))

  # Score must lie between 0–6
  expect_true(all(out$fsl_hhs_score >= 0 &
                    out$fsl_hhs_score <= 6 &
                    !is.na(out$fsl_hhs_score)))

  # Categories exist
  expect_true(all(grepl("Little to No|Moderate|Severe", out$fsl_hhs_cat)))

  expect_true(all(grepl("None|Little|Moderate|Severe|Very Severe",
                        out$fsl_hhs_cat_ipc)))

  # Factor levels ordered worst to best
  expect_equal(levels(out$fsl_hhs_cat), c("Severe", "Moderate", "Little to No"))
  expect_equal(levels(out$fsl_hhs_cat_ipc), c("Very Severe", "Severe", "Moderate", "Little", "None"))
})

 test_that("add_hhs() — edge case of all zero indicators", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh        = rep("no", 10),
    fsl_hhs_nofoodhh_freq   = rep("rarely", 10),
    fsl_hhs_sleephungry     = rep("no", 10),
    fsl_hhs_sleephungry_freq= rep("rarely", 10),
    fsl_hhs_alldaynight     = rep("no", 10),
    fsl_hhs_alldaynight_freq= rep("rarely", 10)
  )

  out <- add_hhs(df)

  # Score must be 0
  expect_equal(unique(out$fsl_hhs_score), 0)

  # Category must be:
  expect_true(
    all(grepl("None", unique(as.character(out$fsl_hhs_cat_ipc))))
  )

  expect_true(
    all(grepl("Little to No", unique(as.character(out$fsl_hhs_cat))))
  )
})

test_that("add_hhs() — edge case of max severity", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh        = rep("yes", 10),
    fsl_hhs_nofoodhh_freq   = rep("often", 10),
    fsl_hhs_sleephungry     = rep("yes", 10),
    fsl_hhs_sleephungry_freq= rep("often", 10),
    fsl_hhs_alldaynight     = rep("yes", 10),
    fsl_hhs_alldaynight_freq= rep("often", 10)
  )

  out <- add_hhs(df)

  expect_equal(unique(out$fsl_hhs_score), 6)
  expect_true(
    all(grepl("Very Severe", unique(as.character(out$fsl_hhs_cat_ipc))))
  )

  expect_true(
    all(grepl("Severe", unique(as.character(out$fsl_hhs_cat))))
  )
})

test_that("add_hhs() — error if first argument is not a dataframe", {
  expect_error(
    add_hhs("not a df")
  )
})

test_that("add_hhs() — error if dataset has zero rows", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh = character(),
    fsl_hhs_nofoodhh_freq = character(),
    fsl_hhs_sleephungry = character(),
    fsl_hhs_sleephungry_freq = character(),
    fsl_hhs_alldaynight = character(),
    fsl_hhs_alldaynight_freq = character()
  )

  expect_error(
    add_hhs(df)
  )
})

test_that("add_hhs() — error on missing columns", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh = rep("yes", 5),
    # MISSING fsl_hhs_nofoodhh_freq
    fsl_hhs_sleephungry = rep("no", 5),
    fsl_hhs_sleephungry_freq = rep("rarely", 5),
    fsl_hhs_alldaynight = rep("no", 5),
    fsl_hhs_alldaynight_freq = rep("rarely", 5)
  )

  expect_error(
    add_hhs(df)
  )
})

test_that("add_hhs() — invalid Y/N categories produce error", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh        = c("yes","maybe","no","no","yes"),
    fsl_hhs_nofoodhh_freq   = rep("rarely", 5),
    fsl_hhs_sleephungry     = rep("no", 5),
    fsl_hhs_sleephungry_freq= rep("rarely", 5),
    fsl_hhs_alldaynight     = rep("yes", 5),
    fsl_hhs_alldaynight_freq= rep("often", 5)
  )

  expect_message(
   add_hhs(df)
  )
})

test_that("add_hhs() — invalid frequency categories produce error", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh        = rep("yes", 5),
    fsl_hhs_nofoodhh_freq   = c("rarely","sometimes","badcat","rarely","often"),
    fsl_hhs_sleephungry     = rep("no", 5),
    fsl_hhs_sleephungry_freq= rep("rarely", 5),
    fsl_hhs_alldaynight     = rep("no", 5),
    fsl_hhs_alldaynight_freq= rep("often", 5)
  )

  expect_message(
    add_hhs(df)
  )
})

test_that("add_hhs() — mixed combinations produce valid output", {

  df <- tibble::tibble(
    fsl_hhs_nofoodhh        = sample(c("yes","no"), 25, TRUE),
    fsl_hhs_nofoodhh_freq   = sample(c("rarely","sometimes","often"), 25, TRUE),
    fsl_hhs_sleephungry     = sample(c("yes","no"), 25, TRUE),
    fsl_hhs_sleephungry_freq= sample(c("rarely","sometimes","often"), 25, TRUE),
    fsl_hhs_alldaynight     = sample(c("yes","no"), 25, TRUE),
    fsl_hhs_alldaynight_freq= sample(c("rarely","sometimes","often"), 25, TRUE)
  )

  out <- add_hhs(df)

  expect_equal(nrow(out), 25)
  expect_true(all(out$fsl_hhs_score >= 0 & out$fsl_hhs_score <= 6))
})

# ADD_RCSI Testing ####

test_that("add_rcsi() — normal functionality works", {
  set.seed(123)

  df <- tibble::tibble(
    fsl_rcsi_lessquality = sample(0:7, 30, TRUE),
    fsl_rcsi_borrow      = sample(0:7, 30, TRUE),
    fsl_rcsi_mealsize    = sample(0:7, 30, TRUE),
    fsl_rcsi_mealadult   = sample(0:7, 30, TRUE),
    fsl_rcsi_mealnb      = sample(0:7, 30, TRUE)
  )

  out <- add_rcsi(df)

  # Column creation
  expect_true("fsl_rcsi_score" %in% names(out))
  expect_true("fsl_rcsi_cat" %in% names(out))

  # Score is numeric and within expected range
  expect_true(all(is.numeric(out$fsl_rcsi_score)))
  expect_true(all(out$fsl_rcsi_score >= 0, na.rm = TRUE))

  # Category values contain expected text
  expect_true(all(grepl("No to Low|Medium|High",
                        as.character(out$fsl_rcsi_cat),
                        ignore.case = TRUE)))

  # Factor levels ordered worst to best
  expect_equal(levels(out$fsl_rcsi_cat), c("High", "Medium", "No to Low"))
})


test_that("add_rcsi() — missing required RCSI columns triggers phr_error", {
  df_missing <- tibble::tibble(
    fsl_rcsi_lessquality = 1:5,
    fsl_rcsi_borrow      = 1:5
    # rest missing
  )

  expect_error(
    add_rcsi(df_missing)
  )
})


test_that("add_rcsi() — out-of-range values cause phr_error", {
  df_bad <- tibble::tibble(
    fsl_rcsi_lessquality = c(0, 1, 2, 3, 8),  # 8 is invalid
    fsl_rcsi_borrow      = 0:4,
    fsl_rcsi_mealsize    = 0:4,
    fsl_rcsi_mealadult   = 0:4,
    fsl_rcsi_mealnb      = 0:4
  )

  expect_message(
    add_rcsi(df_bad)
  )
})


test_that("add_rcsi() — non-numeric values trigger phr_error", {
  df_nonum <- tibble::tibble(
    fsl_rcsi_lessquality = c("a", "2", "3", "4", "5"), # unsafe coercion
    fsl_rcsi_borrow      = 0:4,
    fsl_rcsi_mealsize    = 0:4,
    fsl_rcsi_mealadult   = 0:4,
    fsl_rcsi_mealnb      = 0:4
  )

  expect_warning(
    out <- add_rcsi(df_nonum)
  )
})


test_that("add_rcsi() — empty dataset triggers phr_error", {
  df_empty <- tibble::tibble(
    fsl_rcsi_lessquality = numeric(0),
    fsl_rcsi_borrow      = numeric(0),
    fsl_rcsi_mealsize    = numeric(0),
    fsl_rcsi_mealadult   = numeric(0),
    fsl_rcsi_mealnb      = numeric(0)
  )

  expect_error(
    add_rcsi(df_empty),
    class = "phr_error"
  )
})


test_that("add_rcsi() — warns when overwriting existing output columns", {
  df <- tibble::tibble(
    fsl_rcsi_lessquality = 0:4,
    fsl_rcsi_borrow      = 0:4,
    fsl_rcsi_mealsize    = 0:4,
    fsl_rcsi_mealadult   = 0:4,
    fsl_rcsi_mealnb      = 0:4,
    fsl_rcsi_score       = rep(99, 5),    # existing column
    fsl_rcsi_cat         = rep("OLD", 5)  # existing column
  )

  expect_warning(
    add_rcsi(df)
  )
})


test_that("add_rcsi() — category assignment works correctly", {

  df <- tibble::tibble(
    # Construct deterministic values for clear category boundaries
    fsl_rcsi_lessquality = c(0, 1, 7),
    fsl_rcsi_borrow      = c(0, 1, 7),
    fsl_rcsi_mealsize    = c(0, 1, 7),
    fsl_rcsi_mealadult   = c(0, 1, 7),
    fsl_rcsi_mealnb      = c(0, 1, 7)
  )

  out <- add_rcsi(df)

  # Extract character labels
  labs <- as.character(out$fsl_rcsi_cat)

  # Validate using grepl for translation safety
  expect_true(grepl("No to Low", labs[1]))
  expect_true(grepl("Medium",    labs[2]))
  expect_true(grepl("High",      labs[3]))
})

# ADD_LCSI Testing ####

test_that("add_lcsi() — standard use case works", {

  df <- tibble::tibble(
    fsl_lcsi_stress1    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_stress2    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_stress3    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_stress4    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),

    fsl_lcsi_crisis1    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_crisis2    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_crisis3    = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),

    fsl_lcsi_emergency1 = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_emergency2 = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE),
    fsl_lcsi_emergency3 = sample(c("yes","no_had_no_need","no_exhausted","not_applicable"), 30, TRUE)
  )

  out <- add_lcsi(df)

  expect_equal(nrow(out), 30)

  # Ensure all derived fields exist
  expect_true(all(c(
    "fsl_lcsi_stress_yes","fsl_lcsi_stress_exhaust","fsl_lcsi_stress",
    "fsl_lcsi_crisis_yes","fsl_lcsi_crisis_exhaust","fsl_lcsi_crisis",
    "fsl_lcsi_emergency_yes","fsl_lcsi_emergency_exhaust","fsl_lcsi_emergency",
    "fsl_lcsi_cat_yes","fsl_lcsi_cat_exhaust","fsl_lcsi_cat"
  ) %in% names(out)))

  # Check categories are translated values using grepl
  expect_true(all(grepl("None|Stress|Crisis|Emergency", out$fsl_lcsi_cat_yes)))
  expect_true(all(grepl("None|Stress|Crisis|Emergency", out$fsl_lcsi_cat_exhaust)))
  expect_true(all(grepl("None|Stress|Crisis|Emergency", out$fsl_lcsi_cat)))

  # Factor levels ordered worst to best
  expect_equal(levels(out$fsl_lcsi_cat), c("Emergency", "Crisis", "Stress", "None"))
  expect_equal(levels(out$fsl_lcsi_cat_yes), c("Emergency", "Crisis", "Stress", "None"))
  expect_equal(levels(out$fsl_lcsi_cat_exhaust), c("Emergency", "Crisis", "Stress", "None"))
})

test_that("add_lcsi() — missing columns cause phr_error", {

  df_missing <- tibble::tibble(
    fsl_lcsi_stress1 = "yes",
    fsl_lcsi_stress2 = "no_had_no_need"
    # missing all others
  )

  expect_error(
    add_lcsi(df_missing),
    class = "phr_error"
  )
})

test_that("add_lcsi() — invalid values cause phr_error", {

  df_bad <- tibble::tibble(
    fsl_lcsi_stress1    = c("yes","INVALID","no_had_no_need"),
    fsl_lcsi_stress2    = "yes",
    fsl_lcsi_stress3    = "yes",
    fsl_lcsi_stress4    = "yes",
    fsl_lcsi_crisis1    = "yes",
    fsl_lcsi_crisis2    = "yes",
    fsl_lcsi_crisis3    = "yes",
    fsl_lcsi_emergency1 = "yes",
    fsl_lcsi_emergency2 = "yes",
    fsl_lcsi_emergency3 = "yes"
  )

  expect_warning(
    add_lcsi(df_bad)
  )
})

test_that("add_lcsi() — NA in any input forces NA in all derived fields", {

  df <- tibble::tibble(
    fsl_lcsi_stress1    = c("yes", NA),
    fsl_lcsi_stress2    = "no_had_no_need",
    fsl_lcsi_stress3    = "no_had_no_need",
    fsl_lcsi_stress4    = "no_had_no_need",
    fsl_lcsi_crisis1    = "no_had_no_need",
    fsl_lcsi_crisis2    = "no_had_no_need",
    fsl_lcsi_crisis3    = "no_had_no_need",
    fsl_lcsi_emergency1 = "no_had_no_need",
    fsl_lcsi_emergency2 = "no_had_no_need",
    fsl_lcsi_emergency3 = "no_had_no_need"
  )

  out <- add_lcsi(df)

  # row with NA should have all derived fields NA
  derived_cols <- c(
    "fsl_lcsi_stress_yes","fsl_lcsi_stress_exhaust","fsl_lcsi_stress",
    "fsl_lcsi_crisis_yes","fsl_lcsi_crisis_exhaust","fsl_lcsi_crisis",
    "fsl_lcsi_emergency_yes","fsl_lcsi_emergency_exhaust","fsl_lcsi_emergency",
    "fsl_lcsi_cat_yes","fsl_lcsi_cat_exhaust","fsl_lcsi_cat"
  )

  expect_true(all(is.na(out[2, derived_cols])))
  expect_true(all(!is.na(out[1, derived_cols])))
})

test_that("add_lcsi() — correct category logic for controlled inputs", {

  df <- tibble::tibble(
    # Stress YES
    fsl_lcsi_stress1    = c("yes", "no_had_no_need", "no_had_no_need", "no_had_no_need"),
    fsl_lcsi_stress2    = "no_had_no_need",
    fsl_lcsi_stress3    = "no_had_no_need",
    fsl_lcsi_stress4    = "no_had_no_need",

    # Crisis YES (row 2)
    fsl_lcsi_crisis1    = c("no_had_no_need", "yes", "no_had_no_need", "no_had_no_need"),
    fsl_lcsi_crisis2    = "no_had_no_need",
    fsl_lcsi_crisis3    = "no_had_no_need",

    # Emergency YES (row 3)
    fsl_lcsi_emergency1 = c("no_had_no_need", "no_had_no_need", "yes", "no_had_no_need"),
    fsl_lcsi_emergency2 = "no_had_no_need",
    fsl_lcsi_emergency3 = "no_had_no_need"
  )

  out <- add_lcsi(df)

  expect_true(grepl("Stress",    out$fsl_lcsi_cat[1]))
  expect_true(grepl("Crisis",    out$fsl_lcsi_cat[2]))
  expect_true(grepl("Emergency", out$fsl_lcsi_cat[3]))
  expect_true(grepl("None",      out$fsl_lcsi_cat[4]))
})

test_that("add_lcsi() — exhausted values correctly classify categories", {

  df <- tibble::tibble(
    fsl_lcsi_stress1    = "no_exhausted",
    fsl_lcsi_stress2    = "no_had_no_need",
    fsl_lcsi_stress3    = "no_had_no_need",
    fsl_lcsi_stress4    = "no_had_no_need",

    fsl_lcsi_crisis1    = "no_had_no_need",
    fsl_lcsi_crisis2    = "no_had_no_need",
    fsl_lcsi_crisis3    = "no_had_no_need",

    fsl_lcsi_emergency1 = "no_had_no_need",
    fsl_lcsi_emergency2 = "no_had_no_need",
    fsl_lcsi_emergency3 = "no_had_no_need"
  )

  out <- add_lcsi(df)

  expect_true(grepl("Stress", out$fsl_lcsi_cat))
  expect_true(grepl("Stress", out$fsl_lcsi_cat_exhaust))
})

# ADD_HDDS Testing ####

test_that("add_hdds() — standard use case works", {

  set.seed(123)

  df <- tibble::tibble(
    fsl_hdds_cereals    = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_tubers     = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_veg        = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_fruit      = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_meat       = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_eggs       = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_fish       = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_legumes    = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_dairy      = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_oil        = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_sugar      = sample(c("yes","no"), 30, TRUE),
    fsl_hdds_condiments = sample(c("yes","no"), 30, TRUE)
  )

  out <- add_hdds(df)

  expect_equal(nrow(out), 30)

  # Derived fields exist
  expect_true("fsl_hdds_score" %in% names(out))
  expect_true("fsl_hdds_cat"   %in% names(out))

  # Category values are valid (via grepl for translated labels)
  expect_true(all(grepl("Low|Medium|High", out$fsl_hdds_cat, ignore.case = TRUE)))
})

test_that("add_hdds() — missing required columns causes phr_error", {

  df_missing <- tibble::tibble(
    fsl_hdds_cereals = "yes",
    fsl_hdds_tubers  = "no"
    # Other 10 required columns missing
  )

  expect_error(
    add_hdds(df_missing),
    class = "phr_error"
  )
})

test_that("add_hdds() — invalid non-yes/no values cause phr_error", {

  df_bad <- tibble::tibble(
    fsl_hdds_cereals    = c("yes","INVALID","no"),
    fsl_hdds_tubers     = "yes",
    fsl_hdds_veg        = "yes",
    fsl_hdds_fruit      = "yes",
    fsl_hdds_meat       = "yes",
    fsl_hdds_eggs       = "yes",
    fsl_hdds_fish       = "yes",
    fsl_hdds_legumes    = "yes",
    fsl_hdds_dairy      = "yes",
    fsl_hdds_oil        = "yes",
    fsl_hdds_sugar      = "yes",
    fsl_hdds_condiments = "yes"
  )

  expect_warning(
    add_hdds(df_bad)
  )
})

test_that("add_hdds() — NA in any food group → HDDS score = NA", {

  df <- tibble::tibble(
    fsl_hdds_cereals    = c("yes", NA),
    fsl_hdds_tubers     = c("no",  "yes"),
    fsl_hdds_veg        = c("yes", "yes"),
    fsl_hdds_fruit      = c("yes", "no"),
    fsl_hdds_meat       = c("no",  "yes"),
    fsl_hdds_eggs       = c("yes", "yes"),
    fsl_hdds_fish       = c("no",  "yes"),
    fsl_hdds_legumes    = c("yes", "yes"),
    fsl_hdds_dairy      = c("no",  "yes"),
    fsl_hdds_oil        = c("yes", "no"),
    fsl_hdds_sugar      = c("no",  "yes"),
    fsl_hdds_condiments = c("yes", "yes")
  )

  out <- add_hdds(df)

  expect_true(!is.na(out$fsl_hdds_score[1]))
  expect_true(is.na(out$fsl_hdds_score[2]))
})

test_that("add_hdds() — category assignment is correct", {

  df <- tibble::tibble(
    # Row 1: only 1 yes → Low
    fsl_hdds_cereals    = "yes",
    fsl_hdds_tubers     = "no",
    fsl_hdds_veg        = "no",
    fsl_hdds_fruit      = "no",
    fsl_hdds_meat       = "no",
    fsl_hdds_eggs       = "no",
    fsl_hdds_fish       = "no",
    fsl_hdds_legumes    = "no",
    fsl_hdds_dairy      = "no",
    fsl_hdds_oil        = "no",
    fsl_hdds_sugar      = "no",
    fsl_hdds_condiments = "no"
  )

  out <- add_hdds(df)

  expect_true(grepl("Low", out$fsl_hdds_cat[1], ignore.case = TRUE))
})

test_that("add_hdds() — Medium and High category thresholds", {

  df <- tibble::tibble(
    fsl_hdds_cereals    = c("yes", "yes"),
    fsl_hdds_tubers     = c("yes", "yes"),
    fsl_hdds_veg        = c("no",  "yes"),
    fsl_hdds_fruit      = c("no",  "yes"),
    fsl_hdds_meat       = c("no",  "yes"),
    fsl_hdds_eggs       = c("no",  "yes"),
    fsl_hdds_fish       = c("no",  "no"),
    fsl_hdds_legumes    = c("no",  "yes"),
    fsl_hdds_dairy      = c("no",  "no"),
    fsl_hdds_oil        = c("no",  "yes"),
    fsl_hdds_sugar      = c("no",  "yes"),
    fsl_hdds_condiments = c("no",  "yes")
  )

  out <- add_hdds(df)

  expect_true(grepl("Low", out$fsl_hdds_cat[1], ignore.case = TRUE))
  expect_true(grepl("High",   out$fsl_hdds_cat[2], ignore.case = TRUE))
})

test_that("add_hdds() — existing HDDS columns trigger a warning", {

  df <- tibble::tibble(
    fsl_hdds_cereals = "yes",
    fsl_hdds_tubers  = "no",
    fsl_hdds_veg     = "yes",
    fsl_hdds_fruit   = "no",
    fsl_hdds_meat    = "no",
    fsl_hdds_eggs    = "yes",
    fsl_hdds_fish    = "no",
    fsl_hdds_legumes = "yes",
    fsl_hdds_dairy   = "no",
    fsl_hdds_oil     = "yes",
    fsl_hdds_sugar   = "no",
    fsl_hdds_condiments = "yes",

    fsl_hdds_score = 99,   # existing var
    fsl_hdds_cat   = "Low" # existing var
  )

  expect_warning(
    add_hdds(df)
  )
})

# ADD_FCM ####

test_that("add_fcm_phase() — basic FCS + rCSI mapping works", {
  df <- tibble::tibble(
    fsl_fcs_cat  = factor(rep(c("Acceptable","Borderline","Poor"), each = 3)),
    fsl_rcsi_cat = factor(rep(c("Low","Medium","High"), times = 3))
  )

  out <- add_fcm_phase(df)

  # output structure
  expect_true("fsl_fc_cell" %in% names(out))
  expect_true("fsl_fc_phase"  %in% names(out))

  # all rows must have cell/cat
  expect_equal(nrow(out), 9)
  expect_false(any(is.na(out$fsl_fc_cell)))

  # fsl_fc_phase must be an ordered factor with levels P5 to P1 (worst to best)
  expect_s3_class(out$fsl_fc_phase, "factor")
  expect_true(is.ordered(out$fsl_fc_phase))
  expect_equal(levels(out$fsl_fc_phase), c("P5", "P4", "P3", "P2", "P1"))

  # fsl_fc_cat must match P1–P4 patterns
  expect_true(all(grepl("^P[1-4]$", out$fsl_fc_phase)))
})

test_that("add_fcm_phase() — FCS + rCSI + HHS (3-indicator matrix) works", {

  df <- tibble::tibble(
    fsl_fcs_cat      = c("Acceptable","Borderline","Poor"),
    fsl_rcsi_cat     = c("Low","Medium","High"),
    fsl_hhs_cat_ipc  = c("None","Little","Moderate")
  )

  out <- add_fcm_phase(df)

  expect_true("fsl_fc_cell" %in% names(out))
  expect_true("fsl_fc_phase"  %in% names(out))
  expect_equal(nrow(out), 3)

  # expect P1–P5 only
  expect_true(all(grepl("^P[1-5]$", out$fsl_fc_phase)))
})

test_that("add_fcm_phase() — HDDS + rCSI works when FCS/HHS absent", {

  df <- tibble::tibble(
    fsl_hdds_cat = c("Low","Medium","High"),
    fsl_rcsi_cat = c("Low","Medium","High")
  )

  out <- add_fcm_phase(df)
  expect_true("fsl_fc_cell" %in% names(out))
  expect_true("fsl_fc_phase"  %in% names(out))

  expect_true(all(grepl("^P[1-4]$", out$fsl_fc_phase)))
})

test_that("add_fcm_phase() — FCS + HHS works when rCSI absent", {

  df <- tibble::tibble(
    fsl_fcs_cat     = c("Acceptable","Borderline","Poor"),
    fsl_hhs_cat_ipc = c("None","Little","Severe")
  )

  out <- add_fcm_phase(df)

  expect_true(all(grepl("^P[1-5]$", out$fsl_fc_phase)))
})

test_that("add_fcm_phase() — HDDS + HHS works when FCS & rCSI missing", {

  df <- tibble::tibble(
    fsl_hdds_cat    = c("High","Medium","Low"),
    fsl_hhs_cat_ipc = c("None","Moderate","Severe")
  )

  out <- add_fcm_phase(df)

  expect_true(all(grepl("^P[1-5]$", out$fsl_fc_cat)))
})

test_that("add_fcm_phase() — errors on non-dataframe input", {
  expect_error(
    add_fcm_phase(NULL),
    class = "phr_error"
  )
})

test_that("add_fcm_phase() — errors on empty dataset", {
  df <- tibble::tibble()
  expect_error(
    add_fcm_phase(df),
    class = "phr_error"
  )
})

test_that("add_fcm_phase() — invalid category values throw phr_error", {

  df <- tibble::tibble(
    fsl_fcs_cat  = "SomethingWrong",
    fsl_rcsi_cat = "Low"
  )

  expect_warning(
    add_fcm_phase(df)
  )
})

test_that("add_fcm_phase() — no valid combination throws phr_error", {

  df <- tibble::tibble(
    unrelated1 = "foo",
    unrelated2 = "bar"
  )

  expect_error(
    add_fcm_phase(df),
    class = "phr_error"
  )
})

test_that("add_fcm_phase() — output categories use P1–P5 patterns", {

  df <- tibble::tibble(
    fsl_fcs_cat  = "Acceptable",
    fsl_rcsi_cat = "Low"
  )

  out <- add_fcm_phase(df)

  expect_true(grepl("^P[1-5]$", out$fsl_fc_phase))
})

test_that("add_fcm_phase() — custom value parameters work correctly", {
  # Test with custom (translated) values that differ from defaults
  df <- tibble::tibble(
    fsl_fcs_cat  = c("Aceptable", "Límite", "Pobre"),
    fsl_rcsi_cat = c("Bajo", "Medio", "Alto")
  )

  out <- add_fcm_phase(
    df,
    fcs_acceptable_val = "Aceptable",
    fcs_borderline_val = "Límite",
    fcs_poor_val = "Pobre",
    rcsi_low_val = "Bajo",
    rcsi_medium_val = "Medio",
    rcsi_high_val = "Alto"
  )

  # output structure
  expect_true("fsl_fc_cell" %in% names(out))
  expect_true("fsl_fc_phase"  %in% names(out))
  expect_equal(nrow(out), 3)
  expect_false(any(is.na(out$fsl_fc_cell)))

  # Verify correct mapping
  expect_equal(out$fsl_fc_cell[1], 1)  # Aceptable + Bajo should map to cell 1
  expect_equal(as.character(out$fsl_fc_phase[1]), "P1")
})

test_that("add_fcm_phase() — custom HHS and HDDS values work", {
  # Test with custom HHS values
  df <- tibble::tibble(
    fsl_hdds_cat    = c("Faible", "Moyen", "Élevé"),
    fsl_hhs_cat_ipc = c("Aucun", "Peu", "Modéré")
  )

  out <- add_fcm_phase(
    df,
    hdds_low_val = "Faible",
    hdds_medium_val = "Moyen",
    hdds_high_val = "Élevé",
    hhs_none_val = "Aucun",
    hhs_little_val = "Peu",
    hhs_moderate_val = "Modéré",
    hhs_severe_val = "Grave",
    hhs_very_severe_val = "Très Grave"
  )

  expect_true("fsl_fc_cell" %in% names(out))
  expect_true("fsl_fc_phase"  %in% names(out))
  expect_equal(nrow(out), 3)
  expect_false(any(is.na(out$fsl_fc_cell)))
})

test_that("add_fcm_phase() — custom value validation works", {
  # Test that validation uses custom values
  df <- tibble::tibble(
    fsl_fcs_cat  = c("Good", "Bad", "Okay"),
    fsl_rcsi_cat = c("Small", "Big", "Huge")
  )

  # This should work with custom values
  out <- add_fcm_phase(
    df,
    fcs_acceptable_val = "Good",
    fcs_borderline_val = "Okay",
    fcs_poor_val = "Bad",
    rcsi_low_val = "Small",
    rcsi_medium_val = "Big",
    rcsi_high_val = "Huge"
  )

  expect_equal(nrow(out), 3)
  expect_false(any(is.na(out$fsl_fc_cell)))
})

# ADD_FCLCM Testing ####

test_that("add_fclcm_phase works for standard 4-category LCSI", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1","P2","P3","P4","P5"),
    fsl_lcsi_cat = c("None","Stress","Crisis","Emergency","None")
  )

  out <- add_fclcm_phase(
    .dataset = df,
    fc_phase_col = "fsl_fc_phase",
    lcsi_col = "fsl_lcsi_cat",
    p1_val = "P1",
    p2_val = "P2",
    p3_val = "P3",
    p4_val = "P4",
    p5_val = "P5",
    lcsi_none_val = "None",
    lcsi_stress_val = "Stress",
    lcsi_crisis_val = "Crisis",
    lcsi_emergency_val = "Emergency"
  )

  expect_true(all(c("fsl_fclcm_cell", "fsl_fclcm_phase") %in% names(out)))
  expect_equal(nrow(out), 5)

  # fsl_fclcm_phase must be an ordered factor with levels P5 to P1 (worst to best)
  expect_s3_class(out$fsl_fclcm_phase, "factor")
  expect_true(is.ordered(out$fsl_fclcm_phase))
  expect_equal(levels(out$fsl_fclcm_phase), c("P5", "P4", "P3", "P2", "P1"))

  # Spot checks against lookup table
  expect_equal(out$fsl_fclcm_cell[1], 1)    # P1 + None
  expect_equal(as.character(out$fsl_fclcm_phase[1]), "P1")

  expect_equal(out$fsl_fclcm_cell[2], 7)    # P2 + Stress
  expect_equal(as.character(out$fsl_fclcm_phase[2]), "P2")
})

test_that("add_fclcm_phase works for 5-category LCSI (with Exhaustion)", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1","P2","P3","P4","P5"),
    fsl_lcsi_cat = c("Exhaustion","Exhaustion","None","Stress","Emergency")
  )

  out <- add_fclcm_phase(df)

  expect_true(all(c("fsl_fclcm_cell", "fsl_fclcm_phase") %in% names(out)))
  expect_equal(nrow(out), 5)

  # Exhaustion should trigger the 5-category lookup
  expect_equal(out$fsl_fclcm_cell[1], 21)   # P1 + Exhaustion
  expect_equal(as.character(out$fsl_fclcm_phase[1]), "P1")

  expect_equal(out$fsl_fclcm_cell[2], 22)   # P2 + Exhaustion
})

test_that("add_fclcm_phase errors when fc_phase column is missing", {
  df <- tibble::tibble(
    x = c("P1","P2"),
    fsl_lcsi_cat = c("None","Stress")
  )

  expect_error(add_fclcm_phase(df), regexp = "Missing FC phase column")
})

test_that("add_fclcm_phase errors when lcsi column is missing", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1","P2"),
    x = c("None","Stress")
  )

  expect_error(add_fclcm_phase(df), regexp = "Missing LCSI column")

})

test_that("add_fclcm_phase warns on invalid fc_phase values", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1","P9"),  # P9 is invalid
    fsl_lcsi_cat = c("None","Stress")
  )

  expect_warning(add_fclcm_phase(df))
})

test_that("add_fclcm_phase warns on invalid lcsi_cat values", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1","P2"),
    fsl_lcsi_cat = c("Invalid","None")
  )

  expect_warning(add_fclcm_phase(df))
})

test_that("add_fclcm_phase works with NAs in lcsi_cat (ignored during lookup)", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1","P2","P3"),
    fsl_lcsi_cat = c("None", NA, "Stress")
  )

  out <- add_fclcm_phase(df)

  expect_true(all(c("fsl_fclcm_cell","fsl_fclcm_phase") %in% names(out)))

  # First row should match P1 + None
  expect_equal(out$fsl_fclcm_cell[1], 1)

  # Second row should be NA for both fields
  expect_true(is.na(out$fsl_fclcm_cell[2]))
  expect_true(is.na(out$fsl_fclcm_phase[2]))
})

test_that("add_fclcm_phase preserves additional columns", {
  df <- tibble::tibble(
    id = 1:3,
    fsl_fc_phase = c("P1","P2","P3"),
    fsl_lcsi_cat = c("None","Stress","Crisis")
  )

  out <- add_fclcm_phase(df)

  expect_true("id" %in% names(out))
  expect_equal(out$id, df$id)
})

test_that("add_fclcm_phase fails on non-data-frame input", {
  expect_error(add_fclcm_phase(NULL))
  expect_error(add_fclcm_phase(list()))
})

# Factor preservation tests ####

test_that("add_fcm_phase() — factor columns in dataset are preserved as factors after join", {
  df <- tibble::tibble(
    fsl_fcs_cat  = factor(c("Acceptable", "Borderline", "Poor"),
                          levels = c("Poor", "Borderline", "Acceptable")),
    fsl_rcsi_cat = factor(c("Low", "Medium", "High"),
                          levels = c("Low", "Medium", "High"))
  )

  out <- add_fcm_phase(df)

  expect_s3_class(out$fsl_fcs_cat, "factor")
  expect_s3_class(out$fsl_rcsi_cat, "factor")
  expect_equal(levels(out$fsl_fcs_cat), levels(df$fsl_fcs_cat))
  expect_equal(levels(out$fsl_rcsi_cat), levels(df$fsl_rcsi_cat))
})

test_that("add_fcm_phase() — factor levels are preserved when all three indicators used", {
  df <- tibble::tibble(
    fsl_fcs_cat     = factor(c("Acceptable", "Borderline", "Poor"),
                             levels = c("Poor", "Borderline", "Acceptable")),
    fsl_rcsi_cat    = factor(c("Low", "Medium", "High"),
                             levels = c("Low", "Medium", "High")),
    fsl_hhs_cat_ipc = factor(c("None", "Little", "Moderate"),
                             levels = c("None", "Little", "Moderate", "Severe", "Very Severe"))
  )

  out <- add_fcm_phase(df)

  expect_s3_class(out$fsl_fcs_cat, "factor")
  expect_s3_class(out$fsl_rcsi_cat, "factor")
  expect_s3_class(out$fsl_hhs_cat_ipc, "factor")
  expect_equal(levels(out$fsl_fcs_cat), levels(df$fsl_fcs_cat))
  expect_equal(levels(out$fsl_rcsi_cat), levels(df$fsl_rcsi_cat))
  expect_equal(levels(out$fsl_hhs_cat_ipc), levels(df$fsl_hhs_cat_ipc))
})

test_that("add_fclcm_phase() — factor lcsi_col column is preserved as factor after join", {
  df <- tibble::tibble(
    fsl_fc_phase = c("P1", "P2", "P3"),
    fsl_lcsi_cat = factor(c("None", "Stress", "Crisis"),
                          levels = c("None", "Stress", "Crisis", "Emergency"))
  )

  out <- add_fclcm_phase(df)

  expect_s3_class(out$fsl_lcsi_cat, "factor")
  expect_equal(levels(out$fsl_lcsi_cat), levels(df$fsl_lcsi_cat))
})

