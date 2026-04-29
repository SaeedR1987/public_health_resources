# Tests for calculate_sample_size_general(), calculate_sample_size_individual(),
# and calculate_sample_size_mortality() in R/utils_sample_size.R.

# ---- calculate_sample_size_general ----------------------------------------

test_that("calculate_sample_size_general returns a positive integer", {
  result <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
  expect_true(result > 0)
  expect_equal(result, ceiling(result))  # ceiling returns an integer-valued numeric
})

test_that("calculate_sample_size_general simple random design produces expected size", {
  # z ~ 1.96, p=0.5, d=0.05 → n = (1.96^2 * 0.5 * 0.5) / 0.05^2 = 384.16
  # non_response=5% → 384.16 / 0.95 = 404.38 → ceiling = 405
  result <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 5,
    design              = "simple_random",
    confidence_level    = 0.95
  )
  expect_equal(result, 405L)
})

test_that("calculate_sample_size_general cluster design applies design effect", {
  simple <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 0,
    design              = "simple_random",
    confidence_level    = 0.95
  )
  cluster <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 0,
    design              = "cluster",
    design_effect       = 2,
    confidence_level    = 0.95
  )
  expect_true(cluster > simple)
})

test_that("calculate_sample_size_general with fpc reduces sample size", {
  no_fpc <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 0,
    design              = "simple_random",
    fpc                 = FALSE,
    confidence_level    = 0.95
  )
  with_fpc <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 0,
    design              = "simple_random",
    fpc                 = TRUE,
    total_population    = 500,
    confidence_level    = 0.95
  )
  expect_true(with_fpc < no_fpc)
})

test_that("calculate_sample_size_general non_response_rate increases sample size", {
  no_nr <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 0
  )
  with_nr <- calculate_sample_size_general(
    expected_proportion = 50,
    desired_precision   = 5,
    non_response_rate   = 20
  )
  expect_true(with_nr > no_nr)
})

test_that("calculate_sample_size_general errors on invalid expected_proportion", {
  expect_error(calculate_sample_size_general(-1, 5))
  expect_error(calculate_sample_size_general(101, 5))
})

test_that("calculate_sample_size_general errors on invalid desired_precision", {
  expect_error(calculate_sample_size_general(50, 0))
  expect_error(calculate_sample_size_general(50, 100))
})

test_that("calculate_sample_size_general errors on invalid non_response_rate", {
  expect_error(calculate_sample_size_general(50, 5, non_response_rate = -1))
  expect_error(calculate_sample_size_general(50, 5, non_response_rate = 100))
})

test_that("calculate_sample_size_general errors on unknown design", {
  expect_error(calculate_sample_size_general(50, 5, design = "stratified"))
})

test_that("calculate_sample_size_general errors when design_effect < 1 for cluster", {
  expect_error(
    calculate_sample_size_general(50, 5, design = "cluster", design_effect = 0.5)
  )
})

test_that("calculate_sample_size_general errors when fpc=TRUE but total_population missing", {
  expect_error(
    calculate_sample_size_general(50, 5, fpc = TRUE)
  )
  expect_error(
    calculate_sample_size_general(50, 5, fpc = TRUE, total_population = 0)
  )
})

# ---- calculate_sample_size_individual -------------------------------------

test_that("calculate_sample_size_individual returns a named list", {
  result <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5
  )
  expect_true(is.list(result))
  expect_named(result, c("sample_size_individuals", "sample_size_households",
                          "average_household_size", "sub_population_percent"))
})

test_that("calculate_sample_size_individual sample_size_households is derived from individuals", {
  result <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    non_response_rate      = 0
  )
  expected_hh <- ceiling(result$sample_size_individuals / 5)
  expect_equal(result$sample_size_households, expected_hh)
})

test_that("calculate_sample_size_individual sub_population_percent increases individual sample size", {
  full_pop <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    sub_population_percent = 100,
    non_response_rate      = 0
  )
  sub_pop <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    sub_population_percent = 50,
    non_response_rate      = 0
  )
  expect_true(sub_pop$sample_size_individuals > full_pop$sample_size_individuals)
})

test_that("calculate_sample_size_individual preserves average_household_size in return value", {
  result <- calculate_sample_size_individual(
    expected_proportion    = 25,
    desired_precision      = 5,
    average_household_size = 6.2
  )
  expect_equal(result$average_household_size, 6.2)
})

test_that("calculate_sample_size_individual errors when average_household_size is missing or <= 0", {
  expect_error(
    calculate_sample_size_individual(
      expected_proportion = 30,
      desired_precision   = 5
    )
  )
  expect_error(
    calculate_sample_size_individual(
      expected_proportion    = 30,
      desired_precision      = 5,
      average_household_size = 0
    )
  )
})

test_that("calculate_sample_size_individual errors on invalid sub_population_percent", {
  expect_error(
    calculate_sample_size_individual(
      expected_proportion    = 30,
      desired_precision      = 5,
      average_household_size = 5,
      sub_population_percent = 0
    )
  )
  expect_error(
    calculate_sample_size_individual(
      expected_proportion    = 30,
      desired_precision      = 5,
      average_household_size = 5,
      sub_population_percent = 101
    )
  )
})

test_that("calculate_sample_size_individual with fpc=TRUE reduces sample size", {
  no_fpc <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    non_response_rate      = 0,
    fpc                    = FALSE
  )
  with_fpc <- calculate_sample_size_individual(
    expected_proportion    = 30,
    desired_precision      = 5,
    average_household_size = 5,
    non_response_rate      = 0,
    fpc                    = TRUE,
    total_population       = 500
  )
  expect_true(with_fpc$sample_size_individuals < no_fpc$sample_size_individuals)
})

# ---- calculate_sample_size_mortality --------------------------------------

test_that("calculate_sample_size_mortality returns a named list", {
  result <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5
  )
  expect_true(is.list(result))
  expect_named(result, c("sample_size_households", "sample_size_individuals",
                          "sample_size_person_time", "expected_deaths",
                          "recall_days", "design_effect"))
})

test_that("calculate_sample_size_mortality returns positive numeric values", {
  result <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5
  )
  expect_true(result$sample_size_households > 0)
  expect_true(result$sample_size_individuals > 0)
  expect_true(result$sample_size_person_time > 0)
})

test_that("calculate_sample_size_mortality default design is cluster and applies design_effect", {
  simple <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    design                 = "simple_random",
    non_response_rate      = 0
  )
  cluster <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    design                 = "cluster",
    design_effect          = 1.5,
    non_response_rate      = 0
  )
  expect_true(cluster$sample_size_households > simple$sample_size_households)
})

test_that("calculate_sample_size_mortality design_effect is 1 for simple_random in return value", {
  result <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    design                 = "simple_random"
  )
  expect_equal(result$design_effect, 1)
})

test_that("calculate_sample_size_mortality recall_days is preserved in return value", {
  result <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    recall_days            = 60
  )
  expect_equal(result$recall_days, 60)
})

test_that("calculate_sample_size_mortality longer recall_days increases sample size", {
  short <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    recall_days            = 30,
    non_response_rate      = 0
  )
  long <- calculate_sample_size_mortality(
    expected_death_rate    = 0.5,
    desired_precision      = 0.2,
    average_household_size = 5,
    recall_days            = 90,
    non_response_rate      = 0
  )
  expect_true(long$sample_size_person_time > short$sample_size_person_time)
})

test_that("calculate_sample_size_mortality errors when expected_death_rate <= 0", {
  expect_error(
    calculate_sample_size_mortality(0, 0.2, average_household_size = 5)
  )
})

test_that("calculate_sample_size_mortality errors when desired_precision <= 0", {
  expect_error(
    calculate_sample_size_mortality(0.5, 0, average_household_size = 5)
  )
})

test_that("calculate_sample_size_mortality errors when average_household_size is missing or <= 0", {
  expect_error(
    calculate_sample_size_mortality(0.5, 0.2)
  )
  expect_error(
    calculate_sample_size_mortality(0.5, 0.2, average_household_size = 0)
  )
})

test_that("calculate_sample_size_mortality errors on invalid recall_days", {
  expect_error(
    calculate_sample_size_mortality(0.5, 0.2, average_household_size = 5, recall_days = 0)
  )
})

test_that("calculate_sample_size_mortality errors on unknown design", {
  expect_error(
    calculate_sample_size_mortality(0.5, 0.2, average_household_size = 5,
                                    design = "stratified")
  )
})

test_that("calculate_sample_size_mortality fpc=TRUE requires total_population", {
  expect_error(
    calculate_sample_size_mortality(0.5, 0.2, average_household_size = 5,
                                    fpc = TRUE)
  )
})
