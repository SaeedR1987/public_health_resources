test_that("DataAnalytics retains constructor data and provenance", {
  data <- tibble::tibble(id = 1:3, weight = c(1, 2, 1))
  parent <- suppressMessages(Data$new(
    data = data, dataset_name = "households", uuid = "id"
  ))
  analytics <- suppressWarnings(suppressMessages(DataAnalytics$new(
    data = data,
    parent_data_object = parent,
    dataset_name = "mortality",
    data_stage_name = "standardized",
    data_hash = "household-hash",
    variable_map = list(weight = "weight"),
    value_map = list(status = c(alive = 1)),
    variable_label = list(id = "Identifier"),
    value_label = list(status = c("1" = "Alive"))
  )))

  expect_identical(analytics$get("..data"), data)
  expect_identical(analytics$get("..parent_data_object"), parent)
  expect_identical(analytics$get("..dataset_name"), "mortality")
  expect_identical(analytics$get("..data_stage_name"), "standardized")
  expect_identical(analytics$get("..data_hash"), "household-hash")
  expect_identical(analytics$get("..variable_map"), list(weight = "weight"))
  expect_identical(analytics$get("..value_map"), list(status = c(alive = 1)))
  expect_identical(analytics$get("..variable_label"), list(id = "Identifier"))
  expect_identical(analytics$get("..value_label"), list(status = c("1" = "Alive")))
  expect_equal(analytics$summary()$n_records, 3L)
  expect_equal(analytics$summary()$n_columns, 2L)
  expect_identical(analytics$summary()$parent_object, "households")
  expect_s3_class(analytics$get("..survey_design"), "tbl_svy")
  expect_s3_class(analytics$get("..data_analysis_plan"), "QuantDataAnalysisPlanLog")
  expect_s3_class(analytics$get("..data_analysis_plan")$get("..log_df"), "tbl_df")
  expect_type(analytics$visualizations, "list")
  expect_type(analytics$tables, "list")
})

test_that("analytics constructors do not return partially initialized objects", {
  expect_error(
    DataAnalytics$new(data = 1:3),
    "Expected a data frame"
  )
  expect_error(
    MortalityDataAnalytics$new(data = 1:3),
    "Expected a data frame"
  )

  BrokenAnalytics <- R6::R6Class(
    inherit = MortalityDataAnalytics,
    private = list(
      ..initialize_load_schemas_mortality = function() stop("broken mortality schema")
    )
  )
  expect_error(
    suppressMessages(BrokenAnalytics$new(data = tibble::tibble(id = 1:2))),
    "broken mortality schema"
  )
})

test_that("mortality generation preserves household data without optional links", {
  data <- tibble::tibble(uuid = c("hh1", "hh2"), consent = "yes")
  household <- suppressMessages(HouseholdData$new(data = data))
  household$set("..standardized_data", data)
  household$set("..clean_data", data[2:1, ])

  for (stage in c("standardized", "clean")) {
    analytics <- suppressMessages(
      household$generate_data_analytics(stage = stage, type = "mortality")
    )
    expect_s3_class(analytics, "MortalityDataAnalytics")
    expect_identical(analytics$get("..data"), household$get_data(stage))
    expect_identical(analytics$get("..parent_data_object"), household)
    expect_identical(analytics$get("..data_stage_name"), stage)
    expect_identical(
      analytics$get("..data_hash"),
      household$get("..metadata", member = "hash_id")
    )
    expect_identical(
      analytics$summary()$dataset_name,
      paste0(household$get("..dataset_name"), "_MortalityDataAnalytics")
    )
    expect_null(analytics$get("..linked_ind_roster_data"))
    expect_null(analytics$get("..linked_ind_deaths_data"))
    expect_s3_class(analytics$get("..data_analysis_plan"), "QuantDataAnalysisPlanLog")
  }
})

test_that("mortality generation uses each linked dataset and its actual fallback stage", {
  data <- tibble::tibble(uuid = c("hh1", "hh2"), consent = "yes")
  household <- suppressMessages(HouseholdData$new(data = data))
  household$set("..standardized_data", data)
  household$set("..clean_data", data)
  roster_df <- tibble::tibble(person_id = c("p1", "p2"), gender = c("male", "female"))
  deaths_df <- tibble::tibble(death_id = c("d1", "d2"), gender = c("female", "male"))
  roster <- suppressWarnings(suppressMessages(IndividualData$new(data = roster_df)))
  deaths <- suppressWarnings(suppressMessages(DeathIndividualData$new(
    data = deaths_df, recall_date = "2025-01-01",
    uuid = "death_id", hh_uuid = "hh_uuid"
  )))
  roster$set("..standardized_data", roster_df)
  deaths$set("..clean_data", deaths_df)
  roster$set("..variable_map", list(sex = "gender"))
  deaths$set("..variable_map", list(sex = "gender"))
  roster$set("..value_label", list(sex = c(male = "Male")))
  household$add_linked_dataset(
    "roster", roster, by_self_role = "uuid", by_other_role = "hh_uuid"
  )
  household$add_linked_dataset(
    "deaths", deaths, by_self_role = "uuid", by_other_role = "hh_uuid"
  )

  for (stage in c("standardized", "clean")) {
    analytics <- suppressMessages(
      household$generate_data_analytics(stage = stage, type = "mortality")
    )
    expect_s3_class(analytics, "MortalityDataAnalytics")
    for (role in c("roster", "deaths")) {
      object <- if (role == "roster") roster else deaths
      expected_data <- if (role == "roster") roster_df else deaths_df
      prefix <- paste0("..linked_ind_", role)
      expect_identical(analytics$get(paste0(prefix, "_data")), expected_data)
      expect_identical(
        analytics$get(paste0(prefix, "_data_stage_name")),
        if (role == "roster") "standardized" else "clean"
      )
      expect_identical(
        analytics$get(paste0(prefix, "_data_hash")),
        object$get("..metadata", member = "hash_id")
      )
      for (field in c("variable_map", "value_map", "variable_label", "value_label")) {
        expect_identical(
          analytics$get(paste0(prefix, "_", field)),
          object$get(paste0("..", field))
        )
      }
      expect_s3_class(analytics$get(paste0("..survey_design_", role)), "tbl_svy")
      expect_s3_class(
        analytics$get(paste0("..data_analysis_plan_", role)),
        "QuantDataAnalysisPlanLog"
      )
      plan <- analytics$get(paste0("..data_analysis_plan_", role))$get("..log_df")
      expect_gt(nrow(plan), 0L)
      expect_true(all(plan$var_name == "gender"))
    }
  }
})

test_that("direct mortality construction supports linked data without maps", {
  analytics <- suppressMessages(MortalityDataAnalytics$new(
    data = tibble::tibble(uuid = c("hh1", "hh2")),
    linked_ind_roster_data = tibble::tibble(person_id = c("p1", "p2")),
    linked_ind_deaths_data = tibble::tibble(death_id = c("d1", "d2"))
  ))
  expect_s3_class(analytics$get("..survey_design_roster"), "tbl_svy")
  expect_s3_class(analytics$get("..survey_design_deaths"), "tbl_svy")
})

test_that("mortality generation reports initialization failures instead of returning an object", {
  household <- suppressMessages(HouseholdData$new(
    data = tibble::tibble(uuid = c("hh1", "hh2"), consent = "yes")
  ))
  household$set("..standardized_data", 1:3)
  expect_warning(
    analytics <- suppressMessages(
      household$generate_data_analytics(stage = "standardized", type = "mortality")
    ),
    "Expected a data frame"
  )
  expect_null(analytics)
})

test_that("analytics analysis plans initialize typed empty logs and reject invalid input", {
  plan <- QuantDataAnalysisPlanLog$new()
  expect_equal(nrow(plan$get("..log_df")), 0L)
  expect_type(plan$get("..log_df")$multiplier, "double")
  expect_identical(plan$summary()$log_name, "Quant Data Analysis Plan")
  expect_error(QuantDataAnalysisPlanLog$new(log_df = 1:3), "Expected a data frame")

  supplied <- tibble::tibble(
    indicator_name = "Population", calculation = "mean", var_name = "count"
  )
  plan <- QuantDataAnalysisPlanLog$new(log_df = supplied)
  expect_identical(plan$get("..log_df")$indicator_name, supplied$indicator_name)
})

test_that("generation passes supplied analysis plans through the inherited registry", {
  data <- tibble::tibble(uuid = c("hh1", "hh2"), consent = "yes")
  household <- suppressMessages(HouseholdData$new(data = data))
  household$set("..standardized_data", data)
  dap <- tibble::tibble(
    indicator_name = "Consent", calculation = "cat", var_name = "consent",
    denom_var = NA_character_, disaggregation = NA_character_,
    multiplier = 100, indicator_unit = "%"
  )

  for (type in c("general", "mortality")) {
    analytics <- suppressMessages(household$generate_data_analytics(
      stage = "standardized", type = type, analysis_config = dap
    ))
    expect_s3_class(analytics, "DataAnalytics")
    expect_identical(analytics$get("..data"), data)
    expect_identical(analytics$get("..data_analysis_plan")$get("..log_df"), dap)
  }
})

test_that("mortality generation supports either optional link alone with raw fallback", {
  data <- tibble::tibble(uuid = c("hh1", "hh2"), consent = "yes")
  household <- suppressMessages(HouseholdData$new(data = data))
  household$set("..standardized_data", data)

  for (role in c("roster", "deaths")) {
    linked_data <- tibble::tibble(uuid = c("p1", "p2"), hh_uuid = c("hh1", "hh2"))
    linked <- if (role == "roster") {
      suppressWarnings(suppressMessages(IndividualData$new(
        data = linked_data, uuid = "uuid", hh_uuid = "hh_uuid"
      )))
    } else {
      suppressWarnings(suppressMessages(DeathIndividualData$new(
        data = linked_data, recall_date = "2025-01-01",
        uuid = "uuid", hh_uuid = "hh_uuid"
      )))
    }
    expect_identical(linked$get_data("raw"), linked_data)
    household$set("..linked_objects", list())
    household$add_linked_dataset(
      role, linked, by_self_role = "uuid", by_other_role = "hh_uuid"
    )
    analytics <- suppressMessages(
      household$generate_data_analytics(stage = "standardized", type = "mortality")
    )
    prefix <- paste0("..linked_ind_", role)
    expect_identical(analytics$get(paste0(prefix, "_data")), linked_data)
    expect_identical(analytics$get(paste0(prefix, "_data_stage_name")), "raw")
    absent_role <- if (role == "roster") "deaths" else "roster"
    expect_null(analytics$get(paste0("..linked_ind_", absent_role, "_data")))
  }
})
