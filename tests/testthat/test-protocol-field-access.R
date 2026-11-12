test_that("SurveyProtocol exposes tools, framework, sample_object and sampling_frame via get/set/call", {
  p <- SurveyProtocol$new(
    assessment_title = "t",
    country_name = "c",
    month_year = "Jan 2026",
    framework_type = "ana"
  )

  expect_s3_class(p$get(field = "framework"), "Framework")
  expect_true(is.list(p$get(field = "tools")))

  p$set(field = "framework", member = "secondary_data_sources", value = NULL)
  expect_silent(p$get(field = "sample_object"))
  expect_silent(p$get(field = "sampling_frame"))

  expect_error(p$get(field = "not_a_field"))
})

test_that("ANAFramework allows setting secondary_ana_goal through the protocol", {
  p <- SurveyProtocol$new(framework_type = "ana")
  p$set(field = "framework", member = "secondary_ana_goal", value = TRUE)
  expect_true(p$get(field = "framework", member = "secondary_ana_goal"))
})
