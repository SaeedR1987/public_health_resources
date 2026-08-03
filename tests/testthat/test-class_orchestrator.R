library(testthat)

context("Orchestrator class tests")

# Basic initialization
test_that("Orchestrator initialize sets metadata", {
  inst <- Orchestrator$new()
  expect_s3_class(inst$metadata$created_datetime, "POSIXct")
  expect_s3_class(inst$metadata$modified_datetime, "POSIXct")
  expect_true(
    as.numeric(inst$metadata$modified_datetime) >=
      as.numeric(inst$metadata$created_datetime)
  )
})
