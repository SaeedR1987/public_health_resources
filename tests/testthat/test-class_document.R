library(testthat)

test_that("Document initializes with inherited metadata and default templates", {
  document <- Document$new()

  expect_s3_class(document$metadata$created_datetime, "POSIXct")
  expect_s3_class(document$metadata$modified_datetime, "POSIXct")
  expect_equal(
    document$reference_doc_filename,
    c("reach_tor_template.docx", "protocol_report_template.docx")
  )
  expect_equal(
    document$reference_ppt_filename,
    c("protocol_report_template.pptx")
  )
})

test_that("Document exposes R version through active binding and quarto params", {
  document <- Document$new()
  params <- document$get_quarto_params()

  expect_type(document$.r_version, "character")
  expect_true(nzchar(document$.r_version))
  expect_type(params, "list")
  expect_false(is.null(names(params)))
  expect_true("r_version" %in% names(params))
  expect_identical(params$r_version, document$.r_version)
})

test_that("Document validates missing Quarto template files", {
  document <- Document$new()
  missing_doc_template <- file.path(
    "tests", "testthat", "missing-quarto-doc-template.qmd"
  )
  missing_ppt_template <- file.path(
    "tests", "testthat", "missing-quarto-ppt-template.qmd"
  )

  expect_error(
    document$generate_quarto_doc(template_file = missing_doc_template)
  )
  expect_error(
    document$generate_quarto_ppt(template_file = missing_ppt_template)
  )
})

test_that("Document .r_version active binding is read-only", {
  document <- Document$new()
  current_version <- document$.r_version
  write_attempt <- withVisible(document$.r_version <- "override")

  expect_false(write_attempt$visible)
  expect_false(write_attempt$value)
  expect_identical(document$.r_version, current_version)
})
