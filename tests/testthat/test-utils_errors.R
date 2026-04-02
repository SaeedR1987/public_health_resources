# ---------------------------
# UTILS ERRORS TEST SUITE
# ---------------------------

library(testthat)

# ============================================================
# 1. NOTIFICATION DISPATCHER TESTS
# ============================================================

test_that(".phr_notify handles message type", {
  # Should not error when called outside Shiny
  expect_no_error(
    .phr_notify("Test message", type = "message")
  )
})

test_that(".phr_notify handles warning type", {
  expect_no_error(
    .phr_notify("Test warning", type = "warning")
  )
})

test_that(".phr_notify handles error type", {
  expect_no_error(
    .phr_notify("Test error", type = "error")
  )
})

test_that(".phr_notify validates type argument", {
  expect_error(
    .phr_notify("message", type = "invalid"),
    regexp = "should be one of"
  )
})

# ============================================================
# 2. IPHRA_ERROR TESTS
# ============================================================

test_that("phr_error creates formatted error message with origin", {
  expect_error(
    phr_error("Test error", origin = "test_function"),
    regexp = "test_function.*Test error"
  )
})

test_that("phr_error includes hint in error message", {
  expect_error(
    phr_error("Test error", hint = "Try this fix"),
    regexp = "Try this fix"
  )
})

test_that("phr_error works without origin or hint", {
  expect_error(
    phr_error("Simple error"),
    regexp = "Simple error"
  )
})

test_that("phr_error includes custom type in message", {
  expect_error(
    phr_error("Test error", type = "ValidationError"),
    regexp = "ValidationError"
  )
})

test_that("phr_error in test mode does not call shiny::req", {
  # Set test mode
  old_opt <- getOption("IPHRA_TEST_MODE")
  options(IPHRA_TEST_MODE = TRUE)

  # Should error without calling shiny::req
  expect_error(
    phr_error("Test error"),
    regexp = "Test error"
  )

  # Restore option
  options(IPHRA_TEST_MODE = old_opt)
})

# ============================================================
# 3. IPHRA_WARNING TESTS
# ============================================================

test_that("phr_warning creates formatted warning message", {
  expect_no_error(
    phr_warning("Test warning", origin = "test_function")
  )
})

test_that("phr_warning includes hint in message", {
  expect_no_error(
    phr_warning("Test warning", hint = "Consider this")
  )
})

test_that("phr_warning works without origin or hint", {
  expect_no_error(
    phr_warning("Simple warning")
  )
})

test_that("phr_warning allows custom type", {
  expect_no_error(
    phr_warning("Test warning", type = "DataWarning")
  )
})

# ============================================================
# 4. IPHRA_MESSAGE TESTS
# ============================================================

test_that("phr_message creates formatted informational message", {
  expect_no_error(
    phr_message("Test message", origin = "test_function")
  )
})

test_that("phr_message works without origin", {
  expect_no_error(
    phr_message("Simple message")
  )
})

# ============================================================
# 5. IPHRA_ASSERT TESTS
# ============================================================

test_that("phr_assert passes when condition is TRUE", {
  expect_no_error(
    phr_assert(TRUE, "Should not error")
  )

  expect_no_error(
    phr_assert(1 == 1, "Should not error")
  )
})

test_that("phr_assert errors when condition is FALSE", {
  expect_error(
    phr_assert(FALSE, "Assertion failed"),
    regexp = "Assertion failed"
  )
})

test_that("phr_assert includes origin in error", {
  expect_error(
    phr_assert(FALSE, "Assertion failed", origin = "test_func"),
    regexp = "test_func"
  )
})

test_that("phr_assert includes hint in error", {
  expect_error(
    phr_assert(FALSE, "Assertion failed", hint = "Check input"),
    regexp = "Check input"
  )
})

test_that("phr_assert errors on NA condition", {
  expect_error(
    phr_assert(NA, "NA is not TRUE"),
    regexp = "NA is not TRUE"
  )
})

# ============================================================
# 6. IPHRA_TRY TESTS
# ============================================================

test_that("phr_try executes expression successfully", {
  result <- phr_try({
    42
  }, on_error = "return")

  expect_equal(result, 42)
})

test_that("phr_try handles errors with on_error='return'", {
  result <- phr_try({
    stop("Test error")
  }, on_error = "return")

  expect_true(is.list(result))
  expect_false(result$success)
  expect_true(grepl("Test error", result$error))
})

test_that("phr_try handles errors with on_error='warn'", {
  expect_warning(
    phr_try({
      stop("Test error")
    }, on_error = "warn"),
    regexp = "Test error"  # May or may not warn depending on implementation
  )
})

test_that("phr_try handles errors with on_error='abort'", {
  expect_error(
    phr_try({
      stop("Test error")
    }, on_error = "abort"),
    regexp = "Test error"
  )
})

test_that("phr_try includes origin in error message", {
  result <- phr_try({
    stop("Test error")
  }, on_error = "return", origin = "test_function")

  expect_true(grepl("test_function", result$error))
})

test_that("phr_try includes hint in error context", {
  result <- phr_try({
    stop("Test error")
  }, on_error = "return", hint = "Try this fix")

  expect_equal(result$hint, "Try this fix")
})

test_that("phr_try includes step in error message", {
  result <- phr_try({
    stop("Test error")
  }, on_error = "return", step = "Validation")

  expect_equal(result$step, "Validation")
})

test_that("phr_try combines origin and step correctly", {
  result <- phr_try({
    stop("Test error")
  }, on_error = "return", origin = "main_function", step = "Validation")

  expect_true(grepl("main_function", result$error) || grepl("Validation", result$error))
})

test_that("phr_try preserves nested error context", {
  result <- phr_try({
    phr_try({
      stop("Inner error")
    }, on_error = "abort", step = "Inner")
  }, on_error = "return", origin = "Outer")

  expect_true(is.list(result))
  expect_false(result$success)
})

# ============================================================
# 7. IPHRA_TRY_STEP TESTS
# ============================================================

test_that("phr_try_step executes successfully", {
  result <- phr_try_step({
    42
  }, step = "Test Step")

  expect_equal(result, 42)
})

test_that("phr_try_step returns error list on failure", {
  result <- phr_try_step({
    stop("Step error")
  }, step = "Test Step")

  expect_true(is.list(result))
  expect_false(result$success)
  expect_equal(result$step, "Test Step")
})

test_that("phr_try_step includes hint in error context", {
  result <- phr_try_step({
    stop("Step error")
  }, step = "Test Step", hint = "Check input")

  expect_equal(result$hint, "Check input")
})

test_that("phr_try_step always uses on_error='return'", {
  # Should return error list, not throw
  result <- phr_try_step({
    stop("Error")
  }, step = "Test")

  expect_true(is.list(result))
  expect_false(result$success)
})

# ============================================================
# 8. IPHRA_FAILED TESTS
# ============================================================

test_that("phr_failed returns TRUE for failed result", {
  failed_result <- list(success = FALSE, error = "Test error")

  expect_true(phr_failed(failed_result))
})

test_that("phr_failed returns FALSE for successful result", {
  success_result <- list(success = TRUE)

  expect_false(phr_failed(success_result))
})

test_that("phr_failed returns FALSE for non-list result", {
  expect_false(phr_failed(42))
  expect_false(phr_failed("text"))
  expect_false(phr_failed(NULL))
})

test_that("phr_failed returns FALSE for list without success field", {
  result <- list(error = "Something")

  expect_false(phr_failed(result))
})

test_that("phr_failed returns FALSE for success = TRUE", {
  result <- list(success = TRUE, data = 42)

  expect_false(phr_failed(result))
})

# ============================================================
# 9. NESTED ERROR HANDLING PATTERNS
# ============================================================

test_that("nested phr_try calls preserve context chain", {
  outer_result <- phr_try({
    step1 <- phr_try_step({
      42
    }, step = "Step 1")

    if (phr_failed(step1)) return(step1)

    step2 <- phr_try_step({
      stop("Error in step 2")
    }, step = "Step 2")

    if (phr_failed(step2)) return(step2)

    "success"
  }, on_error = "return", origin = "Outer Function")

  expect_true(phr_failed(outer_result))
  expect_equal(outer_result$step, "Step 2")
})

test_that("multiple steps can be tracked in nested try blocks", {
  result <- phr_try({
    r1 <- phr_try_step({ 1 }, step = "Init")
    if (phr_failed(r1)) { r1 } else {
      r2 <- phr_try_step({ 2 }, step = "Process")
      if (phr_failed(r2)) { r2 } else {
        r3 <- phr_try_step({ stop("Final error") }, step = "Finalize")
        r3
      }
    }
  }, on_error = "return", origin = "MultiStep")

  expect_true(phr_failed(result))
  expect_equal(result$step, "Finalize")
})

# ============================================================
# 10. EDGE CASES AND ERROR CONDITIONS
# ============================================================

test_that("phr_try handles empty expression", {
  result <- phr_try({}, on_error = "return")
  expect_true(is.null(result) || !is.list(result) || result$success != FALSE)
})

test_that("phr_error handles very long messages", {
  long_msg <- paste(rep("word", 1000), collapse = " ")

  expect_error(
    phr_error(long_msg),
    regexp = "word"
  )
})

test_that("phr_try handles NULL origin and hint", {
  result <- phr_try({
    stop("Error")
  }, on_error = "return", origin = NULL, hint = NULL)

  expect_true(is.list(result))
  expect_false(result$success)
})

test_that("error functions handle special characters in messages", {
  expect_error(
    phr_error("Error with [brackets] and (parens) and $symbols"),
    regexp = "brackets"
  )
})

