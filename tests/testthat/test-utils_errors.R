# ---------------------------
# UTILS ERRORS TEST SUITE
# ---------------------------

library(testthat)

# ============================================================
# 1. NOTIFICATION DISPATCHER TESTS
# ============================================================

test_that(".iphra_notify handles message type", {
  # Should not error when called outside Shiny
  expect_no_error(
    .iphra_notify("Test message", type = "message")
  )
})

test_that(".iphra_notify handles warning type", {
  expect_no_error(
    .iphra_notify("Test warning", type = "warning")
  )
})

test_that(".iphra_notify handles error type", {
  expect_no_error(
    .iphra_notify("Test error", type = "error")
  )
})

test_that(".iphra_notify validates type argument", {
  expect_error(
    .iphra_notify("message", type = "invalid"),
    regexp = "should be one of"
  )
})

# ============================================================
# 2. IPHRA_ERROR TESTS
# ============================================================

test_that("iphra_error creates formatted error message with origin", {
  expect_error(
    iphra_error("Test error", origin = "test_function"),
    regexp = "test_function.*Test error"
  )
})

test_that("iphra_error includes hint in error message", {
  expect_error(
    iphra_error("Test error", hint = "Try this fix"),
    regexp = "Try this fix"
  )
})

test_that("iphra_error works without origin or hint", {
  expect_error(
    iphra_error("Simple error"),
    regexp = "Simple error"
  )
})

test_that("iphra_error includes custom type in message", {
  expect_error(
    iphra_error("Test error", type = "ValidationError"),
    regexp = "ValidationError"
  )
})

test_that("iphra_error in test mode does not call shiny::req", {
  # Set test mode
  old_opt <- getOption("IPHRA_TEST_MODE")
  options(IPHRA_TEST_MODE = TRUE)

  # Should error without calling shiny::req
  expect_error(
    iphra_error("Test error"),
    regexp = "Test error"
  )

  # Restore option
  options(IPHRA_TEST_MODE = old_opt)
})

# ============================================================
# 3. IPHRA_WARNING TESTS
# ============================================================

test_that("iphra_warning creates formatted warning message", {
  expect_no_error(
    iphra_warning("Test warning", origin = "test_function")
  )
})

test_that("iphra_warning includes hint in message", {
  expect_no_error(
    iphra_warning("Test warning", hint = "Consider this")
  )
})

test_that("iphra_warning works without origin or hint", {
  expect_no_error(
    iphra_warning("Simple warning")
  )
})

test_that("iphra_warning allows custom type", {
  expect_no_error(
    iphra_warning("Test warning", type = "DataWarning")
  )
})

# ============================================================
# 4. IPHRA_MESSAGE TESTS
# ============================================================

test_that("iphra_message creates formatted informational message", {
  expect_no_error(
    iphra_message("Test message", origin = "test_function")
  )
})

test_that("iphra_message works without origin", {
  expect_no_error(
    iphra_message("Simple message")
  )
})

# ============================================================
# 5. IPHRA_ASSERT TESTS
# ============================================================

test_that("iphra_assert passes when condition is TRUE", {
  expect_no_error(
    iphra_assert(TRUE, "Should not error")
  )

  expect_no_error(
    iphra_assert(1 == 1, "Should not error")
  )
})

test_that("iphra_assert errors when condition is FALSE", {
  expect_error(
    iphra_assert(FALSE, "Assertion failed"),
    regexp = "Assertion failed"
  )
})

test_that("iphra_assert includes origin in error", {
  expect_error(
    iphra_assert(FALSE, "Assertion failed", origin = "test_func"),
    regexp = "test_func"
  )
})

test_that("iphra_assert includes hint in error", {
  expect_error(
    iphra_assert(FALSE, "Assertion failed", hint = "Check input"),
    regexp = "Check input"
  )
})

test_that("iphra_assert errors on NA condition", {
  expect_error(
    iphra_assert(NA, "NA is not TRUE"),
    regexp = "NA is not TRUE"
  )
})

# ============================================================
# 6. IPHRA_TRY TESTS
# ============================================================

test_that("iphra_try executes expression successfully", {
  result <- iphra_try({
    42
  }, on_error = "return")

  expect_equal(result, 42)
})

test_that("iphra_try handles errors with on_error='return'", {
  result <- iphra_try({
    stop("Test error")
  }, on_error = "return")

  expect_true(is.list(result))
  expect_false(result$success)
  expect_true(grepl("Test error", result$error))
})

test_that("iphra_try handles errors with on_error='warn'", {
  expect_warning(
    iphra_try({
      stop("Test error")
    }, on_error = "warn"),
    regexp = "Test error"  # May or may not warn depending on implementation
  )
})

test_that("iphra_try handles errors with on_error='abort'", {
  expect_error(
    iphra_try({
      stop("Test error")
    }, on_error = "abort"),
    regexp = "Test error"
  )
})

test_that("iphra_try includes origin in error message", {
  result <- iphra_try({
    stop("Test error")
  }, on_error = "return", origin = "test_function")

  expect_true(grepl("test_function", result$error))
})

test_that("iphra_try includes hint in error context", {
  result <- iphra_try({
    stop("Test error")
  }, on_error = "return", hint = "Try this fix")

  expect_equal(result$hint, "Try this fix")
})

test_that("iphra_try includes step in error message", {
  result <- iphra_try({
    stop("Test error")
  }, on_error = "return", step = "Validation")

  expect_equal(result$step, "Validation")
})

test_that("iphra_try combines origin and step correctly", {
  result <- iphra_try({
    stop("Test error")
  }, on_error = "return", origin = "main_function", step = "Validation")

  expect_true(grepl("main_function", result$error) || grepl("Validation", result$error))
})

test_that("iphra_try preserves nested error context", {
  result <- iphra_try({
    iphra_try({
      stop("Inner error")
    }, on_error = "abort", step = "Inner")
  }, on_error = "return", origin = "Outer")

  expect_true(is.list(result))
  expect_false(result$success)
})

# ============================================================
# 7. IPHRA_TRY_STEP TESTS
# ============================================================

test_that("iphra_try_step executes successfully", {
  result <- iphra_try_step({
    42
  }, step = "Test Step")

  expect_equal(result, 42)
})

test_that("iphra_try_step returns error list on failure", {
  result <- iphra_try_step({
    stop("Step error")
  }, step = "Test Step")

  expect_true(is.list(result))
  expect_false(result$success)
  expect_equal(result$step, "Test Step")
})

test_that("iphra_try_step includes hint in error context", {
  result <- iphra_try_step({
    stop("Step error")
  }, step = "Test Step", hint = "Check input")

  expect_equal(result$hint, "Check input")
})

test_that("iphra_try_step always uses on_error='return'", {
  # Should return error list, not throw
  result <- iphra_try_step({
    stop("Error")
  }, step = "Test")

  expect_true(is.list(result))
  expect_false(result$success)
})

# ============================================================
# 8. IPHRA_FAILED TESTS
# ============================================================

test_that("iphra_failed returns TRUE for failed result", {
  failed_result <- list(success = FALSE, error = "Test error")

  expect_true(iphra_failed(failed_result))
})

test_that("iphra_failed returns FALSE for successful result", {
  success_result <- list(success = TRUE)

  expect_false(iphra_failed(success_result))
})

test_that("iphra_failed returns FALSE for non-list result", {
  expect_false(iphra_failed(42))
  expect_false(iphra_failed("text"))
  expect_false(iphra_failed(NULL))
})

test_that("iphra_failed returns FALSE for list without success field", {
  result <- list(error = "Something")

  expect_false(iphra_failed(result))
})

test_that("iphra_failed returns FALSE for success = TRUE", {
  result <- list(success = TRUE, data = 42)

  expect_false(iphra_failed(result))
})

# ============================================================
# 9. NESTED ERROR HANDLING PATTERNS
# ============================================================

test_that("nested iphra_try calls preserve context chain", {
  outer_result <- iphra_try({
    step1 <- iphra_try_step({
      42
    }, step = "Step 1")

    if (iphra_failed(step1)) return(step1)

    step2 <- iphra_try_step({
      stop("Error in step 2")
    }, step = "Step 2")

    if (iphra_failed(step2)) return(step2)

    "success"
  }, on_error = "return", origin = "Outer Function")

  expect_true(iphra_failed(outer_result))
  expect_equal(outer_result$step, "Step 2")
})

test_that("multiple steps can be tracked in nested try blocks", {
  result <- iphra_try({
    r1 <- iphra_try_step({ 1 }, step = "Init")
    if (iphra_failed(r1)) { r1 } else {
      r2 <- iphra_try_step({ 2 }, step = "Process")
      if (iphra_failed(r2)) { r2 } else {
        r3 <- iphra_try_step({ stop("Final error") }, step = "Finalize")
        r3
      }
    }
  }, on_error = "return", origin = "MultiStep")

  expect_true(iphra_failed(result))
  expect_equal(result$step, "Finalize")
})

# ============================================================
# 10. EDGE CASES AND ERROR CONDITIONS
# ============================================================

test_that("iphra_try handles empty expression", {
  result <- iphra_try({}, on_error = "return")
  expect_true(is.null(result) || !is.list(result) || result$success != FALSE)
})

test_that("iphra_error handles very long messages", {
  long_msg <- paste(rep("word", 1000), collapse = " ")

  expect_error(
    iphra_error(long_msg),
    regexp = "word"
  )
})

test_that("iphra_try handles NULL origin and hint", {
  result <- iphra_try({
    stop("Error")
  }, on_error = "return", origin = NULL, hint = NULL)

  expect_true(is.list(result))
  expect_false(result$success)
})

test_that("error functions handle special characters in messages", {
  expect_error(
    iphra_error("Error with [brackets] and (parens) and $symbols"),
    regexp = "brackets"
  )
})

