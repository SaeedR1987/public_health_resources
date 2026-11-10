library(testthat)

context("Asset class tests")

# Basic initialization
test_that("Asset initialize sets metadata", {
  inst <- Asset$new()
  expect_s3_class(inst$metadata$created_datetime, "POSIXct")
  expect_s3_class(inst$metadata$modified_datetime, "POSIXct")
  expect_true(
    as.numeric(inst$metadata$modified_datetime) >=
      as.numeric(inst$metadata$created_datetime)
  )
})

test_that("Asset metadata is private and only accessible via public methods", {
  inst <- Asset$new()
  expect_false("metadata" %in% names(inst$.__enclos_env__$private))
  expect_true("..metadata" %in% names(inst$.__enclos_env__$private))
  expect_identical(inst$get(field = "metadata"), inst$metadata)
  expect_identical(
    inst$get(field = "metadata", member = "created_datetime"),
    inst$metadata$created_datetime
  )
})

test_that("Asset initialize sets a hash_id fingerprint", {
  inst <- Asset$new()
  expect_true(is.character(inst$get_hash_id()))
  expect_true(!is.na(inst$get_hash_id()))
  expect_identical(inst$get_hash_id(), inst$metadata$hash_id)
})

test_that("Asset initialize sets version to 1", {
  inst <- Asset$new()
  expect_identical(inst$metadata$version, 1L)
})

# Helper subclass exposing public/private fields for get()/call()/set() tests
TestAsset <- R6::R6Class(
  "TestAsset",
  inherit = Asset,
  public = list(
    tools = NULL,
    initialize = function() {
      super$initialize()
      self$tools <- list(
        tool_household_iphra_v2 = list(name = "household"),
        tool_health_iphra_v2 = list(name = "health")
      )
    },
    greet = function(who = "world") {
      paste0("hello ", who)
    }
  ),
  private = list(
    secret = "init_secret"
  )
)

test_that("hash_id changes when public state changes", {
  inst <- TestAsset$new()
  before <- inst$get_hash_id()
  inst$set(field = "tools", value = list(new_tool = list(name = "new")))
  after <- inst$get_hash_id()
  expect_false(identical(before, after))
})

test_that("version increments each time the object is touched", {
  inst <- TestAsset$new()
  before <- inst$metadata$version
  inst$set(field = "tools", value = list(new_tool = list(name = "new")))
  expect_identical(inst$metadata$version, before + 1L)
  inst$set(field = "tools", value = list(other_tool = list(name = "other")))
  expect_identical(inst$metadata$version, before + 2L)
})

test_that("set() replaces a public top-level field directly", {
  inst <- TestAsset$new()
  inst$set(field = "tools", value = list(new_tool = list(name = "new")))
  expect_equal(names(inst$tools), "new_tool")
})

test_that("set() writes a member on a resolved public field", {
  inst <- TestAsset$new()
  inst$set(field = "metadata", member = "custom_flag", value = TRUE)
  expect_true(inst$metadata$custom_flag)
})

test_that("set() writes a member on a role-resolved (exact-name-matching) list element", {
  inst <- TestAsset$new()
  inst$set(
    field = "tools",
    role = "tool_household_iphra_v2",
    member = "name",
    value = "renamed"
  )
  expect_equal(inst$tools$tool_household_iphra_v2$name, "renamed")
})

test_that("set() writes a member on a role-resolved list element", {
  inst <- TestAsset$new()
  inst$set(
    field = "tools",
    role = "health",
    member = "name",
    value = "renamed_health"
  )
  expect_equal(inst$tools$tool_health_iphra_v2$name, "renamed_health")
})

test_that("set() can write to a private field", {
  inst <- TestAsset$new()
  inst$set(field = "secret", value = "updated_secret")
  expect_equal(inst$.__enclos_env__$private$secret, "updated_secret")
})

test_that("set() updates the modified timestamp by default", {
  inst <- TestAsset$new()
  Sys.sleep(0.01)
  before <- inst$metadata$modified_datetime
  inst$set(field = "secret", value = "again")
  expect_true(inst$metadata$modified_datetime > before)
})

test_that("set() errors for an unknown field", {
  inst <- TestAsset$new()
  expect_error(inst$set(field = "does_not_exist", value = 1))
})

test_that("set() refuses to overwrite a function member", {
  inst <- TestAsset$new()
  expect_error(inst$set(field = "initialize", value = 1))
})

test_that("set() delegates to a nested R6 object's own set() when writing a member", {
  TestAssetWithLog <- R6::R6Class(
    "TestAssetWithLog",
    inherit = Asset,
    public = list(
      log = NULL,
      initialize = function() {
        super$initialize()
        self$log <- Log$new(log_name = "Nested Log")
      }
    )
  )
  inst <- TestAssetWithLog$new()
  new_df <- data.frame(x = 1:2)

  inst$set(field = "log", member = "log_df", value = new_df)

  expect_equal(as.data.frame(inst$log$get("log_df")), new_df)
})

test_that("set() errors when role2 is supplied without role", {
  inst <- TestAsset$new()
  expect_error(
    inst$set(
      field = "tools",
      role2 = "name",
      value = "x"
    )
  )
})

test_that("set() creates a new role-keyed list element with a member when it does not yet exist", {
  inst <- TestAsset$new()
  inst$set(
    field = "tools",
    role = "brand_new_tool",
    member = "name",
    value = "created"
  )
  expect_equal(inst$tools$brand_new_tool$name, "created")
})

test_that("set() creates a new 2nd level (role2) list element when it does not yet exist", {
  inst <- TestAsset$new()
  inst$set(
    field = "tools",
    role = "health",
    role2 = "settings",
    value = list(enabled = TRUE)
  )
  expect_equal(inst$tools$tool_health_iphra_v2$settings, list(enabled = TRUE))
})

test_that("set() creates a new 2nd level (role2) list element with a member when both are new", {
  inst <- TestAsset$new()
  inst$set(
    field = "tools",
    role = "brand_new_tool",
    role2 = "settings",
    member = "enabled",
    value = TRUE
  )
  expect_true(inst$tools$brand_new_tool$settings$enabled)
})

# get()
test_that("get() retrieves a top-level public field", {
  inst <- TestAsset$new()
  expect_equal(
    inst$get(field = "tools"),
    inst$tools
  )
})

test_that("get() retrieves a private top-level field", {
  inst <- TestAsset$new()
  expect_equal(inst$get(field = "secret"), "init_secret")
})

test_that("get() retrieves a member on a role-resolved list element using the literal key", {
  inst <- TestAsset$new()
  expect_equal(
    inst$get(field = "tools", role = "tool_household_iphra_v2", member = "name"),
    "household"
  )
})

test_that("get() retrieves a member on a role-resolved list element", {
  inst <- TestAsset$new()
  expect_equal(
    inst$get(field = "tools", role = "health", member = "name"),
    "health"
  )
})

test_that("get() refuses to return a function member", {
  inst <- TestAsset$new()
  expect_error(inst$get(field = "greet"))
})

test_that("get() does not update modified timestamp by default", {
  inst <- TestAsset$new()
  before <- inst$metadata$modified_datetime
  Sys.sleep(0.01)
  inst$get(field = "tools")
  expect_equal(inst$metadata$modified_datetime, before)
})

test_that("get() can update the modified timestamp when requested", {
  inst <- TestAsset$new()
  before <- inst$metadata$modified_datetime
  Sys.sleep(0.01)
  inst$get(field = "tools", update_modified = TRUE)
  expect_true(inst$metadata$modified_datetime > before)
})

# call()
test_that("call() invokes a public top-level method directly", {
  inst <- TestAsset$new()
  expect_equal(inst$call(field = "greet"), "hello world")
})

test_that("call() invokes a nested method resolved by role", {
  inst <- TestAsset$new()
  inst$tools$tool_household_iphra_v2$get_name <- function() "household"
  expect_equal(
    inst$call(
      field = "tools",
      role = "tool_household_iphra_v2",
      member = "get_name"
    ),
    "household"
  )
})

test_that("call() errors when member does not resolve to a function", {
  inst <- TestAsset$new()
  expect_error(inst$call(field = "tools", member = "tool_household_iphra_v2"))
})

test_that("call() updates the modified timestamp by default", {
  inst <- TestAsset$new()
  inst$tools$tool_household_iphra_v2$get_name <- function() "household"
  before <- inst$metadata$modified_datetime
  Sys.sleep(0.01)
  inst$call(
    field = "tools",
    role = "tool_household_iphra_v2",
    member = "get_name"
  )
  expect_true(inst$metadata$modified_datetime > before)
})

# change_log / ..track() / ..touch(log = ...)
test_that("change_log is NULL until the first logged change", {
  inst <- TestAsset$new()
  expect_null(inst$.__enclos_env__$private$..change_log)
})

test_that("..touch() does not log by default", {
  inst <- TestAsset$new()
  inst$.__enclos_env__$private$..touch()
  expect_null(inst$.__enclos_env__$private$..change_log)
})

test_that("..touch(log = TRUE) appends a row to change_log via ..track()", {
  inst <- TestAsset$new()
  priv <- inst$.__enclos_env__$private
  priv$..touch(log = TRUE, name = "field_updated", message = "Updated a field.")

  log <- priv$..change_log
  expect_s3_class(log, "data.frame")
  expect_identical(
    names(log),
    c("id", "version", "name", "datetime", "log_message")
  )
  expect_equal(nrow(log), 1L)
  expect_identical(log$id[[1]], priv$..metadata$hash_id)
  expect_identical(log$version[[1]], priv$..metadata$version)
  expect_identical(log$name[[1]], "field_updated")
  expect_s3_class(log$datetime, "POSIXct")
  expect_identical(log$log_message[[1]], "Updated a field.")
})

test_that("..touch(log = TRUE) accumulates multiple change_log rows", {
  inst <- TestAsset$new()
  priv <- inst$.__enclos_env__$private
  priv$..touch(log = TRUE, name = "first_change", message = "First.")
  priv$..touch(log = TRUE, name = "second_change", message = "Second.")

  log <- priv$..change_log
  expect_equal(nrow(log), 2L)
  expect_identical(log$name, c("first_change", "second_change"))
  expect_identical(log$version, c(2L, 3L))
})

test_that("..track() can be called directly to append a change_log row", {
  inst <- TestAsset$new()
  priv <- inst$.__enclos_env__$private
  priv$..track(name = "manual_entry", message = "Manually tracked.")

  log <- priv$..change_log
  expect_equal(nrow(log), 1L)
  expect_identical(log$id[[1]], priv$..metadata$hash_id)
  expect_identical(log$version[[1]], priv$..metadata$version)
  expect_identical(log$name[[1]], "manual_entry")
  expect_identical(log$log_message[[1]], "Manually tracked.")
})
