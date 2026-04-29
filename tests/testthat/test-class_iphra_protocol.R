test_that("IPHRAProtocol initializes with an ANAFramework", {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "reference.xlsx not available"
  )
  p <- IPHRAProtocol$new()
  expect_true(inherits(p, "IPHRAProtocol"))
  expect_true(inherits(p, "Protocol"))
  expect_true(inherits(p$framework, "ANAFramework"))
  expect_true(inherits(p$framework, "Framework"))
})

test_that("IPHRAProtocol$get_allowable_tools returns expected tool names", {
  p <- IPHRAProtocol$new()
  tools <- p$get_allowable_tools()
  expect_true(is.character(tools))
  expect_true("tool_household_iphra_v2" %in% tools)
  expect_true("tool_kii_community_iphra_v2" %in% tools)
  expect_true("tool_obs_water_point_iphra_v2" %in% tools)
  expect_length(tools, 12L)
})

test_that("IPHRAProtocol$add_tools rejects unknown tool names", {
  p <- IPHRAProtocol$new()
  expect_error(p$add_tools("unknown_tool_xyz"))
})

test_that("IPHRAProtocol$add_tools stores the tool in the tools list by name", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_household_iphra_v2")
  expect_true("tool_household_iphra_v2" %in% names(p$tools))
  expect_true(inherits(p$tools[["tool_household_iphra_v2"]], "HouseholdTool"))
})

test_that("IPHRAProtocol$add_tools creates KeyInformantTool for KII tools", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_kii_community_iphra_v2")
  expect_true(inherits(p$tools[["tool_kii_community_iphra_v2"]], "KeyInformantTool"))
})

test_that("IPHRAProtocol$add_tools creates ObservationTool for obs tools", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_obs_water_point_iphra_v2")
  expect_true(inherits(p$tools[["tool_obs_water_point_iphra_v2"]], "ObservationTool"))
})

test_that("Protocol$add_tools stores tools by name for $ access", {
  p <- Protocol$new()
  p$add_tools("household")
  expect_true("household" %in% names(p$tools))
  expect_true(inherits(p$tools[["household"]], "HouseholdTool"))
})

test_that("Protocol$add_tools uses tool_name when provided", {
  p <- Protocol$new()
  p$add_tools("household", tool_name = "my_hh_tool")
  expect_true("my_hh_tool" %in% names(p$tools))
})

test_that("Protocol$add_tools auto-increments duplicate tool types", {
  p <- Protocol$new()
  p$add_tools("household")
  p$add_tools("household")
  nms <- names(p$tools)
  expect_true("household" %in% nms)
  expect_true(any(grepl("^household_", nms)))
})

test_that("Protocol$validate_objective_schema works as method", {
  p <- Protocol$new()
  good <- data.frame(
    sector = "Health", pillar = "P1", sub_pillar = "SP1",
    short_objective = "H1", text_objective = "Obj 1",
    stringsAsFactors = FALSE
  )
  expect_true(p$validate_objective_schema(good))
})

test_that("Protocol$validate_objective_schema errors on bad schema", {
  p <- Protocol$new()
  bad <- data.frame(x = 1:3)
  expect_error(p$validate_objective_schema(bad))
})

test_that("Framework$render_framework_svg errors when no SVG is set", {
  fw <- Framework$new()
  expect_error(fw$render_framework_svg())
})

test_that("Tool$survey is a public field", {
  t <- Tool$new()
  expect_true(is.data.frame(t$survey))
  # Can be set directly
  new_survey <- data.frame(type = "text", name = "q1", label = "Question 1",
                           stringsAsFactors = FALSE)
  t$survey <- new_survey
  expect_equal(t$survey$name, "q1")
})

test_that("Tool$choices is a public field", {
  t <- Tool$new()
  expect_true(is.data.frame(t$choices))
})

test_that("Tool$settings is a public field", {
  t <- Tool$new()
  expect_true(is.data.frame(t$settings))
})

test_that("Protocol initializes with framework_type='none' by default", {
  p <- Protocol$new()
  expect_true(inherits(p$framework, "Framework"))
  expect_false(inherits(p$framework, "ANAFramework"))
})
