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
  expect_true("tool_household_iphra_v2"                      %in% tools)
  expect_true("tool_kii_community_iphra_v2"                  %in% tools)
  expect_true("tool_kii_health_service_provider_iphra_v2"    %in% tools)
  expect_true("tool_kii_wash_service_provider_iphra_v2"      %in% tools)
  expect_true("tool_kii_nutrition_service_provider_iphra_v2" %in% tools)
  expect_true("tool_obs_water_point_iphra_v2"                %in% tools)
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

test_that("Protocol initializes with framework_type='none' by default", {
  p <- Protocol$new()
  expect_true(inherits(p$framework, "Framework"))
  expect_false(inherits(p$framework, "ANAFramework"))
})

# ── New optional metadata fields ────────────────────────────────────────────

test_that("IPHRAProtocol stores optional metadata on initialize", {
  p <- IPHRAProtocol$new(
    assessment_title    = "Test IPHRA",
    country_name        = "Somalia",
    month_year          = "March 2025",
    version             = 2L,
    type_of_emergency   = "conflict",
    type_of_crisis      = "protracted",
    mandating_body      = "UNHCR",
    project_code        = "SOM123",
    pilot_date          = "2025-03-01",
    data_start_date     = "2025-03-05",
    data_end_date       = "2025-03-20",
    audience_type       = c("strategic", "programmatic"),
    population          = c("idp_camp", "host_community"),
    gender_disaggregation = TRUE,
    access              = "public"
  )
  expect_equal(p$metadata$assessment_title, "Test IPHRA")
  expect_equal(p$metadata$version, 2L)
  expect_equal(p$metadata$type_of_emergency, "conflict")
  expect_equal(p$metadata$type_of_crisis, "protracted")
  expect_equal(p$metadata$mandating_body, "UNHCR")
  expect_equal(p$metadata$project_code, "SOM123")
  expect_equal(p$metadata$pilot_date, "01/03/2025")
  expect_equal(p$metadata$data_start_date, "05/03/2025")
  expect_equal(p$metadata$data_end_date, "20/03/2025")
  expect_equal(p$metadata$audience_type, c("strategic", "programmatic"))
  expect_equal(p$metadata$population, c("idp_camp", "host_community"))
  expect_true(p$metadata$gender_disaggregation)
  expect_equal(p$metadata$access, "public")
})

test_that("IPHRAProtocol stores secondary_data", {
  p <- IPHRAProtocol$new()
  p$secondary_data <- list(OBJ01 = "ACLED conflict database",
                           OBJ02 = "UNHCR population figures")
  expect_equal(length(p$secondary_data), 2L)
  expect_equal(p$secondary_data$OBJ01, "ACLED conflict database")
})

# ── Protocol helpers ─────────────────────────────────────────────────────────

test_that("Protocol$touch updates modified_date", {
  p <- Protocol$new()
  t_before <- p$metadata$modified_date
  Sys.sleep(0.01)
  p$touch()
  expect_true(p$metadata$modified_date > t_before)
})

test_that("Protocol$get_schema returns data frame from framework", {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "reference.xlsx not available"
  )
  p <- IPHRAProtocol$new()
  schema <- p$get_schema("master")
  expect_true(is.data.frame(schema))
  expect_true(nrow(schema) > 0)
})

test_that("Protocol$get_schema returns empty data.frame when no framework", {
  p <- Protocol$new(framework_type = "none")
  p$framework <- NULL
  schema <- p$get_schema("master")
  expect_true(is.data.frame(schema))
  expect_equal(nrow(schema), 0L)
})

test_that("Protocol$get_indicator_codes_from_schema returns character vector", {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "reference.xlsx not available"
  )
  p <- IPHRAProtocol$new()
  codes <- p$get_indicator_codes_from_schema("master")
  expect_true(is.character(codes))
  expect_true(length(codes) > 0)
})

test_that("Protocol$get_tool_names returns empty vector before add_tools", {
  p <- IPHRAProtocol$new()
  expect_equal(p$get_tool_names(), character(0))
})

test_that("Protocol$get_tool_names returns tool names after add_tools", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_household_iphra_v2")
  expect_equal(p$get_tool_names(), "tool_household_iphra_v2")
})

test_that("Protocol$is_tool_included returns FALSE for unknown tool", {
  p <- IPHRAProtocol$new()
  expect_false(p$is_tool_included("tool_household_iphra_v2"))
})

test_that("Protocol$is_tool_included returns TRUE after add_tools", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_household_iphra_v2")
  expect_true(p$is_tool_included("tool_household_iphra_v2"))
})

test_that("Protocol$get_tool_survey returns NULL for missing tool", {
  p <- IPHRAProtocol$new()
  expect_null(p$get_tool_survey("tool_household_iphra_v2"))
})

test_that("Protocol$get_indicator_codes_from_tools returns character vector", {
  p <- IPHRAProtocol$new()
  p$add_tools("tool_kii_community_iphra_v2")
  codes <- p$get_indicator_codes_from_tools()
  expect_true(is.character(codes))
})

test_that("Protocol$get_schema_for_indicators filters schema rows", {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "reference.xlsx not available"
  )
  p <- IPHRAProtocol$new()
  all_codes <- p$get_indicator_codes_from_schema("master")
  if (length(all_codes) >= 2) {
    sub_codes <- all_codes[1:2]
    filtered  <- p$get_schema_for_indicators(sub_codes)
    expect_true(is.data.frame(filtered))
    expect_true(nrow(filtered) > 0)
    expect_true(all(as.character(filtered$indicator_code) %in% sub_codes))
  }
})

test_that("Protocol$get_schema_column returns values for existing column", {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "reference.xlsx not available"
  )
  p <- IPHRAProtocol$new()
  pillars <- p$get_schema_column("pillar")
  expect_true(is.character(pillars))
  expect_true(length(pillars) > 0)
})

test_that("Protocol$get_schema_column returns empty vector for missing column", {
  p <- IPHRAProtocol$new()
  result <- p$get_schema_column("nonexistent_col_xyz")
  expect_equal(result, character(0))
})

