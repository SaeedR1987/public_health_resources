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
    `audience_type.strategic`    = TRUE,
    `audience_type.programmatic` = TRUE,
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
  expect_true(p$metadata[["audience_type.strategic"]])
  expect_true(p$metadata[["audience_type.programmatic"]])
  expect_false(p$metadata[["audience_type.other"]])
  expect_equal(p$metadata$population, c("idp_camp", "host_community"))
  expect_true(p$metadata$gender_disaggregation)
  expect_equal(p$metadata$access, "public")
})

test_that("IPHRAProtocol defaults audience_type.operational and .programmatic to TRUE", {
  p <- IPHRAProtocol$new()
  expect_false(p$metadata[["audience_type.strategic"]])
  expect_true(p$metadata[["audience_type.operational"]])
  expect_true(p$metadata[["audience_type.programmatic"]])
  expect_false(p$metadata[["audience_type.other"]])
})

test_that("IPHRAProtocol stores general_objective with IPHRA default", {
  p <- IPHRAProtocol$new()
  expect_true(is.character(p$metadata$general_objective))
  expect_true(nzchar(p$metadata$general_objective))
  expect_true(grepl("public health", p$metadata$general_objective))
})

test_that("IPHRAProtocol stores geographic_coverage", {
  p <- IPHRAProtocol$new(geographic_coverage = "Northern Somalia")
  expect_equal(p$metadata$geographic_coverage, "Northern Somalia")
})

test_that("IPHRAProtocol backward-compat: geographic_description populates geographic_coverage", {
  p <- IPHRAProtocol$new(geographic_description = "Southern Somalia")
  expect_equal(p$metadata$geographic_coverage, "Southern Somalia")
})

test_that("IPHRAProtocol stores pop boolean fields", {
  p <- IPHRAProtocol$new(pop_idpcamp = TRUE, pop_host = TRUE)
  expect_true(p$metadata[["pop_idpcamp"]])
  expect_true(p$metadata[["pop_host"]])
  expect_false(p$metadata[["pop_refugee"]])
  expect_false(p$metadata[["pop_other"]])
})

test_that("Protocol base class metadata contains new fields", {
  p <- Protocol$new()
  expect_null(p$metadata$mandating_body)
  expect_null(p$metadata$general_objective)
  expect_false(p$metadata[["audience_type.strategic"]])
  expect_false(p$metadata[["pop_idpcamp"]])
})

test_that("Protocol and SurveyProtocol initialize with blank protocol_schema", {
  p <- Protocol$new()
  sp <- SurveyProtocol$new()
  req_cols <- c("tag_name", "handling", "condition", "default_value")

  expect_true(is.data.frame(p$protocol_schema))
  expect_true(is.data.frame(sp$protocol_schema))
  expect_true(all(req_cols %in% names(p$protocol_schema)))
  expect_true(all(req_cols %in% names(sp$protocol_schema)))
})

test_that("IPHRAProtocol initializes protocol_schema from bundled iphra schema", {
  p <- IPHRAProtocol$new()
  req_cols <- c("tag_name", "handling", "condition", "default_value")

  expect_true(is.data.frame(p$protocol_schema))
  expect_true(all(req_cols %in% names(p$protocol_schema)))
  expect_true(nrow(p$protocol_schema) > 0)
  expect_true(any(p$protocol_schema$handling == "row_delete"))
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

# ── New metadata fields ───────────────────────────────────────────────────────

test_that("IPHRAProtocol stores stakeholder_mapping field", {
  p <- IPHRAProtocol$new(stakeholder_mapping = TRUE)
  expect_true(p$metadata$stakeholder_mapping)
  p2 <- IPHRAProtocol$new()
  expect_false(p2$metadata$stakeholder_mapping)
})

test_that("IPHRAProtocol stores num_geographic_units as numeric", {
  p <- IPHRAProtocol$new(num_geographic_units = 5)
  expect_equal(p$metadata$num_geographic_units, 5)
  expect_true(is.numeric(p$metadata$num_geographic_units))
})

test_that("IPHRAProtocol stores popsize_known_geographic_unit field", {
  p <- IPHRAProtocol$new(popsize_known_geographic_unit = TRUE)
  expect_true(p$metadata$popsize_known_geographic_unit)
  p2 <- IPHRAProtocol$new()
  expect_false(p2$metadata$popsize_known_geographic_unit)
})

test_that("IPHRAProtocol num_strata_units defaults to 0", {
  p <- IPHRAProtocol$new()
  expect_equal(p$metadata$num_strata_units, 0L)
})

test_that("IPHRAProtocol stores popsize_known_strata_unit field", {
  p <- IPHRAProtocol$new(popsize_known_strata_unit = TRUE)
  expect_true(p$metadata$popsize_known_strata_unit)
})

test_that("IPHRAProtocol stores user-defined numeric target fields", {
  p <- IPHRAProtocol$new(
    num_kii_health_target    = 10,
    num_kii_market_target    = 5,
    num_kii_fsl_target       = 3,
    num_kii_wash_target      = 4,
    num_kii_nutrition_target = 6,
    num_obs_health_target    = 8,
    num_obs_latrine_target   = 12,
    num_obs_waterpoint_target = 7
  )
  expect_equal(p$metadata$num_kii_health_target, 10)
  expect_equal(p$metadata$num_kii_market_target, 5)
  expect_equal(p$metadata$num_kii_fsl_target, 3)
  expect_equal(p$metadata$num_kii_wash_target, 4)
  expect_equal(p$metadata$num_kii_nutrition_target, 6)
  expect_equal(p$metadata$num_obs_health_target, 8)
  expect_equal(p$metadata$num_obs_latrine_target, 12)
  expect_equal(p$metadata$num_obs_waterpoint_target, 7)
})

test_that("Protocol$update_metadata updates fields and touch()", {
  p <- IPHRAProtocol$new()
  t_before <- p$metadata$modified_date
  Sys.sleep(0.01)
  p$update_metadata(country_name = "Kenya", num_geographic_units = 3)
  expect_equal(p$metadata$country_name, "Kenya")
  expect_equal(p$metadata$num_geographic_units, 3)
  expect_true(p$metadata$modified_date > t_before)
})

test_that("Protocol$update_metadata errors with no named arguments", {
  p <- Protocol$new()
  expect_error(p$update_metadata())
})

test_that("IPHRAProtocol general_objective default contains 'public health'", {
  p <- IPHRAProtocol$new()
  expect_true(grepl("public health", p$metadata$general_objective, ignore.case = TRUE))
})

test_that("IPHRAProtocol add_stratum updates num_strata_units", {
  p <- IPHRAProtocol$new()
  expect_equal(p$metadata$num_strata_units, 0L)
  p$add_stratum(stratum_id = "north", Population_Name = "Northern Region")
  expect_equal(p$metadata$num_strata_units, 1L)
  p$add_stratum(stratum_id = "south", Population_Name = "Southern Region")
  expect_equal(p$metadata$num_strata_units, 2L)
})

test_that("IPHRAProtocol schema 'replace' handling uses protocol_schema default_value", {
  p <- IPHRAProtocol$new()
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, "@tor_title", style = "Normal")
  schema <- p$protocol_schema
  row <- schema[schema$tag_name == "@tor_title", c("tag_name", "handling", "condition", "default_value"), drop = FALSE]
  expect_true(nrow(row) > 0)
  doc <- p$.__enclos_env__$private$.add_replace_section(doc, row[1, , drop = FALSE])
  body_xml <- officer::docx_body_xml(doc)
  txt <- paste(xml2::xml_text(xml2::xml_find_all(body_xml, ".//w:t", xml2::xml_ns(body_xml))),
               collapse = "")
  expect_true(grepl("Research Terms of Reference", txt, fixed = TRUE))
})

test_that("IPHRAProtocol normalizes schema tag to @kii_nut_inc", {
  p <- IPHRAProtocol$new()
  expect_true("@kii_nut_inc" %in% p$protocol_schema$tag_name)
  expect_false("@kii_nutrition_inc" %in% p$protocol_schema$tag_name)
})

test_that("replace_tag_in_cell preserves item order for objective headers and bullets", {
  p <- IPHRAProtocol$new()
  doc <- officer::read_docx()
  doc <- officer::body_add_table(doc, value = data.frame(col1 = "@specific_objectives"))
  items <- list(
    list(text = "IncomeCoping", bold = TRUE, space_before_pt = 0L, space_after_pt = 0L, font_size_pt = 10L),
    list(text = "\u2022 Coping strategies", bold = FALSE, space_before_pt = 0L, space_after_pt = 0L, font_size_pt = 10L),
    list(text = "\u2022 Household income", bold = FALSE, space_before_pt = 0L, space_after_pt = 0L, font_size_pt = 10L),
    list(text = "LivingConditions", bold = TRUE, space_before_pt = 6L, space_after_pt = 0L, font_size_pt = 10L),
    list(text = "\u2022 Crowdedness", bold = FALSE, space_before_pt = 0L, space_after_pt = 0L, font_size_pt = 10L)
  )

  ok <- p$.__enclos_env__$private$.replace_tag_in_cell(doc, "@specific_objectives", items)
  expect_true(ok)

  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  tc <- xml2::xml_find_first(body_xml, ".//w:tc", ns = ns)
  paras <- xml2::xml_find_all(tc, ".//w:p", ns = ns)
  texts <- vapply(paras, xml2::xml_text, character(1L))
  texts <- texts[nzchar(texts)]

  idx_income <- match("IncomeCoping", texts)
  idx_coping <- match("\u2022 Coping strategies", texts)
  idx_income_b <- match("\u2022 Household income", texts)
  idx_living <- match("LivingConditions", texts)
  idx_crowded <- match("\u2022 Crowdedness", texts)

  expect_true(all(!is.na(c(idx_income, idx_coping, idx_income_b, idx_living, idx_crowded))))
  expect_true(idx_income < idx_coping)
  expect_true(idx_coping < idx_income_b)
  expect_true(idx_living < idx_crowded)
})
