test_that("Framework initializes with NULL fields", {
  fw <- Framework$new()
  expect_null(fw$master_schema)
  expect_null(fw$adjusted_schema)
  expect_null(fw$master_svg)
  expect_null(fw$adjusted_svg)
})

test_that("Framework$set_master_schema validates and stores the schema", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute Illness",
    short_objective = c("H1", "H2"),
    text_objective  = c("Obj 1", "Obj 2"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  expect_equal(nrow(fw$master_schema), 2)
  expect_equal(fw$master_schema$short_objective, c("H1", "H2"))
})

test_that("Framework$set_master_schema rejects invalid schema", {
  fw <- Framework$new()
  bad <- data.frame(x = 1:3)
  expect_error(fw$set_master_schema(bad))
})

test_that("Framework$set_master_svg stores the SVG content", {
  fw <- Framework$new()
  svg <- '<svg><rect id="H1"/></svg>'
  fw$set_master_svg(svg)
  expect_equal(fw$master_svg, svg)
})

test_that("Framework$set_master_svg rejects non-character input", {
  fw <- Framework$new()
  expect_error(fw$set_master_svg(123))
})

test_that("Framework$modify_adjusted_schema filters correctly", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective  = c("Obj 1", "Obj 2", "Obj 3"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$modify_adjusted_schema(c("H1", "H3"))
  expect_equal(nrow(fw$adjusted_schema), 2)
  expect_setequal(fw$adjusted_schema$short_objective, c("H1", "H3"))
})

test_that("Framework$modify_adjusted_schema with NULL resets to full schema", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = c("H1", "H2"),
    text_objective  = c("Obj 1", "Obj 2"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$modify_adjusted_schema(NULL)
  expect_equal(nrow(fw$adjusted_schema), 2)
})

test_that("Framework$modify_adjusted_schema errors without master_schema", {
  fw <- Framework$new()
  expect_error(fw$modify_adjusted_schema(c("H1")))
})

test_that("Framework$modify_adjusted_schema filters by objective codes", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective  = c("Obj 1", "Obj 2", "Obj 3"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$modify_adjusted_schema(c("H1", "H3"))
  expect_equal(nrow(fw$adjusted_schema), 2)
  expect_setequal(fw$adjusted_schema$short_objective, c("H1", "H3"))
})

test_that("Framework$modify_adjusted_schema with NULL uses all objective codes", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective  = c("Obj 1", "Obj 2", "Obj 3"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$modify_adjusted_schema()
  expect_equal(nrow(fw$adjusted_schema), 3)
  expect_setequal(fw$adjusted_schema$short_objective, c("H1", "H2", "H3"))
})

test_that("Framework$modify_adjusted_schema accepts a list of codes", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = c("H1", "H2", "H3"),
    text_objective  = c("Obj 1", "Obj 2", "Obj 3"),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$modify_adjusted_schema(list("H2", "H3"))
  expect_equal(nrow(fw$adjusted_schema), 2)
  expect_setequal(fw$adjusted_schema$short_objective, c("H2", "H3"))
})

test_that("Framework$modify_adjusted_schema errors without master_schema", {
  fw <- Framework$new()
  expect_error(fw$modify_adjusted_schema(c("H1")))
})

test_that("Framework$modify_adjusted_svg runs without error when primary objectives set", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = c("H1", "H2"),
    text_objective  = c("Obj 1", "Obj 2"),
    objective_code = c(1L, 2L),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  svg <- '<svg><g id="H1"><rect fill="white"/></g><g id="H2"><rect fill="white"/></g></svg>'
  fw$set_master_svg(svg)
  fw$primary_objectives <- c(1)
  fw$modify_adjusted_svg()

  expect_false(is.null(fw$adjusted_svg))
})

test_that("Protocol$export_protocol framework data is serialisable", {
  p  <- Protocol$new()
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = "H1",
    text_objective  = "Obj 1",
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  p$framework <- fw
  exported <- p$export_protocol()
  expect_true(is.list(exported$framework))
  expect_equal(exported$framework$class, "Framework")
  expect_equal(nrow(exported$framework$master_schema), 1)
})

test_that("ANAFramework initializes with master_schema from reference.xlsx", {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "reference.xlsx not available"
  )
  af <- ANAFramework$new()
  expect_true(is.data.frame(af$master_schema))
  expect_gt(nrow(af$master_schema), 0)
})

test_that("ANAFramework inherits Framework methods", {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "reference.xlsx not available"
  )
  af <- ANAFramework$new()
  expect_true(inherits(af, "Framework"))
})

test_that("create_framework returns a Framework object", {
  fw <- create_framework()
  expect_true(inherits(fw, "Framework"))
})

test_that("Protocol$new() with framework_type='ana' creates an ANAFramework", {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "reference.xlsx not available"
  )
  p <- Protocol$new(framework_type = "ana")
  expect_true(inherits(p$framework, "ANAFramework"))
  expect_true(inherits(p$framework, "Framework"))
})

test_that("restore_framework reconstructs a Framework from exported data", {
  p  <- Protocol$new()
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = "H1",
    text_objective  = "Obj 1",
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$set_master_svg('<svg><rect id="H1"/></svg>')
  p$framework <- fw
  exported <- p$export_protocol()
  restored <- restore_framework(exported$framework)
  expect_true(inherits(restored, "Framework"))
  expect_equal(nrow(restored$master_schema), 1)
  expect_false(is.null(restored$master_svg))
})

test_that("Protocol initializes with a Framework object by default (framework_type='none')", {
  p <- Protocol$new()
  expect_true(inherits(p$framework, "Framework"))
})

test_that("Protocol$new() rejects invalid framework_type", {
  expect_error(Protocol$new(framework_type = "bogus"))
})

test_that("Protocol framework field can be replaced with another Framework object", {
  p   <- Protocol$new()
  fw  <- Framework$new()
  p$framework <- fw
  expect_true(inherits(p$framework, "Framework"))
})

test_that("Protocol$export_protocol includes framework when set", {
  p  <- Protocol$new()
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = "H1",
    text_objective  = "Obj 1",
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  p$framework <- fw
  exported <- p$export_protocol()
  expect_false(is.null(exported$framework))
  expect_equal(exported$framework$class, "Framework")
})

test_that("Protocol$export_protocol includes default framework", {
  p        <- Protocol$new()
  exported <- p$export_protocol()
  expect_false(is.null(exported$framework))
})

test_that("restore_protocol restores framework field", {
  p  <- Protocol$new(assessment_title = "Test", country_name = "TestLand")
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = "H1",
    text_objective  = "Obj 1",
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  p$framework <- fw
  exported  <- p$export_protocol()
  restored  <- restore_protocol(exported)
  expect_true(inherits(restored$framework, "Framework"))
  expect_equal(nrow(restored$framework$master_schema), 1)
})

# ---- ANAFramework additional method tests ----

.skip_if_no_ana_resources <- function() {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "ANA resources not available"
  )
}

test_that("ANAFramework auto-loads master_svg from ana_framework.svg", {
  .skip_if_no_ana_resources()
  af <- ANAFramework$new()
  expect_false(is.null(af$master_svg))
  expect_true(nzchar(af$master_svg))
  expect_true(grepl("<svg", af$master_svg))
})

test_that("ANAFramework$modify_adjusted_schema returns subset and stores in adjusted_schema", {
  .skip_if_no_ana_resources()
  af <- ANAFramework$new()
  skip_if(is.null(af$master_schema))
  skip_if(!"objective_code" %in% names(af$master_schema))

  available_codes <- unique(af$master_schema$objective_code)
  skip_if(length(available_codes) == 0)

  code <- available_codes[[1]]
  af$modify_adjusted_schema(code)
  expect_true(is.data.frame(af$adjusted_schema))
  expect_true(nrow(af$adjusted_schema) > 0)
  expect_true(all(af$adjusted_schema$objective_code == code))
})

test_that("ANAFramework$modify_adjusted_schema errors without master_schema", {
  af <- ANAFramework$new()
  af$master_schema <- NULL
  expect_error(af$modify_adjusted_schema(101))
})

test_that("ANAFramework$modify_adjusted_svg colours correct sub_pillar blocks", {
  .skip_if_no_ana_resources()
  af <- ANAFramework$new()
  skip_if(is.null(af$master_schema) || is.null(af$master_svg))
  skip_if(!"objective_code" %in% names(af$master_schema))

  available_codes <- unique(af$master_schema$objective_code)
  available_codes <- available_codes[!is.na(available_codes)]
  skip_if(length(available_codes) == 0)

  af$primary_objectives <- available_codes[1]
  af$modify_adjusted_svg()

  expect_false(is.null(af$adjusted_svg))
  expect_true(nzchar(af$adjusted_svg))
})

test_that("ANAFramework$modify_adjusted_svg warns when master_svg is NULL", {
  af <- ANAFramework$new()
  af$master_svg <- NULL
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "HealthStatus",
    short_objective = "H1",
    text_objective  = "Obj 1",
    objective_code = 1L,
    stringsAsFactors = FALSE
  )
  af$master_schema <- schema
  af$adjusted_schema <- schema
  expect_warning(af$modify_adjusted_svg())
})

# ---- Protocol$add_tools / validate_objective_schema tests ----

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

test_that("Protocol$validate_objective_schema works as a method", {
  p <- Protocol$new()
  good <- data.frame(
    sector = "Health", pillar = "P1", sub_pillar = "SP1",
    short_objective = "H1", text_objective = "Obj 1",
    stringsAsFactors = FALSE
  )
  expect_true(p$validate_objective_schema(good))
})

test_that("Protocol$validate_objective_schema errors on bad schema via method", {
  p <- Protocol$new()
  bad <- data.frame(x = 1:3)
  expect_error(p$validate_objective_schema(bad))
})

# ---- Tool public fields tests ----

test_that("Tool$survey is a public field accessible directly", {
  t <- Tool$new()
  expect_true(is.data.frame(t$survey))
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

test_that("Framework$render_framework_svg errors when no SVG is set", {
  fw <- Framework$new()
  expect_error(fw$render_framework_svg())
})

test_that("Framework$render_framework_svg accepts version='master' and version='adjusted'", {
  fw <- Framework$new()
  schema <- data.frame(
    sector         = "Health",
    pillar         = "Morbidity",
    sub_pillar     = "Acute",
    short_objective = "H1",
    text_objective  = "Obj 1",
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$set_master_svg('<svg><rect id="H1"/></svg>')
  # Both version values should run without error (rsvg/grid may not be available,
  # in which case a warning is issued and the temp file path is returned).
  expect_no_error(fw$render_framework_svg(version = "master"))
  expect_no_error(fw$render_framework_svg(version = "adjusted"))
})

test_that("Framework$render_framework_svg errors on invalid version argument", {
  fw <- Framework$new()
  fw$set_master_svg('<svg><rect id="H1"/></svg>')
  expect_error(fw$render_framework_svg(version = "bad_version"))
})

test_that("Framework$render_framework_svg version='master' uses master_svg even when adjusted_svg is set", {
  skip_if_not(
    requireNamespace("rsvg", quietly = TRUE) && requireNamespace("grid", quietly = TRUE),
    "rsvg/grid not available"
  )
  fw <- Framework$new()
  schema <- data.frame(
    sector = "Health", pillar = "P", sub_pillar = "SP",
    short_objective = c("H1", "H2"), text_objective = c("O1", "O2"),
    objective_code = c(1L, 2L),
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$set_master_svg('<svg><g id="H1"><rect fill="white"/></g><g id="H2"><rect fill="white"/></g></svg>')
  fw$primary_objectives <- c(1)
  fw$modify_adjusted_svg()
  expect_false(is.null(fw$adjusted_svg))
  # version='master' should not error
  expect_silent(fw$render_framework_svg(version = "master"))
})

# ---- New field and modify_adjusted_svg tests ----

test_that("Framework initializes primary_objectives and secondary_objectives as NULL", {
  fw <- Framework$new()
  expect_null(fw$primary_objectives)
  expect_null(fw$secondary_objectives)
})

test_that("Framework primary_objectives and secondary_objectives can be set", {
  fw <- Framework$new()
  fw$primary_objectives  <- c(101, 102)
  fw$secondary_objectives <- c(103, 104)
  expect_equal(fw$primary_objectives, c(101, 102))
  expect_equal(fw$secondary_objectives, c(103, 104))
})

test_that("Protocol export includes primary and secondary objectives", {
  p  <- Protocol$new()
  fw <- Framework$new()
  fw$primary_objectives  <- c(101)
  fw$secondary_objectives <- c(102)
  p$framework <- fw
  exported <- p$export_protocol()
  expect_equal(exported$framework$primary_objectives, c(101))
  expect_equal(exported$framework$secondary_objectives, c(102))
})

test_that("restore_framework restores primary_objectives and secondary_objectives", {
  p  <- Protocol$new()
  fw <- Framework$new()
  schema <- data.frame(
    sector = "Health", pillar = "P", sub_pillar = "SP",
    short_objective = "H1", text_objective = "Obj 1",
    stringsAsFactors = FALSE
  )
  fw$set_master_schema(schema)
  fw$primary_objectives  <- c(101, 102)
  fw$secondary_objectives <- c(103)
  p$framework <- fw
  exported <- p$export_protocol()
  restored <- restore_framework(exported$framework)
  expect_equal(restored$primary_objectives, c(101, 102))
  expect_equal(restored$secondary_objectives, c(103))
})

test_that("Framework$modify_adjusted_svg warns when master_svg is NULL", {
  fw <- Framework$new()
  schema <- data.frame(
    sector = "Health", pillar = "P", sub_pillar = "SP1",
    short_objective = "H1", text_objective = "Obj 1",
    objective_code = 101L,
    stringsAsFactors = FALSE
  )
  fw$master_schema <- schema
  expect_warning(fw$modify_adjusted_svg())
})

test_that("Framework$modify_adjusted_svg warns when master_schema is NULL", {
  fw <- Framework$new()
  fw$master_svg <- '<svg><g id="SP1"><rect x="0" y="0" width="10" height="10" fill="white"/></g></svg>'
  expect_warning(fw$modify_adjusted_svg())
})

test_that("Framework$modify_adjusted_svg colours primary sub-pillars light green", {
  fw <- Framework$new()
  schema <- data.frame(
    sector = "Health", pillar = "P", sub_pillar = c("SP1", "SP2"),
    short_objective = c("H1", "H2"), text_objective = c("Obj 1", "Obj 2"),
    objective_code = c(101L, 102L),
    stringsAsFactors = FALSE
  )
  fw$master_schema <- schema
  fw$master_svg <- paste0(
    '<svg>',
    '<g id="SP1"><rect x="0" y="0" width="10" height="10" fill="white"/></g>',
    '<g id="SP2"><rect x="20" y="0" width="10" height="10" fill="white"/></g>',
    '</svg>'
  )
  fw$primary_objectives <- c(101)
  fw$modify_adjusted_svg()
  expect_true(grepl('#90EE90', fw$adjusted_svg))
  # SP1 should not be coloured as secondary or both
  expect_false(grepl('#ADD8E6', fw$adjusted_svg))
  expect_false(grepl('#DDA0DD', fw$adjusted_svg))
})

test_that("Framework$modify_adjusted_svg colours secondary sub-pillars light blue", {
  fw <- Framework$new()
  schema <- data.frame(
    sector = "Health", pillar = "P", sub_pillar = "SP1",
    short_objective = "H1", text_objective = "Obj 1",
    objective_code = 101L,
    stringsAsFactors = FALSE
  )
  fw$master_schema <- schema
  fw$master_svg <- '<svg><g id="SP1"><rect x="0" y="0" width="10" height="10" fill="white"/></g></svg>'
  fw$secondary_objectives <- c(101)
  fw$modify_adjusted_svg()
  expect_true(grepl('#ADD8E6', fw$adjusted_svg))
})

test_that("Framework$modify_adjusted_svg colours both sub-pillars light purple", {
  fw <- Framework$new()
  schema <- data.frame(
    sector = "Health", pillar = "P", sub_pillar = "SP1",
    short_objective = "H1", text_objective = "Obj 1",
    objective_code = 101L,
    stringsAsFactors = FALSE
  )
  fw$master_schema <- schema
  fw$master_svg <- '<svg><g id="SP1"><rect x="0" y="0" width="10" height="10" fill="white"/></g></svg>'
  fw$primary_objectives  <- c(101)
  fw$secondary_objectives <- c(101)
  fw$modify_adjusted_svg()
  expect_true(grepl('#DDA0DD', fw$adjusted_svg))
})

test_that("Framework$modify_adjusted_svg leaves unselected sub-pillars white", {
  fw <- Framework$new()
  schema <- data.frame(
    sector = "Health", pillar = "P", sub_pillar = c("SP1", "SP2"),
    short_objective = c("H1", "H2"), text_objective = c("Obj 1", "Obj 2"),
    objective_code = c(101L, 102L),
    stringsAsFactors = FALSE
  )
  fw$master_schema <- schema
  fw$master_svg <- paste0(
    '<svg>',
    '<g id="SP1"><rect x="0" y="0" width="10" height="10" fill="white"/></g>',
    '<g id="SP2"><rect x="20" y="0" width="10" height="10" fill="white"/></g>',
    '</svg>'
  )
  fw$primary_objectives <- c(101)
  fw$modify_adjusted_svg()
  # SP2 (code 102) is not in primary or secondary, should not have a special colour
  expect_false(grepl('#90EE90', fw$adjusted_svg) && grepl('#ADD8E6', fw$adjusted_svg))
  expect_false(grepl('#DDA0DD', fw$adjusted_svg))
  # The adjusted SVG must still contain SP2
  expect_true(grepl('id="SP2"', fw$adjusted_svg))
})

test_that("ANAFramework initializes adjusted_svg to match master_svg", {
  .skip_if_no_ana_resources()
  af <- ANAFramework$new()
  skip_if(is.null(af$master_svg))
  expect_equal(af$adjusted_svg, af$master_svg)
})

test_that("ANAFramework$modify_adjusted_svg uses objective codes to colour sub-pillars", {
  .skip_if_no_ana_resources()
  af <- ANAFramework$new()
  skip_if(is.null(af$master_schema) || is.null(af$master_svg))
  skip_if(!"objective_code" %in% names(af$master_schema))

  available_codes <- unique(af$master_schema$objective_code)
  available_codes <- available_codes[!is.na(available_codes)]
  skip_if(length(available_codes) < 2)

  af$primary_objectives  <- available_codes[1]
  af$secondary_objectives <- available_codes[2]
  af$modify_adjusted_svg()

  expect_false(is.null(af$adjusted_svg))
  expect_true(nzchar(af$adjusted_svg))
  # At least one coloured block should appear
  has_colour <- grepl("#90EE90", af$adjusted_svg) ||
    grepl("#ADD8E6", af$adjusted_svg) ||
    grepl("#DDA0DD", af$adjusted_svg)
  expect_true(has_colour)
})
