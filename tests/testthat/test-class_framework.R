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

test_that("Framework$update_adjusted_schema filters correctly", {
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
  fw$update_adjusted_schema(c("H1", "H3"))
  expect_equal(nrow(fw$adjusted_schema), 2)
  expect_setequal(fw$adjusted_schema$short_objective, c("H1", "H3"))
})

test_that("Framework$update_adjusted_schema with NULL resets to full schema", {
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
  fw$update_adjusted_schema(NULL)
  expect_equal(nrow(fw$adjusted_schema), 2)
})

test_that("Framework$update_adjusted_schema errors without master_schema", {
  fw <- Framework$new()
  expect_error(fw$update_adjusted_schema(c("H1")))
})

test_that("Framework$update_adjusted_svg hides unselected elements", {
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
  svg <- '<svg><rect id="H1"/><rect id="H2"/></svg>'
  fw$set_master_svg(svg)
  fw$update_adjusted_schema(c("H1"))
  fw$update_adjusted_svg()

  expect_false(is.null(fw$adjusted_svg))
  # H2 should be hidden
  expect_true(grepl('visibility:hidden', fw$adjusted_svg))
  # H1 should not be hidden
  expect_false(grepl('id="H1"[^>]*visibility:hidden', fw$adjusted_svg))
})

test_that("Framework$update_adjusted_svg warns when master_svg is NULL", {
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
  fw$update_adjusted_schema(c("H1"))
  expect_warning(fw$update_adjusted_svg())
})

test_that("Framework$export_framework returns a serialisable list", {
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
  exported <- fw$export_framework()
  expect_true(is.list(exported))
  expect_equal(exported$class, "Framework")
  expect_equal(nrow(exported$master_schema), 1)
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
  # update_adjusted_schema should work
  if (!is.null(af$master_schema) && nrow(af$master_schema) > 0) {
    first_obj <- af$master_schema$short_objective[[1]]
    af$update_adjusted_schema(first_obj)
    expect_equal(nrow(af$adjusted_schema), 1)
  }
})

test_that("create_framework returns a Framework object", {
  fw <- create_framework()
  expect_true(inherits(fw, "Framework"))
})

test_that("create_ana_framework returns an ANAFramework object", {
  skip_if_not(
    file.exists(system.file("resources", "reference.xlsx", package = "phr")) ||
      file.exists(file.path("resources", "reference.xlsx")),
    "reference.xlsx not available"
  )
  af <- create_ana_framework()
  expect_true(inherits(af, "ANAFramework"))
  expect_true(inherits(af, "Framework"))
})

test_that("restore_framework reconstructs a Framework from exported data", {
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
  exported <- fw$export_framework()
  restored <- restore_framework(exported)
  expect_true(inherits(restored, "Framework"))
  expect_equal(nrow(restored$master_schema), 1)
  expect_false(is.null(restored$master_svg))
})

test_that("Protocol has a framework field defaulting to NULL", {
  p <- Protocol$new()
  expect_null(p$framework)
})

test_that("Protocol framework field can be set to a Framework object", {
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

test_that("Protocol$export_protocol has NULL framework when none set", {
  p        <- Protocol$new()
  exported <- p$export_protocol()
  expect_null(exported$framework)
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
