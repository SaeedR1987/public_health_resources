# ---------------------------
# UTILS COLOR PALETTES TEST SUITE
# ---------------------------

library(testthat)

# ============================================================
# 1. IPC COLOR PALETTE TESTS
# ============================================================

test_that("ipc_colors returns 5 unnamed colors by default", {
  result <- ipc_colors()
  expect_length(result, 5)
  expect_null(names(result))
})

test_that("ipc_colors returns correct hex values ordered worst to best (P5 to P1)", {
  result <- ipc_colors()
  expect_equal(result[1], "#8C0000")  # P5 Famine (worst)
  expect_equal(result[2], "#FF0000")  # P4 Emergency
  expect_equal(result[3], "#FF9900")  # P3 Crisis
  expect_equal(result[4], "#FFEE00")  # P2 Stressed
  expect_equal(result[5], "#A1FE8D")  # P1 Minimal (best)
})

test_that("ipc_colors respects n parameter", {
  result <- ipc_colors(n = 3)
  expect_length(result, 3)
  expect_equal(result[1], "#8C0000")  # first 3 worst colors
})

test_that("ipc_colors with n = 4 returns worst 4 colors", {
  result <- ipc_colors(n = 4)
  expect_length(result, 4)
  expect_equal(result[1], "#8C0000")  # P5
  expect_equal(result[4], "#FFEE00")  # P2
})

test_that("ipc_colors interpolates when n > 5", {
  result <- ipc_colors(n = 8)
  expect_length(result, 8)
})

test_that("ipc_colors reverses order when reverse = TRUE", {
  fwd <- ipc_colors()
  rev_result <- ipc_colors(reverse = TRUE)
  expect_equal(unname(rev_result[1]), unname(fwd[5]))
  expect_equal(unname(rev_result[5]), unname(fwd[1]))
})

# ============================================================
# 2. WATER COLOR PALETTE TESTS
# ============================================================

test_that("water_colors returns 3 hex colors by default (unnamed)", {
  result <- water_colors()

  expect_type(result, "character")
  expect_length(result, 3)
  expect_null(names(result))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))
})

test_that("water_colors returns correct default hex values", {
  result <- water_colors()

  expect_equal(result, c("#53B4DB", "#89CBE6", "#BEE3F2"))
})

test_that("water_colors respects n <= 3 by truncating", {
  result <- water_colors(n = 2)

  expect_type(result, "character")
  expect_length(result, 2)
  expect_equal(result, c("#53B4DB", "#89CBE6"))
})

test_that("water_colors interpolates when n > 3", {
  result <- water_colors(n = 6)

  expect_type(result, "character")
  expect_length(result, 6)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))

  # sanity: gradient includes the endpoints used by colorRampPalette()
  expect_equal(result[1], "#28A1D2")
  expect_equal(result[6], "#BEE3F2")
})

test_that("water_colors reverses order when reverse = TRUE (default n)", {
  fwd <- water_colors()
  rev_result <- water_colors(reverse = TRUE)

  expect_equal(rev_result, rev(fwd))
})

test_that("water_colors reverses gradient when reverse = TRUE and n > 3", {
  fwd <- water_colors(n = 6)
  rev_result <- water_colors(n = 6, reverse = TRUE)

  expect_equal(rev_result, rev(fwd))
})

# ============================================================
# 3. SANITATION COLOR PALETTE TESTS
# ============================================================

test_that("sanitation_colors returns 3 hex colors by default (unnamed)", {
  result <- sanitation_colors()

  expect_type(result, "character")
  expect_length(result, 3)
  expect_null(names(result))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))
})

test_that("sanitation_colors returns correct default hex values", {
  result <- sanitation_colors()

  expect_equal(result, c("#75599F", "#A08DBD", "#CBC1DB"))
})

test_that("sanitation_colors respects n <= 3 by truncating", {
  result <- sanitation_colors(n = 2)

  expect_type(result, "character")
  expect_length(result, 2)
  expect_equal(result, c("#75599F", "#A08DBD"))
})

test_that("sanitation_colors interpolates when n > 3", {
  result <- sanitation_colors(n = 5)

  expect_type(result, "character")
  expect_length(result, 5)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))

  # sanity: gradient includes the endpoints used by colorRampPalette()
  expect_equal(result[1], "#532F87")
  expect_equal(result[5], "#CBC1DB")
})

test_that("sanitation_colors reverses order when reverse = TRUE (default n)", {
  fwd <- sanitation_colors()
  rev_result <- sanitation_colors(reverse = TRUE)

  expect_equal(rev_result, rev(fwd))
})

test_that("sanitation_colors reverses gradient when reverse = TRUE and n > 3", {
  fwd <- sanitation_colors(n = 4)
  rev_result <- sanitation_colors(n = 4, reverse = TRUE)

  expect_equal(rev_result, rev(fwd))
})

# ============================================================
# 4. HYGIENE COLOR PALETTE TESTS
# ============================================================

test_that("hygiene_colors returns 3 hex colors by default (unnamed)", {
  result <- hygiene_colors()

  expect_type(result, "character")
  expect_length(result, 3)
  expect_null(names(result))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))
})

test_that("hygiene_colors returns correct default hex values", {
  result <- hygiene_colors()

  expect_equal(result, c("#33A46D", "#73C09A", "#B2DDC8"))
})

test_that("hygiene_colors interpolates when n > 3", {
  result <- hygiene_colors(n = 7)

  expect_type(result, "character")
  expect_length(result, 7)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", result)))
})

test_that("hygiene_colors reverses order when reverse = TRUE (default n)", {
  fwd <- hygiene_colors()
  rev_result <- hygiene_colors(reverse = TRUE)

  expect_equal(rev_result, rev(fwd))
})

test_that("hygiene_colors reverses gradient when reverse = TRUE and n > 3", {
  fwd <- hygiene_colors(n = 4)
  rev_result <- hygiene_colors(n = 4, reverse = TRUE)

  expect_equal(rev_result, rev(fwd))
})

# ============================================================
# 5. GET_COLOR_PALETTE INTEGRATION TESTS
# ============================================================

test_that("get_color_palette supports 'ipc' type", {
  result <- get_color_palette("ipc")
  expect_length(result, 5)
})

test_that("get_color_palette supports 'water' type", {
  result <- get_color_palette("water")
  expect_length(result, 3)
})

test_that("get_color_palette supports 'sanitation' type", {
  result <- get_color_palette("sanitation")
  expect_length(result, 3)
})

test_that("get_color_palette supports 'hygiene' type", {
  result <- get_color_palette("hygiene")
  expect_length(result, 3)
})

test_that("get_color_palette 'water' with n > 3 interpolates", {
  result <- get_color_palette("water", n = 6)
  expect_length(result, 6)
})

test_that("get_color_palette 'sanitation' with reverse returns reversed colors", {
  fwd <- get_color_palette("sanitation")
  rev_result <- get_color_palette("sanitation", reverse = TRUE)
  expect_equal(unname(rev_result[1]), unname(fwd[3]))
})

test_that("get_color_palette rejects invalid type", {
  expect_error(get_color_palette("wash"))
  expect_error(get_color_palette("invalid_type"))
})

