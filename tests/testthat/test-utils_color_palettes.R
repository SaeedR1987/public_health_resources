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

test_that("water_colors returns 3 named tints by default", {
  result <- water_colors()
  expect_length(result, 3)
  expect_named(result, c("80%", "55%", "30%"))
})

test_that("water_colors returns correct hex values", {
  result <- water_colors()
  expect_equal(unname(result["80%"]), "#53B4DB")
  expect_equal(unname(result["55%"]), "#89CBE6")
  expect_equal(unname(result["30%"]), "#BEE3F2")
})

test_that("water_colors respects n <= 3", {
  result <- water_colors(n = 2)
  expect_length(result, 2)
  expect_equal(unname(result[1]), "#53B4DB")
})

test_that("water_colors interpolates when n > 3", {
  result <- water_colors(n = 6)
  expect_length(result, 6)
  expect_type(result, "character")
})

test_that("water_colors reverses order when reverse = TRUE", {
  fwd <- water_colors()
  rev_result <- water_colors(reverse = TRUE)
  expect_equal(unname(rev_result[1]), unname(fwd[3]))
  expect_equal(unname(rev_result[3]), unname(fwd[1]))
})

# ============================================================
# 3. SANITATION COLOR PALETTE TESTS
# ============================================================

test_that("sanitation_colors returns 3 named tints by default", {
  result <- sanitation_colors()
  expect_length(result, 3)
  expect_named(result, c("80%", "55%", "30%"))
})

test_that("sanitation_colors returns correct hex values", {
  result <- sanitation_colors()
  expect_equal(unname(result["80%"]), "#75599F")
  expect_equal(unname(result["55%"]), "#A08DBD")
  expect_equal(unname(result["30%"]), "#CBC1DB")
})

test_that("sanitation_colors interpolates when n > 3", {
  result <- sanitation_colors(n = 5)
  expect_length(result, 5)
  expect_type(result, "character")
})

test_that("sanitation_colors reverses order when reverse = TRUE", {
  fwd <- sanitation_colors()
  rev_result <- sanitation_colors(reverse = TRUE)
  expect_equal(unname(rev_result[1]), unname(fwd[3]))
})

# ============================================================
# 4. HYGIENE COLOR PALETTE TESTS
# ============================================================

test_that("hygiene_colors returns 3 named tints by default", {
  result <- hygiene_colors()
  expect_length(result, 3)
  expect_named(result, c("80%", "55%", "30%"))
})

test_that("hygiene_colors returns correct hex values", {
  result <- hygiene_colors()
  expect_equal(unname(result["80%"]), "#33A46D")
  expect_equal(unname(result["55%"]), "#73C09A")
  expect_equal(unname(result["30%"]), "#B2DDC8")
})

test_that("hygiene_colors interpolates when n > 3", {
  result <- hygiene_colors(n = 7)
  expect_length(result, 7)
  expect_type(result, "character")
})

test_that("hygiene_colors reverses order when reverse = TRUE", {
  fwd <- hygiene_colors()
  rev_result <- hygiene_colors(reverse = TRUE)
  expect_equal(unname(rev_result[1]), unname(fwd[3]))
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
