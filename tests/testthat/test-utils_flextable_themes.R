# Tests for utils_flextable_themes.R

test_that("apply_phr_flextable_theme returns a flextable", {
  ft <- flextable::flextable(data.frame(a = 1:3, b = letters[1:3]))
  ft_themed <- apply_phr_flextable_theme(ft)
  expect_s3_class(ft_themed, "flextable")
})

test_that("apply_phr_flextable_theme applies default REACH red header background", {
  ft <- flextable::flextable(data.frame(a = 1:3, b = letters[1:3]))
  ft_themed <- apply_phr_flextable_theme(ft)
  header_bg <- ft_themed$header$styles$cells$background.color$data[[1]][[1]]
  expect_equal(header_bg, "#EE5859")
})

test_that("apply_phr_flextable_theme accepts palette name string reach2", {
  ft <- flextable::flextable(data.frame(a = 1:3, b = letters[1:3]))
  ft_themed <- apply_phr_flextable_theme(ft, color_palette = "reach2")
  expect_s3_class(ft_themed, "flextable")
  header_bg <- ft_themed$header$styles$cells$background.color$data[[1]][[1]]
  expect_equal(header_bg, get_color_palette("reach2")[1])
})

test_that("apply_phr_flextable_theme accepts palette name string group", {
  ft <- flextable::flextable(data.frame(a = 1:3, b = letters[1:3]))
  ft_themed <- apply_phr_flextable_theme(ft, color_palette = "group")
  expect_s3_class(ft_themed, "flextable")
  header_bg <- ft_themed$header$styles$cells$background.color$data[[1]][[1]]
  expect_equal(header_bg, get_color_palette("group")[1])
})

test_that("apply_phr_flextable_theme warns on invalid input", {
  # Passing a non-flextable should trigger a warning from the error handler
  expect_warning(
    apply_phr_flextable_theme("not a flextable"),
    regexp = NULL
  )
  # Result should be NULL (or the error handler's return value) when input is invalid
  result <- suppressWarnings(apply_phr_flextable_theme("not a flextable"))
  expect_null(result)
})
