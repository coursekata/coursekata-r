test_that("gf_sd_ruler adds a segment layer to a plot", {
  p <- gf_jitter(Thumb ~ Height, data = Fingers)

  result <- suppressMessages(gf_sd_ruler(p))
  expect_s3_class(result, "ggplot")

  layer_types <- vapply(result$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSegment" %in% layer_types)
})

test_that("gf_sd_ruler where parameter options work", {
  p <- gf_jitter(Thumb ~ Height, data = Fingers)

  expect_s3_class(suppressMessages(gf_sd_ruler(p, where = "middle")), "ggplot")
  expect_s3_class(suppressMessages(gf_sd_ruler(p, where = "mean")), "ggplot")
  expect_s3_class(suppressMessages(gf_sd_ruler(p, where = "median")), "ggplot")
})

test_that("gf_sd_ruler works with explicit y and data parameters", {
  p <- gf_jitter(Thumb ~ Height, data = Fingers)

  result <- suppressMessages(
    gf_sd_ruler(p, y = "Thumb", data = Fingers, x = "Height")
  )
  expect_s3_class(result, "ggplot")
})

test_that("gf_sd_ruler errors when y cannot be inferred", {
  p <- ggplot2::ggplot(Fingers) + ggplot2::geom_point(ggplot2::aes(x = Height))
  expect_error(suppressMessages(gf_sd_ruler(p)))
})

test_that("gf_sd_ruler snapshot", {
  skip_if_not_installed("vdiffr")
  p <- gf_jitter(Thumb ~ Height, data = Fingers, seed = 42)

  suppressMessages(gf_sd_ruler(p, color = "red")) %>%
    expect_doppelganger("gf_sd_ruler-basic")
})
