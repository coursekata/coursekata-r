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
  expect_error(suppressMessages(gf_sd_ruler(p)), "Can't infer the outcome variable")
})

test_that("gf_sd_ruler measures the x variable on a histogram", {
  p <- suppressMessages(gf_histogram(~Thumb, data = Fingers, bins = 30))

  built <- ggplot2::ggplot_build(suppressMessages(gf_sd_ruler(p)))
  segment <- built$data[[length(built$data)]]

  expect_equal(segment$x, mean(Fingers$Thumb))
  expect_equal(segment$xend, mean(Fingers$Thumb) + sd(Fingers$Thumb))
  expect_equal(segment$y, 0)
  expect_equal(segment$yend, 0)
})

test_that("gf_sd_ruler takes the outcome as a bare name, a string, or a variable", {
  p <- gf_jitter(Thumb ~ Height, data = Fingers)
  held <- "Thumb"

  ends <- function(plot) {
    built <- ggplot2::ggplot_build(suppressMessages(plot))
    unlist(built$data[[length(built$data)]][c("y", "yend")])
  }

  expect_equal(ends(gf_sd_ruler(p, Thumb)), ends(gf_sd_ruler(p, "Thumb")))
  expect_equal(ends(gf_sd_ruler(p, held)), ends(gf_sd_ruler(p, "Thumb")))
})

test_that("gf_sd_ruler prefers a real column over a like-named object in scope", {
  p <- gf_jitter(Thumb ~ Height, data = Fingers)
  Thumb <- "Height" # nolint: object_name_linter.

  built <- ggplot2::ggplot_build(suppressMessages(gf_sd_ruler(p, Thumb)))
  expect_equal(built$data[[length(built$data)]]$y, mean(Fingers$Thumb))
})

test_that("gf_sd_ruler rejects an outcome it cannot measure", {
  categorical <- suppressMessages(gf_bar(~Sex, data = Fingers))
  expect_error(suppressMessages(gf_sd_ruler(categorical)), "not a quantitative variable")

  p <- gf_jitter(Thumb ~ Height, data = Fingers)
  expect_error(suppressMessages(gf_sd_ruler(p, "Nope")), "Can't find")

  transformed <- suppressMessages(gf_histogram(~ log(Thumb), data = Fingers, bins = 30))
  expect_error(suppressMessages(gf_sd_ruler(transformed)), "Can't read the x variable")
})

test_that("gf_sd_ruler snapshot", {
  skip_if_not_installed("vdiffr")
  p <- gf_jitter(Thumb ~ Height, data = Fingers, seed = 42)

  suppressMessages(gf_sd_ruler(p, color = "red")) %>%
    expect_doppelganger("gf_sd_ruler-basic")
})
