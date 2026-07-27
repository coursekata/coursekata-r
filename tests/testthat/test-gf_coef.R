test_that("gf_coef adds annotation layers for a continuous model", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  result <- suppressMessages(gf_coef(p, model))
  expect_s3_class(result, "ggplot")

  geoms <- vapply(result$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSegment" %in% geoms) # the rise/run slope arrows
  expect_gt(length(result$layers), length(p$layers))
})

test_that("gf_coef marks b0 with a horizontal line for a categorical model", {
  model <- lm(Tip ~ Condition, data = TipExperiment)
  p <- gf_jitter(Tip ~ Condition, data = TipExperiment, width = .1)

  result <- suppressMessages(gf_coef(p, model))
  geoms <- vapply(result$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomHline" %in% geoms) # b0 reference line
  expect_true("GeomSegment" %in% geoms) # coefficient arrow(s)
})

test_that("gf_coef fits a model from the plot when none is given", {
  p <- gf_point(Thumb ~ Height, data = Fingers)
  result <- suppressMessages(gf_coef(p))

  expect_s3_class(result, "ggplot")
  expect_gt(length(result$layers), length(p$layers))
})

test_that("gf_b is an alias for gf_coef", {
  expect_identical(gf_b, gf_coef)
})

test_that("nice_run picks a power of 10 near 10% of the span", {
  expect_equal(nice_run(100), 10)
  expect_equal(nice_run(10), 1)
})

test_that("format_run drops trailing zeros", {
  expect_equal(format_run(10), "10")
  expect_equal(format_run(2.5), "2.5")
  expect_equal(format_run(1), "1")
})

test_that("gf_coef snapshot", {
  skip_if_not_installed("vdiffr")
  model <- lm(Thumb ~ Height, data = Fingers)
  suppressMessages(
    gf_point(Thumb ~ Height, data = Fingers, alpha = .3) %>%
      gf_coef(model)
  ) %>%
    expect_doppelganger("gf_coef-continuous")
})
