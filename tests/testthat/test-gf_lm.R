test_that("gf_lm adds a regression line for a continuous x", {
  p <- gf_point(Thumb ~ Height, data = Fingers)
  result <- suppressMessages(gf_lm(p))

  expect_s3_class(result, "ggplot")
  expect_gt(length(result$layers), length(p$layers))
  stats <- vapply(result$layers, function(l) class(l$stat)[1], character(1))
  expect_true("StatLm" %in% stats)
})

test_that("gf_lm draws group-mean segments for a categorical x", {
  p <- gf_jitter(Tip ~ Condition, data = TipExperiment, width = .1)
  result <- suppressMessages(gf_lm(p))

  geoms <- vapply(result$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSegment" %in% geoms)
})

test_that("gf_lm called standalone behaves like ggformula::gf_lm", {
  standalone <- suppressMessages(gf_lm(Thumb ~ Height, data = Fingers))
  expect_s3_class(standalone, "ggplot")
})

test_that("gf_lm snapshot", {
  skip_if_not_installed("vdiffr")
  suppressMessages(
    gf_point(Thumb ~ Height, data = Fingers, alpha = .3) %>%
      gf_lm()
  ) %>%
    expect_doppelganger("gf_lm-continuous")
})
