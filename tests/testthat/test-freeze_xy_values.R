test_that("freeze_xy_values locks evaluated x/y into hidden columns", {
  p <- gf_point(log(Thumb) ~ Height, data = Fingers)
  fp <- freeze_xy_values(p)

  expect_true(all(c(".gf_x", ".gf_y") %in% names(fp$data)))
  expect_equal(fp$data$.gf_y, log(Fingers$Thumb))
  expect_equal(fp$data$.gf_x, Fingers$Height)
})

test_that("freeze_xy_values preserves the original axis labels", {
  p <- gf_point(log(Thumb) ~ Height, data = Fingers)
  fp <- freeze_xy_values(p)

  # titles should read the original expressions, not the hidden column names
  expect_identical(fp$labels$y, "log(Thumb)")
  expect_identical(fp$labels$x, "Height")
})

test_that("freeze_xy_values is idempotent", {
  p <- gf_point(log(Thumb) ~ Height, data = Fingers)
  fp <- freeze_xy_values(p)
  fp2 <- freeze_xy_values(fp)

  expect_identical(fp2$data$.gf_y, fp$data$.gf_y)
  expect_identical(fp2$data$.gf_x, fp$data$.gf_x)
})

test_that("freeze_xy_values evaluates a random expression once", {
  # shuffle() re-rolls on each evaluation; freezing should capture a single draw
  set.seed(10)
  p <- gf_jitter(shuffle(Thumb) ~ Sex, data = Fingers, width = .1)
  fp <- freeze_xy_values(p)

  # the raw-data layer now maps y to the frozen column
  expect_true(".gf_y" %in% names(fp$layers[[1]]$data))
  # and the frozen values are a permutation of the original Thumb values
  expect_equal(sort(fp$data$.gf_y), sort(Fingers$Thumb))
})
