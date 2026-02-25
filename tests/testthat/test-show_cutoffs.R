test_that("show_cutoffs works with middle() fill", {
  p <- gf_histogram(~Thumb, data = Fingers, fill = ~middle(Thumb, .95))
  result <- suppressMessages(show_cutoffs(p))
  expect_s3_class(result, "ggplot")
})

test_that("show_cutoffs works with tails() fill", {
  p <- gf_histogram(~Thumb, data = Fingers, fill = ~tails(Thumb, .95))
  result <- suppressMessages(show_cutoffs(p))
  expect_s3_class(result, "ggplot")
})

test_that("show_cutoffs works with outer() fill", {
  p <- gf_histogram(~Thumb, data = Fingers, fill = ~outer(Thumb, .05))
  result <- suppressMessages(show_cutoffs(p))
  expect_s3_class(result, "ggplot")
})

test_that("show_cutoffs works with upper() fill", {
  p <- gf_histogram(~Thumb, data = Fingers, fill = ~upper(Thumb, .05))
  result <- suppressMessages(show_cutoffs(p))
  expect_s3_class(result, "ggplot")
})

test_that("show_cutoffs works with lower() fill", {
  p <- gf_histogram(~Thumb, data = Fingers, fill = ~lower(Thumb, .05))
  result <- suppressMessages(show_cutoffs(p))
  expect_s3_class(result, "ggplot")
})

test_that("show_cutoffs labels parameter adds text annotations", {
  p <- gf_histogram(~Thumb, data = Fingers, fill = ~middle(Thumb, .95))
  result <- suppressMessages(show_cutoffs(p, labels = TRUE))
  expect_s3_class(result, "ggplot")
})

test_that("show_cutoffs errors when fill does not use a distribution function", {
  p <- gf_histogram(~Thumb, data = Fingers)
  expect_error(suppressMessages(show_cutoffs(p)))
})

test_that("show_cutoffs snapshot", {
  skip_if_not_installed("vdiffr")
  p <- gf_histogram(~Thumb, data = Fingers, fill = ~middle(Thumb, .95), bins = 30)
  suppressMessages(show_cutoffs(p)) %>%
    expect_doppelganger("show_cutoffs-middle-95")
})
