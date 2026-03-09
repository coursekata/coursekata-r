test_that("gf_resid_fun returns a ggplot with segment layer", {
  df <- data.frame(X = 1:10, Y = 2 + 3 * (1:10) + c(1, -1, 2, -2, 0, 1, -1, 0, 2, -2))
  my_fun <- function(x) 2 + 3 * x
  p <- gf_point(Y ~ X, data = df) %>%
    gf_function(my_fun)

  result <- suppressMessages(gf_resid_fun(p, my_fun))
  expect_s3_class(result, "ggplot")

  layer_types <- vapply(result$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSegment" %in% layer_types)
})

test_that("gf_resid_fun works with non-linear function", {
  df <- data.frame(X = 1:20, Y = (1:20)^2 + rnorm(20, sd = 5))
  quad_fun <- function(x) x^2
  p <- gf_point(Y ~ X, data = df)

  result <- suppressMessages(gf_resid_fun(p, quad_fun))
  expect_s3_class(result, "ggplot")
})

test_that("gf_resid_fun passes additional aesthetics", {
  df <- data.frame(X = 1:10, Y = 1:10)
  p <- gf_point(Y ~ X, data = df)
  my_fun <- function(x) x

  result <- suppressMessages(gf_resid_fun(p, my_fun, color = "red", alpha = 0.5))
  expect_s3_class(result, "ggplot")
})

test_that("gf_resid_fun snapshot", {
  skip_if_not_installed("vdiffr")
  df <- data.frame(X = 1:10, Y = c(3, 5, 8, 10, 14, 16, 20, 22, 25, 28))
  my_fun <- function(x) 1 + 3 * x
  p <- gf_point(Y ~ X, data = df) %>%
    gf_function(my_fun)

  suppressMessages(gf_resid_fun(p, my_fun, color = "red")) %>%
    expect_doppelganger("gf_resid_fun-basic")
})
