test_that("gf_square_resid_fun returns a ggplot with polygon layer", {
  df <- data.frame(X = 1:10, Y = 2 + 3 * (1:10) + c(1, -1, 2, -2, 0, 1, -1, 0, 2, -2))
  my_fun <- function(x) 2 + 3 * x
  p <- gf_point(Y ~ X, data = df) %>%
    gf_function(my_fun)

  result <- suppressMessages(gf_square_resid_fun(p, my_fun))
  expect_s3_class(result, "ggplot")

  layer_types <- vapply(result$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPolygon" %in% layer_types)
})

test_that("gf_square_resid_fun respects aspect ratio parameter", {
  df <- data.frame(X = 1:10, Y = 1:10 + c(1, -1, 2, -2, 0, 1, -1, 0, 2, -2))
  my_fun <- function(x) x
  p <- gf_point(Y ~ X, data = df)

  result_default <- suppressMessages(gf_square_resid_fun(p, my_fun))
  result_custom <- suppressMessages(gf_square_resid_fun(p, my_fun, aspect = 1))
  expect_s3_class(result_default, "ggplot")
  expect_s3_class(result_custom, "ggplot")
})

test_that("gf_square_resid_fun passes additional aesthetics", {
  df <- data.frame(X = 1:10, Y = 1:10)
  p <- gf_point(Y ~ X, data = df)
  my_fun <- function(x) x

  result <- suppressMessages(
    gf_square_resid_fun(p, my_fun, color = "blue", fill = "lightblue")
  )
  expect_s3_class(result, "ggplot")
})

test_that("gf_square_resid_fun snapshot", {
  skip_if_not_installed("vdiffr")
  df <- data.frame(X = 1:10, Y = c(3, 5, 8, 10, 14, 16, 20, 22, 25, 28))
  my_fun <- function(x) 1 + 3 * x
  p <- gf_point(Y ~ X, data = df) %>%
    gf_function(my_fun)

  suppressMessages(gf_square_resid_fun(p, my_fun, color = "blue")) %>%
    expect_doppelganger("gf_square_resid_fun-basic")
})
