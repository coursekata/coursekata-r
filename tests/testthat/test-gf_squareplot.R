test_that("gf_squareplot works with formula input", {
  result <- suppressMessages(gf_squareplot(~Thumb, data = Fingers))
  expect_s3_class(result, "gf_squareplot")
  expect_s3_class(result, "ggplot")
})

test_that("gf_squareplot works with numeric vector input", {
  result <- suppressMessages(gf_squareplot(Fingers$Thumb))
  expect_s3_class(result, "gf_squareplot")
})

test_that("gf_squareplot auto-selects binwidth 1 for integers", {
  int_data <- data.frame(x = sample(1:10, 50, replace = TRUE))
  result <- suppressMessages(gf_squareplot(~x, data = int_data))
  expect_s3_class(result, "gf_squareplot")
})

test_that("gf_squareplot bars parameter options work", {
  result_none <- suppressMessages(gf_squareplot(~Thumb, data = Fingers, bars = "none"))
  result_outline <- suppressMessages(gf_squareplot(~Thumb, data = Fingers, bars = "outline"))
  result_solid <- suppressMessages(gf_squareplot(~Thumb, data = Fingers, bars = "solid"))
  expect_s3_class(result_none, "gf_squareplot")
  expect_s3_class(result_outline, "gf_squareplot")
  expect_s3_class(result_solid, "gf_squareplot")
})

test_that("gf_squareplot auto-switches to solid bars for >75 obs per bin", {
  # Create data where one bin has many observations
  big_data <- data.frame(x = rep(5, 100))
  result <- suppressMessages(gf_squareplot(~x, data = big_data))
  expect_s3_class(result, "gf_squareplot")
})

test_that("gf_squareplot handles factor input with zero-count levels", {
  f <- factor(c(1, 1, 3, 3), levels = 1:5)
  df <- data.frame(x = f)
  result <- suppressMessages(gf_squareplot(~x, data = df))
  expect_s3_class(result, "gf_squareplot")
})

test_that("gf_squareplot errors on non-numeric input", {
  expect_error(suppressMessages(gf_squareplot(c("a", "b", "c"))))
})

test_that("gf_squareplot print method suppresses warnings", {
  result <- suppressMessages(gf_squareplot(~Thumb, data = Fingers))
  expect_no_warning(print(result))
})

test_that("gf_squareplot snapshot", {
  skip_if_not_installed("vdiffr")
  int_data <- data.frame(x = c(1, 1, 2, 2, 2, 3, 3, 4, 5, 5))
  suppressMessages(gf_squareplot(~x, data = int_data)) %>%
    expect_doppelganger("gf_squareplot-basic-int")
})
