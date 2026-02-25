test_that("gf_square_resid produces a ggplot with polygon layer", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers) %>%
    gf_model(model)

  result <- suppressMessages(gf_square_resid(p, model))
  expect_s3_class(result, "ggplot")

  # Check that a polygon layer was added
  layer_types <- vapply(result$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPolygon" %in% layer_types)
})

test_that("gf_squaresid emits deprecation warning and delegates to gf_square_resid", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers) %>%
    gf_model(model)

  lifecycle::expect_deprecated(
    suppressMessages(gf_squaresid(p, model))
  )
})

test_that("gf_resid is unchanged and works without deprecation warning", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers) %>%
    gf_model(model)

  expect_no_warning(gf_resid(p, model))
  result <- gf_resid(p, model)
  expect_s3_class(result, "ggplot")
})
