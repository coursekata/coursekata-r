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

test_that("gf_squaresid is a supported alias of gf_square_resid", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers) %>%
    gf_model(model)

  result <- suppressMessages(gf_squaresid(p, model))
  layer_types <- vapply(result$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPolygon" %in% layer_types)
})

test_that("gf_resid adds a segment layer", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers) %>%
    gf_model(model)

  result <- gf_resid(p, model)
  expect_s3_class(result, "ggplot")

  layer_types <- vapply(result$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSegment" %in% layer_types)
})

test_that("jittered positions are stable across repeated builds", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  plot <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>%
    gf_resid(model)

  build_1 <- ggplot2::ggplot_build(plot)$data[[1]]
  build_2 <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_identical(build_1$x, build_2$x)
  expect_identical(build_1$y, build_2$y)
})

test_that("gf_resid segments are anchored to the jittered points", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  plot <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>%
    gf_resid(model)

  built <- ggplot2::ggplot_build(plot)
  points <- built$data[[1]]
  segments <- built$data[[length(built$data)]]

  expect_equal(segments$x, points$x)
  expect_equal(segments$xend, points$x)
  expect_equal(segments$yend, points$y)
})

test_that("gf_square_resid squares are anchored to the jittered points", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  plot <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>%
    gf_square_resid(model)

  built <- ggplot2::ggplot_build(plot)
  points <- built$data[[1]]
  squares <- split(built$data[[length(built$data)]], built$data[[length(built$data)]]$group)
  vertex <- function(i) vapply(squares, function(square) square$x[[i]], numeric(1))

  # vertices 1 and 4 are the residual side, so both sit at the point's x
  expect_equal(vertex(1), points$x, ignore_attr = TRUE)
  expect_equal(vertex(4), points$x, ignore_attr = TRUE)
  expect_equal(
    vapply(squares, function(square) square$y[[1]], numeric(1)),
    points$y,
    ignore_attr = TRUE
  )
})

test_that("gf_resid does not reset the user's RNG stream", {
  withr::local_preserve_seed()
  model <- lm(Thumb ~ Sex, data = Fingers)

  # the bracket this replaced reseeded with sample(1:100, 1)
  reachable <- lapply(1:100, function(seed) {
    set.seed(seed)
    .Random.seed
  })

  set.seed(20250711)
  invisible(gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>% gf_resid(model))

  expect_false(any(vapply(reachable, identical, logical(1), .Random.seed)))
})
