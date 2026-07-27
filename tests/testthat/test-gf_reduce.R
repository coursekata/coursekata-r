test_that("gf_reduce adds a segment layer", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers) %>%
    gf_model(model)

  result <- gf_reduce(p, model)
  expect_s3_class(result, "ggplot")

  layer_types <- vapply(result$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSegment" %in% layer_types)
})

test_that("gf_square_reduce produces a ggplot with polygon layer", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers) %>%
    gf_model(model)

  result <- suppressMessages(gf_square_reduce(p, model))
  expect_s3_class(result, "ggplot")

  layer_types <- vapply(result$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPolygon" %in% layer_types)
})

test_that("gf_squareduce is a supported alias of gf_square_reduce", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  result <- suppressMessages(gf_squareduce(p, model))
  layer_types <- vapply(result$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPolygon" %in% layer_types)
})

test_that("gf_reduce spans the grand mean to the model prediction at each point", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  built <- ggplot2::ggplot_build(gf_reduce(p, model))
  points <- built$data[[1]]
  segments <- built$data[[length(built$data)]]

  # anchored at the point's x, vertical (x == xend)
  expect_equal(segments$x, points$x)
  expect_equal(segments$xend, points$x)
  # from the empty model's prediction (grand mean) to the complex prediction
  expect_equal(segments$y, rep(mean(stats::fitted(model)), nrow(points)))
  expect_equal(segments$yend, unname(stats::fitted(model)), ignore_attr = TRUE)
})

test_that("gf_square_reduce squares encode SS Model (sum of squared reductions)", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  built <- suppressMessages(ggplot2::ggplot_build(gf_square_reduce(p, model)))
  squares <- split(
    built$data[[length(built$data)]],
    built$data[[length(built$data)]]$group
  )
  # each square's vertical extent is the reduction for that observation
  extents <- vapply(squares, function(s) max(s$y) - min(s$y), numeric(1))

  ss_model <- sum((stats::fitted(model) - mean(stats::fitted(model)))^2)
  expect_equal(sum(extents^2), ss_model, ignore_attr = TRUE)
})

test_that("gf_reduce pins the jitter so overlays stay put", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  plot <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>%
    gf_reduce(model)

  jitter_layer <- Find(function(l) inherits(l$position, "PositionJitter"), plot$layers)
  expect_true(is.finite(jitter_layer$position$seed))

  build_1 <- ggplot2::ggplot_build(plot)$data[[1]]
  build_2 <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_identical(build_1$x, build_2$x)
})

test_that("gf_square_reduce snapshot", {
  skip_if_not_installed("vdiffr")
  set.seed(1)
  penguins_20 <- sample(penguins, 20)
  complex_model <- lm(body_mass_kg ~ flipper_length_m, data = penguins_20)

  p <- gf_point(body_mass_kg ~ flipper_length_m, data = penguins_20) %>%
    gf_model(complex_model)

  suppressMessages(gf_square_reduce(p, complex_model, color = "forestgreen")) %>%
    expect_doppelganger("gf_square_reduce-basic")
})
