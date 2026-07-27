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

test_that("a model fit on fewer rows than the plot draws says so", {
  # lm() drops Fingers' 29 rows with a missing SSLast; the plot still draws all 157
  model <- lm(SSLast ~ Height, data = Fingers)
  p <- gf_point(SSLast ~ Height, data = Fingers)

  expect_error(gf_resid(p, model), "cover the same observations")
  expect_error(gf_resid(p, model), "draws 157 points")
  expect_error(gf_resid(p, model), "gives 128 fitted values")
  expect_error(suppressMessages(gf_square_resid(p, model)), "cover the same observations")
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
  segments <- built$data[[layer_index(plot, "resid")]]

  expect_equal(segments$x, points$x)
  expect_equal(segments$xend, points$x)
  expect_equal(segments$yend, points$y)
})

test_that("gf_resid segments inherit the plot's mapped aesthetics", {
  model <- lm(Thumb ~ Height, data = Fingers)

  colored <- gf_resid(gf_point(Thumb ~ Height, color = ~Sex, data = Fingers), model)
  color_segments <- ggplot2::ggplot_build(colored)$data[[layer_index(colored, "resid")]]
  expect_gt(length(unique(color_segments$colour)), 1)

  faded <- gf_resid(gf_point(Thumb ~ Height, alpha = ~Height, data = Fingers), model)
  alpha_segments <- ggplot2::ggplot_build(faded)$data[[layer_index(faded, "resid")]]
  expect_gt(length(unique(alpha_segments$alpha)), 1)
})

test_that("gf_square_resid squares are anchored to the jittered points", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  plot <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>%
    gf_square_resid(model)

  built <- ggplot2::ggplot_build(plot)
  points <- built$data[[1]]
  drawn <- built$data[[layer_index(plot, "square_resid")]]
  squares <- split(drawn, drawn$group)
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

test_that("gf_square_resid draws squares on a jittered plot, measured off the panel", {
  # the regression this guards lived here, not in the plan: the drawing once took
  # its vertical side from the displayed (jittered) y and its horizontal side from
  # stats::resid(model), which differ by exactly the jitter
  model <- lm(Thumb ~ Sex, data = Fingers)
  plot <- suppressMessages(
    gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>% gf_square_resid(model)
  )

  built <- ggplot2::ggplot_build(plot)
  panel <- built$layout$panel_params[[1]]
  points <- built$data[[1]]
  drawn <- built$data[[layer_index(plot, "square_resid")]]
  squares <- split(drawn, drawn$group)

  side <- function(axis, range) {
    vapply(squares, function(square) diff(range(square[[axis]])) / diff(range), numeric(1))
  }
  widths <- side("x", panel$x.range)
  heights <- side("y", panel$y.range)

  expect_equal(
    heights,
    abs(points$y - stats::predict(model)) / diff(panel$y.range),
    ignore_attr = TRUE
  )
  expect_equal(widths, heights * (4 / 6))
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

test_that("gf_resid seeds a copy of the jitter position, not the shared object", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  base <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1)

  # hold a reference to the layer's position object before freezing. geom_jitter()
  # and position = "jitter" share ggplot2's namespace-level PositionJitter, so
  # writing a seed onto this object in place would leak it into unrelated plots.
  original_pos <- base$layers[[1]]$position
  expect_false(is.finite(original_pos$seed))

  frozen <- gf_resid(base, model)

  # the frozen plot carries a seed, but on a *new* object -- the one we captured
  # is left untouched, proving the seed was copied on rather than mutated in
  expect_true(is.finite(frozen$layers[[1]]$position$seed))
  expect_false(identical(frozen$layers[[1]]$position, original_pos))
  expect_false(is.finite(original_pos$seed))
})
