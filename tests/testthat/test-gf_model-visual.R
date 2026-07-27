# Visual snapshots of what gf_model(), gf_resid() and gf_square_resid() draw:
# the empty/fit/errorbar model shapes, faceting, and the residual overlays.

test_that("the empty model renders as a horizontal line", {
  gf_point(later_anxiety ~ base_anxiety, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ NULL, data = er)) %>%
    expect_doppelganger("empty model, horizontal")
})

test_that("the empty model renders as a vertical line when the outcome is on x", {
  gf_point(base_anxiety ~ later_anxiety, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ NULL, data = er)) %>%
    expect_doppelganger("empty model, vertical")
})

test_that("a continuous predictor renders as a fit line", {
  gf_point(later_anxiety ~ base_anxiety, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety, data = er)) %>%
    expect_doppelganger("fit line")
})

test_that("a categorical predictor renders as hashes at the group means", {
  gf_point(later_anxiety ~ condition, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ condition, data = er)) %>%
    expect_doppelganger("errorbar hashes")
})

test_that("a model renders across facets", {
  gf_point(later_anxiety ~ base_anxiety, data = er) %>%
    gf_facet_grid(condition ~ .) %>%
    gf_model(lm(later_anxiety ~ base_anxiety, data = er)) %>%
    expect_doppelganger("faceted fit line")
})

test_that("residual segments render", {
  model <- lm(Thumb ~ Height, data = Fingers)
  gf_point(Thumb ~ Height, data = Fingers) %>%
    gf_model(model) %>%
    gf_resid(model) %>%
    expect_doppelganger("residual segments")
})

test_that("squared residuals render", {
  model <- lm(Thumb ~ Height, data = Fingers)
  suppressMessages(
    gf_point(Thumb ~ Height, data = Fingers) %>%
      gf_model(model) %>%
      gf_square_resid(model)
  ) %>%
    expect_doppelganger("squared residuals")
})

test_that("it un-maps dynamic aesthetics from underlying layers that are not in the model", {
  plot <- gf_point(
    later_anxiety ~ base_anxiety,
    color = ~condition, shape = ~provider, data = er
  ) %>%
    gf_model(lm(later_anxiety ~ base_anxiety, data = er))

  built <- ggplot2::ggplot_build(plot)
  points <- built$data[[1]]
  model <- built$data[[layer_index(plot, "model")]]

  # the base layer keeps what the user mapped
  expect_length(unique(points$colour), 2)
  expect_length(unique(points$shape), 3)

  # neither variable is in the model, so the model layer takes the geom's
  # default instead of inheriting the mapping
  expect_equal(unique(model$colour), ggplot2::GeomLine$default_aes$colour)

  # the grid is still built across the plot's aesthetics, so the line is drawn
  # once per provider -- overlapping copies of one line, not a line per group
  traces <- split(model[order(model$x), c("x", "y")], model$group[order(model$x)])
  expect_length(traces, 3)
  for (trace in traces[-1]) expect_equal(trace$y, traces[[1]]$y)
})
