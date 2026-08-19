# Visual snapshots of what gf_model(), gf_resid() and gf_square_resid() draw:
# the empty/fit/group model shapes, faceting, and the residual overlays.

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

test_that("a categorical predictor renders as a mark at each group mean", {
  gf_point(later_anxiety ~ condition, color = ~condition, data = er) %>%
    gf_model(lm(later_anxiety ~ condition, data = er)) %>%
    expect_doppelganger("group mark at each mean")
})

test_that("an inferred categorical model renders group marks where gf_lm() draws nothing", {
  # seeded so the points layer's jitter -- unaffected by gf_model(), which
  # never pins position -- does not make this snapshot flaky
  gf_jitter(Thumb ~ Sex, data = Fingers, width = .1, seed = 42) %>%
    gf_model() %>%
    expect_doppelganger("inferred categorical model")
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

test_that("the three-color decomposition: reduction plus residual squares to the same page", {
  # the only genuinely visual claim here -- "blue = red + green" is about areas
  # on the page, which the sum-of-squares and decomposition tests in
  # test-gf_reduce.R already carry the arithmetic for; this snapshot is what
  # confirms the squares still read as one picture
  set.seed(1)
  # R 4.5 added datasets::penguins, whose columns are named differently; qualify
  # the package's own rather than risk a later worker/file resolving the other
  # one -- see test-gf_model-formula.R's top-of-file guard for the same trap
  penguins_20 <- mosaic::sample(coursekata::penguins, 20)
  model <- lm(body_mass_kg ~ flipper_length_m, data = penguins_20)

  suppressMessages(
    gf_point(body_mass_kg ~ flipper_length_m, data = penguins_20) %>%
      gf_model(model) %>%
      gf_square_resid(model, color = "firebrick") %>%
      gf_square_reduce(model, color = "blue")
  ) %>%
    expect_doppelganger("reduce and resid squares decompose the model")
})
