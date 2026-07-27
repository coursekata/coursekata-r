test_that("plot_spec reads a two-axis plot with a mapped aesthetic", {
  p <- gf_point(later_anxiety ~ base_anxiety, color = ~condition, data = er)
  spec <- plot_spec(p)

  expect_equal(spec$axes, c(x = "base_anxiety", y = "later_anxiety"))
  expect_equal(spec$aesthetics, c(colour = "condition"))
  expect_equal(spec$facets, character(0))
  expect_identical(spec$data, er)
})

test_that("plot_spec reports facet variables separately from axes", {
  p <- gf_histogram(~body_mass_kg, data = penguins) %>%
    gf_facet_grid(species ~ .)
  spec <- plot_spec(p)

  expect_equal(unname(spec$axes), "body_mass_kg")
  expect_equal(spec$facets, "species")
  expect_false("species" %in% spec$axes)

  # a model may predict from the facet variable, so it has to count as on the plot
  expect_true("species" %in% spec$variables)
  expect_equal(spec$variables[["facet"]], "species")
})

test_that("plot_spec on a one-axis plot reports only the mapped axis", {
  p <- gf_histogram(~later_anxiety, data = er)
  spec <- plot_spec(p)

  expect_equal(unname(spec$axes), "later_anxiety")
  expect_equal(spec$aesthetics, setNames(character(0), character(0)))
})

test_that("plot_geometry returns the rendered positions and panel ranges", {
  p <- gf_point(Thumb ~ Height, data = Fingers)
  geom <- plot_geometry(p)

  expect_length(geom$x, nrow(Fingers))
  expect_length(geom$y, nrow(Fingers))
  expect_equal(geom$x, Fingers$Height)
  expect_equal(geom$y, Fingers$Thumb)
  expect_length(geom$x_range, 2)
  expect_length(geom$y_range, 2)
  expect_true(geom$x_range[1] < min(Fingers$Height))
})

test_that("plot_geometry reads the observations, not a model layer drawn over them", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_model(model)
  geom <- plot_geometry(p)

  expect_equal(geom$x, Fingers$Height)
  expect_equal(geom$y, Fingers$Thumb)
})

test_that("plot_geometry is stable across repeated reads of a jittered plot", {
  p <- freeze_jitter(gf_jitter(Thumb ~ Sex, data = Fingers, width = .1))

  expect_equal(plot_geometry(p)$x, plot_geometry(p)$x)
  expect_equal(plot_geometry(p)$y, plot_geometry(p)$y)
})

test_that("resolve_aes finds an aesthetic mapped on the plot", {
  p <- ggplot2::ggplot(Fingers, ggplot2::aes(x = Thumb)) + ggplot2::geom_histogram()
  x <- plot_spec(p)$resolve_aes("x")

  expect_equal(rlang::as_name(x$quo), "Thumb")
  expect_identical(x$data, Fingers)
})

test_that("resolve_aes falls back to a layer's mapping and its own data", {
  p <- ggplot2::ggplot() +
    ggplot2::geom_histogram(data = Fingers, mapping = ggplot2::aes(x = Thumb))
  x <- plot_spec(p)$resolve_aes("x")

  expect_equal(rlang::as_name(x$quo), "Thumb")
  expect_identical(x$data, Fingers)
})

test_that("resolve_aes prefers the plot's mapping when both plot and layer map it", {
  p <- ggplot2::ggplot(Fingers, ggplot2::aes(x = Thumb)) +
    ggplot2::geom_histogram(ggplot2::aes(x = Height))
  x <- plot_spec(p)$resolve_aes("x")

  expect_equal(rlang::as_name(x$quo), "Thumb")
})

test_that("resolve_aes returns NULL when an aesthetic is mapped nowhere", {
  p <- ggplot2::ggplot(Fingers, ggplot2::aes(x = Thumb)) + ggplot2::geom_histogram()

  expect_null(plot_spec(p)$resolve_aes("fill"))
})
