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
  p <- resid_jitter(gf_jitter(Thumb ~ Sex, data = Fingers, width = .1))$plot

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

test_that("plot_spec reads axes a plot maps only on its first layer", {
  p <- ggplot2::ggplot(er) + ggplot2::geom_hex(ggplot2::aes(base_anxiety, later_anxiety))
  spec <- plot_spec(p)

  expect_equal(spec$axes, c(x = "base_anxiety", y = "later_anxiety"))
  expect_identical(spec$data, er)
})

test_that("plot_spec prefers the plot's mapping over the layer's", {
  p <- ggplot2::ggplot(Fingers, ggplot2::aes(x = Thumb)) +
    ggplot2::geom_histogram(ggplot2::aes(x = Height), bins = 30)

  expect_equal(unname(plot_spec(p)$axes), "Thumb")
})

test_that("plot_spec falls back to the first layer's data", {
  p <- ggplot2::ggplot() +
    ggplot2::geom_histogram(data = Fingers, mapping = ggplot2::aes(x = Thumb), bins = 30)
  spec <- plot_spec(p)

  expect_identical(spec$data, Fingers)
  expect_equal(unname(spec$axes), "Thumb")
})

test_that("plot_spec reports no axes when nothing anywhere is mapped", {
  p <- ggplot2::ggplot(er) + ggplot2::geom_hex()

  expect_length(plot_spec(p)$axes, 0)
})

test_that("a pinned plot reports the reader's words and the drawn values", {
  # MUTATION: the label/quosure split written backwards in either direction --
  # labels[[a]] built from the pinned quosure prints `.coursekata_pin_y` at
  # readers; mapping$y built from the pin's original re-shuffles on every build
  set.seed(1)
  p <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers)
  q <- pin_plot_values(p)$plot
  spec <- plot_spec(q)

  expect_equal(spec$axes[["y"]], "shuffle(Thumb)")
  expect_equal(spec$labels[["y"]], "shuffle(Thumb)")
  expect_equal(rlang::as_label(spec$mapping$y), ".coursekata_pin_y")
  expect_equal(
    rlang::eval_tidy(spec$resolve_aes("y")$quo, spec$data),
    q$data$.coursekata_pin_y
  )
})

test_that("an unpinned plot reports exactly what it reported before", {
  # MUTATION: the pin machinery leaking into the common path -- pins reported
  # as non-empty, or labels/variables/axes computed differently, for a plot
  # that was never pinned in the first place
  p <- gf_point(Thumb ~ Height, data = Fingers)
  spec <- plot_spec(p)

  expect_equal(spec$pins, list())
  expect_equal(spec$labels, c(x = "Height", y = "Thumb"))
  expect_equal(spec$variables, c(x = "Height", y = "Thumb"))
  expect_equal(spec$axes, c(x = "Height", y = "Thumb"))
})
