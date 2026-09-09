test_that("guide_dgp has the specified constructor and x/y availability", {
  expect_identical(
    names(formals(guide_dgp)),
    c("value", "role", "label", "equation", "colour", "shape", "size",
      "linewidth", "title", "theme", "order", "position")
  )
  guide <- guide_dgp()
  expect_s3_class(guide, "GuideDgp")
  expect_identical(guide$available_aes, c("x", "y"))
  expect_identical(guide$params$value, 0)
  expect_identical(guide$params$role, "population")

  expect_error(guide_dgp(value = c(0, 1)), "one finite number")
  expect_error(guide_dgp(value = Inf), "one finite number")
  expect_error(guide_dgp(role = "other"), "arg")
  expect_error(guide_dgp(label = c("a", "b")), "one label")
  expect_error(guide_dgp(size = -1), "non-negative")
})

test_that("DGP roles retain their distinct teaching meaning", {
  population <- guide_dgp(role = "population")
  estimate <- guide_dgp(role = "estimate")

  expect_identical(population$params$value, 0)
  expect_identical(estimate$params$value, 0)
  expect_identical(population$params$heading, "Population Parameter (DGP)")
  expect_identical(estimate$params$heading, "Parameter Estimate")
  expect_equal(population$params$label, expression(beta[1] == 0))
  expect_equal(estimate$params$label, expression(b[1] == 0))
  expect_equal(
    population$params$equation,
    expression(Y[i] == beta[0] + beta[1] * X[i] + epsilon[i])
  )
  expect_equal(
    estimate$params$equation,
    expression(Y[i] == b[0] + b[1] * X[i] + e[i])
  )
})

test_that("a direct DGP guide builds on x and y positions", {
  values <- data.frame(x = c(-2, -1, 1, 2), y = c(-4, -2, 2, 4))
  x_plot <- ggplot2::ggplot(values, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(guide = guide_dgp(value = 0))
  y_plot <- ggplot2::ggplot(values, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(guide = guide_dgp(value = 0))

  expect_s3_class(ggplot2::ggplotGrob(x_plot), "gtable")
  expect_s3_class(ggplot2::ggplotGrob(y_plot), "gtable")
  x_key <- ggplot2::get_guide_data(x_plot, "x")
  expect_equal(x_key$.value, 0)
  expect_equal(ggplot2::get_guide_data(y_plot, "y")$.value, 0)
  expect_true(all(
    c(".value", ".label", "role", "colour", "shape", "size", "linewidth") %in%
      names(x_key)
  ))
  expect_identical(x_key$role, "population")
  expect_identical(x_key$colour, "#E60000")
  expect_identical(x_key$size, 4)
  expect_identical(x_key$linewidth, 0.5)
})

test_that("guide_dgp linewidth controls the population axis decor", {
  guide <- guide_dgp(role = "population", linewidth = 3)
  params <- guide$params
  params$aes <- "x"
  params$position <- "bottom"
  theme <- ggplot2::theme_get() + ggplot2::theme(
    axis.line.x = ggplot2::element_line(colour = "navy", linewidth = 0.2)
  )

  elements <- guide$setup_elements(params, guide$elements, theme)

  expect_s3_class(elements$line, "element_line")
  expect_identical(elements$line$colour, "navy")
  expect_identical(elements$line$linewidth, 3)
})

test_that("DGP anchors are omitted instead of transformed to a false boundary", {
  values <- data.frame(x = 1:10, y = 1:10)
  direct <- function(scale) {
    p <- ggplot2::ggplot(values, ggplot2::aes(x, y)) +
      ggplot2::geom_point() + scale
    ggplot2::get_guide_data(p, "x")
  }

  censored <- direct(ggplot2::scale_x_continuous(
    limits = c(1, 10), guide = guide_dgp(value = 0)
  ))
  squished <- direct(ggplot2::scale_x_continuous(
    limits = c(1, 10), oob = scales::squish, guide = guide_dgp(value = 0)
  ))
  logged <- direct(ggplot2::scale_x_log10(guide = guide_dgp(value = 0)))

  expect_false(censored$.visible)
  expect_false(squished$.visible)
  expect_false(logged$.visible)
  expect_true(all(is.na(censored$x)))
  expect_true(all(is.na(squished$x)))
  expect_true(all(is.na(logged$x)))
  expect_equal(censored$.value, 0)
  expect_equal(squished$.value, 0)
  expect_equal(logged$.value, 0)
})

test_that("show_dgp does not change panel ranges or confuse zero with the mean", {
  values <- data.frame(x = c(-1, 2, 3, 4, 5))
  base <- ggplot2::ggplot(values, ggplot2::aes(x)) +
    ggplot2::geom_histogram(bins = 5) + ggplot2::scale_y_sqrt()
  out <- show_dgp(show_mean(base))
  built_base <- ggplot2::ggplot_build(base)
  built_out <- ggplot2::ggplot_build(out)

  expect_equal(
    built_out$layout$panel_params[[1]]$x.range,
    built_base$layout$panel_params[[1]]$x.range
  )
  expect_equal(
    built_out$layout$panel_params[[1]]$y.range,
    built_base$layout$panel_params[[1]]$y.range
  )
  expect_equal(ggplot2::layer_data(out, 2)$xintercept, mean(values$x))
  scale <- out$scales$get_scales("x")
  expect_equal(
    position_guide_matches(scale$guide, "GuideDgp", "estimate")[[1]]$params$value,
    0
  )
  expect_equal(
    position_guide_matches(scale$secondary.axis, "GuideDgp", "population")[[1]]$params$value,
    0
  )
})

test_that("show_dgp supports count-axis choices that guides do not own", {
  values <- data.frame(x = rep(c(-1, 1), c(40, 10)), g = rep(c("a", "b"), c(40, 10)))
  base <- ggplot2::ggplot(values, ggplot2::aes(x)) + ggplot2::geom_histogram(bins = 5)

  expect_no_error(ggplot2::ggplotGrob(show_dgp(
    base + ggplot2::scale_y_continuous(limits = c(0, 50))
  )))
  expect_no_error(ggplot2::ggplotGrob(show_dgp(
    base + ggplot2::coord_cartesian(ylim = c(0, 20))
  )))
  expect_no_error(ggplot2::ggplotGrob(show_dgp(
    base + ggplot2::scale_y_sqrt()
  )))
  expect_no_error(ggplot2::ggplotGrob(show_dgp(
    base + ggplot2::facet_wrap(~g, scales = "free_y")
  )))
})

test_that("show_dgp follows ggplot2 coordinate composition", {
  values <- data.frame(x = c(-2, -1, 1, 2))
  base <- ggplot2::ggplot(values, ggplot2::aes(x)) + ggplot2::geom_histogram()

  expect_error(show_dgp(show_dgp(base)), "already")
  expect_error(show_dgp(base + ggplot2::coord_flip()), "upright cartesian")
  expect_error(
    ggplot2::ggplotGrob(show_dgp(base) + ggplot2::coord_flip()),
    "upright cartesian"
  )

  # Polar coordinates do not train Cartesian position guides. If they are
  # added later, ggplot2 therefore drops the DGP guides as part of replacing
  # the coordinate system instead of asking GuideDgp to draw them.
  expect_no_error(ggplot2::ggplotGrob(show_dgp(base) + ggplot2::coord_polar()))
})
