mean_layer_data <- function(plot) {
  ggplot2::layer_data(plot, length(plot$layers))
}

test_that("stat_dist_mean has the conventional fixed-x layer interface", {
  expect_identical(
    names(formals(stat_dist_mean)),
    c("mapping", "data", "geom", "position", "...", "na.rm",
      "show.legend", "inherit.aes")
  )
  expect_identical(formals(stat_dist_mean)$geom, "vline")
  expect_identical(formals(stat_dist_mean)$position, "identity")
  expect_identical(formals(stat_dist_mean)$na.rm, FALSE)
  expect_identical(formals(stat_dist_mean)$show.legend, NA)
  expect_identical(formals(stat_dist_mean)$inherit.aes, TRUE)

  values <- data.frame(value = c(1, 10, 100, 1000))
  layer <- stat_dist_mean(
    mapping = ggplot2::aes(x = value), data = values,
    colour = "purple", linewidth = 2, na.rm = TRUE,
    show.legend = FALSE, inherit.aes = FALSE
  )

  expect_s3_class(layer, "LayerInstance")
  expect_s3_class(layer$stat, "StatDistMean")
  expect_s3_class(layer$geom, "GeomVline")
  expect_s3_class(layer$position, "PositionIdentity")
  expect_identical(layer$stat_params$na.rm, TRUE)
  expect_identical(layer$aes_params$colour, "purple")
  expect_identical(layer$aes_params$linewidth, 2)
  expect_identical(layer$show.legend, FALSE)
  expect_identical(layer$inherit.aes, FALSE)

  built <- mean_layer_data(ggplot2::ggplot() + layer)
  expect_equal(built$xintercept, 277.75)
  expect_false(any(c("y", "ymin", "ymax") %in% names(built)))
})

test_that("StatDistMean preserves the position-scale and coordinate lifecycle", {
  values <- data.frame(x = c(1, 10, 100, 1000))
  base <- ggplot2::ggplot(values, ggplot2::aes(x = x)) +
    stat_dist_mean(na.rm = TRUE)

  expect_equal(mean_layer_data(base)$xintercept, 277.75)
  expect_equal(
    mean_layer_data(base + ggplot2::scale_x_log10())$xintercept,
    1.5
  )
  expect_equal(
    mean_layer_data(base + ggplot2::scale_x_continuous(limits = c(1, 100)))$xintercept,
    37
  )
  expect_equal(
    mean_layer_data(base + ggplot2::coord_cartesian(xlim = c(1, 100)))$xintercept,
    277.75
  )
})

test_that("StatDistMean computes from each panel even when facets are added later", {
  values <- data.frame(
    x = c(1, 10, 100, 1000),
    group = rep(c("low", "high"), each = 2)
  )
  before_faceting <- ggplot2::ggplot(values, ggplot2::aes(x = x)) +
    stat_dist_mean(na.rm = TRUE)
  faceted <- before_faceting + ggplot2::facet_wrap(~group, scales = "free_x")

  expect_equal(sort(mean_layer_data(faceted)$xintercept), c(5.5, 550))
})

test_that("StatDistMean handles missing values and mapped expressions", {
  values <- data.frame(x = c(1, exp(1), exp(2), NA_real_))
  plain <- ggplot2::ggplot(values, ggplot2::aes(x = x)) +
    stat_dist_mean(na.rm = TRUE)
  mapped <- ggplot2::ggplot(values, ggplot2::aes(x = log(x))) +
    stat_dist_mean(na.rm = TRUE)

  expect_equal(mean_layer_data(plain)$xintercept, mean(values$x, na.rm = TRUE))
  expect_equal(mean_layer_data(mapped)$xintercept, 1)
})

test_that("stat_dist_mean refuses mapped styling aesthetics", {
  values <- data.frame(x = 1:6, g = rep(c("a", "b"), each = 3))
  plot <- ggplot2::ggplot(values, ggplot2::aes(x, colour = g)) +
    stat_dist_mean(na.rm = TRUE)

  expect_error(ggplot2::ggplot_build(plot), "colour.*can't be mapped")
})

test_that("new_stat_dist_mean makes the internal x and y subclasses", {
  x_stat <- new_stat_dist_mean("x")
  y_stat <- new_stat_dist_mean("y")

  expect_s3_class(x_stat, "StatDistMean")
  expect_s3_class(y_stat, "StatDistMean")
  expect_identical(x_stat$required_aes, "x")
  expect_identical(y_stat$required_aes, "y")

  values <- data.frame(value = c(1, 3, 8))
  horizontal <- ggplot2::ggplot(values, ggplot2::aes(y = value)) +
    ggplot2::layer(
      stat = y_stat, geom = ggplot2::GeomHline, position = "identity",
      params = list(na.rm = TRUE)
    )

  built <- mean_layer_data(horizontal)
  expect_equal(built$yintercept, 4)
  expect_false("xintercept" %in% names(built))
})
