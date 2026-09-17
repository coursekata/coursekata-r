sd_ruler_data <- function(plot) {
  ggplot2::layer_data(plot, length(plot$layers))
}

test_that("stat_sd_ruler has the conventional layer interface", {
  expect_identical(
    names(formals(stat_sd_ruler)),
    c("mapping", "data", "geom", "position", "...", "where", "na.rm",
      "show.legend", "inherit.aes")
  )
  expect_identical(formals(stat_sd_ruler)$geom, "segment")
  expect_identical(formals(stat_sd_ruler)$position, "identity")
  expect_identical(formals(stat_sd_ruler)$where, "middle")
  expect_identical(formals(stat_sd_ruler)$na.rm, FALSE)
  expect_identical(formals(stat_sd_ruler)$show.legend, NA)
  expect_identical(formals(stat_sd_ruler)$inherit.aes, TRUE)

  layer <- stat_sd_ruler(
    mapping = ggplot2::aes(x = Height, y = Thumb), data = Fingers,
    where = "mean", colour = "purple", linewidth = 2, na.rm = TRUE,
    show.legend = FALSE, inherit.aes = FALSE
  )

  expect_s3_class(layer, "LayerInstance")
  expect_s3_class(layer$stat, "StatSdRuler")
  expect_s3_class(layer$geom, "GeomSegment")
  expect_s3_class(layer$position, "PositionIdentity")
  expect_identical(layer$stat_params$where, "mean")
  expect_identical(layer$stat_params$na.rm, TRUE)
  expect_identical(layer$aes_params$colour, "purple")
  expect_identical(layer$aes_params$linewidth, 2)
  expect_identical(layer$show.legend, FALSE)
  expect_identical(layer$inherit.aes, FALSE)

  built <- sd_ruler_data(ggplot2::ggplot() + layer)
  expect_equal(built$x, mean(Fingers$Height))
  expect_equal(built$y, mean(Fingers$Thumb))

  forwarded <- stat_sd_ruler(
    geom = "point", position = ggplot2::position_nudge(x = 1)
  )
  expect_s3_class(forwarded$geom, "GeomPoint")
  expect_s3_class(forwarded$position, "PositionNudge")
})

test_that("stat_sd_ruler draws vertical and horizontal rulers", {
  vertical <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
    stat_sd_ruler(where = "mean", na.rm = TRUE)
  vertical_data <- sd_ruler_data(vertical)

  expect_equal(vertical_data$x, mean(Fingers$Height))
  expect_equal(vertical_data$xend, mean(Fingers$Height))
  expect_equal(vertical_data$y, mean(Fingers$Thumb))
  expect_equal(vertical_data$yend, mean(Fingers$Thumb) + sd(Fingers$Thumb))

  horizontal <- ggplot2::ggplot(Fingers, ggplot2::aes(Thumb)) +
    stat_sd_ruler(na.rm = TRUE)
  horizontal_data <- sd_ruler_data(horizontal)

  expect_equal(horizontal_data$x, mean(Fingers$Thumb))
  expect_equal(horizontal_data$xend, mean(Fingers$Thumb) + sd(Fingers$Thumb))
  expect_equal(horizontal_data$y, 0)
  expect_equal(horizontal_data$yend, 0)
})

test_that("stat_sd_ruler computes one ruler from each panel", {
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
    stat_sd_ruler(where = "median", na.rm = TRUE) +
    ggplot2::facet_wrap(~Sex)
  built <- sd_ruler_data(plot)

  expected_y <- as.numeric(tapply(Fingers$Thumb, Fingers$Sex, mean))
  expected_x <- as.numeric(tapply(Fingers$Height, Fingers$Sex, stats::median))
  expect_equal(built$y, expected_y)
  expect_equal(built$x, expected_x)
})

test_that("stat_sd_ruler measures values in the panel's drawn space", {
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
    stat_sd_ruler(where = "mean", na.rm = TRUE) +
    ggplot2::scale_y_log10()
  built <- sd_ruler_data(plot)

  expect_equal(built$y, mean(log10(Fingers$Thumb)))
  expect_equal(built$yend, mean(log10(Fingers$Thumb)) + sd(log10(Fingers$Thumb)))

  mapped <- ggplot2::ggplot(Fingers, ggplot2::aes(log(Thumb))) +
    stat_sd_ruler(na.rm = TRUE)
  mapped_data <- sd_ruler_data(mapped)
  expect_equal(mapped_data$x, mean(log(Fingers$Thumb)))
  expect_equal(mapped_data$xend, mean(log(Fingers$Thumb)) + sd(log(Fingers$Thumb)))

  values <- data.frame(x = 10^(0:3), y = 1:4)
  transformed_x <- ggplot2::ggplot(values, ggplot2::aes(x, y)) +
    stat_sd_ruler(where = "mean", na.rm = TRUE) +
    ggplot2::scale_x_log10()
  transformed_x_data <- sd_ruler_data(transformed_x)
  expect_equal(transformed_x_data$x, mean(log10(values$x)))
  expect_equal(transformed_x_data$xend, mean(log10(values$x)))
})

test_that("stat_sd_ruler removes incomplete x-y observations according to na.rm", {
  values <- data.frame(x = c(1, 2, 100), y = c(1, 3, NA_real_))
  base <- ggplot2::ggplot(values, ggplot2::aes(x, y))

  expect_warning(
    warned <- sd_ruler_data(base + stat_sd_ruler(where = "mean")),
    "Removed 1 row"
  )
  expect_no_warning(
    silent <- sd_ruler_data(base + stat_sd_ruler(where = "mean", na.rm = TRUE))
  )

  expected <- data.frame(x = 1.5, xend = 1.5, y = 2, yend = 2 + sqrt(2))
  expect_equal(warned[names(expected)], expected)
  expect_equal(silent[names(expected)], expected)
})

test_that("stat_sd_ruler refuses mapped styling aesthetics", {
  values <- data.frame(x = 1:6, y = c(1:3, 10:12), g = rep(c("a", "b"), each = 3))

  ruler <- ggplot2::ggplot(values, ggplot2::aes(x, y, colour = g)) +
    stat_sd_ruler(na.rm = TRUE)

  expect_error(ggplot2::ggplot_build(ruler), "colour.*can't be mapped")
})

test_that("the ggplot2 and ggformula front doors use the same ruler stat", {
  native <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
    stat_sd_ruler(where = "median", na.rm = TRUE) +
    ggplot2::facet_wrap(~Sex)
  formula <- suppressMessages(
    gf_point(Thumb ~ Height | Sex, data = Fingers) %>%
      gf_sd_ruler(where = "median")
  )

  columns <- c("PANEL", "x", "xend", "y", "yend")
  expect_equal(sd_ruler_data(native)[columns], sd_ruler_data(formula)[columns])
  expect_s3_class(formula$layers[[layer_index(formula, "sd_ruler")]]$stat,
                  "StatSdRuler")
})

test_that("stat_sd_ruler refuses an unknown placement rule", {
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
    stat_sd_ruler(where = "moddle", na.rm = TRUE)

  expect_error(ggplot2::ggplot_build(plot), "where")
})
