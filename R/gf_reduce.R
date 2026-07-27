#' Add Reduction-in-Error Lines to a Plot
#'
#' Draws a vertical line for each observation from the empty model's prediction
#' (the grand mean) to a complex model's prediction. The length of each line is
#' the reduction in error -- SS Model -- that the predictor achieves for that
#' observation. It is the counterpart to [gf_resid()]: where `gf_resid()` shows
#' the error that remains, `gf_reduce()` shows the error that the predictor
#' explains.
#'
#' @param plot A ggformula plot object, typically created with `gf_point()`.
#' @param model A fitted linear model object created using `lm()` (the complex
#'   model). The empty model it is compared against is always the grand mean.
#' @param linewidth A numeric value specifying the width of the lines. Default
#'   is `0.2`.
#' @param ... Additional aesthetics passed to `geom_segment()`, such as `color`,
#'   `alpha`, `linetype`.
#'
#' @return A ggplot object with reduction-in-error lines added.
#'
#' @details
#' The empty model is always the grand mean, so you only pass the complex
#' model. The overlay works best on a small sample (roughly 8-15 points) --
#' with many points the lines crowd together and the picture is hard to read.
#'
#' @seealso [gf_resid()] and [gf_square_resid()] for the error that remains;
#'   [gf_square_reduce()] to draw the same reduction as squares.
#'
#' @export
#' @examples
#' # A small sample keeps the reduction lines readable.
#' set.seed(1)
#' penguins_20 <- sample(penguins, 20)
#'
#' # The complex model uses flipper length to predict body mass; the empty
#' # model it is compared against is the grand mean of body mass.
#' empty_model <- lm(body_mass_kg ~ NULL, data = penguins_20)
#' complex_model <- lm(body_mass_kg ~ flipper_length_m, data = penguins_20)
#'
#' # Each vertical line runs from the grand mean (the flat empty-model line) to
#' # the regression line. Longer lines mean the predictor is doing more work for
#' # that penguin -- the ones with the most extreme flipper lengths move the
#' # most, so they get the longest lines.
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins_20) %>%
#'   gf_model(empty_model) %>%
#'   gf_model(complex_model) %>%
#'   gf_reduce(complex_model, color = "forestgreen")
#'
#' # With a categorical predictor, every member of a group gets the SAME-length
#' # line, because the model predicts that group's mean for all of them. Groups
#' # whose means sit far from the grand mean get the longest lines.
#' gentoo_model <- lm(body_mass_kg ~ gentoo, data = penguins_20)
#' gf_jitter(body_mass_kg ~ gentoo, data = penguins_20, width = .1) %>%
#'   gf_model(gentoo_model) %>%
#'   gf_reduce(gentoo_model, color = "forestgreen")
gf_reduce <- function(plot, model, linewidth = 0.2, ...) {
  # Pin the jitter so every build of this plot draws the same dot positions
  plot <- freeze_jitter(plot)

  y_fitted <- stats::fitted(model)
  y_empty <- mean(y_fitted) # grand mean = empty model prediction for every point

  # Access the x coordinates used in the plot
  x_loc <- ggplot2::ggplot_build(plot)$data[[1]]$x

  plot +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = x_loc,
        xend = x_loc,
        y = y_empty,
        yend = y_fitted
      ),
      inherit.aes = TRUE,
      linewidth = linewidth,
      ...
    )
}

#' Add Reduction-in-Error Squares to a Plot
#'
#' `r lifecycle::badge("experimental")`
#'
#' Draws a square for each observation whose side length equals the reduction in
#' error the predictor achieves -- the distance from the grand mean (the empty
#' model's prediction) to the complex model's prediction. The total area of all
#' squares is SS Model, the numerator of the proportional reduction in error
#' (PRE). Drawn alongside [gf_square_resid()], the squares decompose the total
#' variation: SS Total = SS Model + SS Error.
#'
#' @param plot A ggformula plot object, typically created with `gf_point()`.
#' @param model A fitted linear model object created using `lm()` (the complex
#'   model).
#' @param aspect A numeric value controlling the square's aspect ratio. Default
#'   is `4/6`.
#' @param alpha A numeric value specifying the transparency of the square's
#'   fill. Default is `0.1`.
#' @param linewidth A numeric value specifying the width of the square's border.
#'   Default is `0.25`.
#' @param ... Additional aesthetics passed to `geom_polygon()`, such as `color`
#'   and `fill`.
#'
#' @return A ggplot object with reduction-in-error squares added.
#'
#' @details
#' The total area of the squares is SS Model. Drawn together with the two
#' `gf_square_resid()` layers, the areas add up: SS Total (from the empty
#' model) = SS Error (from the complex model) + SS Model.
#'
#' A few practical notes:
#' * Use a small sample (roughly 8-15 points); with many observations the
#'   squares overlap and the decomposition is hard to see.
#' * Pass fixed colors (e.g. `fill = "green"`), not mapped aesthetics like
#'   `fill = ~species` -- the squares are built from an internal data frame that
#'   does not carry the original variables.
#' * On jitter plots the dot positions are pinned automatically, so the squares
#'   stay aligned with the dots across repeated builds and chained layers.
#'
#' @seealso [gf_square_resid()] for the squared error that remains;
#'   [gf_reduce()] to draw the reduction as lines instead of squares.
#'
#' @export
#' @examples
#' # A small sample keeps the squares readable.
#' set.seed(1)
#' penguins_20 <- sample(penguins, 20)
#' empty_model <- lm(body_mass_kg ~ NULL, data = penguins_20)
#' complex_model <- lm(body_mass_kg ~ flipper_length_m, data = penguins_20)
#'
#' # Each square's area is how much that penguin contributes to SS Model; the
#' # total green area is SS Model, the numerator of PRE.
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins_20) %>%
#'   gf_model(complex_model) %>%
#'   gf_square_reduce(complex_model, color = "forestgreen")
#'
#' # The full decomposition on one plot. The blue squares (SS Total, from the
#' # empty model) equal the red squares (SS Error) plus the green squares
#' # (SS Model). When the green area is large relative to the blue, PRE is high
#' # -- the predictor explains a lot of the variation.
#' gf_point(body_mass_kg ~ flipper_length_m, data = penguins_20) %>%
#'   gf_model(empty_model) %>%
#'   gf_model(complex_model) %>%
#'   gf_square_resid(empty_model, color = "blue") %>%
#'   gf_square_resid(complex_model, color = "red") %>%
#'   gf_square_reduce(complex_model, color = "forestgreen")
gf_square_reduce <- function(plot, model, aspect = 4 / 6, alpha = 0.1, linewidth = 0.25, ...) {
  lifecycle::signal_stage("experimental", "gf_square_reduce()")

  # Pin the jitter so every build of this plot draws the same dot positions
  plot <- freeze_jitter(plot)

  y_fitted <- stats::fitted(model)
  y_empty <- mean(y_fitted)

  # Access the x coordinates and the panel ranges used in the plot
  built <- ggplot2::ggplot_build(plot)
  x_loc <- built$data[[1]]$x
  panel_params <- built$layout$panel_params[[1]]
  x_range <- panel_params$x.range
  y_range <- panel_params$y.range

  # Compute ratio for proper aspect scaling, then square each reduction
  range_ratio <- (x_range[2] - x_range[1]) / (y_range[2] - y_range[1])
  dir <- ifelse(x_loc > mean(x_range), -1, 1)
  side_length <- abs(y_fitted - y_empty) * aspect * range_ratio
  x_opp <- x_loc + dir * side_length

  # Create a dataframe for plotting polygons
  squares_data <- do.call(rbind, lapply(seq_along(x_loc), function(i) {
    data.frame(
      x = c(x_loc[i], x_opp[i], x_opp[i], x_loc[i]),
      y = c(y_empty, y_empty, y_fitted[i], y_fitted[i]),
      id = i # Unique identifier for each square
    )
  }))

  plot +
    ggplot2::geom_polygon(
      data = squares_data,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$id),
      inherit.aes = FALSE,
      alpha = alpha,
      linewidth = linewidth,
      ...
    )
}

#' @rdname gf_square_reduce
#' @description
#' `gf_squareduce()` is a fully supported alias of `gf_square_reduce()`.
#' @export
gf_squareduce <- function(plot, model, aspect = 4 / 6, alpha = 0.1, linewidth = 0.25, ...) {
  gf_square_reduce(plot, model, aspect = aspect, alpha = alpha, linewidth = linewidth, ...)
}
