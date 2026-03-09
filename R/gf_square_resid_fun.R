#' Add Squared Residual Visualization from a Function to a Plot
#'
#' `r lifecycle::badge("experimental")`
#'
#' Draws squared residual polygons between observed points and predicted values
#' computed by a user-supplied function of x.
#'
#' @param plot A ggformula/ggplot object, typically created with `gf_point()`.
#' @param fun A function that takes a numeric vector x and returns predicted y.
#' @param aspect A numeric value controlling the square's aspect ratio.
#'   Default is `4/6`.
#' @param alpha Transparency of the filled squares. Default `0.1`.
#' @param ... Additional aesthetics passed to [ggplot2::geom_polygon()], e.g.,
#'   `color`, `fill`, `linetype`.
#'
#' @return A ggplot object with squared residual polygons added.
#'
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(X = 1:10, Y = 2 + 3 * (1:10) + rnorm(10))
#' my_fun <- function(x) 2 + 3 * x
#'
#' gf_point(Y ~ X, data = df) %>%
#'   gf_function(my_fun) %>%
#'   gf_square_resid_fun(my_fun, color = "red", alpha = 0.3)
gf_square_resid_fun <- function(plot, fun, aspect = 4 / 6, alpha = 0.1, ...) {
  lifecycle::signal_stage("experimental", "gf_square_resid_fun()")

  # Handles random jitter
  rand_int <- sample(1:100, 1)
  set.seed(rand_int)

  # Access the x and y coordinates used in the plot
  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]
  x_loc <- plot_data$x
  y_loc <- plot_data$y

  # Compute predicted values and residuals
  set.seed(rand_int)
  y_hat <- fun(x_loc)
  residual <- y_loc - y_hat

  # Access the range of x and y used in the panel
  plot_layout <- ggplot2::ggplot_build(plot)$layout
  panel_params <- plot_layout$panel_params[[1]]
  x_range <- panel_params$x.range
  y_range <- panel_params$y.range

  # Compute ratio for proper aspect scaling
  range_ratio <- (x_range[2] - x_range[1]) / (y_range[2] - y_range[1])
  dir <- ifelse(x_loc > mean(x_range), -1, 1)
  adj_side <- x_loc + dir * abs(residual * aspect * range_ratio)

  # Build polygons for each residual square
  squares_data <- do.call(rbind, lapply(seq_along(x_loc), function(i) {
    data.frame(
      x = c(x_loc[i], adj_side[i], adj_side[i], x_loc[i]),
      y = c(y_loc[i], y_loc[i], y_hat[i], y_hat[i]),
      id = i
    )
  }))

  # Add polygons
  set.seed(rand_int)
  plot +
    ggplot2::geom_polygon(
      data = squares_data,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$id),
      inherit.aes = FALSE,
      alpha = alpha,
      ...
    )
}
