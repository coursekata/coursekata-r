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

  plot <- freeze_jitter(plot)
  geometry <- plot_geometry(plot)
  plan <- square_resid_plan(geometry, fun(geometry$x), aspect)
  render_square_resid_plan(plot, plan, alpha, ...)
}
