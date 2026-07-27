#' Add Residual Lines from a Function to a Plot
#'
#' `r lifecycle::badge("experimental")`
#'
#' Draws vertical residual lines from observed points to predicted values
#' computed by a user-supplied function of x (e.g., the function plotted with
#' `gf_function()`).
#'
#' @param plot A ggformula/ggplot object, typically created with `gf_point()`.
#' @param fun A function that takes a numeric vector x and returns predicted y.
#' @param linewidth Numeric width of the residual lines. Default `0.2`.
#' @param ... Additional aesthetics passed to [ggplot2::geom_segment()], e.g.,
#'   `color`, `alpha`, `linetype`.
#'
#' @return A ggplot object with residual segments added.
#'
#' @export
#' @examples
#' set.seed(1)
#' df <- data.frame(X = 1:10, Y = 2 + 3 * (1:10) + rnorm(10))
#' my_fun <- function(x) 2 + 3 * x
#'
#' gf_point(Y ~ X, data = df) %>%
#'   gf_function(my_fun) %>%
#'   gf_resid_fun(my_fun, color = "red", alpha = 0.5)
gf_resid_fun <- function(plot, fun, linewidth = 0.2, ...) {
  lifecycle::signal_stage("experimental", "gf_resid_fun()")

  plot <- freeze_jitter(plot)
  geometry <- plot_geometry(plot)
  plan <- resid_plan(geometry, fun(geometry$x))
  render_resid_plan(plot, plan, linewidth, ...)
}
