#' Add the group model (to a point plot)
#'
#' When teaching about regression and group models (regression with a factor as an explanatory
#' variable), it can be useful to visualize the data as a point plot with the outcome on the y-axis
#' and the explanatory variable on the x-axis. It can take a little work to overlay the mean lines
#' for the model fit on this plot though --- using something like `gf_hline()` will put lines all
#' the way across the plot instead of just through their respective groups. This is where
#' `add_group_model()` comes in: pass it the same model and data as the `gf_point()` or
#' `gf_jitter()` plot and it will add the mean lines in the correct locations.
#'
#' @param object When chaining, this holds an object produced in the earlier portions of the chain.
#'   Most users can safely ignore this argument. See details and examples.
#' @param formula A formula with shape y ~ x.
#' @param data A data frame with the variables to be plotted.
#' @param width The width of the mean line(s) to be plotted. Note that factors are plotted 1 unit
#'   apart, so values larger than 1 will overlap into other groups.
#' @param ... Additional arguments passed to `gf_segment()` like `alpha`, `size`, `color`, etc.
#'
#' @return A gg object
#' @export
#'
#' @examples
#' gf_jitter(Thumb ~ Sex, data = Fingers) %>%
#'   add_group_model(Thumb ~ Sex, data = Fingers, size = 2)
add_group_model <- function(object, formula, data, width = .3, ...) {
  five_num <- mosaic::favstats(formula, data = data)
  five_num$x_min <- seq_along(five_num$mean) - width
  five_num$x_max <- seq_along(five_num$mean) + width
  gf_segment(object, mean + mean ~ x_min + x_max, data = five_num, ...)
}
