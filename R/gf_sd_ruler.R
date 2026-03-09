#' Add a Standard Deviation Ruler to a Plot
#'
#' `r lifecycle::badge("experimental")`
#'
#' Adds a vertical segment showing one standard deviation of a variable,
#' placed at a specified x position. Works for both numeric x (scatter) and
#' categorical x (jitter) plots.
#'
#' @param p A ggplot object (typically from `gf_point()` or `gf_jitter()`).
#' @param y The y-variable (bare name or string). Defaults to the plot's
#'   mapped y aesthetic if omitted.
#' @param data Dataset. Defaults to `p$data`.
#' @param x The x-variable for placement. Defaults to the plot's mapped x.
#' @param where Where on the x-axis to place the ruler: `"middle"` (midpoint
#'   of x range), `"mean"`, or `"median"`.
#' @param color Segment color. Default `"red"`.
#' @param size Segment `linewidth`. Default `0.8`.
#' @param ... Additional arguments passed to [ggplot2::geom_segment()].
#'
#' @return A ggplot object with the SD ruler segment added.
#'
#' @export
#' @examples
#' gf_jitter(Thumb ~ Height, data = Fingers) %>%
#'   gf_model(lm(Thumb ~ NULL, data = Fingers)) %>%
#'   gf_sd_ruler()
gf_sd_ruler <- function(p, y = NULL, data = NULL, x = NULL,
                        where = c("middle", "mean", "median"),
                        color = "red", size = 0.8, ...) {
  lifecycle::signal_stage("experimental", "gf_sd_ruler()")
  where <- match.arg(where)
  if (is.null(data)) data <- p$data

  # infer y if not supplied
  if (is.null(y)) {
    if (is.null(p$mapping$y)) abort("Can't infer y; please pass y explicitly.")
    y_name <- as_name(p$mapping$y)
  } else {
    y_name <- if (is.character(y)) y else deparse(substitute(y))
  }

  y_vals <- data[[y_name]]
  m <- mean(y_vals, na.rm = TRUE)
  s <- stats::sd(y_vals, na.rm = TRUE)

  # infer x for placement
  if (is.null(x)) {
    if (!is.null(p$mapping$x)) {
      x_name <- as_name(p$mapping$x)
      x_vals_raw <- data[[x_name]]
    } else {
      x_vals_raw <- seq_along(y_vals)
    }
  } else {
    x_name <- if (is.character(x)) x else deparse(substitute(x))
    x_vals_raw <- data[[x_name]]
  }

  # turn categorical x into numeric positions
  x_vals <- x_vals_raw
  if (!is.numeric(x_vals)) {
    if (is.factor(x_vals)) {
      x_vals <- as.numeric(x_vals)
    } else {
      x_vals <- as.numeric(factor(x_vals, levels = unique(x_vals)))
    }
  }

  # compute placement
  x0 <- switch(where,
    middle = (min(x_vals, na.rm = TRUE) + max(x_vals, na.rm = TRUE)) / 2,
    mean   = mean(x_vals, na.rm = TRUE),
    median = stats::median(x_vals, na.rm = TRUE)
  )

  seg <- data.frame(x = x0, xend = x0, y = m, yend = m + s)

  p +
    ggplot2::geom_segment(
      data = seg,
      mapping = ggplot2::aes(
        x = .data$x, xend = .data$xend,
        y = .data$y, yend = .data$yend
      ),
      inherit.aes = FALSE,
      color = color,
      linewidth = size,
      ...
    )
}
