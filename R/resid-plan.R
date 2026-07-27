#' Refuse fitted values that are not row-aligned with the plotted points
#'
#' @param geometry A [plot_geometry()] list.
#' @param fitted Fitted values, one per plotted point.
#'
#' @return `fitted`, invisibly, when the two line up.
#'
#' @noRd
check_alignment <- function(geometry, fitted, call = caller_env()) {
  if (length(fitted) == length(geometry$x)) {
    return(invisible(fitted))
  }

  abort(
    c(
      "The model and the plot have to cover the same observations",
      glue("the plot draws {length(geometry$x)} points"),
      glue("the model gives {length(fitted)} fitted values"),
      paste0(
        "`lm()` drops rows with missing values, so drop them from the data before plotting ",
        "and fit the model on that same data"
      )
    ),
    call = call
  )
}

#' Where the residual segments go
#'
#' @param geometry A [plot_geometry()] list.
#' @param fitted Fitted values, one per plotted point.
#'
#' @return A data frame with `x`, `y`, `xend`, `yend`.
#'
#' @noRd
resid_plan <- function(geometry, fitted, call = caller_env()) {
  check_alignment(geometry, fitted, call = call)
  data.frame(x = geometry$x, y = fitted, xend = geometry$x, yend = geometry$y)
}

#' Draw a resid_plan() as a tagged segment layer
#'
#' @param plot A ggplot object.
#' @param plan A [resid_plan()] data frame.
#' @param linewidth Width of the drawn segments.
#' @param ... Additional aesthetics passed to [ggplot2::geom_segment()].
#'
#' @return The plot, with a tagged segment layer added.
#'
#' @noRd
render_resid_plan <- function(plot, plan, linewidth, ...) {
  # plan is row-aligned with plot$data, so overlay its geometry onto a copy of
  # the plot's own data and let inherit.aes = TRUE pull in mapped aesthetics.
  segment_data <- plot$data
  segment_data[c("x", "y", "xend", "yend")] <- plan[c("x", "y", "xend", "yend")]

  plot + tag_layer(
    ggplot2::geom_segment(
      data = segment_data,
      mapping = ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      inherit.aes = TRUE,
      linewidth = linewidth,
      ...
    ),
    "resid"
  )
}

#' Where the squared-residual polygons go
#'
#' The squares are squares on the page, not in data units: the side drawn
#' along x is scaled by both `aspect` and the panel's x:y range ratio. They
#' extend away from the nearer edge so they stay inside the plot.
#'
#' @param geometry A [plot_geometry()] list.
#' @param fitted Fitted values, one per plotted point.
#' @param aspect Multiplier on the drawn side length.
#'
#' @return A data frame with `x`, `y`, `id`, four rows per point.
#'
#' @noRd
square_resid_plan <- function(geometry, fitted, aspect, call = caller_env()) {
  check_alignment(geometry, fitted, call = call)

  residual <- geometry$y - fitted
  range_ratio <- diff(geometry$x_range) / diff(geometry$y_range)
  direction <- ifelse(geometry$x > mean(geometry$x_range), -1, 1)
  opposite <- geometry$x + direction * abs(residual * aspect * range_ratio)

  do.call(rbind, lapply(seq_along(geometry$x), function(i) {
    data.frame(
      x = c(geometry$x[i], opposite[i], opposite[i], geometry$x[i]),
      y = c(geometry$y[i], geometry$y[i], fitted[i], fitted[i]),
      id = i
    )
  }))
}

#' Draw a square_resid_plan() as a tagged polygon layer
#'
#' @param plot A ggplot object.
#' @param plan A [square_resid_plan()] data frame.
#' @param alpha Transparency of the filled squares.
#' @param ... Additional aesthetics passed to [ggplot2::geom_polygon()].
#'
#' @return The plot, with a tagged polygon layer added.
#'
#' @noRd
render_square_resid_plan <- function(plot, plan, alpha, ...) {
  plot + tag_layer(
    ggplot2::geom_polygon(
      data = plan,
      mapping = ggplot2::aes(x = .data$x, y = .data$y, group = .data$id),
      inherit.aes = FALSE,
      alpha = alpha,
      ...
    ),
    "square_resid"
  )
}
