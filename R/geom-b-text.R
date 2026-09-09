#' Draw coefficient labels on semantic predictor and outcome sides
#'
#' `gf_b()` decides which side of a coefficient mark a label belongs on in
#' predictor/outcome coordinates. Position scales and coordinates can reverse
#' or exchange those axes after `gf_b()` has added its layers, so ordinary
#' `hjust`/`vjust` values cannot preserve that decision. `GeomBText` resolves
#' the two semantic justifications against the trained panel at draw time,
#' then delegates all text rendering to [ggplot2::GeomText].
#'
#' This is deliberately internal. It is placement machinery for the
#' `gf_b()`/`gf_coef()` front door, not another public geom vocabulary.
#'
#' @noRd
GeomBText <- ggplot2::ggproto(
  "GeomBText", ggplot2::GeomText,

  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE,
                        size.unit = "mm", x_just = 0.5, y_just = 0.5) {
    x_vector <- b_panel_axis_vector(panel_params, coord, "x")
    y_vector <- b_panel_axis_vector(panel_params, coord, "y")
    just <- b_physical_justification(x_just, y_just, x_vector, y_vector)
    data$hjust <- just[["hjust"]]
    data$vjust <- just[["vjust"]]

    grob <- ggplot2::GeomText$draw_panel(
      data, panel_params, coord,
      parse = parse, na.rm = na.rm, check_overlap = check_overlap,
      size.unit = size.unit
    )
    b_clamp_text_grob(grob)
  }
)

#' Keep a coefficient label inside its panel after text measurement
#'
#' Text width depends on the graphics device and its fonts. A justification
#' that clears the coefficient mark can therefore put the far edge of the
#' label outside the panel on one device even when another has room. Clamp the
#' text grob after `GeomText` has measured it, moving only the overflowing axis
#' and retaining the requested side whenever the panel can hold the label.
#'
#' `GeomBText` draws one label per layer, so `grobWidth()` and `grobHeight()`
#' are the bounds of that label rather than a combined multi-label grob.
#'
#' @noRd
b_clamp_text_grob <- function(grob, padding = grid::unit(0.1, "mm")) {
  width <- grid::grobWidth(grob)
  height <- grid::grobHeight(grob)
  grob$x <- grid::unit.pmax(
    padding + grob$hjust * width,
    grid::unit.pmin(
      grob$x,
      grid::unit(1, "npc") - padding - (1 - grob$hjust) * width
    )
  )
  grob$y <- grid::unit.pmax(
    padding + grob$vjust * height,
    grid::unit.pmin(
      grob$y,
      grid::unit(1, "npc") - padding - (1 - grob$vjust) * height
    )
  )
  grob
}

#' Find the trained view scale for an original position aesthetic
#'
#' Coordinate systems such as `coord_flip()` exchange `panel_params$x` and
#' `panel_params$y`. The scale's own aesthetic list still records whether it
#' trained the original x or y aesthetic, which avoids branching on coordinate
#' class names.
#'
#' @noRd
b_panel_view_scale <- function(panel_params, aesthetic) {
  views <- panel_params[intersect(c("x", "y"), names(panel_params))]
  for (view in views) {
    if (aesthetic %in% view$aesthetics) {
      return(view)
    }
  }
  NULL
}

#' Trained scale coordinates ordered by the original data direction
#'
#' A continuous ViewScale's limits have already been transformed. Ordering
#' those endpoints by their inverse values gives the transformed coordinates
#' for increasing original data even for a decreasing transformation such as
#' `scale_x_reverse()`. Discrete positions use their trained numeric order.
#'
#' @noRd
b_increasing_scale_endpoints <- function(view) {
  if (isTRUE(view$is_discrete())) {
    return(range(view$continuous_range))
  }
  transformed <- view$limits
  keep <- is.finite(transformed)
  transformed <- transformed[keep]
  if (length(transformed) < 2L) {
    return(range(view$continuous_range))
  }
  original <- tryCatch(
    view$get_transformation()$inverse(transformed),
    error = function(...) transformed
  )
  ordered <- transformed[order(as.numeric(original))]
  endpoints <- ordered[c(1L, length(ordered))]
  if (any(!is.finite(endpoints)) || diff(endpoints) == 0) {
    range(view$continuous_range)
  } else {
    endpoints
  }
}

#' Screen-space vector for increasing an original data axis
#'
#' A short probe is placed at the middle of the trained panel, after scale
#' transformation but before the coordinate transform. Its direction accounts
#' for a decreasing position transform. Passing both endpoints through the
#' coordinate object then captures flips and coordinate-level reversals too.
#'
#' @noRd
b_panel_axis_vector <- function(panel_params, coord, aesthetic) {
  x_view <- b_panel_view_scale(panel_params, "x")
  y_view <- b_panel_view_scale(panel_params, "y")
  if (is.null(x_view) || is.null(y_view)) {
    abort(
      "`gf_b()` coefficient labels require Cartesian coordinates.",
      class = "coursekata_gf_b_coord"
    )
  }
  ranges <- list(x = x_view$continuous_range, y = y_view$continuous_range)
  finite_range <- function(values) {
    values <- values[is.finite(values)]
    if (length(values) == 0L) c(0, 1) else range(values)
  }
  ranges <- lapply(ranges, finite_range)
  middle <- vapply(ranges, mean, numeric(1))
  view <- if (aesthetic == "x") x_view else y_view
  endpoints <- b_increasing_scale_endpoints(view)
  origin <- data.frame(x = middle[["x"]], y = middle[["y"]])
  probe <- origin
  origin[[aesthetic]] <- endpoints[[1L]]
  probe[[aesthetic]] <- endpoints[[2L]]

  origin <- coord$transform(origin, panel_params)
  probe <- coord$transform(probe, panel_params)
  c(x = probe$x[[1]] - origin$x[[1]], y = probe$y[[1]] - origin$y[[1]])
}

#' Convert original-axis justifications into physical text justifications
#'
#' Each original axis maps to exactly one physical axis for the Cartesian and
#' flipped coordinates supported by `gf_b()`. A reversed mapping mirrors the
#' justification around the glyph centre: for example, right-aligned on an
#' increasing axis becomes left-aligned on a decreasing one.
#'
#' @noRd
b_physical_justification <- function(x_just, y_just, x_vector, y_vector) {
  out <- c(hjust = 0.5, vjust = 0.5)
  for (entry in list(list(just = x_just, vector = x_vector),
                     list(just = y_just, vector = y_vector))) {
    vector <- entry$vector
    if (all(!is.finite(vector)) || max(abs(vector), na.rm = TRUE) == 0) {
      next
    }
    physical <- if (abs(vector[["x"]]) >= abs(vector[["y"]])) "hjust" else "vjust"
    component <- if (physical == "hjust") vector[["x"]] else vector[["y"]]
    out[[physical]] <- if (component >= 0) entry$just else 1 - entry$just
  }
  out
}
