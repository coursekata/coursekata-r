#' Resolve truthful values on a trained continuous position scale
#'
#' Position annotations must distinguish a value that the scale can draw from
#' one that an out-of-bounds handler moved to a boundary. This helper keeps the
#' transform, OOB comparison, and viewport check identical for geoms and
#' guides.
#'
#' @param scale_view A trained ggplot2 position-scale view.
#' @param value Numeric values in the scale's data space.
#'
#' @return A list containing `transformed`, `unchanged`, and `visible` vectors.
#' @noRd
position_anchor_key <- function(scale_view, value) {
  scale <- scale_view$scale
  transformation <- scale$get_transformation()
  transformed <- suppressWarnings(transformation$transform(value))
  limits <- scale$get_limits()
  oob <- suppressWarnings(scale$oob(transformed, range = limits))

  unchanged <- vapply(seq_along(value), function(i) {
    length(transformed) >= i && length(oob) >= i && isTRUE(all.equal(
      as.numeric(oob[[i]]), as.numeric(transformed[[i]]),
      check.attributes = FALSE
    ))
  }, logical(1))
  viewport <- range(scale_view$continuous_range, finite = TRUE)
  visible <- unchanged & length(viewport) == 2L & is.finite(transformed) &
    transformed >= viewport[[1L]] & transformed <= viewport[[2L]]

  list(
    transformed = as.numeric(transformed), unchanged = unchanged,
    visible = visible
  )
}

#' Whether an object can be drawn as one guide label
#'
#' @param x An object supplied as a label.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
is_position_guide_label <- function(x) {
  is.null(x) || (is.character(x) && length(x) == 1L && !is.na(x)) ||
    (is.expression(x) && length(x) == 1L) || is.language(x)
}

#' Put an anchored label's box inside a guide viewport
#'
#' @param at A trained position from zero to one.
#'
#' @return A justification value.
#' @noRd
position_anchor_justification <- function(at) {
  if (at <= 0.1) return(0)
  if (at >= 0.9) return(1)
  0.5
}

#' Draw a marker pointing from a position guide toward its panel
#'
#' @param at Trained position along the guide.
#' @param position Guide side.
#' @param shape Point shape, or a waiver for a directional triangle.
#' @param size,colour,linewidth Marker styling.
#'
#' @return A grid grob.
#' @noRd
position_anchor_grob <- function(at, position, shape, size, colour, linewidth) {
  if (!inherits(shape, "waiver")) {
    if (is.na(shape)) return(grid::nullGrob())
    x <- if (position %in% c("top", "bottom")) at else 0.5
    y <- if (position %in% c("left", "right")) at else 0.5
    return(grid::pointsGrob(
      x = grid::unit(x, "npc"), y = grid::unit(y, "npc"), pch = shape,
      size = grid::unit(size, "mm"),
      gp = grid::gpar(
        col = colour, fill = colour, lwd = linewidth * ggplot2::.pt
      )
    ))
  }

  geometry <- switch(position,
    top = list(
      x = c(0.5, 0, 1), y = c(0, 1, 1),
      vp = grid::viewport(
        x = grid::unit(at, "npc"), y = grid::unit(0.5, "npc"),
        width = grid::unit(size, "mm"), height = grid::unit(size, "mm"),
        just = c(0.5, 0.5)
      )
    ),
    bottom = list(
      x = c(0.5, 0, 1), y = c(1, 0, 0),
      vp = grid::viewport(
        x = grid::unit(at, "npc"), y = grid::unit(0.5, "npc"),
        width = grid::unit(size, "mm"), height = grid::unit(size, "mm"),
        just = c(0.5, 0.5)
      )
    ),
    left = list(
      x = c(1, 0, 0), y = c(0.5, 0, 1),
      vp = grid::viewport(
        x = grid::unit(0.5, "npc"), y = grid::unit(at, "npc"),
        width = grid::unit(size, "mm"), height = grid::unit(size, "mm"),
        just = c(0.5, 0.5)
      )
    ),
    right = list(
      x = c(0, 1, 1), y = c(0.5, 0, 1),
      vp = grid::viewport(
        x = grid::unit(0.5, "npc"), y = grid::unit(at, "npc"),
        width = grid::unit(size, "mm"), height = grid::unit(size, "mm"),
        just = c(0.5, 0.5)
      )
    )
  )
  grid::polygonGrob(
    x = geometry$x, y = geometry$y, default.units = "npc", vp = geometry$vp,
    gp = grid::gpar(
      col = colour, fill = colour, lwd = linewidth * ggplot2::.pt
    )
  )
}
