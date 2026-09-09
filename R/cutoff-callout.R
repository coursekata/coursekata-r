.cutoff_callout_defaults <- list(
  fontface = "italic",
  lineheight = 0.9,
  reflow_clearance = 12,
  border_alpha = 0.28,
  fill_alpha = 0.88,
  border_width = 0.35
)

#' Defer cutoff callout layout until the panel size is known
#'
#' @param data Transformed labelled cutoff rows.
#' @param horizontal Whether the cutoff stems run horizontally.
#' @param height Stem height as a panel fraction.
#' @param label_size Text size in millimetres.
#' @param label_padding,label_radius Callout padding and corner radius in
#'   millimetres.
#' @param avoidance Transformed distribution occupancy profile.
#'
#' @return A `coursekata_cutoff_callouts` gTree.
#' @noRd
cutoff_callout_grob <- function(data, horizontal, height, label_size,
                                label_padding, label_radius,
                                avoidance = NULL) {
  grid::gTree(
    children = grid::gList(), data = data, horizontal = horizontal,
    height = height, label_size = label_size,
    label_padding = label_padding, label_radius = label_radius,
    avoidance = avoidance,
    cl = "coursekata_cutoff_callouts"
  )
}

#' Lay out cutoff callouts once the device and panel are known
#'
#' Measurement, layout, and drawing are deliberately separate. The measured
#' label dimensions are reused by both the solver and renderer, so a label is
#' never measured twice during a draw.
#'
#' @param x A `coursekata_cutoff_callouts` gTree.
#'
#' @return The gTree with measured callout children.
#' @noRd
#' @exportS3Method grid::makeContent
makeContent.coursekata_cutoff_callouts <- function(x) {
  measured <- measure_cutoff_callouts(
    x$data, label_size = x$label_size, label_padding = x$label_padding
  )
  layout <- solve_cutoff_callout_layout(
    measured$data, horizontal = x$horizontal, height = x$height,
    avoidance = x$avoidance, metrics = measured$metrics,
    panel_width = measured$panel_width,
    panel_height = measured$panel_height
  )
  children <- cutoff_callout_grobs(
    measured$data, metrics = measured$metrics,
    label_size = x$label_size, label_radius = x$label_radius,
    layout = layout
  )
  grid::setChildren(x, do.call(grid::gList, children))
}

cutoff_callout_metrics <- function(data, label_size, label_padding) {
  text_gp <- grid::gpar(
    fontsize = label_size * ggplot2::.pt,
    fontface = .cutoff_callout_defaults$fontface,
    lineheight = .cutoff_callout_defaults$lineheight
  )
  text <- lapply(data$label, grid::textGrob, gp = text_gp)
  width <- vapply(text, function(grob) {
    grid::convertWidth(
      grid::grobWidth(grob) + grid::unit(2 * label_padding, "mm"),
      "mm", valueOnly = TRUE
    )
  }, numeric(1))
  height <- vapply(text, function(grob) {
    grid::convertHeight(
      grid::grobHeight(grob) + grid::unit(2 * label_padding, "mm"),
      "mm", valueOnly = TRUE
    )
  }, numeric(1))
  list(width = width, height = height)
}

measure_cutoff_callouts <- function(data, label_size, label_padding) {
  panel_width <- grid::convertWidth(
    grid::unit(1, "npc"), "mm", valueOnly = TRUE
  )
  panel_height <- grid::convertHeight(
    grid::unit(1, "npc"), "mm", valueOnly = TRUE
  )
  metrics <- cutoff_callout_metrics(data, label_size, label_padding)
  needs_reflow <- metrics$width >
    panel_width - .cutoff_callout_defaults$reflow_clearance

  if (any(needs_reflow)) {
    data$label[needs_reflow] <- sub(
      "\nvalues ", "\nvalues\n", data$label[needs_reflow], fixed = TRUE
    )
    reflowed <- cutoff_callout_metrics(
      data[needs_reflow, , drop = FALSE], label_size, label_padding
    )
    metrics$width[needs_reflow] <- reflowed$width
    metrics$height[needs_reflow] <- reflowed$height
  }

  list(
    data = data, metrics = metrics,
    panel_width = panel_width, panel_height = panel_height
  )
}

cutoff_callout_grobs <- function(data, metrics, label_size, label_radius,
                                 layout) {
  leaders <- boxes <- texts <- vector("list", nrow(data))

  for (i in seq_len(nrow(data))) {
    row <- data[i, , drop = FALSE]
    rectangle <- layout$boxes[[i]]
    route <- layout$routes[[i]]
    colour <- ggplot2::alpha(row$colour, row$alpha)
    fill <- if ("fill" %in% names(row)) row$fill else NA_character_
    text_gp <- grid::gpar(
      col = colour, fontsize = label_size * ggplot2::.pt,
      fontface = .cutoff_callout_defaults$fontface,
      lineheight = .cutoff_callout_defaults$lineheight
    )
    box_gp <- grid::gpar(
      col = ggplot2::alpha(
        row$colour, .cutoff_callout_defaults$border_alpha
      ),
      fill = ggplot2::alpha(fill, .cutoff_callout_defaults$fill_alpha),
      lwd = .cutoff_callout_defaults$border_width * ggplot2::.pt
    )

    texts[[i]] <- grid::textGrob(
      row$label, x = grid::unit(rectangle$x, "mm"),
      y = grid::unit(rectangle$y, "mm"), just = "centre", gp = text_gp
    )
    boxes[[i]] <- grid::roundrectGrob(
      x = grid::unit(rectangle$x, "mm"),
      y = grid::unit(rectangle$y, "mm"), just = "centre",
      width = grid::unit(metrics$width[[i]], "mm"),
      height = grid::unit(metrics$height[[i]], "mm"),
      r = grid::unit(label_radius, "mm"), gp = box_gp
    )
    angle <- layout$attachment_angles[[i]]
    route_x <- grid::unit(route[-nrow(route), 1L], "mm")
    route_y <- grid::unit(route[-nrow(route), 2L], "mm")
    leaders[[i]] <- grid::polylineGrob(
      x = grid::unit.c(route_x, grid::grobX(boxes[[i]], angle)),
      y = grid::unit.c(route_y, grid::grobY(boxes[[i]], angle)),
      gp = grid::gpar(
        col = colour, lty = row$linetype,
        lwd = row$linewidth * ggplot2::.pt, lineend = "butt",
        linejoin = "mitre"
      )
    )
  }
  c(leaders, boxes, texts)
}
