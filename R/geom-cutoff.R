#' Draw short cutoff stems inside a panel
#'
#' `GeomCutoff` draws a cutoff at each supplied `xintercept`. The statistical
#' position is transformed by the active coordinate system, while `height` is
#' measured as a fraction of the panel. The result is a short stem that starts
#' at the distribution baseline and follows [ggplot2::coord_flip()] without
#' changing the orthogonal scale. Optional `label`, `side`, and `call_id`
#' aesthetics let a teaching helper add measured callouts without training the
#' count axis.
#'
#' `geom_cutoff()` is the conventional layer constructor. It does not compute
#' cutoffs. [show_cutoffs()] supplies the optional marker and callout metadata
#' after it computes one whole-distribution plan.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use. Defaults to
#'   `"identity"`.
#' @param position A position adjustment. Defaults to `"identity"`.
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @param height Stem height as a fraction of the panel, from `0` to `1`.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in legends?
#' @param inherit.aes If `FALSE`, override the default aesthetics rather than
#'   combining with them.
#'
#' @return `geom_cutoff()` returns a ggplot2 layer.
#'
#' @format `GeomCutoff` is a [ggplot2::Geom] object.
#'
#' @export
GeomCutoff <- ggplot2::ggproto(
  "GeomCutoff", ggplot2::GeomSegment,
  required_aes = "xintercept",
  optional_aes = c(
    ".value", ".coursekata_protect", "label", "side", "call_id"
  ),
  default_aes = {
    defaults <- ggplot2::GeomSegment$default_aes
    defaults$linetype <- "dashed"
    defaults$fill <- ggplot2::GeomLabel$default_aes$fill
    defaults
  },
  extra_params = c(
    "na.rm", "height", "marker", "marker_size", "label_size",
    "label_padding", "label_radius", ".draw_callouts"
  ),
  setup_params = function(data, params) {
    height <- params$height %||% 0.2
    if (!is.numeric(height) || length(height) != 1L ||
        !is.finite(height) || height < 0 || height > 1) {
      abort("`geom_cutoff()`'s `height` must be one number from 0 to 1")
    }
    params$height <- height

    marker <- params$marker %||% FALSE
    if (!is.logical(marker) || length(marker) != 1L || is.na(marker)) {
      abort("`geom_cutoff()`'s `marker` must be `TRUE` or `FALSE`")
    }
    params$marker <- marker

    numeric_params <- list(
      marker_size = params$marker_size %||% 4,
      label_size = params$label_size %||% 3.2,
      label_padding = params$label_padding %||% 1.6,
      label_radius = params$label_radius %||% 0.8
    )
    for (name in names(numeric_params)) {
      value <- numeric_params[[name]]
      if (!is.numeric(value) || length(value) != 1L ||
          !is.finite(value) || value < 0) {
        abort(glue("`geom_cutoff()`'s `{name}` must be one non-negative number"))
      }
      params[[name]] <- value
    }
    params
  },
  draw_panel = function(data, panel_params, coord, height = 0.2,
                        marker = FALSE, marker_size = 4, label_size = 3.2,
                        label_padding = 1.6, label_radius = 0.8,
                        .draw_callouts = TRUE, lineend = "butt",
                        na.rm = FALSE) {
    panel <- cutoff_panel_data(data, panel_params, coord)
    if (is.null(panel)) return(ggplot2::zeroGrob())
    boundary <- panel$boundary
    dx <- panel$dx
    dy <- panel$dy
    horizontal <- panel$horizontal

    if (horizontal) {
      x0 <- grid::unit(boundary$x, "npc")
      x1 <- grid::unit(
        boundary$x + sign(dx) * pmin(height, abs(dx)), "npc"
      )
      y0 <- y1 <- grid::unit(boundary$y, "npc")
    } else {
      x0 <- x1 <- grid::unit(boundary$x, "npc")
      y0 <- grid::unit(boundary$y, "npc")
      y1 <- grid::unit(
        boundary$y + sign(dy) * pmin(height, abs(dy)), "npc"
      )
    }

    stems <- grid::segmentsGrob(
      x0 = x0, y0 = y0, x1 = x1, y1 = y1,
      gp = ggplot2::gg_par(
        col = ggplot2::alpha(boundary$colour, boundary$alpha),
        lwd = boundary$linewidth, lty = boundary$linetype,
        lineend = lineend
      )
    )

    labelled <- "label" %in% names(boundary) & !is.na(boundary$label)
    if (!isTRUE(marker) && !any(labelled)) return(stems)

    children <- list(stems)
    if (isTRUE(marker)) {
      children <- c(children, cutoff_marker_grobs(
        boundary, horizontal = horizontal, marker_size = marker_size
      ))
    }

    if (isTRUE(.draw_callouts) && any(labelled)) {
      children <- c(children, list(cutoff_callout_grob(
        boundary[labelled, , drop = FALSE],
        horizontal = horizontal, height = height,
        label_size = label_size, label_padding = label_padding,
        label_radius = label_radius
      )))
    }

    do.call(grid::grobTree, children)
  }
)

#' Transform cutoff anchors without training the orthogonal scale
#'
#' Both the public stem geom and the private, plot-wide callout coordinator use
#' this path. Keeping the transform in one place makes their anchors identical
#' under scale transforms, reversed scales, guide moves, and coordinate flips.
#'
#' @param data Layer data containing `xintercept`.
#' @param panel_params,coord ggplot2 panel and coordinate objects.
#'
#' @return A transformed panel description, or `NULL` when no truthful anchor
#'   remains visible.
#' @noRd
cutoff_panel_data <- function(data, panel_params, coord) {
  position_view <- NULL
  if (".coursekata_protect" %in% names(data) && ".value" %in% names(data)) {
    protected <- !is.na(data$.coursekata_protect) & data$.coursekata_protect
    position_views <- panel_params[intersect(c("x", "y"), names(panel_params))]
    carries_intercept <- vapply(position_views, function(view) {
      !is.null(view$scale) && "xintercept" %in% view$scale$aesthetics
    }, logical(1))
    matches <- which(carries_intercept)
    if (length(matches) != 1L) return(NULL)

    position_view <- position_views[[matches[[1L]]]]
    key <- position_anchor_key(position_view, data$.value)
    keep_protected <- protected & key$visible
    data$xintercept[keep_protected] <- key$transformed[keep_protected]
    data <- data[!protected | keep_protected, , drop = FALSE]
  }
  if (nrow(data) == 0L) return(NULL)

  # `I()` keeps high-level anchors out of scale training. Once the truthful
  # transformed value has survived the OOB check, remove that marker so the
  # coordinate system can rescale it into panel space for drawing.
  data$xintercept <- as.numeric(data$xintercept)

  boundary <- opposite <- data
  boundary$x <- opposite$x <- data$xintercept
  guide_position <- position_view$position %||% "bottom"
  if (guide_position %in% c("top", "right")) {
    boundary$y <- Inf
    opposite$y <- -Inf
  } else {
    # Distribution geoms sit on a zero baseline inside the scale expansion.
    # Anchoring there reproduces the ordinary numeric axis while leaving its
    # native ticks and labels untouched. If a transformed count scale cannot
    # represent zero, the coordinate system will drop back to the panel edge.
    boundary$y <- 0
    opposite$y <- Inf
  }
  boundary <- coord$transform(boundary, panel_params)
  opposite <- coord$transform(opposite, panel_params)

  invalid_boundary <- !is.finite(boundary$x) | !is.finite(boundary$y)
  if (any(invalid_boundary)) {
    fallback <- data
    fallback$x <- data$xintercept
    fallback$y <- -Inf
    fallback <- coord$transform(fallback, panel_params)
    boundary$x[invalid_boundary] <- fallback$x[invalid_boundary]
    boundary$y[invalid_boundary] <- fallback$y[invalid_boundary]
  }

  dx <- opposite$x - boundary$x
  dy <- opposite$y - boundary$y
  horizontal <- sum(abs(dx), na.rm = TRUE) > sum(abs(dy), na.rm = TRUE)
  boundary$.screen <- if (horizontal) boundary$y else boundary$x
  boundary$.opposite_x <- opposite$x
  boundary$.opposite_y <- opposite$y

  list(boundary = boundary, dx = dx, dy = dy, horizontal = horizontal)
}

# One private layer owns all high-level labels in a panel. Individual
# `GeomCutoff` layers continue to own their stems and markers, so ggplot2 layer
# identity and composition remain conventional while layout gets the complete
# set of boxes and connectors it needs.
GeomCutoffCallout <- ggplot2::ggproto(
  "GeomCutoffCallout", ggplot2::Geom,
  required_aes = "xintercept",
  optional_aes = c(
    ".value", ".coursekata_protect", "label", "side", "call_id"
  ),
  default_aes = GeomCutoff$default_aes,
  extra_params = c(
    "na.rm", "height", "label_size", "label_padding", "label_radius",
    "avoidance"
  ),
  setup_params = function(data, params) {
    params$marker <- FALSE
    GeomCutoff$setup_params(data, params)
  },
  draw_panel = function(data, panel_params, coord, height = 0.2,
                        label_size = 3.2, label_padding = 1.6,
                        label_radius = 0.8, marker = FALSE,
                        marker_size = 4, avoidance = NULL,
                        na.rm = FALSE) {
    panel <- cutoff_panel_data(data, panel_params, coord)
    if (is.null(panel)) return(ggplot2::zeroGrob())
    labelled <- "label" %in% names(panel$boundary) &
      !is.na(panel$boundary$label)
    if (!any(labelled)) return(ggplot2::zeroGrob())

    cutoff_callout_grob(
      panel$boundary[labelled, , drop = FALSE],
      horizontal = panel$horizontal, height = height,
      label_size = label_size, label_padding = label_padding,
      label_radius = label_radius,
      avoidance = cutoff_avoidance_panel(avoidance, panel_params, coord)
    )
  },
  draw_key = ggplot2::draw_key_blank
)

geom_cutoff_callouts <- function(data, avoidance = NULL) {
  mapping <- ggplot2::aes(
    xintercept = .data$xintercept,
    .value = .data$.value,
    .coursekata_protect = .data$.coursekata_protect,
    label = .data$label,
    side = .data$side,
    call_id = .data$call_id,
    colour = .data$.colour,
    fill = .data$.fill,
    linetype = .data$.linetype,
    linewidth = .data$.linewidth
  )
  ggplot2::layer(
    geom = GeomCutoffCallout, mapping = mapping, data = data,
    stat = "identity", position = "identity", show.legend = FALSE,
    inherit.aes = FALSE, params = list(na.rm = TRUE, avoidance = avoidance)
  )
}

cutoff_avoidance_panel <- function(avoidance, panel_params, coord) {
  if (is.null(avoidance) || nrow(avoidance) == 0L) return(NULL)
  probe <- data.frame(
    xintercept = I(avoidance$value), .value = avoidance$value,
    .coursekata_protect = TRUE, .height = avoidance$height
  )
  panel <- cutoff_panel_data(probe, panel_params, coord)
  if (is.null(panel)) return(NULL)
  boundary <- panel$boundary
  if (panel$horizontal) {
    available <- abs(boundary$.opposite_x - boundary$x)
    tip <- boundary$x + sign(boundary$.opposite_x - boundary$x) *
      boundary$.height * available
    data.frame(axis = boundary$y, baseline = boundary$x, tip = tip)
  } else {
    available <- abs(boundary$.opposite_y - boundary$y)
    tip <- boundary$y + sign(boundary$.opposite_y - boundary$y) *
      boundary$.height * available
    data.frame(axis = boundary$x, baseline = boundary$y, tip = tip)
  }
}

# Draw triangles whose tips touch the position axis.
cutoff_marker_grobs <- function(data, horizontal, marker_size) {
  lapply(seq_len(nrow(data)), function(i) {
    row <- data[i, , drop = FALSE]
    col <- ggplot2::alpha(row$colour, row$alpha)
    if (horizontal) {
      edge <- if (row$x < 0.5) 0 else 1
      inward <- if (edge == 0) 1 else -1
      base <- grid::unit(row$x, "npc")
      if (isTRUE(all.equal(row$x, edge))) {
        base <- grid::unit(edge, "npc") + grid::unit(inward * marker_size, "mm")
      }
      x <- grid::unit(c(NA, NA, edge), "npc")
      x[1:2] <- base
      y <- grid::unit(rep(row$y, 3), "npc") +
        grid::unit(c(-marker_size / 2, marker_size / 2, 0), "mm")
    } else {
      edge <- if (row$y < 0.5) 0 else 1
      inward <- if (edge == 0) 1 else -1
      x <- grid::unit(rep(row$x, 3), "npc") +
        grid::unit(c(-marker_size / 2, marker_size / 2, 0), "mm")
      base <- grid::unit(row$y, "npc")
      if (isTRUE(all.equal(row$y, edge))) {
        base <- grid::unit(edge, "npc") + grid::unit(inward * marker_size, "mm")
      }
      y <- grid::unit(c(NA, NA, edge), "npc")
      y[1:2] <- base
    }
    grid::polygonGrob(
      x = x, y = y,
      gp = grid::gpar(
        col = col, fill = col,
        lwd = row$linewidth * ggplot2::.pt
      )
    )
  })
}
#' @rdname GeomCutoff
#' @export
geom_cutoff <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", ..., height = 0.2,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomCutoff, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(height = height, na.rm = na.rm, ...)
  )
}
