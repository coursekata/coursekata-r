#' Draw cutoff anchors as a measured position guide
#'
#' `guide_cutoff()` draws one truthful marker rail and, when requested, one
#' measured label lane per helper call. Two-sided labels dodge along that lane
#' while their leaders remain tied to the truthful cutoff anchors. The guide
#' receives already-computed cutoff values; it never inspects the distribution
#' or recomputes a statistic.
#'
#' @param value One or two finite numeric cutoff values in the position scale's
#'   data space.
#' @param label Optional labels parallel to `value`.
#' @param side Optional side names parallel to `value`, each either `"lower"`
#'   or `"upper"`. Supply `label` and `side` together.
#' @param call_id One positive integer identifying this guide instance.
#' @param colour Colour of the markers, leaders, and labels.
#' @param shape Marker shape. The default triangle points toward the panel.
#' @param size Marker size.
#' @param linewidth Width of marker outlines and leaders.
#' @param title Optional guide title.
#' @param theme A theme for this guide, or `NULL`.
#' @param order Guide order.
#' @param position Guide position. The default is derived from the scale.
#'
#' @return A ggplot2 guide.
#'
#' @format `GuideCutoff` is a [ggplot2::Guide] object.
#' @export
guide_cutoff <- function(value, label = NULL, side = NULL, call_id = 1L,
                         colour = ggplot2::waiver(), shape = 24, size = 4,
                         linewidth = 0.5, title = NULL, theme = NULL,
                         order = 0, position = ggplot2::waiver()) {
  if (!is.numeric(value) || length(value) < 1L || length(value) > 2L ||
      any(!is.finite(value))) {
    abort("`guide_cutoff()`'s `value` must contain one or two finite numbers")
  }

  labels <- cutoff_guide_labels(label, length(value))
  if (is.null(label) && !is.null(side) || !is.null(label) && is.null(side)) {
    abort("`guide_cutoff()` needs `label` and `side` supplied together")
  }
  if (!is.null(side)) {
    if (!is.character(side) || length(side) != length(value) ||
        anyNA(side) || any(!side %in% c("lower", "upper"))) {
      abort("`guide_cutoff()`'s `side` must parallel `value` with `lower` or `upper`")
    }
    if (anyDuplicated(side)) {
      abort("`guide_cutoff()` can draw at most one cutoff for each side")
    }
  } else {
    side <- rep(NA_character_, length(value))
  }

  if (!is.numeric(call_id) || length(call_id) != 1L || is.na(call_id) ||
      call_id < 1 || call_id != as.integer(call_id)) {
    abort("`guide_cutoff()`'s `call_id` must be one positive integer")
  }
  colour <- if (inherits(colour, "waiver")) "#1e3a8a" else colour
  if (!is.character(colour) || length(colour) != 1L || is.na(colour)) {
    abort("`guide_cutoff()`'s `colour` must be one colour")
  }
  if (length(shape) != 1L || is.na(shape)) {
    abort("`guide_cutoff()`'s `shape` must be one point shape")
  }
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size < 0) {
    abort("`guide_cutoff()`'s `size` must be one non-negative number")
  }
  if (!is.numeric(linewidth) || length(linewidth) != 1L ||
      !is.finite(linewidth) || linewidth < 0) {
    abort("`guide_cutoff()`'s `linewidth` must be one non-negative number")
  }
  if (!is_position_guide_label(title)) {
    abort("`guide_cutoff()`'s `title` must be one label or `NULL`")
  }

  ggplot2::new_guide(
    value = value, label = labels, side = side, call_id = as.integer(call_id),
    colour = colour, shape = shape, size = size, linewidth = linewidth,
    title = title, theme = theme, order = order, position = position,
    name = "cutoff", available_aes = c("x", "y"), super = GuideCutoff
  )
}

#' @rdname guide_cutoff
#' @export
GuideCutoff <- ggplot2::ggproto(
  "GuideCutoff", ggplot2::GuideAxis,
  params = c(
    ggplot2::GuideAxis$params,
    list(
      value = numeric(), label = list(), side = character(), call_id = 1L,
      colour = "#1e3a8a", shape = 24, size = 4, linewidth = 0.5
    )
  ),
  hashables = rlang::exprs(title, key$.value, key$.label, key$side, call_id, name),
  extract_key = function(scale, aesthetic, value, label, side, call_id,
                         colour, shape, size, linewidth, ...) {
    anchor <- position_anchor_key(scale, value)
    transformed <- anchor$transformed
    visible <- anchor$visible

    key <- data.frame(ifelse(visible, transformed, NA_real_))
    names(key) <- aesthetic
    key$.value <- value
    key$.label <- I(label)
    key$.visible <- visible
    key$side <- side
    key$call_id <- rep(call_id, length(value))
    key$colour <- rep(colour, length(value))
    key$shape <- I(rep(list(shape), length(value)))
    key$size <- rep(size, length(value))
    key$linewidth <- rep(linewidth, length(value))
    key
  },
  extract_decor = function(...) NULL,
  transform = function(self, params, coord, panel_params) {
    # GuideAxis interprets two NA positions as evidence that a guide is
    # perpendicular to its scale. For this guide they mean something simpler:
    # both truthful anchors are outside the viewport. There is then nothing to
    # transform or draw, and returning early avoids that misleading warning.
    if (!any(params$key$.visible)) return(params)
    ggplot2::GuideAxis$transform(
      params = params, coord = coord, panel_params = panel_params
    )
  },
  setup_elements = function(params, elements, theme) {
    elements <- ggplot2::GuideAxis$setup_elements(params, elements, theme)
    if (!inherits(elements$text, "element_blank")) {
      elements$text$colour <- params$colour
    }
    elements
  },
  build_ticks = function(key, elements, params) {
    visible <- which(key$.visible & !is.na(key[[params$aes]]))
    if (length(visible) == 0L) return(grid::nullGrob())

    markers <- lapply(visible, function(i) {
      marker_shape <- key$shape[[i]]
      if (identical(marker_shape, 24) || identical(marker_shape, 24L)) {
        marker_shape <- ggplot2::waiver()
      }
      position_anchor_grob(
        at = key[[params$aes]][[i]], position = params$position,
        shape = marker_shape, size = key$size[[i]],
        colour = key$colour[[i]], linewidth = key$linewidth[[i]]
      )
    })
    do.call(grid::grobTree, markers)
  },
  build_labels = function(key, elements, params) {
    labelled <- which(
      key$.visible & !is.na(key[[params$aes]]) & !is.na(key$side) &
        !vapply(key$.label, is.null, logical(1))
    )
    if (length(labelled) == 0L || inherits(elements$text, "element_blank")) {
      return(list(grid::nullGrob()))
    }

    # Keep one measured lane per helper call. When both sides are labelled,
    # place them in separate halves of the position span according to their
    # trained screen order. This is the guide analogue of dodging: only the
    # labels move, while every marker and leader begins at the truthful anchor.
    # Sorting the trained positions also keeps leaders from crossing on reverse
    # scales without changing the lower/upper meaning carried by each label.
    trained <- key[[params$aes]][labelled]
    screen_order <- order(trained, match(key$side[labelled], c("lower", "upper")))
    labelled <- labelled[screen_order]
    targets <- if (length(labelled) == 1L) {
      trained[screen_order]
    } else {
      c(0.46, 0.54)
    }
    children <- list()
    for (row in seq_along(labelled)) {
      i <- labelled[[row]]
      at <- key[[params$aes]][[i]]
      target <- targets[[row]]
      center <- 0.5
      inner <- if (params$orth_side == 1) 1 else 0
      justification <- if (length(labelled) == 1L) {
        position_anchor_justification(target)
      } else if (row == 1L) {
        1
      } else {
        0
      }

      text <- if (params$vertical) {
        ggplot2::element_grob(
          elements$text, label = key$.label[[i]],
          x = grid::unit(center, "npc"), y = grid::unit(target, "npc"),
          hjust = 0.5, vjust = justification,
          margin_x = TRUE, margin_y = TRUE
        )
      } else {
        ggplot2::element_grob(
          elements$text, label = key$.label[[i]],
          x = grid::unit(target, "npc"), y = grid::unit(center, "npc"),
          hjust = justification, vjust = 0.5,
          margin_x = TRUE, margin_y = TRUE
        )
      }

      gap <- grid::unit(0.75, "mm")
      edge <- if (params$vertical) {
        grid::unit(center, "npc") +
          (if (params$orth_side == 1) 1 else -1) * (grid::grobWidth(text) / 2 + gap)
      } else {
        grid::unit(center, "npc") +
          (if (params$orth_side == 1) 1 else -1) * (grid::grobHeight(text) / 2 + gap)
      }
      leader <- if (params$vertical) {
        grid::segmentsGrob(
          x0 = grid::unit(inner, "npc"), x1 = edge,
          y0 = grid::unit(at, "npc"), y1 = grid::unit(target, "npc"),
          gp = grid::gpar(
            col = key$colour[[i]], lty = "dashed",
            lwd = key$linewidth[[i]] * ggplot2::.pt
          )
        )
      } else {
        grid::segmentsGrob(
          x0 = grid::unit(at, "npc"), x1 = grid::unit(target, "npc"),
          y0 = grid::unit(inner, "npc"), y1 = edge,
          gp = grid::gpar(
            col = key$colour[[i]], lty = "dashed",
            lwd = key$linewidth[[i]] * ggplot2::.pt
          )
        )
      }
      children <- c(children, list(leader, text))
    }
    list(do.call(grid::grobTree, children))
  },
  measure_grobs = function(grobs, params, elements) {
    marker <- if (any(params$key$.visible)) {
      grid::unit(params$size, "mm")
    } else {
      grid::unit(0, "mm")
    }
    labelled <- which(
      params$key$.visible & !is.na(params$key$side) &
        !vapply(params$key$.label, is.null, logical(1))
    )
    label_size <- grid::unit(0, "mm")
    if (length(labelled) > 0L && !inherits(elements$text, "element_blank")) {
      labels <- lapply(labelled, function(i) {
        ggplot2::element_grob(
          elements$text, label = params$key$.label[[i]],
          x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc")
        )
      })
      dimensions <- lapply(
        labels, if (params$vertical) grid::grobWidth else grid::grobHeight
      )
      label_size <- do.call(grid::unit.pmax, dimensions) + grid::unit(1, "mm")
    }
    spacer <- if (length(labelled) > 0L) grid::unit(0.8, "mm") else grid::unit(0, "mm")
    title <- grid::unit(params$measure_text(grobs$title), "cm")
    sizes <- grid::unit.c(marker, spacer, label_size, title)
    if (isTRUE(params$lab_first)) sizes <- rev(sizes)
    sizes
  }
)

#' Normalize labels for a cutoff guide key
#'
#' @param label Labels supplied to `guide_cutoff()`.
#' @param n Number of cutoff values.
#'
#' @return A list of `n` labels.
#' @noRd
cutoff_guide_labels <- function(label, n) {
  if (is.null(label)) return(rep(list(NULL), n))
  labels <- if (is.expression(label) || is.character(label)) as.list(label) else label
  if (!is.list(labels) || length(labels) != n ||
      any(!vapply(labels, is_position_guide_label, logical(1)))) {
    abort("`guide_cutoff()`'s `label` must parallel `value` with one label each")
  }
  labels
}
