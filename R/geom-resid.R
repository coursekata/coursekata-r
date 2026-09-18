#' Which axis a residual arrived measured on
#'
#' Exactly one of `xend` and `yend` is present. Its name identifies the axis
#' carrying the model's outcome.
#'
#' @param data A layer's data.
#'
#' @return `"y"` or `"x"`.
#'
#' @noRd
resid_axis <- function(data) {
  if ("yend" %in% names(data)) "y" else "x"
}

#' Where a squared residual's four corners go
#'
#' The square is a square on the page, not in data units: the side drawn
#' across the residual is scaled by `aspect` and by the panel's range ratio.
#' The panel's ranges are only final at draw time, which is why this is called
#' from [GeomSquareResid]'s `draw_panel()` and not from a stat.
#'
#' @param data One row per observation, with `x`, `y` (the observation) and one
#'   of `yend`/`xend` (what the model predicts for it). Which one it is says
#'   which axis the residual runs along.
#' @param x_range,y_range The panel's ranges.
#' @param aspect Multiplier on the drawn side length.
#'
#' @return `data` with four rows per observation and one `group` per square.
#'
#' @noRd
square_vertices <- function(data, x_range, y_range, aspect) {
  # The residual defines one side; the square extends along the other axis.
  along <- resid_axis(data)
  across <- if (identical(along, "y")) "x" else "y"
  along_range <- if (identical(along, "y")) y_range else x_range
  across_range <- if (identical(along, "y")) x_range else y_range
  end <- data[[paste0(along, "end")]]

  ratio <- diff(across_range) / diff(along_range)
  side <- abs(data[[along]] - end) * aspect * ratio
  # squares extend away from the nearer edge so they stay inside the panel
  away <- ifelse(data[[across]] > mean(across_range), -1, 1)
  far <- data[[across]] + away * side

  vertices <- data[rep(seq_len(nrow(data)), each = 4), , drop = FALSE]
  vertices[[across]] <- as.vector(rbind(data[[across]], far, far, data[[across]]))
  vertices[[along]] <- as.vector(rbind(data[[along]], data[[along]], end, end))
  vertices$group <- rep(seq_len(nrow(data)), each = 4)
  rownames(vertices) <- NULL
  vertices
}

#' Carry a prediction alongside the observation it belongs to
#'
#' `xend`/`yend` is a positional aesthetic on purpose: that is what gets the
#' prediction transformed by its scale and trained into the panel's range.
#' Exactly one of the two arrives, on the axis the plot put the model's
#' outcome on.
#'
#' `compute_layer` checks the required aesthetics, then returns every row
#' unchanged. An observation with an `NA` on it (a predictor the model dropped)
#' survives the stat instead of being removed
#' before the position runs. The residual's jitter is a function of the seed
#' and the rows it is handed, exactly as the point layer's is, so losing a row
#' here would hand it a different sequence of draws and the segment would land
#' away from its point.
#'
#' @format A [ggplot2::Stat] object.
#'
#' @seealso [stat_resid()], [geom_resid()], and [geom_square_resid()].
#' @export
StatResid <- ggplot2::ggproto(
  "StatResid", ggplot2::Stat,
  required_aes = c("x", "y", "xend|yend"),
  # Fixed aesthetics reach the stat through params before the geom merges them.
  extra_params = c("na.rm", "orientation", "x", "y", "xend", "yend"),
  compute_layer = function(self, data, params, layout) {
    supplied <- union(names(data), names(params))
    missing <- setdiff(c("x", "y"), supplied)
    if (!any(c("xend", "yend") %in% supplied)) {
      missing <- c(missing, "xend or yend")
    }
    if (length(missing) > 0L) {
      fn <- if (inherits(self, "StatReduce")) "stat_reduce" else "stat_resid"
      abort(glue("`{fn}()` requires the following missing aesthetics: {collapse(missing)}."))
    }
    data
  },
  compute_panel = function(data, scales, ...) data
)

#' Carry reduction endpoints alongside their observations
#'
#' A reduction uses the same endpoint representation as a residual, but its
#' starting point is the grand mean rather than the observation. The separate
#' stat class keeps that statistical meaning visible to ggplot2 users and to
#' code inspecting the layer. [GeomResid] and [GeomSquareResid] can draw either
#' stat.
#'
#' @format A [ggplot2::Stat] object.
#'
#' @seealso [stat_reduce()], [geom_reduce()], and [geom_square_reduce()].
#' @export
StatReduce <- ggplot2::ggproto(
  "StatReduce", StatResid
)

#' Jitter a residual's observed end the way its point was jittered
#'
#' Seeded jitter gives the observation the same offset as its point. Running
#' [ggplot2::PositionJitter] on `x` and `y` alone leaves fitted endpoints fixed.
#' For reductions, `outcome` also holds the grand-mean axis fixed; the parent
#' position still computes both axes so the other offset remains unchanged.
#'
#' @format A [ggplot2::Position] object.
#'
#' @seealso [gf_resid()], which pairs this position with the plot's own jitter.
#' @noRd
PositionResidJitter <- ggplot2::ggproto(
  "PositionResidJitter", ggplot2::PositionJitter,
  outcome = NULL,
  compute_panel = function(self, data, params, scales) {
    jittered <- ggplot2::ggproto_parent(ggplot2::PositionJitter, self)$compute_panel(
      data[c("x", "y")], params, scales
    )
    axes <- setdiff(c("x", "y"), self$outcome)
    data[axes] <- jittered[axes]
    data
  }
)
position_resid_jitter <- function(width = NULL, height = NULL, seed = NA, outcome = NULL) {
  ggplot2::ggproto(
    NULL, PositionResidJitter, width = width, height = height, seed = seed, outcome = outcome
  )
}

#' The position a residual has to be drawn with, and the plot to draw it on
#'
#' An unseeded jitter cannot be reproduced by another layer. When needed, this
#' function gives the selected observation layer a seed and returns the matching
#' endpoint-preserving position for the residual layer.
#'
#' @param plot The plot the residual is being drawn on.
#' @param outcome The aesthetic (`"x"` or `"y"`) to hold at its pre-jitter
#'   value, or `NULL` to jitter both.
#'
#' @return A list with `plot` and `position`.
#'
#' @noRd
resid_jitter <- function(plot, outcome = NULL) {
  if (!inherits(plot, "ggplot")) return(list(plot = plot, position = "identity"))
  plot <- stabilize_resid_mappings(plot)
  plot <- stabilize_source_data(plot)
  plain <- list(plot = plot, position = "identity")
  index <- plot_source_index(plot)
  if (is.na(index)) {
    return(plain)
  }
  pos <- plot$layers[[index]]$position
  if (inherits(pos, "PositionJitterdodge")) {
    resid_layer_position(pos, "x")
  }
  if (!inherits(pos, "PositionJitter")) {
    return(plain)
  }

  seed <- pos$seed
  if (!isTRUE(is.finite(seed))) {
    seed <- with_random_seed_restored(sample.int(.Machine$integer.max, 1L))
    plot$layers[[index]] <- layer_with_position(
      plot$layers[[index]],
      ggplot2::position_jitter(width = pos$width, height = pos$height, seed = seed)
    )
  }

  list(
    plot = plot,
    position = position_resid_jitter(pos$width, pos$height, seed, outcome = outcome)
  )
}

#' Draw a residual as a segment from a prediction to an observation
#'
#' The segment is drawn from what the model predicts to what was observed, so
#' `x`/`y` is the observation and `xend`/`yend` the prediction until the moment
#' of drawing. Which of the two ends arrives says which axis the residual is
#' measured on. [geom_resid()] pairs it with [StatResid] and computes those
#' endpoints from a fitted model.
#'
#' @format A [ggplot2::Geom] object.
#'
#' @seealso [gf_resid()], the ggformula function for the same layer.
#' @export
GeomResid <- ggplot2::ggproto(
  "GeomResid", ggplot2::GeomSegment,
  extra_params = c("na.rm", ".resid_fn"),
  required_aes = c("x", "y", "xend|yend"),
  handle_na = function(self, data, params) {
    # the inherited drop reads `required_aes` literally, and `"xend|yend"` is
    # not a column, so neither end would ever be checked: name the one that
    # arrived
    ggplot2::remove_missing(
      data, params$na.rm,
      c("x", "y", paste0(resid_axis(data), "end"), self$non_missing_aes),
      params$.resid_fn %||% "geom_resid"
    )
  },
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        arrow.fill = NULL, lineend = "butt",
                        linejoin = "round", na.rm = FALSE) {
    segments <- if (identical(resid_axis(data), "y")) {
      transform(data, xend = x, y = yend, yend = y)
    } else {
      transform(data, yend = y, x = xend, xend = x)
    }
    ggplot2::GeomSegment$draw_panel(
      segments, panel_params, coord,
      arrow = arrow, arrow.fill = arrow.fill,
      lineend = lineend, linejoin = linejoin, na.rm = na.rm
    )
  }
)

#' Draw a residual as the square it would make
#'
#' The square is expanded here rather than in [StatResid] because a stat runs
#' before the position: four corners jittered one row at a time tear apart.
#' It is also the only place the panel's final ranges are known, and the
#' square is a square on the page rather than in data units.
#'
#' @format A [ggplot2::Geom] object.
#'
#' @seealso [geom_square_resid()] and [gf_square_resid()].
#' @export
GeomSquareResid <- ggplot2::ggproto(
  "GeomSquareResid", ggplot2::GeomPolygon,
  extra_params = c("na.rm", ".resid_fn"),
  required_aes = c("x", "y", "xend|yend"),
  handle_na = function(self, data, params) {
    ggplot2::remove_missing(
      data, params$na.rm,
      c("x", "y", paste0(resid_axis(data), "end"), self$non_missing_aes),
      params$.resid_fn %||% "geom_square_resid"
    )
  },
  draw_panel = function(self, data, panel_params, coord, aspect = 4 / 6,
                        rule = "evenodd", lineend = "butt", linejoin = "round",
                        linemitre = 10, na.rm = FALSE) {
    if (nrow(data) == 0) {
      return(ggplot2::zeroGrob())
    }
    # The observations are still in scale space. Ask the coordinate system for
    # those ranges; panel x/y ranges have already been exchanged by coord_flip.
    ranges <- coord$backtransform_range(panel_params)
    ggplot2::GeomPolygon$draw_panel(
      square_vertices(data, ranges$x, ranges$y, aspect),
      panel_params, coord,
      rule = rule, lineend = lineend, linejoin = linejoin, linemitre = linemitre
    )
  }
)
