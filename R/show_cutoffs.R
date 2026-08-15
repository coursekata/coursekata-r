#' Add Cutoff Markers to a Histogram
#'
#' `r lifecycle::badge("experimental")`
#'
#' Adds downward-pointing triangle markers at the empirical quantile cutoffs on
#' a histogram that uses a distribution part function (`middle()`, `tails()`,
#' `upper()`, `lower()`, or `outer()`) in its fill aesthetic.
#'
#' @param plot A ggplot histogram with `fill` mapped to a distribution part
#'   function, e.g., `fill = ~middle(Thumb, .95)`.
#' @param color Marker/line color. Default `"#1e3a8a"`.
#' @param size Marker size. Default `4`.
#' @param labels Whether to annotate the cutoffs. Default `FALSE`.
#'
#' @return A ggplot object with cutoff markers and optional labels.
#'
#' @export
#' @examples
#' gf_histogram(~Thumb, data = Fingers, binwidth = 5, fill = ~middle(Thumb, .95)) %>%
#'   show_cutoffs(labels = TRUE)
show_cutoffs <- function(plot, color = "#1e3a8a", size = 4, labels = FALSE) {
  lifecycle::signal_stage("experimental", "show_cutoffs()")

  spec <- plot_spec(plot)
  cspec <- cutoff_spec(spec$resolve_aes("fill"))

  x <- spec$resolve_aes("x")
  if (is.null(x) || !is_symbol(quo_get_expr(x$quo))) {
    abort(
      c(
        "show_cutoffs() needs the plot's x aesthetic to be a single variable",
        if (!is.null(x)) glue("found: {deparse(quo_get_expr(x$quo))}"),
        "compute the variable first, then plot it"
      )
    )
  }
  x_var <- as_name(x$quo)
  if (x_var %in% names(x$data) == FALSE) {
    abort(glue("Can't find `{x_var}` in the plot's data"))
  }

  plan <- cutoff_plan(cspec, x$data[[x_var]])

  geometry <- plot_geometry(plot)
  if (is.null(geometry$y_range) || is.null(geometry$x_range)) {
    abort(
      c(
        "show_cutoffs() needs a plot with cartesian x and y axes",
        glue("this plot uses {class(plot$coordinates)[[1]]}"),
        "the markers are placed relative to the axis ranges, which a polar plot has not got"
      )
    )
  }

  render_cutoff_plan(plot, plan, geometry, color, size, labels)
}

#' Draw a cutoff_plan() as tagged marker layers
#'
#' @param plot A ggplot object.
#' @param plan A `cutoff_plan()` list.
#' @param geometry A `plot_geometry()` list.
#' @param color Marker and line color.
#' @param size Marker size.
#' @param labels Whether to annotate the cutoffs.
#'
#' @return The plot, with tagged layers added and its own coord unclipped.
#'
#' @noRd
render_cutoff_plan <- function(plot, plan, geometry, color, size, labels) {
  # the markers hang below the count axis, and a transformed one has no value below zero
  # to hang them at, so every height here is a fraction of the panel the counts are drawn
  # in -- the horizontal panel range when coord_flip() has moved them there
  flipped <- inherits(plot$coordinates, "CoordFlip")
  y_at <- panel_fraction(if (flipped) geometry$x_range else geometry$y_range)
  arrow_y <- y_at(-0.06)
  line_top_y <- y_at(0.20)
  line_bottom_y <- y_at(-0.045)
  label_y <- y_at(0.65)
  leader_y <- y_at(0.57)
  # a step of the axis is only an even visual step in scale space, so the offsets are sized there
  x_scale <- geometry$x_transform %||% list(transform = identity, inverse = identity)
  x_edges <- x_scale$transform(plan$data_range)
  x_span <- diff(x_edges)

  for (side in c("lower", "upper")) {
    cutoff <- plan[[side]]
    if (is.na(cutoff)) next
    direction <- if (side == "lower") 1 else -1

    plot <- plot + tag_layer(
      ggplot2::annotate(
        "segment", x = cutoff, xend = cutoff,
        y = line_bottom_y, yend = line_top_y,
        linetype = "dashed", linewidth = 0.5, color = color
      ),
      paste0("cutoff_", side)
    )

    if (labels) {
      edge <- x_edges[[if (side == "lower") 1 else 2]]
      label_x <- x_scale$inverse(edge + direction * x_span * 0.08)
      leader_x <- x_scale$inverse(edge + direction * x_span * 0.10)
      plot <- plot +
        tag_layer(
          ggplot2::annotate(
            "segment", x = cutoff, xend = leader_x,
            y = line_top_y, yend = leader_y,
            linetype = "dashed", linewidth = 0.5, color = color
          ),
          paste0("cutoff_", side, "_leader")
        ) +
        tag_layer(
          ggplot2::annotate(
            "text", x = label_x, y = label_y,
            label = paste0(plan$label, " of\nvalues ", if (side == "lower") "below" else "above"),
            hjust = 0.5, vjust = 0.5, size = 3.2,
            color = color, fontface = "italic"
          ),
          paste0("cutoff_", side, "_label")
        )
    }

    plot <- plot + tag_layer(
      ggplot2::annotate(
        "point", x = cutoff, y = arrow_y,
        shape = 25, size = size, fill = color, color = color
      ),
      paste0("cutoff_", side, "_marker")
    )
  }

  # ggproto() re-evaluates the parent expression later, so the old coord needs its own name
  coord <- plot$coordinates
  plot$coordinates <- ggplot2::ggproto(NULL, coord, clip = "off")
  plot
}

#' Turn a fraction of the axis into a position no scale will touch
#'
#' The panel's range is already expressed in the space the axis is drawn in, so a
#' fraction of it is a fraction of the drawing whatever transformation produced it.
#' `I()` marks the result as an AsIs value, which ggplot2 has left untransformed and
#' untrained since 3.5.0, so it arrives at the panel as a plain npc position.
#'
#' @param range The panel's range for one axis.
#'
#' @return A function of one fraction of the axis top, returning an AsIs npc position.
#'
#' @noRd
panel_fraction <- function(range) {
  function(f) I((f * range[[2]] - range[[1]]) / (range[[2]] - range[[1]]))
}
