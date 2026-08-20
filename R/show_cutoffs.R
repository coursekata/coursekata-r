#' Add Cutoff Markers to a Distribution
#'
#' `r lifecycle::badge("experimental")`
#'
#' Adds downward-pointing triangle markers at the empirical quantile cutoffs of a
#' distribution part -- `middle()`, `tails()`, `upper()`, `lower()`, or `outer()`.
#' By default the part is read off the plot's fill aesthetic, e.g.
#' `fill = ~middle(Thumb, .95)`. Passing `part` overrides that reading: it marks
#' whatever part is named there instead, and the fill (if any) is ignored --
#' marking the 99% cutoffs on a plot shaded for the 95% is a deliberate, lossless
#' override, not a mismatch.
#'
#' Calling `show_cutoffs()` more than once on the same plot stacks a second,
#' independent set of markers on top of the first; nothing about the plot or the
#' first call needs to change for the second one to land correctly. A second
#' `labels = TRUE` call, though, draws its labels at the same height as the
#' first's and the two overlap -- `show_cutoffs()` warns about that and draws
#' anyway, because the picture is still the one asked for, just harder to read.
#'
#' `show_cutoffs()` refuses a plot whose first layer does not draw a distribution
#' (a scatterplot, for instance) and, when `part` is given explicitly, a `part`
#' that names a variable other than the one the plot puts on x.
#'
#' @param plot A ggplot of one distribution -- a histogram, bar chart, density,
#'   dotplot, or [gf_squareplot()].
#' @param part A distribution part, e.g. `middle(Thumb, .95)`. Optional: without
#'   it, the part is read off the plot's fill aesthetic.
#' @param color Marker/line color. Default `"#1e3a8a"`.
#' @param size Marker size. Default `4`.
#' @param labels Whether to annotate the cutoffs. Default `FALSE`.
#'
#' @return A ggplot object with cutoff markers and optional labels.
#'
#' @seealso [StatCutoff], a `ggplot2::Stat` that computes the same rule per
#'   panel, for building a marker into a plot with `ggplot2::layer()` directly.
#'
#' @export
#' @examples
#' gf_histogram(~Thumb, data = Fingers, binwidth = 5, fill = ~middle(Thumb, .95)) %>%
#'   show_cutoffs(labels = TRUE)
#'
#' # an explicit part overrides the fill instead of requiring it to match
#' gf_histogram(~Thumb, data = Fingers, binwidth = 5, fill = ~middle(Thumb, .95)) %>%
#'   show_cutoffs(middle(Thumb, .99))
show_cutoffs <- function(plot, part, color = "#1e3a8a", size = 4, labels = FALSE) {
  lifecycle::signal_stage("experimental", "show_cutoffs()")

  spec <- plot_spec(plot)
  has_part <- !missing(part)
  part_quo <- if (has_part) enquo(part) else NULL

  source <- if (has_part) "argument" else "fill"
  fill_like <- if (has_part) list(quo = part_quo, data = spec$data) else spec$resolve_aes("fill")
  cspec <- cutoff_spec(fill_like, source = source)

  x <- spec$resolve_aes("x")
  if (is.null(x) || !is_symbol(quo_get_expr(x$quo))) {
    abort(
      c(
        "show_cutoffs() needs the plot's x aesthetic to be a single variable",
        if (!is.null(x)) glue("found: {deparse1(quo_get_expr(x$quo))}"),
        "compute the variable first, then plot it"
      )
    )
  }
  x_var <- as_name(x$quo)
  if (x_var %in% names(x$data) == FALSE) {
    abort(glue("Can't find `{x_var}` in the plot's data"))
  }

  if (has_part && !identical(cspec$var, x_var)) {
    part_text <- deparse1(quo_get_expr(part_quo))
    abort(
      c(
        "`show_cutoffs()` marks cutoffs on the plot's x axis",
        "x" = glue("the plot draws `{x_var}`, and `{part_text}` describes `{cspec$var}`"),
        "i" = "mark the variable the plot shows, or plot the variable you want marked"
      )
    )
  }

  check_distribution_geom(plot)

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

  if (labels && cutoff_labels_already_drawn(plot)) {
    warn(
      c(
        "the plot already carries cutoff labels",
        "*" = "two sets of labels are drawn at the same height and will overlap",
        "*" = "label the innermost set only"
      ),
      class = "coursekata_cutoff_labels_overlap"
    )
  }

  render_cutoff_plan(plot, plan, geometry, color, size, labels)
}

#' Refuse a plot whose first layer does not draw a distribution
#'
#' Checked by geom class rather than by stat or by data shape, because that is
#' the one thing every distribution-shaped layer this package draws agrees on --
#' including [GeomSquareplot], the package's own, which the fill-detection path
#' already accepted before this check existed.
#'
#' @param plot A ggplot object.
#' @param call The calling environment, for error reporting.
#'
#' @return `NULL`, invisibly. Called for its refusal.
#'
#' @noRd
check_distribution_geom <- function(plot, call = caller_env()) {
  distributions <- c(
    "GeomBar", "GeomHistogram", "GeomArea", "GeomDensity", "GeomDotplot", "GeomSquareplot"
  )
  geom_classes <- if (length(plot$layers) > 0) class(plot$layers[[1]]$geom) else character(0)
  if (any(geom_classes %in% distributions)) {
    return(invisible(NULL))
  }

  abort(
    c(
      "`show_cutoffs()` marks cutoffs on a distribution",
      "x" = if (length(geom_classes) > 0) {
        glue("this plot's first layer draws `{geom_classes[[1]]}`")
      } else {
        "this plot draws nothing yet"
      },
      "i" = paste(
        "cutoffs describe where a distribution's mass sits; plot it with",
        "`gf_histogram()`, `gf_density()`, `gf_dotplot()`, `gf_bar()` or `gf_squareplot()` first"
      )
    ),
    call = call
  )
}

#' Whether a plot already carries a cutoff label from an earlier call
#'
#' Checked by tag rather than by counting `layer_indices()`, because a
#' one-sided part (`upper()`, `lower()`) only ever adds one side's label and
#' either side alone is enough to overlap a second call's.
#'
#' @param plot A ggplot object.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @noRd
cutoff_labels_already_drawn <- function(plot) {
  !is.na(layer_index(plot, "cutoff_lower_label")) || !is.na(layer_index(plot, "cutoff_upper_label"))
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
  # Keep the triangle's body in the scale's lower expansion and put its
  # downward tip at the axis tick. Farther down, the marker occupies the same
  # row as the tick label and can make a value such as 80 unreadable.
  arrow_y <- y_at(-0.03)
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

  unclip_coord(plot)
}

#' Make a plot's coordinate system draw outside its panel
#'
#' Idempotent across stacked calls: a second `show_cutoffs()` call sees the
#' first call's already-unclipped coord as `plot$coordinates`, and wrapping it
#' again would grow the ggproto parent chain one level per stacked call with no
#' visible difference in behavior. The attribute on the wrapped coord is what a
#' later call recognizes to leave its own wrapping alone.
#'
#' @param plot A ggplot object.
#'
#' @return `plot`, with its coordinate system unclipped exactly once.
#'
#' @noRd
unclip_coord <- function(plot) {
  coord <- plot$coordinates
  if (isTRUE(attr(coord, "coursekata_cutoff_unclipped"))) {
    return(plot)
  }

  # ggproto() re-evaluates the parent expression later, so the old coord needs its own name
  unclipped <- ggplot2::ggproto(NULL, coord, clip = "off")
  attr(unclipped, "coursekata_cutoff_unclipped") <- TRUE
  plot$coordinates <- unclipped
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
