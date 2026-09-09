#' Add Cutoff Markers to a Distribution
#'
#' `r lifecycle::badge("experimental")`
#'
#' Adds empirical quantile cutoffs for a distribution part -- `middle()`,
#' `tails()`, `upper()`, `lower()`, or `outer()`. Short dashed stems end in
#' downward triangles that touch the numeric axis. Optional callouts are drawn
#' in the data panel. Repeated labelled calls are measured and placed together.
#' Their boxes stay separate when the panel has room, and their routed leaders
#' remain tied to the exact cutoffs.
#'
#' By default the part is read off the plot's fill aesthetic, e.g.
#' `fill = ~middle(Thumb, .95)`. Passing `part` overrides that reading: it marks
#' whatever part is named there instead, and the fill (if any) is ignored --
#' marking the 99% cutoffs on a plot shaded for the 95% is a deliberate, lossless
#' override, not a mismatch.
#'
#' `show_cutoffs()` refuses a plot whose first layer does not draw a distribution
#' (a scatterplot, for instance) and, when `part` is given explicitly, a `part`
#' that names a variable other than the one the plot puts on x.
#'
#' @param object A ggplot of one distribution -- a histogram, bar chart, density,
#'   dotplot, or [gf_squareplot()].
#' @param part A distribution part, e.g. `middle(Thumb, .95)`. Optional: without
#'   it, the part is read off the plot's fill aesthetic.
#' @param color Marker/line color. Default `"#1e3a8a"`.
#' @param size Marker size. Default `4`.
#' @param show_labels Whether to annotate the cutoffs. Default `FALSE`.
#' @param plot Deprecated alias for `object`.
#' @param labels Deprecated alias for `show_labels`.
#'
#' @return A ggplot object with cutoff markers and optional labels.
#'
#' @seealso [stat_cutoff()] computes the same rule per panel; [geom_cutoff()]
#'   is the lower-level drawing component. [guide_cutoff()] can place cutoff
#'   anchors in a position guide instead.
#'
#' @export
#' @examples
#' gf_histogram(~Thumb, data = Fingers, binwidth = 5, fill = ~middle(Thumb, .95)) %>%
#'   show_cutoffs(show_labels = TRUE)
#'
#' # an explicit part overrides the fill instead of requiring it to match
#' gf_histogram(~Thumb, data = Fingers, binwidth = 5, fill = ~middle(Thumb, .95)) %>%
#'   show_cutoffs(middle(Thumb, .99))
show_cutoffs <- function(object = NULL, part, color = "#1e3a8a", size = 4,
                         show_labels = FALSE,
                         plot = lifecycle::deprecated(),
                         labels = lifecycle::deprecated()) {
  lifecycle::signal_stage("experimental", "show_cutoffs()")

  object_missing <- missing(object)
  part_missing <- missing(part)
  plot_missing <- missing(plot)
  show_labels_missing <- missing(show_labels)
  labels_missing <- missing(labels)
  shape <- normalize_cutoff_call_shape(
    enquo(object), enquo(part), object_missing, part_missing, plot_missing,
    user_call = sys.call()
  )
  object_value <- if (shape$object_missing) NULL else eval_tidy(shape$object)
  object <- normalize_plot_argument(
    object_value, plot, shape$object_missing, plot_missing, "show_cutoffs"
  )
  show_labels <- normalize_show_labels_argument(
    show_labels, labels, show_labels_missing, labels_missing
  )
  if (!is.logical(show_labels) || length(show_labels) != 1L || is.na(show_labels)) {
    abort("`show_cutoffs()`'s `show_labels` must be `TRUE` or `FALSE`")
  }
  if (!is.character(color) || length(color) != 1L || is.na(color)) {
    abort("`show_cutoffs()`'s `color` must be one color")
  }
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size < 0) {
    abort("`show_cutoffs()`'s `size` must be one non-negative number")
  }
  if (!inherits(object, "ggplot")) {
    abort("`show_cutoffs()` needs a ggplot object")
  }
  check_distribution_geom(object)
  distribution <- distribution_plot_spec(object, "show_cutoffs")
  spec <- distribution$plot
  has_part <- !shape$part_missing
  part_quo <- if (has_part) shape$part else NULL

  source <- if (has_part) "argument" else "fill"
  fill_like <- if (has_part) list(quo = part_quo, data = spec$data) else spec$resolve_aes("fill")
  cspec <- cutoff_spec(fill_like, source = source)

  x_var <- distribution_plot_column(distribution, "show_cutoffs")

  if (has_part && !identical(cspec$var, distribution$label)) {
    part_text <- deparse1(quo_get_expr(part_quo))
    abort(
      c(
        "`show_cutoffs()` marks cutoffs on the plot's x axis",
        "x" = glue(
          "the plot draws `{distribution$label}`, and `{part_text}` describes `{cspec$var}`"
        ),
        "i" = "mark the variable the plot shows, or plot the variable you want marked"
      )
    )
  }

  values <- distribution$data[[x_var]]
  check_distribution_x_scale(distribution, "show_cutoffs")
  plan <- cutoff_plan(cspec, values)
  sides <- c("lower", "upper")
  anchors <- unlist(plan[sides], use.names = FALSE)
  keep <- !is.na(anchors)
  anchors <- anchors[keep]
  sides <- sides[keep]
  call_id <- next_cutoff_call_id(object)

  cutoff_labels <- if (show_labels) {
    paste0(
      plan$label, " of\nvalues ",
      ifelse(sides == "lower", "below", "above")
    )
  } else {
    rep(NA_character_, length(anchors))
  }
  stem_data <- data.frame(
    xintercept = I(anchors), .value = anchors,
    .coursekata_protect = TRUE, label = cutoff_labels,
    side = sides, call_id = call_id
  )
  stem_mapping <- ggplot2::aes(
    xintercept = .data$xintercept,
    .value = .data$.value,
    .coursekata_protect = .data$.coursekata_protect,
    label = .data$label,
    side = .data$side,
    call_id = .data$call_id
  )
  out <- object + cutoff_annotation_layer(
    data = stem_data, mapping = stem_mapping, colour = color,
    marker_size = size
  )
  update_cutoff_callout_layer(out, cutoff_avoidance_profile(values))
}

#' Build the private stem layer used by `show_cutoffs()`
#'
#' @param data, mapping Cutoff data and mappings.
#' @param colour,marker_size Fixed annotation styling.
#'
#' @return A tagged ggplot2 layer carrying explicit cutoff style metadata.
#' @noRd
cutoff_annotation_layer <- function(data, mapping, colour, marker_size) {
  style <- list(
    colour = colour, fill = "white", linetype = "dashed", linewidth = 0.5
  )
  layer <- ggplot2::layer(
    geom = GeomCutoff, mapping = mapping, data = data,
    stat = "identity", position = "identity", show.legend = FALSE,
    inherit.aes = FALSE,
    params = c(style, list(
      height = 0.2, marker = TRUE, marker_size = marker_size,
      .draw_callouts = FALSE, na.rm = TRUE
    ))
  )
  attr(layer, "coursekata_cutoff_style") <- style
  tag_layer(layer, "distribution_cutoff")
}

#' Read one high-level cutoff layer's callout payload
#'
#' @param layer A ggplot2 layer.
#'
#' @return Labelled rows with fixed styling, or `NULL`.
#' @noRd
cutoff_layer_payload <- function(layer) {
  if (!identical(attr(layer, "coursekata_layer"), "distribution_cutoff")) {
    return(NULL)
  }
  data <- layer$data
  required <- c(
    "xintercept", ".coursekata_protect", ".value", "label", "side", "call_id"
  )
  if (!is.data.frame(data) || !all(required %in% names(data))) return(NULL)
  data <- data[!is.na(data$label), , drop = FALSE]
  if (nrow(data) == 0L) return(NULL)

  style <- attr(layer, "coursekata_cutoff_style")
  if (is.null(style)) return(NULL)
  data$.colour <- rep(style$colour, nrow(data))
  data$.fill <- rep(style$fill, nrow(data))
  data$.linetype <- rep(style$linetype, nrow(data))
  data$.linewidth <- rep(style$linewidth, nrow(data))
  data
}

#' Replace the private callout coordinator from current cutoff layers
#'
#' Stem layers retain the label metadata produced by their helper call. A
#' single final layer receives the labelled rows and fixed styles from all of
#' them, which gives draw-time placement one complete collision domain without
#' changing the public layer API.
#'
#' @param plot A ggplot object.
#' @param avoidance Raw distribution occupancy profile.
#'
#' @return A copied ggplot object with at most one callout coordinator layer.
#' @noRd
update_cutoff_callout_layer <- function(plot, avoidance = NULL) {
  plot <- replace_tagged_layers(plot, "distribution_cutoff_callouts")
  rows <- Filter(Negate(is.null), lapply(plot$layers, cutoff_layer_payload))
  if (length(rows) == 0L) return(plot)

  data <- do.call(rbind, rows)
  rownames(data) <- NULL
  data$xintercept <- I(as.numeric(data$xintercept))
  data$.colour <- I(data$.colour)
  data$.fill <- I(data$.fill)
  data$.linetype <- I(data$.linetype)
  data$.linewidth <- I(data$.linewidth)
  plot + tag_layer(
    geom_cutoff_callouts(data, avoidance = avoidance),
    "distribution_cutoff_callouts"
  )
}

#' Estimate the shape that cutoff callouts should prefer not to cover
#'
#' The supported front doors all display one numeric distribution, even though
#' they may render it as bins, dots, density, or squares. A normalized binned
#' profile gives the layout a stable cross-geom estimate of occupied space. It
#' is used only as a soft placement cost; the plotted data remain authoritative.
#'
#' @param values Numeric distribution values.
#'
#' @return A data frame of raw values and relative occupied heights.
#' @noRd
cutoff_avoidance_profile <- function(values) {
  values <- values[is.finite(values)]
  distinct <- sort(unique(values))
  if (length(distinct) == 0L) {
    return(data.frame(value = numeric(), height = numeric()))
  }
  if (length(distinct) == 1L) {
    spread <- max(1, abs(distinct[[1L]]) * 0.05)
    return(data.frame(
      value = distinct[[1L]] + c(-spread, 0, spread),
      height = c(0, 0.86, 0)
    ))
  }

  bin_count <- max(12L, min(40L, as.integer(ceiling(sqrt(length(values))) * 2L)))
  profile <- graphics::hist(values, breaks = bin_count, plot = FALSE)
  data.frame(
    value = profile$mids,
    height = 0.86 * profile$counts / max(profile$counts)
  )
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
