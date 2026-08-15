#' Read what a one-variable distribution overlay needs from a plot
#'
#' Refuses, by name, everything the overlays have no honest answer for: a plot
#' with a variable on each axis, an unsupported coordinate system, and a
#' categorical distribution. `show_dgp()` additionally requires an upright
#' cartesian layout; `show_mean()` remains meaningful under a flip.
#'
#' @param plot A ggplot object.
#' @param fn The calling function's name, for error messages.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `values`, `label`, `data`, `quo`, `facets`, `count_top`,
#'   `x_limits` and `y_transform`.
#'
#' @noRd
overlay_spec <- function(plot, fn, call = caller_env()) {
  spec <- plot_spec(plot)

  if (identical(fn, "show_dgp") && inherits(plot$coordinates, "CoordFlip")) {
    abort(c(
      "`show_dgp()` needs an upright cartesian plot",
      "*" = "its labels describe a horizontal parameter axis above a vertical count axis"
    ), call = call)
  }

  if (!is.null(spec$resolve_aes("y"))) {
    abort(
      c(
        glue("`{fn}()` needs a plot of one distribution"),
        glue("this plot puts a variable on each axis: {collapse(spec$axes)}"),
        "the middle of a two-variable plot is a model; draw it with `gf_model()`"
      ),
      call = call
    )
  }

  x <- spec$resolve_aes("x")
  if (is.null(x)) {
    abort(
      c(glue("`{fn}()` needs a plot with an x aesthetic"),
        "map the distribution's variable to x"),
      call = call
    )
  }

  values <- eval_tidy(x$quo, data = x$data)
  if (!is.numeric(values)) {
    abort(
      c(glue("`{fn}()` needs a numeric distribution"),
        glue("`{as_label(x$quo)}` is {class(values)[[1]]}")),
      call = call
    )
  }
  values <- values[!is.na(values)]
  if (length(values) == 0) {
    abort(glue("`{as_label(x$quo)}` has no non-missing values"), call = call)
  }

  # the band sits above the count axis and raises it, so it must not raise the
  # top the next overlay measures from: read the plot as it was before it landed
  bare <- plot
  bare$layers <- Filter(
    function(l) !startsWith(attr(l, "coursekata_layer") %||% "", "dgp_"),
    plot$layers
  )
  # one build serves both the geometry below and the label read at the end --
  # a second ggplot_build() would re-run every stat and repeat any warning
  # the build emits (e.g. "Removed 1 row containing non-finite values")
  built <- ggplot2::ggplot_build(bare)
  geometry <- geometry_from_build(built)
  if (is.null(geometry$y_range) || is.null(geometry$x_range)) {
    abort(
      c(
        glue("`{fn}()` needs a plot with cartesian x and y axes"),
        glue("this plot uses {class(plot$coordinates)[[1]]}"),
        "the overlay is placed against the axis ranges, which a polar plot has not got"
      ),
      call = call
    )
  }

  # a discrete y scale reports x_range/y_range (so the cartesian guard above
  # does not catch it) but y_limits is character or zero-length -- there is no
  # count to raise or span, so refuse before indexing it
  if (length(geometry$y_limits) < 2 || !is.numeric(geometry$y_limits)) {
    abort(
      c(
        glue("`{fn}()` needs a plot with a numeric count axis"),
        "this plot's y scale is discrete and has no count to mark or span"
      ),
      call = call
    )
  }

  # y_limits is expressed in the scale's transformed space (e.g. sqrt(count));
  # count_top is handed to layers that get transformed a second time when the
  # scale draws them, so it has to come back to data space first
  count_top <- geometry$y_limits[[2]]
  if (!is.null(geometry$y_transform)) {
    count_top <- geometry$y_transform$inverse(count_top)
  }

  list(
    values = values, label = as_label(x$quo), data = x$data, quo = x$quo,
    facets = spec$facets, count_top = count_top,
    x_limits = geometry$x_limits,
    y_transform = geometry$y_transform
  )
}

#' Reduce a panel to the point its distribution's mean sits at
#'
#' One mean per panel, following `StatSdRuler`'s reasoning: letting ggplot2
#' partition the data by panel -- rather than aggregating by hand before the
#' layer is built -- is what keeps a facet expression that is not one-to-one
#' on its raw variable (`facet_wrap(~ cut(v, 2))`) from getting a different
#' answer than the panel it is drawn in. A hand-rolled aggregation has to
#' re-evaluate the facet expression itself to know which rows share a panel,
#' and `cut()`'s breaks depend on the whole column, so re-evaluating it against
#' only a representative row per group -- the input a bare column name or
#' `factor(g)` tolerates fine -- silently invents extra panels instead. This
#' stat never re-evaluates the facet expression: it receives exactly the rows
#' ggplot2 already assigned to each panel, which also means a facet variable
#' read from the caller's environment or named with backticks needs no special
#' handling here, and a panel whose facet value is missing gets its own mean
#' rather than being silently dropped.
#'
#' The stat emits an `xintercept` and nothing else, because a mean is a value on
#' one variable and the mark for it is a line at that value: [ggplot2::GeomVline]
#' spans whatever panel it lands in, so there is no vertical extent to compute,
#' pass in, or keep in step with a transformed count axis. `gf_model()` draws an
#' intercept the same way and for the same reason.
#'
#' Emitting endpoints instead is what used to require every panel's trained top,
#' measured off the plot as it stood when `show_mean()` was called -- which meant
#' faceting afterwards handed this stat a panel that top was never measured for,
#' and the mean was dropped from it. A line with no endpoints has nothing to
#' measure and nothing to miss, so that ordering constraint is gone.
#'
#' @format A [ggplot2::Stat] object.
#' @noRd
StatDistMean <- ggplot2::ggproto(
  "StatDistMean", ggplot2::Stat,
  required_aes = "x",
  compute_panel = function(data, scales) {
    data.frame(xintercept = mean(data$x, na.rm = TRUE))
  }
)

#' Mark a Distribution's Mean
#'
#' `r lifecycle::badge("experimental")`
#'
#' Draws a vertical line at the mean of the variable a distribution is built
#' from, spanning the count axis. Works on any plot of one distribution --
#' `gf_histogram()`, `gf_dotplot()`, [gf_squareplot()]. A faceted plot gets one
#' line per panel, at that panel's own mean, because a facet is a region with
#' its own subset of the data. Each line spans the count axis of the panel it
#' is drawn in, so `scales = "free_y"` is supported and no panel is stretched
#' to hold another panel's line.
#'
#' A plot with a variable on each axis is refused. The middle of a
#' two-variable plot is a model, and [gf_model()] draws models -- an hline at
#' the mean of the outcome for the empty model. Averaging the x variable of a
#' scatterplot would draw a line nobody asked for.
#'
#' Facet the plot before calling `show_mean()`, not after: each panel's top is
#' measured once, from the plot as it stands when `show_mean()` is called, so
#' a panel added later has no top to draw against and is left without a line.
#'
#' @param plot A plot of one distribution.
#' @param color Line color. Default `"#E60000"`.
#' @param linetype Line type. Default `"longdash"`.
#' @param linewidth Line width. Default `0.7`.
#'
#' @return The plot, with a tagged mean line added.
#'
#' @seealso [show_dgp()] frames a sampling distribution with the process that
#'   generated it; [gf_model()] draws the mean of a two-variable plot's outcome.
#'
#' @export
#' @examples
#' gf_histogram(~Thumb, data = Fingers, binwidth = 5) %>% show_mean()
#'
#' # a facet is a region with its own subset, so each panel gets its own mean
#' gf_histogram(~Thumb | Sex, data = Fingers, binwidth = 5) %>% show_mean()
show_mean <- function(plot, color = "#E60000", linetype = "longdash", linewidth = 0.7) {
  lifecycle::signal_stage("experimental", "show_mean()")
  spec <- overlay_spec(plot, "show_mean")

  # x is mapped from the distribution's own quosure, not a precomputed value,
  # so StatDistMean sees exactly the rows ggplot2 assigned to each panel and
  # takes their mean there -- see StatDistMean's own doc for why that is not
  # the same thing as aggregating by hand before the layer is built
  mapping <- ggplot2::aes()
  mapping$x <- spec$quo

  plot + tag_layer(
    ggplot2::layer(
      data = spec$data, mapping = mapping, geom = ggplot2::GeomVline,
      stat = StatDistMean, position = "identity",
      inherit.aes = FALSE, show.legend = FALSE,
      params = list(
        colour = color, linetype = linetype, linewidth = linewidth, na.rm = TRUE
      )
    ),
    "distribution_mean"
  )
}

#' Frame a Sampling Distribution With Its Data Generating Process
#'
#' `r lifecycle::badge("experimental")`
#'
#' Frames a distribution of estimates with the process that generated them: the
#' population model on a top axis labelled "Population Parameter (DGP)", the
#' sample estimate below the plot, and a marker at the null hypothesis
#' (\eqn{\beta_1 = 0}) on both -- drawn only when zero is on the axis.
#'
#' The band is drawn **inside** the panel, and the count axis is raised to hold
#' it. It has to be: countable squares size the separator between them from the
#' fraction of the panel they occupy, so a band hanging outside the panel would
#' leave every square a different shape. A plot whose count axis is pinned with
#' `scale_y_continuous(limits = )` or `coord_cartesian(ylim = )` is refused,
#' because there is no room to raise without discarding the caller's chosen
#' range; set a minimum height with `expand_limits(y = )` instead. A faceted
#' plot needs a shared count axis (the default): `scales = "free_y"` is
#' refused, because the band's height is one number for every panel. A
#' transformed count axis (`scale_y_sqrt()`, `scale_y_log10()`) is refused
#' too: the sample estimate band is drawn in the margin below the panel,
#' where a transformed scale has no value. `show_mean()` is unaffected by
#' either restriction.
#'
#' @param plot A plot of one distribution of estimates.
#' @param color Color of the axes, equations and titles. Default `"#003d70"`.
#' @param null_color Color of the null hypothesis marker. Default `"#E60000"`.
#' @param size Size of the null hypothesis marker. Default `4`.
#'
#' @return The plot, with tagged annotation layers added and its count axis
#'   raised to hold them.
#'
#' @seealso The sampling distributions guide draws this figure inside a full
#'   shuffle-and-estimate workflow:
#'   <https://coursekata.github.io/coursekata-r/articles/sampling-distributions.html>
#'
#' @export
#' @examples
#' # with only ten shuffles the mean of the distribution can land well away
#' # from the null hypothesis marked on the top axis
#' set.seed(42)
#' shuffled <- data.frame(b1 = replicate(10, {
#'   b1(lm(base::sample(TipExperiment$Tip) ~ Condition, data = TipExperiment))
#' }))
#'
#' # expand_limits() sets the count axis so two runs can be compared side by
#' # side; show_dgp() raises it further to make room for the population band
#' gf_histogram(~b1, data = shuffled, binwidth = 2) %>%
#'   gf_refine(ggplot2::expand_limits(y = 10)) %>%
#'   show_mean() %>%
#'   show_dgp()
show_dgp <- function(plot, color = "#003d70", null_color = "#E60000", size = 4) {
  lifecycle::signal_stage("experimental", "show_dgp()")

  if (!is.na(layer_index(plot, "dgp_axis"))) {
    abort(c(
      "This plot already has a data generating process drawn on it",
      "`show_dgp()` raises the count axis to make room, so a second one would raise it again"
    ))
  }
  spec <- overlay_spec(plot, "show_dgp")

  if (!identical(spec$y_transform$name, "identity")) {
    abort(c(
      "`show_dgp()` needs an untransformed count axis",
      glue("this plot's y scale applies a \"{spec$y_transform$name}\" transformation"),
      paste(
        "the sample estimate band is drawn in the margin below the panel, which has",
        "no value under a transformed scale"
      ),
      "`show_mean()` remains supported under a transformed axis"
    ))
  }

  y_scale <- plot$scales$get_scales("y")
  y_limits <- y_scale$limits
  # a free top (limits = c(0, NA), of any type -- including c(NA, NA), whose
  # top is logical NA rather than numeric) is exactly what expand_limits()
  # can still raise; only a fully-specified top actually pins the axis. A
  # function-valued limits = is treated the same conservative way it always
  # was: as pinning the axis, since there is no way to tell in advance
  # whether it leaves the top free
  top_free <- is.null(y_limits) ||
    (!is.function(y_limits) && length(y_limits) >= 2 && is.na(y_limits[[2]]))
  y_fixed <- !is.null(y_scale) && !top_free
  if (y_fixed) {
    limits_line <- if (is.function(y_limits)) {
      "its y scale sets its limits with a function"
    } else {
      glue("its y scale sets limits = c({collapse(format(y_limits, trim = TRUE))})")
    }
    abort(c(
      "`show_dgp()` needs to raise the count axis, and this plot's count axis is fixed",
      limits_line,
      "drop the limits and set the axis height with `%>% gf_refine(expand_limits(y = ))`"
    ))
  }

  coord_y <- plot$coordinates$limits$y
  if (!is.null(coord_y) && any(!is.na(coord_y))) {
    abort(c(
      "`show_dgp()` needs to raise the count axis, and this plot's coordinate y range is fixed",
      "*" = "drop `coord_cartesian(ylim = )`, or set a minimum height with `%>% gf_refine(expand_limits(y = ))`",
      "*" = "an x-only coordinate zoom is supported and is preserved"
    ))
  }

  if (isTRUE(plot$facet$params$free$y)) {
    abort(c(
      "`show_dgp()` needs a shared count axis across panels",
      "*" = "drop `scales = \"free_y\"` (or `\"free\"`) so every panel shares one count axis",
      "*" = "`show_mean()` does not need one and is unaffected"
    ))
  }

  top <- spec$count_top
  band <- max(3, 0.25 * top)
  axis_y <- top + band * 0.40
  mark_zero <- spec$x_limits[[1]] <= 0 && 0 <= spec$x_limits[[2]]

  plot <- plot +
    tag_layer(ggplot2::geom_blank(), "dgp_headroom") +
    tag_layer(ggplot2::annotate(
      "segment", x = -Inf, xend = Inf, y = axis_y, yend = axis_y,
      color = color, linewidth = 0.5
    ), "dgp_axis") +
    tag_layer(ggplot2::annotate(
      "text", x = -Inf, y = top + band * 0.98, label = "Population Parameter (DGP)",
      hjust = -0.01, vjust = 0, size = 4, fontface = "bold", color = color
    ), "dgp_title") +
    tag_layer(ggplot2::annotate(
      "text", x = -Inf, y = top + band * 0.70,
      label = "Y[i] == beta[0] + beta[1] * X[i] + epsilon[i]", parse = TRUE,
      hjust = -0.01, vjust = 0.5, size = 4, fontface = "bold", color = color
    ), "dgp_population_equation")

  if (mark_zero) {
    plot <- plot +
      tag_layer(ggplot2::annotate(
        "point", x = 0, y = axis_y + band * 0.16, shape = 25, size = size,
        color = null_color, fill = null_color
      ), "dgp_null_marker") +
      tag_layer(ggplot2::annotate(
        "text", x = 0, y = axis_y + band * 0.48, label = "beta[1] == 0", parse = TRUE,
        size = 5, fontface = "bold", color = null_color
      ), "dgp_null_label")
  }

  plot <- plot +
    tag_layer(ggplot2::annotate(
      "text", x = -Inf, y = -Inf, label = "Parameter Estimate",
      hjust = -0.01, vjust = 3.2, size = 4, fontface = "bold", color = color
    ), "dgp_estimate_title") +
    tag_layer(ggplot2::annotate(
      "text", x = -Inf, y = -Inf, label = "Y[i] == b[0] + b[1] * X[i] + e[i]", parse = TRUE,
      hjust = -0.01, vjust = 4.0, size = 4, fontface = "bold", color = color
    ), "dgp_estimate_equation")

  if (mark_zero) {
    plot <- plot + tag_layer(ggplot2::annotate(
      "text", x = 0, y = -Inf, vjust = 2.5, label = "b[1]", parse = TRUE,
      size = 5, fontface = "bold", color = color
    ), "dgp_estimate_marker")
  }

  # ggproto() re-evaluates the parent expression later, so the old coord needs its
  # own name. The band makes room in the panel rather than in the scale: an
  # expanded limit is a count the axis would then put a tick on.
  coord <- plot$coordinates
  plot$coordinates <- ggplot2::ggproto(NULL, coord, clip = "off",
    limits = list(x = coord$limits$x, y = c(NA, top + band + 1.0)))

  # the bottom band lives in the margin, and the x title is replaced by "Parameter Estimate"
  plot +
    ggplot2::labs(x = "") +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(color = color),
      axis.line.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(color = color),
      axis.title.x = ggplot2::element_text(color = color),
      plot.margin = ggplot2::margin(5, 5, 30, 5)
    )
}
