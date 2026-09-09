#' Read what a one-variable distribution helper needs from a plot
#'
#' This reader owns only static distribution validation. It does not build the
#' plot or read panel ranges: stats, scales, coordinates, and guides own those
#' lifecycle stages now.
#'
#' @param plot A ggplot object.
#' @param fn The calling function's name, for error messages.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with the resolved x mapping, its values and scale, and the
#'   underlying `plot_spec()`.
#'
#' @noRd
distribution_plot_spec <- function(plot, fn, call = caller_env()) {
  if (!inherits(plot, "ggplot")) {
    abort(glue("`{fn}()` needs a ggplot object"), call = call)
  }
  spec <- plot_spec(plot)

  if (!inherits(plot$coordinates, "CoordCartesian")) {
    abort(
      c(
        glue("`{fn}()` needs a plot with cartesian x and y axes"),
        glue("this plot uses {class(plot$coordinates)[[1]]}")
      ),
      call = call
    )
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

  y_scale <- plot$scales$get_scales("y")
  if (!is.null(y_scale) && isTRUE(y_scale$is_discrete())) {
    abort(
      c(
        glue("`{fn}()` needs a plot with a numeric count axis"),
        "this plot's y scale is discrete and has no count to mark or span"
      ),
      call = call
    )
  }

  list(
    values = values, label = x$label, data = x$data, quo = x$quo,
    plot = spec, x_scale = plot$scales$get_scales("x")
  )
}

#' Require a distribution to come from one data column
#'
#' Means can follow an expression through ggplot2's stat lifecycle. Empirical
#' cutoffs instead identify observations in the original column, so their
#' high-level helper deliberately requires a bare column mapping.
#'
#' @param spec A `distribution_plot_spec()` list.
#' @param fn The calling function's name, for error messages.
#' @param call The calling environment, for error reporting.
#'
#' @return The mapped column name.
#'
#' @noRd
distribution_plot_column <- function(spec, fn, call = caller_env()) {
  if (!is_symbol(quo_get_expr(spec$quo))) {
    abort(
      c(
        glue("`{fn}()` needs the plot's x aesthetic to be a single variable"),
        glue("found: {deparse1(quo_get_expr(spec$quo))}"),
        "compute the variable first, then plot it"
      ),
      call = call
    )
  }

  column <- as_name(spec$quo)
  if (!column %in% names(spec$data)) {
    abort(glue("Can't find `{spec$label}` in the plot's data"), call = call)
  }
  column
}

#' Require a continuous distribution position scale
#'
#' @param spec A `distribution_plot_spec()` list.
#' @param fn The calling function's name, for error messages.
#' @param call The calling environment, for error reporting.
#'
#' @return `NULL`, invisibly. Called for its refusal.
#'
#' @noRd
check_distribution_x_scale <- function(spec, fn, call = caller_env()) {
  if (!is.null(spec$x_scale) && isTRUE(spec$x_scale$is_discrete())) {
    abort(glue("`{fn}()` needs a continuous x position scale"), call = call)
  }
  invisible(NULL)
}

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
#' @param object A plot of one distribution.
#' @param color Line color. Default `"#E60000"`.
#' @param linetype Line type. Default `"longdash"`.
#' @param linewidth Line width. Default `0.7`.
#' @param plot Deprecated alias for `object`.
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
show_mean <- function(object = NULL, color = "#E60000", linetype = "longdash",
                      linewidth = 0.7, plot = lifecycle::deprecated()) {
  lifecycle::signal_stage("experimental", "show_mean()")
  object <- normalize_plot_argument(
    object, plot, missing(object), missing(plot), "show_mean"
  )
  spec <- distribution_plot_spec(object, "show_mean")

  # x is mapped from the distribution's own quosure, not a precomputed value,
  # so StatDistMean sees exactly the rows ggplot2 assigned to each panel and
  # takes their mean there -- see StatDistMean's own doc for why that is not
  # the same thing as aggregating by hand before the layer is built
  mapping <- ggplot2::aes()
  mapping$x <- spec$quo

  object + tag_layer(
    stat_dist_mean(
      data = spec$data, mapping = mapping, inherit.aes = FALSE,
      show.legend = FALSE, colour = color, linetype = linetype,
      linewidth = linewidth, na.rm = TRUE
    ),
    "distribution_mean"
  )
}

#' Frame a Sampling Distribution With Its Data Generating Process
#'
#' `r lifecycle::badge("experimental")`
#'
#' Frames a distribution of estimates with the process that generated them: the
#' population model on a top axis labeled "Population Parameter (DGP)", the
#' sample estimate below the plot, and a marker at the null hypothesis
#' (\eqn{\beta_1 = 0}) on both -- drawn only when zero is on the axis.
#'
#' The population and estimate frames are position guides, outside the data
#' panel. They therefore do not change the count range and remain compatible
#' with fixed, zoomed, transformed, and free count axes. The ordinary numeric x
#' guide stays in place as the estimate scale. Zero is omitted when it is not a
#' finite visible value on that scale; it is never moved to a boundary.
#'
#' @param object A plot of one distribution of estimates.
#' @param color Color of the axes, equations and titles. Default `"#003d70"`.
#' @param null_color Color of the null hypothesis marker. Default `"#E60000"`.
#' @param size Size of the null hypothesis marker. Default `4`.
#' @param plot Deprecated alias for `object`.
#'
#' @return The plot, with population and estimate guides added.
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
#' # side; show_dgp() does not alter that range
#' gf_histogram(~b1, data = shuffled, binwidth = 2) %>%
#'   gf_refine(ggplot2::expand_limits(y = 10)) %>%
#'   show_mean() %>%
#'   show_dgp()
show_dgp <- function(object = NULL, color = "#003d70", null_color = "#E60000",
                     size = 4, plot = lifecycle::deprecated()) {
  lifecycle::signal_stage("experimental", "show_dgp()")
  object <- normalize_plot_argument(
    object, plot, missing(object), missing(plot), "show_dgp"
  )
  spec <- distribution_plot_spec(object, "show_dgp")

  if (inherits(object$coordinates, "CoordFlip")) {
    abort(c(
      "`show_dgp()` needs an upright cartesian plot",
      "*" = "its guides describe a horizontal parameter axis above a vertical count axis"
    ))
  }
  check_distribution_x_scale(spec, "show_dgp")
  x_scale <- spec$x_scale
  guide_sources <- c(
    position_guide_matches(x_scale$guide %||% ggplot2::waiver(), "GuideDgp"),
    if (is.null(x_scale)) list() else {
      position_guide_matches(x_scale$secondary.axis, "GuideDgp")
    }
  )
  if (length(guide_sources) > 0L) {
    abort("This plot already has a data generating process drawn on it")
  }
  for (name in names(object$guides$guides)) {
    if (length(position_guide_matches(object$guides$guides[[name]], "GuideDgp")) > 0L) {
      abort("This plot already has a data generating process drawn on it")
    }
  }

  population <- dgp_upright_guide(guide_dgp(
    value = 0, role = "population", colour = null_color,
    size = size, linewidth = 0.5, theme = dgp_null_theme()
  ))
  estimate <- dgp_upright_guide(guide_dgp(
    value = 0, role = "estimate", colour = color, theme = dgp_null_theme()
  ))

  out <- add_dgp_position_guides(object, estimate, population)
  out +
    ggplot2::labs(x = "") +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(color = color),
      axis.line.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(color = color),
      axis.title.x = ggplot2::element_text(color = color)
    )
}
