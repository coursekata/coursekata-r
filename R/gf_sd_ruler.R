#' Refuse a placement rule the ruler does not have
#'
#' @param where A single string.
#' @param call The calling environment, for error reporting.
#'
#' @return `where`, invisibly.
#'
#' @noRd
check_ruler_where <- function(where, call = caller_env()) {
  choices <- c("middle", "mean", "median")
  if (!(is.character(where) && length(where) == 1L && where %in% choices)) {
    abort(
      c(
        glue('`where` must be one of "middle", "mean", or "median"'),
        glue("found: {deparse(where)}")
      ),
      call = call
    )
  }
  invisible(where)
}

#' Compute a panel's standard deviation ruler
#'
#' `StatSdRuler` reduces each panel to the segment for one standard deviation,
#' anchored at the outcome's mean. With `x` and `y` aesthetics, the outcome is
#' `y`; the ruler is vertical and `where` places it along `x`. With `x` alone,
#' the outcome is `x` and the ruler runs along the baseline from the mean to
#' one standard deviation above it.
#'
#' `stat_sd_ruler()` is the conventional layer constructor. It uses the same
#' stat as [gf_sd_ruler()], so both interfaces measure transformed values and
#' compute one ruler from each panel's rows. Groups within a panel do not get
#' separate rulers. Styling aesthetics from the source data cannot be mapped;
#' set them to one value, or facet the plot to draw one ruler per group.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object used to display the data. Defaults to
#'   `"segment"`.
#' @param position A position adjustment. Defaults to `"identity"`.
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @param where With both `x` and `y` mapped, where to place the ruler along
#'   `x`: `"middle"`, `"mean"`, or `"median"`. Ignored when only `x` is
#'   mapped.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, override the default aesthetics rather than
#'   combining with them.
#'
#' @return `stat_sd_ruler()` returns a ggplot2 layer.
#'
#' @format A [ggplot2::Stat] object.
#'
#' @seealso [gf_sd_ruler()] provides the ggformula interface.
#' @export
#' @examples
#' ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
#'   ggplot2::geom_point() +
#'   stat_sd_ruler(where = "mean", colour = "red", linewidth = 1)
#'
#' ggplot2::ggplot(Fingers, ggplot2::aes(Thumb)) +
#'   ggplot2::geom_histogram(bins = 30) +
#'   stat_sd_ruler(colour = "red", linewidth = 1)
StatSdRuler <- ggplot2::ggproto(
  "StatSdRuler", ggplot2::Stat,
  required_aes = "x",
  non_missing_aes = "y",
  setup_params = function(data, params) {
    check_panel_stat_aesthetics(data, "stat_sd_ruler", c("x", "y"))
    check_ruler_where(params$where %||% "middle")
    params
  },
  # a categorical outcome arrives as integer positions, so the guard needs the
  # scale; an error raised in compute_panel is downgraded to a warning and the
  # layer silently draws nothing, an error raised here is not
  compute_layer = function(self, data, params, layout) {
    scales <- layout$get_scales(data$PANEL[[1]])
    outcome <- if ("y" %in% names(data)) "y" else "x"
    if (!is.null(scales[[outcome]]) && scales[[outcome]]$is_discrete()) {
      abort(c(
        glue("The plot's {outcome} variable is categorical"),
        "a standard deviation ruler needs a quantitative outcome"
      ))
    }
    ggplot2::ggproto_parent(ggplot2::Stat, self)$compute_layer(data, params, layout)
  },
  # one ruler per panel, so this is compute_panel and not compute_group: the
  # groups a discrete x creates are what `where` measures across
  compute_panel = function(data, scales, where = "middle") {
    if ("y" %in% names(data)) {
      m <- mean(data$y, na.rm = TRUE)
      s <- stats::sd(data$y, na.rm = TRUE)
      at <- switch(where,
        middle = (min(data$x, na.rm = TRUE) + max(data$x, na.rm = TRUE)) / 2,
        mean = mean(data$x, na.rm = TRUE),
        median = stats::median(data$x, na.rm = TRUE)
      )
      return(data.frame(x = at, xend = at, y = m, yend = m + s))
    }
    m <- mean(data$x, na.rm = TRUE)
    s <- stats::sd(data$x, na.rm = TRUE)
    data.frame(x = m, xend = m + s, y = 0, yend = 0)
  }
)

#' @rdname StatSdRuler
#' @export
stat_sd_ruler <- function(mapping = NULL, data = NULL, geom = "segment",
                          position = "identity", ..., where = "middle",
                          na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE) {
  sd_ruler_layer(
    stat = StatSdRuler, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(where = where, na.rm = na.rm, ...)
  )
}

#' Build a standard deviation ruler layer
#'
#' @param geom,stat,position,params,mapping,data,... Passed to
#'   [ggplot2::layer()].
#'
#' @return A ggplot2 layer.
#' @noRd
sd_ruler_layer <- function(geom, stat, position, params, mapping = NULL,
                           data = NULL, ..., .fn = "stat_sd_ruler") {
  rlang::local_error_call(call2(.fn))
  params <- normalize_linewidth(params, geom, .fn)
  layer <- ggplot2::layer(
    geom = geom, stat = stat, position = position, params = params,
    mapping = mapping, data = data, ...
  )
  public_layer_constructor(layer, .fn)
}

#' Adapt the shared ruler layer to the ggformula front door
#'
#' `layer_factory()` reads its extras out of `match.call()`, so `pre` cannot
#' rename one argument into another -- the rename has to happen where the params
#' are assembled. This is also the only place a generated layer can be tagged.
#'
#' @param geom,stat,position,params,mapping,data,... Passed to [ggplot2::layer()].
#'
#' @return A tagged ggplot2 layer.
#'
#' @noRd
gf_sd_ruler_layer <- function(geom, stat, position, params, mapping = NULL,
                              data = NULL, ...) {
  params <- normalize_linewidth(params, geom, "gf_sd_ruler")

  check_panel_stat_aesthetics(mapping, "gf_sd_ruler", c("x", "y"))

  source_layer(tag_layer(
    sd_ruler_layer(
      geom = geom, stat = stat, position = position, params = params,
      mapping = mapping, data = data, .fn = "gf_sd_ruler", ...
    ),
    "sd_ruler"
  ), inherit.data = is.null(data))
}

#' Add a Standard Deviation Ruler to a Plot
#'
#' `r lifecycle::badge("experimental")`
#'
#' Adds a segment showing one standard deviation of the outcome, anchored at
#' its mean. The orientation depends on where the outcome variable lives: on
#' a scatter or jitter plot (outcome on the y-axis) the ruler is a vertical
#' segment placed at a chosen x position; on a histogram (outcome on the
#' x-axis, no y aesthetic) it is a horizontal segment running from the mean to
#' mean + SD along the baseline. The orientation is detected automatically
#' from the plot's axis mappings.
#'
#' Both the outcome and, where relevant, the placement are measured in the
#' space the panel is drawn in: a faceted plot measures each panel's own
#' subset, and a transformed axis or a computed mapping such as
#' `~log(Thumb)` is measured in the transformed or computed values, not the
#' raw column.
#'
#' `gf_sd_ruler()` draws one ruler per panel, so an aesthetic mapped on the
#' call -- `gf_sd_ruler(color = ~Sex)` -- is refused; split the plot instead
#' with `y ~ x | group` to get one ruler per group.
#'
#' @param object The plot or data to add the ruler to; typically a plot
#'   piped in from `gf_point()`, `gf_jitter()`, or `gf_histogram()`.
#' @param gformula A formula naming the outcome and, optionally, the x
#'   variable: `y ~ x`. Defaults to the plot's own mapping when the plot
#'   already names one.
#' @param data Dataset. Defaults to the plot's data.
#' @param where For a vertical ruler, where on the x-axis to place it:
#'   `"middle"` (midpoint of x range), `"mean"`, or `"median"`. Ignored for
#'   a horizontal ruler, which always starts at the mean.
#' @param na.rm Should missing values be silently removed?
#' @param ... Additional arguments accepted by [ggplot2::geom_segment()],
#'   including fixed `color` and `linewidth` values. Defaults come from the
#'   segment geom, as they do for [stat_sd_ruler()].
#' @param xlab,ylab,title,subtitle,caption Axis and plot labels; see
#'   [ggformula::gf_point()].
#' @param geom,stat,position Layer components; see [ggformula::gf_point()].
#' @param show.legend Should this layer be included in the legends?
#' @param show.help If `TRUE`, display some minimal help.
#' @param inherit A logical indicating whether default attributes are
#'   inherited from a parent plot.
#' @param environment An environment in which to evaluate the formula.
#'
#' @return A ggplot object with the SD ruler segment added.
#'
#' @export
#' @seealso [stat_sd_ruler()] provides the ggplot2 interface. The model
#' visualization guide shows the ruler alongside residuals and compares groups
#' with different spread:
#' <https://coursekata.github.io/coursekata-r/articles/model-visualization.html>
#'
#' @examples
#' # the ruler runs from the mean (the empty model) up by one standard
#' # deviation -- it looks like a residual because SD is a typical residual
#' gf_point(Thumb ~ Height, data = Fingers, alpha = .4) %>%
#'   gf_model(lm(Thumb ~ NULL, data = Fingers)) %>%
#'   gf_sd_ruler()
#'
#' # `where` controls placement along the x-axis
#' gf_point(Thumb ~ Height, data = Fingers, alpha = .4) %>%
#'   gf_sd_ruler(where = "mean")
#'
#' # categorical x works the same way
#' gf_jitter(Thumb ~ Sex, data = Fingers, width = .1, alpha = .4) %>%
#'   gf_sd_ruler(where = "median")
#'
#' # on a histogram the outcome is on the x-axis, so the ruler is horizontal
#' # and runs along the baseline from the mean to one SD above it
#' gf_histogram(~Thumb, data = Fingers, binwidth = 5) %>%
#'   gf_sd_ruler(linewidth = 2)
#'
#' # name the variable explicitly when the plot does not make it obvious
#' gf_point(Thumb ~ Height, data = Fingers, alpha = .4) %>%
#'   gf_sd_ruler(Thumb ~ Height)
#'
#' # one ruler per panel
#' gf_sd_ruler(Thumb ~ Height | Sex, data = Fingers)
gf_sd_ruler <- named_layer_factory(
  function_name = "gf_sd_ruler",
  geom = "segment",
  # A bare ggproto symbol here only resolves through the search path -- see the
  # matching note above `gf_squareplot`'s `layer_factory()` call. Here `::`
  # resolves the same regardless of whether `coursekata` is attached.
  stat = coursekata::StatSdRuler,
  position = "identity",
  aes_form = list(NULL, ~x, y ~ x),
  extras = alist(where = "middle", na.rm = FALSE),
  .pre_bindings = alist(
    check_ruler_where = check_ruler_where
  ),
  pre = {
    lifecycle::signal_stage("experimental", "gf_sd_ruler()")
    check_ruler_where(where)
  },
  layer_fun = gf_sd_ruler_layer
)
