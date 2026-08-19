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

#' Recover the formula from a plot that maps an axis on its first layer
#'
#' A layer inherits the plot's mapping, never a sibling layer's, so an axis
#' written on the first layer rather than the plot is invisible to the
#' ruler's own layer. The ruler then measures only what it did inherit --
#' nothing, if neither axis was named at plot level, or the wrong axis
#' drawn horizontally along zero, if only one was. Merging the aesthetics
#' the plot does not already name is what `plot_spec()` does for every
#' other reader of a plot.
#'
#' @param object A ggplot object.
#'
#' @return A list with `gformula` and `data`, or `NULL` when the plot already
#'   names every axis its first layer does and there is nothing to recover.
#'
#' @noRd
sd_ruler_inherited <- function(object) {
  spec <- plot_spec(object)
  mapping <- spec$mapping
  # "the plot itself names an aesthetic" has to mean the same thing on both
  # sides of this comparison: `mapping`'s names are pin-aware (a promoted pin
  # is written straight into `object$mapping`, per `pin_plot_values()`), so
  # `object$mapping`'s names, read directly, already agree -- go through
  # `spec$pins` anyway rather than reach past `plot_spec()` into the raw
  # object, so the two can never drift apart again
  own <- union(names(object$mapping), names(spec$pins))
  recovered <- setdiff(intersect(c("x", "y"), names(mapping)), own)
  if (length(recovered) == 0 || is.null(mapping$x)) {
    return(NULL)
  }

  # a first layer with its own data is drawing that data, so that is what the
  # ruler has to measure; plot_spec()'s own `data` prefers the plot's
  data <- object$layers[[1]]$data
  if (!is.data.frame(data)) data <- object$data

  list(
    gformula = new_formula(
      if (is.null(mapping$y)) NULL else quo_get_expr(mapping$y),
      quo_get_expr(mapping$x)
    ),
    data = data
  )
}

#' Measure one standard deviation of the outcome, anchored at its mean
#'
#' Reduces a panel to the single segment a standard deviation ruler draws. The
#' outcome is whichever axis carries it: with a `y` aesthetic the ruler is
#' vertical and `where` places it along x; without one the outcome is on x and
#' the ruler runs along the baseline from the mean. Both are measured in the
#' space the panel is drawn in, so a facet measures its own subset and a
#' transformed axis measures the transformed values.
#'
#' @format A [ggplot2::Stat] object.
#'
#' @seealso [gf_sd_ruler()], which pairs this stat with a segment for you.
#' @export
StatSdRuler <- ggplot2::ggproto(
  "StatSdRuler", ggplot2::Stat,
  required_aes = "x",
  dropped_aes = c("y", "weight"),
  setup_params = function(data, params) {
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

#' Build the ruler's layer, tagged, reading `size` as `linewidth`
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
sd_ruler_layer <- function(geom, stat, position, params, mapping = NULL,
                           data = NULL, ...) {
  if (!is.null(params$size)) {
    warn(c(
      "`size` is now `linewidth` in `gf_sd_ruler()`",
      glue("write `linewidth = {deparse(params$size)}` instead")
    ))
    params$linewidth <- params$linewidth %||% params$size
    params$size <- NULL
  }

  mapped <- setdiff(names(mapping), c("x", "y"))
  if (length(mapped) > 0) {
    abort(c(
      glue("gf_sd_ruler() draws one ruler per panel, so {collapse(mapped)} can't be mapped"),
      "give it a value instead, or split the plot with `y ~ x | group` to get one per group"
    ))
  }

  params$colour <- params$colour %||% params$color %||% "red"
  params$linewidth <- params$linewidth %||% 0.8
  params$color <- NULL

  tag_layer(
    ggplot2::layer(
      geom = geom, stat = stat, position = position, params = params,
      mapping = mapping, data = data, ...
    ),
    "sd_ruler"
  )
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
#' @param ... Additional arguments: `color` (default `"red"`),
#'   `linewidth` (default `0.8`), and any other [ggplot2::geom_segment()]
#'   parameter.
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
#' @seealso
#' The model visualization guide shows the ruler alongside residuals and
#' compares groups with different spread:
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
gf_sd_ruler <- ggformula::layer_factory(
  geom = "segment",
  # A bare ggproto symbol here only resolves through the search path -- see the
  # matching note above `gf_squareplot`'s `layer_factory()` call, including why
  # this needs ggformula 0.12.0 -- so it is package-qualified instead, which `::`
  # resolves the same regardless of whether `coursekata` is attached.
  stat = coursekata::StatSdRuler,
  position = "identity",
  aes_form = list(NULL, ~x, y ~ x),
  extras = alist(where = "middle", na.rm = TRUE),
  pre = {
    lifecycle::signal_stage("experimental", "gf_sd_ruler()")
    coursekata:::check_ruler_where(where)
    if (!missing(object) && inherits(object, "ggplot") && missing(gformula)) {
      inherited <- coursekata:::sd_ruler_inherited(object)
      if (!is.null(inherited)) {
        gformula <- inherited$gformula
        if (missing(data)) data <- inherited$data
      }
    }
  },
  layer_fun = sd_ruler_layer
)
