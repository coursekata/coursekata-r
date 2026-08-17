#' Choose the whole-number ticks a count axis wants
#'
#' @param max_count The largest count the scale was trained on.
#'
#' @return A numeric vector of breaks.
#'
#' @noRd
count_breaks <- function(max_count) {
  if (!is.finite(max_count) || max_count <= 0) {
    return(0)
  }
  step <- if (max_count <= 10) {
    1
  } else if (max_count <= 20) {
    2
  } else if (max_count <= 50) {
    5
  } else if (max_count <= 100) {
    10
  } else {
    ceiling(max_count / 10)
  }
  seq(0, max_count, by = step)
}

#' A y scale whose breaks are whole counts
#'
#' The rule is applied to the trained count, not to the panel range: a `breaks`
#' *function* is handed the expanded limit -- 10.5 for a maximum of 10 -- and the
#' 1/2/5/10 step rule tips from step 1 to step 2, quietly turning a countable axis
#' into every-other-count.
#'
#' @return A ggplot2 scale.
#'
#' @noRd
scale_y_count <- function() {
  # ggproto() captures `_inherit` lazily and re-evaluates it, so the object being
  # extended has to be bound to a name first or it parents itself
  base <- ggplot2::scale_y_continuous()
  ggplot2::ggproto(NULL, base,
    count_top = NULL,
    train = function(self, x) {
      # a count is a whole number, so the whole numbers this scale is trained on
      # are where the counts reach; an overlay drawn above them is not a count
      whole <- x[is.finite(x) & x >= 0 & x == round(x)]
      if (length(whole) > 0) self$count_top <- max(self$count_top %||% 0, whole)
      ggplot2::ggproto_parent(base, self)$train(x)
    },
    get_breaks = function(self, limits = self$get_limits()) {
      count_breaks(min(self$get_limits()[[2]], self$count_top %||% Inf))
    }
  )
}

#' Refuse a y scale a square cannot be one count tall on
#'
#' A square is one count tall, so the count axis has to be something a count is
#' a position on. A discrete y is not: its values are categories, and there is
#' no count for a square to be one of.
#'
#' A transformed y is a different matter, and the distinction is where the
#' transformation is applied rather than whether one is allowed. A scale
#' transform runs over the data before the bins are counted, so the stack the
#' geom builds afterwards is written in the transformed space and lands beside
#' its own axis labels. A coord transform runs at draw time over squares that
#' are already correct, which is what a reader means by a transformed count
#' axis: the squares thin as they climb, showing the unit changing as the scale
#' rises. So the scale spelling is redirected to the coord spelling rather than
#' refused as impossible.
#'
#' @param scale A y-position scale.
#' @param call The environment used for error reporting.
#'
#' @return Invisible `NULL`, or an abort.
#'
#' @noRd
squareplot_check_y_scale <- function(scale, call = caller_env()) {
  # With no explicit y scale, layout has no y object yet at Stat time; ggplot2
  # will install its identity-continuous default after the Stat produces y.
  if (is.null(scale)) return(invisible(NULL))
  if (scale$is_discrete()) {
    abort(c(
      "`gf_squareplot()` needs a continuous count axis",
      "*" = paste(
        "each square is exactly one count tall, and a discrete y axis has no",
        "count to be one of"
      ),
      "*" = "remove the discrete y scale"
    ), call = call)
  }
  transform <- scale$get_transformation()
  if (is.null(transform) || !identical(transform$name, "identity")) {
    named <- !is.null(transform)
    what <- if (named) glue('a "{transform$name}" transformation') else "a transformation"
    instead <- if (named) transform$name else "sqrt"
    abort(c(
      glue("`gf_squareplot()` cannot count squares on a y scale that applies {what}"),
      "*" = paste(
        "a scale transforms the counts before the squares are built, so the",
        "squares would be drawn in one space and labelled in another"
      ),
      "*" = glue(
        "to draw the same distortion at render, transform the coordinate ",
        'instead: `%>% gf_refine(coord_transform(y = "{instead}"))`'
      ),
      "*" = "each square still spans one count, so the stack thins as it climbs"
    ), call = call)
  }
  invisible(NULL)
}

#' Decide which default scales a squareplot is allowed to add
#'
#' @param object The object supplied to the generated ggformula function.
#'
#' @return A list with logical `add_x` and `add_y` fields.
#'
#' @noRd
squareplot_scale_plan <- function(object) {
  if (!inherits(object, c("gg", "ggplot"))) {
    return(list(add_x = TRUE, add_y = TRUE))
  }
  x <- object$scales$get_scales("x")
  y <- object$scales$get_scales("y")
  if (!is.null(y)) squareplot_check_y_scale(y)
  list(add_x = is.null(x), add_y = is.null(y))
}

#' Resolve the x values a squareplot layer is about to draw
#'
#' A layer's `mapping`/`data` are `NULL` whenever it inherits them from the
#' plot -- measured, for `plot %>% gf_squareplot()` both arrive `NULL` -- so
#' each slot falls back to the plot's own mapping/data before evaluating `x`.
#' This is the one predicate that has to see a discrete x correctly: a scale
#' that misses it merely keeps a default, but a stat that misses it tries to
#' bin a factor's integer codes.
#'
#' @param mapping,data The layer's own mapping and data, as `squareplot_layer()`
#'   receives them.
#' @param object The object supplied to the generated ggformula function.
#'
#' @return The evaluated x values, or `NULL` when they cannot be resolved.
#'
#' @noRd
squareplot_x_values <- function(mapping, data, object) {
  plot <- inherits(object, c("gg", "ggplot"))
  x <- mapping$x %||% (if (plot) object$mapping$x)
  values_from <- data %||% (if (plot) object$data)
  if (is.null(x) || is.null(values_from)) {
    return(NULL)
  }
  tryCatch(eval_tidy(x, values_from), error = function(e) NULL)
}

#' Warn that a binning argument cannot reach a counted x
#'
#' `binwidth` is in `gf_squareplot()`'s own signature and the rest of the
#' binning vocabulary is in its own Rd, so a caller who supplies one alongside
#' a discrete x is advertised an effect that a counted x cannot honor. The
#' plot drawn is still the right plot -- only the override is lost -- so this
#' warns rather than aborts.
#'
#' @param params The layer's params, after ggformula's own extras are merged in.
#'
#' @return Invisible `NULL`, or a warning.
#'
#' @noRd
squareplot_warn_binning <- function(params) {
  binning <- setdiff(ggplot2::StatBin$parameters(TRUE), ggplot2::StatCount$parameters(TRUE))
  present <- intersect(names(params), binning)
  if (length(present) == 0) {
    return(invisible(NULL))
  }
  warn(c(
    "a discrete x is counted, not binned",
    "*" = glue("no effect here: {collapse(paste0('`', present, '`'))}")
  ), class = "coursekata_squareplot_binning")
  invisible(NULL)
}

#' Build the squareplot layer together with defaults it is allowed to own
#'
#' `layer_factory()` composes whatever the returned closure produces with `+`.
#' The closure captures scale ownership before the layer is assembled: defaults
#' are added only for axes the caller has not already configured. The x values
#' are resolved once, here, and drive both which stat draws the layer and
#' whether the x scale keeps unobserved factor levels -- one predicate for both,
#' rather than two that can disagree about what "discrete" means.
#'
#' @param object The object supplied to the generated ggformula function. Also
#'   the input to `squareplot_scale_plan()`, computed here so the call-time
#'   y-scale refusal fires at the same moment it always has.
#'
#' @return A layer function for [ggformula::layer_factory()].
#'
#' @noRd
squareplot_layer <- function(object) {
  force(object)
  scale_plan <- squareplot_scale_plan(object)
  function(geom, stat, position, params, mapping = NULL, data = NULL, ...) {
    # `colour` is the separator between two squares, and it may be mapped like
    # any other aesthetic; `bar_color` is the bar's own. ggformula hands
    # `color` and `colour` through verbatim, so fold the American spelling into
    # the one the geom uses, and default the separators to white only when
    # nothing is mapped to them.
    params$colour <- params$colour %||% params$color
    params$color <- NULL
    if (is.null(mapping$colour)) params$colour <- params$colour %||% "white"
    if (is.null(mapping$fill)) params$fill <- params$fill %||% "#7fcecc"

    values <- squareplot_x_values(mapping, data, object)
    discrete <- is.factor(values) || is.character(values) || is.logical(values)
    if (discrete) {
      stat <- StatSquareplotCount
      squareplot_warn_binning(params)
    }

    parts <- list(ggplot2::layer(
      geom = geom, stat = stat, position = position, params = params,
      mapping = mapping, data = data, ...
    ))
    if (scale_plan$add_y) parts <- c(parts, list(scale_y_count()))

    # Retain unobserved factor levels only when this new plot owns its x scale.
    # An existing scale's order, limits and drop policy belong to the caller.
    if (scale_plan$add_x && discrete) {
      parts <- c(parts, list(ggplot2::scale_x_discrete(drop = FALSE)))
    }
    parts
  }
}

#' Refuse at call time what the layer would discard in silence
#'
#' Runs inside `layer_factory()`'s `pre`, which is evaluated before the help gate,
#' so every argument it reads has to have a real default rather than be missing.
#'
#' @param object,gformula The first two formals of the generated function. The
#'   formula arrives as `object` when it is passed positionally, which is how all
#'   but a handful of calls spell it.
#' @param na.rm The `na.rm` extra.
#' @param call The calling environment, for error reporting.
#'
#' @return Invisible `NULL`, or an abort.
#'
#' @noRd
squareplot_check <- function(object, gformula, na.rm, call = caller_env()) {
  if (!isTRUE(na.rm)) {
    abort(
      c(
        "`na.rm = FALSE` is not supported",
        "*" = "gf_squareplot() draws one square per observation, and a missing value has none",
        "*" = "drop the missing values before plotting, or leave `na.rm = TRUE`"
      ),
      call = call
    )
  }

  frm <- if (inherits(object, "formula")) object else gformula
  if (inherits(frm, "formula") && length(frm) == 3L) {
    abort(
      c(
        "`gf_squareplot()` takes a one-sided formula",
        "*" = glue("found: {deparse(frm)}"),
        "*" = "write it as `~variable`"
      ),
      call = call
    )
  }

  if (!is.null(object) && is.atomic(object)) {
    abort(
      c(
        "`gf_squareplot()` takes a formula and a data frame",
        "*" = "write it as `gf_squareplot(~variable, data = my_data)`"
      ),
      call = call
    )
  }

  invisible(NULL)
}

#' Countable-Rectangle Histogram
#'
#' `r lifecycle::badge("experimental")`
#'
#' Creates histograms where each observation is drawn as its own square, stacked
#' into columns, so a bin's height can be counted as well as read off the axis:
#' `n = 47` is 47 squares. Designed for teaching statistical concepts like
#' sampling distributions and hypothesis testing.
#'
#' @details
#' Sensible defaults are chosen based on the data:
#'
#' - For integer-valued data with a small range, the `binwidth` defaults to 1 so
#'   that each integer gets its own column. Everything else about the bins is
#'   `stat_bin()`'s: `bins`, `center`, `boundary`, `closed`, `breaks` and `pad`
#'   put a squareplot's columns exactly where a `gf_histogram()`'s bars would be.
#' - A factor keeps its levels, so a level nobody landed in still holds its place
#'   on the axis. Knowing a value never occurred is the point.
#' - A discrete x -- a factor, character or logical vector -- is counted, one
#'   column per level, positioned the way `gf_bar()` positions its bars. The
#'   binning arguments (`bins`, `binwidth`, `center`, `boundary`, `closed`,
#'   `breaks`, `pad`) belong to a continuous x; supplying one alongside a
#'   discrete x warns rather than changing the plot.
#' - The white separator between two squares is capped at a quarter of a square's
#'   smaller side, so as a bin fills and its squares shrink the separator thins
#'   with them and the squares stay countable.
#' - The y axis is a count, so its breaks are whole numbers.
#'
#' The bins are a histogram's bins: `binwidth`, `bins`, `center`, `boundary`,
#' `closed` and `breaks` mean what they mean on [ggformula::gf_histogram()], and
#' the same arguments give the same bin edges and the same counts, so squares
#' laid over bars land inside them.
#'
#' Everything that is not the squares is a layer or a scale: `%>% show_mean()`,
#' `%>% show_dgp()`, `%>% gf_lims(x = )`,
#' `%>% gf_refine(ggplot2::expand_limits(y = ))`.
#'
#' @param object A ggplot object, a data frame, or a formula. When a plot, the
#'   squares are added to it.
#' @param gformula A formula with shape `~x`, optionally faceted as `~ x | z`.
#' @param data A data frame holding the variable in `gformula`.
#' @param ... Aesthetics such as `fill` or `alpha`, either set to a value or
#'   mapped with a one-sided formula (`fill = ~group`). `color` sets the color
#'   of the separators between squares and may be mapped; `bar_color` sets the
#'   bar's own color, and `bar_linewidth` its width. Also takes
#'   `ggplot2::stat_bin()`'s `pad`, which adds an empty bin on either end of
#'   the range.
#' @param binwidth Width of the bins, for a continuous x. Chosen from the data
#'   when unset: `1` for whole-number data spanning 50 or less, so every value
#'   gets a column of its own, and a thirtieth of the range otherwise. Has no
#'   effect on a discrete x, which is counted instead.
#' @param bins How many bins to divide the range into, used when `binwidth` is
#'   unset.
#' @param center,boundary The center of one bin, or an edge of one. Either
#'   places the whole grid; give one or the other, not both.
#' @param closed Which end of a bin holds a value that lands exactly on it,
#'   `"right"` or `"left"`. For whole-number data, `boundary = 0.5` puts every
#'   value in the column it is labelled with, whichever end is closed.
#' @param breaks The bin edges themselves, which need not be evenly spaced.
#' @param bars Display style: `"none"` (squares only), `"outline"` (squares inside
#'   the bar they add up to) or `"solid"` (that bar alone).
#' @param na.rm Must be `TRUE`. A missing value has no square to draw.
#' @param xlab,ylab,title,subtitle,caption Labels.
#' @param geom,stat,position The layer's geom, stat and position.
#' @param show.legend Whether to show a legend, or `NA` to decide per aesthetic.
#' @param show.help Print the function's own help instead of drawing.
#' @param inherit Whether to inherit the plot's aesthetics.
#' @param environment Where to evaluate the formula.
#'
#' @return A ggplot object.
#'
#' @seealso [show_mean()] and [show_dgp()] annotate a distribution.
#' The sampling distributions guide shows this plot in the context of a full
#' shuffle-and-estimate workflow:
#' <https://coursekata.github.io/coursekata-r/articles/sampling-distributions.html>
#'
#' @export
#' @examples
#' # each observation is a countable square
#' gf_squareplot(~Thumb, data = Fingers)
#'
#' # `bars` controls the display: "none" (default), "outline", or "solid"
#' gf_squareplot(~Thumb, data = Fingers, bars = "outline")
#'
#' # the bins are a histogram's bins, so squares laid over bars land inside them --
#' # name the grid on both layers, because a layer never reads its neighbour's
#' gf_histogram(~Thumb, data = Fingers, bins = 8) %>% gf_squareplot(bins = 8)
#'
#' # customize fill color, binwidth, and axis limits
#' gf_squareplot(~Thumb, data = Fingers, fill = "coral", binwidth = 5) %>%
#'   gf_lims(x = c(30, 90))
#'
#' # integer data with a small range gets one column per integer
#' int_data <- data.frame(rolls = sample(1:6, 30, replace = TRUE))
#' gf_squareplot(~rolls, data = int_data)
#'
#' # the plot is a real ggformula layer, so it facets and takes mapped aesthetics
#' gf_squareplot(~ Thumb | Sex, data = Fingers)
#' gf_squareplot(~Thumb, data = Fingers, fill = ~Sex)
#'
#' # with 2000 observations the squares shrink, and their separators thin to fit
#' set.seed(24)
#' large_data <- data.frame(x = rnorm(2000, mean = 50, sd = 10))
#' gf_squareplot(~x, data = large_data)
#'
#' # show a dashed line at the sample mean
#' gf_squareplot(~Thumb, data = Fingers) %>% show_mean()
#'
#' # frame a sampling distribution with its data generating process: with only
#' # 10 shuffles, the mean of the distribution can land far from the null.
#' # The limits come before the overlays: show_dgp() reads the top of the count
#' # axis to decide how much room its band needs.
#' shuffled_b1 <- function(n) {
#'   data.frame(b1 = replicate(n, {
#'     shuffled_tip <- base::sample(TipExperiment$Tip)
#'     b1(lm(shuffled_tip ~ Condition, data = TipExperiment))
#'   }))
#' }
#'
#' set.seed(42)
#' gf_squareplot(~b1, data = shuffled_b1(10), binwidth = 2) %>%
#'   gf_lims(x = c(-30, 30)) %>%
#'   gf_refine(ggplot2::expand_limits(y = 10)) %>%
#'   show_mean() %>%
#'   show_dgp()
#'
#' # a factor keeps every level, including the ones nothing landed in
#' ratings <- data.frame(rating = factor(
#'   base::sample(1:5, 20, replace = TRUE, prob = c(1, 2, 4, 2, 1)),
#'   levels = 1:5
#' ))
#' gf_squareplot(~rating, data = ratings)
gf_squareplot <- ggformula::layer_factory(
  # `layer_factory()` captures `geom`/`stat` unevaluated and stores them as the
  # generated closure's own defaults, resolved with `rlang::eval_tidy()` at call
  # time inside that closure -- whose lexical scope is ggformula's namespace, not
  # this package's. A bare symbol only resolves there if `coursekata` is attached;
  # the package-qualified form resolves the same regardless, because `::` looks up
  # the namespace directly rather than walking the calling scope.
  #
  # Capturing unevaluated is what makes the qualified form safe, and it arrived in
  # ggformula 0.12.0. Before that these arguments are forced here, while this
  # namespace is still being built and its exports are empty, and the package fails
  # to install rather than failing at run time. That is the floor DESCRIPTION names.
  geom = coursekata::GeomSquareplot,
  stat = coursekata::StatSquareplot,
  position = "identity",
  aes_form = ~x,
  extras = alist(
    binwidth = NULL, bins = NULL, center = NULL, boundary = NULL,
    closed = NULL, breaks = NULL, bars = "none", na.rm = TRUE
  ),
  note = "each observation is drawn as its own square, so a bin can be counted",
  layer_fun = ggplot2::layer,
  # `pre` is evaluated in the ggformula namespace, so a coursekata helper needs :::
  pre = {
    coursekata:::squareplot_check(object, gformula, na.rm)
    layer_fun <- coursekata:::squareplot_layer(object)
  }
)
