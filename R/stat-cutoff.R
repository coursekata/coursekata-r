#' Compute a distribution part's empirical quantile cutoffs, per panel
#'
#' Calls `cutoff_plan()`, the same function `show_cutoffs()` calls, so there is
#' exactly one implementation of the cutoff rule and the two consumers cannot
#' drift apart.
#'
#' The difference between the two is deliberate, not an oversight: `show_cutoffs()`
#' marks the *whole* distribution, because that is what a `middle()` fill shades --
#' every value in the plot's data is either inside or outside the shaded region,
#' regardless of which facet panel it lands in. This stat computes **per panel**,
#' because that is what a stat does with the rows ggplot2 hands it: a faceted plot
#' gets one set of cutoffs per panel, each drawn from that panel's own rows. Pair it
#' with [ggplot2::GeomVline] to draw the intercepts it emits -- one for `upper()` or
#' `lower()`, two for `middle()`, `tails()`, or `outer()`.
#'
#' `show_cutoffs()` keeps its own `annotate()`-based rendering rather than being
#' rebuilt on this stat: its markers and labels are placed against the panel's npc
#' range in `draw_panel()`-adjacent code that a stat's `compute_panel()` has no
#' access to, and moving to a stat would silently turn its whole-distribution
#' marking into this per-panel one on any faceted plot -- disagreeing with the
#' whole-data fill it marks.
#'
#' @format A [ggplot2::Stat] object.
#'
#' @seealso [show_cutoffs()], which marks the same cutoffs by a different route.
#' @export
#' @examples
#' gf_histogram(~Thumb, data = Fingers, binwidth = 5) %>%
#'   gf_refine(ggplot2::layer(
#'     stat = coursekata::StatCutoff, geom = ggplot2::GeomVline, position = "identity",
#'     params = list(func = "middle", prop = .95, na.rm = TRUE)
#'   ))
StatCutoff <- ggplot2::ggproto(
  "StatCutoff", ggplot2::Stat,
  required_aes = "x",
  # `show_cutoffs()` reaches `cutoff_plan()` through `cutoff_spec()`, which
  # reads a CALL and can refuse a name it does not know. Nothing reads this
  # route's parameters, which arrive as plain values from someone writing a
  # layer by hand -- and every unusable one still plans a cutoff rather than
  # failing. Measured before this: `func = "bogus"` fell through the switch and
  # marked a lower cutoff, and `prop = 2` returned an "upper" cutoff sitting on
  # the smallest observation. Both drew a mark that looks like every other one.
  setup_params = function(data, params) {
    func <- params$func %||% "middle"
    if (!is_string(func) || func %in% cutoff_functions() == FALSE) {
      abort(c(
        glue("`StatCutoff`'s `func` names the part of the distribution to cut"),
        x = glue("got {as_label(func)}"),
        i = glue("one of: {collapse(cutoff_functions())}")
      ))
    }

    prop <- params$prop %||% .95
    if (!is.numeric(prop) || length(prop) != 1L || is.na(prop) || prop < 0 || prop > 1) {
      abort(c(
        glue("`StatCutoff`'s `prop` is the proportion `{func}()` describes"),
        x = glue("got {as_label(prop)}"),
        i = "a single number from 0 to 1"
      ))
    }

    params
  },
  compute_panel = function(data, scales, func = "middle", prop = .95,
                           greedy = TRUE, na.rm = TRUE) {
    # planned in the DATA's own space, then carried back. `data$x` arrives
    # already transformed by the scale, and a cutoff is a quantile: quantiles
    # survive a transformation that increases (`log`) and turn over under one
    # that decreases. Under `scale_x_reverse()` the upper tail of the
    # transformed values is the LOWER tail of the reader's own numbers, so an
    # upper cutoff on 1:10 landed on 2. ggplot2's own transformation object
    # both ways round, so nothing here needs to know which scale it is on.
    # `scales` is whatever the caller has: ggplot2 hands over the panel's own
    # pair, a direct call may pass NULL, and a scale class that predates the
    # accessor does not answer to it. None of those is an error -- an
    # untransformed axis is the ordinary case and needs nothing done to it.
    has_transformation <- !is.null(scales$x) && is.function(scales$x$get_transformation)
    transformation <- if (has_transformation) scales$x$get_transformation() else NULL
    values <- if (is.null(transformation)) data$x else transformation$inverse(data$x)

    plan <- cutoff_plan(list(func = func, prop = prop, greedy = greedy), values)
    xintercept <- c(plan$lower, plan$upper)
    xintercept <- xintercept[!is.na(xintercept)]
    if (!is.null(transformation)) xintercept <- transformation$transform(xintercept)

    data.frame(xintercept = xintercept)
  }
)
