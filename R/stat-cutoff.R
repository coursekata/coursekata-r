#' Compute a distribution part's empirical cutoffs, per panel
#'
#' `StatCutoff` calls the same `cutoff_plan()` used by [show_cutoffs()], but the
#' two interfaces deliberately operate at different scopes. ggplot2 supplies a
#' stat with one panel's rows at a time, so this stat computes panel-local
#' cutoffs. `show_cutoffs()` plans once from the whole resolved distribution and
#' repeats those global marks across facets.
#'
#' The stat accepts `"middle"`, `"tails"`, `"upper"`, `"lower"`, and
#' `"outer"`. It selects cutoffs in the data's original space: transformed
#' position values are inverted before `cutoff_plan()` runs, and the selected
#' cutoffs are transformed forward again for ggplot2. This preserves the
#' requested distribution part on increasing, decreasing, and transformed
#' scales.
#'
#' `stat_cutoff()` is the conventional layer constructor. Its default geom is
#' `"cutoff"`; use `geom = "vline"` when a full-height reference line is
#' wanted instead. Since the stat computes once per panel, styling aesthetics
#' from the source data cannot be mapped; set them to one value, or facet the
#' plot to compute one set of cutoffs per group.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' @param data The data to be displayed in this layer.
#' @param geom The geometric object used to display the data. Defaults to
#'   `"cutoff"`.
#' @param position A position adjustment. Defaults to `"identity"`.
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @param part The distribution part to mark: `"middle"`, `"tails"`,
#'   `"upper"`, `"lower"`, or `"outer"`.
#' @param prop The requested proportion. The endpoints `0` and `1` are valid
#'   for this lower-level stat.
#' @param greedy Whether a fractional observation is included in the selected
#'   region.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, override the default aesthetics rather than
#'   combining with them.
#'
#' @return `stat_cutoff()` returns a ggplot2 layer.
#'
#' @format `StatCutoff` is a [ggplot2::Stat] object.
#'
#' @seealso [show_cutoffs()], which marks whole-distribution cutoffs.
#' @export
StatCutoff <- ggplot2::ggproto(
  "StatCutoff", ggplot2::Stat,
  required_aes = "x",
  extra_params = c("na.rm", "func"),
  setup_params = function(data, params) {
    check_panel_stat_aesthetics(data, "stat_cutoff", "x")
    if ("func" %in% names(params)) {
      abort("`stat_cutoff()` no longer takes `func`; use `part` instead.")
    }

    part <- params$part %||% "middle"
    if (!is_string(part) || part %in% cutoff_functions() == FALSE) {
      abort(c(
        "`stat_cutoff()`'s `part` names the part of the distribution to cut",
        x = glue("got {as_label(part)}"),
        i = glue("one of: {collapse(cutoff_functions())}")
      ))
    }

    prop <- params$prop %||% .95
    if (!is.numeric(prop) || length(prop) != 1L || is.na(prop) || prop < 0 || prop > 1) {
      abort(c(
        glue("`stat_cutoff()`'s `prop` is the proportion `{part}()` describes"),
        x = glue("got {as_label(prop)}"),
        i = "a single number from 0 to 1"
      ))
    }

    params$part <- part
    params$prop <- prop
    params
  },
  compute_panel = function(data, scales, part = "middle", prop = .95,
                           greedy = TRUE, na.rm = TRUE) {
    has_transformation <- !is.null(scales$x) && is.function(scales$x$get_transformation)
    transformation <- if (has_transformation) scales$x$get_transformation() else NULL
    values <- if (is.null(transformation)) data$x else transformation$inverse(data$x)

    plan <- cutoff_plan(list(func = part, prop = prop, greedy = greedy), values)
    xintercept <- c(plan$lower, plan$upper)
    xintercept <- xintercept[!is.na(xintercept)]
    if (!is.null(transformation)) xintercept <- transformation$transform(xintercept)

    data.frame(xintercept = xintercept)
  }
)

#' @rdname StatCutoff
#' @export
stat_cutoff <- function(mapping = NULL, data = NULL, geom = "cutoff",
                        position = "identity", ..., part = "middle", prop = 0.95,
                        greedy = TRUE,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if ("func" %in% names(rlang::enquos(...))) {
    abort("`stat_cutoff()` no longer takes `func`; use `part` instead.")
  }

  ggplot2::layer(
    stat = StatCutoff, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(
      part = part, prop = prop, greedy = greedy, na.rm = na.rm, ...
    )
  )
}
