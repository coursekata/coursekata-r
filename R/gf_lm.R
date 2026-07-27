#' Add a Linear Model Layer, with Categorical and Shuffle Support
#'
#' `r lifecycle::badge("experimental")`
#'
#' An extended `gf_lm()` that adds two capabilities on top of
#' [ggformula::gf_lm()]:
#'
#' * **Categorical x** -- when the x variable is a factor or character, it draws
#'   horizontal group-mean segments (via [gf_lm_cat()]) instead of a regression
#'   line, so `gf_lm()` gives a sensible model layer for either kind of predictor.
#' * **Shuffle-safe continuous x** -- when the y mapping contains `shuffle()` (or
#'   any computed expression), it is evaluated once and locked in, so the
#'   regression line and the jittered dots reflect the same permutation instead
#'   of two independent shuffles.
#'
#' Called standalone (with a formula rather than a plot), it behaves exactly like
#' [ggformula::gf_lm()]; the extra behavior activates only when piped onto an
#' existing plot. This function masks [ggformula::gf_lm()] when coursekata is
#' attached.
#'
#' @param object A formula (standalone use) or an existing ggplot object (piped
#'   use).
#' @param gformula A formula specifying the plot, passed through to ggformula.
#' @param data A data frame (standalone use).
#' @param ... Additional arguments passed to [ggformula::gf_lm()] (continuous x)
#'   or `geom_segment()` (categorical x).
#' @param width For categorical x, the width of each group-mean segment.
#'   Default `0.4`.
#' @param color For categorical x, the segment color. Default `"#663abe"`.
#' @param linewidth For categorical x, the segment width. Default `1`.
#'
#' @return A ggplot object (or, for standalone use, whatever
#'   [ggformula::gf_lm()] returns).
#'
#' @seealso [gf_lm_cat()] for the categorical case; [ggformula::gf_lm()], which
#'   this extends.
#'
#' @export
#' @examples
#' # Continuous x: an ordinary regression line, identical to ggformula::gf_lm().
#' gf_point(Thumb ~ Height, data = Fingers, alpha = .3) %>%
#'   gf_lm()
#'
#' # Categorical x: horizontal group-mean segments instead of a sloped line.
#' gf_jitter(Tip ~ Condition, data = TipExperiment, width = .1) %>%
#'   gf_lm()
#'
#' # Shuffle-safe: the line is fit to the same shuffled y the dots show, because
#' # the mapping is evaluated once rather than re-shuffled for the line.
#' set.seed(1)
#' gf_jitter(shuffle(Thumb) ~ Height, data = Fingers, width = .1) %>%
#'   gf_lm()
gf_lm <- function(object = NULL, gformula = NULL, data = NULL, ...,
                  width = 0.4, color = "#663abe", linewidth = 1) {
  # Standalone use: object is a formula or data frame, not a plot. Forward
  # everything to ggformula and do nothing extra.
  if (!inherits(object, c("gg", "ggplot"))) {
    return(ggformula::gf_lm(object = object, gformula = gformula, data = data, ...))
  }

  # Piped use: object is an existing plot. Evaluate the x/y mappings once,
  # locking shuffle() and other computed expressions into hidden columns so all
  # downstream layers see the same values.
  p <- freeze_xy_values(object)

  x_raw <- p$data[[".gf_x"]]
  is_cat <- is.factor(x_raw) || is.character(x_raw)

  if (is_cat) {
    # Categorical x: delegate to gf_lm_cat (p is already frozen, so its own
    # freeze call is a no-op).
    gf_lm_cat(p, width = width, color = color, linewidth = linewidth, ...)
  } else {
    # Continuous x: hand off to ggformula using the frozen columns so StatLm
    # never re-evaluates shuffle().
    ggformula::gf_lm(p, gformula = .gf_y ~ .gf_x, data = p$data, ...)
  }
}
