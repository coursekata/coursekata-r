#' Overlay a Categorical Model's Group Means
#'
#' `r lifecycle::badge("experimental")`
#'
#' Draws a short horizontal segment at each group's mean -- the fitted value of
#' a one-predictor categorical model -- on top of an existing `ggformula` plot.
#' It is the categorical counterpart to a regression line: [gf_lm()] dispatches
#' to it automatically when the x variable is a factor or character.
#'
#' Because it reads the plot's x and y mappings, it works with in-formula
#' transformations, including `shuffle()`.
#'
#' @param p A ggformula plot object with a categorical x, typically from
#'   `gf_jitter()` or `gf_point()`.
#' @param ... Additional aesthetics passed to `geom_segment()`.
#' @param width Total width of each group-mean segment. Default `0.4`.
#' @param color Segment color. Default CourseKata purple `"#663abe"`.
#' @param linewidth Segment width. Default `1`.
#'
#' @return A ggplot object with group-mean segments added.
#'
#' @seealso [gf_lm()], which calls this for categorical predictors.
#'
#' @export
#' @examples
#' # Each segment sits at a group's mean -- the categorical model's prediction
#' # for every member of that group.
#' gf_jitter(Tip ~ Condition, data = TipExperiment, width = .1) %>%
#'   gf_lm_cat()
#'
#' # It works with shuffle(): the segments track the same shuffled values the
#' # dots show, because the plot's mapping is evaluated only once.
#' set.seed(1)
#' gf_jitter(shuffle(Tip) ~ Condition, data = TipExperiment, width = .1) %>%
#'   gf_lm_cat()
gf_lm_cat <- function(p, ..., width = 0.4, color = "#663abe", linewidth = 1) {
  lifecycle::signal_stage("experimental", "gf_lm_cat()")

  # Evaluate x and y once and lock them into hidden columns so gf_lm_cat, any
  # coefficient overlay, and the dots all use the same shuffle().
  p <- freeze_xy_values(p)

  y_vals <- p$data[[".gf_y"]]
  x_raw <- p$data[[".gf_x"]]

  if (is.numeric(x_raw)) {
    abort(c(
      "gf_lm_cat() needs a categorical (factor or character) x variable",
      "for a continuous x, use gf_lm()"
    ))
  }

  if (!is.factor(x_raw)) x_raw <- factor(x_raw)
  lvls <- levels(x_raw)

  group_means <- tapply(y_vals, x_raw, mean, na.rm = TRUE)

  seg_data <- data.frame(
    x = seq_along(lvls) - width / 2,
    xend = seq_along(lvls) + width / 2,
    y = as.numeric(group_means[lvls]),
    yend = as.numeric(group_means[lvls])
  )

  p + ggplot2::geom_segment(
    data = seg_data,
    mapping = ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
    inherit.aes = FALSE,
    color = color,
    linewidth = linewidth,
    ...
  )
}
