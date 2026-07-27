#' Find a percentage of a distribution
#'
#' Given a distribution, find which values lie in the upper, lower, or middle proportion of the
#' distribution. Useful when you want to do something like shade in the middle 95% of a plot. This
#' is a greedy operation, meaning that if the cutoff point is between two whole numbers the
#' specified region will suck up the extra space. For example, the requesting the upper 30% of the
#' `[1 2 3 4]` will return `[FALSE FALSE TRUE TRUE]` because the 30% was greedy.
#'
#' Note that `NA` values are ignored, i.e. they will always return `FALSE`.
#'
#' @param x The distribution of values to check.
#' @param prop The proportion of values to find.
#' @param greedy Whether the function should be greedy, as per the description above.
#'
#' @return A logical vector indicating which values are in the specified region.
#'
#' @rdname distribution_parts
#' @export
#' @seealso
#' The sampling distributions guide walks through building these distributions
#' with `do()` and `shuffle()`, and shows the bootstrap variant:
#' <https://coursekata.github.io/coursekata-r/articles/sampling-distributions.html>
#'
#' @examples
#' # each function returns a logical vector marking the values in its region
#' upper(1:10, .1)
#' lower(1:10, .2)
#' middle(1:10, .5)
#' tails(1:10, .5)
#'
#' # they are most often used as the fill aesthetic of a histogram of a
#' # sampling distribution -- here, b1s estimated from shuffled (null) data
#' set.seed(42)
#' shuffled <- data.frame(b1 = replicate(200, {
#'   shuffled_tip <- base::sample(TipExperiment$Tip)
#'   b1(lm(shuffled_tip ~ Condition, data = TipExperiment))
#' }))
#'
#' # color the middle 95%: the b1 values we would expect to see often
#' # if the empty model were true
#' gf_histogram(~b1, data = shuffled, fill = ~ middle(b1, .95))
#'
#' # tails() marks the same cutoffs with the opposite coloring: the values
#' # outside the middle 95% are the 5% most extreme
#' gf_histogram(~b1, data = shuffled, fill = ~ tails(b1, .95))
#'
#' # outer() marks the same region as tails() but takes the tail proportion
#' # directly: the outer 5%
#' gf_histogram(~b1, data = shuffled, fill = ~ outer(b1, .05))
#'
#' # upper() and lower() are for directional hypotheses: all 5% goes in one tail
#' gf_histogram(~b1, data = shuffled, fill = ~ upper(b1, .05))
#' gf_histogram(~b1, data = shuffled, fill = ~ lower(b1, .05))
middle <- function(x, prop = .95, greedy = TRUE) {
  tail_prop <- (1 - prop) / 2
  in_upper <- upper(x, tail_prop, !greedy)
  in_lower <- lower(x, tail_prop, !greedy)

  !in_upper & !in_lower
}

#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `outer()` marks values in both outer tails of a distribution. It is the
#' complement of [middle()]: `outer(x, prop)` is equivalent to
#' `tails(x, 1 - prop)`.
#'
#' @param x The distribution of values to check.
#' @param prop The total proportion in both tails combined, must be in (0, 1).
#'
#' @rdname distribution_parts
#' @export
outer <- function(x, prop) {
  lifecycle::signal_stage("experimental", "outer()")
  if (!is.numeric(prop) || length(prop) != 1 || prop <= 0 || prop >= 1) {
    abort("`prop` must be a single number between 0 and 1 (exclusive).")
  }
  tails(x, 1 - prop)
}

#' @rdname distribution_parts
#' @export
tails <- function(x, prop = .95, greedy = TRUE) {
  !middle(x, prop, greedy)
}


#' @rdname distribution_parts
#' @export
lower <- function(x, prop = .025, greedy = TRUE) {
  values <- data.frame(x = x, original_pos = seq_along(x))
  values <- values[order(x), , drop = FALSE]
  values$in_zone <- seq_along(x) <= tail_size(x, prop, greedy)
  values$in_zone[is.na(values$x)] <- rlang::na_lgl

  values[order(values$original_pos), "in_zone", drop = TRUE]
}


#' @rdname distribution_parts
#' @export
upper <- function(x, prop = .025, greedy = TRUE) {
  values <- data.frame(x = x, original_pos = seq_along(x))
  values <- values[order(x, decreasing = TRUE), , drop = FALSE]
  values$in_zone <- seq_along(x) <= tail_size(x, prop, greedy)
  values$in_zone[is.na(values$x)] <- rlang::na_lgl

  values[order(values$original_pos), "in_zone", drop = TRUE]
}

#' Calculate the number of values in the tail of a distribution
#'
#' @param x The distribution of values to check.
#' @param prop The proportion of values to find.
#' @param greedy Whether the function should be greedy, as per the description above.
#'
#' @return The number of values in the tail of the distribution.
#'
#' @noRd
tail_size <- function(x, prop, greedy) {
  na_rm <- stats::na.omit(x)
  tail_unbiased <- length(na_rm) * prop
  if (greedy) ceiling(tail_unbiased) else floor(tail_unbiased)
}
