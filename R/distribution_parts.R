#' Find a percentage of a distribution
#'
#' Given a distribution, find which values lie in the upper, lower, or middle proportion of the
#' distribution. Useful when you want to do something like shade in the middle 95% of a plot. This
#' is a greedy operation, meaning that if the cutoff point is between two whole numbers the
#' specified region will suck up the extra space. For example, the requesting the upper 30% of the
#' \code{[1 2 3 4]} will return \code{[FALSE FALSE TRUE TRUE]} because the 30% was greedy.
#'
#' @param x The distribution of values to check.
#' @param prop The proportion of values to find.
#' @param greedy Whether the function should be greedy, as per the description above.
#'
#' @return A logical vector indicating which values are in the specified region.
#'
#' @rdname distribution_parts
#' @export
#'
#' @examples
#'
#' upper(1:10, .1)
#' lower(1:10, .2)
#' middle(1:10, .5)
#'
#' sampling_distribution <- do(1000) * mean(rnorm(100, 5, 10))
#' sampling_distribution %>%
#'   gf_histogram(~ mean, data = sampling_distribution, fill = ~ middle(mean, .68)) %>%
#'   gf_refine(scale_fill_manual(values = c("blue", "coral")))
middle <- function(x, prop = .95, greedy = TRUE) {
  tail_prop <- (1 - prop) / 2
  in_upper <- upper(x, tail_prop, !greedy)
  in_lower <- lower(x, tail_prop, !greedy)
  !in_upper & !in_lower
}


#' @rdname distribution_parts
#' @export
lower <- function(x, prop = .025, greedy = TRUE) {
  tail_size <- if (greedy) ceiling(length(x) * prop) else floor(length(x) * prop)

  values <- data.frame(x = x, original_pos = seq_along(x))
  values <- values[order(x), , drop = FALSE]
  values$in_zone <- seq_along(x) <= tail_size

  values[order(values$original_pos), 'in_zone', drop = TRUE]
}


#' @rdname distribution_parts
#' @export
upper <- function(x, prop = .025, greedy = TRUE) {
  tail_size <- if (greedy) ceiling(length(x) * prop) else floor(length(x) * prop)

  values <- data.frame(x = x, original_pos = seq_along(x))
  values <- values[order(x, decreasing = TRUE), , drop = FALSE]
  values$in_zone <- seq_along(x) <= tail_size

  values[order(values$original_pos), 'in_zone', drop = TRUE]
}
