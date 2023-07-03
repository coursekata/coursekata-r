#' Kargle, Bargle, and Zargle video game score simulator.
#'
#' Kargle, Bargle, and Zargle are three fictitious video games referenced in the
#' CourseKata Statistics and Data Science course. The games are used to introduce
#' the concept of the standard deviation. This function simulates scores from the
#' three games and allows students to explore how differences in sample size, mean,
#' and standard deviation affect the distribution of scores.
compare_score_distributions <- function(sd = 3500, mean = 35000, n = 1000, ..., .seed = 5) {
  set.seed(.seed)
  kargle <- simulate_scores("Kargle", 1000, 35000, 5000)
  bargle <- simulate_scores("Bargle", 1000, 35000, 1000)
  zargle <- simulate_scores("Zargle", n, mean, sd)
  games <- vctrs::vec_c(kargle, bargle, zargle)

  # combine all zones > 3 into a single "outside 3" zone
  games$zone <- ifelse(games$zone > 3, "outside 3", games$zone)
  # convert the proportions to cumulative proportions for all except "outside 3"
  props <- data.frame(tally(zone ~ game, data = games, format = "proportion"))
  props <- purrr::map_dfr(split(props, props$game), function(x) {
    x$Freq <- c(cumsum(x$Freq[1:3]), x$Freq[4])
    x
  })
  # re-format the table to be wide (one column per game)
  zone_table <- tidyr::pivot_wider(props, names_from = game, values_from = Freq)

  print(data.frame(zone_table))
  gf_histogram(~scores, fill = ~zone, data = games, bins = 160, alpha = .8) %>%
    gf_facet_grid(game ~ .)
}

#' Simulate score for one of the *argle games.
#'
#' @param game The name of the game.
#' @param n The number of scores to simulate.
#' @param mean The mean of the scores.
#' @param sd The standard deviation of the scores.
#' @return A data frame with the simulated scores.
#' @keywords internal
simulate_scores <- function(game, n, mean, sd) {
  scores <- rnorm(n, mean, sd)
  z <- (scores - mean) / sd
  interval <- ifelse(z > 0, trunc(1 + z), trunc(z - 1))
  data.frame(game = game, scores = scores, z = z, interval = interval, zone = abs(interval))
}
