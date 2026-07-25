#' Split a character into columns for terminal output
#'
#' Split the string into `n` columns, then glue the columns together row-wise with `space_between`,
#' then glue the rows together with new line characters.
#'
#' @param strings The strings to divide into columns.
#' @param n_cols The number of columns.
#' @param space_between What to put between the columns.
#'
#' @return A string that prints to the terminal as columns.
#'
#' @noRd
to_cols <- function(strings, n_cols = 2, space_between = "       ") {
  items_per_col <- ceiling(length(strings) / n_cols)
  spacers <- rep("", items_per_col * n_cols - length(strings))
  strings <- append(strings, spacers)
  cols <- purrr::map(seq_len(n_cols), ~ strings[seq_len(items_per_col) + items_per_col * (.x - 1)])
  paste(purrr::reduce(cols, ~ paste0(.x, space_between, .y)), collapse = "\n")
}

#' Pin every unseeded jitter layer to a fixed seed
#'
#' `position_jitter()` defaults to `seed = NA`, which re-rolls the jitter on
#' every `ggplot_build()`. Functions that anchor overlays to the jittered
#' positions (`gf_resid()`, `gf_square_resid()`) used to bracket their build
#' with `sample()`/`set.seed()` so the *next* render would redraw the same
#' jitter -- but that bracket breaks when the plot is built more than once,
#' is last-writer-wins when several such functions are chained, and resets
#' the user's RNG stream.
#'
#' Instead, give each unseeded `PositionJitter` layer a seeded copy of its
#' position. ggproto objects are environments, so the seed sticks to the layer
#' itself and every subsequent build reproduces the same jitter -- whichever
#' function builds it, however many times. No-op for layers that already have
#' a seed (user-set or frozen by an earlier call in the chain) and for
#' non-jitter plots. Never calls `set.seed()`.
#'
#' Copy rather than seed in place: `geom_jitter()` and `position = "jitter"`
#' share ggplot2's namespace-level `PositionJitter` object.
#'
#' @param plot A ggplot object.
#'
#' @return The plot, with any unseeded jitter layers pinned to a fixed seed.
#'
#' @noRd
freeze_jitter <- function(plot) {
  for (i in seq_along(plot$layers)) {
    pos <- plot$layers[[i]]$position
    if (inherits(pos, "PositionJitter") && !isTRUE(is.finite(pos$seed))) {
      plot$layers[[i]]$position <- ggplot2::ggproto(
        NULL, pos,
        seed = sample.int(.Machine$integer.max, 1L)
      )
    }
  }
  plot
}
