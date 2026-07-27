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
#' Companion to `freeze_xy_values()`, which pins the mapped *values* (e.g. a
#' `shuffle()`ed outcome) rather than the jitter *position* this pins.
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

#' Evaluate a plot's x and y mappings once and lock them into hidden columns
#'
#' ggformula formulas like `shuffle(y) ~ x` are re-evaluated at render time by
#' ggplot2, so the jittered dots and any downstream layer (a regression line,
#' group-mean segments, coefficient arrows) can each see a *different* shuffle
#' of `y`. This evaluates `y` and `x` exactly once and stores the results in
#' hidden `.gf_y` / `.gf_x` columns, then points the raw-data layers' mappings
#' at those columns so every layer draws from the same values.
#'
#' Only `StatIdentity` layers that still hold the original data frame are
#' rewritten (`geom_point()`, `geom_jitter()`); layers with a computed stat
#' (`StatSmooth`, model layers) receive already-transformed data and never carry
#' `.gf_y`, so they are left alone. Axis labels are restored from the original
#' mapping expressions so titles read `shuffle(y)` rather than `.gf_y`. No-op if
#' the plot is already frozen.
#'
#' New quosures are built with [rlang::base_env()] rather than `quo()` because
#' the calling function's environment is gone by render time; `eval_tidy()`
#' resolves `.gf_y` / `.gf_x` from the data mask anyway.
#'
#' Companion to `freeze_jitter()`, which pins the jitter *position* rather than
#' these mapped *values*.
#'
#' @param p A ggplot object.
#'
#' @return The plot, with `x`/`y` evaluated once and locked into `.gf_x`/`.gf_y`.
#'
#' @noRd
freeze_xy_values <- function(p) {
  if (".gf_y" %in% names(p$data)) {
    return(p)
  }

  orig_data <- p$data

  label_from <- function(quo) tryCatch(as_label(quo), error = function(e) NULL)
  orig_y_label <- label_from(p$mapping$y) %||%
    if (length(p$layers)) label_from(p$layers[[1]]$mapping$y)
  orig_x_label <- label_from(p$mapping$x) %||%
    if (length(p$layers)) label_from(p$layers[[1]]$mapping$x)

  y_vals <- eval_tidy(p$mapping$y, data = orig_data)
  x_vals <- eval_tidy(p$mapping$x, data = orig_data)

  frozen_y <- new_quosure(quote(.gf_y), base_env())
  frozen_x <- new_quosure(quote(.gf_x), base_env())

  p$data[[".gf_y"]] <- y_vals
  p$data[[".gf_x"]] <- x_vals

  for (i in seq_along(p$layers)) {
    ld <- p$layers[[i]]$data
    is_identity_stat <- inherits(p$layers[[i]]$stat, "StatIdentity")
    if (is_identity_stat && is.data.frame(ld) && identical(ld, orig_data)) {
      p$layers[[i]]$data[[".gf_y"]] <- y_vals
      p$layers[[i]]$data[[".gf_x"]] <- x_vals
      lm <- p$layers[[i]]$mapping
      if (!is.null(lm[["y"]])) p$layers[[i]]$mapping[["y"]] <- frozen_y
      if (!is.null(lm[["x"]])) p$layers[[i]]$mapping[["x"]] <- frozen_x
    }
  }

  if (!is.null(orig_y_label)) p$labels$y <- orig_y_label
  if (!is.null(orig_x_label)) p$labels$x <- orig_x_label

  p
}
