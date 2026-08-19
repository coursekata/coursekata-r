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
#' Copy a layer, giving it a different position
#'
#' A ggplot2 layer is a ggproto object, which is an environment, so assigning to
#' a layer's `position` field in place writes into the layer the caller still
#' holds: a function that reads a plot in order to draw over it would leave the
#' caller's own plot changed. Copying the layer's bindings into a fresh
#' environment and replacing the whole layer leaves the original alone.
#'
#' @param layer A ggplot2 layer.
#' @param position The position for the copy.
#'
#' @return A copy of `layer`, drawn with `position`.
#'
#' @noRd
layer_with_position <- function(layer, position) {
  copy <- as.environment(as.list.environment(layer, all.names = TRUE))
  parent.env(copy) <- parent.env(layer)
  class(copy) <- class(layer)
  copy$position <- position
  copy
}

#' Run an expression and put the caller's random stream back where it was
#'
#' Evaluating a mapping such as `shuffle(Thumb)` consumes draws. A package whose
#' subject is randomization must not move a reader's stream as a side effect of
#' drawing, so every call-time evaluation of a mapping goes through this.
#'
#' @noRd
with_random_seed_restored <- function(expr) {
  if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) {
    old <- get(".Random.seed", .GlobalEnv, inherits = FALSE)
    on.exit(assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
  } else {
    on.exit(suppressWarnings(rm(".Random.seed", envir = .GlobalEnv)), add = TRUE)
  }
  expr
}

#' Run an expression against a fixed random seed, then put the stream back
#'
#' `withr` is only a test dependency, and this is the whole of what a position
#' needs from it. The save/restore half lives in `with_random_seed_restored()`,
#' one place, because two callers need it: a position pinning a jitter seed and
#' `model_plan()`'s call-time probe of an outcome expression.
#'
#' @param seed A seed, or `NA` to run `expr` untouched.
#' @param expr The expression to evaluate.
#'
#' @return `expr`'s value.
#'
#' @noRd
with_jitter_seed <- function(seed, expr) {
  if (!isTRUE(is.finite(seed))) {
    expr
  } else {
    with_random_seed_restored({
      set.seed(seed)
      expr
    })
  }
}

#' Join items into a comma-separated string for error messages
#'
#' @noRd
collapse <- function(x) glue::glue_collapse(x, sep = ", ")

#' Turn a variable name into a one-sided formula
#'
#' @noRd
name_to_frm <- function(x) stats::formula(glue("~{x}"))
