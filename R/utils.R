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
#' Copy a layer, replacing named fields
#'
#' A ggplot2 layer is a ggproto object, which is an environment, so assigning to
#' a layer's field in place writes into the layer the caller still holds: a
#' function that reads a plot in order to draw over it would leave the caller's
#' own plot changed. Copying the bindings into a fresh environment and replacing
#' the whole layer leaves the original alone.
#'
#' `attributes()`, not `class()`: `as.list.environment()` carries the bindings
#' and nothing else, so restoring only the class silently drops
#' `attr(layer, "coursekata_layer")` and `layer_index()` starts returning NA for
#' a layer this package put there itself.
#'
#' @noRd
layer_with <- function(layer, ...) {
  copy <- as.environment(as.list.environment(layer, all.names = TRUE))
  parent.env(copy) <- parent.env(layer)
  attributes(copy) <- attributes(layer)
  fields <- list(...)
  for (name in names(fields)) copy[[name]] <- fields[[name]]
  copy
}

#' @noRd
layer_with_position <- function(layer, position) layer_with(layer, position = position)

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
#' `withr` is only a test dependency, and this is the whole of what its callers
#' need from it. The save/restore half lives in `with_random_seed_restored()`,
#' one place, because that half is also wanted on its own: `model_plan()`'s
#' call-time probe of an outcome expression spends a draw it must not charge
#' the reader for, but has no seed to fix.
#'
#' The two callers that do fix one want the same thing from it -- two separate
#' evaluations of one random expression producing one draw. A residual's jitter
#' has to match its points layer's; a pin evaluated against both a plot's data
#' and a layer's own copy of it has to land the same permutation on each.
#'
#' @param seed A seed, or `NA` to run `expr` untouched.
#' @param expr The expression to evaluate.
#'
#' @return `expr`'s value.
#'
#' @noRd
with_fixed_seed <- function(seed, expr) {
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
