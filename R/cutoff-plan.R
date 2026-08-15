#' Read the facts out of a distribution-part fill aesthetic
#'
#' Matches the fill call against the real function's formals, so a caller may
#' name arguments, reorder them, rely on documented defaults, or qualify the
#' call with `coursekata::`.
#'
#' @param fill The result of `plot_spec(p)$resolve_aes("fill")`, or `NULL`.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `func`, `prop` and `greedy`. `greedy` is `NA` for
#'   `outer()`, which has no such formal, and [cutoff_plan()] reads that as
#'   `TRUE`.
#'
#' @noRd
cutoff_spec <- function(fill, call = caller_env()) {
  valid <- c("middle", "tails", "upper", "lower", "outer")

  expr <- if (is.null(fill)) NULL else quo_get_expr(fill$quo)
  if (is.null(expr) || !is.call(expr)) {
    options <- paste(paste0("~", valid, "(...)"), collapse = " or ")
    abort(
      c(
        "Could not find a distribution function in the plot's fill aesthetic",
        glue("use fill = {options}")
      ),
      call = call
    )
  }

  func <- call_name(expr)
  if (is.null(func) || func %in% valid == FALSE) {
    abort(
      c(
        glue("Expected fill to use {collapse(valid)}"),
        glue("found: {deparse(expr)}")
      ),
      call = call
    )
  }

  matched <- call_match(expr, get(func, envir = asNamespace("coursekata")), defaults = TRUE)
  env <- quo_get_env(fill$quo)
  prop <- eval_tidy(call_args(matched)$prop, env = env)
  greedy <- if ("greedy" %in% names(call_args(matched))) {
    eval_tidy(call_args(matched)$greedy, env = env)
  } else {
    NA
  }

  if (!is.numeric(prop) || length(prop) != 1 || is.na(prop) || prop <= 0 || prop >= 1) {
    abort(
      glue("`prop` must be a single number between 0 and 1, not {deparse(prop)}"),
      call = call
    )
  }

  list(func = func, prop = prop, greedy = greedy)
}

#' Decide where a distribution's cutoff markers belong
#'
#' Calls [tail_size()] rather than re-deriving it, so the marker and the shading
#' are derived from one count and cannot drift apart.
#'
#' @param cspec A [cutoff_spec()] list.
#' @param values The x values the plot was built from.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `tail_prop`, `label`, `lower`, `upper`, and the
#'   `data_range` the values span. Every position in it is a data value.
#'
#' @noRd
cutoff_plan <- function(cspec, values, call = caller_env()) {
  x <- sort(values[!is.na(values)])
  n <- length(x)
  if (n == 0) {
    abort("The plot's variable has no non-missing values to place cutoffs on", call = call)
  }

  greedy <- if (is.na(cspec$greedy)) TRUE else cspec$greedy

  # middle() delegates to its tails with the greediness inverted: a greedy middle
  # means non-greedy tails. outer(x, p) is tails(x, 1 - p).
  two_sided <- cspec$func %in% c("middle", "tails", "outer")
  tail_prop <- switch(cspec$func,
    middle = ,
    tails  = (1 - cspec$prop) / 2,
    outer  = cspec$prop / 2,
    cspec$prop
  )
  tail_greedy <- if (two_sided) !greedy else greedy
  k <- tail_size(x, tail_prop, tail_greedy)

  lower <- NA_real_
  upper <- NA_real_
  if (two_sided) {
    # tails() and outer() invert the coloring, so this is the middle's edge either way
    lower <- x[cutoff_index(k + 1, n)]
    upper <- x[cutoff_index(n - k, n)]
  } else if (cspec$func == "upper") {
    upper <- x[cutoff_index(n - k + 1, n)]
  } else {
    lower <- x[cutoff_index(k, n)]
  }

  list(
    tail_prop = tail_prop,
    label = cutoff_label(tail_prop), lower = lower, upper = upper,
    data_range = x[c(1, n)]
  )
}

#' Pin a marker's index inside the sorted values
#'
#' A tail can round to no values at all -- `lower(x, .05, greedy = FALSE)` on 10
#' values asks for half an observation and gets none -- and then there is no
#' shaded value to mark. The marker goes on the extreme value instead, saying
#' the cutoff lies beyond every observation rather than silently disappearing.
#'
#' @param i The index the marker rule chose.
#' @param n The number of values.
#'
#' @return An index between 1 and `n`.
#'
#' @noRd
cutoff_index <- function(i, n) max(1, min(n, i))

#' Format a tail proportion the way a textbook writes it
#'
#' @param tail_prop A single proportion.
#'
#' @return A string with no leading zero, e.g. `".025"`.
#'
#' @noRd
cutoff_label <- function(tail_prop) {
  sub("^0", "", format(tail_prop, scientific = FALSE, trim = TRUE))
}
