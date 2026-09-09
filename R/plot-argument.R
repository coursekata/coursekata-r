#' Normalize a plot argument during the `plot` to `object` transition
#'
#' The public helpers have to capture `missing()` before forwarding their
#' arguments here: once an argument crosses a function boundary, its default no
#' longer says whether the caller supplied it. Keeping that fact explicit also
#' lets this helper avoid forcing `lifecycle::deprecated()` defaults.
#'
#' @param object,plot The canonical and deprecated argument values.
#' @param object_missing,plot_missing Results of `missing(object)` and
#'   `missing(plot)` in the public helper.
#' @param fn The public helper name, without parentheses.
#' @param call The call to report when both arguments are supplied.
#'
#' @return The plot supplied through `object`, or through deprecated `plot`.
#'
#' @noRd
normalize_plot_argument <- function(object, plot, object_missing, plot_missing,
                                    fn, call = caller_env()) {
  if (!object_missing && !plot_missing) {
    abort(
      c(
        glue("`{fn}()` received both `object` and deprecated `plot`"),
        "i" = "Supply the plot once with `object =`."
      ),
      call = call
    )
  }

  if (!plot_missing) {
    lifecycle::deprecate_warn(
      "0.21.0",
      paste0(fn, "(plot)"),
      paste0(fn, "(object)"),
      id = paste0("coursekata-", fn, "-plot"),
      env = caller_env(),
      user_env = caller_env(2)
    )
    return(plot)
  }

  object
}

#' Normalize `show_cutoffs()`'s label switch during its argument transition
#'
#' As with `normalize_plot_argument()`, the public helper supplies the two
#' `missing()` results so deprecated defaults are not forced accidentally.
#'
#' @param show_labels,labels The canonical and deprecated argument values.
#' @param show_labels_missing,labels_missing Results of `missing()` in
#'   `show_cutoffs()`.
#' @param call The call to report when both arguments are supplied.
#'
#' @return The logical value supplied through `show_labels`, or through
#'   deprecated `labels`.
#'
#' @noRd
normalize_show_labels_argument <- function(show_labels, labels,
                                           show_labels_missing, labels_missing,
                                           call = caller_env()) {
  if (!show_labels_missing && !labels_missing) {
    abort(
      c(
        "`show_cutoffs()` received both `show_labels` and deprecated `labels`",
        "i" = "Supply the label switch once with `show_labels =`."
      ),
      call = call
    )
  }

  if (!labels_missing) {
    lifecycle::deprecate_warn(
      "0.21.0",
      "show_cutoffs(labels)",
      "show_cutoffs(show_labels)",
      id = "coursekata-show_cutoffs-labels",
      env = caller_env(),
      user_env = caller_env(2)
    )
    return(labels)
  }

  show_labels
}

#' Recognize the legacy named-`plot` plus positional-`part` cutoff call
#'
#' With the transitional signature `show_cutoffs(object = NULL, part, ...,
#' plot = deprecated())`, R binds the unnamed `middle(...)` in
#' `show_cutoffs(plot = p, middle(x, .95))` to `object`, leaving `part`
#' missing. This helper moves only that call shape: `plot` must be named,
#' `object` must not be named, there must be one unnamed argument, and its
#' expression must name a CourseKata distribution part. An explicit
#' `object =` plus `plot =`, or a positional plot plus `plot =`, is therefore
#' left for `normalize_plot_argument()` to reject.
#'
#' The public helper should pass `enquo(object)`, `enquo(part)`, its three
#' `missing()` results, and `sys.call()`. It then evaluates `object` only when
#' `object_missing` in the returned list is false; the returned `part` remains
#' a quosure for `cutoff_spec()`.
#'
#' @param object,part Quosures captured in `show_cutoffs()`.
#' @param object_missing,part_missing,plot_missing Results of `missing()` in
#'   `show_cutoffs()`.
#' @param user_call The original, unmatched call to `show_cutoffs()`.
#'
#' @return A list containing `object`, `part`, updated missingness flags, and
#'   `legacy_plot_part`, which records whether the move occurred.
#'
#' @noRd
normalize_cutoff_call_shape <- function(object, part, object_missing,
                                        part_missing, plot_missing, user_call) {
  args <- if (is_call(user_call)) as.list(user_call)[-1] else list()
  arg_names <- names(args)
  if (is.null(arg_names)) arg_names <- rep("", length(args))
  unnamed <- is.na(arg_names) | arg_names == ""

  object_expr <- quo_get_expr(object)
  legacy <- !plot_missing && !object_missing && part_missing &&
    "plot" %in% arg_names && !("object" %in% arg_names) &&
    sum(unnamed) == 1L && is_call(object_expr, cutoff_functions())

  if (legacy) {
    part <- object
    object_missing <- TRUE
    part_missing <- FALSE
  }

  list(
    object = object,
    part = part,
    object_missing = object_missing,
    part_missing = part_missing,
    legacy_plot_part = legacy
  )
}
