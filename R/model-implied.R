#' Read the model a plot implies out of the plot
#'
#' The single outcome/predictor/kind decision `gf_model()`'s inference and
#' `gf_b()` both need, so that the two cannot drift apart on it: which axis
#' carries the outcome, whether there is a predictor, and what shape a fit or
#' a mark takes to draw it.
#'
#' The decision is read off the DRAWN values, after the plot is pinned
#' (`pin_plot_values()`) -- a `shuffle()` mapping is one fixed permutation by
#' the time this reads it, not a fresh one on every read, and a downstream fit
#' built from the returned `data` agrees with what the returned `plot` draws.
#' An aesthetic whose expression is or contains `after_stat()`, `stat()` or
#' `after_scale()` computes something only ggplot2's build can supply, not a
#' value this could read or fit against, so it counts as unmapped for the
#' whole of the rule below: `gf_density(~Thumb)` maps `y = after_stat(density)`,
#' and without this guard the rule would find neither axis numeric and refuse
#' with a message describing nothing the reader wrote.
#'
#' The rule, on the axes that remain once build-time mappings are set aside:
#'   - neither mapped -> refuse; there is no axis to place a model on.
#'   - exactly one mapped -> that axis carries the outcome and there is no
#'     predictor; the shape is named for the geom that draws it -- `hline`
#'     when the outcome is on y, `vline` when it is on x, matching
#'     `model_plan()`'s own table.
#'   - both mapped -> the outcome is whichever axis is numeric. When both are
#'     numeric, y is the outcome and x the predictor, a regression's usual
#'     orientation. When neither is numeric there is no numeric outcome to
#'     fit, and this refuses the same way an explicit model does. With a
#'     predictor, a numeric predictor draws a line and anything else draws a
#'     group mark (`segment`).
#'
#' This does not refuse when the pin reports an aesthetic some drawer of which
#' it could not reach (`unreached`) -- deciding what that means belongs to the
#' caller, which has its own name to put in the message; it is only carried
#' through here.
#'
#' @param object A plot already known to be a plot -- refusing a non-plot
#'   first argument is the caller's job, not this function's.
#' @param fn The name to refuse in, e.g. `"gf_model"` or `"gf_b"`.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with:
#'   `plot`      the pinned copy, or the plot unchanged when nothing needed pinning
#'   `data`      the pinned plot's data -- the frame both a layer and any fit read
#'   `outcome`   list(column =, label =), e.g. `.coursekata_pin_y` / "shuffle(Thumb)"
#'   `predictor` the same, or NULL when the plot draws only an outcome
#'   `kind`      "line" | "segment" | "hline" | "vline"
#'   `formula`   <outcome column> ~ <predictor column>, or <outcome column> ~ NULL
#'   `flipped`   TRUE when the outcome is on x
#'   `unreached` aesthetics some drawer of which the pin could not reach
#'   `facets`    the plot's facet variables
#'
#' @noRd
implied_model <- function(object, fn = "gf_model", call = caller_env()) {
  pinned <- pin_plot_values(object)
  spec <- plot_spec(pinned$plot)

  drawn <- list()
  for (a in c("x", "y")) {
    resolved <- spec$resolve_aes(a)
    if (is.null(resolved) || is.null(resolved$quo)) {
      next
    }

    expr <- quo_get_expr(resolved$quo)
    if (has_build_time_call(expr)) {
      next
    }

    drawn[[a]] <- list(
      value = eval_tidy(resolved$quo, resolved$data),
      column = if (is.null(spec$pins[[a]])) as_label(expr) else paste0(".coursekata_pin_", a),
      label = spec$labels[[a]]
    )
  }

  mapped <- names(drawn)
  if (length(mapped) == 0) {
    check_model_axes(list(axes = list()), fn = fn, call = call)
  }

  if (length(mapped) == 1) {
    outcome_axis <- mapped
  } else {
    numeric_axis <- mapped[vapply(drawn[mapped], function(d) is.numeric(d$value), logical(1))]
    # both numeric: y is the outcome, a regression's usual orientation.
    # neither numeric: there is no numeric axis to prefer, so fall back to y
    # for the refusal below to name -- the message is the same either way.
    outcome_axis <- if (length(numeric_axis) == 1) numeric_axis else "y"
    if (length(numeric_axis) == 0) {
      check_numeric_outcome(drawn[[outcome_axis]]$label, drawn[[outcome_axis]]$value, call)
    }
  }

  predictor_axis <- setdiff(mapped, outcome_axis)
  outcome <- drawn[[outcome_axis]][c("column", "label")]
  predictor <- if (length(predictor_axis) == 1) drawn[[predictor_axis]][c("column", "label")]

  kind <- if (is.null(predictor)) {
    if (outcome_axis == "y") "hline" else "vline"
  } else if (is.numeric(drawn[[predictor_axis]]$value)) {
    "line"
  } else {
    "segment"
  }

  formula <- new_formula(
    sym(outcome$column),
    if (is.null(predictor)) quote(NULL) else sym(predictor$column)
  )

  list(
    plot = pinned$plot,
    data = spec$data,
    outcome = outcome,
    predictor = predictor,
    kind = kind,
    formula = formula,
    flipped = identical(outcome_axis, "x"),
    unreached = pinned$unreached,
    facets = spec$facets
  )
}
