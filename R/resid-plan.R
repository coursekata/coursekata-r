#' What the model predicts for every row the plot holds
#'
#' Prediction over the plot data preserves row alignment when model fitting
#' omitted observations.
#'
#' @noRd
resid_fitted <- function(model, data, call = caller_env()) {
  tryCatch(
    stats::predict(model, newdata = data),
    error = function(cnd) {
      used <- all.vars(stats::formula(model)[-2])
      absent <- setdiff(used, names(data))
      if (length(absent) == 0) {
        abort(conditionMessage(cnd), parent = cnd, call = call)
      }
      abort(
        c(
          "The model uses variables the plot's data does not have",
          glue("model: {collapse(used)}"),
          glue("missing from the plot's data: {collapse(absent)}")
        ),
        parent = cnd,
        call = call
      )
    }
  )
}

#' Which end aesthetic a model's residuals are measured on
#'
#' The prediction belongs on the end aesthetic for the axis carrying the
#' model's outcome. A residual is undefined for this plot if neither axis maps
#' that outcome.
#'
#' @noRd
resid_end <- function(spec, model, call = caller_env(), orientation = NA) {
  outcome <- as_label(rlang::f_lhs(stats::formula(model)))
  outcome_axis <- names(spec$axes[spec$axes %in% outcome])
  if (length(outcome_axis) == 0) {
    axes <- purrr::imap_chr(spec$axes, function(variable, aes) glue("{aes} = {variable}"))
    abort(
      c(
        "A residual is measured along the axis carrying the model's outcome",
        glue("the model predicts: {collapse(outcome)}"),
        glue("the plot's axes are: {collapse(axes)}"),
        "plot the outcome this model predicts, or measure the model this plot was built for"
      ),
      call = call
    )
  }
  preferred <- if (identical(orientation, "y")) "x" else "y"
  if (preferred %in% outcome_axis) paste0(preferred, "end") else paste0(outcome_axis[[1]], "end")
}

#' Refuse a fit whose squares would not add up
#'
#' The area identity requires the residuals to be orthogonal to the fitted
#' values' reduction from the grand mean. Unweighted least squares with an
#' intercept guarantees that orthogonality. Weighted fits use a weighted inner
#' product instead, so their unweighted areas do not satisfy the identity.
#' An offset can also contribute fitted variation outside the model matrix,
#' where least squares does not guarantee orthogonality to the residuals.
#'
#' @param model A model fit by `lm()` or `aov()`.
#' @param fn The name to refuse in, e.g. `"gf_reduce"`.
#' @param call The calling environment, for error reporting.
#'
#' @return `model`, invisibly.
#'
#' @noRd
check_decomposable <- function(model, fn, call = caller_env()) {
  no_intercept <- identical(as.integer(attr(stats::terms(model), "intercept")), 0L)
  weighted <- length(model$weights) > 0
  offset <- length(model$offset) > 0
  residual <- switch(fn,
    geom_reduce = "geom_resid", geom_square_reduce = "geom_square_resid",
    stat_reduce = "stat_resid", gf_square_reduce = "gf_square_resid",
    gf_squareduce = "gf_squaresid", "gf_resid")

  if (!no_intercept && !weighted && !offset) {
    return(invisible(model))
  }

  abort(
    c(
      glue("`{fn}()` draws a reduction that this model's own arithmetic does not support"),
      x = if (no_intercept) {
        "this model was fit without an intercept"
      } else if (weighted) {
        "this model was fit with weights"
      } else {
        "this model was fit with an offset"
      },
      "*" = paste(
        "the sums of the total, error and reduction squares only add up when residuals",
        "are orthogonal to the model's reduction from the grand mean"
      ),
      i = paste(
        if (no_intercept) "fit the model with its intercept" else if (weighted) {
          "fit the model unweighted"
        } else "fit the model without its offset",
        glue("or measure it with `{residual}()`, which needs no such identity")
      )
    ),
    call = call
  )
}

warn_empty_reduction <- function(model, fn) {
  if (ncol(model$model) <= 1) {
    warn(
      c(
        glue("`{fn}()` was given the empty model, so every reduction is zero"),
        "*" = "the empty model is the grand mean; there is nothing for it to reduce",
        "*" = "pass the model whose predictor you want to see the work of"
      ),
      class = "coursekata_reduce_empty"
    )
  }
  invisible(model)
}

#' The grand mean a model's reductions start from
#'
#' @param model A model fit by `lm()` or `aov()`.
#'
#' @return One number.
#'
#' @noRd
reduction_grand <- function(model) {
  mean(model$model[[1]])
}

#' Refuse a plot that does not draw both of the axes a residual spans
#'
#' Run this check before prediction so a missing axis is reported as a plot
#' problem, not as a model or function failure.
#'
#' @param spec A `plot_spec()`.
#' @param call The calling environment, for error reporting.
#'
#' @return `spec`, invisibly.
#'
#' @noRd
check_resid_axes <- function(spec, call = caller_env()) {
  absent <- c("x", "y")[purrr::map_lgl(c("x", "y"), ~ is.null(spec$mapping[[.x]]))]
  if (length(absent) > 0) {
    # Labels retain the expressions the user wrote; pinned quosures do not.
    mapped <- purrr::imap_chr(spec$labels, function(label, aes) glue("{aes} = {label}"))
    abort(
      c(
        "A residual needs both an x and a y on the plot",
        glue("the plot maps: {if (length(mapped) > 0) collapse(mapped) else 'nothing'}"),
        glue("missing: {collapse(absent)}")
      ),
      call = call
    )
  }
  invisible(spec)
}

check_resid_plot <- function(object, fn, call = caller_env()) {
  if (!inherits(object, c("gg", "ggplot"))) {
    abort(glue("`{fn}()` needs to be layered on top of a plot."), call = call)
  }
  invisible(object)
}

#' Build a residual specification from a model
#'
#' Validation order is part of the error contract: plot, model, axes,
#' prediction, then outcome axis.
#'
#' @param object The plot the layer is being added to.
#' @param model A model fit by `lm()` or `aov()`.
#' @param fn The name to refuse in, e.g. `"gf_resid"`.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `data` (the selected source rows plus `.fitted`) and
#'   `aesthetics`.
#'
#' @noRd
resid_spec <- function(object, model, fn = "gf_resid", call = caller_env()) {
  check_resid_plot(object, fn, call = call)
  if (is.null(model)) {
    abort(
      c(
        glue("`{fn}()` needs to be told which model to measure residuals from"),
        i = glue("a model you already fit: `{fn}(lm(Thumb ~ Height, data = Fingers))`")
      ),
      call = call
    )
  }
  spec <- plot_spec(object)
  resid_layer_spec(spec$data, spec$mapping[c("x", "y")], model,
                   fn = fn, labels = spec$labels, call = call)
}

#' Build a residual specification from a function of x
#'
#' A function of x predicts y by definition, so this path always uses `yend`.
#' Keep the argument named `fun`: the resulting base R error is part of the
#' existing interface. As with `resid_spec()`, validate the axes before calling
#' user code.
#'
#' @param object The plot the layer is being added to.
#' @param fun A function of the plot's x values returning a predicted y for each.
#' @param fn The name to refuse in, e.g. `"gf_resid_fun"`.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `data` (the selected source rows plus `.fitted`) and
#'   `aesthetics`.
#'
#' @noRd
resid_fun_spec <- function(object, fun, fn = "gf_resid_fun", call = caller_env()) {
  check_resid_plot(object, fn, call = call)
  if (is.null(fun)) {
    abort(
      c(
        glue("`{fn}()` needs to be told which function to measure residuals from"),
        i = glue("a function of x: `{fn}(function(x) 2 + 3 * x)`")
      ),
      call = call
    )
  }
  spec <- plot_spec(object)
  resid_layer_spec(spec$data, spec$mapping[c("x", "y")], fun = fun,
                   fn = fn, labels = spec$labels, call = call)
}

#' Build a reduction specification from a model
#'
#' A reduction starts at the grand mean in the model frame and ends at the
#' fitted value. Using the model frame matters when the model was fit on fewer
#' rows than the plot contains. Validation follows the same order as
#' `resid_spec()`.
#'
#' @param object The plot the layer is being added to.
#' @param model A model fit by `lm()` or `aov()`.
#' @param fn The name to refuse in, e.g. `"gf_reduce"`.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `data` (the selected source rows plus `.fitted` and
#'   `.grand`) and `aesthetics`.
#'
#' @noRd
reduce_spec <- function(object, model, fn = "gf_reduce", call = caller_env()) {
  check_resid_plot(object, fn, call = call)
  if (is.null(model)) {
    abort(
      c(
        glue("`{fn}()` needs to be told which model to measure the reduction of"),
        i = glue("a model you already fit: `{fn}(lm(Thumb ~ Height, data = Fingers))`")
      ),
      call = call
    )
  }
  spec <- plot_spec(object)
  resid_layer_spec(spec$data, spec$mapping[c("x", "y")], model,
                   reduction = TRUE, fn = fn, labels = spec$labels, call = call)
}

#' Adapt the shared residual layer builder to `layer_factory()`
#'
#' Named `model` and `fun` arguments arrive in `params` but are inputs to layer
#' construction, not geom or stat parameters, so they are removed here. The
#' plot's positional quosures replace ggformula's copies to preserve their
#' original evaluation environments. Both interfaces prepare these inputs with
#' `resid_layer_spec()` before calling the same `resid_layer()` builder.
#' `check.param` is passed through because ggformula and ggplot2 use different
#' defaults.
#'
#' @param tag The tag to name the layer with.
#' @param spec The prepared data, mappings, orientation, and operation.
#'
#' @return A function with the formals `layer_factory()` expects.
#'
#' @noRd
resid_layer_fun <- function(tag, spec, linewidth_given = TRUE) {
  force(tag)
  force(spec)
  force(linewidth_given)
  function(geom, stat, position, params = NULL, mapping = NULL, data = NULL,
           check.aes = TRUE, check.param = FALSE, show.legend = NA,
           inherit.aes = TRUE, ...) {
    params[["model"]] <- NULL
    params[["fun"]] <- NULL
    if (!linewidth_given && !is.null(params$size)) params$linewidth <- NULL
    mapping <- mapping %||% ggplot2::aes()
    mapping[names(spec$aesthetics)] <- spec$aesthetics
    resid_layer(
      geom = geom,
      stat = stat,
      position = position,
      mapping = mapping,
      data = data,
      params = params,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      tag = tag,
      check.aes = check.aes,
      check.param = check.param,
      orientation = spec$orientation,
      reduction = spec$reduction,
      fn = spec$fn,
      call = call2(spec$fn),
      ...
    )
  }
}
