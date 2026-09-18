#' Read coefficients from the inferred stat's prediction grid without fitting
#' @noRd
b_grid_coefficients <- function(rows, implied, scales, fn, call = caller_env()) {
  flipped <- isTRUE(unique(rows$flipped_aes))
  outcome <- if (flipped) "x" else "y"
  predictor <- setdiff(c("x", "y"), outcome)
  kind <- unique(rows$.model_kind)
  if (length(kind) != 1L) {
    abort(glue("`{fn}()` cannot identify the model represented by this plot."), call = call)
  }
  rows <- rows[is.finite(rows[[outcome]]), , drop = FALSE]
  positions <- NULL
  if (identical(kind, "line")) {
    rows <- rows[is.finite(rows[[predictor]]), , drop = FALSE]
    if (length(unique(rows$group)) != 1L || nrow(rows) < 2L) {
      abort(glue("`{fn}()` needs one fitted line with at least two distinct positions."), call = call)
    }
    rows <- rows[order(rows[[predictor]]), , drop = FALSE]
    x <- rows[[predictor]]
    y <- rows[[outcome]]
    slope <- (utils::tail(y, 1L) - y[[1L]]) / diff(range(x))
    intercept <- y[[1L]] - slope * x[[1L]]
    if (!is.finite(slope) || !isTRUE(all.equal(y, intercept + slope * x,
                                            tolerance = 1e-8))) {
      abort(glue("`{fn}()` cannot draw one coefficient for a nonlinear fitted curve."), call = call)
    }
    coefs <- c(intercept, slope)
    values <- x
  } else if (identical(kind, "segment")) {
    end <- paste0(predictor, "end")
    if (end %in% names(rows)) {
      rows[[predictor]] <- (rows[[predictor]] + rows[[end]]) / 2
    }
    reference_order <- rows[[paste0(".b_order_", predictor)]] %||% rows$group
    rows <- rows[order(reference_order), , drop = FALSE]
    x <- as.numeric(rows[[predictor]])
    if (nrow(rows) < 1L || anyDuplicated(x)) {
      abort(glue("`{fn}()` needs one mean for each displayed group."), call = call)
    }
    means <- rows[[outcome]]
    coefs <- c(means[[1L]], means[-1L] - means[[1L]])
    values <- x
    positions <- x
  } else {
    if (nrow(rows) != 1L) {
      abort(glue("`{fn}()` needs one intercept to annotate."), call = call)
    }
    coefs <- rows[[outcome]]
    values <- NULL
  }
  list(coefs = coefs, values = values, scales = scales, positions = positions,
       outcome_axis = outcome, predictor = if (kind %in% c("hline", "vline")) NULL else predictor,
       categorical = if (kind %in% c("hline", "vline")) NA else kind == "segment")
}

#' Convert annotation coordinates through public position scales
#'
#' Supplied coefficients produce marks in the model's original units; forward
#' transformation places these marks in stat space. Labels themselves stay in
#' the model's units. Inferred marks already use stat-space coordinates.
#' @noRd
b_transform_marks <- function(marks, scales) {
  lapply(marks, function(mark) {
    if (!inherits(mark, "Layer") || !is.data.frame(mark$data)) return(mark)
    data <- mark$data
    for (axis in c("x", "y")) {
      scale <- scales[[axis]]
      if (is.null(scale) || scale$is_discrete()) next
      transform <- scale$transform
      for (column in intersect(c(axis, paste0(axis, "end")), names(data))) {
        # Infinite positions anchor to panel edges, not to a transform's
        # mathematical boundary (exp(-Inf) would incorrectly become zero).
        finite <- is.finite(data[[column]])
        data[[column]][finite] <- transform(data[[column]][finite])
      }
    }
    layer_with(mark, data = data)
  })
}
