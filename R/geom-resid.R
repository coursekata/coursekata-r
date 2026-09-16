#' Which axis a residual arrived measured on
#'
#' Exactly one of `xend` and `yend` is present. Its name identifies the axis
#' carrying the model's outcome.
#'
#' @param data A layer's data.
#'
#' @return `"y"` or `"x"`.
#'
#' @noRd
resid_axis <- function(data) {
  if ("yend" %in% names(data)) "y" else "x"
}

#' Where a squared residual's four corners go
#'
#' The square is a square on the page, not in data units: the side drawn
#' across the residual is scaled by `aspect` and by the panel's range ratio.
#' The panel's ranges are only final at draw time, which is why this is called
#' from [GeomSquareResid]'s `draw_panel()` and not from a stat.
#'
#' @param data One row per observation, with `x`, `y` (the observation) and one
#'   of `yend`/`xend` (what the model predicts for it). Which one it is says
#'   which axis the residual runs along.
#' @param x_range,y_range The panel's ranges.
#' @param aspect Multiplier on the drawn side length.
#'
#' @return `data` with four rows per observation and one `group` per square.
#'
#' @noRd
square_vertices <- function(data, x_range, y_range, aspect) {
  # The residual defines one side; the square extends along the other axis.
  along <- resid_axis(data)
  across <- if (identical(along, "y")) "x" else "y"
  along_range <- if (identical(along, "y")) y_range else x_range
  across_range <- if (identical(along, "y")) x_range else y_range
  end <- data[[paste0(along, "end")]]

  ratio <- diff(across_range) / diff(along_range)
  side <- abs(data[[along]] - end) * aspect * ratio
  # squares extend away from the nearer edge so they stay inside the panel
  away <- ifelse(data[[across]] > mean(across_range), -1, 1)
  far <- data[[across]] + away * side

  vertices <- data[rep(seq_len(nrow(data)), each = 4), , drop = FALSE]
  vertices[[across]] <- as.vector(rbind(data[[across]], far, far, data[[across]]))
  vertices[[along]] <- as.vector(rbind(data[[along]], data[[along]], end, end))
  vertices$group <- rep(seq_len(nrow(data)), each = 4)
  rownames(vertices) <- NULL
  vertices
}

#' Carry a prediction alongside the observation it belongs to
#'
#' `xend`/`yend` is a positional aesthetic on purpose: that is what gets the
#' prediction transformed by its scale and trained into the panel's range.
#' Exactly one of the two arrives, on the axis the plot put the model's
#' outcome on.
#'
#' `compute_layer` is overridden to a pass-through, the same shape
#' [ggplot2::StatIdentity] uses, so an observation with an `NA` on it (a
#' predictor the model dropped) survives the stat instead of being removed
#' before the position runs. The residual's jitter is a function of the seed
#' and the rows it is handed, exactly as the point layer's is, so losing a row
#' here would hand it a different sequence of draws and the segment would land
#' away from its point.
#'
#' @format A [ggplot2::Stat] object.
#'
#' @seealso [stat_resid()], [geom_resid()], and [geom_square_resid()].
#' @export
StatResid <- ggplot2::ggproto(
  "StatResid", ggplot2::Stat,
  required_aes = c("x", "y", "xend|yend"),
  extra_params = c("na.rm", "orientation"),
  compute_layer = function(self, data, params, layout) data,
  compute_panel = function(data, scales, ...) data
)

#' Carry reduction endpoints alongside their observations
#'
#' A reduction uses the same endpoint representation as a residual, but its
#' starting point is the grand mean rather than the observation. The separate
#' stat class keeps that statistical meaning visible to ggplot2 users and to
#' code inspecting the layer. [GeomResid] and [GeomSquareResid] can draw either
#' stat.
#'
#' @format A [ggplot2::Stat] object.
#'
#' @seealso [stat_reduce()], [geom_reduce()], and [geom_square_reduce()].
#' @export
StatReduce <- ggplot2::ggproto(
  "StatReduce", StatResid
)

#' Jitter a residual's observed end the way its point was jittered
#'
#' Seeded jitter gives the observation the same offset as its point. Running
#' [ggplot2::PositionJitter] on `x` and `y` alone leaves fitted endpoints fixed.
#' For reductions, `outcome` also holds the grand-mean axis fixed; the parent
#' position still computes both axes so the other offset remains unchanged.
#'
#' @format A [ggplot2::Position] object.
#'
#' @seealso [gf_resid()], which pairs this position with the plot's own jitter.
#' @noRd
PositionResidJitter <- ggplot2::ggproto(
  "PositionResidJitter", ggplot2::PositionJitter,
  outcome = NULL,
  compute_panel = function(self, data, params, scales) {
    jittered <- ggplot2::ggproto_parent(ggplot2::PositionJitter, self)$compute_panel(
      data[c("x", "y")], params, scales
    )
    axes <- setdiff(c("x", "y"), self$outcome)
    data[axes] <- jittered[axes]
    data
  }
)

position_resid_jitter <- function(width = NULL, height = NULL, seed = NA, outcome = NULL) {
  ggplot2::ggproto(
    NULL, PositionResidJitter, width = width, height = height, seed = seed, outcome = outcome
  )
}

#' The position a residual has to be drawn with, and the plot to draw it on
#'
#' An unseeded jitter cannot be reproduced by another layer. When needed, this
#' function gives the plot's first layer a seed and returns the matching
#' endpoint-preserving position for the residual layer.
#'
#' @param plot The plot the residual is being drawn on.
#' @param outcome The aesthetic (`"x"` or `"y"`) to hold at its pre-jitter
#'   value, or `NULL` to jitter both.
#'
#' @return A list with `plot` and `position`.
#'
#' @noRd
resid_jitter <- function(plot, outcome = NULL) {
  plain <- list(plot = plot, position = "identity")
  if (length(plot$layers) == 0) {
    return(plain)
  }
  pos <- plot$layers[[1]]$position
  if (!inherits(pos, "PositionJitter")) {
    return(plain)
  }

  seed <- pos$seed
  if (!isTRUE(is.finite(seed))) {
    seed <- sample.int(.Machine$integer.max, 1L)
    plot$layers[[1]] <- layer_with_position(
      plot$layers[[1]],
      ggplot2::position_jitter(width = pos$width, height = pos$height, seed = seed)
    )
  }

  list(
    plot = plot,
    position = position_resid_jitter(pos$width, pos$height, seed, outcome = outcome)
  )
}

#' Draw a residual as a segment from a prediction to an observation
#'
#' The segment is drawn from what the model predicts to what was observed, so
#' `x`/`y` is the observation and `xend`/`yend` the prediction until the moment
#' of drawing. Which of the two ends arrives says which axis the residual is
#' measured on. [geom_resid()] pairs it with [StatResid] and computes those
#' endpoints from a fitted model.
#'
#' @format A [ggplot2::Geom] object.
#'
#' @seealso [gf_resid()], the ggformula function for the same layer.
#' @export
GeomResid <- ggplot2::ggproto(
  "GeomResid", ggplot2::GeomSegment,
  required_aes = c("x", "y", "xend|yend"),
  handle_na = function(self, data, params) {
    # the inherited drop reads `required_aes` literally, and `"xend|yend"` is
    # not a column, so neither end would ever be checked: name the one that
    # arrived
    ggplot2::remove_missing(
      data, params$na.rm,
      c("x", "y", paste0(resid_axis(data), "end"), self$non_missing_aes),
      "geom_resid"
    )
  },
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        arrow.fill = NULL, lineend = "butt",
                        linejoin = "round", na.rm = FALSE) {
    segments <- if (identical(resid_axis(data), "y")) {
      transform(data, xend = x, y = yend, yend = y)
    } else {
      transform(data, yend = y, x = xend, xend = x)
    }
    ggplot2::GeomSegment$draw_panel(
      segments, panel_params, coord,
      arrow = arrow, arrow.fill = arrow.fill,
      lineend = lineend, linejoin = linejoin, na.rm = na.rm
    )
  }
)

#' Draw a residual as the square it would make
#'
#' The square is expanded here rather than in [StatResid] because a stat runs
#' before the position: four corners jittered one row at a time tear apart.
#' It is also the only place the panel's final ranges are known, and the
#' square is a square on the page rather than in data units.
#'
#' @format A [ggplot2::Geom] object.
#'
#' @seealso [geom_square_resid()] and [gf_square_resid()].
#' @export
GeomSquareResid <- ggplot2::ggproto(
  "GeomSquareResid", ggplot2::GeomPolygon,
  required_aes = c("x", "y", "xend|yend"),
  draw_panel = function(self, data, panel_params, coord, aspect = 4 / 6,
                        rule = "evenodd", lineend = "butt", linejoin = "round",
                        linemitre = 10, na.rm = FALSE) {
    keep <- c("x", "y", paste0(resid_axis(data), "end"))
    data <- data[stats::complete.cases(data[keep]), , drop = FALSE]
    if (nrow(data) == 0) {
      return(ggplot2::zeroGrob())
    }
    ggplot2::GeomPolygon$draw_panel(
      square_vertices(data, panel_params$x.range, panel_params$y.range, aspect),
      panel_params, coord,
      rule = rule, lineend = lineend, linejoin = linejoin, linemitre = linemitre
    )
  }
)


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
resid_end <- function(spec, model, call = caller_env()) {
  mspec <- model_spec(spec$data, model, call = call)
  outcome_axis <- names(spec$axes[spec$axes %in% mspec$outcome])
  if (length(outcome_axis) == 0) {
    axes <- purrr::imap_chr(spec$axes, function(variable, aes) glue("{aes} = {variable}"))
    abort(
      c(
        "A residual is measured along the axis carrying the model's outcome",
        glue("the model predicts: {collapse(mspec$outcome)}"),
        glue("the plot's axes are: {collapse(axes)}"),
        "plot the outcome this model predicts, or measure the model this plot was built for"
      ),
      call = call
    )
  }
  if (identical(outcome_axis, "x")) "xend" else "yend"
}

#' Build the positional mapping for a residual-family layer
#'
#' @param from `NULL` to use the plot's observed value as the starting point,
#'   or a column name such as `.grand` to replace that value.
#'
#' @noRd
resid_mapping <- function(spec, end, from = NULL) {
  mapping <- ggplot2::aes(yend = .data$.fitted)
  names(mapping) <- end
  # Keep the plot's quosures and their environments. Square layers cannot rely
  # on inherited positional mappings.
  mapping[c("x", "y")] <- spec$mapping[c("x", "y")]
  if (!is.null(from)) {
    axis <- sub("end$", "", end)
    mapping[[axis]] <- rlang::quo(.data[[from]])
  }
  mapping
}

#' Refuse a fit whose squares would not add up
#'
#' The area identity requires the residuals to be orthogonal to the fitted
#' values' reduction from the grand mean. Unweighted least squares with an
#' intercept guarantees that orthogonality. Weighted fits use a weighted inner
#' product instead, so their unweighted areas do not satisfy the identity.
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

  if (!no_intercept && !weighted) {
    return(invisible(model))
  }

  abort(
    c(
      glue("`{fn}()` draws a reduction that this model's own arithmetic does not support"),
      x = if (no_intercept) {
        "this model was fit without an intercept"
      } else {
        "this model was fit with weights"
      },
      "*" = paste(
        "the sums of the total, error and reduction squares only add up when residuals",
        "are orthogonal to the model's reduction from the grand mean"
      ),
      i = paste(
        if (no_intercept) "fit the model with its intercept" else "fit the model unweighted",
        "or measure it with `gf_resid()`, which needs no such identity"
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

#' Assemble residual data and mappings
#'
#' Keep the complete plot data so facet variables and omitted rows stay aligned
#' with the point layer.
#'
#' @param spec A `plot_spec()`.
#' @param fitted One prediction per row of `spec$data`.
#' @param end `"xend"` or `"yend"`: the axis the residual is measured along.
#'
#' @return A list with `data` (the plot's own data plus `.fitted`) and
#'   `aesthetics`.
#'
#' @noRd
resid_pieces <- function(spec, fitted, end) {
  data <- spec$data
  data$.fitted <- fitted
  list(data = data, aesthetics = resid_mapping(spec, end))
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
#' @return A list with `data` (the plot's own data plus `.fitted`) and
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
  check_resid_axes(spec, call = call)
  fitted <- resid_fitted(model, spec$data, call = call)
  resid_pieces(spec, fitted, resid_end(spec, model, call = call))
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
#' @return A list with `data` (the plot's own data plus `.fitted`) and
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
  check_resid_axes(spec, call = call)
  fitted <- fun(plot_x_values(spec))
  resid_pieces(spec, fitted, "yend")
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
#' @return A list with `data` (the plot's own data plus `.fitted` and
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
  check_resid_axes(spec, call = call)
  fitted <- resid_fitted(model, spec$data, call = call)
  end <- resid_end(spec, model, call = call)

  check_decomposable(model, fn, call = call)

  warn_empty_reduction(model, fn)

  grand <- reduction_grand(model)
  data <- spec$data
  data$.fitted <- fitted
  data$.grand <- grand
  list(data = data, aesthetics = resid_mapping(spec, end, from = ".grand"))
}

#' Adapt residual layer construction to `layer_factory()`
#'
#' Named `model` and `fun` arguments arrive in `params` but are inputs to layer
#' construction, not geom or stat parameters, so they are removed here. The
#' plot's positional quosures replace ggformula's copies to preserve their
#' original evaluation environments. `check.param` is passed through because
#' ggformula and ggplot2 intentionally use different defaults.
#'
#' @param tag The tag to name the layer with.
#' @param aesthetics The mapping `resid_spec()` or `resid_fun_spec()` computed,
#'   with its own quosures.
#'
#' @return A function with the formals `layer_factory()` expects. These formals
#'   must be explicit because `create_formals()` removes arguments hidden in
#'   `...`.
#'
#' @noRd
resid_layer_fun <- function(tag, aesthetics) {
  force(tag)
  force(aesthetics)
  function(geom, stat, position, params = NULL, mapping = NULL, data = NULL,
           check.param = FALSE, ...) {
    params[["model"]] <- NULL
    params[["fun"]] <- NULL
    mapping <- mapping %||% ggplot2::aes()
    mapping[names(aesthetics)] <- aesthetics
    orientation <- if ("xend" %in% names(aesthetics)) "y" else "x"
    resid_layer(
      geom = geom,
      stat = stat,
      position = position,
      mapping = mapping,
      data = data,
      params = params,
      check.param = check.param,
      tag = tag,
      orientation = orientation,
      reduction = tag %in% c("reduce", "square_reduce"),
      ...
    )
  }
}

#' The x values the plot draws, as the caller's function will be handed them
#'
#' Evaluating the plot's quosure preserves transformations such as
#' `~log(Height)` and the factor representation of a discrete x.
#'
#' @noRd
plot_x_values <- function(spec) eval_tidy(spec$mapping$x, spec$data)
