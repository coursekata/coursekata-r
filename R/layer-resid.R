#' Resolve the direction a model layer runs
#'
#' ggplot2 uses `"x"` for a layer whose outcome is on y and `"y"` for the
#' turned version. A missing orientation takes the usual `"x"` default.
#'
#' @param orientation `NA`, `"x"`, or `"y"`.
#' @param call The calling environment or call.
#'
#' @return `"x"` or `"y"`.
#'
#' @noRd
resid_orientation <- function(orientation, call = caller_env()) {
  if (length(orientation) == 1 && is.na(orientation)) {
    return("x")
  }
  if (!is.character(orientation) || length(orientation) != 1 ||
      !orientation %in% c("x", "y")) {
    abort('`orientation` must be one of "x" or "y".', call = call)
  }
  orientation
}

#' Add model predictions to the complete data a layer receives
#'
#' A ggplot2 stat sees mapped aesthetics, not the complete source rows. Model
#' prediction has to happen one step earlier because a model can use columns
#' that the plot never maps. A layer data function is that step: ggplot2 hands
#' it the complete plot data before evaluating the layer's aesthetics.
#'
#' @param data A layer's `data` argument.
#' @param model A fitted model.
#' @param reduction Whether to add the grand mean used by a reduction.
#' @param call The public call to name in an error raised during a later build.
#'
#' @return A data frame or a function from plot data to a data frame, matching
#'   the form of `data` that [ggplot2::layer()] accepts.
#'
#' @noRd
resid_layer_data <- function(data, model, reduction = FALSE, call = caller_env()) {
  add_predictions <- function(rows) {
    rows$.fitted <- resid_fitted(model, rows, call = call)
    if (reduction) {
      rows$.grand <- reduction_grand(model)
    }
    rows
  }

  if (is.null(data)) {
    return(add_predictions)
  }
  if (is.function(data) || rlang::is_formula(data)) {
    data_fun <- rlang::as_function(data)
    return(function(plot_data) add_predictions(data_fun(plot_data)))
  }
  add_predictions(data)
}

#' State the endpoint mappings a residual or reduction owns
#'
#' @param mapping A ggplot2 aesthetic mapping.
#' @param orientation `"x"` or `"y"`.
#' @param reduction Whether the layer starts at the grand mean.
#'
#' @return `mapping` with the layer-owned positional aesthetics installed.
#'
#' @noRd
resid_layer_mapping <- function(mapping, orientation, reduction = FALSE) {
  mapping <- mapping %||% ggplot2::aes()
  owned <- if (identical(orientation, "x")) {
    if (reduction) {
      ggplot2::aes(y = .data$.grand, yend = .data$.fitted)
    } else {
      ggplot2::aes(yend = .data$.fitted)
    }
  } else {
    if (reduction) {
      ggplot2::aes(x = .data$.grand, xend = .data$.fitted)
    } else {
      ggplot2::aes(xend = .data$.fitted)
    }
  }
  mapping[names(owned)] <- owned
  mapping
}

#' Make a normal jitter safe for model endpoints
#'
#' A regular ggplot2 jitter moves every positional aesthetic, including
#' `xend` and `yend`. A residual needs its observed end to move with the point
#' while its prediction stays on the model. A reduction also holds the grand
#' mean's axis fixed. The caller still supplies ggplot2's own position object;
#' the specialized copy remains an implementation detail.
#'
#' @param position A ggplot2 position or its string name.
#' @param orientation `"x"` or `"y"`.
#' @param reduction Whether the layer is a reduction.
#' @param call The public call.
#'
#' @return A ggplot2 position.
#'
#' @noRd
resid_layer_position <- function(position, orientation, reduction = FALSE,
                                 call = caller_env()) {
  if (inherits(position, "PositionResidJitter")) {
    return(position)
  }
  if (is.character(position) && length(position) == 1 &&
      grepl("jitter", position, fixed = TRUE)) {
    abort(
      c(
        "Residual and reduction layers need a fixed jitter seed to stay on their points.",
        i = "Pass `position = position_jitter(..., seed = 1)` to both layers."
      ),
      call = call
    )
  }
  if (inherits(position, "PositionJitterdodge")) {
    abort(
      c(
        "Residual and reduction layers do not support `position_jitterdodge()`.",
        i = "Use a seeded `position_jitter()` for the points and model layer."
      ),
      call = call
    )
  }
  if (!inherits(position, "PositionJitter")) {
    return(position)
  }

  seed <- position$seed
  if (!isTRUE(is.finite(seed))) {
    abort(
      c(
        "Residual and reduction layers need a fixed jitter seed to stay on their points.",
        i = "Give `position_jitter()` a numeric `seed` and use that position for both layers."
      ),
      call = call
    )
  }

  outcome <- if (reduction) {
    if (identical(orientation, "x")) "y" else "x"
  } else {
    NULL
  }
  position_resid_jitter(
    width = position$width,
    height = position$height,
    seed = seed,
    outcome = outcome
  )
}

#' Build a residual-family ggplot2 layer
#'
#' This is the one layer constructor used by the ggplot2 functions and by
#' the adapters behind the `gf_` functions.
#'
#' @noRd
resid_layer <- function(mapping = NULL, data = NULL, geom, stat,
                        position = "identity", params = list(),
                        inherit.aes = TRUE, check.aes = TRUE,
                        check.param = TRUE, show.legend = NA,
                        tag = NULL, orientation = NA, reduction = FALSE,
                        call = caller_env(), ...) {
  orientation <- resid_orientation(orientation, call = call)
  position <- resid_layer_position(
    position, orientation = orientation, reduction = reduction, call = call
  )
  params$orientation <- orientation

  layer <- ggplot2::layer(
    geom = geom,
    stat = stat,
    data = data,
    mapping = mapping,
    position = position,
    params = params,
    inherit.aes = inherit.aes,
    check.aes = check.aes,
    check.param = check.param,
    show.legend = show.legend,
    ...
  )
  if (is.null(tag)) layer else tag_layer(layer, tag)
}

resid_geom_defaults <- function(params, geom) {
  is_line <- identical(geom, "resid") || inherits(geom, "GeomResid")
  is_square <- identical(geom, "square_resid") || inherits(geom, "GeomSquareResid")
  if (is_line && is.null(params$linewidth)) {
    params$linewidth <- 0.2
  }
  if (is_square && is.null(params$alpha)) {
    params$alpha <- 0.1
  }
  params
}

model_resid_layer <- function(fn, mapping, data, geom, stat, position, params,
                              model, orientation, reduction, show.legend,
                              inherit.aes, call) {
  if (is.null(model)) {
    abort(glue("`{fn}()` needs a fitted `model`."), call = call)
  }
  orientation <- resid_orientation(orientation, call = call)
  if (reduction) {
    check_decomposable(model, fn, call = call)
    warn_empty_reduction(model, fn)
  }

  resid_layer(
    mapping = resid_layer_mapping(mapping, orientation, reduction = reduction),
    data = resid_layer_data(data, model, reduction = reduction, call = call),
    geom = geom,
    stat = stat,
    position = position,
    params = params,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    tag = if (reduction) {
      if (inherits(geom, "GeomSquareResid") || identical(geom, "square_resid")) {
        "square_reduce"
      } else {
        "reduce"
      }
    } else if (inherits(geom, "GeomSquareResid") || identical(geom, "square_resid")) {
      "square_resid"
    } else {
      "resid"
    },
    orientation = orientation,
    reduction = reduction,
    call = call
  )
}

#' Residual and reduction layers for ggplot2
#'
#' These layers measure a fitted model directly from an ordinary [ggplot2::ggplot()].
#' A residual runs from an observed value to the model's prediction. A reduction
#' runs from the model's grand mean to that prediction. The square variants draw
#' the same distances as areas.
#'
#' Supply `orientation = "y"` when the model's outcome is mapped to x. With the
#' default `orientation = NA`, the layer follows ggplot2's usual x orientation:
#' x is the predictor axis and the outcome is on y. Coordinate systems such as
#' [ggplot2::coord_flip()] are applied later and do not change this argument.
#'
#' A jittered point layer and its model layer must use the same
#' [ggplot2::position_jitter()] object with a numeric seed. The model layer keeps
#' the fitted endpoint fixed while moving the observed endpoint by the same
#' amount as its point.
#'
#' Reduction layers express the ordinary least-squares sum-of-squares identity.
#' They require an unweighted model with an intercept. The identity holds across
#' the sums of the square areas, not separately for each observation.
#'
#' @param mapping,data,position,show.legend,inherit.aes See
#'   [ggplot2::geom_segment()]. `data` may also be a function or one-sided
#'   formula; predictions are added after that function has selected its rows.
#' @param stat The statistical transformation to use. The geom constructors use
#'   `"resid"` or `"reduce"` by default.
#' @param geom The geometric object to use. The stat constructors use
#'   `"resid"` by default; use `"square_resid"` to draw areas.
#' @param model A model already fit by [stats::lm()] or [stats::aov()]. Name this
#'   argument in a ggplot2 call, as in `geom_resid(model = fit)`.
#' @param orientation Layer orientation. `NA` and `"x"` put the model's outcome
#'   on y; `"y"` puts it on x.
#' @param linewidth The line width. The default is `0.2`.
#' @param aspect The square's aspect ratio. The default is `4 / 6`.
#' @param alpha The square's transparency. The default is `0.1`.
#' @param na.rm If `FALSE`, the default, missing observations are removed with a
#'   warning. If `TRUE`, they are removed silently.
#' @param ... Other arguments passed to [ggplot2::layer()]. These are usually
#'   fixed aesthetics such as `colour`, `fill`, `alpha`, or `linetype`.
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' model <- lm(Thumb ~ Height, data = Fingers)
#' ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
#'   ggplot2::geom_point() +
#'   geom_resid(model = model, colour = "firebrick")
#'
#' ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
#'   ggplot2::geom_point() +
#'   geom_square_reduce(model = model, fill = "forestgreen")
#'
#' jitter <- ggplot2::position_jitter(width = 0.1, seed = 42)
#' group_model <- lm(Thumb ~ Sex, data = Fingers)
#' ggplot2::ggplot(Fingers, ggplot2::aes(Sex, Thumb)) +
#'   ggplot2::geom_point(position = jitter) +
#'   geom_resid(model = group_model, position = jitter)
#'
#' @name geom_resid
NULL

#' @rdname geom_resid
#' @export
geom_resid <- function(mapping = NULL, data = NULL, stat = "resid",
                       position = "identity", ..., model = NULL,
                       orientation = NA, linewidth = 0.2, na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE) {
  model_resid_layer(
    fn = "geom_resid",
    mapping = mapping,
    data = data,
    geom = GeomResid,
    stat = stat,
    position = position,
    params = rlang::list2(na.rm = na.rm, linewidth = linewidth, ...),
    model = model,
    orientation = orientation,
    reduction = FALSE,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    call = caller_env()
  )
}

#' @rdname geom_resid
#' @export
geom_square_resid <- function(mapping = NULL, data = NULL, stat = "resid",
                              position = "identity", ..., model = NULL,
                              orientation = NA, aspect = 4 / 6, alpha = 0.1,
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
  model_resid_layer(
    fn = "geom_square_resid",
    mapping = mapping,
    data = data,
    geom = GeomSquareResid,
    stat = stat,
    position = position,
    params = rlang::list2(na.rm = na.rm, aspect = aspect, alpha = alpha, ...),
    model = model,
    orientation = orientation,
    reduction = FALSE,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    call = caller_env()
  )
}

#' @rdname geom_resid
#' @export
geom_reduce <- function(mapping = NULL, data = NULL, stat = "reduce",
                        position = "identity", ..., model = NULL,
                        orientation = NA, linewidth = 0.2, na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE) {
  model_resid_layer(
    fn = "geom_reduce",
    mapping = mapping,
    data = data,
    geom = GeomResid,
    stat = stat,
    position = position,
    params = rlang::list2(na.rm = na.rm, linewidth = linewidth, ...),
    model = model,
    orientation = orientation,
    reduction = TRUE,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    call = caller_env()
  )
}

#' @rdname geom_resid
#' @export
geom_square_reduce <- function(mapping = NULL, data = NULL, stat = "reduce",
                               position = "identity", ..., model = NULL,
                               orientation = NA, aspect = 4 / 6, alpha = 0.1,
                               na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE) {
  model_resid_layer(
    fn = "geom_square_reduce",
    mapping = mapping,
    data = data,
    geom = GeomSquareResid,
    stat = stat,
    position = position,
    params = rlang::list2(na.rm = na.rm, aspect = aspect, alpha = alpha, ...),
    model = model,
    orientation = orientation,
    reduction = TRUE,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    call = caller_env()
  )
}

#' @rdname geom_resid
#' @export
stat_resid <- function(mapping = NULL, data = NULL, geom = "resid",
                       position = "identity", ..., model = NULL,
                       orientation = NA, na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE) {
  params <- resid_geom_defaults(rlang::list2(na.rm = na.rm, ...), geom)
  model_resid_layer(
    fn = "stat_resid",
    mapping = mapping,
    data = data,
    geom = geom,
    stat = StatResid,
    position = position,
    params = params,
    model = model,
    orientation = orientation,
    reduction = FALSE,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    call = caller_env()
  )
}

#' @rdname geom_resid
#' @export
stat_reduce <- function(mapping = NULL, data = NULL, geom = "resid",
                        position = "identity", ..., model = NULL,
                        orientation = NA, na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE) {
  params <- resid_geom_defaults(rlang::list2(na.rm = na.rm, ...), geom)
  model_resid_layer(
    fn = "stat_reduce",
    mapping = mapping,
    data = data,
    geom = geom,
    stat = StatReduce,
    position = position,
    params = params,
    model = model,
    orientation = orientation,
    reduction = TRUE,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    call = caller_env()
  )
}
