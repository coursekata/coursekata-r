#' Prepare complete source rows before ggplot2 evaluates aesthetics
#'
#' Both interfaces validate axes, predict, validate the outcome axis, and then
#' validate the reduction. Keep that order explicit: compound-invalid inputs
#' must give the same first diagnostic. No grand mean is computed on a refused
#' model, and prediction never compacts rows omitted by the fit.
#' @noRd
resid_layer_spec <- function(data, mapping, model = NULL, fun = NULL,
                             orientation = NA, reduction = FALSE,
                             fn = "geom_resid", labels = NULL, call = caller_env()) {
  labels <- labels %||% vapply(mapping, source_mapping_label, character(1))
  spec <- list(data = data, mapping = mapping, labels = labels,
               axes = labels[intersect(c("x", "y"), names(labels))])
  check_resid_axes(spec, call = call)
  direction <- layer_orientation(orientation, default = "x", call = call)
  fitted <- if (is.null(fun)) {
    resid_fitted(model, data, call = call)
  } else {
    axis <- if (identical(direction, "x")) "x" else "y"
    fun(with_random_seed_restored(eval_tidy(mapping[[axis]], data)))
  }
  data$.fitted <- if (length(fitted) == 1L) rep(fitted, nrow(data)) else fitted
  if (is.null(fun)) {
    inferred <- if (identical(resid_end(spec, model, call, orientation), "xend")) "y" else "x"
    if (!is.na(orientation) && !identical(direction, inferred)) {
      abort("`orientation` must put the model's outcome on its mapped axis.", call = call)
    }
    direction <- inferred
  }
  check_resid_live_mappings(mapping, data, call = call)
  if (reduction) {
    check_decomposable(model, fn, call = call)
    if (is.null(model$model)) {
      abort("A reduction needs the model's fitted data; refit with `model = TRUE`.", call = call)
    }
    warn_empty_reduction(model, fn)
    data$.grand <- rep(reduction_grand(model), nrow(data))
  }
  list(data = data,
       aesthetics = resid_layer_mapping(mapping, direction, reduction),
       orientation = direction, reduction = reduction, fn = fn)
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
  mapping[c("xend", "yend")] <- NULL
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
                        call = caller_env(), fn = "geom_resid", ...) {
  rlang::local_error_call(call2(fn))
  params <- normalize_linewidth(params, geom, fn)
  params <- resid_geom_defaults(params, geom, mapping)
  orientation <- layer_orientation(orientation, default = "x", call = call)
  position <- resid_layer_position(
    position, orientation = orientation, reduction = reduction, call = call
  )
  params$orientation <- orientation
  if (!is.null(fn) && !is.null(resid_geom_kind(geom))) {
    params$.resid_fn <- fn
  }

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
  layer <- public_layer_constructor(layer, fn)
  if (is.null(tag)) layer else tag_layer(layer, tag)
}

resid_geom_defaults <- function(params, geom, mapping = NULL) {
  kind <- resid_geom_kind(geom)
  if (identical(kind, "line") && is.null(params$linewidth) && is.null(mapping$linewidth)) {
    params$linewidth <- 0.2
  }
  if (identical(kind, "square") && is.null(params$alpha)) {
    params$alpha <- 0.1
  }
  params
}

resid_geom_kind <- function(geom) {
  if (identical(geom, "resid") || inherits(geom, "GeomResid")) return("line")
  if (identical(geom, "square_resid") || inherits(geom, "GeomSquareResid")) return("square")
  NULL
}

resid_layer_tag <- function(geom, reduction = FALSE) {
  kind <- resid_geom_kind(geom)
  if (identical(kind, "square")) {
    if (reduction) "square_reduce" else "square_resid"
  } else {
    if (reduction) "reduce" else "resid"
  }
}

model_resid_layer <- function(fn, mapping, data, geom, stat, position, params,
                              model, orientation, reduction, show.legend,
                              inherit.aes, call = call2(fn), fun = NULL) {
  if (is.null(model) && is.null(fun)) {
    abort(glue("`{fn}()` needs a fitted `model`."), call = call)
  }
  if (!is.null(model) && !is.null(fun)) {
    abort("Supply only one of `model` and `fun`.", call = call)
  }
  if (reduction && !is.null(fun)) {
    abort("A reduction needs a fitted `model`; `fun` only defines residuals.", call = call)
  }
  if (ggplot2::is_waiver(data)) data <- NULL
  direction <- layer_orientation(orientation, default = "x", call = call)
  position <- resid_layer_position(position, direction, reduction, call)
  layer <- resid_layer(
    fn = fn,
    mapping = mapping,
    data = data,
    geom = geom,
    stat = stat,
    position = position,
    params = params,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    tag = resid_layer_tag(geom, reduction),
    orientation = direction,
    reduction = reduction,
    call = call
  )
  # setup_layer receives both the full rows and the current plot mapping.
  # Preparing here preserves native mapping and data additions, including when a
  # layer object is reused. The parent still owns ordinary ggplot2 inheritance.
  parent <- layer
  position <- layer$position
  setup <- function(self, data, plot) {
    data <- ggplot2::ggproto_parent(parent, self)$setup_layer(data, plot)
    mapping <- self$computed_mapping
    labels <- vapply(mapping, source_mapping_label, character(1))
    pins <- plot_pins(plot)
    for (axis in intersect(names(pins), names(mapping))) {
      if (identical(mapping[[axis]], plot_source(plot, resolve.data = FALSE)$mapping[[axis]])) {
        labels[[axis]] <- source_mapping_label(pins[[axis]])
      }
    }
    prepared <- resid_layer_spec(data, mapping, model, fun, orientation,
                                 reduction, fn, labels, call)
    self$computed_mapping <- prepared$aesthetics
    self$stat_params$orientation <- prepared$orientation
    self$position <- resid_layer_position(position, prepared$orientation, reduction, call)
    if (inherits(self$position, "PositionResidJitter") && reduction) {
      self$position <- position_resid_jitter(position$width, position$height, position$seed,
        outcome = if (prepared$orientation == "x") "y" else "x")
    }
    prepared$data
  }
  layer <- source_layer(layer_with(layer, setup_layer = setup,
                           constructor = rlang::call2(fn)), inherit.data = is.null(data))
  class(layer) <- c("coursekata_resid_layer", class(layer))
  layer
}

#' Residual and reduction layers for ggplot2
#'
#' These layers measure a fitted model directly from an ordinary [ggplot2::ggplot()].
#' A residual segment runs from the model's prediction to the observed value;
#' an arrow at its last end points to the observation. A reduction segment runs
#' from the prediction to the model's grand mean. The square variants draw
#' areas proportional to those squared distances at a shared aspect ratio.
#'
#' When the observations have their own data or mappings, these layers follow
#' the first point layer (or the first non-annotation layer when there are no
#' points). Explicit layer `data` and `mapping` arguments take precedence.
#'
#' With `orientation = NA`, a model's outcome determines the direction. The
#' outcome expression must match an axis exactly: `log(y)` is refused for a
#' model of `y`, because that distance is not the model's residual. An explicit
#' `orientation = "x"` puts the outcome on y; `"y"` puts it on x and must agree
#' with that mapping. Coordinate systems such as
#' [ggplot2::coord_flip()] are applied later and do not change this argument.
#'
#' A jittered point layer and its model layer must use the same
#' [ggplot2::position_jitter()] object with a numeric seed. The model layer keeps
#' the fitted endpoint fixed while moving the observed endpoint by the same
#' amount as its point.
#'
#' Positional expressions are evaluated reproducibly on the current data, so
#' random mappings give the observations and residuals the same coordinates.
#' Native layers still respond to later data and mapping changes. A new unseeded
#' random mapping must be added before the residual layer, or carry its own
#' fixed seed. Checks that need the rows or mappings run when the plot is built:
#' axes, prediction,
#' outcome axis, then reduction eligibility.
#'
#' Native square layers inherit mapped aesthetics by default, including colour.
#' [gf_square_resid()] and [gf_square_reduce()] default to `inherit = FALSE`
#' for neutral square outlines. Set their `inherit = TRUE` to match this API.
#'
#' Reduction layers express the ordinary least-squares sum-of-squares identity.
#' They require an unweighted model with an intercept, no offset, and its stored model frame.
#' On the fitted observations, the identity holds across the sums of the square
#' areas, not separately for each observation or for new prediction data.
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
#' @param fun For residual layers, a function instead of `model`. It receives
#'   the mapped predictor values and returns predicted outcomes. The default
#'   orientation predicts y from x; `orientation = "y"` predicts x from y.
#'   Supply only one of `model` and `fun`. Reductions require a fitted model.
#' @param orientation Layer orientation. `NA` infers a model's outcome axis;
#'   `"x"` puts it on y and `"y"` puts it on x. For `fun`, `NA` means `"x"`.
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
                       position = "identity", ..., model = NULL, fun = NULL,
                       orientation = NA, linewidth = 0.2, na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE) {
  model_resid_layer(
    fn = "geom_resid",
    mapping = mapping,
    data = data,
    geom = GeomResid,
    stat = stat,
    position = position,
    params = rlang::list2(
      na.rm = na.rm, linewidth = if (missing(linewidth)) NULL else linewidth, ...
    ),
    model = model, fun = fun,
    orientation = orientation,
    reduction = FALSE,
    inherit.aes = inherit.aes,
    show.legend = show.legend
  )
}

#' @rdname geom_resid
#' @export
geom_square_resid <- function(mapping = NULL, data = NULL, stat = "resid",
                              position = "identity", ..., model = NULL, fun = NULL,
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
    model = model, fun = fun,
    orientation = orientation,
    reduction = FALSE,
    inherit.aes = inherit.aes,
    show.legend = show.legend
  )
}

#' @rdname geom_resid
#' @export
geom_reduce <- function(mapping = NULL, data = NULL, stat = "reduce",
                        position = "identity", ..., model = NULL, fun = NULL,
                        orientation = NA, linewidth = 0.2, na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE) {
  model_resid_layer(
    fn = "geom_reduce",
    mapping = mapping,
    data = data,
    geom = GeomResid,
    stat = stat,
    position = position,
    params = rlang::list2(
      na.rm = na.rm, linewidth = if (missing(linewidth)) NULL else linewidth, ...
    ),
    model = model, fun = fun,
    orientation = orientation,
    reduction = TRUE,
    inherit.aes = inherit.aes,
    show.legend = show.legend
  )
}

#' @rdname geom_resid
#' @export
geom_square_reduce <- function(mapping = NULL, data = NULL, stat = "reduce",
                               position = "identity", ..., model = NULL, fun = NULL,
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
    model = model, fun = fun,
    orientation = orientation,
    reduction = TRUE,
    inherit.aes = inherit.aes,
    show.legend = show.legend
  )
}

#' @rdname geom_resid
#' @export
stat_resid <- function(mapping = NULL, data = NULL, geom = "resid",
                       position = "identity", ..., model = NULL, fun = NULL,
                       orientation = NA, na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE) {
  params <- rlang::list2(na.rm = na.rm, ...)
  model_resid_layer(
    fn = "stat_resid",
    mapping = mapping,
    data = data,
    geom = geom,
    stat = StatResid,
    position = position,
    params = params,
    model = model, fun = fun,
    orientation = orientation,
    reduction = FALSE,
    inherit.aes = inherit.aes,
    show.legend = show.legend
  )
}

#' @rdname geom_resid
#' @export
stat_reduce <- function(mapping = NULL, data = NULL, geom = "resid",
                        position = "identity", ..., model = NULL, fun = NULL,
                        orientation = NA, na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE) {
  params <- rlang::list2(na.rm = na.rm, ...)
  model_resid_layer(
    fn = "stat_reduce",
    mapping = mapping,
    data = data,
    geom = geom,
    stat = StatReduce,
    position = position,
    params = params,
    model = model, fun = fun,
    orientation = orientation,
    reduction = TRUE,
    inherit.aes = inherit.aes,
    show.legend = show.legend
  )
}
