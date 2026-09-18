#' Resolve a model layer's orientation
#'
#' @noRd
model_orientation <- function(orientation, call = caller_env()) {
  if (length(orientation) == 1L && is.na(orientation)) {
    return(orientation)
  }
  if (!is.character(orientation) || length(orientation) != 1L ||
      !orientation %in% c("x", "y")) {
    abort('`orientation` must be one of "x" or "y".', call = call)
  }
  orientation
}


#' Build the ggplot2 layer shared by native and formula interfaces
#'
#' Supplied models are planned when the layer is added to a plot, where both
#' inherited and local mappings are available. Prepared rows and inferred models
#' go straight to the shared stat and geom.
#'
#' @noRd
model_layer <- function(mapping = NULL, data = NULL, geom = GeomModel,
                        stat = StatModel, position = "identity", params = list(),
                        model = NULL, orientation = NA, show.legend = NA,
                        inherit.aes = TRUE, tag = "model", prepared = FALSE,
                        fn = "geom_model", call = call2(fn), ...) {
  rlang::local_error_call(call2(fn))
  params <- normalize_linewidth(params, geom, fn, warn_size = fn != "gf_model")
  orientation <- model_orientation(orientation, call = call)
  if (!prepared && !is.null(model)) {
    if (is_formula(model) && is.null(f_lhs(model))) {
      abort(
        c(
          glue("`{fn}()` needs a two-sided model formula."),
          i = "Write the outcome on the left, as in `Thumb ~ Height`."
        ),
        call = call
      )
    }
    if (isTRUE(params$se)) {
      abort(glue("`{fn}()` supports `se = TRUE` only without a supplied `model`."), call = call)
    }
    layer <- ggplot2::layer(
      geom = geom, stat = stat, data = data, mapping = mapping,
      position = position, params = params, inherit.aes = inherit.aes,
      show.legend = show.legend, ...
    )
    layer <- public_layer_constructor(layer, fn)
    attr(layer, "model_spec") <- list(
      model = model, mapping = mapping, data = data, geom = geom, stat = stat,
      position = position, params = params, orientation = orientation,
      inherit.aes = inherit.aes, show.legend = show.legend, tag = tag,
      fn = fn, call = call, dots = list(...)
    )
    class(layer) <- c("coursekata_model_layer", class(layer))
    return(layer)
  }
  params[["model"]] <- NULL
  params[["orientation"]] <- orientation

  layer <- ggplot2::layer(
    geom = geom, stat = stat, data = data, mapping = mapping,
    position = position, params = params, inherit.aes = inherit.aes,
    show.legend = show.legend, ...
  )
  layer <- public_layer_constructor(layer, fn)
  layer <- with_model_layer_fit(layer, model)
  layer <- if (is.null(tag)) layer else tag_layer(layer, tag)
  if (!prepared && is.null(model)) {
    source_layer(layer, inherit.data = is.null(data) || ggplot2::is_waiver(data))
  } else {
    layer
  }
}

#' Resolve an explicit model against its destination plot
#'
#' @noRd
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.coursekata_model_layer <- function(object, plot, ...) {
  request <- attr(object, "model_spec")
  plot <- stabilize_source_data(plot)
  binding <- source_layer_binding(
    plot, request$mapping, request$data, request$inherit.aes
  )
  mapping <- source_mapping(binding$mapping, plot$mapping, binding$inherit.aes)
  # A fitted model must apply the reader's original expression to its
  # prediction grid, not a hidden storage column pinned to observation rows.
  current <- plot_spec(plot)
  restored <- model_plot_spec(plot)
  for (aesthetic in names(restored$pins)) {
    if (identical(mapping[[aesthetic]], current$mapping[[aesthetic]])) {
      mapping[[aesthetic]] <- restored$mapping[[aesthetic]]
    }
  }
  if (!any(c("x", "y") %in% names(mapping))) {
    abort(glue("`{request$fn}()` needs a positional mapping; map its predictor with `aes()`."),
          call = request$call)
  }
  data <- binding$data
  if (is.null(data) || ggplot2::is_waiver(data)) {
    data <- plot$data
  } else if (is.function(data) || is_formula(data)) {
    data <- with_random_seed_restored(as_function(data)(plot$data))
  }
  data <- ggplot2::fortify(data)

  # An explicit local predictor is sufficient for a standalone fitted layer.
  # An existing outcome mapping, however, must name this model's outcome.
  outcome <- all.vars(stats::formula(request$model)[[2L]])
  axis <- if (identical(request$orientation, "y")) "x" else "y"
  if (is.null(mapping[[axis]]) && length(outcome) == 1L &&
      !any(outcome %in% label_columns(vapply(mapping, as_label, character(1))))) {
    mapping[[axis]] <- new_quosure(sym(outcome), base_env())
  }
  context <- ggplot2::ggplot(data, mapping)
  spec <- model_layer_spec(context, request$model, request$params,
                           fn = request$fn, call = request$call)
  if (!is.na(request$orientation) && !identical(request$orientation, spec$orientation)) {
    abort(glue("`{request$fn}()`'s `orientation` conflicts with the model's outcome axis."),
          call = request$call)
  }
  layer <- rlang::exec(
    model_layer, mapping = spec$aesthetics, data = spec$data,
    geom = request$geom, stat = request$stat, position = request$position,
    params = spec$params, model = spec$model, orientation = spec$orientation,
    inherit.aes = binding$inherit.aes && spec$inherit,
    show.legend = request$show.legend, tag = request$tag, prepared = TRUE,
    fn = request$fn, call = request$call,
    !!!request$dots
  )
  ggplot2::ggplot_add(layer, plot, ...)
}

#' Draw fitted and implied models with ggplot2
#'
#' `geom_model()` adds a model to an ordinary [ggplot2::ggplot()]. A supplied
#' `model` may be a fit from [stats::lm()] or [stats::aov()], or a two-sided
#' formula that is fitted once against the layer data. With no `model`, the
#' layer draws the model implied by its mapped positions, separately in each
#' panel and group.
#'
#' Empty models draw an intercept, numeric predictors draw fitted lines, and
#' categorical predictors draw one short mark per group. A model with one
#' numeric and one categorical predictor draws one line per category.
#'
#' A supplied model is one fixed claim evaluated on a prediction grid built
#' from the layer data.
#'
#' The displayed predictor is read from the plot mapping or this layer's local
#' mapping. One additional predictor may be present in the data: categories
#' produce separate traces, and numeric values use the mean and mean plus or
#' minus one standard deviation. Outcome-axis expressions such as `log(y)` are
#' applied to the predictions. A supplied model is prepared once when added to
#' the plot, including when `data` is a function or formula.
#'
#' Override unrelated inherited aesthetics locally, or set `inherit.aes = FALSE`
#' and supply a predictor mapping such as `aes(x = Height)`. Without explicit
#' data or positions, the layer follows the first observation layer, preferring
#' points, so a model describes the rows and axes the plot actually shows.
#'
#' @param mapping,data,position,show.legend,inherit.aes See
#'   [ggplot2::geom_smooth()]. `data` may be a data frame, a function, or a
#'   one-sided formula.
#' @param stat The statistical transformation. Defaults to `"model"`.
#' @param geom The geometric object. `stat_model()` defaults to `"model"`.
#' @param model A fitted `lm` or `aov`, a two-sided model formula, or `NULL` to
#'   draw the model implied by the mapped positions.
#' @param orientation Layer orientation. `"x"` puts the outcome on y; `"y"`
#'   puts it on x. `NA` uses the mapped model outcome for a supplied model, and
#'   ggplot2's orientation rules for an inferred model.
#' @param na.rm If `FALSE`, missing values are removed with a warning. If
#'   `TRUE`, they are removed silently.
#' @param ... Fixed aesthetics and other layer parameters. For an inferred
#'   continuous model these include `formula`, `se`, `n`, `fullrange`, `level`,
#'   and `method.args`, with the meanings used by [ggplot2::stat_smooth()].
#'   `width` controls categorical model marks. With a supplied model, `n`
#'   controls the prediction grid. `se = TRUE` requires an inferred continuous
#'   model; it is refused when `model` is supplied.
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' fit <- lm(Thumb ~ Height, data = Fingers)
#' ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
#'   ggplot2::geom_point() +
#'   geom_model(model = fit)
#'
#' group_fit <- lm(Thumb ~ Sex, data = Fingers)
#' ggplot2::ggplot(Fingers, ggplot2::aes(Sex, Thumb)) +
#'   ggplot2::geom_jitter(width = 0.1) +
#'   geom_model(model = group_fit)
#'
#' @name geom_model
NULL

#' @rdname geom_model
#' @export
geom_model <- function(mapping = NULL, data = NULL, stat = "model",
                       position = "identity", ..., model = NULL,
                       orientation = NA, na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE) {
  model_layer(
    mapping = mapping, data = data, geom = GeomModel, stat = stat,
    position = position, params = rlang::list2(na.rm = na.rm, ...),
    model = model, orientation = orientation, show.legend = show.legend,
    inherit.aes = inherit.aes, fn = "geom_model"
  )
}

#' @rdname geom_model
#' @export
stat_model <- function(mapping = NULL, data = NULL, geom = "model",
                       position = "identity", ..., model = NULL,
                       orientation = NA, na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE) {
  model_layer(
    mapping = mapping, data = data, geom = geom, stat = StatModel,
    position = position, params = rlang::list2(na.rm = na.rm, ...),
    model = model, orientation = orientation, show.legend = show.legend,
    inherit.aes = inherit.aes, fn = "stat_model"
  )
}
