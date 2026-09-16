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

#' Find the model predictor carried by the displayed axis
#'
#' A local axis mapping is authoritative. Without one, the native layer uses
#' the only predictor, or the only numeric predictor in a mixed model. More
#' ambiguous arrangements need a local mapping because a ggplot2 layer cannot
#' inspect the plot mapping that it will later inherit.
#'
#' @noRd
model_axis_column <- function(mspec, mapping, orientation,
                              fn = "geom_model", call = caller_env()) {
  predictors <- label_columns(mspec$predictors)
  if (length(predictors) == 0L) return(NULL)

  axis <- if (identical(orientation, "x")) "x" else "y"
  mapped <- mapping[[axis]]
  if (!is.null(mapped)) {
    columns <- intersect(label_columns(as_label(quo_get_expr(mapped))), predictors)
    if (length(columns) == 1L) return(columns)
    abort(
      c(
        glue("`{fn}()` cannot identify one model predictor in the `{axis}` mapping."),
        i = "Map the displayed predictor directly in this layer."
      ),
      call = call
    )
  }

  if (length(predictors) == 1L) return(predictors)
  abort(
    c(
      glue("`{fn}()` cannot choose which model predictor belongs on the axis."),
      i = glue("Model predictors: {collapse(predictors)}"),
      i = glue("Map the displayed predictor in this layer with `aes({axis} = ...)`.")
    ),
    call = call
  )
}

#' Values used to build a model prediction grid
#'
#' A numeric predictor on the displayed axis spans its observed range. An
#' off-axis numeric predictor uses its mean and mean plus or minus one standard
#' deviation. Factors retain their levels and ordering.
#'
#' @noRd
model_grid_values <- function(values, dense = FALSE, n = 80L) {
  if (is.logical(values)) return(c(TRUE, FALSE))
  if (!is.numeric(values)) {
    observed <- levels(factor(values))
    if (is.factor(values)) {
      return(factor(
        observed,
        levels = levels(values),
        ordered = is.ordered(values)
      ))
    }
    return(observed)
  }
  if (dense) {
    range <- range(values, na.rm = TRUE)
    return(seq(range[[1]], range[[2]], length.out = n))
  }
  middle <- mean(values, na.rm = TRUE)
  spread <- stats::sd(values, na.rm = TRUE)
  unique(c(middle - spread, middle, middle + spread))
}

#' Prepare a named model for the shared model stat and geom
#'
#' @noRd
prepare_model_layer_data <- function(data, model, mapping, orientation,
                                     n = NULL, fn = "geom_model",
                                     call = caller_env()) {
  mspec <- model_spec(data, model, call = call)
  model_columns <- label_columns(mspec$terms)
  missing <- setdiff(model_columns, names(data))
  if (length(missing) > 0L) {
    abort(
      c(
        "The model uses variables that do not exist in the layer data.",
        i = glue("Missing: {collapse(missing)}")
      ),
      call = call
    )
  }
  if (length(mspec$outcome) != 1L || !is.name(str2lang(mspec$outcome))) {
    abort(
      c(
        glue("`{fn}()` supports one untransformed outcome variable."),
        i = "Create the transformed outcome as a column before fitting the model."
      ),
      call = call
    )
  }
  check_numeric_outcome(mspec$outcome, data[[mspec$outcome]], call)

  points <- if (is.null(n)) {
    min(max(nrow(data), 80L), model_grid_max_points)
  } else {
    if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 2 || n != as.integer(n)) {
      abort("`n` must be one integer greater than 1.", call = call)
    }
    as.integer(n)
  }

  focal <- model_axis_column(
    mspec, mapping, orientation, fn = fn, call = call
  )
  predictors <- label_columns(mspec$predictors)
  secondary <- setdiff(predictors, focal %||% character())
  if (length(secondary) > 1L) {
    abort(
      c(
        glue("`{fn}()` supports at most one predictor away from the displayed axis."),
        i = glue("Off-axis predictors: {collapse(secondary)}")
      ),
      call = call
    )
  }

  values <- lapply(predictors, function(column) {
    model_grid_values(data[[column]], dense = identical(column, focal), n = points)
  })
  names(values) <- predictors
  grid <- if (length(values)) {
    do.call(
      expand.grid,
      c(values, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
    )
  } else {
    data.frame(.model_dummy = 1L)
  }

  prediction <- stats::predict(mspec$fit, newdata = grid)
  grid$.model_outcome <- unname(prediction)
  grid$.model_group <- if (length(secondary)) {
    interaction(grid[secondary], drop = TRUE, lex.order = TRUE)
  } else {
    1L
  }
  grid$.model_kind <- if (is.null(focal)) {
    if (identical(orientation, "x")) "hline" else "vline"
  } else if (is.numeric(data[[focal]])) {
    "line"
  } else {
    "segment"
  }
  grid
}

#' Compose a model layer's data callback
#'
#' @noRd
model_layer_data <- function(data, model, mapping, orientation, n = NULL,
                             fn = "geom_model", call = caller_env()) {
  prepare <- function(rows) {
    prepare_model_layer_data(
      rows, model, mapping = mapping, orientation = orientation,
      n = n, fn = fn, call = call
    )
  }
  if (is.null(data)) return(prepare)
  if (is.function(data) || is_formula(data)) {
    data_fun <- as_function(data)
    return(function(plot_data) prepare(data_fun(plot_data)))
  }
  prepare(data)
}

#' Install the positional mappings owned by an explicit model layer
#'
#' @noRd
model_layer_mapping <- function(mapping, orientation) {
  mapping <- mapping %||% ggplot2::aes()
  outcome <- if (identical(orientation, "x")) {
    ggplot2::aes(y = .data$.model_outcome)
  } else {
    ggplot2::aes(x = .data$.model_outcome)
  }
  mapping[names(outcome)] <- outcome

  if (is.null(mapping[["group"]])) {
    mapping[["group"]] <- new_quosure(expr(.data$.model_group), base_env())
  }
  mapping[[".model_kind"]] <- new_quosure(
    expr(.data$.model_kind), base_env()
  )
  mapping
}

#' Build the ggplot2 layer shared by native and formula interfaces
#'
#' Unless `prepared` is `TRUE`, a supplied model is evaluated against the layer
#' data before ggplot2 computes its stat. The ggformula adapter sets `prepared`
#' after doing that work from the plot it wraps.
#'
#' @noRd
model_layer <- function(mapping = NULL, data = NULL, geom = GeomModel,
                        stat = StatModel, position = "identity", params = list(),
                        model = NULL, orientation = NA, show.legend = NA,
                        inherit.aes = TRUE, tag = "model", prepared = FALSE,
                        fn = "geom_model", call = caller_env(), ...) {
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
    resolved_orientation <- if (is.na(orientation)) "x" else orientation
    formula <- stats::formula(model)
    has_predictors <- length(attr(stats::terms(formula), "term.labels")) > 0L
    user_mapping <- mapping %||% ggplot2::aes()
    data <- model_layer_data(
      data, model, mapping = user_mapping,
      orientation = resolved_orientation, n = params[["n"]], fn = fn,
      call = call
    )
    mapping <- model_layer_mapping(user_mapping, resolved_orientation)
    if (!has_predictors) inherit.aes <- FALSE
    orientation <- resolved_orientation
  }
  params[["model"]] <- NULL
  params[["orientation"]] <- orientation

  layer <- ggplot2::layer(
    geom = geom, stat = stat, data = data, mapping = mapping,
    position = position, params = params, inherit.aes = inherit.aes,
    show.legend = show.legend, ...
  )
  if (is.null(tag)) layer else tag_layer(layer, tag)
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
#' A layer cannot inspect mappings on sibling layers. For a supplied model with
#' more than one predictor, map the displayed predictor in the model layer
#' itself. Single-predictor plot mappings, including transformations, are
#' inherited normally. Set `orientation = "y"` when a supplied model's outcome
#' is on x. Override unrelated inherited aesthetics locally or set
#' `inherit.aes = FALSE`.
#'
#' @param mapping,data,position,show.legend,inherit.aes See
#'   [ggplot2::geom_smooth()]. `data` may be a data frame, a function, or a
#'   one-sided formula.
#' @param stat The statistical transformation. Defaults to `"model"`.
#' @param geom The geometric object. `stat_model()` defaults to `"model"`.
#' @param model A fitted `lm` or `aov`, a two-sided model formula, or `NULL` to
#'   draw the model implied by the mapped positions.
#' @param orientation Layer orientation. With a supplied `model`, `NA` and
#'   `"x"` put its outcome on y; `"y"` puts it on x. With no `model`, `NA`
#'   infers the orientation from a one-axis mapping.
#' @param na.rm If `FALSE`, missing values are removed with a warning. If
#'   `TRUE`, they are removed silently.
#' @param ... Fixed aesthetics and other layer parameters. For an inferred
#'   continuous model these include `formula`, `se`, `n`, `fullrange`, `level`,
#'   and `method.args`, with the meanings used by [ggplot2::stat_smooth()].
#'   `width` controls categorical model marks. With a supplied model, `n`
#'   controls the prediction grid.
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
    inherit.aes = inherit.aes, fn = "geom_model", call = caller_env()
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
    inherit.aes = inherit.aes, fn = "stat_model", call = caller_env()
  )
}
