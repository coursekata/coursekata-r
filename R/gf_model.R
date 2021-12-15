#' Add a model to a plot
#'
#' When teaching about regression it can be useful to visualize the data as a point plot with the
#' outcome on the y-axis and the explanatory variable on the x-axis. For regression models, this is
#' most easily achieved by calling [`gf_lm()`], with empty models [`gf_hline()`] using the mean, and
#' a more complicated call to [`gf_segment()`] for group models. This function simplifies this by
#' making a guess about what kind of model you are plotting (empty/null, regression, group) and then
#' making the appropriate plot layer for it. **Note**: this function only works with models that
#' have a *single* or `NULL` explanatory variable, and it will not work with multiple regression.
#'
#' @param object When chaining, this holds an object produced in the earlier portions of the chain.
#'   Most users can safely ignore this argument. See details and examples.
#' @param gformula A formula with shape y ~ x. Superseded by `model` if one is given.
#' @param data A data frame with the variables to be plotted. Superseded by `model` if one is given.
#' @param model A model fit by [`lm()`]. If a model is given, it supersedes the `data` and
#'   `gformula`.
#' @param width The width of the mean line(s) to be plotted for group models. Note that factors are
#'   plotted 1 unit apart, so values larger than 1 will overlap into other groups.
#' @param ... Arguments passed on to the respective `gf_*` function.
#'
#' @return A `gg` object
#' @export
#'
#' @examples
#' # basic examples
#' gf_model(Thumb ~ NULL, data = Fingers)
#' gf_model(Thumb ~ Height, data = Fingers)
#' gf_model(Thumb ~ RaceEthnic, data = Fingers)
#'
#' # specifying the model using a fitted model
#' model <- lm(Thumb ~ Height, data = Fingers)
#' gf_model(model)
#'
#' # chaining on to previous plots
#' gf_point(Thumb ~ Height, data = Fingers) %>%
#'   gf_model()
#'
#' gf_point(Thumb ~ Height, data = Fingers) %>%
#'   gf_model() %>%
#'   gf_model(Thumb ~ NULL)
gf_model <- function(object = NULL, gformula = NULL, data = NULL, model = NULL, width = .3, ...) {
  # phase 1: handle arguments in different positions
  if (inherits(object, 'formula')) {
    gformula <- object
    object <- NULL
  }

  if (inherits(object, 'data.frame')) {
    data <- object
    object <- NULL
  }

  if (inherits(object, 'lm')) {
    model <- object
    object <- NULL
  }

  if (inherits(gformula, 'lm')) {
    model <- gformula
    gformula <- NULL
  }

  # phase 2: find the formula and data
  if (inherits(model, 'lm')) {
    if (!is.null(gformula) || !is.null(data)) {
      rlang::warn(paste(
        'You have passed both a `model` and a `gformula` and/or `data` to `gf_model()`.',
        'The formula and data from the `model` will be used and the others ignored.'
      ))
    }

    gformula <- stats::as.formula(model)
    data <- if (is.null(model$call$data)) {
      model$model
    } else {
      rlang::env_get(rlang::f_env(gformula), rlang::as_string(model$call$data), inherit = TRUE)
    }
  }

  if (inherits(object, 'gg')) {
    if (is.null(model) && is.null(gformula)) {
      # we don't have the model or formula yet, so we need to get it from up the chain
      # if the previous layers have variables on both the x and y axes, the formula is y ~ x
      y <- object$mapping[['y']]
      x <- object$mapping[['x']]
      gformula <- stats::as.formula(
        paste(rlang::as_name(y), "~", rlang::as_name(x)),
        rlang::quo_get_env(object$mapping[['y']])
      )

      # if the previous layers use faceting to introduce the second variable, we have to determine
      # what axis the outcome is on (either x or y) and what the faceting variable is, then the
      # formula is outcome ~ facet
    }

    object_has_data <- !inherits(object$data, 'waiver')
    if (!is.null(data) && object_has_data && !identical(data, object$data)) {
      rlang::abort(paste(
        "Can't plot two different data sets. A different set of data was passed to `gf_model()`",
        "compared to the previous function in the chain."
      ))
    }

    if (object_has_data) {
      data <- object$data
    }
  }

  # TODO: for now, data is required with a formula, in the future allow data$var syntax
  if (inherits(gformula, 'formula') && inherits(data, 'data.frame')) {
    if (is.null(model)) {
      # construct the model so that we can guess what kind of plot we need
      model <- stats::lm(gformula, data = data)
    }
  } else {
    rlang::abort("You must supply a `model` or a `gformula` and `data`.")
  }

  variables <- supernova::variables(model)
  if (length(variables$predictor) == 0) {
    add_empty_model(object, model, ...)
  } else if (is.factor(data[[variables$predictor]]) || is.factor(data[[variables$predictor]])) {
    # TODO: documentation -- group model does not support formulae with facets
    add_group_model(object, gformula, data, width = .3, ...)
  } else {
    ggformula::gf_lm(object, gformula, data, ...)
  }
}

#' Plot the empty model
#'
#' The empty model is extracted by pulling out [`b0()`] from the passed model. This expression is
#' passed to [`gf_hline()`] to plot the empty model. If this plot is not being chained to a prior
#' plot, a blank point plot is created to make sure the y-axis is informative and has the range of
#' the original data.
#'
#' @param object A gg object to chain to, optionally.
#' @param model An empty model fit by [`lm()`].
#' @param ... Additional arguments to pass to [`gf_hline()`].
#'
#' @return A gg object.
#' @keywords internal
add_empty_model <- function(object, model, ...) {
  if (is.null(object)) {
    # build a blank plot so that the y-axis is informative
    outcome <- supernova::variables(model)$outcome
    frm <- stats::as.formula(paste(outcome, "~ 1"))
    object <- ggformula::gf_blank(gformula = frm, data = model$model)
  }

  ggformula::gf_hline(object, yintercept = ~b0(model), ...)
}

#' Plot the group model
#'
#' The group model is represented by mean lines for each group in the model. The values for these
#' lines are extracted using the formula with the passed data, and then plotted via
#' [`gf_segment()`]. If this plot is not being chained to a prior plot, a blank point plot is
#' created to make sure the axes are informative and have the range of the original data.
#'
#' @param object A gg object to chain to, optionally.
#' @param gformula A formula of the shape `y ~ x`.
#' @param data The data the formula refers to.
#' @param width The width of the mean lines to be plotted.
#' @param ... Additional arguments passed to [`gf_segment()`]
#'
#' @return A gg object.
#' @keywords internal
add_group_model <- function(object, gformula, data, width = .3, ...) {
  if (is.null(object)) {
    # build a blank plot so that the axes are informative
    object <- ggformula::gf_blank(gformula = gformula, data = data)
  }

  five_num <- mosaic::favstats(gformula, data = data)
  five_num$x_min <- seq_along(five_num$mean) - width
  five_num$x_max <- seq_along(five_num$mean) + width
  gf_segment(object, mean + mean ~ x_min + x_max, data = five_num, ...)
}

