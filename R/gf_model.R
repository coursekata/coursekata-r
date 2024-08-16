#' Add a model to a plot
#'
#' When teaching about regression it can be useful to visualize the data as a point plot with the
#' outcome on the y-axis and the explanatory variable on the x-axis. For regression models, this is
#' most easily achieved by calling [`ggformula::gf_lm()`], with empty models
#' [`ggformula::gf_hline()`] using the mean, and a more complicated call to
#' [`ggformula::gf_segment()`] for group models. This function simplifies this
#' by making a guess about what kind of model you are plotting (empty/null, regression, group) and
#' then making the appropriate plot layer for it.
#'
#' This function only works with models that have a continuous outcome measure.
#'
#' @param object A plot created with the `ggformula` package.
#' @param model A linear model fit by either [`lm()`] or [`aov()`].
#' @param ... Additional arguments. Typically these are (a) ggplot2 aesthetics to be set with
#'   `attribute = value`, (b) ggplot2 aesthetics to be mapped with `attribute = ~ expression`, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
#'
#' @return a gg object (a plot layer) that can be added to a plot.
#'
#' @export
gf_model <- function(object, model, ...) {
  args <- list2(...)

  if (!inherits(object, c("gg", "ggplot"))) {
    abort("`gf_model()` needs to be layered on top of a plot.")
  }

  info <- list(layer = list())
  info$model <- fortify_model(object, model)
  info$plot <- fortify_plot(object, info$model)

  info$layer$args <- list2(...)
  info$layer$args$object <- object
  # standardize user-defined aesthetics
  if (!is.null(info$layer$args$color)) {
    info$layer$args$colour <- info$layer$args$color
    info$layer$args$color <- NULL
  }

  missing_in_plot <- setdiff(info$model$terms, info$plot$variables)
  if (length(missing_in_plot) > 0) {
    abort(c(
      "The model you are trying to plot uses variables that do not exist in the plot",
      glue("plot: {collapse(unique(info$plot$variables))}"),
      glue("model: {collapse(info$model$terms)}"),
      glue("missing in plot: {collapse(missing_in_plot)}")
    ))
  }

  # TODO: no test case
  if (length(info$model$outcome) > 1) {
    abort(c(
      "There is only support for plotting models with one outcome variable at this time",
      glue("detected outcomes: {info$model$outcome}")
    ))
  }

  if (info$model$outcome %in% info$plot$axes == FALSE) {
    abort(c(
      "The model outcome variable must be represented on the plot as one of the axes",
      glue("model outcome: {info$model$outcome}"),
      glue("plot axes: {collapse(info$plot$axes)}")
    ))
  }

  # TODO: no test case
  if (!is.numeric(info$model$data[[info$model$outcome]])) {
    abort(c(
      "There is only support for plotting models with numeric outcome variables at this time",
      glue("detected outcome type: {class(info$model$outcome)}")
    ))
  }

  # if the plot has an aesthetic mapped, but the model doesn't have that variable, unset it
  not_in_model <- info$plot$variables[info$plot$variables %in% info$model$terms == FALSE]
  for (aesthetic in names(not_in_model)) {
    if (aesthetic %in% ggplot2::GeomLine$aesthetics() && is.null(info$layer$args[[aesthetic]])) {
      info$layer$args[[aesthetic]] <- ggplot2::GeomLine$default_aes[[aesthetic]]
    }
  }

  # TODO: no test case for allowing the aesthetic
  # only allow mapped aesthetics that are predictors in the model
  for (arg_name in names(info$layer$args)) {
    if (is_formula(info$layer$args[[arg_name]])) {
      var_name <- sub("^~", "", quo_name(info$layer$args[[arg_name]]))
      if (var_name %in% info$model$predictors == FALSE) {
        info$layer$args[[arg_name]] <- NULL
      }
    }
  }

  # find grouping variable
  non_axis_predictor <- setdiff(info$model$predictors, info$plot$axes)
  if (length(non_axis_predictor) == 1) {
    # if it is a predictor *and* an aesthetic, it must be the grouping variable
    info$layer$args$group <- name_to_frm(non_axis_predictor)
  } else if (length(non_axis_predictor) > 1) {
    abort("Not sure how to plot a model with multiple variables mapped to aesthetic properties.")
  }

  # determine what to plot with ------------
  if (
    length(info$model$predictors) == 0 ||
      (length(info$model$predictors) == 1 && info$model$predictors %in% info$plot$axes == FALSE)
  ) {
    if (info$plot$flipped) {
      info$layer$plotter <- ggformula::gf_vline
      info$layer$geom <- ggplot2::GeomVline
      info$layer$args$xintercept <- name_to_frm(info$model$outcome)
    } else {
      info$layer$plotter <- ggformula::gf_hline
      info$layer$geom <- ggplot2::GeomHline
      info$layer$args$yintercept <- name_to_frm(info$model$outcome)
    }
  } else {
    non_outcome_axis_data <- object$data[[info$plot$non_outcome_axis]]
    if (is.numeric(non_outcome_axis_data)) {
      info$layer$geom <- ggplot2::GeomLine
      info$layer$plotter <- ggformula::gf_line
    } else {
      info$layer$plotter <- ggformula::gf_errorbar
      info$layer$geom <- ggplot2::GeomErrorbar
      info$layer$args$width <- if_not_null(info$layer$args$width, .4)

      if (info$plot$flipped) {
        info$layer$args$xmin <- name_to_frm(info$model$outcome)
        info$layer$args$xmax <- info$layer$args$xmin
      } else {
        info$layer$args$ymin <- name_to_frm(info$model$outcome)
        info$layer$args$ymax <- info$layer$args$ymin
      }
    }
  }

  # translate dot size to linewidth if needed
  info$layer$args$linewidth <- if_not_null(
    info$layer$args$linewidth,
    if_not_null(info$layer$args$size, 1)
  )

  # TODO: no test case
  # re-map dynamic aesthetics from previous layers if they are predictors in the model
  remap <- info$plot$variables[info$plot$variables %in% info$model$predictors]
  remap <- remap[names(remap) %in% info$layer$geom$aesthetics()]
  remap <- remap[names(remap) %in% names(info$layer$args) == FALSE]
  info$layer$args[names(remap)] <- purrr::map(remap, name_to_frm)
  # equivalent of point plot's `size` is roughly `linewidth`
  if ("size" %in% names(info$plot$aesthetics)) {
    info$layer$args$linewidth <- name_to_frm(info$plot$variables[["size"]])
  }
  # translate `fill` to `color`
  if (
    is.null(info$layer$args$color) &&
      "color" %in% names(info$plot$aesthetics) == FALSE &&
      "fill" %in% names(info$plot$aesthetics)
  ) {
    info$layer$args$color <- name_to_frm(info$plot$variables[["fill"]])
  }

  # build data grid ----------------
  params <- list()
  for (term in c(info$model$predictors, info$plot$aesthetics)) {
    term_data <- object$data[[term]]
    if (term == info$model$outcome) {
      abort("How did you use the outcome as a predictor?")
    } else if (!is.numeric(term_data)) {
      # discrete term
      if (is.logical(term_data)) {
        params[[term]] <- c(TRUE, FALSE)
      } else {
        params[[term]] <- levels(factor(term_data))
      }
    } else if (term %in% info$plot$axes) {
      # continuous on a continuous axis
      rng <- range(term_data)
      len <- max(nrow(object$data), 80L)
      params[[term]] <- seq(rng[[1]], rng[[2]], length.out = len)
    } else {
      # continuous on an aesthetic
      spread <- stats::sd(term_data, na.rm = TRUE)
      middle <- mean(term_data, na.rm = TRUE)
      params[[term]] <- c(middle - spread, middle, middle + spread)
    }
  }

  # predict y values using the grid
  grid <- expand.grid(if (length(params)) params else list(dummy = 1))
  grid[info$model$outcome] <- stats::predict(info$model$fit, newdata = grid)
  info$layer$args$data <- grid

  return(do.call(info$layer$plotter, info$layer$args))
}

fortify_layer <- function(object, ...) {
  args <- list2(...)
  args$object <- object

  # standardize user-defined aesthetics
  if (!is.null(info$layer$args$color)) {
    info$layer$args$colour <- info$layer$args$color
    info$layer$args$color <- NULL
  }

  list(args = args)
}

fortify_model <- function(object, model) {
  formula <- stats::formula(model)
  data <- if (inherits(model, "lm")) model$model else object$data
  fit <- if (inherits(model, "lm")) model else stats::lm(formula, data = data)
  terms <- sort(names(fit$model))
  predictors <- sort(setdiff(terms, deparse(f_lhs(formula))))
  outcome <- setdiff(terms, predictors)
  list(
    formula = formula, data = data, fit = fit,
    terms = terms, predictors = predictors, outcome = outcome
  )
}

fortify_plot <- function(object, fortified_model) {
  mapping <- object$mapping
  aesthetics <- sort(setdiff(names(mapping), c("x", "y")))
  facets <- object$facet$vars()
  variables <- sort(c(purrr::map_chr(mapping, quo_name), facet = facets))
  axes <- variables[names(variables) %in% aesthetics == FALSE & variables %in% facets == FALSE]
  outcome_axis <- axes[axes %in% fortified_model$outcome]
  non_outcome_axis <- axes[axes %in% outcome_axis == FALSE]
  flipped <- names(outcome_axis) == "x"
  list(
    mapping = mapping, variables = variables, aesthetics = variables[aesthetics],
    facets = facets, axes = axes, outcome_axis = outcome_axis, non_outcome_axis = non_outcome_axis,
    flipped = flipped, env = object$plot_env
  )
}

check_aesthetics <- function(args, predictor_vars) {
  mappable <- c(ggplot2::GeomLine$aesthetics(), "color")
  to_map <- union(names(args), mappable)

  purrr::walk(to_map, function(aesthetic) {
    if (inherits(args[[aesthetic]], "formula")) {
      var_name <- deparse(f_rhs(args[[aesthetic]]))
      if (!var_name %in% predictor_vars) {
        abort(c(
          "Cannot apply an aesthetic using variables that are not predictors in the model",
          glue("trying to apply: `{aesthetic} ~ {var_name}`"),
          glue("predictor variables: {paste(predictor_vars, collapse = ', ')}")
        ))
      }
    }
  })
}


sd_spread <- list(
  "-1 SD" = function(x) mean(x, na.rm = TRUE) - stats::sd(x, na.rm = TRUE),
  "mean" = function(x) mean(x, na.rm = TRUE),
  "+1 SD" = function(x) mean(x, na.rm = TRUE) + stats::sd(x, na.rm = TRUE)
)
is_categorical <- function(x) !is.numeric(x)
collapse <- function(x) glue::glue_collapse(x, sep = ", ")
name_to_frm <- function(x) stats::formula(glue("~{x}"))
if_not_null <- function(x, other) if (!is.null(x)) x else other
