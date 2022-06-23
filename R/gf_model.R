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


#' Add a model to a plot
#'
#' When teaching about regression it can be useful to visualize the data as a point plot with the
#' outcome on the y-axis and the explanatory variable on the x-axis. For regression models, this is
#' most easily achieved by calling [`gf_lm()`], with empty models [`gf_hline()`] using the mean,
#' and a more complicated call to [`gf_segment()`] for group models. This function simplifies this
#' by making a guess about what kind of model you are plotting (empty/null, regression, group) and
#' then making the appropriate plot layer for it.
#'
#' This function only works with models that have a continuous outcome measure.
#'
#' @param object A plot created with the `ggformula` package.
#' @param model A linear model fit by either [`lm()`] or [`aov()`].
#' @param ... Additional arguments. Typically these are (a) ggplot2 aesthetics to be set with
#'   `attribute = value`, (b) ggplot2 aesthetics to be mapped with `attribute = ~ expression``, or
#'   (c) attributes of the layer as a whole, which are set with `attribute = value`.
gf_model <- function(object, model, ...) {
  args <- list2(...)

  if (!inherits(object, c("gg", "ggplot"))) {
    abort("`gf_model()` needs to be layered on top of a plot.")
  }

  info <- list(layer = list())
  info$model <- fortify_model(object, model)
  info$plot <- fortify_plot(object, info$model)

  layer <- list()
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

  # TODO: no test case
  if (length(intersect(info$model$terms, info$plot$aesthetics)) > 1) {
    abort("Not sure how to plot a model with multiple variables mapped to aesthetic properties.")
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

  # no predictor ----
  if (length(info$model$terms) == 1) {
    if (info$plot$flipped) {
      info$layer$plotter <- ggformula::gf_vline
      info$layer$geom <- ggplot2::GeomVline
      info$layer$args$xintercept <- name_to_frm(info$model$outcome)
    } else {
      info$layer$plotter <- ggformula::gf_hline
      info$layer$geom <- ggplot2::GeomHline
      info$layer$args$yintercept <- name_to_frm(info$model$outcome)
    }

    info$layer$args$data <- mosaic::favstats(
      info$model$formula,
      data = object$data,
      na.rm = TRUE
    )
    info$layer$args$data[[info$model$outcome]] <- info$layer$args$data$mean

    # TODO: no test case
    # re-map dynamic aesthetics from previous layers for predictors in the model
    remap <- info$plot$variables[info$plot$variables %in% info$model$predictors]
    remap <- remap[names(remap) %in% ggplot2::GeomLine$aesthetics()]
    info$layer$args[names(remap)] <- purrr::map(remap, name_to_frm)

    return(do.call(info$layer$plotter, info$layer$args))
  }

  # single predictor models ----
  if (length(info$model$terms) == 2) {
    # predictor on axis ----
    if (all(info$model$terms %in% info$plot$axes)) {
      grid <- object$data
      x_name <- info$model$predictors
      x_data <- object$data[[x_name]]

      # predictor is continuous ----
      if (is.numeric(x_data)) {
        rng <- range(x_data)
        grid[[x_name]] <- seq(rng[[1]], rng[[2]], length.out = nrow(object$data))

        grid$pred <- stats::predict(model, newdata = grid)
        info$layer$args$data <- grid
        info$layer$args$gformula <- if (info$plot$flipped) {
          stats::as.formula(paste(x_name, "~ pred"))
        } else {
          stats::as.formula(paste("pred ~", x_name))
        }
        return(do.call(ggformula::gf_line, info$layer$args))
      }

      # predictor is categorical ----
      info$layer$plotter <- ggformula::gf_errorbar
      info$layer$geom <- ggplot2::GeomErrorbar
      info$layer$args$size <- if_not_null(info$layer$args$size, 2)

      if (info$plot$flipped) {
        info$layer$args$xmin <- name_to_frm(info$model$outcome)
        info$layer$args$xmax <- info$layer$args$xmin
        info$layer$args$width <- if_not_null(info$layer$args$width, .2)
      } else {
        info$layer$args$ymin <- name_to_frm(info$model$outcome)
        info$layer$args$ymax <- info$layer$args$ymin
        info$layer$args$width <- if_not_null(info$layer$args$width, .2)
      }

      info$layer$args$data <- mosaic::favstats(
        info$model$formula,
        data = object$data,
        na.rm = TRUE
      )

      info$layer$args$data[[info$model$predictors]] <- info$layer$args$data[[1]]
      info$layer$args$data[[info$model$outcome]] <- info$layer$args$data$mean

      return(do.call(info$layer$plotter, info$layer$args))
    } else {
      # predictor on aesthetic or facet ----
      if (info$plot$flipped) {
        info$layer$plotter <- ggformula::gf_vline
        info$layer$geom <- ggplot2::GeomVline
        info$layer$args$xintercept <- name_to_frm(info$model$outcome)
      } else {
        info$layer$plotter <- ggformula::gf_hline
        info$layer$geom <- ggplot2::GeomHline
        info$layer$args$yintercept <- name_to_frm(info$model$outcome)
      }

      grid <- object$data
      pred_name <- info$model$predictors
      pred_data <- object$data[[pred_name]]

      if (is.numeric(pred_data)) {
        spread <- stats::sd(pred_data, na.rm = TRUE)
        middle <- mean(pred_data, na.rm = TRUE)
        data <- tibble::tibble(!!pred_name := c(
          middle + spread,
          middle,
          middle - spread
        ))
        outcome <- stats::predict(info$model$fit, newdata = data)
      } else {
        data <- mosaic::favstats(
          info$model$formula,
          data = object$data,
          na.rm = TRUE
        )
        outcome <- data$mean
      }

      # create the new data object
      data[[info$model$outcome]] <- outcome
      info$layer$args$data <- data.frame(data)

      # TODO: no test case
      # re-map dynamic aesthetics from previous layers for predictors in the model
      remap <- info$plot$variables[info$plot$variables %in% info$model$predictors]
      remap <- remap[names(remap) %in% ggplot2::GeomLine$aesthetics()]
      info$layer$args[names(remap)] <- purrr::map(remap, name_to_frm)
      if (
        is.null(info$layer$args$color) &&
          "color" %in% names(info$plot$aesthetics) == FALSE &&
          "fill" %in% names(info$plot$aesthetics)
      ) {
        info$layer$args$color <- name_to_frm(info$plot$variables[["fill"]])
      }

      return(do.call(info$layer$plotter, info$layer$args))
    }
  }

  if (length(info$model$terms) > 2) {
    # build a grid with predictor params
    params <- list()
    for (term in info$model$predictors) {
      pred_data <- object$data[[term]]

      if (term == info$plot$axes[[if (info$plot$flipped) "y" else "x"]]) {
        if (is.numeric(pred_data)) {
          rng <- range(pred_data)
          len <- max(nrow(object$data), 80L)
          params[[term]] <- seq(rng[[1]], rng[[2]], length.out = len)

          info$layer$geom <- ggplot2::GeomLine
          info$layer$plotter <- ggformula::gf_line
        } else {
          params[[term]] <- levels(factor(pred_data))

          info$layer$plotter <- ggformula::gf_errorbar
          info$layer$geom <- ggplot2::GeomErrorbar
          info$layer$args$size <- if_not_null(info$layer$args$size, 2)
          info$layer$args$width <- if_not_null(info$layer$args$width, .2)

          if (info$plot$flipped) {
            info$layer$args$xmin <- name_to_frm(info$model$outcome)
            info$layer$args$xmax <- info$layer$args$xmin
          } else {
            info$layer$args$ymin <- name_to_frm(info$model$outcome)
            info$layer$args$ymax <- info$layer$args$ymin
          }
        }
      } else if (term != info$model$outcome) {
        info$layer$args$group <- name_to_frm(term)
        if (is.numeric(pred_data)) {
          spread <- stats::sd(pred_data, na.rm = TRUE)
          middle <- mean(pred_data, na.rm = TRUE)
          params[[term]] <- c(middle - spread, middle, middle + spread)
        } else {
          params[[term]] <- levels(factor(pred_data))
        }
      } else {
        abort("How did you use the outcome as a predictor?")
      }
    }

    # predict y values using the grid
    grid <- expand.grid(params)
    grid[info$model$outcome] <- stats::predict(info$model$fit, newdata = grid)
    info$layer$args$data <- grid

    return(do.call(info$layer$plotter, info$layer$args))
  }

  return(info)
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
  outcome_axis <- names(axes)[match(fortified_model$outcome, axes, 0L)]
  list(
    mapping = mapping, variables = variables, aesthetics = variables[aesthetics],
    facets = facets, axes = axes, outcome_axis = outcome_axis, flipped = outcome_axis == "x",
    env = object$plot_env
  )
}
