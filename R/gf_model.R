check_model <- function(model, data, mapping) {
  model_vars <- all.vars(stats::terms(model))
  plot_vars <- purrr::map_chr(mapping, quo_name)
  missing_in_plot <- setdiff(model_vars, plot_vars)

  if (length(missing_in_plot) > 0) {
    abort(c(
      "The model you are trying to plot uses variables that do not exist in the plot",
      glue("missing terms in plot: {paste(missing_in_plot, collapse = ', ')}")
    ))
  }

  if (is_formula(model)) {
    model <- stats::lm(model, data = data)
  }

  model
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

  # model <- check_model(model, object$data, object$mapping)
  model_vars <- supernova::variables(model)
  mapped_vars <- purrr::map_chr(object$mapping, quo_name)
  mapped_aesthetics <- mapped_vars[names(mapped_vars) %in% c("x", "y") == FALSE]

  info <- list(model = list(), plot = list())

  info$layer$args <- list2(...)
  info$layer$args$object <- object
  # standardize user-defined aesthetics
  if (!is.null(info$layer$args$color)) {
    info$layer$args$colour <- info$layer$args$color
    info$layer$args$color <- NULL
  }

  info$model$formula <- stats::formula(model)
  info$model$data <- if (inherits(model, "lm")) model$model else object$data
  info$model$fit <- stats::lm(info$model$formula, data = info$model$data)
  info$model$terms <- sort(names(info$model$fit$model))
  info$model$predictors <- sort(base::setdiff(info$model$terms, deparse(f_lhs(info$model$formula))))
  info$model$outcome <- base::setdiff(info$model$terms, info$model$predictors)

  info$plot$env <- object$plot_env
  info$plot$mapping <- object$mapping
  info$plot$aesthetics <- sort(base::setdiff(names(object$mapping), c("x", "y")))
  info$plot$facets <- object$facet$vars()
  info$plot$variables <- sort(c(purrr::map_chr(object$mapping, quo_name), facet = info$plot$facets))
  info$plot$axes <- info$plot$variables[
    names(info$plot$variables) %in% info$plot$aesthetics == FALSE &
      info$plot$variables %in% info$plot$facets == FALSE
  ]
  info$plot$outcome_axis <- names(info$plot$axes)[match(info$model$outcome, info$plot$axes, 0L)]
  info$plot$flipped <- info$plot$outcome_axis == "x"

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
      data = info$model$data,
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
        data = info$model$data,
        na.rm = TRUE
      )
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
          data = info$model$data,
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
          "color" %in% info$plot$aesthetics == FALSE &&
          "fill" %in% info$plot$aesthetics
      ) {
        info$layer$args$color <- name_to_frm(info$plot$variables[["fill"]])
      }

      # return(info)
      return(do.call(info$layer$plotter, info$layer$args))
    }
  }

  return(info)
}
