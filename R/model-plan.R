#' Read the facts out of a fitted model
#'
#' @param plot_data The data frame the plot was built from.
#' @param model A model fit by `lm()` or `aov()`, or a formula.
#'
#' @return A list with `formula`, `data`, `fit`, `terms`, `predictors`, `outcome`.
#'
#' @noRd
model_spec <- function(plot_data, model, call = caller_env()) {
  formula <- stats::formula(model)
  data <- if (inherits(model, "lm")) model$model else plot_data
  fit <- if (inherits(model, "lm")) model else stats::lm(formula, data = data)
  terms <- sort(names(fit$model))
  predictors <- sort(setdiff(terms, deparse(f_lhs(formula))))
  outcome <- setdiff(terms, predictors)
  list(
    formula = formula, data = data, fit = fit,
    terms = terms, predictors = predictors, outcome = outcome
  )
}

#' Decide what to draw for a model on a plot
#'
#' @param spec A [plot_spec()] list.
#' @param mspec A [model_spec()] list.
#' @param args Named list of user arguments (aesthetics and layer parameters).
#'
#' @return A list with `kind`, `args`, `grid` and `tag`.
#'
#' @noRd
model_plan <- function(spec, mspec, args = list(), call = caller_env()) {
  if (length(spec$axes) == 0) {
    abort(
      c(
        paste0(
          "gf_model() supports plots built with gf_point(), gf_jitter(), gf_boxplot(), ",
          "gf_violin() and gf_histogram()"
        ),
        paste0(
          "the plot given maps neither x nor y to a variable, so there is no axis to place a ",
          "model on"
        ),
        paste0(
          "if you need another plot type, open an issue at ",
          "https://github.com/coursekata/coursekata-r/issues"
        )
      ),
      call = call
    )
  }

  if (!is.null(args$color)) {
    args$colour <- args$color
    args$color <- NULL
  }

  outcome_axis <- spec$axes[spec$axes %in% mspec$outcome]
  non_outcome_axis <- spec$axes[spec$axes %in% outcome_axis == FALSE]
  flipped <- identical(names(outcome_axis), "x")

  missing_in_plot <- setdiff(mspec$terms, spec$variables)
  if (length(missing_in_plot) > 0) {
    abort(
      c(
        "The model you are trying to plot uses variables that do not exist in the plot",
        glue("plot: {collapse(unique(spec$variables))}"),
        glue("model: {collapse(mspec$terms)}"),
        glue("missing in plot: {collapse(missing_in_plot)}")
      ),
      call = call
    )
  }

  if (length(mspec$outcome) > 1) {
    abort(
      c(
        "There is only support for plotting models with one outcome variable at this time",
        glue("detected outcomes: {mspec$outcome}")
      ),
      call = call
    )
  }

  if (mspec$outcome %in% spec$axes == FALSE) {
    abort(
      c(
        "The model outcome variable must be represented on the plot as one of the axes",
        glue("model outcome: {mspec$outcome}"),
        glue("plot axes: {collapse(spec$axes)}")
      ),
      call = call
    )
  }

  if (!is.numeric(mspec$data[[mspec$outcome]])) {
    abort(
      c(
        "There is only support for plotting models with numeric outcome variables at this time",
        glue("detected outcome type: {class(mspec$outcome)}")
      ),
      call = call
    )
  }

  mapped <- purrr::keep(args, is_formula)
  bad_aes <- purrr::keep(
    purrr::map_chr(mapped, ~ as_string(f_rhs(.x))),
    ~ .x %in% mspec$predictors == FALSE
  )
  if (length(bad_aes) > 0) {
    abort(
      c(
        "Cannot apply aesthetics using variables that are not predictors in the model",
        glue("trying to apply: {collapse(paste0(names(bad_aes), ' ~ ', bad_aes))}"),
        glue("model predictors: {collapse(mspec$predictors)}")
      ),
      call = call
    )
  }

  not_in_model <- spec$variables[spec$variables %in% mspec$terms == FALSE]
  for (aesthetic in names(not_in_model)) {
    if (aesthetic %in% ggplot2::GeomLine$aesthetics() && is.null(args[[aesthetic]])) {
      args[[aesthetic]] <- ggplot2::GeomLine$default_aes[[aesthetic]]
    }
  }

  non_axis_predictor <- setdiff(mspec$predictors, spec$axes)
  if (length(non_axis_predictor) == 1) {
    args$group <- name_to_frm(non_axis_predictor)
  } else if (length(non_axis_predictor) > 1) {
    abort(
      "Not sure how to plot a model with multiple variables mapped to aesthetic properties.",
      call = call
    )
  }

  no_predictors <- length(mspec$predictors) == 0
  predictor_off_axis <- length(mspec$predictors) == 1 &&
    mspec$predictors %in% spec$axes == FALSE

  if (no_predictors || predictor_off_axis) {
    if (flipped) {
      kind <- "vline"
      geom <- ggplot2::GeomVline
      args$xintercept <- name_to_frm(mspec$outcome)
    } else {
      kind <- "hline"
      geom <- ggplot2::GeomHline
      args$yintercept <- name_to_frm(mspec$outcome)
    }
  } else if (is.numeric(spec$data[[non_outcome_axis]])) {
    kind <- "line"
    geom <- ggplot2::GeomLine
  } else {
    kind <- "errorbar"
    geom <- ggplot2::GeomErrorbar
    args$width <- args$width %||% .4
    if (flipped) {
      args$xmin <- name_to_frm(mspec$outcome)
      args$xmax <- args$xmin
    } else {
      args$ymin <- name_to_frm(mspec$outcome)
      args$ymax <- args$ymin
    }
  }

  args$linewidth <- args$linewidth %||% args$size %||% 1

  remap <- spec$variables[spec$variables %in% mspec$predictors]
  remap <- remap[names(remap) %in% geom$aesthetics()]
  remap <- remap[names(remap) %in% names(args) == FALSE]
  args[names(remap)] <- purrr::map(remap, name_to_frm)

  if ("size" %in% names(spec$aesthetics)) {
    args$linewidth <- name_to_frm(spec$variables[["size"]])
  }

  if (
    is.null(args$colour) &&
      "colour" %in% names(spec$aesthetics) == FALSE &&
      "fill" %in% names(spec$aesthetics)
  ) {
    args$colour <- name_to_frm(spec$variables[["fill"]])
  }

  params <- list()
  for (term in c(mspec$predictors, spec$aesthetics)) {
    term_data <- spec$data[[term]]
    if (term == mspec$outcome) {
      abort("How did you use the outcome as a predictor?", call = call)
    } else if (!is.numeric(term_data)) {
      params[[term]] <- if (is.logical(term_data)) {
        c(TRUE, FALSE)
      } else {
        levels(factor(term_data))
      }
    } else if (term %in% spec$axes) {
      rng <- range(term_data)
      len <- max(nrow(spec$data), 80L)
      params[[term]] <- seq(rng[[1]], rng[[2]], length.out = len)
    } else {
      spread <- stats::sd(term_data, na.rm = TRUE)
      middle <- mean(term_data, na.rm = TRUE)
      params[[term]] <- c(middle - spread, middle, middle + spread)
    }
  }

  grid <- expand.grid(if (length(params)) params else list(dummy = 1))
  grid[mspec$outcome] <- stats::predict(mspec$fit, newdata = grid)

  list(kind = kind, args = args, grid = grid, tag = "model")
}
