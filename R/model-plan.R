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

  # lm() coerces a non-numeric outcome to double and dies with NA/NaN/Inf in 'y',
  # so the outcome has to be checked before lm() ever runs
  named <- if (is.null(f_lhs(formula))) NULL else as_label(f_lhs(formula))
  if (!inherits(model, "lm") && !is.null(named) && named %in% names(data)) {
    check_numeric_outcome(named, data[[named]], call)
  }

  fit <- if (inherits(model, "lm")) model else stats::lm(formula, data = data)
  terms <- sort(names(fit$model))
  predictors <- sort(setdiff(terms, deparse(f_lhs(formula))))
  outcome <- setdiff(terms, predictors)
  list(
    formula = formula, data = data, fit = fit,
    terms = terms, predictors = predictors, outcome = outcome
  )
}

#' Refuse an outcome that is not a number
#'
#' @param name The outcome variable's name.
#' @param values The outcome variable's values.
#' @param call The calling environment, for error reporting.
#'
#' @return Nothing. Called for the error it raises.
#'
#' @noRd
check_numeric_outcome <- function(name, values, call = caller_env()) {
  if (is.numeric(values)) {
    return(invisible(NULL))
  }
  abort(
    c(
      "There is only support for plotting models with numeric outcome variables at this time",
      glue("model outcome: {name}"),
      glue("detected outcome type: {class(values)[[1]]}")
    ),
    call = call
  )
}

#' Reduce term/mapping labels to the columns they read
#'
#' A model records its terms as deparsed labels (`names(fit$model)`, so
#' `log(age)` for `lm(y ~ log(age))`) and a plot records its mappings the same
#' way (`as_label()`). `predict()`, though, consumes columns: it re-evaluates
#' `log(age)` against a column literally named `age`. Every comparison
#' between what a model needs and what a plot has -- and the prediction grid
#' itself -- has to be made on that column footing; the labels survive only
#' in the messages, because they are what the caller wrote.
#'
#' @param labels A character vector of term or mapping labels.
#'
#' @return A character vector of the unique columns those labels read from.
#'
#' @noRd
label_columns <- function(labels) {
  unique(unlist(lapply(labels, function(label) all.vars(str2lang(label))), use.names = FALSE))
}

#' Refuse a plot with no axis to place a model on
#'
#' Shared by `model_plan()` (an explicit model), `implied_model_spec()` (an
#' inferred one) and, through `implied_model()`, `gf_b()`/`gf_coef()`, so a
#' bare `ggplot()` with nothing mapped is refused in the same words whichever
#' path found it -- there is no axis-shaped difference between "I don't know
#' what model to draw" and "I don't know what model this implies" when there
#' is no axis at all.
#'
#' @param spec A `plot_spec()` list.
#' @param fn The name to refuse in, e.g. `"gf_model"` or `"gf_b"`.
#' @param call The calling environment, for error reporting.
#'
#' @return `spec`, invisibly.
#'
#' @noRd
check_model_axes <- function(spec, fn = "gf_model", call = caller_env()) {
  if (length(spec$axes) == 0) {
    abort(
      c(
        paste0(
          glue("{fn}() supports plots built with gf_point(), gf_jitter(), gf_boxplot(), "),
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
  invisible(spec)
}

# A line is a rendered interpolation, not a copy of the observations. Keep the
# established floor of 80 points for small data and preserve observed positions
# for ordinary classroom-sized data, but stop at 256: beyond that, additional
# vertices are not visually useful and make prediction plus data-frame expansion
# scale with the source data. The bound matters especially in JupyterLite, where
# both operations run in WebAssembly.
model_grid_max_points <- 256L

#' Collect the column-level facts shared by every planning phase
#'
#' @param spec A `plot_spec()` list.
#' @param mspec A `model_spec()` list.
#'
#' @return A list of columns and axes used to validate and build the plan.
#'
#' @noRd
model_plan_facts <- function(spec, mspec) {
  columns_by_axis <- purrr::map(spec$axes, label_columns)
  columns_by_variable <- purrr::map(spec$variables, label_columns)
  outcome_columns <- label_columns(mspec$outcome)

  outcome_axis <- spec$axes[purrr::map_lgl(columns_by_axis, ~ any(outcome_columns %in% .x))]
  # select by aesthetic name, not by value, so two axes mapping the same
  # label cannot collide
  non_outcome_axis <- spec$axes[names(spec$axes) %in% names(outcome_axis) == FALSE]

  list(
    columns_by_axis = columns_by_axis,
    columns_by_variable = columns_by_variable,
    axis_columns = label_columns(spec$axes),
    plot_columns = label_columns(spec$variables),
    model_columns = label_columns(mspec$terms),
    outcome_columns = outcome_columns,
    predictor_columns = label_columns(mspec$predictors),
    aesthetic_columns = label_columns(spec$aesthetics),
    outcome_axis = outcome_axis,
    non_outcome_axis = non_outcome_axis,
    flipped = identical(names(outcome_axis), "x")
  )
}

#' Refuse an explicit model that the plot cannot represent
#'
#' Checks intentionally remain in the order readers encounter their mistakes:
#' absent variables, unsupported outcomes, a missing outcome axis, outcome
#' type, then caller-supplied aesthetic mappings.
#'
#' @param spec A `plot_spec()` list.
#' @param mspec A `model_spec()` list.
#' @param args Named list of normalized user arguments.
#' @param facts A list from `model_plan_facts()`.
#' @param call The calling environment, for error reporting.
#'
#' @return Nothing. Called for errors it may raise.
#'
#' @noRd
check_model_plan <- function(spec, mspec, args, facts, call = caller_env()) {
  missing_in_plot <- setdiff(facts$model_columns, facts$plot_columns)
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

  if (!is.name(str2lang(mspec$outcome))) {
    abort(
      c(
        paste0(
          "There is only support for plotting models whose outcome is a variable in the data ",
          "at this time"
        ),
        glue("model outcome: {mspec$outcome}"),
        i = paste0(
          "predict() returns the outcome on the transformed scale, and there is no general ",
          "way to invert that for the plot to draw"
        ),
        i = "add the transformed value to the data as its own variable, then fit the model on that"
      ),
      call = call
    )
  }

  if (length(facts$outcome_axis) == 0) {
    abort(
      c(
        "The model outcome variable must be represented on the plot as one of the axes",
        glue("model outcome: {mspec$outcome}"),
        glue("plot axes: {collapse(spec$axes)}")
      ),
      call = call
    )
  }

  check_numeric_outcome(mspec$outcome, mspec$data[[mspec$outcome]], call)

  mapped <- purrr::keep(args, is_formula)
  bad_aes <- purrr::keep(
    purrr::map_chr(mapped, ~ as_label(f_rhs(.x))),
    ~ all(label_columns(.x) %in% facts$predictor_columns) == FALSE
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

  invisible(NULL)
}

#' Choose a model geom and the arguments it needs
#'
#' @param spec A `plot_spec()` list.
#' @param args Named list of normalized user arguments.
#' @param facts A list from `model_plan_facts()`.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `kind`, `geom` and `args`.
#'
#' @noRd
model_layer_plan <- function(spec, args, facts, call = caller_env()) {
  not_in_model <- spec$variables[
    purrr::map_lgl(facts$columns_by_variable, ~ any(.x %in% facts$model_columns) == FALSE)
  ]
  for (aesthetic in names(not_in_model)) {
    if (aesthetic %in% ggplot2::GeomLine$aesthetics() && is.null(args[[aesthetic]])) {
      args[[aesthetic]] <- ggplot2::get_geom_defaults("line")[[aesthetic]]
    }
  }

  non_axis_predictor <- setdiff(facts$predictor_columns, facts$axis_columns)
  if (length(non_axis_predictor) == 1) {
    args$group <- name_to_frm(non_axis_predictor)
  } else if (length(non_axis_predictor) > 1) {
    abort(
      "Not sure how to plot a model with multiple variables mapped to aesthetic properties.",
      call = call
    )
  }

  # the shape drawn is a property of what the plot puts on the non-outcome
  # axis, not of which column its mapping names -- a transformed axis (e.g.
  # log(age)) still shows a numeric axis to draw a line against. Read after
  # every abort above, so a mapping that cannot be evaluated never pre-empts
  # a validation message.
  along <- if (length(facts$non_outcome_axis) == 1) {
    spec$resolve_aes(names(facts$non_outcome_axis))
  }
  along_values <- if (!is.null(along)) eval_tidy(along$quo, along$data)

  no_predictors <- length(facts$predictor_columns) == 0
  predictor_off_axis <- length(facts$predictor_columns) == 1 &&
    facts$predictor_columns %in% facts$axis_columns == FALSE

  if (no_predictors || predictor_off_axis) {
    if (facts$flipped) {
      kind <- "vline"
      geom <- ggplot2::GeomVline
      # the intercept inherits nothing, so it is the one shape that has to
      # spell the plot's own mapping out rather than inheriting it
      args$xintercept <- name_to_frm(unname(facts$outcome_axis))
    } else {
      kind <- "hline"
      geom <- ggplot2::GeomHline
      args$yintercept <- name_to_frm(unname(facts$outcome_axis))
    }
  } else if (is.numeric(along_values)) {
    kind <- "line"
    geom <- ggplot2::GeomLine
  } else {
    kind <- "segment"
    geom <- GeomModelMark
    args$width <- args$width %||% .4
    # internal and authoritative: a caller cannot change which axis holds groups
    args$mark_axis <- if (facts$flipped) "y" else "x"
  }

  # `size` is the pre-3.4 spelling of `linewidth`; leaving it in args sends both to
  # the layer and ggplot2 deprecation-warns, naming coursekata as the culprit
  width_given <- !is.null(args$linewidth) || !is.null(args$size)
  args$linewidth <- args$linewidth %||% args$size %||% 1
  args$size <- NULL

  remap <- spec$variables[
    purrr::map_lgl(facts$columns_by_variable, ~ any(.x %in% facts$predictor_columns))
  ]
  remap <- remap[names(remap) %in% geom$aesthetics()]
  remap <- remap[names(remap) %in% names(args) == FALSE]
  args[names(remap)] <- purrr::map(remap, name_to_frm)

  if (!width_given && "size" %in% names(spec$aesthetics)) {
    args$linewidth <- name_to_frm(spec$variables[["size"]])
  }

  if (
    is.null(args$colour) &&
      "colour" %in% names(spec$aesthetics) == FALSE &&
      "fill" %in% names(spec$aesthetics)
  ) {
    args$colour <- name_to_frm(spec$variables[["fill"]])
  }

  if (kind == "segment") {
    # GeomSegment's colour default is themed while GeomErrorbar's was not, so an
    # unpinned mark would silently turn blue; keep the neutral the fit line uses
    args$colour <- args$colour %||% ggplot2::get_geom_defaults("line")$colour
  }

  list(kind = kind, geom = geom, args = args)
}

#' Choose one value for an inherited aesthetic the model does not use
#'
#' The column must exist so ggplot2 can evaluate an inherited expression, but
#' crossing every level would duplicate an identical model trace. Preserve the
#' source type and choose a non-missing observation where one exists.
#'
#' @param values A plot-data column.
#'
#' @return A length-one vector of the same type.
#'
#' @noRd
model_grid_support_value <- function(values) {
  present <- which(!is.na(values))
  values[if (length(present)) present[[1]] else 1L]
}

#' Build and evaluate the bounded prediction grid
#'
#' @param spec A `plot_spec()` list.
#' @param mspec A `model_spec()` list.
#' @param layer A list from `model_layer_plan()`.
#' @param facts A list from `model_plan_facts()`.
#' @param call The calling environment, for error reporting.
#'
#' @return A list with `grid` and the possibly augmented layer `args`.
#'
#' @noRd
model_prediction_grid <- function(spec, mspec, layer, facts, call = caller_env()) {
  params <- list()
  support <- list()

  # A model layer inherits the plot's mappings. An unused mapping that the geom
  # does not accept as a static aesthetic (shape on a line, for example) still
  # needs its columns to exist when ggplot2 evaluates it. Give those expressions
  # one representative row rather than crossing every level into the prediction
  # grid and drawing redundant, perfectly overlapping traces.
  inherited_columns <- if (layer$kind %in% c("line", "segment")) {
    inherited <- spec$aesthetics[names(spec$aesthetics) %in% names(layer$args) == FALSE]
    label_columns(inherited)
  } else {
    character()
  }
  support_columns <- setdiff(
    inherited_columns,
    c(facts$predictor_columns, facts$outcome_columns)
  )

  # Retain the old iteration order so failures involving malformed columns do
  # not reorder the package's diagnostics. Only model predictors become grid
  # dimensions; inherited support columns are scalar.
  grid_columns <- unique(c(facts$predictor_columns, facts$aesthetic_columns))
  for (column in grid_columns) {
    column_data <- spec$data[[column]]
    if (column %in% facts$outcome_columns) {
      abort("How did you use the outcome as a predictor?", call = call)
    } else if (column %in% facts$predictor_columns && !is.numeric(column_data)) {
      params[[column]] <- if (is.logical(column_data)) {
        c(TRUE, FALSE)
      } else {
        levels(factor(column_data))
      }
    } else if (column %in% facts$predictor_columns && column %in% facts$axis_columns) {
      # the observed range, and never the plot's axis. A model's line is a claim
      # about where its predictions are warranted, and inside the data every
      # point interpolated has observations bracketing it. Outside there are
      # none, and only a theory or a physical constraint can license the claim
      # -- neither of which a plot can find out by measuring itself. Whatever
      # widened the axis (a `gf_lims()`, a `gf_b()` intercept dot at zero) is
      # not evidence about what the model does out there.
      #
      # a predictor with a missing value gives range() an NA endpoint, and the
      # prediction grid seq() builds from it aborts before anything is drawn
      rng <- range(column_data, na.rm = TRUE)
      points <- min(max(nrow(spec$data), 80L), model_grid_max_points)
      params[[column]] <- seq(rng[[1]], rng[[2]], length.out = points)
    } else if (column %in% facts$predictor_columns) {
      spread <- stats::sd(column_data, na.rm = TRUE)
      middle <- mean(column_data, na.rm = TRUE)
      params[[column]] <- c(middle - spread, middle, middle + spread)
    } else if (column %in% support_columns) {
      support[[column]] <- model_grid_support_value(column_data)
    }
  }

  grid <- expand.grid(if (length(params)) params else list(dummy = 1))
  for (column in names(support)) {
    grid[[column]] <- rep(support[[column]], nrow(grid))
  }
  grid[mspec$outcome] <- stats::predict(mspec$fit, newdata = grid)

  # The formula names what to fit; the plot names what to show. An inherited
  # outcome expression is re-evaluated against this grid, which is right for
  # sqrt() and catastrophic for shuffle(): geom_line then joins the correct
  # predictions in a random order. Evaluate it here, once, and travel as a
  # plain column of the layer's own grid.
  if (layer$kind %in% c("line", "segment")) {
    outcome_quo <- spec$resolve_aes(names(facts$outcome_axis))$quo
    prediction <- grid[[mspec$outcome]]
    drawn <- if (is.name(quo_get_expr(outcome_quo))) {
      prediction
    } else {
      probe <- with_random_seed_restored(eval_tidy(outcome_quo, grid))
      # a permutation is the one thing that leaves sort() unchanged
      if (identical(sort(probe), sort(prediction))) prediction else probe
    }
    grid$.model_outcome <- drawn
    layer$args[[names(facts$outcome_axis)]] <- ~.model_outcome
  }

  list(grid = grid, args = layer$args)
}

#' Decide what to draw for a model on a plot
#'
#' @param spec A `plot_spec()` list.
#' @param mspec A `model_spec()` list.
#' @param args Named list of user arguments (aesthetics and layer parameters).
#'
#' @return A list with `kind`, `args`, `grid` and `tag`.
#'
#' @noRd
model_plan <- function(spec, mspec, args = list(), call = caller_env()) {
  check_model_axes(spec, call = call)

  if (!is.null(args$color)) {
    args$colour <- args$color
    args$color <- NULL
  }

  # a model's terms and a plot's mappings are both recorded as labels, but
  # predict() consumes columns -- log(age) is not a column, age is -- so every
  # phase works from the same column-level facts.
  facts <- model_plan_facts(spec, mspec)
  check_model_plan(spec, mspec, args, facts, call)
  layer <- model_layer_plan(spec, args, facts, call)
  prediction <- model_prediction_grid(spec, mspec, layer, facts, call)

  list(
    kind = layer$kind,
    args = prediction$args,
    grid = prediction$grid,
    tag = "model"
  )
}
