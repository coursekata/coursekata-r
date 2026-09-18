#' Read the expressions an explicit model must apply to its prediction grid
#'
#' A pinned column contains observation values, not values on the prediction
#' grid. Use its original expression for the fixed model claim without changing
#' the pinned observations or their mappings.
#' @noRd
model_plot_spec <- function(object) {
  spec <- plot_spec(object)
  originals <- list()
  for (aesthetic in names(spec$pins)) {
    if (!is.null(spec$mapping[[aesthetic]]) &&
        identical(quo_get_expr(spec$mapping[[aesthetic]]),
                  sym(paste0(".coursekata_pin_", aesthetic)))) {
      originals[[aesthetic]] <- spec$pins[[aesthetic]]
      spec$mapping[[aesthetic]] <- spec$pins[[aesthetic]]
    }
  }
  resolve <- spec$resolve_aes
  spec$resolve_aes <- function(aesthetic) {
    resolved <- resolve(aesthetic)
    if (!is.null(originals[[aesthetic]])) resolved$quo <- originals[[aesthetic]]
    resolved
  }
  spec
}

#' Shared explicit-model preparation for native and formula interfaces
#'
#' @noRd
model_layer_spec <- function(object, model, args = list(), fn = "gf_model",
                              call = caller_env()) {
  if (!inherits(object, c("gg", "ggplot"))) {
    abort(
      c(
        "`gf_model()` needs to be layered on top of a plot.",
        i = "start one: `gf_point(Thumb ~ Height, data = Fingers) %>% gf_model()`"
      ),
      call = call
    )
  }

  if (is_formula(model) && is.null(f_lhs(model))) {
    abort(
      c(
        "`gf_model()` needs to be told what the model predicts",
        x = glue("`{deparse1(model)}` names predictors but no outcome"),
        i = "write the outcome on the left: `body_mass_kg ~ species`",
        i = "a model with no predictors is written `body_mass_kg ~ NULL`"
      ),
      call = call
    )
  }

  spec <- model_plot_spec(object)
  mspec <- model_spec(spec$data, model, call = call)
  if (isTRUE(args$se)) {
    abort(glue("`{fn}()` supports `se = TRUE` only without a supplied `model`."), call = call)
  }
  plan <- model_plan(spec, mspec, args, fn = fn, call = call)

  if (identical(plan$kind, "hline")) {
    plan$args$y <- plan$args$yintercept
    plan$args$yintercept <- NULL
  } else if (identical(plan$kind, "vline")) {
    plan$args$x <- plan$args$xintercept
    plan$args$xintercept <- NULL
  }
  plan$grid$.model_kind <- plan$kind
  plan$args$.model_kind <- ~.model_kind
  mapped <- purrr::map_lgl(plan$args, ~ is_formula(.x) && length(.x) == 2L)

  list(
    geom = GeomModel,
    data = plan$grid,
    aesthetics = do.call(ggplot2::aes, purrr::map(plan$args[mapped], function(value) {
      new_quosure(f_rhs(value), f_env(value))
    })),
    params = plan$args[!mapped],
    # An intercept carries the whole claim and spans the panel on its own.
    # Lines and group marks still need compatible plot aesthetics, including
    # the predictor position and any model grouping.
    inherit = !(plan$kind %in% c("hline", "vline")),
    orientation = plan$orientation,
    tag = plan$tag
  )
}

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
  missing_in_plot <- setdiff(facts$model_columns, names(spec$data))
  if (length(missing_in_plot) > 0) {
    abort(
      c(
        "The model uses variables that do not exist in the plot's data",
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

  if (length(facts$outcome_axis) != 1) {
    abort(
      c(
        "The model outcome variable must be represented on one of the axes (exactly one)",
        glue("model outcome: {mspec$outcome}"),
        glue("plot axes: {collapse(spec$axes)}")
      ),
      call = call
    )
  }

  check_numeric_outcome(mspec$outcome, mspec$data[[mspec$outcome]], call)

  if (length(facts$non_outcome_axis)) {
    focal <- intersect(label_columns(facts$non_outcome_axis), facts$predictor_columns)
    if (length(facts$predictor_columns) && length(focal) != 1L) {
      abort("Map one unambiguous model predictor on the non-outcome axis.", call = call)
    }
  }

  mapped <- purrr::keep(args, is_formula)
  if (model_is_intercept(facts)) mapped$group <- NULL
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

#' Whether a model spans the panel without a displayed predictor
#' @noRd
model_is_intercept <- function(facts) {
  length(facts$predictor_columns) == 0L ||
    (length(facts$predictor_columns) == 1L &&
       !facts$predictor_columns %in% facts$axis_columns)
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
  mapped_formula <- function(aesthetic) {
    quo <- spec$mapping[[aesthetic]]
    if (is_quosure(quo)) {
      new_formula(NULL, quo_get_expr(quo), quo_get_env(quo))
    } else {
      new_formula(NULL, quo, base_env())
    }
  }
  not_in_model <- spec$variables[
    purrr::map_lgl(facts$columns_by_variable, ~ any(.x %in% facts$model_columns) == FALSE)
  ]
  for (aesthetic in names(not_in_model)) {
    if (aesthetic %in% ggplot2::GeomLine$aesthetics() && is.null(args[[aesthetic]])) {
      args[[aesthetic]] <- ggplot2::get_geom_defaults("line")[[aesthetic]]
    }
  }

  non_axis_predictor <- setdiff(facts$predictor_columns, facts$axis_columns)
  if (length(non_axis_predictor) > 1) {
    abort("A model layer supports at most one predictor away from the displayed axis.", call = call)
  }

  # the shape drawn is a property of what the plot puts on the non-outcome
  # axis, not of which column its mapping names -- a transformed axis (e.g.
  # log(age)) still shows a numeric axis to draw a line against. Read after
  # every abort above, so a mapping that cannot be evaluated never pre-empts
  # a validation message.
  along <- if (length(facts$non_outcome_axis) == 1) {
    spec$resolve_aes(names(facts$non_outcome_axis))
  }
  along_values <- if (!is.null(along)) {
    with_random_seed_restored(eval_tidy(along$quo, along$data))
  }

  if (model_is_intercept(facts)) {
    if (facts$flipped) {
      kind <- "vline"
      geom <- ggplot2::GeomVline
      # the intercept inherits nothing, so it is the one shape that has to
      # spell the plot's own mapping out rather than inheriting it
      args$xintercept <- mapped_formula("x")
    } else {
      kind <- "hline"
      geom <- ggplot2::GeomHline
      args$yintercept <- mapped_formula("y")
    }
  } else if (is.numeric(along_values)) {
    kind <- "line"
    geom <- ggplot2::GeomLine
  } else {
    kind <- "segment"
    geom <- GeomModelMark
    args$width <- args$width %||% .4
  }

  if (kind %in% c("line", "segment")) {
    if (!"group" %in% names(args) && "group" %in% names(spec$mapping)) {
      args$group <- mapped_formula("group")
    }
    if (!"group" %in% names(args) && length(non_axis_predictor) == 1L) {
      args$group <- name_to_frm(non_axis_predictor)
    }
  } else {
    args$group <- NULL
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
  if (!kind %in% c("line", "segment")) remap <- remap[names(remap) != "group"]
  remap <- remap[names(remap) %in% names(args) == FALSE]
  args[names(remap)] <- purrr::map(names(remap), mapped_formula)

  if (!width_given && "size" %in% names(spec$aesthetics)) {
    args$linewidth <- mapped_formula("size")
  }

  if (
    is.null(args$colour) &&
      "colour" %in% names(spec$aesthetics) == FALSE &&
      "fill" %in% names(spec$aesthetics)
  ) {
    args$colour <- mapped_formula("fill")
  }

  if (kind == "segment") {
    # Group marks use the same neutral colour as model lines unless the caller
    # or plot supplies one.
    args$colour <- args$colour %||% ggplot2::get_geom_defaults("line")$colour
  }

  # State inherited mappings explicitly so a native layer can use this plan
  # without evaluating the destination plot's outcome expression a second time.
  if (kind %in% c("line", "segment")) {
    inherited <- intersect(
      setdiff(names(spec$mapping), c(names(args), names(facts$outcome_axis))),
      GeomModel$aesthetics()
    )
    args[inherited] <- purrr::map(inherited, mapped_formula)
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
    mapped <- purrr::keep(layer$args, ~ is_formula(.x) && length(.x) == 2L)
    inherited <- spec$aesthetics[names(spec$aesthetics) %in% names(layer$args) == FALSE]
    unique(c(label_columns(inherited),
             label_columns(vapply(mapped, function(value) as_label(f_rhs(value)), character(1)))))
  } else {
    character()
  }
  support_columns <- setdiff(
    inherited_columns,
    c(facts$predictor_columns, facts$outcome_columns)
  )

  # Predictor columns define grid dimensions; inherited support columns are scalar.
  grid_columns <- unique(c(facts$predictor_columns, support_columns))
  points <- layer$args[["n"]] %||% min(max(nrow(spec$data), 80L), model_grid_max_points)
  if (!is.numeric(points) || length(points) != 1L || !is.finite(points) ||
      points < 2 || points > .Machine$integer.max || points != as.integer(points)) {
    abort("`n` must be one integer greater than 1.", call = call)
  }
  for (column in grid_columns) {
    column_data <- spec$data[[column]]
    if (column %in% facts$predictor_columns) {
      params[[column]] <- model_grid_values(
        column_data, dense = column %in% facts$axis_columns, n = points
      )
    } else if (column %in% support_columns) {
      support[[column]] <- model_grid_support_value(column_data)
    }
  }

  grid <- expand.grid(if (length(params)) params else list(dummy = 1),
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  for (column in names(support)) {
    grid[[column]] <- rep(support[[column]], nrow(grid))
  }
  grid[mspec$outcome] <- stats::predict(mspec$fit, newdata = grid)

  # The formula names what to fit; the plot names what to show. An inherited
  # outcome expression is re-evaluated against this grid, which is right for
  # sqrt() and catastrophic for shuffle(): geom_line then joins the correct
  # predictions in a random order. Evaluate it here, once, and travel as a
  # plain column of the layer's own grid.
  if (length(facts$outcome_axis)) {
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
    position <- switch(layer$kind, hline = "yintercept", vline = "xintercept",
                       names(facts$outcome_axis))
    layer$args[[position]] <- ~.model_outcome
  }

  list(grid = grid, args = layer$args)
}

#' Decide what to draw for a model on a plot
#'
#' @param spec A `plot_spec()` list.
#' @param mspec A `model_spec()` list.
#' @param args Named list of user arguments (aesthetics and layer parameters).
#'
#' @return A list with `kind`, `args`, `grid`, `orientation`, and `tag`.
#'
#' @noRd
model_plan <- function(spec, mspec, args = list(), fn = "gf_model", call = caller_env()) {
  check_model_axes(spec, fn = fn, call = call)

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
    orientation = if (facts$flipped) "y" else "x",
    tag = "model"
  )
}
