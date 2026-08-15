# Constraints ---------------------------------------------------------------------------------

# Answered: no, and deliberately not. gf_model() draws a claim you made, and naming the claim
# is the feature -- it is the thing gf_lm() cannot do. The only model a default could infer
# from the axes is lm(y ~ x), which is exactly what gf_lm() already draws, so a default would
# have nothing of its own to be. What the model may be has widened instead: either a model
# already fit, or the formula for one, fit against the data the plot was built from.
test_that("it needs to be layered onto a plot", {
  gf_model(lm(later_anxiety ~ NULL, data = er)) %>%
    expect_error()
})

test_that("the model variables must be in the underlying plot", {
  wrong_model <- lm(Thumb ~ NULL, data = Fingers)
  gf_point(later_anxiety ~ base_anxiety, color = ~condition, data = er) %>%
    gf_model(wrong_model) %>%
    expect_error(".*missing in plot: Thumb.*")
})

test_that("the model outcome has to be one of the axes", {
  gf_point(base_anxiety ~ condition, color = ~later_anxiety, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety, data = er)) %>%
    expect_error(".*model outcome.*one of the axes.*")
})

# The layer_factory contract -------------------------------------------------------------------

model_layer_of <- function(p) p$layers[[layer_index(p, "model")]]
built_model <- function(p) ggplot2::ggplot_build(p)$data[[layer_index(p, "model")]]

# any guide box that has grobs in it; the name and the count differ across
# ggplot2 versions, the presence of a rendered key does not
legend_grobs <- function(p) {
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  boxes <- gt$grobs[grepl("^guide-box", gt$layout$name)]
  sum(vapply(boxes, function(g) length(g$grobs), integer(1)))
}

test_that("the model is accepted positionally, which is how every call site writes it", {
  model <- lm(later_anxiety ~ base_anxiety, data = er)
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)

  piped <- p %>% gf_model(model)
  named <- gf_model(p, model = model)
  direct <- gf_model(p, model)

  expect_equal(layer_index(piped, "model"), 2L)
  expect_equal(class(model_layer_of(piped)$geom)[[1]], "GeomLine")
  expect_equal(class(model_layer_of(named)$geom)[[1]], "GeomLine")
  expect_equal(class(model_layer_of(direct)$geom)[[1]], "GeomLine")
  expect_equal(built_model(piped), built_model(named))
})

test_that("a formula is accepted positionally too, and fits the same model", {
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)
  from_formula <- p %>% gf_model(later_anxiety ~ base_anxiety)
  from_fit <- p %>% gf_model(lm(later_anxiety ~ base_anxiety, data = er))

  expect_equal(built_model(from_formula), built_model(from_fit))
})

test_that("the labelling arguments every ggformula layer takes reach the plot", {
  p <- gf_point(later_anxiety ~ base_anxiety, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety, data = er), title = "TT", xlab = "XX", ylab = "YY")

  expect_equal(p$labels$title, "TT")
  expect_equal(p$labels$x, "XX")
  expect_equal(p$labels$y, "YY")
})

test_that("a bare call emits ggformula's help instead of drawing", {
  expect_message(result <- gf_model(), "key attributes")
  expect_null(result)
})

test_that("show.help = TRUE emits help even with a plot and no model to check", {
  # every other gf_* layer answers an explicit show.help before touching its
  # other arguments; gf_model() must too, rather than reaching the missing-model
  # guard first and aborting instead of printing help
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)
  expect_message(result <- gf_model(p, show.help = TRUE), "key attributes")
  expect_null(result)
})

test_that("arguments routed through ... still reach the layer", {
  # exactly once each: model_plan() has already turned the caller's arguments into
  # the plan's, so handing ggformula's copy to the layer as well would supply
  # `color` beside `colour` and ggplot2 would warn about a duplicated aesthetic
  expect_no_warning(
    p <- gf_point(later_anxiety ~ base_anxiety, data = er) %>%
      gf_model(lm(later_anxiety ~ base_anxiety, data = er), color = "firebrick", linewidth = 3)
  )

  expect_equal(model_layer_of(p)$aes_params$colour, "firebrick")
  expect_equal(model_layer_of(p)$aes_params$linewidth, 3)
  expect_false("color" %in% names(model_layer_of(p)$aes_params))
})

test_that("an aesthetic mapped on the model layer produces a legend", {
  base <- gf_point(later_anxiety ~ condition, data = er)
  p <- base %>% gf_model(lm(later_anxiety ~ condition, data = er), color = ~condition)

  expect_equal(legend_grobs(base), 0L)
  expect_gt(legend_grobs(p), 0L)
  expect_true("colour" %in% names(model_layer_of(p)$mapping))
  expect_length(unique(built_model(p)$colour), nlevels(factor(er$condition)))
})

test_that("faceting repeats a named model's claim and never refits it", {
  model <- lm(later_anxiety ~ base_anxiety, data = er)
  expect_no_warning(
    p <- gf_point(later_anxiety ~ base_anxiety, data = er) %>%
      gf_facet_grid(condition ~ .) %>%
      gf_model(model)
  )
  drawn <- split(built_model(p), built_model(p)$PANEL)

  expect_length(drawn, nlevels(factor(er$condition)))
  for (panel in drawn) {
    expect_equal(
      panel$y,
      unname(stats::predict(model, newdata = data.frame(base_anxiety = panel$x)))
    )
  }
  expect_length(unique(vapply(drawn, function(d) unname(coef(lm(y ~ x, d))[[2]]), 0)), 1)
})

test_that("a faceted two-sided formula is fit once against the full plot data", {
  base <- gf_point(later_anxiety ~ base_anxiety, data = er) %>%
    gf_facet_grid(condition ~ .)
  expect_no_warning(from_formula <- base %>% gf_model(later_anxiety ~ base_anxiety))
  from_fit <- base %>% gf_model(lm(later_anxiety ~ base_anxiety, data = er))

  expect_equal(built_model(from_formula), built_model(from_fit))
})

test_that("panel-specific model claims must be explicit in the supplied model", {
  model <- lm(later_anxiety ~ base_anxiety * condition, data = er)
  expect_no_warning(
    p <- gf_point(later_anxiety ~ base_anxiety, data = er) %>%
      gf_facet_grid(condition ~ .) %>%
      gf_model(model)
  )
  built <- ggplot2::ggplot_build(p)
  drawn <- built$data[[layer_index(p, "model")]]
  panel_keys <- built$layout$layout[c("PANEL", "condition")]

  for (i in seq_len(nrow(panel_keys))) {
    key <- panel_keys[i, ]
    panel <- drawn[drawn$PANEL == key$PANEL, ]
    expected <- data.frame(base_anxiety = panel$x, condition = key$condition)
    expect_equal(panel$y, unname(stats::predict(model, newdata = expected)))
  }
})

test_that("gf_model says what it needs when it is given no plot", {
  # data-first piping is the one contract row gf_model cannot honour: it draws a
  # model over a plot's own axes, so there has to be a plot
  expect_error(er %>% gf_model(lm(later_anxiety ~ NULL, data = er)), "layered on top of a plot")
  expect_error(gf_model(lm(later_anxiety ~ NULL, data = er)), "layered on top of a plot")
})

# The invariant ----------------------------------------------------------------------------------

model_shapes <- function() {
  list(
    hline = list(gf_point(later_anxiety ~ base_anxiety, data = er),
      lm(later_anxiety ~ NULL, data = er)),
    vline = list(gf_histogram(~later_anxiety, data = er, bins = 30),
      lm(later_anxiety ~ NULL, data = er)),
    line = list(gf_point(later_anxiety ~ base_anxiety, data = er),
      lm(later_anxiety ~ base_anxiety, data = er)),
    group = list(gf_jitter(later_anxiety ~ condition, data = er, width = .1),
      lm(later_anxiety ~ condition, data = er)),
    flipped_line = list(gf_point(base_anxiety ~ later_anxiety, data = er),
      lm(later_anxiety ~ base_anxiety, data = er)),
    flipped_group = list(gf_boxplot(condition ~ later_anxiety, data = er),
      lm(later_anxiety ~ condition, data = er))
  )
}

test_that("the axis the plot puts the outcome on is never mapped by the model layer", {
  # THE INVARIANT, in the only form that is true of all six shapes. Of the two
  # positional aesthetics x and y, the model layer never maps the one the plot is
  # using to carry the outcome -- it leaves that one free and inherits it. That is
  # the only reason a flipped plot draws correctly; nothing anywhere reasons about
  # orientation at draw time. xend/yend and the intercepts are terminal companions
  # ggplot2 cannot inherit, so they are named outright and are not part of this.
  for (name in names(model_shapes())) {
    shape <- model_shapes()[[name]]
    outcome <- "later_anxiety"
    axes <- plot_spec(shape[[1]])$axes
    outcome_axis <- names(axes)[axes == outcome]
    mapping <- model_layer_of(shape[[1]] %>% gf_model(shape[[2]]))$mapping

    expect_false(outcome_axis %in% names(mapping), label = paste(name, "leaves", outcome_axis))

    positional <- mapping[intersect(names(mapping), c("x", "y"))]
    drawn <- vapply(positional, rlang::as_label, character(1))
    expect_false(outcome %in% drawn, label = paste(name, "maps the outcome positionally"))
  }
})

test_that("each shape inherits exactly the aesthetics it can use", {
  # An intercept spans the panel on its own and must not be handed the plot's x
  # and y; every other shape must be, or the outcome never arrives. Only a build
  # says which -- the mapping is resolved against the prediction grid there.
  for (name in names(model_shapes())) {
    shape <- model_shapes()[[name]]
    p <- shape[[1]] %>% gf_model(shape[[2]])
    drawn <- built_model(p)
    panel <- ggplot2::ggplot_build(shape[[1]])$layout$panel_params[[1]]
    xs <- unlist(drawn[intersect(names(drawn), c("x", "xend", "xmin", "xmax", "xintercept"))])
    ys <- unlist(drawn[intersect(names(drawn), c("y", "yend", "ymin", "ymax", "yintercept"))])

    expect_gt(nrow(drawn), 0)
    expect_true(all(is.finite(xs)) && all(is.finite(ys)), label = name)
    expect_true(all(xs >= panel$x.range[[1]] & xs <= panel$x.range[[2]]), label = name)
    expect_true(all(ys >= panel$y.range[[1]] & ys <= panel$y.range[[2]]), label = name)
  }
})

# Orientation ------------------------------------------------------------------------------------

test_that("a flipped continuous predictor draws the fit inside the data's panel", {
  model <- lm(later_anxiety ~ base_anxiety, data = er)
  base <- gf_point(base_anxiety ~ later_anxiety, data = er)
  p <- base %>% gf_model(model)

  drawn <- built_model(p)
  panel <- ggplot2::ggplot_build(base)$layout$panel_params[[1]]

  # the predictor spans its own range on y; the prediction lands on x
  expect_equal(range(drawn$y), range(er$base_anxiety))
  expect_equal(
    drawn$x,
    unname(stats::predict(model, newdata = data.frame(base_anxiety = drawn$y)))
  )
  expect_true(all(drawn$x >= panel$x.range[[1]] & drawn$x <= panel$x.range[[2]]))
  expect_true(all(drawn$y >= panel$y.range[[1]] & drawn$y <= panel$y.range[[2]]))
})

test_that("a flipped group model draws its marks inside the data's panel", {
  model <- lm(later_anxiety ~ condition, data = er)
  base <- gf_boxplot(condition ~ later_anxiety, data = er)
  p <- base %>% gf_model(model)

  drawn <- built_model(p)
  panel <- ggplot2::ggplot_build(base)$layout$panel_params[[1]]
  xs <- unlist(drawn[intersect(names(drawn), c("x", "xend"))])
  ys <- unlist(drawn[intersect(names(drawn), c("y", "yend"))])

  expect_equal(nrow(drawn), nlevels(factor(er$condition)))
  expect_setequal(
    round(unique(xs), 8),
    round(unname(tapply(er$later_anxiety, factor(er$condition), mean)), 8)
  )
  expect_true(all(xs >= panel$x.range[[1]] & xs <= panel$x.range[[2]]))
  expect_true(all(ys >= panel$y.range[[1]] & ys <= panel$y.range[[2]]))
})

# The guards -------------------------------------------------------------------------------------

test_that("every guard fires at the call, not at the draw", {
  # ggplot2 turns an error raised in compute_group() into a warning and hands back
  # a zero-row layer, so a guard that drifted into the stat would let a wrong model
  # through in silence. None of these builds a plot.
  expect_error(
    (ggplot2::ggplot(er) + ggplot2::geom_hex()) %>%
      gf_model(lm(later_anxiety ~ base_anxiety, data = er)),
    "gf_model\\(\\) supports"
  )
  expect_error(
    gf_point(later_anxiety ~ base_anxiety, data = er) %>%
      gf_model(lm(Thumb ~ NULL, data = Fingers)),
    "missing in plot: Thumb"
  )
  expect_error(
    gf_point(base_anxiety ~ condition, color = ~later_anxiety, data = er) %>%
      gf_model(lm(later_anxiety ~ base_anxiety, data = er)),
    "one of the axes"
  )
  expect_error(
    gf_point(later_anxiety ~ base_anxiety, data = er) %>%
      gf_model(lm(later_anxiety ~ base_anxiety, data = er), color = ~condition),
    "not predictors in the model"
  )
  expect_error(
    gf_point(later_anxiety ~ base_anxiety, color = ~condition, shape = ~provider, data = er) %>%
      gf_model(lm(later_anxiety ~ condition + provider, data = er)),
    "multiple variables mapped"
  )
})

test_that("a plot with no model is told what a model looks like", {
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)

  expect_error(p %>% gf_model(), "which model to draw")
  # both spellings are named, because both work
  expect_error(p %>% gf_model(), "lm\\(")
  expect_error(p %>% gf_model(), "~")
})

test_that("a one-sided formula is refused rather than guessed at", {
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)

  expect_error(p %>% gf_model(~base_anxiety), "what the model predicts")
  # the spelling that is adopted still works
  expect_s3_class(p %>% gf_model(later_anxiety ~ NULL), "ggplot")
})

test_that("a shape that inherits the outcome refuses a plot that maps it on a layer", {
  # the fit line and the group mark leave the outcome's axis free and inherit it,
  # so the outcome has to be on the plot; without this they die at build time in
  # ggplot2's words instead of at the call in ours
  scatter <- ggplot2::ggplot() +
    ggplot2::geom_point(data = er, mapping = ggplot2::aes(base_anxiety, later_anxiety))
  groups <- ggplot2::ggplot() +
    ggplot2::geom_point(data = er, mapping = ggplot2::aes(condition, later_anxiety))

  expect_error(
    scatter %>% gf_model(lm(later_anxiety ~ base_anxiety, data = er)),
    "mapped by a layer rather than by the plot"
  )
  expect_error(
    groups %>% gf_model(lm(later_anxiety ~ condition, data = er)),
    "mapped by a layer rather than by the plot"
  )
})

test_that("an intercept draws on a plot that maps only on its layer", {
  # hline and vline state their position outright and inherit nothing, so a
  # layer-mapped plot is no obstacle to them
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = er, mapping = ggplot2::aes(base_anxiety, later_anxiety))
  drawn <- p %>% gf_model(lm(later_anxiety ~ NULL, data = er))

  expect_equal(built_model(drawn)$yintercept, mean(er$later_anxiety))
})
