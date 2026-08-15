plan_for <- function(p, model, ...) {
  spec <- plot_spec(p)
  model_plan(spec, model_spec(spec$data, model), rlang::list2(...))
}

test_that("the empty model draws a horizontal line when the outcome is on y", {
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ NULL, data = er))

  expect_equal(plan$kind, "hline")
  expect_true("yintercept" %in% names(plan$args))
  expect_equal(plan$tag, "model")
})

test_that("the empty model draws a vertical line when the outcome is on x", {
  p <- gf_point(base_anxiety ~ later_anxiety, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ NULL, data = er))

  expect_equal(plan$kind, "vline")
  expect_true("xintercept" %in% names(plan$args))
})

test_that("a continuous predictor on the axis draws a fit line", {
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ base_anxiety, data = er))

  expect_equal(plan$kind, "line")
  expect_true(all(c("base_anxiety", "later_anxiety") %in% names(plan$grid)))
  expect_equal(nrow(plan$grid), nrow(er))
})

test_that("the fit line is drawn from at least 80 points however small the data", {
  er20 <- er[1:20, ]
  p <- gf_point(later_anxiety ~ base_anxiety, data = er20)
  plan <- plan_for(p, lm(later_anxiety ~ base_anxiety, data = er20))

  expect_equal(nrow(plan$grid), 80L)
})

test_that("a categorical predictor on the axis draws a mark at each group mean", {
  p <- gf_point(later_anxiety ~ condition, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ condition, data = er))

  expect_equal(plan$kind, "segment")
  expect_equal(plan$args$width, .4)
  expect_equal(plan$args$mark_axis, "x")
  expect_equal(nrow(plan$grid), nlevels(factor(er$condition)))
  expect_equal(
    sort(plan$grid$later_anxiety),
    sort(unname(as.vector(tapply(er$later_anxiety, er$condition, mean)))),
    tolerance = 1e-8
  )
})

test_that("the group mark spans the group position and claims one value", {
  p <- gf_point(later_anxiety ~ condition, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ condition, data = er))

  expect_equal(rlang::f_rhs(plan$args$x), quote(condition))
  expect_false("y" %in% names(plan$args))
  expect_equal(plan$args$width, .4)
  expect_equal(plan$args$mark_axis, "x")
  expect_false(any(c("ymin", "ymax", "xmin", "xmax") %in% names(plan$args)))
})

test_that("the group mark advertises the parameters it consumes", {
  expect_true(all(c("width", "mark_axis") %in% GeomModelMark$parameters(TRUE)))
})

test_that("a categorical predictor on y puts the group position on y", {
  p <- gf_boxplot(condition ~ later_anxiety, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ condition, data = er))

  expect_equal(plan$kind, "segment")
  expect_equal(rlang::f_rhs(plan$args$y), quote(condition))
  expect_false("x" %in% names(plan$args))
  expect_equal(plan$args$mark_axis, "y")
  expect_false(any(c("ymin", "ymax", "xmin", "xmax") %in% names(plan$args)))
})

test_that("a continuous predictor on an aesthetic is split at the mean and +-1 SD", {
  p <- gf_point(later_anxiety ~ condition, color = ~base_anxiety, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ condition + base_anxiety, data = er))

  levels_used <- sort(unique(plan$grid$base_anxiety))
  expect_length(levels_used, 3L)
  expect_equal(
    levels_used,
    sort(c(
      mean(er$base_anxiety) - sd(er$base_anxiety),
      mean(er$base_anxiety),
      mean(er$base_anxiety) + sd(er$base_anxiety)
    ))
  )
})

test_that("the grid crosses every plot aesthetic, not only the model's predictors", {
  # A plot aesthetic the model ignores still enters the prediction grid, so
  # the same line is drawn once per level of it.
  p <- gf_point(later_anxiety ~ base_anxiety, shape = ~provider, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ base_anxiety, data = er))

  expect_true("provider" %in% names(plan$grid))
  expect_equal(
    nrow(plan$grid),
    length(unique(plan$grid$base_anxiety)) * nlevels(factor(er$provider))
  )
})

test_that("it refuses a model whose variables are not on the plot", {
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)
  expect_error(
    plan_for(p, lm(later_anxiety ~ condition, data = er)),
    "do not exist in the plot"
  )
})

test_that("it refuses a model whose outcome is not on an axis", {
  p <- gf_point(base_anxiety ~ condition, color = ~later_anxiety, data = er)
  expect_error(
    plan_for(p, lm(later_anxiety ~ condition, data = er)),
    "must be represented on the plot"
  )
})

test_that("the missing-variables error wins when the outcome is also off-axis", {
  # Pins abort order: a plot can simultaneously omit a model term AND leave
  # the outcome off-axis. Which check runs first decides which message a
  # student sees, so that order is frozen here rather than left to chance.
  p <- gf_point(heart_rate ~ resp_rate, color = ~later_anxiety, data = er)
  expect_error(
    plan_for(p, lm(later_anxiety ~ base_anxiety, data = er)),
    "do not exist in the plot"
  )
})

test_that("a predictor mapped to an aesthetic off the axes sets args$group", {
  p <- gf_point(later_anxiety ~ base_anxiety, color = ~condition, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ base_anxiety + condition, data = er))

  expect_equal(rlang::f_rhs(plan$args$group), quote(condition))
})

test_that("a model can predict from the variable the plot facets by", {
  p <- gf_point(later_anxiety ~ provider | condition, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ provider + condition, data = er))

  expect_equal(plan$kind, "segment")
  expect_true("condition" %in% names(plan$grid))
  expect_equal(rlang::f_rhs(plan$args$group), quote(condition))
})

test_that("an aesthetic the user maps to a real predictor survives into the plan", {
  p <- gf_point(later_anxiety ~ base_anxiety, color = ~condition, data = er)
  model <- lm(later_anxiety ~ base_anxiety + condition, data = er)
  plan <- plan_for(p, model, linetype = ~condition)

  expect_true(rlang::is_formula(plan$args$linetype))
  expect_equal(rlang::f_rhs(plan$args$linetype), quote(condition))
})

test_that("a static colour is renamed to colour and reaches the plan unchanged", {
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ base_anxiety, data = er), color = "dodgerblue")

  expect_equal(plan$args$colour, "dodgerblue")
  expect_null(plan$args$color)
})

test_that("size sets the linewidth, and an explicit linewidth wins over it", {
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)
  model <- lm(later_anxiety ~ base_anxiety, data = er)

  expect_equal(plan_for(p, model)$args$linewidth, 1)
  expect_equal(plan_for(p, model, size = 3)$args$linewidth, 3)
  expect_equal(plan_for(p, model, size = 3, linewidth = 5)$args$linewidth, 5)
})

test_that("the group mark keeps its default width unless the user sets one", {
  p <- gf_point(later_anxiety ~ condition, data = er)
  model <- lm(later_anxiety ~ condition, data = er)

  expect_equal(plan_for(p, model)$args$width, .4)
  expect_equal(plan_for(p, model, width = .9)$args$width, .9)
})

test_that("it refuses an aesthetic mapped to something the model does not use", {
  # Deliberate behaviour change: previously dropped in silence, now an error.
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)
  expect_error(
    plan_for(p, lm(later_anxiety ~ base_anxiety, data = er), color = ~condition),
    "not predictors in the model"
  )
})

test_that("model_spec() accepts a bare formula and matches the equivalent fitted lm()", {
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)
  plan_formula <- plan_for(p, later_anxiety ~ base_anxiety)
  plan_fit <- plan_for(p, lm(later_anxiety ~ base_anxiety, data = er))

  expect_equal(plan_formula$kind, plan_fit$kind)
  expect_equal(plan_formula$grid, plan_fit$grid)
})

test_that("a logical predictor's grid carries TRUE and FALSE, not factor level strings", {
  er2 <- er
  er2$high <- er2$base_anxiety > median(er2$base_anxiety)
  p <- gf_point(later_anxiety ~ high, data = er2)
  plan <- plan_for(p, lm(later_anxiety ~ high, data = er2))

  expect_setequal(plan$grid$high, c(TRUE, FALSE))
  expect_equal(
    sort(plan$grid$later_anxiety),
    sort(unname(as.vector(tapply(er2$later_anxiety, er2$high, mean)))),
    tolerance = 1e-8
  )
})

test_that("an unmapped colour aesthetic falls back to the plot's fill mapping", {
  p <- gf_point(later_anxiety ~ base_anxiety, fill = ~condition, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ base_anxiety + condition, data = er))

  expect_equal(rlang::f_rhs(plan$args$colour), quote(condition))
})

test_that("gf_model tags the layer it adds", {
  p <- gf_point(later_anxiety ~ base_anxiety, data = er) %>%
    gf_model(lm(later_anxiety ~ base_anxiety, data = er))

  expect_equal(layer_index(p, "model"), 2L)
})

test_that("gf_model still refuses a plot it was not layered onto", {
  expect_error(gf_model(er, lm(later_anxiety ~ NULL, data = er)), "layered on top of a plot")
})

test_that("it names the supported base plots when nothing on the plot is mapped", {
  p <- ggplot2::ggplot(er) + ggplot2::geom_hex()
  expect_error(
    plan_for(p, lm(later_anxiety ~ base_anxiety, data = er)),
    "gf_model\\(\\) supports"
  )
})

test_that("a layer-level mapping is read when the plot itself maps nothing", {
  p <- ggplot2::ggplot(er) + ggplot2::geom_hex(ggplot2::aes(base_anxiety, later_anxiety))
  plan <- plan_for(p, lm(later_anxiety ~ base_anxiety, data = er))

  expect_equal(plan$kind, "line")
})

test_that("a plot-level mapping is read whatever geom draws it", {
  p <- ggplot2::ggplot(er, ggplot2::aes(base_anxiety, later_anxiety)) + ggplot2::geom_hex()
  plan <- plan_for(p, lm(later_anxiety ~ base_anxiety, data = er))

  expect_equal(plan$kind, "line")
})

test_that("every supported base plot yields the plan kind its axes call for", {
  cases <- list(
    "gf_point, empty model" = list(
      plot = function() gf_point(later_anxiety ~ base_anxiety, data = er),
      model = function() lm(later_anxiety ~ NULL, data = er),
      kind = "hline"
    ),
    "gf_point, continuous predictor" = list(
      plot = function() gf_point(later_anxiety ~ base_anxiety, data = er),
      model = function() lm(later_anxiety ~ base_anxiety, data = er),
      kind = "line"
    ),
    "gf_jitter, categorical predictor" = list(
      plot = function() gf_jitter(later_anxiety ~ condition, data = er),
      model = function() lm(later_anxiety ~ condition, data = er),
      kind = "segment"
    ),
    "gf_boxplot, categorical predictor" = list(
      plot = function() gf_boxplot(later_anxiety ~ condition, data = er),
      model = function() lm(later_anxiety ~ condition, data = er),
      kind = "segment"
    ),
    "gf_violin, categorical predictor" = list(
      plot = function() gf_violin(later_anxiety ~ condition, data = er),
      model = function() lm(later_anxiety ~ condition, data = er),
      kind = "segment"
    ),
    "gf_histogram, empty model" = list(
      plot = function() gf_histogram(~later_anxiety, data = er, bins = 30),
      model = function() lm(later_anxiety ~ NULL, data = er),
      kind = "vline"
    )
  )

  kinds <- vapply(cases, function(case) plan_for(case$plot(), case$model())$kind, character(1))
  expect_equal(kinds, vapply(cases, function(case) case$kind, character(1)))
})

test_that("a non-numeric outcome is refused before lm() coerces it", {
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)

  expect_error(plan_for(p, condition ~ base_anxiety), "numeric outcome variables")
  # the symptom of the guard sitting after the fit: lm() coerced first and the
  # student got "NA/NaN/Inf in 'y'" on top of a coercion warning
  expect_no_warning(try(plan_for(p, condition ~ base_anxiety), silent = TRUE))
})

test_that("the refusal names the outcome's own type", {
  er_f <- er
  er_f$grp <- factor(er_f$condition)
  p <- gf_point(grp ~ base_anxiety, data = er_f)

  expect_error(plan_for(p, grp ~ base_anxiety), "detected outcome type: factor")
})

test_that("a logical outcome reports logical, not the class of its name", {
  er_l <- er
  er_l$hi <- er_l$base_anxiety > median(er_l$base_anxiety)
  p <- gf_point(hi ~ base_anxiety, data = er_l)

  expect_error(
    plan_for(p, lm(hi ~ base_anxiety, data = er_l)),
    "detected outcome type: logical"
  )
})

test_that("size is folded into linewidth and does not also reach the layer", {
  p <- gf_point(later_anxiety ~ base_anxiety, data = er)
  model <- lm(later_anxiety ~ base_anxiety, data = er)

  expect_null(plan_for(p, model, size = 3)$args$size)
  expect_no_warning(suppressMessages(gf_model(p, model, size = 3)))
})

test_that("a size mapped by the plot sets the linewidth only if none was given", {
  p <- gf_point(later_anxiety ~ base_anxiety, size = ~base_anxiety, data = er)
  model <- lm(later_anxiety ~ base_anxiety, data = er)

  expect_equal(rlang::f_rhs(plan_for(p, model)$args$linewidth), quote(base_anxiety))
  expect_equal(plan_for(p, model, linewidth = 5)$args$linewidth, 5)
  expect_equal(plan_for(p, model, size = 5)$args$linewidth, 5)
})

test_that("the group mark is drawn as one two-point segment", {
  p <- gf_point(later_anxiety ~ condition, data = er) %>%
    gf_model(lm(later_anxiety ~ condition, data = er))
  i <- layer_index(p, "model")
  d <- ggplot2::ggplot_build(p)$data[[i]]

  expect_equal(d$x, c(0.8, 1.8))
  expect_equal(d$xend, c(1.2, 2.2))
  expect_equal(d$y, d$yend)
  expect_false(any(c("ymin", "ymax", "flipped_aes") %in% names(d)))

  g <- ggplot2::layer_grob(p, i)[[1]]
  expect_s3_class(g, "segments")
  expect_length(g$x0, 2L)
})

test_that("a flipped group model draws inside the panel", {
  p <- gf_boxplot(condition ~ later_anxiety, data = er) %>%
    gf_model(lm(later_anxiety ~ condition, data = er))
  b <- ggplot2::ggplot_build(p)
  d <- b$data[[layer_index(p, "model")]]
  pp <- b$layout$panel_params[[1]]

  expect_equal(d$y, c(0.8, 1.8))
  expect_equal(d$yend, c(1.2, 2.2))
  expect_equal(d$x, d$xend)
  expect_true(all(d$y >= pp$y.range[[1]] & d$yend <= pp$y.range[[2]]))
  expect_true(all(d$x >= pp$x.range[[1]] & d$xend <= pp$x.range[[2]]))
})

test_that("every group draws at its own position, whatever the grid crosses", {
  p <- gf_point(later_anxiety ~ provider | condition, data = er) %>%
    gf_model(lm(later_anxiety ~ provider + condition, data = er), width = .9)
  d <- ggplot2::ggplot_build(p)$data[[layer_index(p, "model")]]

  expect_equal(d$xend - d$x, rep(.9, nrow(d)))
  expect_setequal((d$x + d$xend) / 2, seq_len(nlevels(factor(er$provider))))
})

test_that("the group mark follows a reordered discrete scale", {
  expected <- tapply(er$later_anxiety, er$condition, mean)
  levels <- rev(names(expected))
  p <- gf_point(later_anxiety ~ condition, data = er) %>%
    gf_model(lm(later_anxiety ~ condition, data = er)) %>%
    gf_refine(ggplot2::scale_x_discrete(limits = levels))
  b <- ggplot2::ggplot_build(p)
  d <- b$data[[layer_index(p, "model")]]
  labels <- b$layout$panel_params[[1]]$x$get_labels()

  expect_equal((d$x + d$xend) / 2, match(names(expected), labels))
  expect_equal(d$y, as.vector(expected))
})

test_that("a flipped group mark follows a reordered discrete scale", {
  expected <- tapply(er$later_anxiety, er$condition, mean)
  levels <- rev(names(expected))
  p <- gf_boxplot(condition ~ later_anxiety, data = er) %>%
    gf_model(lm(later_anxiety ~ condition, data = er)) %>%
    gf_refine(ggplot2::scale_y_discrete(limits = levels))
  b <- ggplot2::ggplot_build(p)
  d <- b$data[[layer_index(p, "model")]]
  labels <- b$layout$panel_params[[1]]$y$get_labels()

  expect_equal((d$y + d$yend) / 2, match(names(expected), labels))
  expect_equal(d$x, as.vector(expected))
})

test_that("the group mark keeps the neutral colour the fit line uses", {
  p <- gf_point(later_anxiety ~ condition, data = er) %>%
    gf_model(lm(later_anxiety ~ condition, data = er))
  d <- ggplot2::ggplot_build(p)$data[[layer_index(p, "model")]]

  expect_equal(unique(d$colour), ggplot2::GeomLine$default_aes$colour)
})

test_that("a predictor with a missing value still gets a prediction grid", {
  # lm() drops Fingers' 29 rows with a missing SSLast, but the plot keeps them,
  # so range() over the plot's own column returns NA and seq() aborts on it
  p <- gf_point(Thumb ~ SSLast, data = Fingers)
  plan <- plan_for(p, lm(Thumb ~ SSLast, data = Fingers))

  expect_true(all(is.finite(plan$grid$SSLast)))
  expect_equal(range(plan$grid$SSLast), range(Fingers$SSLast, na.rm = TRUE))
})

# Terms the plot and the model spell differently ------------------------------------------

test_that("a plot of the raw column takes a model of a transformation of it", {
  p <- gf_point(later_anxiety ~ age, data = er)
  model <- lm(later_anxiety ~ log(age), data = er)
  plan <- plan_for(p, model)

  expect_equal(plan$kind, "line")
  expect_equal(names(plan$grid), c("age", "later_anxiety"))
  expect_equal(range(plan$grid$age), range(er$age))
  expect_equal(nrow(plan$grid), max(nrow(er), 80L))
  expect_equal(
    plan$grid$later_anxiety,
    unname(predict(model, newdata = data.frame(age = plan$grid$age)))
  )
})

test_that("a plot of the transformation takes the model written the same way", {
  p <- gf_point(later_anxiety ~ log(age), data = er)
  plan <- plan_for(p, lm(later_anxiety ~ log(age), data = er))

  expect_equal(plan$kind, "line")
  expect_equal(names(plan$grid), c("age", "later_anxiety"))
  expect_equal(rlang::f_rhs(plan$args$x), quote(log(age)))
})

test_that("a transformed axis is drawn at the transformed positions", {
  model <- lm(later_anxiety ~ log(age), data = er)
  p <- gf_point(later_anxiety ~ log(age), data = er) %>% gf_model(model)
  d <- ggplot2::ggplot_build(p)$data[[layer_index(p, "model")]]

  grid_age <- seq(min(er$age), max(er$age), length.out = nrow(er))
  expect_equal(d$x, log(grid_age))
  expect_equal(d$y, unname(predict(model, newdata = data.frame(age = grid_age))))
})

test_that("a term the plot cannot supply is still refused, named by its column", {
  p <- gf_point(later_anxiety ~ age, data = er)
  expect_error(
    plan_for(p, lm(later_anxiety ~ log(base_total), data = er)),
    "missing in plot: base_total"
  )
})

test_that("a model whose own outcome is transformed is refused, not drawn wrong", {
  p <- gf_point(later_anxiety ~ age, data = er)

  expect_error(
    plan_for(p, lm(sqrt(later_anxiety) ~ age, data = er)),
    "outcome is a variable in the data"
  )
  expect_error(
    plan_for(p, lm(sqrt(later_anxiety) ~ age, data = er)),
    "model outcome: sqrt\\(later_anxiety\\)"
  )
  expect_error(
    plan_for(p, sqrt(later_anxiety) ~ age),
    "outcome is a variable in the data"
  )
})

test_that("a plot that transforms the outcome's axis draws the prediction there", {
  model <- lm(later_anxiety ~ age, data = er)
  p <- gf_point(sqrt(later_anxiety) ~ age, data = er) %>% gf_model(model)
  d <- ggplot2::ggplot_build(p)$data[[layer_index(p, "model")]]

  expect_equal(d$y, sqrt(unname(predict(model, newdata = data.frame(age = d$x)))))
})

test_that("an intercept lands where the plot's own mapping would put it", {
  model <- lm(later_anxiety ~ NULL, data = er)
  p <- gf_point(sqrt(later_anxiety) ~ age, data = er) %>% gf_model(model)
  d <- ggplot2::ggplot_build(p)$data[[layer_index(p, "model")]]

  expect_equal(unique(d$yintercept), sqrt(mean(er$later_anxiety)))

  # flipped: the outcome is on x, so it is xintercept, not yintercept, that
  # has to spell the plot's own mapping out
  flipped_p <- gf_point(base_anxiety ~ sqrt(later_anxiety), data = er) %>% gf_model(model)
  flipped_d <- ggplot2::ggplot_build(flipped_p)$data[[layer_index(flipped_p, "model")]]

  expect_equal(unique(flipped_d$xintercept), sqrt(mean(er$later_anxiety)))
})

test_that("a transformed categorical axis still draws a group mark at each level", {
  p <- gf_point(later_anxiety ~ factor(condition), data = er)
  model <- lm(later_anxiety ~ condition, data = er)
  plan <- plan_for(p, model)

  expect_equal(plan$kind, "segment")
  expect_equal(rlang::f_rhs(plan$args$x), quote(factor(condition)))

  built <- gf_point(later_anxiety ~ factor(condition), data = er) %>% gf_model(model)
  d <- ggplot2::ggplot_build(built)$data[[layer_index(built, "model")]]
  expected <- tapply(er$later_anxiety, er$condition, mean)

  expect_equal((d$x + d$xend) / 2, seq_len(nlevels(factor(er$condition))))
  expect_equal(d$y, as.vector(expected))
})

test_that("a transformed predictor on the axis is not mistaken for a second grouping", {
  p <- gf_point(later_anxiety ~ age, color = ~condition, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ log(age) + condition, data = er))

  expect_equal(plan$kind, "line")
  expect_equal(rlang::f_rhs(plan$args$group), quote(condition))
})

test_that("a plot aesthetic written as an expression still reaches the model layer", {
  p <- gf_point(later_anxiety ~ age, color = ~factor(condition), data = er)
  plan <- plan_for(p, lm(later_anxiety ~ age + condition, data = er))

  expect_equal(rlang::f_rhs(plan$args$colour), quote(factor(condition)))
  expect_true("condition" %in% names(plan$grid))
})

test_that("an aesthetic and the model term it belongs to may be written differently", {
  p <- gf_point(later_anxiety ~ age, color = ~condition, data = er)

  plan <- plan_for(
    p, lm(later_anxiety ~ age + factor(condition), data = er),
    linetype = ~condition
  )
  expect_equal(rlang::f_rhs(plan$args$linetype), quote(condition))

  expect_no_error(
    plan_for(p, lm(later_anxiety ~ age + condition, data = er), linetype = ~ factor(condition))
  )
})
