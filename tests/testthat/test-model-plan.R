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

test_that("a categorical predictor on the axis draws errorbar hashes at the means", {
  p <- gf_point(later_anxiety ~ condition, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ condition, data = er))

  expect_equal(plan$kind, "errorbar")
  expect_equal(plan$args$width, .4)
  expect_equal(nrow(plan$grid), nlevels(factor(er$condition)))
  expect_equal(
    sort(plan$grid$later_anxiety),
    sort(unname(as.vector(tapply(er$later_anxiety, er$condition, mean)))),
    tolerance = 1e-8
  )
})

test_that("a categorical predictor on y draws the errorbar flipped, on xmin/xmax", {
  p <- gf_boxplot(condition ~ later_anxiety, data = er)
  plan <- plan_for(p, lm(later_anxiety ~ condition, data = er))

  expect_equal(plan$kind, "errorbar")
  expect_true(all(c("xmin", "xmax") %in% names(plan$args)))
  expect_false(any(c("ymin", "ymax") %in% names(plan$args)))
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

  expect_equal(plan$kind, "errorbar")
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

test_that("the errorbar hash keeps its default width unless the user sets one", {
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

test_that("it names the supported base plots when the plot itself maps no axis", {
  p <- ggplot2::ggplot(er) + ggplot2::geom_hex(ggplot2::aes(base_anxiety, later_anxiety))
  expect_error(
    plan_for(p, lm(later_anxiety ~ base_anxiety, data = er)),
    "gf_model\\(\\) supports"
  )
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
      kind = "errorbar"
    ),
    "gf_boxplot, categorical predictor" = list(
      plot = function() gf_boxplot(later_anxiety ~ condition, data = er),
      model = function() lm(later_anxiety ~ condition, data = er),
      kind = "errorbar"
    ),
    "gf_violin, categorical predictor" = list(
      plot = function() gf_violin(later_anxiety ~ condition, data = er),
      model = function() lm(later_anxiety ~ condition, data = er),
      kind = "errorbar"
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
