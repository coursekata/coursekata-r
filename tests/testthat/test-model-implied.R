test_that("a numeric predictor on the axis implies a regression line", {
  # MUTATION: outcome/predictor swapped when both axes are numeric
  m <- implied_model(gf_point(Thumb ~ Height, data = Fingers))

  expect_equal(m$kind, "line")
  expect_equal(m$outcome$column, "Thumb")
  expect_equal(m$predictor$column, "Height")
  expect_false(m$flipped)
  expect_equal(deparse(m$formula), "Thumb ~ Height")
})

test_that("a categorical predictor implies a group mark, whichever axis it is on", {
  # MUTATION: reading orientation off the aesthetic name rather than off which
  # axis is numeric
  m1 <- implied_model(gf_jitter(Thumb ~ Sex, data = Fingers, width = .1))
  expect_equal(m1$kind, "segment")
  expect_false(m1$flipped)

  m2 <- implied_model(gf_boxplot(Sex ~ Thumb, data = Fingers))
  expect_equal(m2$kind, "segment")
  expect_true(m2$flipped)
  expect_equal(m2$outcome$column, "Thumb")
})

test_that("one axis implies an intercept named for the geom that draws it", {
  # MUTATION: swapping vline and hline between the flipped and unflipped
  # one-axis cases
  m_x <- implied_model(gf_histogram(~Thumb, data = Fingers, binwidth = 5))
  expect_equal(m_x$kind, "vline")
  expect_null(m_x$predictor)

  p_y <- ggplot2::ggplot(Fingers, ggplot2::aes(y = Thumb)) + ggplot2::geom_point()
  m_y <- implied_model(p_y)
  expect_equal(m_y$kind, "hline")
  expect_null(m_y$predictor)
})

test_that("an after_stat() aesthetic is not a value", {
  # MUTATION: dropping the after_stat() guard, which refuses with a
  # numeric-outcome message about stats::density instead of treating the
  # aesthetic as unmapped
  m <- implied_model(gf_density(~Thumb, data = Fingers))

  expect_equal(m$kind, "vline")
  expect_null(m$predictor)
})

test_that("the decision is made on the values the plot draws", {
  # MUTATION: returning the original quosure so a downstream fit re-shuffles
  set.seed(1)
  p <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers, width = .1)

  m <- implied_model(p)

  expect_equal(m$outcome$column, ".coursekata_pin_y")
  expect_equal(m$outcome$label, "shuffle(Thumb)")
  drawn_outcome <- rlang::eval_tidy(rlang::sym(m$outcome$column), m$data)
  built_y <- ggplot2::ggplot_build(m$plot)$data[[1]]$y
  expect_gt(cor(drawn_outcome, built_y), 0.99)
})

test_that("a non-symbol x is read as the column it was pinned to", {
  # MUTATION: the kind rule reading a pinned factor column as numeric, or the
  # pin treating a non-symbol x as unpinnable
  m <- implied_model(gf_point(Thumb ~ factor(Year), data = Fingers))

  expect_equal(m$kind, "segment")
  expect_true(is.factor(rlang::eval_tidy(rlang::sym(m$predictor$column), m$data)))
})

test_that("a plot that maps neither axis is refused", {
  # MUTATION: inventing new refusal wording instead of reusing the one that
  # already exists in model_plan()
  p <- ggplot2::ggplot(Fingers) + ggplot2::geom_blank()

  expect_error(implied_model(p), "gf_model\\(\\) supports plots built with")
})

test_that("a plot with two discrete axes is refused by outcome type", {
  # MUTATION: inventing new refusal wording instead of reusing
  # check_numeric_outcome()'s existing message
  p <- gf_point(Sex ~ Year, data = Fingers)

  expect_error(implied_model(p), "numeric outcome variables")
})

test_that("an unreachable second drawer is reported, not refused here", {
  # MUTATION: moving the refusal into the shared decision, where it would
  # fire for both gf_model() and gf_b() with one wording and no caller name
  g <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, shuffle(Thumb))) +
    ggplot2::geom_point() +
    ggplot2::geom_point(ggplot2::aes(y = shuffle(Height)))

  m <- implied_model(g)

  expect_equal(m$unreached, "y")
})

test_that("a plot's facet variables travel with the decision", {
  # MUTATION: dropping the facets field that gf_b()'s faceted refusal is built on
  m <- implied_model(gf_point(Thumb ~ Height | Sex, data = Fingers))

  expect_equal(m$facets, "Sex")
})
