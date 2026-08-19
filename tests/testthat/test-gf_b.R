mark <- function(p, tag) p$layers[[layer_index(p, tag)]]

test_that("a continuous model's b0 dot and rise arrow read the model, not the plot", {
  # MUTATION: an off-by-one in coefs, a rise measured over the wrong run, or a
  # slope read from the plot instead of the model
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers, alpha = .3) %>% gf_b(model, run = 5)

  b0 <- mark(p, "b0")
  expect_equal(b0$data$y, coef(model)[[1]])

  b1 <- mark(p, "b1")
  expect_equal(b1$data$yend - b1$data$y, coef(model)[[2]] * 5)
})

test_that("the rise and run labels follow the run == 1 rule, not an eyeballed default", {
  # MUTATION: the label rule -- the notebooks only checked this by eye
  model <- lm(Thumb ~ Height, data = Fingers)

  p10 <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_b(model, run = 10)
  expect_equal(mark(p10, "run_label")$data$label, "10")
  expect_equal(mark(p10, "b1_label")$data$label, "10 %*% b[1]")

  p1 <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_b(model, run = 1)
  expect_equal(mark(p1, "b1_label")$data$label, "b[1]")
})

test_that("nice_run() picks the power of ten inside the [5%, 80%] window, not any rounding", {
  # MUTATION: the [5%, 80%] window collapsing to "round to a power of ten",
  # which would also pick 0.1 for a span of 3 (target 0.3, but 0.1 < 5% of 3)
  expect_equal(nice_run(1), 0.1)
  expect_equal(nice_run(10), 1)
  expect_equal(nice_run(137), 10)
  expect_equal(nice_run(3), 1)
  expect_equal(nice_run(0.004), 0.001)
})

test_that("the run triangle never crowds the b0 dot, in every notebook scenario", {
  # MUTATION: run_x() picking the candidate NEAREST x = 0 (which.min) instead
  # of farthest from it (which.max) -- the two agree whenever every candidate
  # is positive, so a fixture confined to positive x cannot tell them apart;
  # `at()` pins the exact chosen fraction, and the negative-x and
  # fallback-too-wide scenarios below are where the two rules diverge.
  at <- function(rng, frac) rng[[1]] + frac * diff(rng)

  scenario <- function(data, model) {
    p <- gf_point(y ~ x, data = data) %>% gf_b(model)
    run <- mark(p, "run")$data
    b0 <- mark(p, "b0")$data
    x_range <- range(data$x)
    list(
      inside = run$x >= x_range[[1]] && run$xend <= x_range[[2]],
      b0_at_zero = b0$x == 0,
      run_x = run$x,
      run_xend = run$xend,
      x_range = x_range
    )
  }

  set.seed(11)
  inside_data <- data.frame(x = runif(50, 0, 20))
  inside_data$y <- 5 + 2 * inside_data$x + rnorm(50)
  r1 <- scenario(inside_data, lm(y ~ x, data = inside_data))
  expect_true(r1$inside)
  expect_true(r1$b0_at_zero)
  expect_equal(r1$run_x, at(r1$x_range, 0.60))

  set.seed(12)
  far_below <- data.frame(x = runif(50, 60, 75))
  far_below$y <- -50 + far_below$x + rnorm(50)
  r2 <- scenario(far_below, lm(y ~ x, data = far_below))
  expect_true(r2$inside)
  expect_true(r2$b0_at_zero)
  expect_equal(r2$run_x, at(r2$x_range, 0.60))

  set.seed(13)
  far_above <- data.frame(x = runif(50, 0, 30))
  far_above$y <- 100 - 2 * far_above$x + rnorm(50)
  r3 <- scenario(far_above, lm(y ~ x, data = far_above))
  expect_true(r3$inside)
  expect_true(r3$b0_at_zero)
  expect_equal(r3$run_x, at(r3$x_range, 0.60))

  # data left of x = 0 -- the one shape where "farthest from x = 0" and
  # "larger x" disagree, i.e. the crowding the rule exists to prevent
  set.seed(7)
  neg <- data.frame(x = runif(60, -120, 30))
  neg$y <- 3 + 2 * neg$x + rnorm(60)
  r4 <- scenario(neg, lm(y ~ x, data = neg))
  expect_true(r4$inside)
  expect_equal(r4$run_x, at(r4$x_range, 0.15))
  expect_gt(abs(r4$run_x), abs(at(r4$x_range, 0.60)))

  # run too large for the 60% candidate to fit, which pins the documented
  # fallback to the 15% position
  set.seed(9)
  wide <- data.frame(x = c(0, 22, runif(48, 0, 22)))
  wide$y <- 1 + wide$x + rnorm(50)
  r5 <- scenario(wide, lm(y ~ x, data = wide))
  expect_true(r5$inside)
  expect_equal(r5$run_x, at(r5$x_range, 0.15))
  expect_lte(r5$run_xend, r5$x_range[[2]])
})

test_that("show_b0 controls the axis expansion and the mark together, not just the label", {
  # MUTATION: show_b0 controlling only the label, leaving the axis or the b0
  # mark itself unaffected
  model <- lm(Thumb ~ Height, data = Fingers)

  p_on <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_b(model)
  x_range_on <- ggplot2::ggplot_build(p_on)$layout$panel_params[[1]]$x.range
  expect_true(x_range_on[[1]] <= 0)

  p_off <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_b(model, show_b0 = FALSE)
  x_range_off <- ggplot2::ggplot_build(p_off)$layout$panel_params[[1]]$x.range
  expect_false(x_range_off[[1]] <= 0)
  expect_true(is.na(layer_index(p_off, "b0")))
})

test_that("the empty model's b0 label sits at the panel edge, not a level coordinate", {
  # MUTATION: b_empty_marks() delegating wholesale to b_ref_line(), which
  # places the label at predictor coordinate `1 - label_nudge` -- a level-unit
  # constant that means nothing on the empty model's count axis
  empty <- lm(Thumb ~ NULL, data = Fingers)

  # outcome on x: the b0 line is vertical, so the label's predictor
  # coordinate (y, the count axis) must be infinite, not a level position
  p_vertical <- gf_histogram(~Thumb, data = Fingers) %>% gf_b(empty)
  expect_true(is.infinite(mark(p_vertical, "b0_label")$data$y))

  # outcome on y: the b0 line is horizontal, so the label's predictor
  # coordinate (x) must be infinite, and -- because an infinite coordinate is
  # dropped from scale training -- adding it must not move the x axis at all
  p_horizontal <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_b(empty)
  expect_true(is.infinite(mark(p_horizontal, "b0_label")$data$x))
  plain <- gf_point(Thumb ~ Height, data = Fingers)
  expect_equal(
    ggplot2::ggplot_build(p_horizontal)$layout$panel_params[[1]]$x.range,
    ggplot2::ggplot_build(plain)$layout$panel_params[[1]]$x.range
  )
})

test_that("a categorical model's arrows read coef(), not group means", {
  # MUTATION: drawing arrows to group means instead of to b0 + b_k.
  #
  # This fixture CANNOT tell those two apart, and saying so is the point. Under
  # treatment coding `b0 + b_k` IS group k's mean, exactly, so both the correct
  # code and the mutation produce identical numbers here. What this test pins
  # is that the arrows report the coefficients the model was fit with -- which
  # is worth pinning on its own, and is all it claims.
  #
  # The coding that would separate the two is `contr.sum`, and it is refused
  # outright rather than drawn; the test below is where that lives.
  set.seed(21)
  df <- data.frame(y = rnorm(60), g = factor(rep(c("a", "b", "c"), each = 20)))
  model <- lm(y ~ g, data = df)
  p <- gf_jitter(y ~ g, data = df, width = .1) %>% gf_b(model)

  expect_false(is.na(layer_index(p, "b0")))
  for (j in 2:3) {
    arrow <- mark(p, paste0("bk_", j))$data
    expect_equal(arrow$yend - arrow$y, coef(model)[[j]])
  }
})

test_that("arrow position and label follow coef() order, not sort(levels())", {
  # MUTATION: level order read from sort(levels()) rather than from the
  # factor -- the reordered levels below would then draw b[1] on group
  # "control" instead of "treatment"
  df <- Fingers
  df$Group <- factor(df$Sex, levels = c("male", "female"))
  levels(df$Group) <- c("treatment", "control")
  model <- lm(Thumb ~ Group, data = df)
  p <- gf_jitter(Thumb ~ Group, data = df, width = .1) %>% gf_b(model)

  arrow <- mark(p, "bk_2")$data
  expect_equal(arrow$x, 2 - 0.18)
  expect_equal(mark(p, "b0_label")$data$label, "b[0]")
  expect_equal(mark(p, "bk_2_label")$data$label, "b[1]")
})

test_that("an inferred gf_b() reads the same decision gf_model()'s inference reads", {
  # MUTATION: the outcome/predictor/kind decision drifting between gf_model()
  # and gf_b() -- a second copy of the inference living in R/gf_b.R. gf_b()
  # fits its own lm() by design; what must not drift is which variables and
  # which shape.
  set.seed(31)
  q <- gf_jitter(shuffle(Height) ~ Sex, data = Fingers, width = .1) %>% gf_b()

  expect_true(!is.null(attr(q, "coursekata_pins")))
  im <- implied_model(q)
  expect_equal(im$kind, "segment")
  expect_equal(im$outcome$column, ".coursekata_pin_y")
  expect_equal(im$predictor$column, "Sex")

  reference <- levels(Fingers$Sex)[[1]]
  expected <- mean(q$data$.coursekata_pin_y[Fingers$Sex == reference])
  expect_equal(mark(q, "b0")$data$y, expected)
})

test_that("an inferred gf_model() and an inferred gf_b() agree about the same plot", {
  # MUTATION: the two features disagreeing about the same implied model --
  # previously checked only by eye ("segments and arrows should align")
  set.seed(32)
  p <- gf_jitter(shuffle(Height) ~ Sex, data = Fingers, width = .1) %>%
    gf_model() %>%
    gf_b()

  model_layer <- ggplot2::ggplot_build(p)$data[[layer_index(p, "model")]]
  expect_equal(mark(p, "b0")$data$y, model_layer$y[[1]])
})

test_that("no model on a faceted plot is refused, not silently drawn over per-panel lines", {
  # MUTATION: silently drawing whole-data arrows over gf_model()'s per-panel lines
  p <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_facet_wrap(~RaceEthnic)
  expect_error(gf_b(p), "faceted plot")
})

test_that("a model with more than one predictor is refused", {
  # MUTATION: a coefs[[2]] that quietly annotates the first slope of a
  # multiple regression instead of refusing
  model <- lm(Thumb ~ Height + Weight, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)
  expect_error(gf_b(p, model), "one predictor")
})

test_that("run on a categorical model warns and still draws the right plot", {
  # MUTATION: the warning becoming an abort, or vanishing entirely
  model <- lm(Tip ~ Condition, data = TipExperiment)
  p <- gf_jitter(Tip ~ Condition, data = TipExperiment, width = .1)

  expect_warning(result <- gf_b(p, model, run = 10), "categorical")
  expect_s3_class(result, "gg")
  expect_false(is.na(layer_index(result, "bk_2")))
})

test_that("gf_b and gf_coef are the same factory call with only the name changed", {
  # MUTATION: an edit made to one and not the other -- the alias is
  # generated, not forwarded, precisely so this can be asserted
  b_env <- environment(gf_b)
  coef_env <- environment(gf_coef)
  rename <- function(x) {
    gsub("gf_coef", "gf_b", paste(deparse(x), collapse = "\n"), fixed = TRUE)
  }

  expect_setequal(ls(coef_env), ls(b_env))
  for (stored in setdiff(ls(b_env), "res")) {
    expect_identical(rename(get(stored, coef_env)), rename(get(stored, b_env)), info = stored)
  }
  expect_identical(formals(gf_coef), formals(gf_b))
  expect_identical(body(gf_coef), body(gf_b))
})

test_that("gf_b and gf_coef draw identical plots", {
  # MUTATION: alias drift that only shows up in the built output, not in the
  # stored factory arguments
  model <- lm(Thumb ~ Height, data = Fingers)
  strip <- function(fn) {
    p <- fn(gf_point(Thumb ~ Height, data = Fingers), model)
    lapply(p$layers, function(l) {
      list(
        geom = class(l$geom)[[1]], data = l$data,
        params = l$aes_params, tag = attr(l, "coursekata_layer")
      )
    })
  }
  expect_equal(strip(gf_b), strip(gf_coef))
})

test_that("a bare call prints help naming the function the caller wrote", {
  # MUTATION: the generated-not-forwarded decision reverted to a forwarder,
  # which would report the wrong name here
  # ggformula's bare-call help is emitted with message(), not cat()
  expect_message(gf_b(), "gf_b")
  expect_message(gf_coef(), "gf_coef")
})

test_that("a formula where a fitted model belongs is refused, not silently fit", {
  # MUTATION: gf_b() quietly fitting a formula itself, which would fit a
  # different model than the one the caller's own lm() call intended (and
  # duplicate gf_model()'s fitting path)
  p <- gf_point(Thumb ~ Height, data = Fingers)
  expect_error(gf_b(p, Thumb ~ Height), "not a fit")
})

test_that("an unmappable plot is refused in the caller's own name", {
  # MUTATION: check_model_axes()'s shared refusal called positionally, which
  # binds the caller env to `fn` instead of the intended function name -- and
  # separately, hardcoding `gf_model()` in the message regardless of which of
  # gf_b()/gf_coef() reached it through implied_model()
  p <- ggplot2::ggplot(Fingers)
  expect_error(gf_b(p), "gf_b\\(\\) supports plots built with")
  expect_error(gf_coef(p), "gf_coef\\(\\) supports plots built with")
})

test_that("a formula for a mark's appearance is refused, not silently dropped", {
  # MUTATION: `color`/`label_color`/the other extras staying declared as
  # layer_factory() `extras`, so `color = ~species` binds as a literal
  # formula and reaches ggplot2::layer(params = ) -- an internal vctrs error
  # at render, not a mapping and not a refusal
  p <- gf_point(Thumb ~ Height, data = Fingers)
  model <- lm(Thumb ~ Height, data = Fingers)
  expect_error(gf_b(p, model, color = ~Sex), "not a mapping")
  expect_error(gf_coef(p, model, label_size = ~Sex), "not a mapping")
})

test_that("an extra no mark can honor is warned about, not silently dropped", {
  # MUTATION: gf_b_layer_fun() discarding ggformula's params/mapping/... with
  # nothing telling the caller `alpha`/`linetype`/`show.legend` never reached
  # any mark
  p <- gf_point(Thumb ~ Height, data = Fingers)
  model <- lm(Thumb ~ Height, data = Fingers)

  expect_warning(gf_b(p, model, alpha = .2), "cannot reach them")
  expect_warning(gf_coef(p, model, linetype = 2), "cannot reach them")
  expect_warning(gf_b(p, model, show.legend = TRUE), "cannot reach them")
  expect_no_warning(gf_b(p, model, color = "red"))
})

test_that("colour and label_colour are accepted alongside color and label_color", {
  # MUTATION: reading only the American spelling out of `...`, which silently
  # drops `colour =`/`label_colour =` and changes nothing about the marks
  p <- gf_point(Thumb ~ Height, data = Fingers)
  model <- lm(Thumb ~ Height, data = Fingers)

  b <- gf_b(p, model, colour = "red", label_colour = "blue")
  expect_equal(mark(b, "b0")$aes_params$colour, "red")
  expect_equal(mark(b, "b0_label")$aes_params$colour, "blue")

  coef_plot <- gf_coef(p, model, colour = "red", label_colour = "blue")
  expect_equal(mark(coef_plot, "b0")$aes_params$colour, "red")
  expect_equal(mark(coef_plot, "b0_label")$aes_params$colour, "blue")
})

test_that("a model whose predictor the plot does not draw is refused", {
  # MUTATION: checking only the outcome axis, which `resid_end()` already does.
  # Every mark is placed from the coefficients rather than from the points, so
  # nothing about drawing one notices that its numbers describe some other
  # variable -- the picture that comes out looks entirely reasonable.
  p_race <- gf_jitter(Thumb ~ RaceEthnic, data = Fingers)

  expect_error(gf_b(p_race, lm(Thumb ~ Sex, data = Fingers)), "annotates a model of what")
  expect_error(gf_coef(p_race, lm(Thumb ~ Sex, data = Fingers)), "annotates a model of what")
})

test_that("a predictor is matched as it is spelled, not as the column underneath", {
  # MUTATION: comparing columns rather than labels, which would accept
  # `log(Height)` on a raw `Height` axis. Measured under that mutation: the
  # rise-over-run triangle lands at x = 4.23 on an axis running 59 to 76.5 --
  # off the panel entirely, because b1 is a rise per unit of log height and
  # the axis is measuring inches.
  raw <- gf_point(Thumb ~ Height, data = Fingers)
  logged <- gf_point(Thumb ~ log(Height), data = Fingers)
  log_model <- lm(Thumb ~ log(Height), data = Fingers)

  expect_error(gf_b(raw, log_model), "the plot's x axis draws `Height`")
  expect_no_error(gf_b(logged, log_model))
  expect_no_error(gf_b(raw, lm(Thumb ~ Height, data = Fingers)))
})

test_that("a basis expansion is refused rather than drawn without its extra coefficients", {
  # MUTATION: none needed beyond the above -- this is the case that made the
  # spelling comparison worth stating outright. `poly(Height, 2)` has two
  # coefficients and a triangle has one slope, and no plot has a
  # `poly(Height, 2)` axis to draw either on.
  p <- gf_point(Thumb ~ Height, data = Fingers)

  expect_error(
    gf_b(p, lm(Thumb ~ poly(Height, 2), data = Fingers)),
    "poly\\(Height, 2\\)"
  )
})

test_that("a plot with no predictor axis at all is refused as such", {
  # MUTATION: indexing `spec$axes[[axis]]` without checking it is there, which
  # errors with `subscript out of bounds` instead of saying what is wrong.
  p <- gf_histogram(~Thumb, data = Fingers, binwidth = 5)

  expect_error(gf_b(p, lm(Thumb ~ Height, data = Fingers)), "no y axis to draw it on")
})

test_that("the empty model is drawable over any predictor", {
  # MUTATION: refusing when `predictor` is NULL. The empty model predicts the
  # same number everywhere, so there is no predictor for a plot to disagree
  # with and b0 is a horizontal line wherever it is drawn.
  empty <- lm(Thumb ~ NULL, data = Fingers)

  expect_no_error(gf_point(Thumb ~ Height, data = Fingers) %>% gf_b(empty))
  expect_no_error(gf_jitter(Thumb ~ RaceEthnic, data = Fingers) %>% gf_b(empty))
})

test_that("a coding where coefficient k is not group k's difference is refused", {
  # MUTATION: assuming treatment coding, which is what the placement code does.
  # Coefficient k is drawn at group position k; under `contr.sum` the intercept
  # is a grand mean and each coefficient a deviation, under `contr.helmert` a
  # running comparison, and an ordered factor gets `contr.poly`, whose terms
  # are a linear trend and a quadratic one. Every one of those still draws an
  # arrow, at a group, with a length -- none of them the number the model
  # reports for that group, and nothing on the page says so.
  df <- Fingers
  df$R <- factor(df$RaceEthnic)

  # set on the column, not written into the formula, so the model's term stays
  # a plain `R` and this is not caught by the predictor-spelling check instead
  summed <- df
  contrasts(summed$R) <- contr.sum
  expect_error(
    gf_jitter(Thumb ~ R, data = summed) %>% gf_b(lm(Thumb ~ R, data = summed)),
    "difference from the reference group"
  )

  ordered_levels <- df
  ordered_levels$R <- factor(ordered_levels$R, ordered = TRUE)
  expect_error(
    gf_jitter(Thumb ~ R, data = ordered_levels) %>% gf_b(lm(Thumb ~ R, data = ordered_levels)),
    "contr.poly"
  )
})

test_that("the coding check reaches a model this package fit for itself", {
  # MUTATION: checking inside the explicit branch only. A global
  # `options(contrasts = )` reaches the fit `gf_b()` makes from the plot just
  # as surely as one the reader handed in, and the inferred path draws its
  # marks with the same placement code.
  withr::local_options(contrasts = c("contr.sum", "contr.poly"))
  df <- Fingers
  df$R <- factor(df$RaceEthnic)

  expect_error(gf_jitter(Thumb ~ R, data = df) %>% gf_b(), "difference from the reference group")
})

test_that("changing which group is the reference is still fine", {
  # MUTATION: rejecting every coding, treatment included. The plot orders its
  # groups by the same factor the model coded, so a `relevel()` moves both
  # together and coefficient k still belongs to position k.
  df <- Fingers
  df$R <- relevel(factor(df$RaceEthnic), ref = "Asian")

  expect_no_error(gf_jitter(Thumb ~ R, data = df) %>% gf_b(lm(Thumb ~ R, data = df)))
})

test_that("a model with no intercept is refused rather than dying on its own coefficients", {
  # MUTATION: none -- before this check the call died with `subscript out of
  # bounds`, reaching past the end of a one-element coefficient vector for a
  # slope it assumed was second. Every mark here is measured from b0, and a
  # model fit through the origin does not have one.
  p <- gf_point(Thumb ~ Height, data = Fingers)

  expect_error(gf_b(p, lm(Thumb ~ Height - 1, data = Fingers)), "starting from b0")
  expect_error(gf_coef(p, lm(Thumb ~ Height - 1, data = Fingers)), "starting from b0")
})
