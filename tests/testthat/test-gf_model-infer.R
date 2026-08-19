# gf_model() with no model draws the model the plot implies. Every test names the
# mutation it detects, per the file's own header rule. No cross-file state:
# `built()` is defined locally rather than shared, because testthat runs in
# parallel.

built <- function(p) ggplot2::ggplot_build(p)$data[[layer_index(p, "model")]]

sorted_cols <- function(df, cols) {
  df <- df[cols]
  df <- df[order(df$PANEL, df$x), ]
  rownames(df) <- NULL
  df
}

test_that("a categorical predictor's group marks agree with the named model, over four types", {
  Fingers$sex_chr <- as.character(Fingers$Sex)
  Fingers$sex_lgl <- Fingers$Sex == "female"

  predictors <- list(Sex = Fingers$Sex, Job = Fingers$Job, sex_chr = Fingers$sex_chr,
                      sex_lgl = Fingers$sex_lgl)

  for (name in names(predictors)) {
    inferred <- gf_jitter(as.formula(paste("Thumb ~", name)), data = Fingers, width = .1) %>%
      gf_model()
    named <- gf_jitter(as.formula(paste("Thumb ~", name)), data = Fingers, width = .1) %>%
      gf_model(lm(as.formula(paste("Thumb ~", name)), data = Fingers))

    b_inferred <- built(inferred)
    expect_equal(unname(b_inferred$y), as.numeric(tapply(Fingers$Thumb, predictors[[name]], mean)),
                 info = name)

    # StatSummary also emits ymin/ymax/flipped_aes and its group carries no `n`
    # attribute, so the comparison is restricted to the columns both frames
    # agree on; a logical predictor comes back in opposite row order from
    # StatSummary's x order vs. model_plan()'s level order, so both sides are
    # sorted the same way before comparing
    cols <- c("x", "xend", "y", "yend", "PANEL")
    expect_equal(sorted_cols(b_inferred, cols), sorted_cols(built(named), cols), info = name)
  }
})

test_that("the categorical mark is drawn with GeomModelMark and StatSummary", {
  p <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1) %>% gf_model()
  layer <- p$layers[[layer_index(p, "model")]]

  # a regression to a precomputed grid, or a swap to GeomSegment, would not
  # build the mark's width the way GeomModelMark's setup_data does
  expect_s3_class(layer$geom, "GeomModelMark")
  expect_s3_class(layer$stat, "StatSummary")
})

test_that("a continuous predictor's line is the regression the plot implies", {
  p <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_model()
  fitted <- lm(y ~ x, built(p))
  named <- lm(Thumb ~ Height, data = Fingers)

  # catches fitting the wrong variables, the wrong rows, or drawing a mean
  # line instead of a fit
  expect_equal(unname(coef(fitted)), unname(coef(named)), tolerance = 1e-8)
})

test_that("an inferred line names its own formula instead of ggplot2 guessing one", {
  # MUTATION: leaving `formula` out of the line's params, which lets
  # StatSmooth infer its own default and print "`geom_smooth()` using
  # formula = 'y ~ x'" -- naming a function the caller never called
  expect_silent(ggplot2::ggplot_build(gf_point(Thumb ~ Height, data = Fingers) %>% gf_model()))
})

test_that("gf_lm()'s fitting vocabulary reaches the inferred line through ...", {
  inferred <- gf_point(Thumb ~ Height, data = Fingers) %>%
    gf_model(formula = y ~ poly(x, 2))
  named <- suppressMessages(gf_lm(Thumb ~ Height, data = Fingers, formula = y ~ poly(x, 2)))
  # catches adding a `formula` formal that shadows it, or a layer function that
  # drops params the way model_layer_fun() deliberately does
  expect_equal(built(inferred)$y, ggplot2::ggplot_build(named)$data[[1]]$y)

  inferred_se <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_model(se = TRUE)
  named_ci <- suppressMessages(gf_lm(Thumb ~ Height, data = Fingers, interval = "confidence"))
  expect_equal(built(inferred_se)$ymin, ggplot2::ggplot_build(named_ci)$data[[1]]$ymin)
  expect_equal(built(inferred_se)$ymax, ggplot2::ggplot_build(named_ci)$data[[1]]$ymax)
})

test_that("a plot with only an outcome implies the grand mean, drawn as a vline", {
  p <- gf_histogram(~Thumb, data = Fingers, binwidth = 5) %>% gf_model()
  b <- built(p)

  # the inverted geom table would draw an hline where a vline belongs here --
  # the outcome is on x, so the mark must be a vline
  expect_equal(nrow(b), 1L)
  expect_equal(b$xintercept, mean(Fingers$Thumb))
  expect_false("yintercept" %in% names(b))
})

test_that("a faceted continuous plot fits each panel's own line", {
  p <- gf_point(Thumb ~ Height | Sex, data = Fingers) %>% gf_model()
  b <- built(p)

  # a precomputed whole-data grid replicated per panel would give both panels
  # the same slope
  slopes <- vapply(split(b, b$PANEL), function(sub) unname(coef(lm(y ~ x, sub))[[2]]), numeric(1))
  by_group <- vapply(split(Fingers, Fingers$Sex), function(sub) {
    unname(coef(lm(Thumb ~ Height, sub))[[2]])
  }, numeric(1))
  expect_equal(unname(slopes), unname(by_group), tolerance = 1e-6)
})

test_that("a faceted plot with only an outcome gives each panel its own mean", {
  p <- gf_histogram(~Thumb | Sex, data = Fingers, binwidth = 5) %>% gf_model()
  b <- built(p)

  # compute_group instead of compute_panel in the axis variant would collapse
  # or scatter this differently than one row per panel
  expect_equal(nrow(b), 2L)
  expect_setequal(b$xintercept, unname(tapply(Fingers$Thumb, Fingers$Sex, mean)))
})

test_that("the inferred line is stable across builds of the same plot", {
  # gf_point(), not gf_jitter(): gf_model() deliberately does not pin the
  # jitter (its position stays unseeded), so a jittered y is never stable
  # across builds for a reason unrelated to what this test names
  set.seed(1)
  p <- gf_point(shuffle(Thumb) ~ Height, data = Fingers)
  q <- p %>% gf_model()

  first <- ggplot2::ggplot_build(q)$data[[1]]$y
  second <- ggplot2::ggplot_build(q)$data[[1]]$y
  expect_equal(first, second)
})

test_that("the inferred line is fit from the pinned column, not a fresh shuffle", {
  set.seed(1)
  p <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers)
  q <- p %>% gf_model()

  from_layer <- coef(lm(y ~ x, built(q)))
  from_pin <- coef(lm(q$data$.coursekata_pin_y ~ Fingers$Height))
  # fitting from a fresh evaluation instead of the pinned column would give a
  # slope near zero, not this one
  expect_equal(unname(from_layer), unname(from_pin))
  expect_gt(cor(ggplot2::ggplot_build(q)$data[[1]]$y, q$data$.coursekata_pin_y), 0.99)
})

test_that("a pinned axis keeps the reader's label while its mapping points at the pin", {
  set.seed(1)
  p <- gf_point(shuffle(Thumb) ~ Height, data = Fingers)
  q <- p %>% gf_model()
  spec <- plot_spec(q)

  # getting labels and quosures backwards would silently reintroduce a fresh
  # shuffle with no error anywhere
  expect_equal(q$labels$y, "shuffle(Thumb)")
  expect_equal(spec$axes[["y"]], "shuffle(Thumb)")
  expect_equal(rlang::as_label(spec$mapping$y), ".coursekata_pin_y")
})

test_that("the caller's own plot is untouched by the pin gf_model() records on its copy", {
  set.seed(1)
  p <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers, width = .2)
  q <- p %>% gf_model()

  # the bug this guards against lives in p$layers[[i]], a ggproto environment
  # -- p$data and p$mapping are copy-on-modify and cannot show it, so a test
  # that checked those instead would pass while the caller was still mutated
  expect_false(".coursekata_pin_y" %in% names(p$layers[[1]]$data))
  expect_equal(rlang::as_label(p$layers[[1]]$mapping$y), "shuffle(Thumb)")
  expect_false(isTRUE(all.equal(
    ggplot2::ggplot_build(p)$data[[1]]$y,
    ggplot2::ggplot_build(q)$data[[1]]$y
  )))
})

test_that("a pinned plot's tagged layers keep their tags", {
  # MUTATION: the pin's reach test treating this package's own `gf_model()`
  # line -- which states its own fitted-y mapping, different from the plot's
  # -- as a rival drawer of `y` and refusing to infer at all
  set.seed(1)
  p <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers) %>%
    gf_model(lm(Thumb ~ Height, data = Fingers))
  q <- p %>% gf_model()

  expect_false(is.na(layer_index(q, "model")))
  expect_equal(length(layer_indices(q, "model")), 2L)
})

test_that("a layer restating the plot's own expression is not a rival drawer", {
  # MUTATION: comparing layer and plot mappings as whole quosures (env and
  # all) instead of as expressions, which refuses even an identical mapping
  # rebuilt in its own environment
  p <- gf_point(Thumb ~ log(Height), data = Fingers) %>%
    gf_model(lm(Thumb ~ log(Height), data = Fingers))
  expect_no_error(p %>% gf_model())
})

test_that("a deterministic non-symbol mapping is pinned unconditionally", {
  p <- gf_point(Thumb ~ log(Height), data = Fingers)
  q <- p %>% gf_model()

  expect_true("x" %in% names(plot_pins(q)))
  expect_equal(ggplot2::ggplot_build(q)$data[[1]]$x, ggplot2::ggplot_build(p)$data[[1]]$x)
  expect_equal(q$labels$x, "log(Height)")

  precomputed <- Fingers
  precomputed$log_height <- log(precomputed$Height)
  reference <- gf_point(Thumb ~ log_height, data = precomputed) %>% gf_model()
  # reintroducing a conditional pin would change more than the mapping's
  # spelling; pinning a deterministic expression should change nothing else
  expect_equal(built(q), built(reference))
})

test_that("the inferred model does not draw an interaction nobody named", {
  p <- gf_point(Thumb ~ Height, color = ~Sex, data = Fingers) %>% gf_model()
  b <- built(p)

  # inheriting the plot's aesthetics would draw one line per color instead of
  # one model of the two axes
  expect_equal(length(unique(b$colour)), 1L)
  expect_equal(unique(b$colour), "black")
})

test_that("a categorical outcome is refused in the reader's own words", {
  expect_error(gf_bar(~Sex, data = Fingers) %>% gf_model(), "numeric outcome variables")
})

test_that("a mapping drawn two different ways by two layers cannot be inferred from", {
  p <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, shuffle(Thumb))) +
    ggplot2::geom_point() +
    ggplot2::geom_point(ggplot2::aes(y = shuffle(Height)))

  # the unreachable-pin case has to land at the call, not surface as a build-time
  # error from a stat fit on the wrong thing
  expect_error(p %>% gf_model(), "two different ways")
})

test_that("a ggplot2-assembled plot with plain symbol mappings needs no pin and builds", {
  p <- ggplot2::ggplot(er) + ggplot2::geom_point(ggplot2::aes(base_anxiety, later_anxiety))
  q <- p %>% gf_model()

  # over-refusing this plot would contradict gf_resid(), which already works on it
  expect_equal(
    unname(coef(lm(y ~ x, built(q)))),
    unname(coef(lm(later_anxiety ~ base_anxiety, data = er)))
  )
})

test_that("a bare call prints help instead of inferring a model from nothing", {
  # the `pre` gate must not let implied_model_spec() run without a plot
  expect_message(gf_model(), "model")
})

test_that("a non-symbol categorical predictor draws one mark per level", {
  p <- gf_point(Thumb ~ factor(Year), data = Fingers) %>% gf_model()
  b <- built(p)
  expect_equal(unname(b$y), as.numeric(tapply(Fingers$Thumb, Fingers$Year, mean)))

  ntile3 <- function(x) as.integer(cut(x, quantile(x, c(0, 1 / 3, 2 / 3, 1)), include.lowest = TRUE))
  Fingers$tile <- ntile3(Fingers$Thumb)
  p2 <- gf_jitter(Thumb ~ as.factor(ntile3(Thumb)), data = Fingers, width = .1) %>% gf_model()
  b2 <- built(p2)
  expect_equal(unname(b2$y), as.numeric(tapply(Fingers$Thumb, Fingers$tile, mean)))
})

test_that("two gf_model() calls in one pipe do not re-pin an already-pinned column", {
  set.seed(1)
  q <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers) %>% gf_model() %>% gf_model()

  idx <- layer_indices(q, "model")
  expect_equal(length(idx), 2L)
  built_layers <- lapply(idx, function(i) ggplot2::ggplot_build(q)$data[[i]])
  expect_equal(built_layers[[1]]$y, built_layers[[2]]$y)

  # recording .coursekata_pin_y as the "original" would lose shuffle(Thumb)
  # from the axis title on the second call
  pins <- plot_pins(q)
  expect_equal(names(pins), "y")
  expect_equal(rlang::as_label(pins$y), "shuffle(Thumb)")
})

test_that("after_stat() mappings are read as unmapped, not as the value they compute", {
  p <- gf_density(~Thumb, data = Fingers) %>% gf_model()
  b <- built(p)

  # without the after_stat() guard this refuses with a numeric-outcome message
  # about the density function itself
  expect_equal(nrow(b), 1L)
  expect_equal(b$xintercept, mean(Fingers$Thumb))
})

test_that("an inferred line stops at the data, whatever else widened the axis", {
  # MUTATION: `fullrange = TRUE` as the stated default, or leaving the
  # parameter unstated so nothing records that this is a choice.
  model <- lm(Thumb ~ Height, data = Fingers)
  observed <- range(Fingers$Height)
  base <- gf_point(Thumb ~ Height, data = Fingers)

  expect_equal(range(built(base %>% gf_lims(x = c(40, 100)) %>% gf_model())$x), observed)
  expect_equal(range(built(base %>% gf_b(model) %>% gf_model())$x), observed)
})

test_that("naming the model does not change how far its line is drawn", {
  # MUTATION: one path sizing its grid from the plot's axis while the other
  # sizes it from the data. The two build their grids by completely different
  # routes -- a stat handed the panel's scale as it draws, and a grid computed
  # when the call is made -- so nothing but a test holds them to one answer.
  # It was `gf_b()` that first pulled them apart: it widens the axis, and for a
  # while only one of the two lines stretched to follow.
  model <- lm(Thumb ~ Height, data = Fingers)
  base <- gf_point(Thumb ~ Height, data = Fingers)
  widenings <- list(
    plain = function(p) p,
    b = function(p) gf_b(p, model),
    lims = function(p) gf_lims(p, x = c(40, 100))
  )

  for (widen in widenings) {
    p <- widen(base)
    expect_equal(range(built(p %>% gf_model())$x), range(built(p %>% gf_model(model))$x))
  }
})

test_that("`se = TRUE` draws the band, not just the numbers for one", {
  # MUTATION: GeomLine instead of GeomSmooth. StatSmooth computes `ymin` and
  # `ymax` either way, so a test that checks the computed columns passes under
  # the mutation and the reader still sees a bare line. The rendered grob is
  # the only place the difference exists: a ribbon and a polyline, which is
  # what `gf_lm(interval = "confidence")` draws for the same request.
  p <- gf_point(Thumb ~ Height, data = Fingers)

  banded <- ggplot2::layer_grob(p %>% gf_model(se = TRUE), 2)[[1]]
  reference <- ggplot2::layer_grob(p %>% gf_lm(interval = "confidence"), 2)[[1]]

  expect_length(banded$children, length(reference$children))
  expect_gt(length(banded$children), 1)
})

test_that("the ordinary line keeps the family's own color, not a smoother's", {
  # MUTATION: dropping the stated `colour` after the geom swap. GeomSmooth's
  # default is a blue that means "a smoother" in ggplot2's vocabulary; this
  # line means "the model", and every other mark in this family draws it in
  # GeomLine's neutral.
  p <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_model()

  expect_equal(unique(built(p)$colour), ggplot2::GeomLine$default_aes$colour)
})

test_that("a caller's own color wins over the stated default, either spelling", {
  # MUTATION: dropping the color/colour normalization in `implied_layer_fun()`.
  # `modifyList()` matches names literally, so a default stated as `colour`
  # outranks the caller's `color =` and the line silently draws black. The
  # `segment` shape has stated `colour` for longer than the `line` shape has,
  # and was worse off: measured before the normalization, `color = "firebrick"`
  # there warned "Duplicated aesthetics after name standardisation: colour".
  line <- gf_point(Thumb ~ Height, data = Fingers)
  segment <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1)

  expect_equal(unique(built(line %>% gf_model(color = "firebrick"))$colour), "firebrick")
  expect_equal(unique(built(line %>% gf_model(colour = "darkgreen"))$colour), "darkgreen")
  expect_no_warning(marked <- built(segment %>% gf_model(color = "firebrick")))
  expect_equal(unique(marked$colour), "firebrick")
})
