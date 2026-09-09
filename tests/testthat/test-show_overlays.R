shuffles <- function(n) {
  set.seed(42)
  data.frame(b1 = replicate(n, {
    shuffled <- base::sample(TipExperiment$Tip)
    b1(lm(shuffled ~ Condition, data = TipExperiment))
  }))
}

# the flagship figure: ten shuffles with the count axis held at 10, so two of
# these plots can be read side by side
framed <- function() {
  gf_histogram(~b1, data = shuffles(10), binwidth = 2) +
    ggplot2::expand_limits(y = 10)
}

tagged <- function(p, tag) {
  i <- layer_index(p, tag)
  if (is.na(i)) NULL else ggplot2::ggplot_build(p)$data[[i]]
}
count_top <- function(p) {
  ggplot2::ggplot_build(p)$layout$panel_scales_y[[1]]$get_limits()[[2]]
}
# the top every panel is drawn against, in panel order: a free scale gives each
# panel its own, and facet_grid() gives a whole row of panels one shared scale
panel_tops <- function(p) {
  b <- ggplot2::ggplot_build(p)
  vapply(
    b$layout$layout$SCALE_Y,
    function(i) b$layout$panel_scales_y[[i]]$get_limits()[[2]],
    numeric(1)
  )
}
panel_top <- function(p) {
  ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range[[2]]
}

test_that("the mean line marks the mean of what is on the x axis", {
  p <- show_mean(gf_histogram(~Thumb, data = Fingers, binwidth = 5))
  expect_equal(tagged(p, "distribution_mean")$xintercept, mean(Fingers$Thumb))

  # the axis holds log(Thumb), so the mean drawn is the mean of log(Thumb)
  q <- show_mean(gf_histogram(~ log(Thumb), data = Fingers, bins = 20))
  expect_equal(tagged(q, "distribution_mean")$xintercept, mean(log(Fingers$Thumb)))
})

test_that("the mean line is an intercept, so it spans whatever panel it lands in", {
  bare <- framed()
  p <- show_mean(bare)

  expect_s3_class(p$layers[[layer_index(p, "distribution_mean")]]$geom, "GeomVline")
  seg <- tagged(p, "distribution_mean")
  expect_null(seg$yend)
  # and drawing it must not retrain the axis it spans
  expect_equal(panel_top(p), panel_top(bare))
})

test_that("the mean line reads a countable histogram the same way", {
  p <- suppressMessages(gf_squareplot(~b1, data = shuffles(10), binwidth = 2))
  seg <- tagged(show_mean(p), "distribution_mean")
  expect_equal(seg$xintercept, mean(shuffles(10)$b1))
})

test_that("missing values are dropped rather than drawn at NA", {
  d <- data.frame(v = c(1, 2, 3, NA))
  p <- suppressWarnings(show_mean(gf_histogram(~v, data = d, binwidth = 1)))
  expect_equal(suppressWarnings(tagged(p, "distribution_mean"))$xintercept, 2)
})

test_that("each facet gets its own mean, because a facet is its own subset", {
  set.seed(11)
  d <- data.frame(v = c(rnorm(40, 0, 3), rnorm(40, 8, 3)), g = rep(c("a", "b"), each = 40))
  seg <- tagged(show_mean(gf_histogram(~ v | g, data = d, binwidth = 1)), "distribution_mean")
  expect_equal(nrow(seg), 2)
  expect_equal(sort(seg$xintercept), as.numeric(sort(tapply(d$v, d$g, mean))))
})

test_that("a facet given as an expression is evaluated, not used to index the data", {
  set.seed(11)
  d <- data.frame(v = c(rnorm(40, 0, 3), rnorm(40, 8, 3)), g = rep(c(1, 2), each = 40))
  p <- gf_histogram(~v, data = d, binwidth = 1) + ggplot2::facet_wrap(~ factor(g))
  seg <- tagged(show_mean(p), "distribution_mean")
  expect_equal(nrow(seg), 2)
  expect_equal(sort(seg$xintercept), as.numeric(sort(tapply(d$v, d$g, mean))))
})

test_that("a facet expression that is not one-to-one on its raw variable still gets one mean per panel", {
  # cut(v, 2) and v > 0 each partition on a *computed* value that many raw
  # rows share. cut()'s breaks depend on the whole column, so re-evaluating
  # it against a representative row per group (rather than reading the panel
  # ggplot2 already assigned) invents new panels instead of finding the real
  # two -- assert on the figure's own panel count, not just the aggregation,
  # since that is exactly what a wrong grouping cannot see
  set.seed(11)
  d <- data.frame(v = c(rnorm(40, 0, 3), rnorm(40, 8, 3)))

  above_zero_plot <- gf_histogram(~v, data = d, binwidth = 1) + ggplot2::facet_wrap(~ v > 0)
  above_zero_drawn <- show_mean(above_zero_plot)
  above_zero <- tagged(above_zero_drawn, "distribution_mean")
  expect_equal(
    nrow(ggplot2::ggplot_build(above_zero_drawn)$layout$layout),
    nrow(ggplot2::ggplot_build(above_zero_plot)$layout$layout)
  )
  expect_equal(sort(as.integer(as.character(above_zero$PANEL))), 1:2)
  expect_equal(nrow(above_zero), 2)
  expect_equal(sort(above_zero$xintercept), as.numeric(sort(tapply(d$v, d$v > 0, mean))))

  cut_two_plot <- gf_histogram(~v, data = d, binwidth = 1) + ggplot2::facet_wrap(~ cut(v, 2))
  cut_two_drawn <- show_mean(cut_two_plot)
  cut_two <- tagged(cut_two_drawn, "distribution_mean")
  expect_equal(
    nrow(ggplot2::ggplot_build(cut_two_drawn)$layout$layout),
    nrow(ggplot2::ggplot_build(cut_two_plot)$layout$layout)
  )
  expect_equal(sort(as.integer(as.character(cut_two$PANEL))), 1:2)
  expect_equal(nrow(cut_two), 2)
  expect_equal(sort(cut_two$xintercept), as.numeric(sort(tapply(d$v, cut(d$v, 2), mean))))
})

test_that("a facet variable read from the caller's environment is still found", {
  # the facet expression is evaluated by ggplot2 itself against the panel's
  # own rows, in the plot's own environment, so a variable that is not a
  # column at all resolves the same way it does for the histogram layer
  k <- 2
  set.seed(11)
  d <- data.frame(v = c(rnorm(40, 0, 3), rnorm(40, 8, 3)))
  p <- gf_histogram(~v, data = d, binwidth = 1) + ggplot2::facet_wrap(~ cut(v, k))
  seg <- tagged(show_mean(p), "distribution_mean")
  expect_equal(nrow(seg), 2)
  expect_equal(sort(seg$xintercept), as.numeric(sort(tapply(d$v, cut(d$v, k), mean))))
})

test_that("a panel whose facet value is missing gets its own mean, not none", {
  set.seed(11)
  d <- data.frame(
    v = c(rnorm(20, 0, 3), rnorm(20, 8, 3), rnorm(10)),
    g = c(rep("a", 20), rep("b", 20), rep(NA, 10))
  )
  p <- gf_histogram(~v, data = d, binwidth = 1) + ggplot2::facet_wrap(~g)
  seg <- tagged(show_mean(p), "distribution_mean")
  expect_equal(nrow(seg), 3)
  expected <- c(
    mean(d$v[d$g %in% "a"]), mean(d$v[d$g %in% "b"]), mean(d$v[is.na(d$g)])
  )
  expect_equal(sort(seg$xintercept), sort(expected))
})

test_that("a facet on a non-syntactic column name is aggregated, not parsed as an expression", {
  set.seed(11)
  d <- data.frame(v = c(rnorm(40, 0, 3), rnorm(40, 8, 3)), rep(c("a", "b"), each = 40))
  names(d)[2] <- "my var"
  p <- gf_histogram(~v, data = d, binwidth = 1) + ggplot2::facet_wrap(~ `my var`)
  seg <- tagged(show_mean(p), "distribution_mean")
  expect_equal(nrow(seg), 2)
  expect_equal(sort(seg$xintercept), as.numeric(sort(tapply(d$v, d[["my var"]], mean))))
})

test_that("a shared count axis is left alone, and every panel still gets a line", {
  d <- data.frame(v = c(rep(1, 100), rep(2, 10)), g = c(rep("a", 100), rep("b", 10)))
  shared <- gf_histogram(~v, data = d, binwidth = 1) + ggplot2::facet_wrap(~g)
  expect_equal(unname(panel_tops(shared)), c(100, 100))

  drawn <- show_mean(shared)
  expect_equal(unname(panel_tops(drawn)), c(100, 100))
  seg <- tagged(drawn, "distribution_mean")
  expect_equal(sort(as.integer(as.character(seg$PANEL))), 1:2)
})

test_that("panels that share one free count axis all span its top", {
  d <- data.frame(
    v = c(rep(1, 100), rep(2, 10)),
    g = c(rep("a", 100), rep("b", 10)),
    h = c(rep(c("p", "q"), 50), rep(c("p", "q"), 5))
  )
  p <- gf_histogram(~v, data = d, binwidth = 1) +
    ggplot2::facet_grid(g ~ h, scales = "free_y")
  expect_equal(unname(panel_tops(p)), c(50, 50, 5, 5))

  # a free_y facet is where an overlay with endpoints retrained every panel to
  # the first panel's ceiling; an intercept has none to retrain them with
  drawn <- show_mean(p)
  expect_equal(unname(panel_tops(drawn)), c(50, 50, 5, 5))
  expect_equal(nrow(tagged(drawn, "distribution_mean")), 4)
})

test_that("a facet added after show_mean() still gets one mean per panel", {
  # the line is a vline: it has no vertical extent, so nothing about it has to
  # be measured off the plot before the facet exists. This used to drop the
  # mean from every panel show_mean() had not already seen, with a warning
  # telling the caller to reorder their pipeline
  d <- data.frame(v = c(rep(1, 100), rep(2, 10)), g = c(rep("a", 100), rep("b", 10)))
  p <- show_mean(gf_histogram(~v, data = d, binwidth = 1))

  expect_no_warning(seg <- tagged(p + ggplot2::facet_wrap(~g), "distribution_mean"))
  expect_equal(nrow(seg), 2)
  expect_equal(sort(as.integer(as.character(seg$PANEL))), 1:2)
  expect_equal(sort(seg$xintercept), as.numeric(sort(tapply(d$v, d$g, mean))))
})

test_that("show_mean() leaves the count axis label alone", {
  p <- show_mean(gf_histogram(~Thumb, data = Fingers, binwidth = 5))
  expect_equal(as.character(ggplot2::ggplot_build(p)$plot$labels$y), "count")
})

test_that("a transformed count axis does not move the mean line", {
  # the endpoints used to be mapped through aes() so the scale would transform
  # them exactly once. A vline has no endpoints, so a transformed count axis is
  # simply not its business: only the x position matters, and x is untouched
  plain <- tagged(show_mean(framed()), "distribution_mean")
  sqrt_y <- tagged(show_mean(framed() + ggplot2::scale_y_sqrt()), "distribution_mean")

  expect_equal(sqrt_y$xintercept, plain$xintercept)
  expect_null(sqrt_y$y)
})

test_that("show_dgp() reserves guide space without changing the panel", {
  base <- framed()
  p <- show_dgp(base)
  expect_equal(panel_top(p), panel_top(base))
  expect_equal(count_top(p), count_top(base))

  scale <- p$scales$get_scales("x")
  expect_s3_class(scale$guide, "GuideAxisStack")
  expect_length(position_guide_matches(scale$guide, "GuideDgp", "estimate"), 1)
  expect_length(position_guide_matches(scale$secondary.axis, "GuideDgp", "population"), 1)
})

test_that("the population and estimate narratives keep their teaching roles", {
  p <- show_dgp(framed())
  scale <- p$scales$get_scales("x")
  estimate <- position_guide_matches(scale$guide, "GuideDgp", "estimate")[[1]]
  population <- position_guide_matches(scale$secondary.axis, "GuideDgp", "population")[[1]]

  expect_identical(population$params$heading, "Population Parameter (DGP)")
  expect_identical(estimate$params$heading, "Parameter Estimate")
  expect_equal(population$params$label, expression(beta[1] == 0))
  expect_equal(estimate$params$label, expression(b[1] == 0))
  expect_identical(population$params$value, 0)
  expect_identical(estimate$params$value, 0)
})

test_that("the two overlays compose in either order", {
  a <- framed() %>% show_mean() %>% show_dgp()
  b <- framed() %>% show_dgp() %>% show_mean()
  expect_equal(tagged(a, "distribution_mean")$yend, tagged(b, "distribution_mean")$yend)
  expect_equal(count_top(a), count_top(b))
  expect_equal(
    length(position_guide_matches(a$scales$get_scales("x")$guide, "GuideDgp")),
    length(position_guide_matches(b$scales$get_scales("x")$guide, "GuideDgp"))
  )
})

test_that("the DGP null remains zero when the distribution mean is not", {
  p <- framed() %>% show_mean() %>% show_dgp()
  scale <- p$scales$get_scales("x")
  expect_false(isTRUE(all.equal(tagged(p, "distribution_mean")$xintercept, 0)))
  expect_identical(
    position_guide_matches(scale$guide, "GuideDgp", "estimate")[[1]]$params$value,
    0
  )
})

test_that("a second data generating process is refused rather than stacked", {
  expect_error(show_dgp(show_dgp(framed())), "already")
})

test_that("fixed and zoomed count axes are left to ggplot2", {
  fixed <- gf_histogram(~b1, data = shuffles(10), binwidth = 2) +
    ggplot2::scale_y_continuous(limits = c(0, 10))
  expect_no_error(ggplot2::ggplotGrob(show_dgp(fixed)))
  expect_no_error(show_mean(fixed))

  coord_fixed <- gf_histogram(~b1, data = shuffles(10), binwidth = 2) +
    ggplot2::coord_cartesian(ylim = c(0, 10))
  expect_no_error(ggplot2::ggplotGrob(show_dgp(coord_fixed)))

  x_zoom <- gf_histogram(~b1, data = shuffles(10), binwidth = 2) +
    ggplot2::coord_cartesian(xlim = c(-20, 20))
  drawn <- show_dgp(x_zoom)
  expect_equal(drawn$coordinates$limits$x, c(-20, 20))
})

test_that("partial and function-valued count limits are supported", {
  free_top <- gf_histogram(~b1, data = shuffles(10), binwidth = 2) +
    ggplot2::scale_y_continuous(limits = c(0, NA))
  expect_no_error(ggplot2::ggplotGrob(show_dgp(free_top)))
  raised <- show_dgp(free_top + ggplot2::expand_limits(y = 25))
  expect_equal(count_top(raised), 25)

  na_bottom <- gf_histogram(~b1, data = shuffles(10), binwidth = 2) +
    ggplot2::scale_y_continuous(limits = c(NA, 10))
  expect_no_error(ggplot2::ggplotGrob(show_dgp(na_bottom)))

  fn_limits <- framed() +
    ggplot2::scale_y_continuous(limits = function(r) c(0, max(r) * 2))
  expect_no_error(ggplot2::ggplotGrob(show_dgp(fn_limits)))
})

test_that("an entirely free count scale is unchanged", {
  both_na <- framed() + ggplot2::scale_y_continuous(limits = c(NA, NA))
  expect_equal(panel_top(show_dgp(both_na)), panel_top(both_na))
})

test_that("a transformed count axis does not affect either guide", {
  transformed <- framed() + ggplot2::scale_y_sqrt()
  expect_no_error(ggplot2::ggplotGrob(show_dgp(transformed)))
  expect_no_error(show_mean(transformed))
})

test_that("free count scales remain panel-local", {
  d <- data.frame(v = c(rep(1, 100), rep(2, 10)), g = c(rep("a", 100), rep("b", 10)))
  free_y <- gf_histogram(~v, data = d, binwidth = 1) + ggplot2::facet_wrap(~g, scales = "free_y")
  expect_equal(unname(panel_tops(free_y)), c(100, 10))
  expect_equal(unname(panel_tops(show_dgp(free_y))), c(100, 10))
  drawn <- show_mean(free_y)
  expect_equal(unname(panel_tops(drawn)), c(100, 10))

  seg <- tagged(drawn, "distribution_mean")
  expect_equal(seg$xintercept[order(seg$PANEL)], c(1, 2))
})

test_that("a two-variable plot is refused, and pointed at the function that draws it", {
  for (p in list(gf_point(Thumb ~ Height, data = Fingers),
                 gf_boxplot(Thumb ~ Sex, data = Fingers))) {
    expect_error(show_mean(p), "one distribution")
    expect_error(show_mean(p), "gf_model")
    expect_error(show_dgp(p), "one distribution")
  }
})

test_that("a non-cartesian plot is refused by name", {
  p <- gf_histogram(~Thumb, data = Fingers, binwidth = 5) + ggplot2::coord_polar()
  expect_error(show_mean(p), "cartesian")
  expect_error(show_mean(p), "CoordPolar")
  expect_error(show_dgp(p), "cartesian")
  expect_error(show_dgp(p), "CoordPolar")

  flipped <- gf_histogram(~Thumb, data = Fingers, binwidth = 5) + ggplot2::coord_flip()
  expect_error(show_dgp(flipped), "upright cartesian")
  expect_no_error(show_mean(flipped))
})

test_that("a categorical distribution is refused by name", {
  p <- gf_bar(~Sex, data = Fingers)
  expect_error(show_mean(p), "numeric")
  expect_error(show_mean(p), "Sex")
})

test_that("a discrete count axis is refused, because there is no count to mark or span", {
  # scale_y_discrete() still reports a cartesian x_range/y_range (the guard
  # a few lines above this one does not catch it), but its y_limits is
  # character/zero-length, not a top to raise or span
  p <- gf_histogram(~Thumb, data = Fingers, binwidth = 5) + ggplot2::scale_y_discrete()
  expect_error(show_mean(p), "count axis")
  expect_error(show_dgp(p), "count axis")
})

test_that("only marks that remain layers keep layer tags", {
  p <- framed() %>% show_mean() %>% show_dgp()
  added <- setdiff(
    vapply(p$layers, function(l) attr(l, "coursekata_layer") %||% "", character(1)),
    ""
  )
  expect_identical(added, "distribution_mean")
})

test_that("show_dgp snapshot", {
  skip_if_not_installed("vdiffr")
  framed() %>%
    show_mean() %>%
    show_dgp() %>%
    expect_doppelganger("show-dgp-shuffled-b1")
})
