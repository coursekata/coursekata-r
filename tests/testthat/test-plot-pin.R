test_that("a nondeterministic mapping is pinned to the one vector the plot drew", {
  # MUTATION: not pinning at all; or pinning `plot$mapping` only, which changes
  # nothing because ggformula copies the mapping onto the layer too
  set.seed(1)
  p <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers)

  q <- pin_plot_values(p)$plot

  # two separate builds, not one: a mutation that leaves the layer's own
  # mapping unpinned can still correlate with the first build by coincidence
  # -- restoring the caller's RNG stream after the pin's one evaluation means
  # a still-unpinned layer replays that exact draw on its very next build --
  # but a second build draws a fresh, uncorrelated permutation
  built_1 <- ggplot2::ggplot_build(q)$data[[1]]$y
  built_2 <- ggplot2::ggplot_build(q)$data[[1]]$y
  pin <- q$data$.coursekata_pin_y
  # a correlation, not equality: the jitter position still adds its own noise
  expect_gt(cor(built_1, pin), 0.99)
  expect_gt(cor(built_2, pin), 0.99)

  first <- q$data$.coursekata_pin_y
  ggplot2::ggplot_build(q)
  second <- q$data$.coursekata_pin_y
  expect_equal(first, second)
})

test_that("two random mappings on one plot are pinned to two independent draws", {
  # MUTATION: one save/restore per evaluation (or one fixed seed for the whole
  # loop) rewinds the stream between aesthetics, so `shuffle(Thumb)` and
  # `shuffle(Height)` receive the SAME permutation and the pinned plot keeps
  # the original correlation exactly -- in the lesson whose entire point is
  # that shuffling destroys it. Measured under the mutation: r = 0.391 every
  # time, which is `cor(Height, Thumb)` to the digit.
  drawn_cor <- function(seed) {
    set.seed(seed)
    p <- gf_point(shuffle(Thumb) ~ shuffle(Height), data = Fingers)
    built <- ggplot2::ggplot_build(pin_plot_values(p)$plot)$data[[1]]
    cor(built$x, built$y)
  }

  observed <- vapply(1:40, drawn_cor, numeric(1))
  intact <- cor(Fingers$Height, Fingers$Thumb)

  # never the intact relationship, and scattered around zero rather than
  # sitting on any one value the way a shared permutation would
  expect_false(any(abs(observed - intact) < 0.01))
  expect_lt(abs(mean(observed)), 3 / sqrt(nrow(Fingers)))
  expect_gt(sd(observed), 0)
})

test_that("one random mapping is pinned to one draw wherever it is evaluated", {
  # MUTATION: dropping the per-aesthetic fixed seed. The expression is
  # evaluated twice -- once against the plot's data, once against the layer's
  # own copy of it -- and without a seed the two are different permutations, so
  # the layer draws rows the plot's own pin column does not describe.
  set.seed(1)
  p <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers)

  q <- pin_plot_values(p)$plot

  expect_true(is.data.frame(q$layers[[1]]$data))
  expect_equal(q$layers[[1]]$data$.coursekata_pin_y, q$data$.coursekata_pin_y)
})

test_that("the pin leaves the caller's random stream exactly where it was", {
  # MUTATION: dropping the outer `with_random_seed_restored()`. The pin spends
  # draws the reader never asked for, so their next `sample()` differs by
  # nothing they wrote.
  set.seed(1)
  before <- .Random.seed
  p <- gf_point(shuffle(Thumb) ~ shuffle(Height), data = Fingers)

  pin_plot_values(p)

  expect_identical(.Random.seed, before)
})

test_that("pinning happens on a copy, in the place the mutation can appear", {
  # MUTATION: `q$layers[[i]]$mapping[[a]] <- ...` writing through the layer's
  # environment into the caller's own plot, since a layer is a ggproto object
  p <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers)

  res <- pin_plot_values(p)

  # asserting on p$data / p$mapping would not catch this: they are
  # copy-on-modify and are the two places the mutation cannot appear
  expect_false(".coursekata_pin_y" %in% names(p$layers[[1]]$data))
  expect_equal(rlang::as_label(p$layers[[1]]$mapping$y), "shuffle(Thumb)")
  invisible(res)
})

test_that("a deterministic mapping is pinned to exactly the numbers it already drew", {
  # MUTATION: reintroducing a conditional pin, or pinning the wrong evaluation
  p <- gf_point(Thumb ~ log(Height), data = Fingers)

  q <- pin_plot_values(p)$plot

  expect_true("x" %in% names(plot_pins(q)))
  expect_equal(ggplot2::ggplot_build(q)$data[[1]]$x, ggplot2::ggplot_build(p)$data[[1]]$x)
  expect_equal(q$labels$x, "log(Height)")
})

test_that("a symbol mapping and an after_stat() mapping are left alone", {
  # MUTATION: dropping rule 1's guards, which pins the function `stats::density`
  # as if it were data
  p1 <- gf_point(Thumb ~ Height, data = Fingers)
  expect_equal(pin_plot_values(p1)$pins, list())

  p2 <- gf_density(~Thumb, data = Fingers)
  res2 <- pin_plot_values(p2)
  expect_equal(res2$pins, list())
  expect_equal(plot_pins(res2$plot), list())
})

test_that("a layer carrying derived data is pinned too", {
  # MUTATION: rule 4 reverted to "only layers whose data is identical to the
  # plot's", which leaves a layer carrying its own derived data half-pinned
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, shuffle(Thumb)))
  base <- base + ggplot2::geom_point(mapping = base$mapping)
  extra <- Fingers
  extra$extra_col <- seq_len(nrow(extra))
  g <- base + ggplot2::geom_point(data = extra, mapping = base$mapping)

  q <- pin_plot_values(g)$plot

  expect_identical(rlang::quo_get_expr(q$layers[[1]]$mapping$y), quote(.coursekata_pin_y))
  expect_identical(rlang::quo_get_expr(q$layers[[2]]$mapping$y), quote(.coursekata_pin_y))
  expect_true(".coursekata_pin_y" %in% names(q$data))
  expect_true(".coursekata_pin_y" %in% names(q$layers[[2]]$data))
})

test_that("a second drawer of the same aesthetic is reported, not silently pinned", {
  # MUTATION: pinning half the plot (the layer that matches) and reporting
  # success instead of naming the layer the pin could not reach
  g <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, shuffle(Thumb))) +
    ggplot2::geom_point() +
    ggplot2::geom_point(ggplot2::aes(y = shuffle(Height)))

  res <- pin_plot_values(g)

  expect_equal(res$unreached, "y")
})

test_that("pinning twice keeps the reader's words", {
  # MUTATION: rule 7 dropped -- a second, no-op pin forgets the originally
  # recorded quosure instead of carrying it forward untouched
  p <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers)
  q1 <- pin_plot_values(p)$plot

  q2 <- pin_plot_values(q1)$plot

  pins <- plot_pins(q2)
  expect_true("y" %in% names(pins))
  expect_equal(rlang::as_label(pins$y), "shuffle(Thumb)")
  expect_false(".coursekata_pin_.coursekata_pin_y" %in% names(q2$data))
})

test_that("a reader's own axis title survives the pin", {
  # MUTATION: writing plot$labels unconditionally, which replaces the words
  # the reader chose with the deparsed mapping (`.coursekata_pin_y`, once the
  # mapping is rewritten, rather than the mapping's own spelling)
  set.seed(1)
  p <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers, ylab = "Shuffled thumb (mm)")
  q <- pin_plot_values(p)$plot

  expect_equal(q$labels$y, "Shuffled thumb (mm)")
  # messages and inference still name the mapping, not the pin column
  expect_equal(plot_spec(q)$labels[["y"]], "shuffle(Thumb)")
})

test_that("an unlabeled axis title never shows the pin column's own name", {
  # MUTATION: the fallback itself printing `.coursekata_pin_y` (the rewritten
  # mapping's spelling) instead of the ORIGINAL mapping's spelling
  set.seed(1)
  p <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers)
  q <- pin_plot_values(p)$plot

  expect_false(grepl("coursekata_pin", ggplot2::get_labs(q)$y))
})

test_that("pinning leaves the caller's random stream where it found it", {
  # MUTATION: dropping `with_random_seed_restored()`, which would change a
  # student's sampling distribution depending on whether they piped a model on
  set.seed(42)
  p1 <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers)
  a <- runif(3)

  set.seed(42)
  p2 <- gf_jitter(shuffle(Thumb) ~ Height, data = Fingers)
  invisible(pin_plot_values(p2))
  b <- runif(3)

  expect_equal(a, b)
})

test_that("a pinned plot's tagged layers keep their tags", {
  # MUTATION: `layer_with()` losing attributes, which drops
  # attr(layer, "coursekata_layer") off exactly the layer this pin has to copy
  g <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, shuffle(Thumb))) +
    ggplot2::geom_point()
  g <- g + tag_layer(ggplot2::geom_point(mapping = g$mapping), "model")

  q <- pin_plot_values(g)$plot

  expect_false(is.na(layer_index(q, "model")))
})
