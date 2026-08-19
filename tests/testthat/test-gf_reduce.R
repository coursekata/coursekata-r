# D1: THE CLAIM this whole item exists for. MUTATION: any error in the grand
# mean, in which end is which, or in the prediction.
test_that("the squared reduction sums to the model's SS Model", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers) %>% gf_reduce(model)
  built <- ggplot2::ggplot_build(p)$data[[layer_index(p, "reduce")]]

  expect_equal(
    sum((built$yend - built$y)^2),
    supernova::supernova(model)$tbl$SS[1],
    tolerance = 1e-6
  )
})

# D2: MUTATION: per-row prediction leaking observed values in rather than the
# per-group fitted mean.
test_that("a categorical reduction takes one value per group, each a group mean's own distance", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  p <- gf_point(Thumb ~ Sex, data = Fingers) %>% gf_reduce(model)
  built <- ggplot2::ggplot_build(p)$data[[layer_index(p, "reduce")]]

  grand <- mean(model$model[[1]])
  group_means <- tapply(Fingers$Thumb, Fingers$Sex, mean)
  reductions <- built$yend - built$y

  expect_length(unique(reductions), nlevels(Fingers$Sex))
  expect_equal(sort(unique(reductions)), sort(as.vector(group_means - grand)))
})

# D3: THE DECOMPOSITION. MUTATION: a grand mean computed from the plot's rows
# rather than the model's when the two differ -- here, the outcome carries a
# few NAs that lm() drops from both models' own training data, but that the
# plot's raw data (spec$data) still carries whole. A grand mean read off
# spec$data's outcome column without excluding exactly those rows -- most
# plainly, without excluding them at all, which leaves it NA -- computes a
# different number than the models were actually fit on, and the
# decomposition below stops closing.
test_that("resid(empty) minus resid(complex) equals reduce(complex), even with rows lm() dropped", {
  set.seed(1)
  na_rows <- sample(nrow(Fingers), 5)
  gappy <- Fingers
  gappy$Thumb[na_rows] <- NA

  empty_model <- lm(Thumb ~ NULL, data = gappy)
  complex_model <- lm(Thumb ~ Height, data = gappy)

  p <- gf_point(Thumb ~ Height, data = gappy) %>%
    gf_resid(empty_model) %>%
    gf_resid(complex_model) %>%
    gf_reduce(complex_model)
  built <- ggplot2::ggplot_build(p)$data
  resid_layers <- layer_indices(p, "resid")

  # ggplot_build() runs ahead of the geom's own NA drop (that happens at draw
  # time), so every layer still carries all 157 rows here, the resid layers'
  # `y` NA at the same 5 positions the reduce layer's `.grand`-based `y` is
  # not; drop them from all three so every sum is taken over the same rows
  empty_drawn <- built[[resid_layers[[1]]]][-na_rows, ]
  complex_drawn <- built[[resid_layers[[2]]]][-na_rows, ]
  reduce_drawn <- built[[layer_index(p, "reduce")]][-na_rows, ]

  ss_resid_empty <- sum((empty_drawn$yend - empty_drawn$y)^2)
  ss_resid_complex <- sum((complex_drawn$yend - complex_drawn$y)^2)
  ss_reduce <- sum((reduce_drawn$yend - reduce_drawn$y)^2)

  expect_equal(ss_resid_empty - ss_resid_complex, ss_reduce, tolerance = 1e-6)
})

# D4: x-only jitter. MUTATION: height = NULL (segments float off the mean
# line), a fresh unseeded position, or reversing the x/y draw order.
test_that("a reduction's x offsets equal the jittered points layer's own", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  p <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1, seed = 42)
  q <- p %>% gf_reduce(model)
  built <- ggplot2::ggplot_build(q)

  expect_equal(
    built$data[[layer_index(q, "reduce")]]$x,
    built$data[[1]]$x
  )
})

# D5: MUTATION: y ever being jittered.
test_that("a reduction's y is never jittered off the grand mean", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  p <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1, seed = 42)
  q <- p %>% gf_reduce(model)
  built <- ggplot2::ggplot_build(q)$data[[layer_index(q, "reduce")]]

  expect_length(unique(built$y), 1L)
})

test_that("a flipped reduction is measured on x, not on the plot's own y jitter", {
  # MUTATION: the jitter hardcoding `height = 0` (holding y still) regardless
  # of orientation, which on a flipped plot -- the model's outcome on x, not
  # y -- jitters the outcome instead of the grouping axis, and leaves the
  # segments floating off the grand mean rather than starting from it
  model <- lm(Thumb ~ Sex, data = Fingers)
  p <- gf_jitter(Sex ~ Thumb, data = Fingers, height = .1, seed = 42)
  q <- p %>% gf_reduce(model)
  built <- ggplot2::ggplot_build(q)

  expect_equal(
    built$data[[layer_index(q, "reduce")]]$y,
    built$data[[1]]$y
  )
  expect_equal(unique(built$data[[layer_index(q, "reduce")]]$x), mean(model$model[[1]]))
})

test_that("a flipped squared reduction is measured on x, not on the plot's own y jitter", {
  # MUTATION: same orientation bug as gf_reduce()'s, for the squared variant
  model <- lm(Thumb ~ Sex, data = Fingers)
  p <- gf_jitter(Sex ~ Thumb, data = Fingers, height = .1, seed = 42)
  q <- suppressMessages(p %>% gf_square_reduce(model))
  built <- ggplot2::ggplot_build(q)

  expect_equal(
    built$data[[layer_index(q, "square_reduce")]]$y,
    built$data[[1]]$y
  )
  expect_equal(unique(built$data[[layer_index(q, "square_reduce")]]$x), mean(model$model[[1]]))
})

# D6: MUTATION: pinning the caller's plot instead of the returned one.
test_that("gf_reduce() pins the jitter on the plot it returns, not the caller's", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  p <- gf_jitter(Thumb ~ Sex, data = Fingers, width = .1)
  expect_true(is.na(p$layers[[1]]$position$seed))

  q <- p %>% gf_reduce(model)

  expect_true(is.na(p$layers[[1]]$position$seed))
  expect_true(is.finite(q$layers[[1]]$position$seed))
})

# D7: square shape. MUTATION: a stat-time expansion -- do NOT assert
# 4 * nrow(data), since square_vertices() runs in draw_panel() and
# ggplot_build() output is one row per observation.
test_that("a squared reduction is one row per observation; square_vertices() draws the expansion", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)
  q <- suppressMessages(gf_square_reduce(p, model))
  built <- ggplot2::ggplot_build(q)$data[[layer_index(q, "square_reduce")]]

  expect_equal(nrow(built), nrow(Fingers))
  expect_true(all(c("x", "y") %in% names(built)))
  expect_equal(sum(c("xend", "yend") %in% names(built)), 1L)

  vertices <- square_vertices(built, x_range = c(50, 80), y_range = c(50, 70), aspect = 4 / 6)
  for (g in unique(vertices$group)) {
    corners <- vertices[vertices$group == g, ]
    expect_equal(nrow(corners), 4L)
    expect_length(unique(corners$x), 2L)
    expect_length(unique(corners$y), 2L)
  }
})

# D8: absolute side length. MUTATION: aspect applied per row instead of
# globally, and the range-ratio scaling dropped. Do NOT assert a ratio of two
# sides -- both aspect and ratio cancel there.
test_that("a square's side is the reduction scaled by aspect and the panel's range ratio", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)
  q <- suppressMessages(gf_square_reduce(p, model))
  built <- ggplot2::ggplot_build(q)$data[[layer_index(q, "square_reduce")]]

  # x_range's span (40) must differ from y_range's (20) -- equal spans make
  # the range-ratio factor exactly 1, so it cancels and stops discriminating
  # the mutation this test names
  x_range <- c(55, 95)
  y_range <- c(50, 70)
  aspect <- 4 / 6
  vertices <- square_vertices(built, x_range = x_range, y_range = y_range, aspect = aspect)

  row <- built[1, ]
  side <- diff(range(vertices$x[vertices$group == 1]))
  expected <- abs(row$y - row$yend) * aspect * diff(x_range) / diff(y_range)

  expect_equal(side, expected)
})

# D9: MUTATION: the warning missing, or becoming a refusal.
test_that("the empty model warns rather than refuses, and still returns a plot", {
  empty_model <- lm(Thumb ~ NULL, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  expect_warning(
    result <- gf_reduce(p, empty_model),
    class = "coursekata_reduce_empty"
  )
  expect_s3_class(result, "ggplot")
})

# MUTATION: the empty-model warning hardcoding `gf_reduce()` in its text
# rather than naming the function it was actually raised from.
test_that("the empty model's warning names gf_square_reduce(), not gf_reduce()", {
  empty_model <- lm(Thumb ~ NULL, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  expect_warning(
    suppressMessages(gf_square_reduce(p, empty_model)),
    "`gf_square_reduce\\(\\)` was given the empty model"
  )
})

# D10: refusal order. MUTATION: reordering reduce_spec()'s guards -- both
# resid_fitted() and resid_end() would refuse too, for the model's variables
# being entirely absent from the plot's data, so seeing the axes refusal
# specifically is evidence of order and not just of the check existing.
test_that("a plot with no y refuses for its axes before anything is predicted", {
  model <- lm(later_anxiety ~ base_anxiety, data = er)
  p <- gf_histogram(~Thumb, data = Fingers)

  expect_error(gf_reduce(p, model), "needs both an x and a y")
})

test_that("gf_reduce() refuses anything but a plot before it looks at the model", {
  expect_error(gf_reduce(Fingers, lm(Thumb ~ Height, data = Fingers)), "layered on top of a plot")
})

test_that("gf_reduce() refuses a missing model after confirming it has a plot", {
  p <- gf_point(Thumb ~ Height, data = Fingers)
  expect_error(gf_reduce(p, NULL), "which model to measure")
})

# D11: spellings. MUTATION: the gformula rescue in `pre`.
test_that("gf_reduce(p, m), gf_reduce(p, model = m) and piped all build identical layers", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)

  positional <- gf_reduce(p, model)
  named <- gf_reduce(p, model = model)
  piped <- p %>% gf_reduce(model)

  built_positional <- ggplot2::ggplot_build(positional)$data[[layer_index(positional, "reduce")]]
  built_named <- ggplot2::ggplot_build(named)$data[[layer_index(named, "reduce")]]
  built_piped <- ggplot2::ggplot_build(piped)$data[[layer_index(piped, "reduce")]]

  expect_equal(built_positional, built_named)
  expect_equal(built_positional, built_piped)
})

# D12: facets. MUTATION: a per-panel grand mean sneaking in.
test_that("every panel's reduction segments share one grand mean", {
  model <- lm(later_anxiety ~ base_anxiety, data = er)
  p <- gf_point(later_anxiety ~ base_anxiety, data = er) %>%
    gf_facet_grid(condition ~ .) %>%
    gf_reduce(model)
  built <- ggplot2::ggplot_build(p)$data[[layer_index(p, "reduce")]]

  expect_length(unique(built$y), 1L)
  expect_equal(unique(built$y), mean(model$model[[1]]))
})

# D13: MUTATION: the `pre` gate.
test_that("a bare call prints the layer's own help instead of drawing", {
  expect_message(bare <- gf_reduce(), "does not require a formula")
  expect_null(bare)
})

# A mapped aesthetic written on the call resolves on the squares, because
# `reduce_spec()` carries `spec$data` rather than only the columns the geom
# itself reads.
test_that("a mapped fill written on the call resolves on the squares", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)
  q <- suppressMessages(gf_square_reduce(p, model, fill = ~Sex))
  built <- ggplot2::ggplot_build(q)$data[[layer_index(q, "square_reduce")]]

  expect_length(unique(built$fill), nlevels(Fingers$Sex))
})

test_that("the three square layers carry the sums of squares they claim to", {
  # The reason gf_square_reduce() exists: total = error + reduction, as areas a
  # reader can compare. Segment lengths are asserted elsewhere; the areas are the
  # claim the layer is named for, and were only ever checked by a picture.
  #
  # Every expected value is computed here from lm() rather than from the package,
  # so a change in how a square is built cannot move the target with it.
  d <- head(Fingers, 12)
  complex_model <- lm(Thumb ~ Height, data = d)
  empty_model <- lm(Thumb ~ NULL, data = d)

  ss_total <- sum((d$Thumb - mean(d$Thumb))^2)
  ss_error <- sum(residuals(complex_model)^2)
  ss_model <- sum((fitted(complex_model) - mean(d$Thumb))^2)

  built <- ggplot2::ggplot_build(
    gf_point(Thumb ~ Height, data = d) %>%
      gf_square_resid(empty_model) %>%
      gf_square_resid(complex_model) %>%
      gf_square_reduce(complex_model)
  )
  # a square's side is the distance it spans, so the areas are the squared sides
  area_of <- function(i) sum((built$data[[i]]$y - built$data[[i]]$yend)^2)

  expect_equal(area_of(2), ss_total)
  expect_equal(area_of(3), ss_error)
  expect_equal(area_of(4), ss_model)

  # the identity itself, which no single layer can satisfy alone
  expect_equal(area_of(2), area_of(3) + area_of(4))
})

test_that("a fit whose squares would not add up is refused", {
  # MUTATION: drawing the reduction anyway from an unweighted grand mean. The
  # whole claim is an identity, and PRE is read off it as a ratio of areas a
  # student can count. Measured on `Thumb ~ Height - 1`: error plus reduction
  # comes to 11700.01 against a total of 11880.21, so about 180 of the total
  # belongs to neither square and nothing on the page says so. Weighted, the
  # parts overshoot the total instead, by about 11.
  set.seed(3)
  df <- Fingers[!is.na(Fingers$Thumb) & !is.na(Fingers$Height), ]
  df$w <- runif(nrow(df), .5, 2)
  p <- gf_point(Thumb ~ Height, data = df)

  expect_error(gf_reduce(p, lm(Thumb ~ Height - 1, data = df)), "arithmetic does not support")
  unsupported <- "arithmetic does not support"
  expect_error(gf_reduce(p, lm(Thumb ~ Height, data = df, weights = w)), unsupported)
  expect_error(gf_square_reduce(p, lm(Thumb ~ Height - 1, data = df)), unsupported)
})

test_that("the same fits are still measurable as residuals", {
  # MUTATION: putting the refusal somewhere `gf_resid()` shares. A residual is
  # `observed - fitted` whatever the fit, and needs no identity to hold; only
  # the reduction is measured from a grand mean the model may not reduce from.
  set.seed(3)
  df <- Fingers[!is.na(Fingers$Thumb) & !is.na(Fingers$Height), ]
  df$w <- runif(nrow(df), .5, 2)
  p <- gf_point(Thumb ~ Height, data = df)

  expect_no_error(gf_resid(p, lm(Thumb ~ Height - 1, data = df)))
  expect_no_error(gf_square_resid(p, lm(Thumb ~ Height, data = df, weights = w)))
})

test_that("the alias is generated from the same factory call, with only its name changed", {
  # MUTATION: writing `gf_squareduce()` as a forwarder. `gf_squareduce()` is its
  # own `layer_factory()` call so that its refusals, its help and its
  # experimental signal name the function the caller actually wrote. That second
  # call is what has to be paid for: every argument the factory stored on the two
  # closures, `pre` included, must match after a mechanical rename, so an edit
  # made to one and not the other fails here rather than reaching a reader.
  reduce <- environment(gf_square_reduce)
  alias <- environment(gf_squareduce)
  rename <- function(x) {
    gsub("gf_squareduce", "gf_square_reduce", paste(deparse(x), collapse = "\n"), fixed = TRUE)
  }

  expect_setequal(ls(alias), ls(reduce))
  # `res` is the generated closure itself, and is compared by its own parts below
  for (stored in setdiff(ls(reduce), "res")) {
    expect_identical(rename(get(stored, alias)), rename(get(stored, reduce)), info = stored)
  }
  expect_identical(formals(gf_squareduce), formals(gf_square_reduce))
  expect_identical(body(gf_squareduce), body(gf_square_reduce))
})

test_that("the alias draws exactly what the function it aliases draws", {
  # MUTATION: the alias tagging its layer with a name of its own. Both names
  # draw one layer and `layer_index(p, "square_reduce")` has to find either.
  model <- lm(Thumb ~ Sex, data = Fingers)
  strip <- function(plot) {
    i <- layer_index(plot, "square_reduce")
    layer <- plot$layers[[i]]
    list(
      i = i, geom = class(layer$geom)[[1]], stat = class(layer$stat)[[1]],
      position = class(layer$position)[[1]], inherit = layer$inherit.aes,
      aes_params = layer$aes_params, geom_params = layer$geom_params,
      mapping = vapply(layer$mapping, rlang::as_label, character(1)),
      built = ggplot2::ggplot_build(plot)$data[[i]],
      grob_x = as.numeric(ggplot2::layer_grob(plot, i)[[1]]$x)
    )
  }
  # an unseeded jitter is pinned with a fresh seed per call, so a jittered pair
  # is only comparable under the same seed
  jittered <- function(fn) {
    set.seed(7)
    suppressMessages(fn(gf_jitter(Thumb ~ Sex, data = Fingers, width = .1), model))
  }
  faceted <- function(fn) {
    suppressMessages(fn(
      gf_point(Thumb ~ Height, data = Fingers) %>% gf_facet_wrap(~RaceEthnic),
      lm(Thumb ~ Height, data = Fingers)
    ))
  }
  plain <- function(fn) suppressMessages(fn(gf_point(Thumb ~ Sex, data = Fingers), model))

  for (build in list(plain, jittered, faceted)) {
    expect_equal(strip(build(gf_squareduce)), strip(build(gf_square_reduce)))
  }
})

test_that("the alias refuses in its own name", {
  # MUTATION: a forwarder, which reports `gf_square_reduce()` for a call the
  # reader wrote as `gf_squareduce()` -- the whole reason this is generated.
  expect_error(gf_squareduce(gf_point(Thumb ~ Height, data = Fingers)), "gf_squareduce")
  expect_error(
    gf_squareduce(gf_point(Thumb ~ Height, data = Fingers), lm(Thumb ~ Height - 1, data = Fingers)),
    "gf_squareduce"
  )
})

test_that("the alias resolves a mapping written in the frame that wrote it", {
  # MUTATION: a forwarder. `layer_factory()`'s generated
  # `environment = parent.frame()` would then resolve to the forwarder's own
  # frame, and this mapping would stop resolving with no error until build.
  model <- lm(Thumb ~ Height, data = Fingers)
  drawn_in_a_function <- function(fn) {
    local_color <- Fingers$Sex
    suppressMessages(fn(gf_point(Thumb ~ Height, data = Fingers), model, color = ~local_color))
  }

  for (fn in list(gf_square_reduce, gf_squareduce)) {
    plot <- drawn_in_a_function(fn)
    drawn <- ggplot2::ggplot_build(plot)$data[[layer_index(plot, "square_reduce")]]
    expect_gt(length(unique(drawn$colour)), 1)
  }
})
