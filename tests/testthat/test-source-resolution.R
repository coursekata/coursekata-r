test_that("source rows and mappings agree with ggplot2's observation layer", {
  d <- data.frame(x = 1:8, y = c(2, 8, 3, 9, 5, 12, 6, 20), group = rep(1:2, 4))
  sources <- list(d[1:3, ], function(data) data[1:3, ], ~ .x[1:3, ])
  for (source in sources) {
    for (local in c(FALSE, TRUE)) {
      p <- if (local) ggplot2::ggplot(d) else ggplot2::ggplot(d, ggplot2::aes(x, y))
      p <- p + ggplot2::geom_point(
        data = source, mapping = if (local) ggplot2::aes(x, y) else NULL
      )
      spec <- plot_spec(p)
      actual <- ggplot2::layer_data(p)
      expect_equal(nrow(spec$data), nrow(actual))
      for (a in c("x", "y")) {
        resolved <- spec$resolve_aes(a)
        expect_identical(resolved$data, spec$data)
        expect_equal(rlang::eval_tidy(resolved$quo, resolved$data), actual[[a]])
      }
    }
  }
})

test_that("source mapping overrides and removals follow ggplot2 inheritance", {
  d <- data.frame(x = 1:4, y = 4:1, z = 11:14)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = z)) +
    ggplot2::geom_point(ggplot2::aes(x = z, colour = NULL))
  expect_equal(plot_spec(p)$labels, c(x = "z", y = "y"))
  expect_equal(plot_spec(p)$resolve_aes("x")$owner, "layer")
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = z)) +
    ggplot2::geom_point(ggplot2::aes(z, x), inherit.aes = FALSE)
  expect_equal(plot_spec(p)$labels, c(x = "z", y = "x"))
  expect_null(plot_spec(p)$resolve_aes("colour"))
})

test_that("45 points receive 45 residuals through both interfaces", {
  males <- subset(Fingers, Gender == "male")
  model <- lm(Thumb ~ Height, data = Fingers)
  expect_equal(nrow(males), 45L)
  for (source in list(males, function(d) subset(d, Gender == "male"),
                     ~ subset(.x, Gender == "male"))) {
    p <- ggplot2::ggplot(Fingers) +
      ggplot2::geom_point(data = source, ggplot2::aes(Height, Thumb))
    for (q in list(p + geom_resid(model = model), gf_resid(p, model))) {
      drawn <- ggplot2::ggplot_build(q)$data
      expect_equal(nrow(drawn[[2]]), 45L)
      expect_equal(drawn[[2]]$x, drawn[[1]]$x)
      expect_equal(drawn[[2]]$y, drawn[[1]]$y)
      expect_equal(drawn[[2]]$yend, unname(predict(model, males)))
      point <- ggplot2::layer_grob(q, 1)[[1]]
      residual <- ggplot2::layer_grob(q, 2)[[1]]
      expect_length(residual$x0, 45L)
      expect_equal(as.numeric(residual$x0), as.numeric(point$x))
      expect_equal(as.numeric(residual$y1), as.numeric(point$y))
    }
  }
})

test_that("source selection prefers observations and jitter uses that same source", {
  d <- data.frame(x = rep(1:2, each = 5), y = c(1:5, 8:12))
  model <- lm(y ~ x, d)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_boxplot(ggplot2::aes(group = x)) +
    ggplot2::geom_point(data = d[2:8, ], position = ggplot2::position_jitter(seed = 17)) +
    ggplot2::geom_point(data = d[9:10, ])
  expect_identical(plot_source_index(p), 2L)
  expect_equal(plot_spec(p)$data$x, d$x[2:8])
  expect_equal(plot_spec(p)$data$y, d$y[2:8])
  q <- gf_resid(p, model)
  drawn <- ggplot2::ggplot_build(q)$data
  expect_equal(drawn[[4]]$x, drawn[[2]]$x)
  expect_equal(drawn[[4]]$y, drawn[[2]]$y)
  expect_equal(drawn[[4]]$yend, unname(predict(model, d[2:8, ])))
  expect_identical(plot_source_index(q), 2L)
})

test_that("faceted residuals retain the selected layer's panels and rows", {
  d <- Fingers[seq(1, nrow(Fingers), by = 3), ]
  model <- lm(Thumb ~ Height, Fingers)
  p <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
    ggplot2::geom_point(data = d) + ggplot2::facet_wrap(~Sex)
  for (q in list(p + geom_resid(model = model), gf_resid(p, model))) {
    drawn <- ggplot2::ggplot_build(q)$data
    expect_gt(length(unique(drawn[[1]]$PANEL)), 1L)
    expect_identical(drawn[[2]]$PANEL, drawn[[1]]$PANEL)
    expect_equal(drawn[[2]]$y, drawn[[1]]$y)
  }
})

test_that("every residual and reduction form measures the selected observations", {
  d <- data.frame(x = 1:12, y = c(2, 5, 3, 8, 4, 9, 10, 6, 15, 13, 12, 20))
  rows <- d[c(2, 4, 6, 9, 12), ]
  model <- lm(y ~ x, d)
  p <- ggplot2::ggplot(d) + ggplot2::geom_point(ggplot2::aes(x, y), data = rows)
  native <- list(geom_resid, geom_reduce, geom_square_resid, geom_square_reduce,
                 stat_resid, stat_reduce)
  gf <- list(gf_resid, gf_reduce, gf_square_resid, gf_square_reduce)
  plots <- c(lapply(native, function(fn) p + fn(model = model)),
             lapply(gf, function(fn) fn(p, model)))
  for (q in plots) {
    layer <- q$layers[[2]]
    drawn <- ggplot2::layer_data(q, 2)
    expect_equal(nrow(drawn), nrow(rows))
    expect_equal(drawn$x, rows$x)
    expect_equal(drawn$yend, unname(predict(model, rows)))
    reduction <- inherits(layer$stat, "StatReduce") || grepl("reduce", attr(layer, "coursekata_layer"))
    expect_equal(drawn$y, if (reduction) rep(mean(d$y), nrow(rows)) else rows$y)
    grob <- ggplot2::layer_grob(q, 2)[[1]]
    if (inherits(layer$geom, "GeomSquareResid")) {
      expect_length(grob$x, 4 * nrow(rows))
      expect_length(unique(grob$id), nrow(rows))
    } else {
      expect_length(grob$x0, nrow(rows))
    }
  }
  for (fn in list(gf_resid_fun, gf_square_resid_fun)) {
    q <- fn(p, function(x) 2 + x)
    drawn <- ggplot2::layer_data(q, 2)
    expect_equal(drawn$y, rows$y)
    expect_equal(drawn$yend, 2 + rows$x)
  }
})

test_that("ordinary native inheritance survives post-add plot mutation", {
  d <- data.frame(x = 1:8, y = 2:9, z = 3:10)
  model <- lm(y ~ x, d)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    geom_resid(model = model)
  q <- p + d[2:4, ]
  drawn <- ggplot2::ggplot_build(q)$data
  expect_equal(drawn[[2]]$y, drawn[[1]]$y)
  expect_equal(nrow(drawn[[2]]), 3L)
  q <- p + ggplot2::aes(x = z)
  drawn <- ggplot2::ggplot_build(q)$data
  expect_equal(drawn[[2]]$x, d$z)
  expect_equal(drawn[[2]]$x, drawn[[1]]$x)
  expect_null(p$layers[[2]]$mapping$x)
  q <- p + ggplot2::aes(x = NULL)
  expect_error(ggplot2::ggplot_build(q), "missing aesthetics")
})

test_that("an explicit source mapping remains local even when it repeats the plot", {
  d <- data.frame(x = 1:6, y = 2:7, z = 11:16)
  mapping <- ggplot2::aes(x, y)
  p <- ggplot2::ggplot(d, mapping) + ggplot2::geom_point(mapping) +
    geom_resid(model = lm(y ~ x, d))
  q <- p + ggplot2::aes(x = z)
  drawn <- ggplot2::ggplot_build(q)$data
  expect_equal(drawn[[1]]$x, d$x)
  expect_equal(drawn[[2]]$x, drawn[[1]]$x)
})

test_that("function-owned samples stay shared across builds and new plot data", {
  d <- data.frame(x = 1:30, y = (1:30)^2)
  model <- lm(y ~ x, d)
  sample_rows <- function(data) data[sample(nrow(data), 8), ]
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point(data = sample_rows)
  original_data <- get("data", p$layers[[1]])
  set.seed(8)
  before <- .Random.seed
  q <- p + geom_resid(model = model)
  expect_identical(.Random.seed, before)
  for (plot in list(q, q, q + d[1:20, ])) {
    drawn <- ggplot2::ggplot_build(plot)$data
    expect_equal(drawn[[2]]$x, drawn[[1]]$x)
    expect_equal(drawn[[2]]$y, drawn[[1]]$y)
  }
  expect_identical(get("data", p$layers[[1]]), original_data)
})

test_that("rulers and distribution annotations use source rows and local quosures", {
  d <- data.frame(x = 1:8, y = (1:8)^2)
  local_mapping <- local({ shift <- function(x) x + 100; ggplot2::aes(shift(x), y) })
  p <- ggplot2::ggplot(d, ggplot2::aes(x, x)) +
    ggplot2::geom_point(data = ~ .x[2:4, ], mapping = local_mapping)
  q <- gf_sd_ruler(p, where = "mean")
  ruler <- ggplot2::layer_data(q, 2)
  expect_equal(ruler$x, 103)
  expect_equal(ruler$y, mean(d$y[2:4]))
  expect_equal(ruler$yend, mean(d$y[2:4]) + sd(d$y[2:4]))
  p <- ggplot2::ggplot(d, ggplot2::aes(y)) +
    ggplot2::geom_histogram(data = ~ .x[2:4, ], mapping = ggplot2::aes(x), bins = 3)
  expect_equal(ggplot2::layer_data(show_mean(p), 2)$xintercept, 3)
})

test_that("squareplots resolve layer-owned categorical data", {
  d <- data.frame(x = factor(c("a", "a", "b", "c")))
  p <- ggplot2::ggplot() + ggplot2::geom_blank(data = d, ggplot2::aes(x))
  for (q in list(p + geom_squareplot(), gf_squareplot(p))) {
    drawn <- ggplot2::layer_data(q, 2)
    expect_equal(nrow(drawn), 4L)
    expect_equal(drawn$count, c(2, 2, 1, 1))
  }
})

test_that("mapping probes do not spend the reader's RNG", {
  d <- data.frame(x = 1:12)
  set.seed(5)
  p <- ggplot2::ggplot(d, ggplot2::aes(sample(x))) + ggplot2::geom_histogram(bins = 4)
  before <- .Random.seed
  show_mean(p)
  expect_identical(.Random.seed, before)
  p + geom_squareplot(bins = 4)
  expect_identical(.Random.seed, before)
})

test_that("pinning callbacks follows constructed rows and retains evaluation environments", {
  d <- data.frame(x = 1:8, y = (1:8)^2)
  first <- local({ offset <- 10; ggplot2::aes(x, y + offset) })
  second <- local({ offset <- 20; ggplot2::aes(x, y + offset) })
  p <- ggplot2::ggplot(d, first) +
    ggplot2::geom_point(data = function(data) data.frame(x = data$x[2:5], y = data$y[2:5])) +
    ggplot2::geom_point(second)
  result <- pin_plot_values(p)
  expect_equal(result$unreached, "y")
  q <- result$plot
  drawn <- ggplot2::ggplot_build(q)$data
  expect_equal(drawn[[1]]$y, d$y[2:5] + 10)
  expect_equal(drawn[[2]]$y, d$y + 20)
  spec <- plot_spec(q)
  expect_equal(rlang::eval_tidy(spec$mapping$y, spec$data), drawn[[1]]$y)
  expect_identical(ggplot2::ggplot_build(q)$data, drawn)
})

test_that("pinning preserves existing columns and notices replaced mappings", {
  d <- data.frame(x = 1:5, y = (1:5)^2, .coursekata_pin_y = 101:105)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, log(y))) + ggplot2::geom_point()
  q <- pin_plot_values(p)$plot
  expect_equal(q$data$.coursekata_pin_y, d$.coursekata_pin_y)
  expect_equal(ggplot2::layer_data(q)$y, log(d$y))
  implied <- implied_model(q)
  expect_equal(implied$data[[implied$outcome$column]], log(d$y))
  q <- q + ggplot2::aes(y = x)
  expect_null(plot_pins(q)$y)
  expect_equal(plot_spec(q)$labels[["y"]], "x")
  expect_equal(ggplot2::layer_data(q)$y, d$x)
})

test_that("stage expressions remain build-time mappings", {
  p <- ggplot2::ggplot(data.frame(x = 1:5), ggplot2::aes(x)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::stage(after_stat = count)), bins = 3)
  expect_equal(pin_plot_values(p)$pins, list())
})

test_that("a sibling's explicit mapping removal is retained in native residuals", {
  d <- data.frame(x = 1:5, y = (1:5)^2, z = letters[1:5])
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = z)) +
    ggplot2::geom_point(ggplot2::aes(colour = NULL)) +
    geom_resid(model = lm(y ~ x, d))
  drawn <- ggplot2::ggplot_build(p)$data
  expect_length(unique(drawn[[2]]$colour), 1L)
  expect_null(p$layers[[2]]$mapping$colour)
})

test_that("a plot-owned expression is pinned on rows a callback constructs", {
  d <- data.frame(x = 1:10)
  construct <- function(data) transform(data, y = exp(2 + data$x / 4))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, log(y))) +
    ggplot2::geom_point(data = construct)
  before <- ggplot2::layer_data(p)
  q <- pin_plot_values(p)$plot
  expect_identical(q$data, d)
  expect_identical(q$mapping, p$mapping)
  expect_true(is.function(q$layers[[1]]$data))
  expect_equal(ggplot2::layer_data(q)$y, before$y)
  expect_identical(ggplot2::layer_data(q), ggplot2::layer_data(q))
  expect_equal(plot_spec(q)$labels[["y"]], "log(y)")
  expect_equal(plot_spec(q)$resolve_aes("y")$owner, "layer")
  expect_identical(ggplot2::layer_data(pin_plot_values(q)$plot), ggplot2::layer_data(q))

  changed <- q + data.frame(x = 21:26)
  expect_equal(plot_spec(changed)$labels[["y"]], "log(y)")
  expect_equal(ggplot2::layer_data(changed)$y, 2 + (21:26) / 4)
  # New drawers still receive the original expression and can construct their
  # own rows; a layer-local storage column must not leak into plot inheritance.
  changed <- changed + ggplot2::geom_point(data = construct)
  drawn <- ggplot2::ggplot_build(changed)$data
  expect_equal(drawn[[2]]$y, drawn[[1]]$y)
})

test_that("implied models fit callback-created outcomes and rebuild consistently", {
  d <- data.frame(x = 1:18)
  construct <- function(data) {
    transform(data[sample(nrow(data), 10), , drop = FALSE], y = exp(1 + x / 3))
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(x, log(y))) +
    ggplot2::geom_point(data = construct)
  set.seed(111)
  before <- .Random.seed
  inferred <- gf_model(p)
  expect_identical(.Random.seed, before)
  first <- ggplot2::ggplot_build(inferred)$data
  second <- ggplot2::ggplot_build(inferred)$data
  expect_equal(first[[1]]$y, 1 + first[[1]]$x / 3)
  expect_equal(first[[2]]$y, 1 + first[[2]]$x / 3, tolerance = 1e-12)
  expect_equal(range(first[[2]]$x), range(first[[1]]$x))
  expect_identical(second, first)
  expect_identical(.Random.seed, before)

  # The same binding used by native source-aware layers supplies StatModel
  # with the callback's rows while preserving its ordinary build-time fit.
  native <- pin_plot_values(p)$plot + source_layer(geom_model())
  first <- ggplot2::ggplot_build(native)$data
  expect_equal(first[[2]]$y, 1 + first[[2]]$x / 3, tolerance = 1e-12)
  expect_equal(range(first[[2]]$x), range(first[[1]]$x))
  expect_identical(ggplot2::ggplot_build(native)$data, first)
  changed <- native + data.frame(x = 31:50)
  drawn <- ggplot2::ggplot_build(changed)$data
  expect_equal(drawn[[2]]$y, 1 + drawn[[2]]$x / 3, tolerance = 1e-12)
  expect_equal(range(drawn[[2]]$x), range(drawn[[1]]$x))
  expect_gt(min(drawn[[1]]$x), 30)
})

test_that("local source pins coexist with a sibling that uses the raw plot rows", {
  d <- data.frame(x = 1:8, y = exp(1:8))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, log(y))) +
    ggplot2::geom_point(data = function(data) transform(data[2:5, ], y = y * 2)) +
    ggplot2::geom_point()
  q <- pin_plot_values(p)$plot
  expect_identical(q$data, d)
  drawn <- ggplot2::ggplot_build(q)$data
  expect_equal(drawn[[1]]$y, log(d$y[2:5] * 2))
  expect_equal(drawn[[2]]$y, log(d$y))
  changed <- q + transform(d, y = y * 3)
  drawn <- ggplot2::ggplot_build(changed)$data
  expect_equal(drawn[[1]]$y, log(d$y[2:5] * 6))
  expect_equal(drawn[[2]]$y, log(d$y * 3))
})

test_that("pin columns do not overwrite columns made by sibling callbacks", {
  d <- data.frame(x = 1:6, y = exp(1:6))
  calls <- 0L
  callback <- function(data) {
    calls <<- calls + 1L
    transform(data, .coursekata_pin_x = 21:26, .coursekata_pin_y = 101:106)
  }
  for (data in list(callback, ~ callback(.x))) {
    p <- ggplot2::ggplot(d, ggplot2::aes(log(x), log(y))) +
      ggplot2::geom_point() +
      ggplot2::geom_point(data = data, ggplot2::aes(colour = .coursekata_pin_y))
    before <- ggplot2::layer_grob(p, 2)[[1]]$gp$col
    calls <- 0L
    set.seed(82)
    seed <- .Random.seed
    q <- pin_plot_values(p)$plot
    expect_equal(calls, 1L)
    expect_identical(.Random.seed, seed)
    rows <- q$layers[[2]]$layer_data(q$data)
    expect_equal(rows$.coursekata_pin_x, 21:26)
    expect_equal(rows$.coursekata_pin_y, 101:106)
    expect_equal(ggplot2::layer_data(q, 2)$y, log(d$y))
    expect_identical(ggplot2::layer_grob(q, 2)[[1]]$gp$col, before)
    expect_identical(ggplot2::layer_grob(q, 2)[[1]]$gp$col, before)
  }
})

test_that("an unrelated source cannot claim a pin by its column's spelling", {
  d <- data.frame(x = 1:5, y = exp(1:5))
  p <- ggplot2::ggplot(d, ggplot2::aes(x, log(y))) + ggplot2::geom_point()
  q <- pin_plot_values(p)$plot
  unrelated <- data.frame(x = 11:15, .coursekata_pin_y = 101:105)
  # Even inheriting the exact pinned plot quosure is insufficient: this new
  # source owns unrelated values that happen to use the same column name.
  new_source <- ggplot2::geom_point(data = unrelated)
  replaced <- q
  replaced$layers <- list(new_source)
  prepended <- q
  prepended$layers <- c(list(new_source), q$layers)
  replaced_data <- q + unrelated
  for (plot in list(replaced, prepended, replaced_data)) {
    expect_null(plot_pins(plot)$y)
    expect_equal(plot_spec(plot)$labels[["y"]], ".coursekata_pin_y")
    expect_equal(ggplot2::layer_data(plot, 1)$y, unrelated$.coursekata_pin_y)
  }
  # A fresh layer inheriting the actual pinned plot rows still uses the pin.
  inherited <- q
  inherited$layers <- list(ggplot2::geom_point())
  expect_equal(plot_spec(inherited)$labels[["y"]], "log(y)")
})

test_that("replacing a pinned layer's data binding invalidates its old label", {
  d <- data.frame(x = 1:5, y = exp(1:5))
  raw <- data.frame(x = 21:26)
  unrelated <- data.frame(x = 11:15, .coursekata_pin_y = 101:105)
  for (data in list(d, function(input) d, ~ d)) {
    p <- ggplot2::ggplot(raw, ggplot2::aes(x, log(y))) +
      ggplot2::geom_point(data = data)
    q <- pin_plot_values(p)$plot
    expect_equal(plot_spec(q)$labels[["y"]], "log(y)")
    # Keep the exact source layer attributes and mapping while replacing only
    # the data binding. The layer's old token alone cannot validate the pin.
    for (replacement in list(unrelated, function(input) unrelated)) {
      changed <- q
      changed$layers[[1]] <- layer_with(q$layers[[1]], data = replacement)
      expect_null(plot_pins(changed)$y)
      expect_equal(plot_spec(changed)$labels[["y"]], ".coursekata_pin_y")
      expect_equal(ggplot2::layer_data(changed)$y, unrelated$.coursekata_pin_y)
      expect_identical(ggplot2::layer_data(changed), ggplot2::layer_data(changed))
      expect_null(pin_plot_values(changed)$pins$y)
    }
    # Copies of an unchanged binding, including repeated builds, stay valid.
    copied <- q
    copied$layers[[1]] <- layer_with(q$layers[[1]], show.legend = FALSE)
    expect_equal(plot_spec(copied)$labels[["y"]], "log(y)")
    expect_equal(ggplot2::layer_data(copied)$y, log(d$y))
  }
})

test_that("replacing a stabilized callback establishes a new reproducible source", {
  raw <- data.frame(x = 1:20)
  original <- function(data) transform(data, y = exp(x / 4))
  p <- ggplot2::ggplot(raw, ggplot2::aes(x, log(y))) +
    ggplot2::geom_point(data = original)
  q <- pin_plot_values(p)$plot
  replacement <- function(data) {
    data <- data[sample(nrow(data), 10), , drop = FALSE]
    transform(data, .coursekata_pin_y = x * 10)
  }
  q$layers[[1]] <- layer_with(q$layers[[1]], data = replacement)
  inferred <- gf_model(q)
  expect_equal(plot_spec(inferred)$labels[["y"]], ".coursekata_pin_y")
  first <- ggplot2::ggplot_build(inferred)$data
  expect_equal(first[[2]]$y, first[[2]]$x * 10, tolerance = 1e-12)
  expect_equal(range(first[[2]]$x), range(first[[1]]$x))
  expect_identical(ggplot2::ggplot_build(inferred)$data, first)
})
