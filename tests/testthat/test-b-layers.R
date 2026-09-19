b_test_mark <- coefficient_test_mark

b_test_built_mark <- function(plot, tag) {
  ggplot2::ggplot_build(plot)$data[[layer_index(plot, tag)]]
}

test_that("addition evaluates no source callbacks or models and each build fits once", {
  rows <- data.frame(x = 1:10, y = 2 * (1:10) + 1)
  callbacks <- fits <- 0L
  source <- function(data) { callbacks <<- callbacks + 1L; data }
  suppressMessages(trace("lm", where = asNamespace("stats"), print = FALSE,
                         tracer = function() fits <<- fits + 1L))
  withr::defer(suppressMessages(untrace("lm", where = asNamespace("stats"))))
  base <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) + ggplot2::geom_point(data = source)
  out <- base + geom_b(show_b0 = FALSE, run = 1)
  expect_equal(callbacks, 0L)
  expect_equal(fits, 0L)
  for (i in 1:2) {
    expect_no_warning(ggplot2::ggplot_build(out))
    expect_equal(callbacks, i)
    expect_equal(fits, i)
  }
  fit <- lm(y ~ x, rows)
  callbacks <- fits <- 0L
  out <- base + geom_b(model = fit)
  expect_equal(callbacks, 0L)
  expect_equal(fits, 0L)
  expect_no_warning(ggplot2::ggplot_build(out))
  expect_equal(callbacks, 1L)
  expect_equal(fits, 0L)
})

test_that("temporary inference follows final inherited and owned mappings", {
  rows <- data.frame(x = 1:10, y = 11 * (1:10))
  base <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) + ggplot2::geom_point()
  out <- base + geom_b(show_b0 = FALSE, run = 1) + ggplot2::aes(y = y * 2)
  rise <- b_test_built_mark(out, "b1")
  expect_equal(rise$yend - rise$y, 22)
  owned <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) +
    ggplot2::geom_point(ggplot2::aes(y = y * 3)) + geom_b(show_b0 = FALSE, run = 1) +
    ggplot2::aes(y = y * 2)
  rise <- b_test_built_mark(owned, "b1")
  expect_equal(rise$yend - rise$y, 33)
  local <- base + geom_b(data = function(data) data[1:6, ], show_b0 = FALSE, run = 1) +
    ggplot2::aes(y = y * 2)
  rise <- b_test_built_mark(local, "b1")
  expect_equal(rise$yend - rise$y, 22)
  local_owned <- base + geom_b(mapping = ggplot2::aes(y = y * 3),
    data = function(data) data[1:6, ], show_b0 = FALSE, run = 1) + ggplot2::aes(y = y * 2)
  rise <- b_test_built_mark(local_owned, "b1")
  expect_equal(rise$yend - rise$y, 33)
})

test_that("observing an identity source preserves its within-panel row order", {
  rows <- data.frame(g = factor(rep(c("b", "a"), 4)), y = 1:8)
  base <- ggplot2::ggplot(rows, ggplot2::aes(g, y)) + ggplot2::geom_point()
  original <- ggplot2::ggplot_build(base)$data[[1L]]
  expect_equal(ggplot2::ggplot_build(base + geom_b())$data[[1L]][names(original)], original)
})

test_that("nested builds cannot replace the outer coefficient frame", {
  rows <- data.frame(x = 1:10, y = 2 * (1:10))
  nested <- FALSE
  inner <- NULL
  errors <- 0L
  stat <- ggplot2::ggproto(NULL, ggplot2::Stat,
    compute_group = function(data, scales) {
      if (!nested) {
        nested <<- TRUE
        on.exit(nested <<- FALSE)
        tryCatch(ggplot2::ggplot_build(inner), error = function(error) {
          errors <<- errors + 1L
        })
      }
      data.frame()
    })
  trigger <- ggplot2::layer(geom = ggplot2::GeomBlank, stat = stat,
                            position = "identity")
  out <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) + ggplot2::geom_point() +
    geom_model() + trigger + geom_b(show_b0 = FALSE, run = 1)
  inner <- replace_plot_data(out, transform(rows, y = y + 100))
  expect_no_warning(rise <- b_test_built_mark(out, "b1"))
  expect_equal(rise$y, 2 * rise$x)
  expect_equal(b_test_built_mark(inner, "b1")$y, rise$y + 100)
  expect_equal(b_test_built_mark(out, "b1")$y, rise$y)
  inner <- inner + ggplot2::geom_point(ggplot2::aes(size = ggplot2::after_stat(stop("nested failure"))))
  expect_no_warning(recovered <- b_test_built_mark(out, "b1"))
  expect_equal(errors, 1L)
  expect_equal(recovered$y, rise$y)
})

test_that("placeholder rows are neutral to facets named x or y", {
  rows <- data.frame(x = rep(1:3, each = 2), y = rep(c(3, 5, 7), each = 2),
                     f = rep(letters[1:3], each = 2))
  fit <- lm(y ~ x, rows)
  for (facet in c("x", "y", "f")) {
    out <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) + ggplot2::geom_point() +
      ggplot2::facet_wrap(stats::reformulate(facet)) +
      geom_b(model = fit, run = 1, run_x = 1, show_b0 = FALSE)
    expect_no_warning(rise <- b_test_built_mark(out, "b1"))
    expect_equal(as.integer(rise$PANEL), 1:3)
    expect_equal(rise$y, rep(3, 3))
    expect_equal(rise$yend, rep(5, 3))
  }
})

test_that("local-source selection hands callbacks to the planner without evaluating them", {
  rows <- data.frame(x = 1:4, y = 2:5)
  calls <- 0L
  callback <- function(data) {
    calls <<- calls + 1L
    data[1:2, ]
  }
  base <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) +
    ggplot2::geom_point(data = callback)
  # Isolate the planning handoff; selection itself must not run the callback.
  testthat::local_mocked_bindings(
    b_layer_spec = function(object, model, args, fn, call, source_is_local = FALSE) {
      expect_equal(calls, 0L)
      selected <- object$layers[[1L]]
      expect_true(is.function(selected$data))
      expect_equal(selected$layer_data(rows), rows[1:2, ])
      expect_equal(calls, 1L)
      list(plot = object, marks = list())
    },
    b_defer_spec = identity
  )
  expect_no_warning(base + geom_b(mapping = ggplot2::aes(x, y)))
  expect_equal(calls, 1L)
})

test_that("deferred placement follows replaced plot rows and local callbacks", {
  withr::local_options(lifecycle_verbosity = "quiet")
  rows <- data.frame(x = 1:10, y = c(3, 3, 6, 7, 8, 10, 12, 13, 15, 19))
  fit <- lm(y ~ x, rows)
  replacement <- transform(rows, x = x + 100, y = y + 50)
  for (model in list(NULL, fit)) {
    for (callback in list(NULL, function(d) d[seq_len(6), ])) {
      base <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) + ggplot2::geom_point()
      original <- base + geom_b(data = callback, model = model, show_b0 = FALSE, run = 1)
      changed <- replace_plot_data(original, replacement)
      early <- replace_plot_data(base, replacement) +
        geom_b(data = callback, model = model, show_b0 = FALSE, run = 1)
      expect_no_warning(actual <- b_test_built_mark(changed, "b1"))
      expect_equal(actual, b_test_built_mark(early, "b1"))
      expect_gt(actual$x, 100)
      used <- if (is.null(callback)) replacement else callback(replacement)
      expected_fit <- model %||% lm(y ~ x, used)
      expect_equal(actual$y, unname(predict(expected_fit, data.frame(x = actual$x))))
      expect_equal(b_test_built_mark(original, "b1")$x, b_test_mark(original, "b1")$data$x)
      expect_equal(b_test_built_mark(replace_plot_data(original, replacement), "b1"), actual)
    }
  }
})

test_that("deferred categorical components accept fewer, more, and reordered levels", {
  withr::local_options(lifecycle_verbosity = "quiet")
  rows <- data.frame(g = factor(rep(c("a", "b"), each = 3)), y = 1:6)
  replacements <- list(
    data.frame(g = factor(rep(c("a", "b", "c", "d"), each = 3)), y = 1:12),
    data.frame(g = factor(rep("a", 3)), y = 1:3),
    data.frame(g = factor(rep(c("b", "a", "c"), each = 3), levels = c("b", "a", "c")), y = 1:9)
  )
  for (callback in list(NULL, function(d) d[d$y > 1, ])) {
    base <- ggplot2::ggplot(rows, ggplot2::aes(g, y)) + ggplot2::geom_point()
    original <- base + geom_b(data = callback, run_x = 1)
    for (replacement in replacements) {
      out <- replace_plot_data(original, replacement)
      expect_no_warning(built <- ggplot2::ggplot_build(out))
      tags <- vapply(out$layers, function(l) attr(l, "coursekata_layer") %||% "", character(1))
      arrows <- do.call(rbind, built$data[grepl("^bk_([0-9]+|more)$", tags)])
      used <- if (is.null(callback)) replacement else callback(replacement)
      means <- tapply(used$y, used$g, mean)
      expect_equal(nrow(arrows), length(means) - 1L)
      if (nrow(arrows)) {
        expect_equal(arrows$y, rep(unname(means[[1L]]), nrow(arrows)))
        expect_equal(arrows$yend, as.numeric(means[-1L]))
      }
      expect_no_warning(ggplot2::ggplotGrob(out))
      overflow <- built$data[[layer_index(out, "bk_more_label")]]
      if (nrow(overflow) > 1L) {
        labels <- ggplot2::layer_grob(out, layer_index(out, "bk_more_label"))[[1L]]
        expect_length(labels$children, nrow(overflow))
        expect_true(all(vapply(labels$children, inherits, logical(1), "text")))
      }
      expect_equal(nrow(b_test_built_mark(original, "bk_2")), 1L)
    }
  }
})

test_that("canonical orientation determines the coefficient outcome axis", {
  rows <- data.frame(x = 1:10, y = c(3, 3, 6, 7, 8, 10, 12, 13, 15, 19))
  base <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) + ggplot2::geom_point()
  for (orientation in c("x", "y")) {
    fit <- if (orientation == "x") lm(y ~ x, rows) else lm(x ~ y, rows)
    out <- base + geom_model(orientation = orientation) + geom_b(show_b0 = FALSE, run = 1)
    for (flip in c(FALSE, TRUE)) {
      plot <- if (flip) out + ggplot2::coord_flip() else out
      rise <- b_test_built_mark(plot, "b1")
      outcome <- if (orientation == "x") "y" else "x"
      predictor <- if (orientation == "x") "x" else "y"
      expect_equal(rise[[paste0(outcome, "end")]] - rise[[outcome]], unname(coef(fit)[[2L]]))
      expect_equal(rise[[outcome]], unname(coef(fit)[[1L]] + coef(fit)[[2L]] * rise[[predictor]]))
      grob <- ggplot2::layer_grob(plot, layer_index(plot, "b1"))[[1L]]
      expect_length(grob$x0, 1L)
      physical <- if (flip) predictor else outcome
      expect_gt(abs(as.numeric(grob[[paste0(physical, "1")]]) - as.numeric(grob[[paste0(physical, "0")]])), 0)
    }
  }
})

test_that("standalone inference carries mapped weights to the canonical model", {
  rows <- data.frame(x = 1:8, y = c(1, 2, 4, 2, 8, 3, 4, 20), w = c(rep(1, 7), 20))
  base <- ggplot2::ggplot(rows, ggplot2::aes(x, y, weight = w)) + ggplot2::geom_point()
  fit <- lm(y ~ x, rows, weights = w)
  for (model in list(NULL, geom_model())) {
    expect_no_warning(out <- base + model + geom_b(show_b0 = FALSE, run = 1))
    expect_no_warning(rise <- b_test_built_mark(out, "b1"))
    expect_equal(rise$yend - rise$y, unname(coef(fit)[[2L]]))
    expect_equal(rise$y, unname(predict(fit, data.frame(x = rise$x))))
  }
})

test_that("explicit placement works on one-row and constant-predictor facets", {
  rows <- data.frame(x = 1:10, y = (1:10) * 2 + 1)
  fit <- lm(y ~ x, rows)
  for (panel in c(FALSE, TRUE)) {
    source <- if (panel) data.frame(x = c(2, 2, 7, 7), y = c(3, 4, 5, 6), g = c("a", "a", "b", "b")) else rows[1, ]
    base <- ggplot2::ggplot(source, ggplot2::aes(x, y)) + ggplot2::geom_point()
    if (panel) base <- base + ggplot2::facet_wrap(~g)
    expect_no_warning(out <- base + geom_b(model = fit, run = 2, run_x = 3, show_b0 = FALSE))
    expect_no_warning(rise <- b_test_built_mark(out, "b1"))
    expect_equal(nrow(rise), if (panel) 2L else 1L)
    expect_equal(rise$x, rep(3, nrow(rise)))
    expect_equal(rise$y, rep(7, nrow(rise)))
    expect_equal(rise$yend, rep(11, nrow(rise)))
  }
})

test_that("unsupported coefficient claims fail with native diagnostics", {
  rows <- data.frame(x = 1:10, z = c(2, 1, 5, 3, 8, 6, 4, 10, 7, 9), y = 1:10)
  base <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) + ggplot2::geom_point()
  expect_error(base + geom_b(model = lm(y ~ x:z, rows)), "geom_b.*one predictor")
  expect_error(base + geom_b(model = lm(y ~ x + offset(z), rows)), "geom_b.*offset")
  expect_error(base + geom_model(position = ggplot2::position_nudge(y = 10)) + geom_b(), "geom_b.*identity model position")
  expect_error(ggplot2::ggplotGrob(base + geom_b() + ggplot2::coord_polar()), "geom_b.*Cartesian")
})

test_that("the native coefficient constructor and gf doors share every mark", {
  models <- list(lm(Thumb ~ Height, Fingers), lm(Thumb ~ Sex, Fingers),
                 lm(Thumb ~ 1, Fingers))
  mappings <- list(ggplot2::aes(Height, Thumb), ggplot2::aes(Sex, Thumb),
                   ggplot2::aes(Height, Thumb))
  for (i in seq_along(models)) {
    base <- ggplot2::ggplot(Fingers, mappings[[i]]) + ggplot2::geom_point()
    native <- base + geom_b(model = models[[i]])
    for (door in list(gf_b, gf_coef)) {
      translated <- door(base, models[[i]])
      left <- ggplot2::ggplot_build(native)$data
      right <- ggplot2::ggplot_build(translated)$data
      expect_equal(left, right)
      for (j in seq_along(left)[-1L]) {
        # Compare actual segment endpoints / text positions; a segments grob
        # has no $x member, so checking it would be vacuous.
        a <- ggplot2::layer_grob(native, j)[[1L]]
        b <- ggplot2::layer_grob(translated, j)[[1L]]
        columns <- if (inherits(a, "segments")) c("x0", "y0", "x1", "y1") else c("x", "y")
        if (inherits(a, c("segments", "text", "points"))) {
          expect_true(length(a[[columns[[1L]]]]) > 0)
          for (column in columns) {
            convert <- if (startsWith(column, "x")) grid::convertX else grid::convertY
            expect_equal(convert(a[[column]], "npc", valueOnly = TRUE),
                         convert(b[[column]], "npc", valueOnly = TRUE))
          }
          expect_equal(a$gp, b$gp)
        }
      }
    }
  }
})

test_that("inferred triangles attach to the censored fit rather than whole-data coefficients", {
  for (limits in list(c(60, 72), c(55, 68))) {
    base <- gf_point(Thumb ~ Height, data = Fingers) |>
      gf_lims(x = limits) |> gf_model()
    out <- suppressWarnings(base |> gf_b(show_b0 = FALSE))
    built <- suppressWarnings(ggplot2::ggplot_build(out))
    line <- built$data[[layer_index(out, "model")]]
    rise <- built$data[[layer_index(out, "b1")]]
    run <- built$data[[layer_index(out, "run")]]
    kept <- subset(Fingers, Height >= limits[[1L]] & Height <= limits[[2L]])
    fit <- lm(Thumb ~ Height, kept)
    expect_true(all(is.finite(unlist(rise[c("x", "y", "xend", "yend")]))))
    expect_equal(rise$y, unname(predict(fit, data.frame(Height = rise$x))))
    expect_equal(run$y, unname(predict(fit, data.frame(Height = run$xend))))
    expect_equal(rise$yend - rise$y, unname(coef(fit)[[2L]]) * (run$xend - run$x))
    expect_equal(line$y, unname(predict(fit, data.frame(Height = line$x))))
    rise_grob <- suppressWarnings(ggplot2::layer_grob(out, layer_index(out, "b1")))[[1L]]
    run_grob <- suppressWarnings(ggplot2::layer_grob(out, layer_index(out, "run")))[[1L]]
    expect_length(rise_grob$y0, 1L)
    expect_equal(as.numeric(rise_grob$y0),
                 as.numeric(run_grob$y0) -
                 (rise$yend - rise$y) /
                   diff(built$layout$panel_params[[1L]]$y.range))
  }
})

test_that("transformed inferred coordinates are inverted exactly once", {
  base <- gf_point(Thumb ~ Height, data = Fingers) + ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10()
  out <- base |> gf_model() |> gf_b(show_b0 = FALSE, run = 0.02)
  fit <- lm(log10(Thumb) ~ log10(Height), Fingers)
  built <- ggplot2::ggplot_build(out)
  rise <- built$data[[layer_index(out, "b1")]]
  run <- built$data[[layer_index(out, "run")]]
  expect_equal(run$xend - run$x, 0.02)
  expect_equal(rise$y, unname(coef(fit)[[1L]] + coef(fit)[[2L]] * rise$x))
  expect_equal(rise$yend - rise$y, unname(coef(fit)[[2L]]) * 0.02)
  expect_equal(b_test_mark(out, "run_label")$data$label, "0.02")
  expect_equal(ggplot2::ggplot_build(out + ggplot2::coord_flip())$data, built$data)
})

test_that("source callbacks and pinned mappings keep row alignment and RNG state", {
  set.seed(23)
  rows <- Fingers
  rows$Height[c(3, 11)] <- NA_real_
  base <- ggplot2::ggplot(rows, ggplot2::aes(log(Height), shuffle(Thumb))) +
    ggplot2::geom_point(data = function(d) d[seq_len(60), ])
  seed <- .Random.seed
  out <- suppressWarnings(base + geom_b(show_b0 = FALSE, run = 0.02))
  expect_identical(.Random.seed, seed)
  source <- ggplot2::ggplot_build(out)$data[[1L]]
  x <- source$x
  y <- source$y
  fit <- lm(y ~ x)
  rise <- b_test_mark(out, "b1")$data
  run <- b_test_mark(out, "run")$data
  expect_equal(nrow(source), 60L)
  expect_equal(rise$y, unname(coef(fit)[[1L]] + coef(fit)[[2L]] * rise$x))
  expect_equal(rise$yend - rise$y, unname(coef(fit)[[2L]]) * (run$xend - run$x))
  expect_null(attr(base, "coursekata_pins"))
  expect_null(base$layers[[1L]]$mapping)
})

test_that("explicit claims retain coefficients through model metadata and layer copies", {
  fit <- lm(Thumb ~ Height, subset(Fingers, Sex == "male"))
  base <- gf_point(Thumb ~ Height, data = Fingers)
  for (model_plot in list(base + geom_model(model = fit), gf_model(base, fit))) {
    layer <- model_plot$layers[[layer_index(model_plot, "model")]]
    expect_identical(model_layer_fit(layer), fit)
    expect_identical(model_layer_fit(layer_with(layer)), fit)
    expect_identical(model_layer_fit(tag_layer(layer, "another")), fit)
    out <- model_plot |> gf_b(run = 2)
    expect_equal(b_test_mark(out, "b0")$data$y, unname(coef(fit)[[1L]]))
    rise <- b_test_mark(out, "b1")$data
    expect_equal(rise$yend - rise$y, unname(coef(fit)[[2L]]) * 2)
  }
  weighted <- lm(Thumb ~ Height, Fingers, weights = seq_len(nrow(Fingers)))
  out <- base + geom_b(model = weighted, run = 2)
  rise <- b_test_mark(out, "b1")$data
  expect_equal(rise$yend - rise$y, unname(coef(weighted)[[2L]]) * 2)
  for (model_plot in list(base + geom_model(model = weighted), gf_model(base, weighted))) {
    expect_identical(
      model_layer_fit(model_plot$layers[[layer_index(model_plot, "model")]]),
      weighted
    )
    out <- model_plot + geom_b(run = 2)
    rise <- b_test_mark(out, "b1")$data
    expect_equal(rise$yend - rise$y, unname(coef(weighted)[[2L]]) * 2)
  }
})

test_that("native diagnostics name the public constructor and reject ambiguous claims", {
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) + ggplot2::geom_point()
  expect_error(base + geom_b(model = Thumb ~ Height), "geom_b.*annotates a model that has been fit")
  expect_error(base + geom_b(model = lm(Thumb ~ Height - 1, Fingers)), "starting from b0")
  expect_error(base + geom_b(model = lm(Thumb ~ Height, Fingers), color = ~Sex), "not a mapping")
  expect_error(base + ggplot2::coord_polar() + geom_b(), "cartesian")
  expect_error(base + geom_model() + geom_model() + geom_b(), "one model")
  expect_error(base + geom_model(formula = y ~ poly(x, 2)) + geom_b(), "linear model")
  transformed <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, log(Thumb))) +
    ggplot2::geom_point()
  expect_error(ggplot2::ggplot_build(transformed + geom_b(model = lm(Thumb ~ Height, Fingers))), "outcome")
  expect_error(base + ggplot2::facet_wrap(~Sex) + geom_b(), "faceted plot")
  expect_error(ggplot2::ggplot_build(base + geom_b() + ggplot2::facet_wrap(~Sex)), "geom_b.*faceted plot")
  expect_no_error(base + ggplot2::scale_x_continuous(limits = c(1, 2)) +
                    geom_b(model = lm(Thumb ~ Height, Fingers)))
})

test_that("one annotation object can be reused without changing its source plots", {
  layer <- geom_b(show_b0 = FALSE)
  first <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) + ggplot2::geom_point()
  second <- ggplot2::ggplot(Fingers, ggplot2::aes(Weight, Thumb)) + ggplot2::geom_point()
  a <- first + layer
  b <- second + layer
  expect_false(identical(b_test_mark(a, "b1")$data, b_test_mark(b, "b1")$data))
  expect_length(first$layers, 1L)
  expect_length(second$layers, 1L)
  expect_null(layer$model)
})

test_that("categorical arrows follow coefficient levels through reordered scales", {
  rows <- data.frame(g = factor(rep(c("a", "b", "c"), each = 3)),
                     y = c(2, 3, 4, 7, 8, 9, 11, 12, 13))
  fit <- lm(y ~ g, rows)
  base <- ggplot2::ggplot(rows, ggplot2::aes(g, y)) + ggplot2::geom_point() +
    ggplot2::scale_x_discrete(limits = c("c", "a", "b"))
  for (out in list(base + geom_b(model = fit), base + geom_b())) {
    expect_equal(b_test_mark(out, "bk_2")$data$x, 3 - 0.18)
    expect_equal(b_test_mark(out, "bk_3")$data$x, 1 - 0.18)
    expect_equal(b_test_mark(out, "b0")$data$y, 3)
    expect_equal(b_test_mark(out, "bk_2")$data$yend, 8)
    expect_equal(b_test_mark(out, "bk_3")$data$yend, 12)
  }
})

test_that("late scales and limits replan coefficients and reset on every build", {
  base <- gf_point(Thumb ~ Height, data = Fingers) |> gf_model()
  annotated <- base |> gf_b(show_b0 = FALSE)
  original_layer <- base$layers[[layer_index(base, "model")]]
  for (change in list(ggplot2::scale_x_continuous(limits = c(55, 68)),
                      ggplot2::scale_x_log10(), ggplot2::scale_x_reverse())) {
    early <- suppressWarnings(gf_b(base + change, show_b0 = FALSE))
    late <- annotated + change
    a <- suppressWarnings(ggplot2::ggplot_build(early))
    b <- suppressWarnings(ggplot2::ggplot_build(late))
    for (tag in c("b1", "run", "b1_label", "run_label")) {
      expect_equal(a$data[[layer_index(early, tag)]], b$data[[layer_index(late, tag)]])
    }
    # A build of a derived plot must not leave transformed/censored rows in
    # the shared plan when the original is built again.
    repeated <- ggplot2::ggplot_build(annotated)
    expect_equal(repeated$data[[layer_index(annotated, "b1")]]$y,
                 b_test_mark(annotated, "b1")$data$y)
  }
  expect_identical(original_layer$stat, StatModel)
  expect_identical(base$layers[[layer_index(base, "model")]], original_layer)
  with_intercept <- base |> gf_b()
  expect_no_warning(transformed <- ggplot2::ggplot_build(with_intercept + ggplot2::scale_x_log10()))
  expect_equal(transformed$data[[layer_index(with_intercept, "b0")]]$x, 0)
})

test_that("a supplied fit stays fixed while late limits move automatic placement", {
  fit <- lm(Thumb ~ Height, Fingers)
  base <- gf_point(Thumb ~ Height, data = Fingers)
  late <- (base + geom_b(model = fit, show_b0 = FALSE)) |>
    gf_lims(x = c(55, 68))
  early <- (base |> gf_lims(x = c(55, 68))) + geom_b(model = fit, show_b0 = FALSE)
  a <- ggplot2::ggplot_build(late)$data[[layer_index(late, "b1")]]
  b <- ggplot2::ggplot_build(early)$data[[layer_index(early, "b1")]]
  expect_equal(a, b)
  expect_true(is.finite(a$x))
  expect_equal(a$y, unname(predict(fit, data.frame(Height = a$x))))
})

test_that("coefficient components never refit the canonical model during a build", {
  out <- gf_point(Thumb ~ Height, data = Fingers) |> gf_model() |> gf_b()
  calls <- 0L
  # Trace the real function so lm's data-masked weights expression keeps its
  # ordinary evaluation environment; a forwarding mock changes that contract.
  suppressMessages(trace("lm", where = asNamespace("stats"), print = FALSE,
                         tracer = function() calls <<- calls + 1L))
  withr::defer(suppressMessages(untrace("lm", where = asNamespace("stats"))))
  expect_no_warning(built <- ggplot2::ggplot_build(out))
  expect_equal(calls, 1L)
  expect_equal(nrow(built$data[[layer_index(out, "model")]]), 80L)
  expect_equal(nrow(built$data[[layer_index(out, "b1")]]), 1L)
  expect_no_warning(ggplot2::ggplot_build(out))
  expect_equal(calls, 2L)
})

test_that("explicit coefficients are transformed for drawing without changing their claim", {
  fit <- lm(Thumb ~ Height, Fingers)
  out <- (gf_point(Thumb ~ Height, data = Fingers) |>
    gf_b(fit, run = 5, run_x = 60, show_b0 = FALSE)) +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10()
  built <- ggplot2::ggplot_build(out)
  rise <- built$data[[layer_index(out, "b1")]]
  run <- built$data[[layer_index(out, "run")]]
  predictions <- unname(predict(fit, data.frame(Height = c(60, 65))))
  expect_equal(rise$x, log10(60))
  expect_equal(c(rise$y, rise$yend), log10(predictions))
  expect_equal(run$xend, log10(65))
  expect_equal(built$data[[layer_index(out, "run_label")]]$label, "5")
})

test_that("deferred categorical roles survive levels restored by a later scale", {
  rows <- data.frame(g = factor(rep(c("a", "b", "c"), each = 3)),
                     y = c(2, 3, 4, 7, 8, 9, 11, 12, 13))
  base <- ggplot2::ggplot(rows, ggplot2::aes(g, y)) + ggplot2::geom_point() +
    geom_model()
  complete <- base + ggplot2::scale_x_discrete(limits = c("a", "b", "c")) + geom_b()
  expected <- ggplot2::ggplot_build(complete)
  for (visible in list(c("a", "b"), "a", c("b", "c"))) {
    partial <- suppressWarnings(base + ggplot2::scale_x_discrete(limits = visible) + geom_b())
    restored <- suppressMessages(partial + ggplot2::scale_x_discrete(limits = c("a", "b", "c")))
    built <- ggplot2::ggplot_build(restored)
    for (tag in c("b0", "bk_2", "bk_2_label", "bk_3", "bk_3_label")) {
      expect_false(is.na(layer_index(restored, tag)))
      actual <- built$data[[layer_index(restored, tag)]]
      expect_equal(nrow(actual), 1L)
      expect_equal(actual, expected$data[[layer_index(complete, tag)]])
    }
    suppressed <- suppressWarnings(ggplot2::ggplot_build(partial))
    expect_equal(nrow(suppressed$data[[layer_index(partial, "bk_3")]]), 0L)
    expect_equal(ggplot2::ggplot_build(restored)$data, built$data)
  }
})
