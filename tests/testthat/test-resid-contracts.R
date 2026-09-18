resid_contract_cases <- function() {
  list(
    list(geom_resid, gf_resid, FALSE, FALSE),
    list(geom_square_resid, gf_square_resid, FALSE, TRUE),
    list(geom_reduce, gf_reduce, TRUE, FALSE),
    list(geom_square_reduce, gf_square_reduce, TRUE, TRUE),
    list(stat_resid, gf_resid, FALSE, FALSE),
    list(function(...) stat_resid(..., geom = "square_resid"), gf_square_resid, FALSE, TRUE),
    list(stat_reduce, gf_reduce, TRUE, FALSE),
    list(function(...) stat_reduce(..., geom = "square_resid"), gf_square_reduce, TRUE, TRUE)
  )
}

resid_contract_data <- function() {
  data.frame(x = 1:6, y = c(3, 5, 4, 8, 9, 12), z = c(0, 1, 0, 1, 0, 1),
             other = 101:106, group = factor(rep(c("a", "b"), 3)))
}

test_that("every residual constructor shares arithmetic and real rendered endpoints", {
  d <- resid_contract_data()
  fit <- lm(y ~ x, d)
  slope <- sum((d$x - mean(d$x)) * (d$y - mean(d$y))) / sum((d$x - mean(d$x))^2)
  fitted <- mean(d$y) + slope * (d$x - mean(d$x))
  grand <- mean(d$y)
  expect_equal(sum((d$y - fitted)^2) + sum((fitted - grand)^2), sum((d$y - grand)^2))

  for (horizontal in c(FALSE, TRUE)) {
    mapping <- if (horizontal) ggplot2::aes(y, x) else ggplot2::aes(x, y)
    base <- ggplot2::ggplot(d, mapping) + ggplot2::geom_point()
    for (case in resid_contract_cases()) {
      native <- base + case[[1]](model = fit)
      formula <- suppressMessages(case[[2]](base, fit))
      a <- ggplot2::ggplot_build(native)$data[[2]]
      b <- ggplot2::ggplot_build(formula)$data[[2]]
      expect_equal(native$layers[[2]]$stat_params$orientation, if (horizontal) "y" else "x")
      axis <- if (horizontal) "x" else "y"
      expect_equal(a[[paste0(axis, "end")]], fitted)
      expect_equal(a[[axis]], if (case[[3]]) rep(grand, nrow(d)) else d$y)
      expect_equal(a[sort(names(a))], b[sort(names(b))])
      for (flip in c(FALSE, TRUE)) {
        left <- if (flip) native + ggplot2::coord_flip() else native
        right <- if (flip) formula + ggplot2::coord_flip() else formula
        ga <- ggplot2::layer_grob(left, 2)[[1]]
        gb <- ggplot2::layer_grob(right, 2)[[1]]
        columns <- if (case[[4]]) c("x", "y") else c("x0", "y0", "x1", "y1")
        for (column in columns) {
          expect_length(ga[[column]], nrow(d) * if (case[[4]]) 4L else 1L)
          expect_equal(as.numeric(ga[[column]]), as.numeric(gb[[column]]))
        }
      }
    }
  }
})

test_that("native outcome guards use current local and inherited mappings", {
  d <- resid_contract_data()
  fit <- lm(y ~ x, d)
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point()
  for (case in resid_contract_cases()) {
    make <- case[[1]]
    expect_error(ggplot2::ggplot_build(base + make(ggplot2::aes(y = other), model = fit)),
                 "axis carrying the model's outcome")
    expect_error(ggplot2::ggplot_build(base + make(ggplot2::aes(y = log(y)), model = fit)),
                 "axis carrying the model's outcome")
    expect_error(ggplot2::ggplot_build(base + make(model = fit, orientation = "y")),
                 "orientation.*mapped axis")
    layer <- make(model = fit)
    plot <- base + layer
    expect_no_error(ggplot2::ggplot_build(plot))
    expect_error(ggplot2::ggplot_build(plot + ggplot2::aes(y = other)),
                 "axis carrying the model's outcome")
    flipped <- plot + ggplot2::aes(y, x)
    expect_equal(ggplot2::ggplot_build(flipped)$data[[2]]$xend, unname(predict(fit, d)))
    new <- transform(d, x = x + 10, y = y + 20)
    expect_equal(ggplot2::ggplot_build(plot + new)$data[[2]]$yend,
                 unname(predict(fit, new)))
    expect_null(layer$computed_mapping)
    expect_null(base$layers[[1]]$mapping$y)
  }
})

test_that("all interfaces diagnose compound-invalid reductions in the same order", {
  d <- resid_contract_data()
  fit <- lm(y ~ x + z - 1, d)
  small <- d[setdiff(names(d), "z")]
  for (case in resid_contract_cases()) {
    native <- function(p, m) ggplot2::ggplot_build(p + case[[1]](model = m))
    formula <- function(p, m) suppressMessages(case[[2]](p, m))
    for (run in list(native, formula)) {
      expect_error(run(ggplot2::ggplot(small, ggplot2::aes(x)), fit), "needs both an x and a y")
      expect_error(run(ggplot2::ggplot(small, ggplot2::aes(x, other)), fit), "missing from the plot's data: z")
      expect_error(run(ggplot2::ggplot(d, ggplot2::aes(x, other)), fit), "axis carrying the model's outcome")
      if (case[[3]]) {
        expect_error(run(ggplot2::ggplot(d, ggplot2::aes(x, y)), fit), "arithmetic does not support")
        frame_missing <- lm(y ~ x, d, model = FALSE)
        expect_no_warning(expect_error(run(ggplot2::ggplot(d, ggplot2::aes(x, other)), frame_missing),
                                      "axis carrying the model's outcome"))
        expect_no_warning(expect_error(run(ggplot2::ggplot(d, ggplot2::aes(x, y)), frame_missing),
                                      "refit with `model = TRUE`"))
      }
    }
    for (position in list("jitter", ggplot2::position_jitter(), ggplot2::position_jitterdodge())) {
      expect_error(case[[1]](model = fit, data = small, position = position),
                   "fixed jitter seed|do not support")
    }
  }
})

test_that("predictor and outcome missingness retain row identity until drawing", {
  complete <- resid_contract_data()
  fit <- lm(y ~ x + z, complete)
  d <- complete
  d$z[c(2, 5)] <- NA_real_
  d$y[4] <- NA_real_
  expected <- unname(predict(fit, d))
  for (horizontal in c(FALSE, TRUE)) {
    mapping <- if (horizontal) ggplot2::aes(y, x) else ggplot2::aes(x, y)
    base <- ggplot2::ggplot(d, mapping)
    for (case in resid_contract_cases()) {
      # A reduction's observed outcome is the grand mean, so only predictions
      # missing from its off-axis predictor are omitted at draw time.
      dropped <- if (case[[3]]) 2L else 3L
      for (native in c(TRUE, FALSE)) {
        plot <- if (native) base + case[[1]](model = fit) else suppressMessages(case[[2]](base, fit))
        drawn <- ggplot2::ggplot_build(plot)$data[[1]]
        expect_equal(drawn[[if (horizontal) "xend" else "yend"]], expected)
        expect_equal(which(is.na(expected)), c(2L, 5L))
        expect_warning(grob <- ggplot2::layer_grob(plot, 1)[[1]], paste("Removed", dropped, "rows"))
        expect_length(grob[[if (case[[4]]) "x" else "x0"]],
                      (nrow(d) - dropped) * if (case[[4]]) 4L else 1L)
        quiet <- if (native) base + case[[1]](model = fit, na.rm = TRUE) else {
          suppressMessages(case[[2]](base, fit, na.rm = TRUE))
        }
        expect_no_warning(ggplot2::layer_grob(quiet, 1))
      }
    }
  }
})

test_that("exported residual stats enforce endpoints even with another geom", {
  d <- resid_contract_data()
  for (stat in list(StatResid, StatReduce)) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
      ggplot2::layer(stat = stat, geom = "point", position = "identity")
    expect_error(ggplot2::ggplot_build(p), "missing aesthetics.*xend or yend")
  }
})

test_that("native function residuals use the mapped predictor in both orientations", {
  d <- resid_contract_data()
  shift <- 3
  fun <- function(x) x^2 + shift
  for (horizontal in c(FALSE, TRUE)) {
    mapping <- if (horizontal) ggplot2::aes(y, log(x)) else ggplot2::aes(log(x), y)
    base <- ggplot2::ggplot(d, mapping)
    for (make in list(geom_resid, geom_square_resid, stat_resid)) {
      p <- base + make(fun = fun, orientation = if (horizontal) "y" else "x")
      drawn <- ggplot2::ggplot_build(p)$data[[1]]
      expect_equal(drawn[[if (horizontal) "xend" else "yend"]], log(d$x)^2 + shift)
      expect_no_error(ggplot2::layer_grob(p, 1))
      expect_error(make(fun = fun, model = lm(y ~ x, d)), "only one")
    }
  }
  for (make in list(geom_reduce, geom_square_reduce, stat_reduce)) {
    expect_error(make(fun = fun), "fun.*only defines residuals")
  }
})

test_that("random expressions stay aligned without freezing native data or caller bindings", {
  d <- resid_contract_data()
  shift <- 1
  base <- ggplot2::ggplot(d, ggplot2::aes(sample(x) + shift, y)) + ggplot2::geom_point()
  original <- base$mapping
  set.seed(419)
  seed <- .Random.seed
  native <- base + geom_resid(fun = function(x) x^2)
  formula <- suppressMessages(gf_resid_fun(base, function(x) x^2))
  expect_identical(.Random.seed, seed)
  expect_identical(base$mapping, original)
  for (p in list(native, formula)) {
    built <- ggplot2::ggplot_build(p)$data
    expect_equal(built[[2]]$x, built[[1]]$x)
    expect_equal(built[[2]]$yend, built[[1]]$x^2)
    expect_identical(ggplot2::ggplot_build(p)$data, built)
    expect_identical(.Random.seed, seed)
  }
  shift <- 10
  new <- transform(d, x = x + 20)
  built <- ggplot2::ggplot_build(native + new)$data
  expect_equal(sort(built[[1]]$x), new$x + shift)
  expect_equal(built[[2]]$yend, built[[1]]$x^2)
})

test_that("later random mappings are refused with a supported ordering", {
  d <- resid_contract_data()
  helper <- function(x) sample(x)
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point()
  set.seed(98)
  before <- .Random.seed
  for (fun in list(geom_resid, geom_square_resid, stat_resid)) {
    plot <- base + fun(fun = function(x) x^2) + ggplot2::aes(x = helper(x))
    expect_error(ggplot2::ggplot_build(plot), "Add this mapping.*before adding the residual")
    expect_identical(.Random.seed, before)
    supported <- base + ggplot2::aes(x = helper(x)) + fun(fun = function(x) x^2)
    built <- ggplot2::ggplot_build(supported)$data
    expect_equal(built[[2]]$x, built[[1]]$x)
    expect_equal(built[[2]]$yend, built[[1]]$x^2)
  }
  fixed <- base + geom_resid(fun = identity) +
    ggplot2::aes(x = withr::with_seed(5, sample(x)))
  built <- ggplot2::ggplot_build(fixed)$data
  expect_equal(built[[2]]$yend, built[[1]]$x)
})

test_that("native and formula squares deliberately differ only in inherited styling", {
  d <- resid_contract_data()
  fit <- lm(y ~ x, d)
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = group)) + ggplot2::geom_point()
  for (pair in list(list(geom_square_resid, gf_square_resid), list(geom_square_reduce, gf_square_reduce))) {
    a <- ggplot2::ggplot_build(base + pair[[1]](model = fit))$data[[2]]
    b <- ggplot2::ggplot_build(suppressMessages(pair[[2]](base, fit)))$data[[2]]
    expect_length(unique(a$colour), 2)
    expect_true(all(is.na(b$colour)))
    expect_equal(a[c("x", "y", "yend")], b[c("x", "y", "yend")])
    matched <- ggplot2::ggplot_build(suppressMessages(pair[[2]](base, fit, inherit = TRUE)))$data[[2]]
    expect_equal(a[sort(names(a))], matched[sort(names(matched))])
  }
})

test_that("square proportions use unflipped data ranges before coordinate transformation", {
  d <- resid_contract_data()
  d$y <- d$y * 10
  fit <- lm(y ~ x, d)
  for (horizontal in c(FALSE, TRUE)) {
    mapping <- if (horizontal) ggplot2::aes(y, x) else ggplot2::aes(x, y)
    for (flip in c(FALSE, TRUE)) {
      for (make in list(geom_square_resid, geom_square_reduce)) {
        plot <- ggplot2::ggplot(d, mapping) + make(model = fit, aspect = 2 / 3)
        if (flip) plot <- plot + ggplot2::coord_flip()
        grob <- ggplot2::layer_grob(plot, 1)[[1]]
        x <- matrix(as.numeric(grob$x), nrow = 4)
        y <- matrix(as.numeric(grob$y), nrow = 4)
        ratio <- apply(x, 2, function(v) diff(range(v))) /
          apply(y, 2, function(v) diff(range(v)))
        expect_equal(ratio, rep(if (xor(horizontal, flip)) 3 / 2 else 2 / 3, nrow(d)))
      }
    }
  }
})

test_that("segment arrows end on observations and reductions end on the grand mean", {
  d <- resid_contract_data()
  fit <- lm(y ~ x, d)
  for (make in list(geom_resid, geom_reduce)) {
    plot <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
      make(model = fit, arrow = grid::arrow())
    built <- ggplot2::ggplot_build(plot)
    expected <- data.frame(x = d$x, y = unname(predict(fit, d)))
    start <- built$layout$coord$transform(expected, built$layout$panel_params[[1]])
    expected$y <- if (identical(make, geom_resid)) d$y else mean(d$y)
    end <- built$layout$coord$transform(expected, built$layout$panel_params[[1]])
    grob <- ggplot2::layer_grob(plot, 1)[[1]]
    expect_equal(as.numeric(grob$y0), start$y)
    expect_equal(as.numeric(grob$y1), end$y)
    expect_equal(grob$arrow$ends, 2L)
  }
})

test_that("offset reductions are refused while residuals retain their predictions", {
  d <- resid_contract_data()
  d$z <- c(2, 10, -3, 8, -5, 7)
  for (fit in list(lm(y ~ x + offset(z), d), lm(y ~ x, d, offset = z))) {
    predictions <- unname(predict(fit, d))
    total <- sum((d$y - mean(d$y))^2)
    pieces <- sum((d$y - predictions)^2) + sum((predictions - mean(d$y))^2)
    expect_gt(abs(total - pieces), 1)
    for (case in resid_contract_cases()) {
      base <- ggplot2::ggplot(d, ggplot2::aes(x, y))
      if (case[[3]]) {
        expect_error(ggplot2::ggplot_build(base + case[[1]](model = fit)), "fit with an offset")
        expect_error(case[[2]](base, fit), "fit with an offset")
        wrong <- base + ggplot2::aes(y = other)
        expect_error(ggplot2::ggplot_build(wrong + case[[1]](model = fit)), "axis carrying the model's outcome")
        expect_error(case[[2]](wrong, fit), "axis carrying the model's outcome")
        missing <- ggplot2::ggplot(d[setdiff(names(d), "z")], ggplot2::aes(x))
        expect_error(ggplot2::ggplot_build(missing + case[[1]](model = fit)), "needs both an x and a y")
      } else {
        expect_equal(ggplot2::ggplot_build(base + case[[1]](model = fit))$data[[1]]$yend, predictions)
        expect_equal(ggplot2::ggplot_build(case[[2]](base, fit))$data[[1]]$yend, predictions)
      }
    }
  }
})

test_that("mapping stabilization preserves live derived labels and explicit labels", {
  d <- resid_contract_data()
  base <- ggplot2::ggplot(d, ggplot2::aes(log(x), y)) + ggplot2::geom_point()
  plot <- base + geom_resid(model = lm(y ~ x, d))
  expect_equal(ggplot2::get_labs(plot)$x, "log(x)")
  expect_equal(ggplot2::get_labs(plot + ggplot2::aes(x = sqrt(x)))$x, "sqrt(x)")
  chained <- plot + ggplot2::aes(x = sqrt(x)) + geom_reduce(model = lm(y ~ x, d))
  expect_equal(ggplot2::get_labs(chained)$x, "sqrt(x)")
  expect_equal(ggplot2::get_labs(chained + ggplot2::aes(x = x^2))$x, "x^2")
  inferred <- gf_model(plot)
  expect_equal(ggplot2::get_labs(inferred)$x, "log(x)")
  expect_equal(plot_spec(inferred)$labels[["x"]], "log(x)")
  expect_no_error(ggplot2::ggplot_build(inferred + geom_reduce(model = lm(y ~ x, d))))
  labelled <- base + ggplot2::labs(x = "My predictor") + geom_resid(model = lm(y ~ x, d))
  expect_equal(ggplot2::get_labs(labelled + ggplot2::aes(x = sqrt(x)))$x, "My predictor")
  expect_null(base$labels$x)
})

test_that("the live mapping check measures reproducibility rather than RNG state mutation", {
  d <- resid_contract_data()
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = runif(length(x)))) + ggplot2::geom_point()
  set.seed(918)
  before <- .Random.seed
  late <- base + geom_resid(fun = identity) +
    ggplot2::aes(x = withr::with_preserve_seed(sample(x)))
  expect_error(ggplot2::ggplot_build(late), "cannot share the random `x` mapping")
  expect_identical(.Random.seed, before)
  supported <- base + ggplot2::aes(x = withr::with_preserve_seed(sample(x))) + geom_resid(fun = identity)
  built <- ggplot2::ggplot_build(supported)$data
  expect_equal(built[[2]]$yend, built[[1]]$x)

  own_seed <- function(x) { set.seed(5); sample(x) }
  mapping <- ggplot2::aes(x = own_seed(x), y = y)
  set.seed(918)
  expect_no_error(check_resid_live_mappings(mapping, d))
  expect_identical(.Random.seed, before)
  self_seeded <- base + geom_resid(fun = identity) + mapping
  built <- ggplot2::ggplot_build(self_seeded)$data
  expect_equal(built[[2]]$yend, built[[1]]$x)

  bad_model <- lm(y ~ x + z - 1, d)
  missing <- ggplot2::ggplot(d[setdiff(names(d), "z")], ggplot2::aes(x, y)) +
    geom_reduce(model = bad_model) + ggplot2::aes(x = sample(x))
  expect_error(ggplot2::ggplot_build(missing), "missing from the plot's data: z")
})

test_that("ordinary inherited base transformations bypass reproducibility probes", {
  evaluations <- 0L
  caller <- new.env(parent = baseenv())
  makeActiveBinding("x", function() {
    evaluations <<- evaluations + 1L
    1:3
  }, caller)
  mapping <- list(x = rlang::new_quosure(quote(sqrt(x)), caller))
  expect_true(resid_deterministic_expr(quote(sqrt(x)), caller))
  expect_no_error(check_resid_live_mappings(mapping, data.frame()))
  expect_identical(evaluations, 0L)

  caller$sqrt <- function(x) base::sqrt(x)
  expect_false(resid_deterministic_expr(quote(sqrt(x)), caller))
  expect_no_error(check_resid_live_mappings(mapping, data.frame()))
  expect_identical(evaluations, 8L)
})

test_that("colliding two-row samples cannot pass the live mapping guard", {
  d <- data.frame(x = 1:2, y = c(2, 5))
  # The first two value probes collide; their different final RNG states must
  # still identify this expression as dependent on the ambient stream.
  expect_identical(with_fixed_seed(1L, sample(d$x)), with_fixed_seed(2L, sample(d$x)))
  base <- ggplot2::ggplot(d, ggplot2::aes(x, y, colour = runif(length(x)))) +
    ggplot2::geom_point()
  set.seed(813)
  before <- .Random.seed
  for (mapping in list(ggplot2::aes(x = sample(x)),
                      ggplot2::aes(x = withr::with_preserve_seed(sample(x))))) {
    plot <- base + geom_resid(fun = identity) + mapping
    expect_error(ggplot2::ggplot_build(plot), "cannot share the random `x` mapping")
    expect_identical(.Random.seed, before)
    supported <- base + mapping + geom_resid(fun = identity)
    built <- ggplot2::ggplot_build(supported)$data
    expect_equal(built[[2]]$x, built[[1]]$x)
    expect_equal(built[[2]]$yend, built[[1]]$x)
    set.seed(813)
  }
  own_seed <- function(x) { set.seed(5); sample(x) }
  expect_no_error(check_resid_live_mappings(ggplot2::aes(x = own_seed(x)), d))
  expect_identical(.Random.seed, before)
  expect_no_error(check_resid_live_mappings(ggplot2::aes(x = withr::with_seed(5, sample(x))), d))
  expect_identical(.Random.seed, before)
  deterministic <- function(x) rev(x)
  expect_no_error(check_resid_live_mappings(ggplot2::aes(x = deterministic(x)), d))
  expect_identical(.Random.seed, before)
})

test_that("an explicit orientation disambiguates an outcome mapped to both axes", {
  d <- resid_contract_data()
  fit <- lm(y ~ x, d)
  base <- ggplot2::ggplot(d, ggplot2::aes(y, y))
  for (case in resid_contract_cases()) {
    for (direction in c("x", "y")) {
      plot <- base + case[[1]](model = fit, orientation = direction)
      drawn <- ggplot2::ggplot_build(plot)$data[[1]]
      end <- if (direction == "y") "xend" else "yend"
      expect_equal(drawn[[end]], unname(predict(fit, d)))
      expect_null(drawn[[if (direction == "y") "yend" else "xend"]])
    }
  }
})

test_that("empty source frames draw no residuals or reductions", {
  d <- resid_contract_data()
  fit <- lm(y ~ x, d)
  base <- ggplot2::ggplot(d[FALSE, ], ggplot2::aes(x, y))
  for (case in resid_contract_cases()) {
    native <- base + case[[1]](model = fit)
    formula <- suppressMessages(case[[2]](base, fit))
    expect_equal(nrow(ggplot2::ggplot_build(native)$data[[1]]), 0L)
    expect_equal(nrow(ggplot2::ggplot_build(formula)$data[[1]]), 0L)
    expect_no_warning(ggplot2::ggplotGrob(native))
  }
  expect_equal(nrow(ggplot2::ggplot_build(base + geom_resid(fun = function(x) 3))$data[[1]]), 0L)
})

test_that("all constructors preserve jitter, facets, and missing predictor row positions", {
  d <- resid_contract_data()
  fit <- lm(y ~ x + z, d)
  d$z[2] <- NA_real_
  jitter <- ggplot2::position_jitter(width = .2, height = .4, seed = 813)
  for (horizontal in c(FALSE, TRUE)) {
    mapping <- if (horizontal) ggplot2::aes(y, x) else ggplot2::aes(x, y)
    base <- ggplot2::ggplot(d, mapping) + ggplot2::geom_point(position = jitter) +
      ggplot2::facet_wrap(~group)
    axis <- if (horizontal) "x" else "y"
    other <- if (horizontal) "y" else "x"
    for (case in resid_contract_cases()) {
      native <- base + case[[1]](model = fit, position = jitter)
      formula <- suppressMessages(case[[2]](base, fit))
      a <- ggplot2::ggplot_build(native)$data
      b <- ggplot2::ggplot_build(formula)$data
      expect_equal(a[[2]][[other]], a[[1]][[other]])
      expect_equal(a[[2]][[axis]], if (case[[3]]) rep(mean(fit$model$y), nrow(d)) else a[[1]][[axis]])
      expect_equal(a[[2]][[paste0(axis, "end")]], unname(predict(fit, d))[order(d$group)])
      expect_equal(a[[2]][sort(names(a[[2]]))], b[[2]][sort(names(b[[2]]))])
    }
  }
  expect_equal(jitter$seed, 813)
  expect_false(inherits(jitter, "PositionResidJitter"))
})

test_that("weighted residuals use prediction rather than the model residual vector", {
  d <- resid_contract_data()
  fit <- lm(y ~ x, d, weights = x)
  shifted <- transform(d, x = x + 4)
  base <- ggplot2::ggplot(shifted, ggplot2::aes(x, y))
  expected <- coef(fit)[[1]] + coef(fit)[[2]] * shifted$x
  for (case in resid_contract_cases()[c(1, 2, 5, 6)]) {
    expect_equal(ggplot2::ggplot_build(base + case[[1]](model = fit))$data[[1]]$yend, expected)
    expect_equal(ggplot2::ggplot_build(suppressMessages(case[[2]](base, fit)))$data[[1]]$yend, expected)
  }
})
