shared_model_rows <- function(plot) {
  rows <- ggplot2::ggplot_build(plot)$data[[layer_index(plot, "model")]]
  rows[sort(names(rows))]
}

shared_model_marks <- function(plot) {
  grobs <- ggplot2::layer_grob(plot, layer_index(plot, "model"))
  leaves <- function(grob) {
    if (length(grob$children)) return(unlist(lapply(grob$children, leaves), recursive = FALSE))
    fields <- intersect(c("x", "y", "x0", "x1", "y0", "y1", "id.lengths"), names(grob))
    if (!length(fields)) return(list())
    list(list(kind = class(grob)[1], positions = lapply(grob[fields], as.numeric), gp = grob$gp))
  }
  unname(unlist(lapply(grobs, leaves), recursive = FALSE))
}

test_that("native orientation recognizes horizontal group means", {
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Thumb, Sex)) + geom_model()
  rows <- shared_model_rows(plot)
  expect_equal(nrow(rows), nlevels(Fingers$Sex))
  expect_identical(unique(rows$.model_kind), "segment")
  expect_equal(rows$x, as.vector(tapply(Fingers$Thumb, Fingers$Sex, mean)))
  expect_equal(rows$xend, rows$x)
  expect_equal(rows$yend - rows$y, rep(0.4, nrow(rows)))
  expect_identical(unique(rows$flipped_aes), TRUE)
  marks <- shared_model_marks(plot)
  expect_length(marks, 1L)
  expect_equal(length(marks[[1]]$positions$x0), nlevels(Fingers$Sex))
  expect_equal(marks[[1]]$positions$x0, marks[[1]]$positions$x1)
  flipped <- shared_model_marks(plot + ggplot2::coord_flip())
  expect_equal(flipped[[1]]$positions$y0, flipped[[1]]$positions$y1)
})

test_that("both explicit interfaces agree on complete data and rendered marks", {
  for (formula in list(Thumb ~ Height, Thumb ~ Sex, Thumb ~ 1,
                       Thumb ~ Height + Sex, Thumb ~ Sex + Height)) {
    fit <- lm(formula, data = Fingers)
    focal <- if (identical(formula, Thumb ~ Sex + Height) ||
                 identical(formula, Thumb ~ Sex)) "Sex" else "Height"
    for (horizontal in c(FALSE, TRUE)) {
      mapping <- ggplot2::aes(!!rlang::sym(focal), log(Thumb))
      if (horizontal) mapping <- mapping[c("y", "x")]
      if (horizontal) names(mapping) <- c("x", "y")
      base <- ggplot2::ggplot(Fingers, mapping) + ggplot2::geom_point()
      native <- base + geom_model(model = fit, n = 13, colour = "black")
      adapted <- gf_model(base, fit, n = 13, colour = "black")
      expect_equal(shared_model_rows(native), shared_model_rows(adapted))
      a <- shared_model_marks(native)
      b <- shared_model_marks(adapted)
      expect_gt(length(a), 0L)
      expect_gt(sum(lengths(a[[1]]$positions)), 0L)
      expect_equal(a, b)
      expect_equal(shared_model_marks(native + ggplot2::coord_flip()),
                   shared_model_marks(adapted + ggplot2::coord_flip()))
    }
  }
})

test_that("outcome transforms are applied once to independently computed predictions", {
  fit <- lm(Thumb ~ Height, data = Fingers)
  for (constructor in list(geom_model, stat_model)) {
    plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, log(Thumb))) +
      constructor(model = fit, n = 17)
    rows <- shared_model_rows(plot)
    expect_equal(nrow(rows), 17L)
    expected <- log(unname(predict(fit, data.frame(Height = rows$x))))
    expect_equal(rows$y, expected)
    expect_equal(shared_model_rows(plot + ggplot2::scale_y_log10())$y, log10(expected))
  }
  empty <- lm(Thumb ~ 1, data = Fingers)
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, log(Thumb))) + geom_model(model = empty)
  expect_equal(shared_model_rows(plot)$yintercept, log(mean(Fingers$Thumb)))
  group <- lm(Thumb ~ Sex, data = Fingers)
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(log(Thumb), Sex)) + geom_model(model = group)
  expect_equal(shared_model_rows(plot)$x, log(as.vector(tapply(Fingers$Thumb, Fingers$Sex, mean))))
})

test_that("explicit models restore collision-safe pin expressions", {
  d <- data.frame(
    x = 1:8, y = 2 * (1:8) + 5,
    .coursekata_pin_x = 101:108,
    .coursekata_pin_y = 201:208
  )
  clean <- d[c("x", "y")]
  fit <- lm(y ~ x, data = d)
  make_plot <- function(data) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(log(x), log(y))) +
      ggplot2::geom_point()
    pin_plot_values(plot)$plot
  }
  collided <- make_plot(d)
  baseline <- make_plot(clean)

  expect_match(rlang::as_label(plot_spec(collided)$mapping$x), "\\.1$")
  expect_match(rlang::as_label(plot_spec(collided)$mapping$y), "\\.1$")
  expect_equal(
    shared_model_rows(collided + geom_model(model = fit, n = 11)),
    shared_model_rows(baseline + geom_model(model = fit, n = 11))
  )
})

test_that("secondary predictors use one grid and unused aesthetics do not multiply it", {
  fit <- lm(Thumb ~ Height + Sex, data = Fingers)
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb, shape = RaceEthnic)) +
    ggplot2::geom_point()
  native <- base + geom_model(model = fit, n = 11)
  adapted <- gf_model(base, fit, n = 11)
  expect_equal(shared_model_rows(native), shared_model_rows(adapted))
  expect_equal(nrow(shared_model_rows(native)), 11L * nlevels(Fingers$Sex))
  grid <- native$layers[[2]]$data
  expect_equal(grid$.model_outcome, unname(predict(fit, grid)))
  expect_length(unique(grid$RaceEthnic), 1L)

  constant <- transform(Fingers, extra = 1)
  fit <- lm(Thumb ~ Height + extra, data = constant)
  base <- ggplot2::ggplot(constant, ggplot2::aes(Height, Thumb))
  native <- suppressWarnings(base + geom_model(model = fit, n = 9))
  adapted <- suppressWarnings(gf_model(base, fit, n = 9))
  expect_equal(nrow(shared_model_rows(native)), 9L)
  expect_equal(shared_model_rows(native), shared_model_rows(adapted))
})

test_that("local mappings and their environments override inherited mappings", {
  fit <- lm(Thumb ~ Height, data = Fingers)
  shifted <- function(value) value + 2
  local_log <- function(value) log(value) + 1
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Weight, Index))
  plot <- base + geom_model(ggplot2::aes(shifted(Height), local_log(Thumb)), model = fit, n = 12)
  rows <- shared_model_rows(plot)
  expect_equal(rows$y, local_log(unname(predict(fit, data.frame(Height = rows$x - 2)))))
  independent <- base + geom_model(ggplot2::aes(x = Height), inherit.aes = FALSE,
                                   model = fit, n = 12)
  expect_equal(shared_model_rows(independent)$y,
               unname(predict(fit, data.frame(Height = shared_model_rows(independent)$x))))
  expect_error(base + geom_model(model = fit, inherit.aes = FALSE), "positional mapping")
  expect_error(base + geom_model(model = fit), "outcome variable")
  expect_error(ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
                 geom_model(model = fit, orientation = "y"), "orientation.*conflicts")
})

test_that("prepared model layers preserve source selection and plot reuse", {
  fit <- lm(Thumb ~ Height, data = Fingers)
  selected <- head(Fingers, 25)
  for (data in list(selected, function(d) head(d, 25), ~ head(.x, 25))) {
    plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
      geom_model(data = data, model = Thumb ~ Height, n = 10)
    rows <- shared_model_rows(plot)
    expect_equal(range(rows$x), range(selected$Height))
    expect_equal(rows$y, unname(predict(lm(Thumb ~ Height, data = selected),
                                       data.frame(Height = rows$x))))
    expect_identical(shared_model_rows(plot), shared_model_rows(plot))
  }
  layer <- geom_model(model = fit, n = 15)
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb))
  p <- base + layer
  q <- ggplot2::ggplot(Fingers, ggplot2::aes(log(Thumb), Height)) + layer
  expect_length(base$layers, 0L)
  expect_equal(shared_model_rows(q)$x, log(shared_model_rows(p)$y))
  expect_null(layer$mapping)
})

test_that("explicit uncertainty requests fail while inferred intervals still render", {
  fit <- lm(Thumb ~ Height, data = Fingers)
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb))
  expect_error(geom_model(model = fit, se = TRUE), "geom_model.*se = TRUE.*without")
  expect_error(stat_model(model = fit, se = TRUE), "stat_model.*se = TRUE.*without")
  expect_error(gf_model(base, fit, se = TRUE), "gf_model.*se = TRUE.*without")
  inferred <- base + geom_model(se = TRUE)
  rows <- shared_model_rows(inferred)
  interval <- predict(fit, data.frame(Height = rows$x), interval = "confidence")
  expect_equal(rows$ymin, unname(interval[, "lwr"]))
  expect_equal(rows$ymax, unname(interval[, "upr"]))
  expect_true(any(vapply(shared_model_marks(inferred), function(mark) {
    identical(mark$kind, "polygon")
  }, logical(1))))
})

test_that("outcome permutation probes preserve RNG and model predictions", {
  fit <- lm(Thumb ~ Height, data = Fingers)
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, base::sample(Thumb)))
  set.seed(81)
  before <- .Random.seed
  plot <- base + geom_model(model = fit, n = 19)
  expect_identical(.Random.seed, before)
  rows <- shared_model_rows(plot)
  expect_equal(rows$y, unname(predict(fit, data.frame(Height = rows$x))))
  expect_identical(shared_model_rows(plot), rows)
  predictor <- ggplot2::ggplot(Fingers, ggplot2::aes(base::sample(Height), Thumb))
  before <- .Random.seed
  expect_no_error(predictor + geom_model(model = fit))
  expect_identical(.Random.seed, before)
})

test_that("missing predictors and grid sizes retain the same explicit prediction claim", {
  data <- Fingers
  data$Height[c(3, 11)] <- NA_real_
  fit <- lm(Thumb ~ Height, data = data)
  base <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb))
  native <- base + geom_model(model = fit, n = 23)
  adapted <- gf_model(base, fit, n = 23)
  rows <- shared_model_rows(native)
  expect_equal(shared_model_rows(adapted), rows)
  expect_equal(nrow(rows), 23L)
  expect_equal(range(rows$x), range(data$Height, na.rm = TRUE))
  expect_equal(rows$y, unname(predict(fit, data.frame(Height = rows$x))))
  for (n in list(1, 0, NA_real_, Inf, 2.5, c(2, 3), "10")) {
    expect_error(base + geom_model(model = fit, n = n), "integer greater than 1")
    expect_error(gf_model(base, fit, n = n), "integer greater than 1")
  }
  expect_no_warning(expect_error(
    base + geom_model(model = fit, n = .Machine$integer.max + 1),
    "`n` must be one integer greater than 1", fixed = TRUE
  ))
  expect_no_warning(expect_error(
    gf_model(base, fit, n = .Machine$integer.max + 1),
    "`n` must be one integer greater than 1", fixed = TRUE
  ))
})

test_that("native and formula grouping agree for ordered, character and logical predictors", {
  for (value in list(ordered(Fingers$Sex), as.character(Fingers$Sex), Fingers$Sex == "male")) {
    data <- transform(Fingers, group = value)
    fit <- lm(Thumb ~ group, data = data)
    base <- ggplot2::ggplot(data, ggplot2::aes(group, Thumb))
    native <- base + geom_model(model = fit)
    adapted <- gf_model(base, fit)
    expect_equal(shared_model_rows(native), shared_model_rows(adapted))
    rows <- shared_model_rows(native)
    expect_equal(length(unique(rows$group)), length(unique(value)))
    expect_equal(sort(rows$y), sort(as.vector(tapply(data$Thumb, data$group, mean))))
  }
})

test_that("explicit predictions retain transformed axes after an inferred model pins the plot", {
  fit <- lm(Thumb ~ Height, data = Fingers)
  base <- gf_point(log(Thumb) ~ log(Height), data = Fingers) %>% gf_model()
  before <- ggplot2::ggplot_build(base)$data[[1]]
  native <- base + geom_model(model = fit, n = 14)
  adapted <- gf_model(base, fit, n = 14)
  # There are two tagged layers; inspect the newly added explicit model.
  rows <- ggplot2::ggplot_build(native)$data[[3]]
  expect_equal(nrow(rows), 14L)
  expect_gt(diff(range(rows$x)), 0)
  expect_equal(rows$y, log(unname(predict(fit, data.frame(Height = exp(rows$x))))))
  expect_equal(rows, ggplot2::ggplot_build(adapted)$data[[3]])
  expect_identical(ggplot2::ggplot_build(base)$data[[1]], before)
})

test_that("unrelated inherited styling has one explicit-model policy", {
  fit <- lm(Thumb ~ Height, data = Fingers)
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb,
                                              colour = Sex, alpha = RaceEthnic, shape = Sex))
  native <- base + geom_model(model = fit, n = 10)
  adapted <- gf_model(base, fit, n = 10)
  expect_equal(shared_model_rows(native), shared_model_rows(adapted))
  expect_equal(nrow(shared_model_rows(native)), 10L)
  fixed <- base + geom_model(model = fit, colour = "red", alpha = 0.7)
  expect_equal(unique(shared_model_rows(fixed)$colour), "red")
  expect_equal(unique(shared_model_rows(fixed)$alpha), 0.7)
  unmapped <- base + geom_model(ggplot2::aes(colour = NULL), model = fit)
  expect_no_error(ggplot2::layer_grob(unmapped, 1))
})

test_that("explicit group mappings override automatic secondary-predictor grouping", {
  fit <- lm(Thumb ~ Height + Sex, data = Fingers)
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb, group = Sex))
  native <- base + geom_model(ggplot2::aes(group = 1), model = fit, n = 12)
  adapted <- gf_model(base, fit, group = ~1, n = 12)
  rows <- shared_model_rows(native)
  expect_identical(unique(rows$group), 1L)
  expect_equal(nrow(rows), 12L * nlevels(Fingers$Sex))
  expect_equal(rows, shared_model_rows(adapted))
  expect_equal(shared_model_marks(native), shared_model_marks(adapted))

  inherited <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb, group = 1))
  expect_identical(unique(shared_model_rows(inherited + geom_model(model = fit))$group), 1L)
  expect_identical(unique(shared_model_rows(gf_model(inherited, fit))$group), 1L)

  native <- base + geom_model(ggplot2::aes(group = Height > 70), model = fit, n = 12)
  adapted <- gf_model(base, fit, group = ~Height > 70, n = 12)
  rows <- shared_model_rows(native)
  expect_identical(rows$group, as.integer(factor(rows$x > 70)))
  expect_equal(rows, shared_model_rows(adapted))
  expect_equal(shared_model_marks(native), shared_model_marks(adapted))
})

test_that("intercept models ignore grouping and remain standalone in both orientations", {
  fit <- lm(Thumb ~ 1, data = Fingers)
  mappings <- list(ggplot2::aes(Height, Thumb, group = Sex),
                   ggplot2::aes(Thumb, Height, group = Sex))
  for (mapping in mappings) {
    base <- ggplot2::ggplot(Fingers, mapping)
    native <- base + geom_model(model = fit)
    adapted <- gf_model(base, fit)
    rows <- shared_model_rows(native)
    expect_equal(rows, shared_model_rows(adapted))
    expect_equal(nrow(rows), 1L)
    intercept <- if ("yintercept" %in% names(rows)) rows$yintercept else rows$xintercept
    expect_equal(intercept, mean(Fingers$Thumb))
    expect_null(native$layers[[1]]$mapping$group)
    expect_null(adapted$layers[[1]]$mapping$group)
    expect_false(native$layers[[1]]$inherit.aes)
    expect_false(adapted$layers[[1]]$inherit.aes)
    expect_equal(shared_model_marks(native), shared_model_marks(adapted))
    expect_length(shared_model_marks(native), 1L)
    expect_equal(shared_model_rows(base + geom_model(ggplot2::aes(group = Sex), model = fit)),
                 rows)
    expect_equal(shared_model_rows(gf_model(base, fit, group = ~Sex)), rows)
  }
})
