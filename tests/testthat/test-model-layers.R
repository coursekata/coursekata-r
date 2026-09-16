model_layer_data_built <- function(plot) {
  ggplot2::ggplot_build(plot)$data[[layer_index(plot, "model")]]
}

test_that("ggplot2 model constructors build the shared stat and geom", {
  model <- lm(Thumb ~ Height, data = Fingers)
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb))

  from_geom <- base + geom_model(model = model)
  from_stat <- base + stat_model(model = model)

  for (plot in list(from_geom, from_stat)) {
    layer <- plot$layers[[1]]
    expect_s3_class(layer$geom, "GeomModel")
    expect_s3_class(layer$stat, "StatModel")
    expect_equal(layer_index(plot, "model"), 1L)
    expect_no_error(ggplot2::layer_grob(plot, 1))
  }
})

test_that("model constructors respect explicit ggplot2 stat and geom choices", {
  model <- lm(Thumb ~ Height, data = Fingers)
  identity <- ggplot2::ggproto(
    NULL,
    ggplot2::StatIdentity,
    extra_params = c("na.rm", "orientation")
  )
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb))
  geom_plot <- base + geom_model(model = model, stat = identity)
  stat_plot <- base + stat_model(model = model, geom = "line")

  expect_s3_class(geom_plot$layers[[1]]$stat, "StatIdentity")
  expect_s3_class(stat_plot$layers[[1]]$geom, "GeomLine")
  expect_equal(
    ggplot2::ggplot_build(geom_plot)$data[[1]]$y,
    ggplot2::ggplot_build(stat_plot)$data[[1]]$y
  )
})

test_that("ggplot2 and ggformula explicit models share their representation", {
  cases <- list(
    line = list(
      ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
        geom_model(model = lm(Thumb ~ Height, data = Fingers)),
      gf_point(Thumb ~ Height, data = Fingers) %>%
        gf_model(lm(Thumb ~ Height, data = Fingers))
    ),
    group = list(
      ggplot2::ggplot(Fingers, ggplot2::aes(Sex, Thumb)) +
        geom_model(model = lm(Thumb ~ Sex, data = Fingers)),
      gf_jitter(Thumb ~ Sex, data = Fingers, width = 0.1) %>%
        gf_model(lm(Thumb ~ Sex, data = Fingers))
    ),
    empty = list(
      ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
        geom_model(model = lm(Thumb ~ NULL, data = Fingers)),
      gf_point(Thumb ~ Height, data = Fingers) %>%
        gf_model(lm(Thumb ~ NULL, data = Fingers))
    )
  )

  for (case in cases) {
    gg_data <- ggplot2::ggplot_build(case[[1]])$data[[1]]
    gf_data <- ggplot2::ggplot_build(case[[2]])$data[[2]]
    positions <- intersect(
      c("x", "y", "xend", "yend", "xintercept", "yintercept"),
      intersect(names(gg_data), names(gf_data))
    )

    expect_gt(length(positions), 0L)
    expect_equal(gg_data[positions], gf_data[positions])
  }
})

test_that("ggplot2 and ggformula infer the same model from the axes", {
  cases <- list(
    line = list(
      ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) + geom_model(),
      gf_point(Thumb ~ Height, data = Fingers) %>% gf_model()
    ),
    group = list(
      ggplot2::ggplot(Fingers, ggplot2::aes(Sex, Thumb)) + geom_model(),
      gf_jitter(Thumb ~ Sex, data = Fingers, width = 0.1) %>% gf_model()
    ),
    empty = list(
      ggplot2::ggplot(Fingers, ggplot2::aes(x = Thumb)) + geom_model(),
      gf_histogram(~Thumb, data = Fingers, binwidth = 5) %>% gf_model()
    )
  )

  for (case in cases) {
    gg_data <- ggplot2::ggplot_build(case[[1]])$data[[1]]
    gf_data <- ggplot2::ggplot_build(case[[2]])$data[[2]]
    positions <- intersect(
      c("x", "y", "xend", "yend", "xintercept", "yintercept"),
      intersect(names(gg_data), names(gf_data))
    )

    expect_gt(length(positions), 0L)
    expect_equal(gg_data[positions], gf_data[positions])
  }
})

test_that("a supplied continuous model draws its predictions across the data", {
  model <- lm(Thumb ~ Height, data = Fingers)
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
    geom_model(model = model)
  drawn <- model_layer_data_built(plot)

  expect_equal(range(drawn$x), range(Fingers$Height, na.rm = TRUE))
  expect_equal(
    drawn$y,
    unname(predict(model, data.frame(Height = drawn$x)))
  )
  expect_equal(unique(drawn$.model_kind), "line")
})

test_that("a model formula is fitted against the layer data", {
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb))
  formula_plot <- base + geom_model(model = Thumb ~ Height)
  fitted_plot <- base + geom_model(model = lm(Thumb ~ Height, data = Fingers))

  expect_equal(
    model_layer_data_built(formula_plot)[c("x", "y")],
    model_layer_data_built(fitted_plot)[c("x", "y")]
  )
})

test_that("a supplied group model draws one mark at each predicted mean", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Sex, Thumb)) +
    geom_model(model = model)
  drawn <- model_layer_data_built(plot)
  expected <- predict(model, data.frame(Sex = levels(Fingers$Sex)))

  expect_equal((drawn$x + drawn$xend) / 2, seq_along(expected))
  expect_equal(drawn$y, unname(expected))
  expect_equal(drawn$yend, drawn$y)
  expect_equal(unique(drawn$.model_kind), "segment")
})

test_that("group marks follow a reordered discrete scale in either orientation", {
  model <- lm(Thumb ~ Sex, data = Fingers)
  levels <- rev(levels(Fingers$Sex))
  horizontal <- ggplot2::ggplot(Fingers, ggplot2::aes(Sex, Thumb)) +
    geom_model(model = model) +
    ggplot2::scale_x_discrete(limits = levels)
  vertical <- ggplot2::ggplot(Fingers, ggplot2::aes(Thumb, Sex)) +
    geom_model(ggplot2::aes(y = Sex), model = model, orientation = "y") +
    ggplot2::scale_y_discrete(limits = levels)

  horizontal_data <- model_layer_data_built(horizontal)
  vertical_data <- model_layer_data_built(vertical)
  expected <- unname(predict(model, data.frame(Sex = levels)))

  expect_equal(horizontal_data$y[order(horizontal_data$x)], expected)
  expect_equal(vertical_data$x[order(vertical_data$y)], expected)
  expect_equal(vertical_data$xend, vertical_data$x)
})

test_that("an empty model draws an intercept and does not inherit plot positions", {
  model <- lm(Thumb ~ NULL, data = Fingers)
  horizontal <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
    geom_model(model = model)
  vertical <- ggplot2::ggplot(Fingers, ggplot2::aes(Thumb, Height)) +
    geom_model(model = model, orientation = "y")

  expect_false(horizontal$layers[[1]]$inherit.aes)
  expect_equal(model_layer_data_built(horizontal)$yintercept, mean(Fingers$Thumb))
  expect_equal(model_layer_data_built(vertical)$xintercept, mean(Fingers$Thumb))
})

test_that("an inferred model follows ggplot2 grouping and orientation", {
  line <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) + geom_model()
  groups <- ggplot2::ggplot(Fingers, ggplot2::aes(Sex, Thumb)) + geom_model()
  x_only <- ggplot2::ggplot(Fingers, ggplot2::aes(x = Thumb)) + geom_model()
  y_only <- ggplot2::ggplot(Fingers, ggplot2::aes(y = Thumb)) + geom_model()

  line_data <- model_layer_data_built(line)
  group_data <- model_layer_data_built(groups)
  expect_equal(
    unname(coef(lm(y ~ x, data = line_data))),
    unname(coef(lm(Thumb ~ Height, data = Fingers))),
    tolerance = 1e-8
  )
  expect_equal(
    group_data$y,
    unname(as.vector(tapply(Fingers$Thumb, Fingers$Sex, mean)))
  )
  expect_equal(model_layer_data_built(x_only)$xintercept, mean(Fingers$Thumb))
  expect_equal(model_layer_data_built(y_only)$yintercept, mean(Fingers$Thumb))
})

test_that("an inferred continuous model follows ggplot2 groups and weights", {
  grouped <- ggplot2::ggplot(
    Fingers,
    ggplot2::aes(Height, Thumb, colour = Sex)
  ) + geom_model()
  grouped_data <- model_layer_data_built(grouped)
  slopes <- vapply(split(grouped_data, grouped_data$group), function(group) {
    unname(coef(lm(y ~ x, data = group))[[2]])
  }, numeric(1))
  expected <- vapply(split(Fingers, Fingers$Sex), function(group) {
    unname(coef(lm(Thumb ~ Height, data = group))[[2]])
  }, numeric(1))

  expect_no_warning(
    weighted <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
      geom_model(ggplot2::aes(weight = Weight))
  )
  expect_no_warning(weighted_data <- model_layer_data_built(weighted))

  expect_equal(length(unique(grouped_data$colour)), nlevels(Fingers$Sex))
  expect_equal(unname(slopes), unname(expected), tolerance = 1e-6)
  expect_equal(
    unname(coef(lm(y ~ x, data = weighted_data))[[2]]),
    unname(coef(lm(Thumb ~ Height, data = Fingers, weights = Weight))[[2]]),
    tolerance = 1e-6
  )
})

test_that("an inferred model is computed separately by panel", {
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
    geom_model() +
    ggplot2::facet_wrap(~Sex)
  drawn <- model_layer_data_built(plot)
  slopes <- vapply(split(drawn, drawn$PANEL), function(panel) {
    unname(coef(lm(y ~ x, data = panel))[[2]])
  }, numeric(1))
  expected <- vapply(split(Fingers, Fingers$Sex), function(panel) {
    unname(coef(lm(Thumb ~ Height, data = panel))[[2]])
  }, numeric(1))

  expect_equal(unname(slopes), unname(expected), tolerance = 1e-6)
})

test_that("a supplied mixed model draws one line per secondary predictor level", {
  model <- lm(Thumb ~ Height + Sex, data = Fingers)
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb, colour = Sex)) +
    geom_model(ggplot2::aes(x = Height), model = model)
  drawn <- model_layer_data_built(plot)
  lines <- split(drawn, drawn$group)

  expect_equal(length(unique(drawn$group)), nlevels(Fingers$Sex))
  expect_equal(length(unique(drawn$colour)), nlevels(Fingers$Sex))
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    level <- levels(Fingers$Sex)[[i]]
    newdata <- data.frame(
      Height = line$x,
      Sex = factor(level, levels = levels(Fingers$Sex))
    )
    expect_equal(range(line$x), range(Fingers$Height, na.rm = TRUE))
    expect_equal(line$y, unname(predict(model, newdata = newdata)))
  }
})

test_that("an explicit group mapping keeps ggplot2 grouping control", {
  model <- lm(Thumb ~ Height + Sex, data = Fingers)
  layer <- geom_model(
    ggplot2::aes(x = Height, group = Sex),
    model = model
  )

  expect_equal(rlang::as_label(layer$mapping$group), "Sex")
  expect_no_error(
    ggplot2::ggplot_build(
      ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) + layer
    )
  )
})

test_that("a supplied model preserves an ordered predictor's type", {
  data <- transform(Fingers, ordered_sex = ordered(Sex))
  model <- lm(Thumb ~ ordered_sex, data = data)
  plot <- ggplot2::ggplot(data, ggplot2::aes(ordered_sex, Thumb)) +
    geom_model(model = model)
  prepared <- plot$layers[[1]]$data(data)

  expect_no_error(drawn <- model_layer_data_built(plot))
  expect_equal(nrow(drawn), nlevels(data$ordered_sex))
  expect_true(is.ordered(prepared$ordered_sex))
  expect_identical(levels(prepared$ordered_sex), levels(data$ordered_sex))
})

test_that("supplied multi-predictor models require a local predictor mapping", {
  mixed <- lm(Thumb ~ Height + Sex, data = Fingers)
  categorical_plot <- ggplot2::ggplot(
    Fingers,
    ggplot2::aes(Sex, Thumb)
  ) + geom_model(model = mixed)

  expect_error(ggplot2::ggplot_build(categorical_plot), "cannot choose")

  categorical <- ggplot2::ggplot(Fingers, ggplot2::aes(Sex, Thumb)) +
    geom_model(ggplot2::aes(x = Sex), model = mixed)
  prepared <- categorical$layers[[1]]$data(Fingers)
  categorical_data <- model_layer_data_built(categorical)
  centers <- (categorical_data$x + categorical_data$xend) / 2
  support <- mean(Fingers$Height) + c(-1, 0, 1) * stats::sd(Fingers$Height)

  expect_equal(sort(unique(prepared$Height)), sort(support))
  expect_equal(
    prepared$.model_outcome,
    unname(predict(mixed, newdata = prepared))
  )
  expect_equal(sort(categorical_data$y), sort(prepared$.model_outcome))
  expect_equal(as.integer(table(centers)), rep(3L, nlevels(Fingers$Sex)))
  expect_equal(unique(categorical_data$.model_kind), "segment")

  data <- transform(Fingers, Height2 = Height^2)
  model <- lm(Thumb ~ Height + Height2, data = data)
  ambiguous <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb)) +
    geom_model(model = model)
  resolved <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb)) +
    geom_model(ggplot2::aes(x = Height), model = model)

  expect_error(ggplot2::ggplot_build(ambiguous), "cannot choose")
  expect_no_error(ggplot2::ggplot_build(resolved))
  expect_equal(length(unique(model_layer_data_built(resolved)$group)), 3L)
})

test_that("a supplied continuous model follows a flipped orientation", {
  model <- lm(Thumb ~ Height, data = Fingers)
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Thumb, Height)) +
    geom_model(
      ggplot2::aes(y = Height),
      model = model,
      orientation = "y"
    )
  drawn <- model_layer_data_built(plot)

  expect_equal(range(drawn$y), range(Fingers$Height, na.rm = TRUE))
  expect_equal(
    drawn$x,
    unname(predict(model, data.frame(Height = drawn$y)))
  )
})

test_that("a supplied model repeats one whole-data fit across facets", {
  model <- lm(Thumb ~ Height, data = Fingers)
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
    geom_model(model = model) +
    ggplot2::facet_wrap(~Sex)
  panels <- split(model_layer_data_built(plot), model_layer_data_built(plot)$PANEL)

  expect_length(panels, nlevels(Fingers$Sex))
  for (panel in panels) {
    expect_equal(
      panel$y,
      unname(predict(model, data.frame(Height = panel$x)))
    )
  }
})

test_that("a transformed plot mapping is evaluated on the model grid", {
  model <- lm(Thumb ~ log(Height), data = Fingers)
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(log(Height), Thumb)) +
    geom_model(model = model)
  drawn <- model_layer_data_built(plot)

  expect_equal(range(drawn$x), log(range(Fingers$Height)))
  expect_equal(
    drawn$y,
    unname(predict(model, data.frame(Height = exp(drawn$x))))
  )
})

test_that("layer data selectors run before a supplied model grid is built", {
  model <- lm(Thumb ~ Height, data = Fingers)
  selected <- head(Fingers, 20)
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
    geom_model(data = ~ head(.x, 20), model = model, n = 24)
  drawn <- model_layer_data_built(plot)

  expect_equal(nrow(drawn), 24L)
  expect_equal(range(drawn$x), range(selected$Height))
})

test_that("native prediction grids stay bounded", {
  model <- lm(Thumb ~ Height, data = Fingers)
  small <- head(Fingers, 10)
  large <- Fingers[rep(seq_len(nrow(Fingers)), length.out = 400), ]

  rows <- function(data) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb)) +
      geom_model(model = model)
    nrow(model_layer_data_built(plot))
  }

  expect_equal(rows(small), 80L)
  expect_equal(rows(large), model_grid_max_points)
})

test_that("native supplied-model diagnostics name the active front door", {
  model <- lm(Thumb ~ Height + Weight, data = Fingers)
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb))

  expect_error(geom_model(model = ~Height), "geom_model.*two-sided")
  expect_error(stat_model(model = ~Height), "stat_model.*two-sided")
  expect_error(ggplot2::ggplot_build(base + geom_model(model = model)), "geom_model")
  expect_error(ggplot2::ggplot_build(base + stat_model(model = model)), "stat_model")

  transformed <- lm(log(Thumb) ~ Height, data = Fingers)
  expect_error(
    ggplot2::ggplot_build(base + geom_model(model = transformed)),
    "untransformed outcome"
  )
  expect_error(
    ggplot2::ggplot_build(base + geom_model(model = model, n = 1)),
    "integer greater than 1"
  )

  missing_height <- ggplot2::ggplot(Fingers["Thumb"], ggplot2::aes(y = Thumb)) +
    geom_model(ggplot2::aes(x = Height), model = lm(Thumb ~ Height, data = Fingers))
  expect_error(ggplot2::ggplot_build(missing_height), "Missing: Height")

  three_predictors <- lm(Thumb ~ Height + Weight + Index, data = Fingers)
  too_many <- base + geom_model(
    ggplot2::aes(x = Height),
    model = three_predictors
  )
  expect_error(ggplot2::ggplot_build(too_many), "at most one predictor")
})

test_that("the continuous inference vocabulary reaches StatModel", {
  base <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb))
  curved <- base + geom_model(formula = y ~ poly(x, 2), n = 31)
  banded <- base + geom_model(se = TRUE)

  expect_equal(nrow(model_layer_data_built(curved)), 31L)
  expect_true(all(c("ymin", "ymax") %in% names(model_layer_data_built(banded))))
  grob <- ggplot2::layer_grob(banded, 1)[[1]]
  expect_s3_class(grob, "gTree")
})

test_that("ggplot2 layer arguments and fixed aesthetics reach model layers", {
  model <- lm(Thumb ~ Height, data = Fingers)
  layer <- geom_model(
    mapping = ggplot2::aes(Height), data = Fingers, model = model,
    colour = "firebrick", linewidth = 0.7, na.rm = TRUE,
    show.legend = FALSE, inherit.aes = FALSE
  )
  plot <- ggplot2::ggplot() + layer
  drawn <- model_layer_data_built(plot)

  expect_false(layer$inherit.aes)
  expect_false(layer$show.legend)
  expect_true(layer$geom_params$na.rm)
  expect_true(layer$stat_params$na.rm)
  expect_equal(unique(drawn$colour), "firebrick")
  expect_equal(unique(drawn$linewidth), 0.7)
  expect_warning(geom_model(model = model, nonesuch = 1), "Ignoring unknown")
  expect_error(geom_model(model = model, orientation = "sideways"), "orientation")
})
