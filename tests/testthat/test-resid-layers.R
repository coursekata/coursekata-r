test_that("ggplot2 constructors build every residual and reduction form", {
  data <- head(Fingers, 20)
  model <- lm(Thumb ~ Height, data = data)
  base <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb)) +
    ggplot2::geom_point()

  cases <- list(
    list(geom_resid, "GeomResid", "StatResid", "resid"),
    list(geom_square_resid, "GeomSquareResid", "StatResid", "square_resid"),
    list(geom_reduce, "GeomResid", "StatReduce", "reduce"),
    list(geom_square_reduce, "GeomSquareResid", "StatReduce", "square_reduce")
  )

  for (case in cases) {
    plot <- base + case[[1]](model = model)
    layer <- plot$layers[[2]]
    drawn <- ggplot2::ggplot_build(plot)$data[[2]]

    expect_s3_class(layer$geom, case[[2]])
    expect_s3_class(layer$stat, case[[3]])
    expect_equal(layer_index(plot, case[[4]]), 2L)
    expect_equal(drawn$yend, unname(predict(model, data)))
  }
})

test_that("stat constructors choose the same geoms without a raw layer", {
  data <- head(Fingers, 20)
  model <- lm(Thumb ~ Height, data = data)
  base <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb))

  residual <- base + stat_resid(model = model)
  residual_square <- base + stat_resid(model = model, geom = "square_resid")
  reduction <- base + stat_reduce(model = model)
  reduction_square <- base + stat_reduce(model = model, geom = "square_resid")

  expect_s3_class(residual$layers[[1]]$stat, "StatResid")
  expect_s3_class(residual$layers[[1]]$geom, "GeomResid")
  expect_s3_class(residual_square$layers[[1]]$geom, "GeomSquareResid")
  expect_s3_class(reduction$layers[[1]]$stat, "StatReduce")
  expect_s3_class(reduction$layers[[1]]$geom, "GeomResid")
  expect_s3_class(reduction_square$layers[[1]]$geom, "GeomSquareResid")

  residual_drawn <- ggplot2::ggplot_build(residual)$data[[1]]
  residual_square_drawn <- ggplot2::ggplot_build(residual_square)$data[[1]]
  reduction_drawn <- ggplot2::ggplot_build(reduction)$data[[1]]
  reduction_square_drawn <- ggplot2::ggplot_build(reduction_square)$data[[1]]

  expect_equal(residual_drawn$y, data$Thumb)
  expect_equal(residual_drawn$yend, unname(predict(model, data)))
  expect_equal(residual_square_drawn$y, data$Thumb)
  expect_equal(residual_square_drawn$yend, unname(predict(model, data)))
  expect_equal(unique(reduction_drawn$y), mean(model$model[[1]]))
  expect_equal(reduction_drawn$yend, unname(predict(model, data)))
  expect_equal(unique(reduction_square_drawn$y), mean(model$model[[1]]))
  expect_equal(reduction_square_drawn$yend, unname(predict(model, data)))

  expect_equal(unique(residual_drawn$linewidth), 0.2)
  expect_equal(unique(residual_square_drawn$alpha), 0.1)
  expect_equal(unique(reduction_drawn$linewidth), 0.2)
  expect_equal(unique(reduction_square_drawn$alpha), 0.1)
})

test_that("stat constructors respect geom objects and explicit visual parameters", {
  data <- head(Fingers, 20)
  model <- lm(Thumb ~ Height, data = data)
  base <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb))

  residual <- base + stat_resid(model = model, geom = GeomResid)
  residual_override <- base + stat_resid(
    model = model, geom = GeomResid, linewidth = 0.8
  )
  reduction_square <- base + stat_reduce(model = model, geom = GeomSquareResid)
  reduction_square_override <- base + stat_reduce(
    model = model, geom = GeomSquareResid, alpha = 0.6
  )

  expect_s3_class(residual$layers[[1]]$geom, "GeomResid")
  expect_equal(unique(ggplot2::ggplot_build(residual)$data[[1]]$linewidth), 0.2)
  expect_equal(
    unique(ggplot2::ggplot_build(residual_override)$data[[1]]$linewidth),
    0.8
  )
  expect_s3_class(reduction_square$layers[[1]]$geom, "GeomSquareResid")
  expect_equal(unique(ggplot2::ggplot_build(reduction_square)$data[[1]]$alpha), 0.1)
  expect_equal(
    unique(ggplot2::ggplot_build(reduction_square_override)$data[[1]]$alpha),
    0.6
  )
})

test_that("geom constructors honor an explicit ggplot2 stat", {
  data <- head(Fingers, 20)
  model <- lm(Thumb ~ Height, data = data)
  identity <- ggplot2::ggproto(
    NULL,
    ggplot2::StatIdentity,
    extra_params = c("na.rm", "orientation")
  )
  plot <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb)) +
    geom_resid(model = model, stat = identity)
  drawn <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_s3_class(plot$layers[[1]]$stat, "StatIdentity")
  expect_false(inherits(plot$layers[[1]]$stat, "StatResid"))
  expect_equal(drawn$y, data$Thumb)
  expect_equal(drawn$yend, unname(predict(model, data)))
})

test_that("ggplot2 and ggformula functions build the same model layers", {
  data <- head(Fingers, 20)
  model <- lm(Thumb ~ Height, data = data)
  gg_base <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb)) +
    ggplot2::geom_point()
  gf_base <- gf_point(Thumb ~ Height, data = data)

  pairs <- list(
    list(geom_resid, gf_resid),
    list(geom_square_resid, gf_square_resid),
    list(geom_reduce, gf_reduce),
    list(geom_square_reduce, gf_square_reduce)
  )

  for (pair in pairs) {
    gg_plot <- suppressMessages(gg_base + pair[[1]](model = model))
    gf_plot <- suppressMessages(pair[[2]](gf_base, model))
    gg_data <- ggplot2::ggplot_build(gg_plot)$data[[2]]
    gf_data <- ggplot2::ggplot_build(gf_plot)$data[[2]]
    expect_equal(
      gg_data[sort(names(gg_data))],
      gf_data[sort(names(gf_data))]
    )
    expect_equal(
      as.numeric(ggplot2::layer_grob(gg_plot, 2)[[1]]$x),
      as.numeric(ggplot2::layer_grob(gf_plot, 2)[[1]]$x)
    )
  }
})

test_that("orientation y measures a model whose outcome is on x", {
  data <- head(Fingers, 20)
  model <- lm(Height ~ Thumb, data = data)
  plot <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb)) +
    ggplot2::geom_point() +
    geom_resid(model = model, orientation = "y")
  drawn <- ggplot2::ggplot_build(plot)$data[[2]]
  gf_plot <- gf_point(Thumb ~ Height, data = data) %>% gf_resid(model)
  gf_drawn <- ggplot2::ggplot_build(gf_plot)$data[[2]]

  expect_equal(drawn$xend, unname(predict(model, data)))
  expect_equal(drawn$x, data$Height)
  expect_equal(drawn$y, data$Thumb)
  expect_equal(drawn[sort(names(drawn))], gf_drawn[sort(names(gf_drawn))])
  expect_no_error(ggplot2::layer_grob(plot + ggplot2::coord_flip(), 2))
})

test_that("orientation y keeps a reduction baseline fixed while jittering its groups", {
  data <- head(Fingers, 30)
  model <- lm(Height ~ Sex, data = data)
  jitter <- ggplot2::position_jitter(width = 0.3, height = 0.2, seed = 42)
  plot <- ggplot2::ggplot(data, ggplot2::aes(Height, Sex)) +
    ggplot2::geom_point(position = jitter) +
    geom_reduce(model = model, orientation = "y", position = jitter)
  drawn <- ggplot2::ggplot_build(plot)$data

  expect_equal(drawn[[2]]$y, drawn[[1]]$y)
  expect_equal(unique(drawn[[2]]$x), mean(model$model[[1]]))
  expect_equal(drawn[[2]]$xend, unname(predict(model, data)))
})

test_that("a fixed ggplot2 jitter keeps residuals on their points", {
  data <- head(Fingers, 30)
  model <- lm(Thumb ~ Sex, data = data)
  jitter <- ggplot2::position_jitter(width = 0.2, height = 0.3, seed = 42)
  before <- list(width = jitter$width, height = jitter$height, seed = jitter$seed)
  plot <- ggplot2::ggplot(data, ggplot2::aes(Sex, Thumb)) +
    ggplot2::geom_point(position = jitter) +
    geom_resid(model = model, position = jitter)
  drawn <- ggplot2::ggplot_build(plot)$data

  expect_equal(drawn[[2]]$x, drawn[[1]]$x)
  expect_equal(drawn[[2]]$y, drawn[[1]]$y)
  expect_equal(drawn[[2]]$yend, unname(predict(model, data)))
  expect_equal(
    list(width = jitter$width, height = jitter$height, seed = jitter$seed),
    before
  )
  expect_s3_class(jitter, "PositionJitter")
  expect_false(inherits(jitter, "PositionResidJitter"))
})

test_that("reduction jitter holds the outcome axis and matches the point's other offset", {
  data <- head(Fingers, 30)
  model <- lm(Thumb ~ Sex, data = data)
  jitter <- ggplot2::position_jitter(width = 0.2, height = 0.3, seed = 42)
  plot <- ggplot2::ggplot(data, ggplot2::aes(Sex, Thumb)) +
    ggplot2::geom_point(position = jitter) +
    geom_reduce(model = model, position = jitter)
  drawn <- ggplot2::ggplot_build(plot)$data

  expect_equal(drawn[[2]]$x, drawn[[1]]$x)
  expect_equal(unique(drawn[[2]]$y), mean(model$model[[1]]))
  expect_equal(drawn[[2]]$yend, unname(predict(model, data)))
})

test_that("an unseeded jitter is refused before it can misalign an overlay", {
  model <- lm(Thumb ~ Sex, data = Fingers)

  expect_error(
    geom_resid(model = model, position = ggplot2::position_jitter(width = 0.1)),
    "fixed jitter seed"
  )
  expect_error(
    geom_reduce(model = model, position = "jitter"),
    "fixed jitter seed"
  )
  expect_error(
    geom_resid(model = model, position = ggplot2::position_jitterdodge()),
    "do not support"
  )
})

test_that("complete layer data keeps facets and model predictions together", {
  data <- Fingers[!is.na(Fingers$Thumb) & !is.na(Fingers$Height), ]
  model <- lm(Thumb ~ Height, data = data)
  plot <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb)) +
    ggplot2::geom_point() +
    geom_resid(model = model) +
    ggplot2::facet_wrap(~Sex, scales = "free")
  drawn <- ggplot2::ggplot_build(plot)$data

  expect_equal(table(drawn[[2]]$PANEL), table(drawn[[1]]$PANEL))
  expect_equal(sort(drawn[[2]]$yend), sort(unname(predict(model, data))))
})

test_that("predictions can use model variables that are not mapped to the plot", {
  data <- Fingers[!is.na(Fingers$Thumb) & !is.na(Fingers$Height), ]
  model <- lm(Thumb ~ Height + Sex, data = data)
  plot <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb)) +
    geom_resid(model = model)
  drawn <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_equal(drawn$yend, unname(predict(model, data)))

  missing_predictor <- data[c("Height", "Thumb")]
  broken <- ggplot2::ggplot(missing_predictor, ggplot2::aes(Height, Thumb)) +
    geom_resid(model = model)
  expect_error(ggplot2::ggplot_build(broken), "missing from the plot's data: Sex")
})

test_that("rows omitted by a model stay aligned with the plot", {
  data <- head(Fingers, 30)
  data$Height[c(3, 11)] <- NA_real_
  model <- lm(Thumb ~ Height, data = data)
  plot <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb)) +
    ggplot2::geom_point() +
    geom_resid(model = model)
  built <- ggplot2::ggplot_build(plot)$data

  expect_equal(which(is.na(built[[2]]$yend)), which(is.na(data$Height)))
  expect_no_error(suppressWarnings(ggplot2::layer_grob(plot, 2)))
})

test_that("a layer data function runs before predictions are added", {
  model <- lm(Thumb ~ Height, data = Fingers)
  plot <- ggplot2::ggplot(Fingers, ggplot2::aes(Height, Thumb)) +
    geom_resid(data = ~ head(.x, 12), model = model)
  drawn <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_equal(nrow(drawn), 12L)
  expect_equal(drawn$yend, unname(predict(model, head(Fingers, 12))))
})

test_that("explicit layer data controls predictions but not the model's grand mean", {
  data <- Fingers[!is.na(Fingers$Thumb) & !is.na(Fingers$Height), ]
  model <- lm(Thumb ~ Height, data = data)
  layer_data <- head(data, 12)
  plot <- ggplot2::ggplot(data, ggplot2::aes(Height, Thumb)) +
    geom_reduce(data = layer_data, model = model)
  drawn <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_equal(nrow(drawn), nrow(layer_data))
  expect_equal(unique(drawn$y), mean(model$model[[1]]))
  expect_equal(drawn$yend, unname(predict(model, layer_data)))
})

test_that("reduction contracts are shared by the ggplot2 constructors", {
  set.seed(3)
  data <- Fingers[!is.na(Fingers$Thumb) & !is.na(Fingers$Height), ]
  data$w <- runif(nrow(data), 0.5, 2)

  expect_error(
    geom_reduce(model = lm(Thumb ~ Height - 1, data = data)),
    "arithmetic does not support"
  )
  expect_error(
    stat_reduce(model = lm(Thumb ~ Height, data = data, weights = w)),
    "arithmetic does not support"
  )
  expect_warning(
    layer <- geom_square_reduce(model = lm(Thumb ~ NULL, data = data)),
    class = "coursekata_reduce_empty"
  )
  expect_s3_class(layer, "Layer")
})

test_that("ggplot2 constructors keep ggplot2 parameter checking", {
  model <- lm(Thumb ~ Height, data = Fingers)

  expect_warning(
    geom_resid(model = model, nonesuch = 1),
    "Ignoring unknown parameters"
  )
  expect_error(geom_resid(), "fitted `model`")
  expect_error(geom_reduce(model = model, orientation = "sideways"), "orientation")
})

test_that("ggplot2 layer arguments and fixed aesthetics reach the layer", {
  data <- head(Fingers, 20)
  model <- lm(Thumb ~ Height, data = data)
  layer <- geom_resid(
    mapping = ggplot2::aes(Height, Thumb),
    data = data,
    model = model,
    colour = "firebrick",
    linewidth = 0.7,
    na.rm = TRUE,
    show.legend = FALSE,
    inherit.aes = FALSE
  )

  expect_false(layer$inherit.aes)
  expect_false(layer$show.legend)
  expect_true(layer$geom_params$na.rm)
  expect_true(layer$stat_params$na.rm)
  drawn <- ggplot2::ggplot_build(ggplot2::ggplot() + layer)$data[[1]]
  expect_equal(unique(drawn$colour), "firebrick")
  expect_equal(unique(drawn$linewidth), 0.7)
})
