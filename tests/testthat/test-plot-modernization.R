test_that("distribution helpers require their current argument names", {
  p <- ggplot2::ggplot(data.frame(x = 1:10), ggplot2::aes(x)) +
    ggplot2::geom_histogram(bins = 5)
  expect_error(show_mean(plot = p), "unused argument.*plot")
  expect_error(show_dgp(plot = p), "unused argument.*plot")
  expect_s3_class(show_mean(object = p), "ggplot")
  expect_s3_class(show_dgp(object = p, size = 7), "ggplot")
  expect_s3_class(show_cutoffs(object = p, part = middle(x, .8), size = 7), "ggplot")
})

test_that("residual stats require endpoints without removing incomplete rows", {
  data <- data.frame(x = c(1, NA, 3), y = c(2, 4, NA), yend = c(3, NA, 5))
  for (stat in list(StatResid, StatReduce)) {
    expect_identical(stat$compute_layer(data, list(), NULL), data)
    expect_error(stat$compute_layer(data[c("x", "y")], list(), NULL), "xend or yend")
    expect_error(stat$compute_layer(data[c("y", "yend")], list(), NULL), "missing aesthetics: x")
    x_end <- data
    names(x_end)[3] <- "xend"
    expect_identical(stat$compute_layer(x_end, list(), NULL), x_end)
  }
})

test_that("residual stats preserve fixed endpoints through a full build", {
  data <- data.frame(x = c(1, NA, 3), y = c(2, 4, NA))
  for (stat in list(StatResid, StatReduce)) {
    for (endpoint in c("xend", "yend")) {
      layer <- ggplot2::layer(
        stat = stat, geom = GeomResid, position = "identity",
        params = setNames(list(5), endpoint)
      )
      built <- ggplot2::ggplot_build(
        ggplot2::ggplot(data, ggplot2::aes(x, y)) + layer
      )$data[[1]]
      expect_equal(nrow(built), nrow(data))
      expect_equal(built[[endpoint]], rep(5, nrow(data)))
      expect_equal(built$x, data$x)
      expect_equal(built$y, data$y)
    }
    missing <- ggplot2::ggplot(data, ggplot2::aes(x, y)) + ggplot2::layer(
      stat = stat, geom = "point", position = "identity"
    )
    expect_error(ggplot2::ggplot_build(missing), "xend or yend")
  }
})

test_that("line width translation names the public constructor and retains precedence", {
  model <- lm(Thumb ~ Height, data = Fingers)
  calls <- list(
    quote(geom_resid(model = model, size = 2, linewidth = 3)),
    quote(geom_square_resid(model = model, size = 2, linewidth = 3)),
    quote(stat_resid(model = model, size = 2, linewidth = 3)),
    quote(geom_reduce(model = model, size = 2, linewidth = 3)),
    quote(geom_square_reduce(model = model, size = 2, linewidth = 3)),
    quote(stat_reduce(model = model, size = 2, linewidth = 3)),
    quote(geom_model(model = model, size = 2, linewidth = 3)),
    quote(stat_model(model = model, size = 2, linewidth = 3)),
    quote(stat_sd_ruler(size = 2, linewidth = 3)),
    quote(geom_squareplot(size = 2, linewidth = 3)),
    quote(stat_squareplot(size = 2, linewidth = 3))
  )
  for (call in calls) {
    warnings <- character()
    layer <- withCallingHandlers(eval(call), warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })
    expect_length(warnings, 1L)
    expect_match(warnings, paste0("`", call[[1]], "()`"), fixed = TRUE)
    expect_false(any(grepl("coursekata package|report the issue", warnings)))
    expect_null(layer$aes_params$size)
    expect_equal(layer$aes_params$linewidth, 3)
    expect_identical(layer$constructor, as.call(list(call[[1]])))
  }
})

test_that("point sizes and live annotation marker sizes keep their meanings", {
  expect_no_warning(point <- stat_sd_ruler(geom = "point", size = 7))
  expect_equal(point$aes_params$size, 7)
  expect_null(point$aes_params$linewidth)
  params <- list(size = 5, linewidth = 2)
  expect_identical(normalize_linewidth(params, ggplot2::GeomPoint, "point"), params)
})

test_that("size-only inputs override implicit line-width defaults", {
  model <- lm(Thumb ~ Height, data = Fingers)
  p <- gf_point(Thumb ~ Height, data = Fingers)
  native <- list(
    geom_resid, geom_square_resid, stat_resid, geom_reduce,
    geom_square_reduce, stat_reduce, geom_model, stat_model
  )
  for (constructor in native) {
    expect_warning(layer <- constructor(model = model, size = 2), class = "coursekata_linewidth")
    expect_equal(layer$aes_params$linewidth, 2)
    expect_null(layer$aes_params$size)
  }
  for (constructor in list(stat_sd_ruler, geom_squareplot, stat_squareplot)) {
    expect_warning(layer <- constructor(size = 2), class = "coursekata_linewidth")
    expect_equal(layer$aes_params$linewidth, 2)
    expect_null(layer$aes_params$size)
  }
  adapters <- list(
    gf_resid, gf_square_resid, gf_squaresid, gf_reduce, gf_square_reduce, gf_squareduce
  )
  for (constructor in adapters) {
    expect_warning(out <- suppressMessages(constructor(p, model, size = 2)),
                   class = "coursekata_linewidth")
    expect_equal(tail(out$layers, 1)[[1]]$aes_params$linewidth, 2)
    expect_warning(out <- suppressMessages(constructor(p, model, size = 2, linewidth = 3)),
                   class = "coursekata_linewidth")
    expect_equal(tail(out$layers, 1)[[1]]$aes_params$linewidth, 3)
  }
  for (constructor in list(gf_resid_fun, gf_square_resid_fun)) {
    expect_warning(out <- constructor(p, function(x) x, size = 2),
                   class = "coursekata_linewidth")
    expect_equal(tail(out$layers, 1)[[1]]$aes_params$linewidth, 2)
  }
  for (args in list(list(), list(model = model))) {
    out <- suppressMessages(do.call(gf_model, c(list(object = p, size = 2), args)))
    expect_equal(tail(out$layers, 1)[[1]]$aes_params$linewidth, 2)
  }
  expect_warning(ruler <- suppressMessages(gf_sd_ruler(p, size = 2)),
                 class = "coursekata_linewidth")
  expect_equal(tail(ruler$layers, 1)[[1]]$aes_params$linewidth, 2)
  expect_warning(squares <- gf_squareplot(~Thumb, data = Fingers, size = 2),
                 class = "coursekata_linewidth")
  expect_equal(squares$layers[[1]]$aes_params$linewidth, 2)
})

test_that("construction-time ggplot2 warnings identify the public layer", {
  calls <- list(
    quote(geom_resid(model = lm(Thumb ~ Height, data = Fingers), unknown = 1)),
    quote(geom_model(unknown = 1)),
    quote(stat_sd_ruler(unknown = 1)),
    quote(ggplot2::ggplot(Fingers, ggplot2::aes(Thumb)) + geom_squareplot(unknown = 1))
  )
  expected <- c("geom_resid", "geom_model", "stat_sd_ruler", "geom_squareplot")
  for (i in seq_along(calls)) {
    warnings <- list()
    withCallingHandlers(eval(calls[[i]]), warning = function(cnd) {
      warnings[[length(warnings) + 1L]] <<- cnd
      invokeRestart("muffleWarning")
    })
    expect_length(warnings, 1L)
    expect_match(conditionMessage(warnings[[1]]), "Ignoring unknown parameters")
    expect_identical(conditionCall(warnings[[1]]), call2(expected[[i]]))
  }
})

test_that("constructor and build failures name the public function inside wrappers", {
  residual <- function() geom_resid(model = NULL)
  model <- function() geom_model(model = ~Height)
  squareplot <- function() geom_squareplot(na.rm = FALSE)
  expect_identical(conditionCall(tryCatch(residual(), error = identity)), quote(geom_resid()))
  expect_identical(conditionCall(tryCatch(model(), error = identity)), quote(geom_model()))
  expect_identical(
    conditionCall(tryCatch(squareplot(), error = identity)), quote(geom_squareplot())
  )

  fit <- lm(Thumb ~ Height + Sex, data = Fingers)
  p <- ggplot2::ggplot(Fingers[c("Height", "Thumb")], ggplot2::aes(Height, Thumb)) +
    geom_resid(model = fit)
  error <- tryCatch(ggplot2::ggplot_build(p), error = identity)
  expect_identical(conditionCall(error), quote(geom_resid()))
  expect_match(conditionMessage(error), "missing from the plot's data: Sex")
})
