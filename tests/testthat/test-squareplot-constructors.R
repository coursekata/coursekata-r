squareplot_contract <- function(plot) {
  built <- ggplot2::ggplot_build(plot)
  panel <- built$layout$panel_params[[1]]
  list(
    data = built$data,
    x_labels = panel$x$get_labels(),
    x_range = panel$x.range,
    y_breaks = panel$y$get_breaks(),
    y_range = panel$y.range,
    labels = unclass(as.list(built$plot$labels))[sort(names(built$plot$labels))]
  )
}

expect_squareplot_contract <- function(native, formula) {
  expect_identical(squareplot_contract(native), squareplot_contract(formula))
}

test_that("the native front doors return ggplot2 layers", {
  geom_layer <- geom_squareplot()
  stat_layer <- stat_squareplot()

  expect_s3_class(geom_layer, "LayerInstance")
  expect_s3_class(stat_layer, "LayerInstance")

  d <- data.frame(x = c(1, 1, 2))
  geom_plot <- ggplot2::ggplot(d, ggplot2::aes(x)) + geom_layer
  stat_plot <- ggplot2::ggplot(d, ggplot2::aes(x)) + stat_layer
  expect_identical(geom_plot$layers[[1]]$geom, GeomSquareplot)
  expect_identical(geom_plot$layers[[1]]$stat, StatSquareplot)
  expect_identical(stat_plot$layers[[1]]$geom, GeomSquareplot)
  expect_identical(stat_plot$layers[[1]]$stat, StatSquareplot)
})

test_that("both native constructors reproduce the continuous gf_squareplot contract", {
  d <- data.frame(
    x = c(1, 1, 2, 2, 2, 3),
    g = c("a", "b", "a", "b", "a", "b")
  )
  formula <- gf_squareplot(
    ~x, data = d, fill = ~g, binwidth = 1, boundary = 0.5,
    bars = "outline"
  )
  geom <- ggplot2::ggplot(d, ggplot2::aes(x)) +
    geom_squareplot(
      ggplot2::aes(fill = g), binwidth = 1, boundary = 0.5,
      bars = "outline"
    )
  stat <- ggplot2::ggplot(d, ggplot2::aes(x)) +
    stat_squareplot(
      ggplot2::aes(fill = g), binwidth = 1, boundary = 0.5,
      bars = "outline"
    )

  expect_squareplot_contract(geom, formula)
  expect_squareplot_contract(stat, formula)
})

test_that("both native constructors pass through the complete binning vocabulary", {
  d <- data.frame(x = c(0, 2.5, 5, 5, 7.5, 10))
  cases <- list(
    bins = list(bins = 4),
    center = list(binwidth = 2.5, center = 1.25),
    breaks = list(breaks = c(0, 2.5, 5, 10)),
    closed = list(binwidth = 2.5, boundary = 0, closed = "left"),
    pad = list(binwidth = 2.5, boundary = 0, pad = TRUE)
  )

  for (args in cases) {
    formula <- rlang::exec(gf_squareplot, ~x, data = d, !!!args)
    geom <- ggplot2::ggplot(d, ggplot2::aes(x)) +
      rlang::exec(geom_squareplot, !!!args)
    stat <- ggplot2::ggplot(d, ggplot2::aes(x)) +
      rlang::exec(stat_squareplot, !!!args)

    expect_squareplot_contract(geom, formula)
    expect_squareplot_contract(stat, formula)
  }
})

test_that("both native constructors preserve square and bar settings", {
  d <- data.frame(x = c(1, 1, 2))
  args <- list(
    bars = "solid", colour = "red", fill = "coral", alpha = 0.4,
    bar_color = "navy", bar_linewidth = 2
  )
  plots <- list(
    formula = rlang::exec(gf_squareplot, ~x, data = d, !!!args),
    geom = ggplot2::ggplot(d, ggplot2::aes(x)) +
      rlang::exec(geom_squareplot, !!!args),
    stat = ggplot2::ggplot(d, ggplot2::aes(x)) +
      rlang::exec(stat_squareplot, !!!args)
  )
  settings <- lapply(plots, function(plot) {
    layer <- ggplot2::ggplot_build(plot)$plot$layers[[1]]
    list(aes = layer$aes_params, geom = layer$geom_params)
  })

  expect_identical(settings$geom, settings$formula)
  expect_identical(settings$stat, settings$formula)
})

test_that("plot-level fill and colour mappings retain gf_squareplot's fixed defaults", {
  d <- data.frame(x = c(1, 1, 2), g = c("a", "b", "a"))
  make_base <- function() {
    ggplot2::ggplot(d, ggplot2::aes(x, fill = g, colour = g))
  }
  formula <- make_base() %>% gf_squareplot()

  for (constructor in list(geom_squareplot, stat_squareplot)) {
    native <- make_base() + constructor()
    expect_squareplot_contract(native, formula)
    layer <- ggplot2::layer_data(native)
    expect_identical(unique(layer$fill), "#7fcecc")
    expect_identical(unique(layer$colour), "white")
    expect_length(ggplot2::ggplot_build(native)$plot$guides$guides, 0)
  }
})

test_that("the native constructors count the same discrete inputs and retain factor levels", {
  cases <- list(
    factor = factor(c("a", "a", "e", "e"), levels = letters[1:5]),
    character = c("a", "a", "b", "c"),
    logical = c(TRUE, TRUE, FALSE, TRUE)
  )

  for (name in names(cases)) {
    d <- data.frame(x = cases[[name]])
    formula <- gf_squareplot(~x, data = d)
    geom <- ggplot2::ggplot(d, ggplot2::aes(x)) + geom_squareplot()
    stat <- ggplot2::ggplot(d, ggplot2::aes(x)) + stat_squareplot()

    expect_squareplot_contract(geom, formula)
    expect_squareplot_contract(stat, formula)
  }
})

test_that("native squareplots preserve gf_squareplot scale ownership", {
  d <- data.frame(x = c(1, 1, 2, 2, 2, 3))
  make_base <- function() {
    ggplot2::ggplot(d, ggplot2::aes(x)) +
      ggplot2::scale_x_continuous(breaks = c(1, 3), limits = c(0, 4)) +
      ggplot2::scale_y_continuous(breaks = c(0, 2, 4), limits = c(0, 4))
  }

  formula <- make_base() %>% gf_squareplot()
  for (constructor in list(geom_squareplot, stat_squareplot)) {
    native <- make_base() + constructor()
    expect_squareplot_contract(native, formula)
    expect_identical(native$scales$get_scales("x")$breaks, c(1, 3))
    expect_identical(native$scales$get_scales("y")$breaks, c(0, 2, 4))
  }
})

test_that("native squareplots do not replace a caller-owned discrete x scale", {
  d <- data.frame(x = factor(c("a", "c"), levels = c("a", "b", "c")))
  make_base <- function() {
    ggplot2::ggplot(d, ggplot2::aes(x)) +
      ggplot2::scale_x_discrete(limits = c("c", "b", "a"), drop = TRUE)
  }

  formula <- make_base() %>% gf_squareplot()
  for (constructor in list(geom_squareplot, stat_squareplot)) {
    native <- make_base() + constructor()
    expect_squareplot_contract(native, formula)
    expect_identical(native$scales$get_scales("x")$limits, c("c", "b", "a"))
    expect_true(native$scales$get_scales("x")$drop)
  }
})

test_that("native squareplots reproduce the discrete binning warning", {
  d <- data.frame(x = factor(c("a", "a", "b")))
  expect_warning(
    ggplot2::ggplot(d, ggplot2::aes(x)) + geom_squareplot(binwidth = 2),
    class = "coursekata_squareplot_binning"
  )
  expect_warning(
    ggplot2::ggplot(d, ggplot2::aes(x)) + stat_squareplot(boundary = 0),
    "`boundary`"
  )
  expect_no_warning(
    ggplot2::ggplot(d, ggplot2::aes(x)) + geom_squareplot()
  )
  geom <- suppressWarnings(
    ggplot2::ggplot(d, ggplot2::aes(x)) + geom_squareplot(binwidth = 2)
  )
  stat <- suppressWarnings(
    ggplot2::ggplot(d, ggplot2::aes(x)) + stat_squareplot(boundary = 0)
  )
  expect_no_warning(expect_equal(nrow(ggplot2::layer_data(geom)), 3))
  expect_no_warning(expect_equal(nrow(ggplot2::layer_data(stat)), 3))
})

test_that("discrete dispatch validates parameters once against stat_count", {
  d <- data.frame(x = factor(c("a", "a", "b")))
  expect_no_warning(
    plot <- ggplot2::ggplot(d, ggplot2::aes(x)) + geom_squareplot(width = 0.5)
  )
  columns <- unique(ggplot2::layer_data(plot)[, c("xmin", "xmax")])
  expect_equal(columns$xmax - columns$xmin, c(0.5, 0.5))

  warnings <- character()
  withCallingHandlers(
    ggplot2::ggplot(data.frame(x = 1:3), ggplot2::aes(x)) +
      geom_squareplot(wibble = TRUE),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1)
  expect_match(warnings, "wibble")
})

test_that("geom_squareplot honours an explicit stat", {
  d <- data.frame(x = factor(c("a", "a", "b")))
  native_count <- ggplot2::ggplot(d, ggplot2::aes(x)) +
    geom_squareplot(stat = "count")
  formula_count <- gf_squareplot(~x, data = d, stat = "count")
  expect_squareplot_contract(native_count, formula_count)

  native_identity <- ggplot2::ggplot(d, ggplot2::aes(x)) +
    geom_squareplot(stat = "identity")
  formula_identity <- gf_squareplot(~x, data = d, stat = "identity")
  expect_identical(native_identity$layers[[1]]$stat, ggplot2::StatIdentity)
  expect_identical(formula_identity$layers[[1]]$stat, ggplot2::StatIdentity)
})

test_that("stat_squareplot leaves an overridden geom's defaults alone", {
  d <- data.frame(x = c(1, 1, 2, 3))
  expect_no_warning(
    formula <- gf_squareplot(~x, data = d, geom = "point")
  )
  expect_no_warning(
    native <- ggplot2::ggplot(d, ggplot2::aes(x)) +
      stat_squareplot(geom = "point")
  )

  expect_squareplot_contract(native, formula)
  expect_identical(native$layers[[1]]$geom, ggplot2::GeomPoint)
  expect_false(any(ggplot2::layer_data(native)$colour == "white"))
})

test_that("native squareplots keep gf_squareplot's x-only and missing-value contracts", {
  d <- data.frame(x = 1:3)
  expect_error(geom_squareplot(na.rm = FALSE), "na.rm = FALSE")
  expect_error(stat_squareplot(na.rm = FALSE), "na.rm = FALSE")
  expect_error(
    ggplot2::ggplot(d, ggplot2::aes(y = x)) + geom_squareplot(),
    "mapped to x"
  )
  expect_error(
    ggplot2::ggplot(d, ggplot2::aes(x, y = x)) + stat_squareplot(),
    "mapped to x"
  )
})

test_that("native squareplots reproduce gf_squareplot's count-axis refusal", {
  d <- data.frame(x = c(1, 1, 2))
  formula_base <- ggplot2::ggplot(d, ggplot2::aes(x)) + ggplot2::scale_y_sqrt()
  native_base <- ggplot2::ggplot(d, ggplot2::aes(x)) + ggplot2::scale_y_sqrt()

  formula_error <- expect_error(formula_base %>% gf_squareplot())
  native_error <- expect_error(native_base + geom_squareplot())
  expect_identical(conditionMessage(native_error), conditionMessage(formula_error))

  later_error <- expect_error(
    ggplot2::ggplot_build(
      ggplot2::ggplot(d, ggplot2::aes(x)) +
        geom_squareplot() +
        ggplot2::scale_y_sqrt()
    )
  )
  expect_no_match(conditionMessage(later_error), "gf_squareplot", fixed = TRUE)
})

test_that("a native squareplot can own its data and mapping", {
  d <- data.frame(x = c(1, 1, 2, 3))
  formula <- gf_squareplot(~x, data = d, fill = "coral")
  for (constructor in list(geom_squareplot, stat_squareplot)) {
    native <- ggplot2::ggplot() + constructor(
      mapping = ggplot2::aes(x), data = d, fill = "coral",
      inherit.aes = FALSE
    )
    expect_squareplot_contract(native, formula)
  }
})

test_that("native squareplots reject deferred layer data like gf_squareplot", {
  data_fn <- function(data) data.frame(x = factor(c("a", "b")))
  expect_error(geom_squareplot(data = data_fn), "data frame or `NULL`")
  expect_error(stat_squareplot(data = ~data_fn(.x)), "data frame or `NULL`")
})

test_that("a layer-local x can opt out of unrelated plot mappings", {
  d <- data.frame(x = c(1, 1, 2), y = c(10, 20, 30))
  formula <- gf_squareplot(~x, data = d)
  base <- ggplot2::ggplot(d, ggplot2::aes(y = y))

  for (constructor in list(geom_squareplot, stat_squareplot)) {
    native <- base + constructor(
      mapping = ggplot2::aes(x), inherit.aes = FALSE
    )
    expect_squareplot_contract(native, formula)
  }
})
