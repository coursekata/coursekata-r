test_that("native models follow the selected observation source", {
  d <- data.frame(x = 1:8, y = c(1, 2, 4, 7, 11, 16, 22, 29), w = 1:8)
  source <- ggplot2::ggplot() +
    suppressWarnings(ggplot2::geom_point(data = d, ggplot2::aes(x, y, weight = w)))

  inferred <- source + geom_model()
  rows <- ggplot2::layer_data(inferred, layer_index(inferred, "model"))
  fit <- lm(y ~ x, data = d, weights = w)
  expect_equal(rows$y, unname(predict(fit, data.frame(x = rows$x))))

  other <- data.frame(a = 11:18, b = 31:38)
  explicit <- ggplot2::ggplot(other, ggplot2::aes(a, b)) +
    ggplot2::geom_point(data = d, ggplot2::aes(x, y), inherit.aes = FALSE) +
    geom_model(model = lm(y ~ x, data = d), n = 9)
  explicit_rows <- ggplot2::layer_data(explicit, layer_index(explicit, "model"))
  expect_equal(range(explicit_rows$x), range(d$x))
})

test_that("source callbacks remain live for native models and squareplots", {
  d <- data.frame(x = 1:8, y = (1:8)^2)
  select <- function(rows) rows[seq_len(min(5L, nrow(rows))), , drop = FALSE]
  source <- ggplot2::ggplot(d) +
    ggplot2::geom_point(data = select, ggplot2::aes(x, y))

  model <- source + geom_model()
  rows <- ggplot2::layer_data(model, layer_index(model, "model"))
  fit <- lm(y ~ x, data = select(d))
  expect_equal(rows$y, unname(predict(fit, data.frame(x = rows$x))))

  distribution_source <- ggplot2::ggplot(d) +
    ggplot2::geom_rug(data = select, ggplot2::aes(x))
  counted <- distribution_source + geom_squareplot()
  expect_s3_class(counted$layers[[2]]$stat, "StatSquareplot")
  expect_equal(sum(ggplot2::layer_data(counted, 2)$count), nrow(select(d)))

  replacement <- transform(d, x = x + 100, y = y + 10)
  replaced <- model + replacement
  replaced_rows <- ggplot2::layer_data(replaced, layer_index(replaced, "model"))
  expect_equal(range(replaced_rows$x), range(select(replacement)$x))
})

test_that("waiver data inherits the canonical residual source", {
  d <- data.frame(x = 1:6, y = c(2, 4, 5, 8, 9, 13))
  fit <- lm(y ~ x, data = d)
  plot <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point()
  for (constructor in list(geom_resid, geom_square_resid, geom_reduce,
                           geom_square_reduce, stat_resid, stat_reduce)) {
    built <- ggplot2::ggplot_build(
      plot + constructor(data = ggplot2::waiver(), model = fit)
    )
    expect_equal(nrow(utils::tail(built$data, 1L)[[1L]]), nrow(d))
  }
})
