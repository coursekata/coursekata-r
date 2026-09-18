test_that("inherited local mappings share source callbacks and matching evaluations", {
  rows <- data.frame(x = 1:15, y = (1:15)^2)
  calls <- evaluations <- 0L
  callback <- function(data) { calls <<- calls + 1L; data[sample(nrow(data), 6), ] }
  shuffled <- function(value) { evaluations <<- evaluations + 1L; sample(value) }
  base <- ggplot2::ggplot(rows, ggplot2::aes(x, shuffled(y))) + ggplot2::geom_point(data = callback)
  out <- base + geom_b(mapping = ggplot2::aes(x, shuffled(y)), show_b0 = FALSE, run = 1)
  expect_equal(calls, 0L)
  for (i in 1:2) {
    expect_no_warning(built <- ggplot2::ggplot_build(out))
    expect_equal(calls, i)
    expect_equal(evaluations, i)
    points <- built$data[[plot_source_index(out)]]
    fit <- lm(y ~ x, points)
    rise <- built$data[[layer_index(out, "b1")]]
    expect_equal(rise$y, unname(predict(fit, data.frame(x = rise$x))))
    expect_equal(rise$yend - rise$y, unname(coef(fit)[[2L]]))
  }
  expect_null(attr(base$scales, "coursekata_b_sources"))
  expect_null(attr(out$scales, "coursekata_b_sources"))
  calls <- 0L
  changed <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) + ggplot2::geom_point(data = callback) +
    geom_b(mapping = ggplot2::aes(y = y * 2), show_b0 = FALSE, run = 1)
  built <- ggplot2::ggplot_build(changed)
  expect_equal(calls, 1L)
  points <- built$data[[plot_source_index(changed)]]
  rise <- built$data[[layer_index(changed, "b1")]]
  expect_equal(rise$yend - rise$y, 2 * unname(coef(lm(y ~ x, points))[[2L]]))
})

test_that("explicit local data remains independent and overrides sibling inference", {
  rows <- data.frame(x = 1:10, y = (1:10)^2)
  local <- transform(rows, y = 3 * x + 5)
  base <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) + ggplot2::geom_point() + geom_model()
  out <- base + geom_b(data = local, show_b0 = FALSE, run = 1)
  expect_no_warning(built <- ggplot2::ggplot_build(out))
  rise <- built$data[[layer_index(out, "b1")]]
  expect_equal(rise$yend - rise$y, 3)
  expect_equal(rise$y, 5 + 3 * rise$x)
  mapped <- base + geom_b(mapping = ggplot2::aes(y = y * 2), show_b0 = FALSE, run = 1)
  rise <- ggplot2::ggplot_build(mapped)$data[[layer_index(mapped, "b1")]]
  expect_equal(rise$yend - rise$y, 2 * unname(coef(lm(y ~ x, rows))[[2L]]))
  visible_calls <- local_calls <- 0L
  visible <- function(data) { visible_calls <<- visible_calls + 1L; data }
  independent <- function(data) { local_calls <<- local_calls + 1L; transform(data, y = 3 * x + 5) }
  out <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) + ggplot2::geom_point(data = visible) +
    geom_b(data = independent, show_b0 = FALSE, run = 1)
  built <- ggplot2::ggplot_build(out)
  expect_equal(visible_calls, 1L)
  expect_equal(local_calls, 1L)
  rise <- built$data[[layer_index(out, "b1")]]
  expect_equal(rise$yend - rise$y, 3)
})

test_that("standalone categorical limits and unrelated grouping preserve the claim", {
  mark <- function(plot, tag) ggplot2::ggplot_build(plot)$data[[layer_index(plot, tag)]]
  rows <- data.frame(g = factor(rep(c("a", "b", "c"), each = 3)), y = 1:9,
                     colour = rep(c("z", "a", "m"), each = 3))
  base <- ggplot2::ggplot(rows, ggplot2::aes(g, y)) + ggplot2::geom_point()
  limited <- base + ggplot2::scale_x_discrete(limits = c("b", "c"))
  a <- limited + geom_b()
  b <- limited + geom_model() + geom_b()
  for (tag in c("b0", "bk_2", "bk_2_label")) {
    expect_equal(suppressWarnings(mark(a, tag)), suppressWarnings(mark(b, tag)))
  }
  expect_equal(mark(a, "b0")$y, 5)
  expected <- mark(base + geom_b(), "b0")$y
  for (mapping in list(ggplot2::aes(colour = colour), ggplot2::aes(group = colour))) {
    out <- base + mapping + geom_b()
    expect_equal(mark(out, "b0")$y, expected)
    expect_equal(mark(out, "bk_2")$yend, 5)
    expect_equal(mark(out, "bk_3")$yend, 8)
  }
})

test_that("supplied fits resolve final source axes rather than constructor axes", {
  mark <- function(plot, tag) ggplot2::ggplot_build(plot)$data[[layer_index(plot, tag)]]
  rows <- data.frame(x = 1:10, y = (1:10)^2)
  fit <- lm(y ~ x, rows)
  base <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) + ggplot2::geom_point()
  mapping <- ggplot2::aes(x = y, y = x)
  early <- base + mapping + geom_b(model = fit, show_b0 = FALSE, run = 1)
  late <- base + geom_b(model = fit, show_b0 = FALSE, run = 1) + mapping
  for (tag in c("b1", "run", "b1_label", "run_label")) expect_equal(mark(early, tag), mark(late, tag))
  rise <- mark(late, "b1")
  expect_equal(rise$xend - rise$x, unname(coef(fit)[[2L]]))
})

test_that("supplied fits retain original labels after residual mapping stabilization", {
  rows <- data.frame(x = 1:10, y = 2 + 3 * log(1:10))
  fit <- lm(y ~ log(x), rows)
  base <- ggplot2::ggplot(rows, ggplot2::aes(log(x), y)) + ggplot2::geom_point()
  stabilized <- stabilize_resid_mappings(base)
  out <- stabilized + geom_b(model = fit, show_b0 = FALSE, run = 0.1)
  expect_no_error(built <- ggplot2::ggplot_build(out))
  rise <- built$data[[layer_index(out, "b1")]]
  expect_equal(rise$yend - rise$y, unname(coef(fit)[[2L]]) * 0.1)
})

test_that("the documented lm and aov contract excludes generalized linear models", {
  rows <- data.frame(x = 1:8, y = c(1, 1, 2, 3, 2, 4, 5, 6))
  base <- ggplot2::ggplot(rows, ggplot2::aes(x, y)) + ggplot2::geom_point()
  fit <- glm(y ~ x, rows, family = poisson())
  expect_error(base + geom_b(model = fit), "geom_b.*lm.*aov")
  expect_error(gf_b(base, fit), "gf_b.*lm.*aov")
})
