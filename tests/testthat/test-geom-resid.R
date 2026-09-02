test_that("square_vertices emits four corners per point, squared on the residual side", {
  data <- data.frame(x = c(1, 3), y = c(2, 4), yend = c(1, 5))
  v <- square_vertices(data, x_range = c(0, 4), y_range = c(0, 8), aspect = 1)

  expect_equal(nrow(v), 8L)
  expect_equal(v$group, rep(1:2, each = 4))

  first <- v[v$group == 1, ]
  # corners 1 and 4 sit on the point's own x -- that is the residual side
  expect_equal(first$x[[1]], 1)
  expect_equal(first$x[[4]], 1)
  expect_equal(first$y[[1]], 2)
  expect_equal(first$y[[4]], 1)
})

test_that("square_vertices draws away from the nearer edge of the panel", {
  data <- data.frame(x = c(1, 9), y = c(0, 0), yend = c(1, 1))
  v <- square_vertices(data, x_range = c(0, 10), y_range = c(0, 10), aspect = 1)

  expect_gt(max(v$x[v$group == 1]), 1)
  expect_lt(min(v$x[v$group == 2]), 9)
})

test_that("square_vertices scales the drawn x side by the aspect and the panel's shape", {
  data <- data.frame(x = c(1, 2), y = c(4, 6), yend = c(2, 3))
  v <- square_vertices(data, x_range = c(0, 10), y_range = c(0, 20), aspect = 4 / 6)
  side <- function(axis, i) diff(range(v[[axis]][v$group == i]))

  expect_equal(c(side("y", 1), side("y", 2)), c(2, 3))
  expect_equal(c(side("x", 1), side("x", 2)), c(2, 3) * (4 / 6) * (10 / 20))
})

test_that("StatResid keeps the prediction it was handed", {
  data <- data.frame(x = 1:3, y = c(2, 4, 6), yend = c(1, 5, 6))
  out <- StatResid$compute_panel(data, scales = NULL)

  expect_equal(out$yend, c(1, 5, 6))
  expect_equal(out$y, c(2, 4, 6))
})

test_that("residual jitter matches each panel's points while holding the requested axis", {
  d <- data.frame(x = rep(1:4, 2), y = c(2, 4, 6, 8, 10, 20, 30, 40), panel = rep(1:2, each = 4))
  amounts <- list(
    list(NULL, NULL), list(.2, NULL), list(NULL, .3),
    list(0, 0), list(.2, 0), list(0, .3), list(.2, .3)
  )
  for (amount in amounts) {
    for (outcome in list(NULL, "x", "y")) {
      p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
        ggplot2::geom_point(position = ggplot2::position_jitter(amount[[1]], amount[[2]], seed = 42)) +
        ggplot2::layer(
          geom = GeomResid, stat = StatResid,
          position = position_resid_jitter(amount[[1]], amount[[2]], seed = 42, outcome = outcome),
          mapping = ggplot2::aes(yend = 0)
        ) +
        ggplot2::facet_wrap(~panel, scales = "free")
      built <- ggplot2::ggplot_build(p)$data
      expect_equal(built[[2]]$x, if (identical(outcome, "x")) d$x else built[[1]]$x)
      expect_equal(built[[2]]$y, if (identical(outcome, "y")) d$y else built[[1]]$y)
      expect_equal(built[[2]]$yend, rep(0, nrow(d)))
    }
  }
})

test_that("square_vertices measures the residual on the axis the prediction arrived on", {
  data <- data.frame(x = c(4, 6), y = c(1, 2), xend = c(2, 3))
  v <- square_vertices(data, x_range = c(0, 20), y_range = c(0, 10), aspect = 4 / 6)
  side <- function(axis, i) diff(range(v[[axis]][v$group == i]))

  expect_equal(c(side("x", 1), side("x", 2)), c(2, 3))
  expect_equal(c(side("y", 1), side("y", 2)), c(2, 3) * (4 / 6) * (10 / 20))

  first <- v[v$group == 1, ]
  # the residual edge holds y, and runs from the observation to the prediction
  expect_equal(first$y[[1]], 1)
  expect_equal(first$y[[4]], 1)
  expect_equal(first$x[[1]], 4)
  expect_equal(first$x[[4]], 2)
})
