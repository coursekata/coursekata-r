test_that("resid_plan draws a segment from each point to its fitted value", {
  geom <- list(x = c(1, 2, 3), y = c(2, 4, 6), x_range = c(0, 4), y_range = c(0, 8))
  plan <- resid_plan(geom, fitted = c(1, 5, 6))

  expect_equal(plan$x, c(1, 2, 3))
  expect_equal(plan$xend, c(1, 2, 3))
  expect_equal(plan$y, c(1, 5, 6))
  expect_equal(plan$yend, c(2, 4, 6))
})

test_that("square_resid_plan emits four vertices per point, squared on the residual side", {
  geom <- list(x = c(1, 3), y = c(2, 4), x_range = c(0, 4), y_range = c(0, 8))
  plan <- square_resid_plan(geom, fitted = c(1, 5), aspect = 1)

  expect_equal(nrow(plan), 8L)
  expect_equal(sort(unique(plan$id)), c(1L, 2L))

  first <- plan[plan$id == 1, ]
  # vertices 1 and 4 sit on the point's own x -- that is the residual side
  expect_equal(first$x[[1]], 1)
  expect_equal(first$x[[4]], 1)
  expect_equal(first$y[[1]], 2)
  expect_equal(first$y[[4]], 1)
})

test_that("square_resid_plan draws squares away from the nearer edge of the panel", {
  geom <- list(x = c(1, 9), y = c(0, 0), x_range = c(0, 10), y_range = c(0, 10))
  plan <- square_resid_plan(geom, fitted = c(1, 1), aspect = 1)

  left <- plan[plan$id == 1, ]
  right <- plan[plan$id == 2, ]
  expect_gt(max(left$x), 1)   # a point left of centre extends rightwards
  expect_lt(min(right$x), 9)  # a point right of centre extends leftwards
})

test_that("square_resid_plan scales the drawn x side by the aspect and the panel's shape", {
  geom <- list(x = c(1, 2), y = c(4, 6), x_range = c(0, 10), y_range = c(0, 20))
  plan <- square_resid_plan(geom, fitted = c(2, 3), aspect = 4 / 6)
  side <- function(axis, i) diff(range(plan[[axis]][plan$id == i]))

  expect_equal(c(side("y", 1), side("y", 2)), c(2, 3))
  expect_equal(c(side("x", 1), side("x", 2)), c(2, 3) * (4 / 6) * (10 / 20))
})

test_that("the plans refuse fitted values that are not one per plotted point", {
  geom <- list(x = c(1, 2, 3), y = c(2, 4, 6), x_range = c(0, 4), y_range = c(0, 8))

  expect_error(resid_plan(geom, fitted = c(1, 2)), "cover the same observations")
  expect_error(resid_plan(geom, fitted = c(1, 2)), "draws 3 points")
  expect_error(resid_plan(geom, fitted = c(1, 2)), "gives 2 fitted values")
  expect_error(square_resid_plan(geom, c(1, 2), aspect = 1), "cover the same observations")
})
