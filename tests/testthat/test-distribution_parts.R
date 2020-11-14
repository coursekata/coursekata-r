test_that("distribution helpers output logical vectors whose lengths match input lengths", {
  expect_vector(middle(1:10), logical(), 10)
  expect_vector(upper(1:11), logical(), 11)
  expect_vector(lower(1:10), logical(), 10)
})


test_that("lower and upper mark 5% of their respective positions by default", {
  expect_identical(lower(1:1000), c(rep(TRUE, 25), rep(FALSE, 975)))
  expect_identical(upper(1:1000), c(rep(FALSE, 975), rep(TRUE, 25)))
})


test_that("middle marks the middle 95% of the data by default", {
  expect_identical(middle(1:1000), c(rep(FALSE, 25), rep(TRUE, 950), rep(FALSE, 25)))
})


test_that("upper, lower, and middle take variable proportions", {
  expect_identical(lower(1:5, .2), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(middle(1:5, .2), c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(upper(1:5, .6), c(FALSE, FALSE, TRUE, TRUE, TRUE))
})


test_that("upper, lower, and middle are greedy for when the cutoff is not clean", {
  expect_identical(lower(1:5, .3), c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(middle(1:5, .3), c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(upper(1:5, .7), c(FALSE, TRUE, TRUE, TRUE, TRUE))
})
