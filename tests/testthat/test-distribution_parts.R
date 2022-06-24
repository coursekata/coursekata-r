test_that("they output logical vectors whose lengths match input lengths", {
  expect_vector(middle(1:10), logical(), 10)
  expect_vector(upper(1:11), logical(), 11)
  expect_vector(lower(1:10), logical(), 10)
})


test_that("lower and upper mark 5% of their respective positions by default", {
  expect_identical(lower(1:1000), c(rep(TRUE, 25), rep(FALSE, 975)))
  expect_identical(upper(1:1000), c(rep(FALSE, 975), rep(TRUE, 25)))
})


test_that("middle and tails mark the middle 95% of the data by default", {
  expect_identical(middle(1:1000), c(rep(FALSE, 25), rep(TRUE, 950), rep(FALSE, 25)))
  expect_identical(tails(1:1000), c(rep(TRUE, 25), rep(FALSE, 950), rep(TRUE, 25)))
})


test_that("upper, lower, tails, and middle take variable proportions", {
  expect_identical(lower(1:5, .2), c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(middle(1:5, .2), c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(tails(1:5, .2), c(TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_identical(upper(1:5, .6), c(FALSE, FALSE, TRUE, TRUE, TRUE))
})


test_that("upper, lower, tails, and middle are greedy for when the cutoff is not clean", {
  expect_identical(lower(1:5, .3), c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(middle(1:5, .3), c(FALSE, TRUE, TRUE, TRUE, FALSE))
  expect_identical(tails(1:5, .3), c(TRUE, FALSE, FALSE, FALSE, TRUE))
  expect_identical(upper(1:5, .7), c(FALSE, TRUE, TRUE, TRUE, TRUE))
})


test_that("the values do not need to be pre-arranged", {
  expect_identical(lower(c(2, 1, 3, 4), .25), c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(upper(c(2, 1, 3, 4), .25), c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(middle(c(2, 1, 3, 4), .5), c(TRUE, FALSE, TRUE, FALSE))
  expect_identical(tails(c(2, 1, 3, 4), .5), c(FALSE, TRUE, FALSE, TRUE))
})

test_that("it ignores NA values", {
  expect_identical(lower(c(2, 1, 3, 4, NA_real_), .25), c(FALSE, TRUE, FALSE, FALSE, rlang::na_lgl))
  expect_identical(upper(c(2, 1, 3, 4, NA_real_), .25), c(FALSE, FALSE, FALSE, TRUE, rlang::na_lgl))
  expect_identical(middle(c(2, 1, 3, 4, NA_real_), .5), c(TRUE, FALSE, TRUE, FALSE, rlang::na_lgl))
  expect_identical(tails(c(2, 1, 3, 4, NA_real_), .5), c(FALSE, TRUE, FALSE, TRUE, rlang::na_lgl))
})
