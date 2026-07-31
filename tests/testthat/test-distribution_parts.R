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


# outer() tests -----------------------------------------------------------

test_that("outer(x, prop) is equivalent to tails(x, 1 - prop)", {
  x <- 1:1000
  expect_identical(
    suppressMessages(outer(x, .05)),
    tails(x, .95)
  )
  expect_identical(
    suppressMessages(outer(x, .10)),
    tails(x, .90)
  )
})

test_that("outer errors on prop <= 0 or prop >= 1", {
  expect_error(outer(1:10, 0))
  expect_error(outer(1:10, 1))
  expect_error(outer(1:10, -0.1))
  expect_error(outer(1:10, 1.5))
})

test_that("outer handles NA values the same as tails", {
  x <- c(2, 1, 3, 4, NA_real_)
  expect_identical(
    suppressMessages(outer(x, .5)),
    tails(x, .5)
  )
})

test_that("outer returns logical vector of correct length", {
  x <- 1:100
  result <- suppressMessages(outer(x, .05))
  expect_vector(result, logical(), 100)
})

test_that("a tail whose exact size is a whole number is not rounded down", {
  # (1 - .90) / 2 is 0.049999999999999989, so 100 * that floors to 4, not 5
  expect_equal(sum(middle(1:100, .90)), 90)
  expect_equal(sum(middle(1:100, .80)), 80)
  expect_equal(sum(middle(1:20, .90)), 18)
  expect_equal(sum(middle(1:1000, .80)), 800)
})

test_that("outer() and tails() inherit the corrected tail size", {
  expect_equal(sum(outer(1:100, .10)), 10)
  expect_equal(sum(outer(1:100, .20)), 20)
  expect_equal(sum(tails(1:100, .90)), 10)
})

test_that("greedy semantics are unchanged", {
  # documented: the upper 30% of 1:4 is greedy, so it takes 2 of the 4
  expect_equal(upper(1:4, .3), c(FALSE, FALSE, TRUE, TRUE))
  # middle() makes its tails non-greedy, so an exact 2.5-value tail floors to 2
  expect_equal(sum(middle(1:100, .95)), 96)
  expect_equal(sum(middle(1:100, .50)), 50)
})

test_that("a large sample's whole-number tail is not rounded down either", {
  # the drift grows with the tail, so a fixed decimal tolerance fails at scale
  # 10 * (5e6 * (1 - .90) / 2) is bit-identical to 5e7 * ((1 - .90) / 2), without
  # allocating 5e7 values
  expect_equal(tail_size(1:10, (1 - .90) / 2 * 5e6, greedy = FALSE), 2500000)
  expect_equal(tail_size(1:10, (1 - .90) / 2 * 1e7, greedy = FALSE), 5000000)
})

test_that("a greedy tail still takes at least one value for any positive proportion", {
  expect_equal(tail_size(1, 1e-10, greedy = TRUE), 1)
  expect_equal(tail_size(1, 1e-12, greedy = TRUE), 1)
})

test_that("a genuinely fractional tail is never snapped, however large", {
  # a tolerance that grows with n eventually exceeds 0.5 and rounds everything
  expect_equal(tail_size(seq_len(10), 4999999.96 / 10, greedy = FALSE), 4999999)
})
