test_that("the fivethirtyeightdata repo is included", {
  expect_true("https://fivethirtyeightdata.github.io/drat/" %in% coursekata_repos())
})


test_that("a default CRAN mirror is selected if one is not given", {
  expect_true("CRAN" %in% names(coursekata_repos(repos = character(0))))
})
