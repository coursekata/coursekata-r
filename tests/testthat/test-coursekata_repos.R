test_that("no drat repository is added", {
  expect_false(any(grepl("drat", coursekata_repos(), fixed = TRUE)))
})


test_that("a default CRAN mirror is selected if one is not given", {
  expect_true("CRAN" %in% names(coursekata_repos(repos = character(0))))
})
