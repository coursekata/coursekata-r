test_that("it determines whether a package is attached or not", {
  # use a package that can be unloaded and reloaded (so, can't be imported by coursekata)
  pkgs <- "fivethirtyeight"

  # make sure the package can be added
  if (!require(pkgs, character.only = TRUE, quietly = TRUE)) {
    fail(paste("Package not available:", pkgs, "Make sure to install all SUGGESTS packages."))
  }

  purrr::walk(pkgs, detacher)
  attached <- pkg_is_attached(pkgs)
  expect_vector(attached, logical(), length(pkgs))
  expect_true(all(!attached))

  # reattach for other tests
  purrr::walk(pkgs, attacher)
  expect_true(all(pkg_is_attached(pkgs)))
})


test_that("it retrieves the package version for currently installed packages, or NA", {
  pkgs <- c("supernova", "lsr", "does-not-exist")
  expect_vector(pkg_version(pkgs), character(), length(pkgs))
  expect_identical(pkg_version(pkgs)[[3]], NA_character_)
})


test_that("requiring a package is vectorized", {
  pkgs <- "fivethirtyeight" # use this package because it is not imported
  purrr::walk(pkgs, detacher)
  expect_identical(pkg_require(pkgs, quietly = TRUE), pkg_is_installed(pkgs))
})


test_that("requiring a package can be quiet", {
  pkgs <- "fivethirtyeight" # use this package because it is not imported
  purrr::walk(pkgs, detacher)
  expect_message(pkg_require(pkgs, quietly = TRUE), NA)
})
