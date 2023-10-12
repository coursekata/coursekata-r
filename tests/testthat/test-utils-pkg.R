test_that("it determines whether a package is attached or not", {
  pkgs <- "fivethirtyeight" # use this package because it is not imported

  purrr::walk(pkgs, detacher)
  expect_vector(pkg_is_attached(pkgs), logical(), length(pkgs))
  expect_true(all(!pkg_is_attached(pkgs)))

  # reattach for other tests
  purrr::walk(pkgs, attacher)
  expect_true(all(pkg_is_attached(pkgs)))
})


test_that("it retrieves the library location for currently installed packages, or NA", {
  pkgs <- c("supernova", "lsr", "does_not_exist")
  locations <- pkg_library_location(pkgs)
  expect_identical(unname(locations[3]), NA_character_)
  expect_true(all(dir.exists(locations[1:2])))
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

test_that("requiring a missing package calmly asks if you want to install it", {
  skip("Make sure to run this when testing locally.")

  menu_mock <- mockery::mock(FALSE)
  mockr::with_mock(
    .env = as.environment("package:coursekata"),
    ask_to_install = menu_mock,
    {
      expect_warning(pkg_require("does not exist"), NA)
      expect_length(mockery::mock_calls(menu_mock), 1)
    }
  )
})


test_that("installing fivethirtyeightdata works as intended", {
  skip("Make sure to run this when testing locally.")

  detacher("fivethirtyeightdata")

  tryCatch(
    suppressMessages(remove.packages("fivethirtyeightdata")),
    error = function(e) invisible(NULL)
  )

  pkg_install("fivethirtyeightdata")
  expect_true(require("fivethirtyeightdata"))
})
